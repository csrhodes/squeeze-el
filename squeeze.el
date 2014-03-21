(defgroup squeeze nil
  "Interact with Squeezebox media servers"
  :prefix "squeeze-" 
  :group 'applications)

(defcustom squeeze-server-address "localhost"
  "Address for the Squeezebox server"
  :group 'squeeze)
(defcustom squeeze-server-port 9090
  "Port number for the Squeezebox server"
  :group 'squeeze)

(defvar squeeze-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") 'completion-at-point)
    map))

(define-derived-mode squeeze-mode comint-mode "Squeeze"
  "Major mode for interacting with the Squeezebox Server CLI.\\<squeeze-mode-map>"
  (add-hook 'comint-preoutput-filter-functions 'url-unhex-string nil t)
  (add-hook 'comint-preoutput-filter-functions 'squeeze-update-state nil t))

(defvar squeeze-control-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "SPC") 'squeeze-control-toggle-power)
    (define-key map (kbd "g") 'squeeze-control-display-players)
    map))

(define-derived-mode squeeze-control-mode special-mode "SqueezeControl"
  "Major mode for controlling Squeezebox Servers.\\<squeeze-control-mode-map>")

(defun squeeze-update-state (string)
  (cond
   ((string-match "^players 0" string)
    (setq squeeze-players (squeeze-parse-players-line string))))
  string)

(defface squeeze-player-face
  '((t :weight bold))
  "Face for displaying players")

(defvar squeeze-players ())

(defun squeeze-control-toggle-power (&optional id)
  (interactive)
  (unless id
    (setq id (get-text-property (point) 'squeeze-playerid)))
  (comint-send-string (get-buffer-process "*squeeze*") (format "%s power\n" id)))

(defun squeeze-control-query-power (&optional id)
  (interactive)
  (unless id
    (setq id (get-text-property (point) 'squeeze-playerid)))
  (comint-send-string (get-buffer-process "*squeeze*") (format "%s power ?\n" id)))

(defun squeeze-control-display-players ()
  (interactive)
  (let ((players squeeze-players))
    (with-current-buffer (get-buffer-create "*squeeze-control*")
      (squeeze-control-mode)
      (read-only-mode -1)
      (erase-buffer)
      (dolist (player players)
        (insert (propertize (squeeze-player-name player)
                            'face 'squeeze-player-face
                            'squeeze-playerid (squeeze-player-playerid player))
                "\n"))
      (read-only-mode 1))))

(cl-defstruct (squeeze-player (:constructor squeeze-make-player))
  playerindex playerid uuid ip name model isplayer displaytype canpoweroff connected)

(defun squeeze-string-plistify (string start end)
  (save-match-data
    (let (result)
      (loop
       (message "start: %d" start)
       (if (string-match "\\([a-z]+\\)%3A\\([^ \n]+\\)" string start)
           (let ((mend (match-end 0)))
             (when (> mend end)
               (return))
             (push (intern (format ":%s" (substring string (match-beginning 1) (match-end 1)))) result)
             (push (url-unhex-string (substring string (match-beginning 2) (match-end 2))) result)
             (setq start mend))
         (return)))
      (nreverse result))))

(defun squeeze-parse-players-line (string)
  (let ((countpos (string-match " count%3A\\([0-9]\\) " string))
        (startpos (match-end 0)))
    (unless countpos
      (message "no count found in players line"))
    (let ((count (parse-integer string (match-beginning 1) (match-end 1)))
          result endpos)
      (dotimes (i (1- count))
        (setq endpos (progn (string-match " connected%3A[0-1] " string startpos)
                            (match-end 0)))
        (push (apply 'squeeze-make-player (squeeze-string-plistify string startpos endpos)) result)
        (setq startpos endpos))
      (push (apply 'squeeze-make-player (squeeze-string-plistify string startpos (length string))) result)
      result)))

(defun squeeze-complete-command-at-point ()
  (save-excursion
    (list (progn (backward-word) (point))
          (progn (forward-word) (point))
          '(;; General commands and queries
            "login" "can" "version" "listen" "subscribe" "pref"
            "logging" "getstring" "setsncredentials" "debug"
            "exit" "shutdown"

            ;; Player commands and queries
            "player" "count" "id" "uuid" "name" "ip" "model" "isplayer"
            "displaytype" "canpoweroff" "?" "signalstrength" "connected"
            "sleep" "sync" "syncgroups" "power" "mixer" "volume" "muting"
            "bass" "treble" "pitch" "show" "display" "linesperscreen"
            "displaynow" "playerpref" "button" "ir" "irenable"
            "connect" "client" "forget" "disconnect" "players"
            
            ;; Database commands and queries
            "rescan" "rescanprogress" "abortscan" "wipecache" "info"
            "total" "genres" "artists" "albums" "songs" "years"
            "musicfolder" "playlists" "tracks" "new" "rename" "delete"
            "edit" "songinfo" "titles" "search" "pragma"

            ;; Playlist commands and queries
            "play" "stop" "pause" "mode" "time" "genre" "artist" "album"
            "title" "duration" "remote" "current_title" "path" "playlist"
            "add" "insert" "deleteitem" "move" "delete" "preview" "resume"
            "save" "loadalbum" "addalbum" "loadtracks" "addtracks"
            "insertalbum" "deletealbum" "clear" "zap" "name" "url"
            "modified" "playlistsinfo" "index" "shuffle" "repeat"
            "playlistcontrol"

            ;; Compound queries
            "serverstatus" "status" "displaystatus" "readdirectory"

            ;; Notifications
            
            ;; Alarm commands and queries
            "alarm" "alarms"

            ;; Plugins commands and queries
            "favorites"
            ))))

(defun squeeze ()
  (interactive)
  (let ((buffer (make-comint-in-buffer "squeeze" nil
                                       (cons squeeze-server-address
                                             squeeze-server-port))))
    (switch-to-buffer buffer)
    (squeeze-mode)))

(defun squeeze-control ()
  (interactive)
  (squeeze)
  (let ((buffer (get-buffer-create "*squeeze-control*")))
    (switch-to-buffer buffer)
    (squeeze-control-display-players)))
