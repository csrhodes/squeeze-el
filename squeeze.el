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
  (add-to-list 'completion-at-point-functions 'squeeze-complete-command-at-point)
  (add-hook 'comint-preoutput-filter-functions 'url-unhex-string nil t)
  (add-hook 'comint-preoutput-filter-functions 'squeeze-update-state nil t))

(defvar squeeze-control-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "SPC") 'squeeze-control-toggle-power)
    (define-key map (kbd "g") 'squeeze-control-refresh)
    (define-key map (kbd "+") 'squeeze-control-volume-up)
    (define-key map (kbd "-") 'squeeze-control-volume-down)
    (define-key map (kbd "t") 'squeeze-control-toggle-syncgroup-display)
    map))

(define-derived-mode squeeze-control-mode special-mode "SqueezeControl"
  "Major mode for controlling Squeezebox Servers.\\<squeeze-control-mode-map>")

(defvar squeeze-control-inhibit-display nil)

(defun squeeze-update-state (string)
  (let (done-something)
    (dolist (line (split-string string "\n"))
      (when (squeeze-update-state-from-line line)
        (setq done-something t)))
    (when done-something
      (unless squeeze-control-inhibit-display
        (squeeze-control-display-players))))
  string)

(defconst squeeze-player-line-regexp
  "^\\(\\(?:[0-9a-f]\\{2\\}%3A\\)\\{5\\}[0-9a-f]\\{2\\}\\) ")

(defun squeeze-find-player (id)
  (dolist (player squeeze-players)
    (when (string= id (squeeze-player-playerid player))
      (return player))))

(defun squeeze-update-power (player state)
  (if state
      (setf (squeeze-player-power player) state)
    (let ((current (squeeze-player-power player)))
      (setf (squeeze-player-power player)
            (cond ((string= current "0") "1")
                  ((string= current "1") "0"))))))

(defun squeeze-update-mixer-volume (player value)
  (let ((current (squeeze-player-volume player))
        (number (string-to-number value)))
    (if (string-match "^[-+]" value)
        (setf (squeeze-player-volume player)
              (and current (max 0 (min 100 (+ current number)))))
      (setf (squeeze-player-volume player) number))))

(defun squeeze-update-state-from-line (string)
  (cond
   ((string-match "^players 0" string)
    (setq squeeze-players (squeeze-parse-players-line string))
    t)
   ((string-match "^syncgroups" string)
    (setq squeeze-syncgroups (squeeze-parse-syncgroups-line string))
    t)
   ((string-match squeeze-player-line-regexp string)
    (let ((substring (substring string (match-end 0)))
          (id (url-unhex-string (match-string 1 string))))
      (cond
       ((string-match "^power\\(?: \\([01]\\)\\)?" substring)
        (let ((state (match-string 1 substring))
              (player (squeeze-find-player id)))
          (squeeze-update-power player state))
        t)
       ((string-match "^mixer volume \\(\\(?:-\\|%2B\\)?[0-9]*\\)" substring)
        (let ((value (url-unhex-string (match-string 1 substring)))
              (player (squeeze-find-player id)))
          (squeeze-update-mixer-volume player value))
        t))))))

(defface squeeze-player-face
  '((t))
  "Face for displaying players"
  :group 'squeeze)
(defface squeeze-player-on-face
  '((t :weight bold :inherit squeeze-player-face))
  "Face for displaying players which are on"
  :group 'squeeze)
(defface squeeze-player-off-face
  '((t :weight light :inherit squeeze-player-face))
  "Face for displaying players which are off"
  :group 'squeeze)

(defface squeeze-mixer-face
  '((t :weight bold))
  "Face for displaying mixer information"
  :group 'squeeze)
(defface squeeze-mixer-muted-face
  '((t :weight light :inherit squeeze-mixer-face))
  "Face for displaying mixer information when muted"
  :group 'squeeze)
(defface squeeze-mixer-quiet-face
  '((t :foreground "green3" :inherit squeeze-mixer-face))
  "Face for quiet volume"
  :group 'squeeze)
(defface squeeze-mixer-medium-face
  '((t :foreground "gold" :inherit squeeze-mixer-face))
  "Face for medium volume"
  :group 'squeeze)
(defface squeeze-mixer-loud-face
  '((t :foreground "OrangeRed1" :inherit squeeze-mixer-face))
  "Face for loud volume"
  :group 'squeeze)
(defface squeeze-mixer-muted-quiet-face
  '((t :inherit (squeeze-mixer-muted-face squeeze-mixer-quiet-face)))
  "Face for quiet volume when muted")
(defface squeeze-syncgroup-face
  '((t :slant italic))
  "Face for syncgroups"
  :group 'squeeze)

(defun squeeze-mixer-compute-bar (vol width)
  (let* ((exact (* width (/ vol 100.0)))
         (nfull (floor exact))
         (frac (- exact nfull))
         (nblank (floor (- width exact))))
    (format "%s%s%s"
            (make-string nfull ?█)
            (cond ((= width (+ nfull nblank)) "")
                  ((< frac 0.0625) " ")
                  ((< frac 0.1875) "▏")
                  ((< frac 0.3125) "▎")
                  ((< frac 0.4375) "▍")
                  ((< frac 0.5625) "▌")
                  ((< frac 0.6875) "▋")
                  ((< frac 0.8125) "▊")
                  ((< frac 0.9375) "▉")
                  (t "█"))
            (make-string nblank ? ))))

(defun squeeze-mixer-make-bar (vol width)
  (let ((bar (squeeze-mixer-compute-bar vol width))
        (lo (floor (* 0.65 width)))
        (hi (floor (* 0.9 width))))
    (concat "▕"
            (propertize (substring bar 0 lo) 'face 'squeeze-mixer-quiet-face)
            (propertize (substring bar lo hi) 'face 'squeeze-mixer-medium-face)
            (propertize (substring bar hi) 'face 'squeeze-mixer-loud-face)
            (propertize "▏" 'intangible t))))

(defvar squeeze-players ())
(defvar squeeze-syncgroups ())

(defun squeeze-control-query-syncgroups ()
  (interactive)
  (comint-send-string (get-buffer-process "*squeeze*") (format "syncgroups ?\n")))

(defun squeeze-control-query-players ()
  (interactive)
  (comint-send-string (get-buffer-process "*squeeze*") (format "players 0\n")))

(defun squeeze-control-toggle-power (&optional id)
  (interactive)
  (unless id
    (setq id (get-text-property (point) 'squeeze-playerid)))
  (comint-send-string (get-buffer-process "*squeeze*") (format "%s power\n" id)))

(defun squeeze-control-query-power (&optional id)
  (interactive)
  (unless id
    (setq id (get-text-property (point) 'squeeze-playerid)))
  (when id
    (comint-send-string (get-buffer-process "*squeeze*") (format "%s power ?\n" id))))

(defun squeeze-control-volume-up (&optional id inc)
  (interactive)
  (unless inc (setq inc 5))
  (unless id
    (setq id (get-text-property (point) 'squeeze-playerid)))
  (when id
    (comint-send-string (get-buffer-process "*squeeze*") (format "%s mixer volume %+d\n" id inc))))

(defun squeeze-control-volume-down (&optional id inc)
  (interactive)
  (unless inc (setq inc 5))
  (unless id
    (setq id (get-text-property (point) 'squeeze-playerid)))
  (when id
    (comint-send-string (get-buffer-process "*squeeze*") (format "%s mixer volume %+d\n" id (- inc)))))

(defun squeeze-control-volume-set (id val)
  (interactive)
  (comint-send-string (get-buffer-process "*squeeze*") (format "%s mixer volume %d\n" id val)))

(defun squeeze-control-query-mixer-volume (&optional id)
  (interactive)
  (unless id
    (setq id (get-text-property (point) 'squeeze-playerid)))
  (when id
    (comint-send-string (get-buffer-process "*squeeze*") (format "%s mixer volume ?\n" id))))

(defun squeeze-control-player-face (player)
  (let ((power (squeeze-player-power player)))
    (cond ((string= power "1") 'squeeze-player-on-face)
          ((string= power "0") 'squeeze-player-off-face)
          (t 'squeeze-player-face))))

(defun squeeze-control-listen ()
  (comint-send-string (get-buffer-process "*squeeze*") (format "listen 1\n")))

(defun squeeze-control-refresh ()
  (interactive)
  (let ((squeeze-control-inhibit-display t))
    (squeeze-control-query-players)
    (accept-process-output (get-buffer-process "*squeeze*"))
    (squeeze-control-query-syncgroups)
    (accept-process-output (get-buffer-process "*squeeze*"))
    (dolist (player squeeze-players)
      (squeeze-control-query-power (squeeze-player-playerid player))
      (accept-process-output (get-buffer-process "*squeeze*"))
      (squeeze-control-query-mixer-volume (squeeze-player-playerid player))
      (accept-process-output (get-buffer-process "*squeeze*"))))
  (squeeze-control-display-players))

(defvar squeeze-control-mixer-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'squeeze-control-mixer-set-volume)
    (define-key map [mouse-1] 'squeeze-control-mixer-mouse-1)
    map))

(defun squeeze-control-compute-volume (pos)
  (let* ((end (next-single-property-change pos 'keymap))
         (start (previous-single-property-change end 'keymap)))
    (/ (* 100 (- (point) start)) (- end start 1))))

(defun squeeze-control-mixer-mouse-1 (event)
  (interactive "e")
  (let* ((pos (cadadr event))
         (val (squeeze-control-compute-volume pos))
         (id (get-text-property pos 'squeeze-playerid)))
    (squeeze-control-volume-set id val)))

(defun squeeze-control-mixer-set-volume ()
  (interactive)
  (let* ((val (squeeze-control-compute-volume (point)))
         (id (get-text-property (point) 'squeeze-playerid)))
    (squeeze-control-volume-set id val)))

(defvar squeeze-control-display-syncgroups nil)

(defun squeeze-control-toggle-syncgroup-display ()
  (interactive)
  (setf squeeze-control-display-syncgroups
        (not squeeze-control-display-syncgroups))
  (squeeze-control-display-players))

(defun squeeze-control-insert-player (player)
  (insert (propertize (format "%20s" (squeeze-player-name player))
                      'face (squeeze-control-player-face player)
                      'squeeze-playerid (squeeze-player-playerid player)))
  (when (squeeze-player-volume player)
    (insert (propertize
             (squeeze-mixer-make-bar (squeeze-player-volume player) 28)
             'squeeze-playerid (squeeze-player-playerid player)
             'keymap squeeze-control-mixer-map
             'pointer 'hdrag
             'rear-nonsticky '(keymap))))
  (insert (propertize "\n" 'intangible t)))

(defun squeeze-control-display-players ()
  (interactive)
  (cond
   (squeeze-control-display-syncgroups
    (with-current-buffer (get-buffer-create "*squeeze-control*")
      (squeeze-control-mode)
      (read-only-mode -1)
      (erase-buffer)
      (let ((syncgroups squeeze-syncgroups)
            (seen))
        (while syncgroups
          (let ((names (getf syncgroups :names))
                (members (split-string (getf syncgroups :members) ",")))
            (insert (propertize names 'face 'squeeze-syncgroup-face) "\n")
            (dolist (member members)
              (let ((player (squeeze-find-player member)))
                (squeeze-control-insert-player player)
                (push player seen))))
          (setq syncgroups (cddddr syncgroups)))
        (insert (propertize "No syncgroup" 'face 'squeeze-syncgroup-face) "\n")
        (dolist (player squeeze-players)
          (unless (member player seen)
            (squeeze-control-insert-player player))))))
   (t
    (with-current-buffer (get-buffer-create "*squeeze-control*")
      (squeeze-control-mode)
      (read-only-mode -1)
      (erase-buffer)
      (dolist (player squeeze-players)
        (squeeze-control-insert-player player))
      (read-only-mode 1)))))

(cl-defstruct (squeeze-player (:constructor squeeze-make-player))
  playerindex playerid uuid ip name model isplayer displaytype canpoweroff connected power volume)

(defun squeeze-string-plistify (string start end)
  (save-match-data
    (let (result)
      (loop
       (if (string-match "\\([a-z]+\\)%3A\\([^ \n]+\\)" string start)
           (let ((mend (match-end 0)))
             (when (> mend end)
               (return))
             (push (intern (format ":%s" (substring string (match-beginning 1) (match-end 1)))) result)
             (push (url-unhex-string (substring string (match-beginning 2) (match-end 2))) result)
             (setq start mend))
         (return)))
      (nreverse result))))

(defun squeeze-parse-syncgroups-line (string)
  (let ((syncgroupspos (string-match "^syncgroups " string))
        (startpos (match-end 0)))
    (when startpos
      (squeeze-string-plistify string startpos (length string)))))

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
    (squeeze-control-listen)
    (squeeze-control-refresh)
    (squeeze-control-display-players)))
