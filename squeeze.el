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
(defcustom squeeze-server-http-port 9000
  "Port number for the Squeezebox HTTP server"
  :group 'squeeze)
(defvar squeeze-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") 'completion-at-point)
    map))

(defun squeeze-unhex-string (string)
  (with-temp-buffer
    (let ((case-fold-search t)
          (start 0))
      (while (string-match "%[0-9a-f][0-9a-f]" string start)
        (let* ((s (match-beginning 0))
               (ch1 (url-unhex (elt string (+ s 1))))
               (code (+ (* 16 ch1)
                        (url-unhex (elt string (+ s 2))))))
          (insert (substring string start s)
                  (byte-to-string code))
          (setq start (match-end 0))))
      (insert (substring string start)))
    (buffer-string)))

(defun squeeze-unhex-and-decode-utf8-string (string)
  (decode-coding-string (squeeze-unhex-string string) 'utf-8))

(define-derived-mode squeeze-mode comint-mode "Squeeze"
  "Major mode for interacting with the Squeezebox Server CLI.\\<squeeze-mode-map>"
  (add-to-list 'completion-at-point-functions 'squeeze-complete-command-at-point)
  (add-hook 'comint-preoutput-filter-functions 'squeeze-unhex-and-decode-utf8-string nil t)
  (add-hook 'comint-preoutput-filter-functions 'squeeze-update-state nil t))

(defvar squeeze-control-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "SPC") 'squeeze-control-toggle-power)
    (define-key map (kbd "f") 'squeeze-control-play-favorite)
    (define-key map (kbd "g") 'squeeze-control-refresh)
    (define-key map (kbd "+") 'squeeze-control-volume-up)
    (define-key map (kbd "-") 'squeeze-control-volume-down)
    (define-key map (kbd "t") 'squeeze-control-toggle-syncgroup-display)
    (define-key map (kbd ">") 'squeeze-control-next-track)
    (define-key map (kbd "<") 'squeeze-control-previous-track)
    (define-key map (kbd "s") 'squeeze-control-select-player)
    (define-key map (kbd "a") 'squeeze-list-albums)
    map))

(defvar squeeze-control-current-player nil)

(defun squeeze-control-select-player (id)
  (interactive
   (list (or (get-text-property (point) 'squeeze-playerid)
             (let ((name (completing-read "Select player: " (mapcar 'squeeze-player-name squeeze-players))))
               (squeeze-player-playerid (squeeze-find-player-from-name name))))))
  (setq squeeze-control-current-player id))

(defun squeeze-control-next-track ()
  (interactive)
  (squeeze-send-string "%s playlist index +1" squeeze-control-current-player))

(defun squeeze-control-previous-track ()
  (interactive)
  (squeeze-send-string "%s playlist index -1" squeeze-control-current-player))

(define-derived-mode squeeze-control-mode special-mode "SqueezeControl"
  "Major mode for controlling Squeezebox Servers.\\<squeeze-control-mode-map>")

(defvar squeeze-control-inhibit-display nil)

(lexical-let ((buffer (get-buffer-create " *squeeze-update-state*")))
  (defun squeeze-update-state (string)
    ;; FIXME: we could make this a lot more elegant by using the
    ;; buffer abstraction more
    (if (cl-position ?\n string)
        (let (done-something)
          (with-current-buffer buffer
            (insert string)
            (setq string (buffer-string))
            (erase-buffer))
          (dolist (line (split-string string "\n"))
            (when (squeeze-update-state-from-line line)
              (setq done-something t)))
          (when done-something
            (unless squeeze-control-inhibit-display
              (squeeze-control-display-players))))
      (with-current-buffer buffer
        (insert string)))
    string))

(defconst squeeze-player-line-regexp
  "^\\(\\(?:[0-9a-f]\\{2\\}%3A\\)\\{5\\}[0-9a-f]\\{2\\}\\) ")

(defun squeeze-find-player (id)
  (dolist (player squeeze-players)
    (when (string= id (squeeze-player-playerid player))
      (return player))))

(defun squeeze-find-player-from-name (name)
  (dolist (player squeeze-players)
    (when (string= name (squeeze-player-name player))
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

(require 'notifications)

(defun squeeze-update-state-from-line (string)
  (cond
   ((string-match "^players 0" string)
    (setq squeeze-players (squeeze-parse-players-line string))
    t)
   ((string-match "^syncgroups" string)
    (setq squeeze-syncgroups (squeeze-parse-syncgroups-line string))
    t)
   ((string-match "^albums" string)
    (squeeze-parse-albums-line string)
    t)
   ((string-match squeeze-player-line-regexp string)
    (let ((substring (substring string (match-end 0)))
          (id (url-unhex-string (match-string 1 string))))
      (cond
       ((string-match "^playlist newsong \\(.*\\) \\([0-9]+\\)$" substring)
        (let ((value (save-match-data (url-unhex-string (match-string 1 substring))))
              (index (url-unhex-string (match-string 2 substring))))
          (notifications-notify :title "Now playing" :body (encode-coding-string (format "%s: %s" index value) 'utf-8)))
        t)
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
            (if (= width (+ nfull nblank))
                ""
              (string (aref " ▏▎▍▌▋▊▉█" (floor (+ frac 0.0625) 0.125))))
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

(defun squeeze-send-string (control &rest arguments)
  (let* ((process (get-buffer-process "*squeeze*"))
         (string (apply #'format control arguments))
         (length (length string)))
    (unless (and (> length 0) (char-equal (aref string (1- length)) ?\n))
      (setq string (format "%s\n" string)))
    (if process
        (comint-send-string process string)
      (error "can't find squeeze process"))))

(defun squeeze-control-query-syncgroups ()
  (interactive)
  (squeeze-send-string "syncgroups ?"))

(defun squeeze-control-query-players ()
  (interactive)
  (squeeze-send-string "players 0"))

(defun squeeze-control-toggle-power (&optional id)
  (interactive)
  (unless id
    (setq id (get-text-property (point) 'squeeze-playerid)))
  (squeeze-send-string "%s power" id))

(defun squeeze-control-play-favorite (&optional favorite id)
  (interactive "nFavourite: ")
  (unless id
    (setq id (get-text-property (point) 'squeeze-playerid)))
  (squeeze-send-string "%s favorites playlist play item_id:%d" id favorite))

(defun squeeze-control-query-power (&optional id)
  (interactive)
  (unless id
    (setq id (get-text-property (point) 'squeeze-playerid)))
  (when id
    (squeeze-send-string "%s power ?" id)))

(defun squeeze-control-volume-up (&optional id inc)
  (interactive)
  (unless inc (setq inc 5))
  (unless id
    (setq id (get-text-property (point) 'squeeze-playerid)))
  (when id
    (squeeze-send-string "%s mixer volume %+d" id inc)))

(defun squeeze-control-volume-down (&optional id inc)
  (interactive)
  (unless inc (setq inc 5))
  (unless id
    (setq id (get-text-property (point) 'squeeze-playerid)))
  (when id
    (squeeze-send-string "%s mixer volume %+d" id (- inc))))

(defun squeeze-control-volume-set (id val)
  (interactive)
  (squeeze-send-string "%s mixer volume %d" id val))

(defun squeeze-control-query-mixer-volume (&optional id)
  (interactive)
  (unless id
    (setq id (get-text-property (point) 'squeeze-playerid)))
  (when id
    (squeeze-send-string "%s mixer volume ?" id)))

(defun squeeze-control-player-face (player)
  (let ((power (squeeze-player-power player)))
    (cond ((string= power "1") 'squeeze-player-on-face)
          ((string= power "0") 'squeeze-player-off-face)
          (t 'squeeze-player-face))))

(defun squeeze-control-listen ()
  (squeeze-send-string "listen 1"))

(defun squeeze-accept-process-output ()
  (while (accept-process-output (get-buffer-process "*squeeze*") 0.1 nil t)))

(defun squeeze-control-refresh ()
  (interactive)
  (let ((squeeze-control-inhibit-display t))
    (squeeze-control-query-players)
    (squeeze-accept-process-output)
    (squeeze-control-query-syncgroups)
    (dolist (player squeeze-players)
      (squeeze-control-query-power (squeeze-player-playerid player))
      (squeeze-control-query-mixer-volume (squeeze-player-playerid player))))
  (squeeze-accept-process-output)
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
  (with-current-buffer (get-buffer-create "*squeeze-control*")
    (let ((saved (point)))
      (squeeze-control-mode)
      (read-only-mode -1)
      (erase-buffer)
      (cond
       (squeeze-control-display-syncgroups
        (let ((syncgroups squeeze-syncgroups)
              (seen))
          (while syncgroups
            (let ((names (getf syncgroups :sync_member_names))
                  ;; new server version has sync_members and sync_member_names
                  (members (split-string (getf syncgroups :sync_members) ",")))
              (insert (propertize names 'face 'squeeze-syncgroup-face) "\n")
              (dolist (member members)
                (let ((player (squeeze-find-player member)))
                  (squeeze-control-insert-player player)
                  (push player seen))))
            (setq syncgroups (cddddr syncgroups)))
          (insert (propertize "No syncgroup" 'face 'squeeze-syncgroup-face) "\n")
          (dolist (player squeeze-players)
            (unless (member player seen)
              (squeeze-control-insert-player player)))))
       (t
        (dolist (player squeeze-players)
          (squeeze-control-insert-player player))
        (read-only-mode 1)))
      (goto-char saved))))

(cl-defstruct (squeeze-player (:constructor squeeze-make-player))
  playerindex playerid uuid ip name model isplayer displaytype canpoweroff connected power volume)

(defun squeeze-string-plistify (string start end)
  (unless end
    (setq end (length string)))
  (save-match-data
    (let (result)
      (loop
       (if (string-match "\\([a-z_]+\\)%3A\\([^ \n]+\\)" string start)
           (let ((mend (match-end 0)))
             (when (> mend end)
               (return))
             (push (intern (format ":%s" (substring string (match-beginning 1) (match-end 1)))) result)
             (push (decode-coding-string
                    (url-unhex-string (substring string (match-beginning 2) (match-end 2)))
                    'utf-8)
                   result)
             (setq start mend))
         (return)))
      (nreverse result))))

(defun squeeze-parse-syncgroups-line (string)
  (let ((syncgroupspos (string-match "^syncgroups " string))
        (startpos (match-end 0)))
    (when startpos
      (squeeze-string-plistify string startpos (length string)))))

(defun squeeze-parse-count (string)
  (save-match-data
    (let ((countpos (string-match "count%3A\\([0-9]*\\)\\>" string)))
      (if countpos
          (string-to-number
           (substring string (match-beginning 1) (match-end 1)))
        (let ((kind
               (progn (string-match "^\\([a-z]*\\) " string)
                      (substring string (match-beginning 1) (match-end 1)))))
          (message "no count found in %s line" kind)
          nil)))))

(defun squeeze-parse-players-line (string)
  (let ((count (squeeze-parse-count string))
        (startpos (string-match "playerindex" string))
        result endpos)
    (when (> count 0)
      (dotimes (i (1- count))
        (setq endpos (progn (string-match " connected%3A[0-1] " string startpos)
                            (match-end 0)))
        (push (apply 'squeeze-make-player (squeeze-string-plistify string startpos endpos)) result)
        (setq startpos endpos))
      (push (apply 'squeeze-make-player (squeeze-string-plistify string startpos (length string))) result))
    result))

(defcustom squeeze-artwork-directory "~/.emacs.d/squeeze/artwork/"
  "Base directory for album and track artwork")

(defvar squeeze-albums nil)

(cl-defstruct (squeeze-album (:constructor squeeze-make-album))
  id album artwork_track_id artist index)

(defun squeeze-parse-albums-line (string)
  (let ((count (squeeze-parse-count string))
        (countpos (string-match "\\_<count%3a" string))
        (start-index-pos (progn (string-match "^albums \\([0-9]+\\)\\>" string)
                                (match-beginning 1)))
        index start end)
    (unless squeeze-albums
      (setq squeeze-albums (make-vector count nil)))
    (when start-index-pos
      (setq index (string-to-number (substring string start-index-pos)))
      (setq start (string-match "\\_<id%3a" string start-index-pos))
      (while start
        (setq end (string-match "\\_<\\(id%3a\\|count%3a\\)" string (1+ start)))
        (aset squeeze-albums index
              (apply 'squeeze-make-album
                     :index index
                     (squeeze-string-plistify string start end)))
        (incf index)
        (setq start (if (= end countpos) nil end))))))

(defun squeeze-get-albums ()
  (squeeze-send-string "albums 0 1000 tags:lja")
  (squeeze-accept-process-output))

(defvar squeeze-albums-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'squeeze-albums-load-album)
    map))

(define-derived-mode squeeze-albums-mode tabulated-list-mode "SqueezeAlbums"
  "Major mode for displaying Albums from Squeezebox Servers.\\<squeeze-albums-mode-map>"
  (setq tabulated-list-format [("Cover" 10 nil) ("Title" 30 t)])
  (add-hook 'tabulated-list-revert-hook 'squeeze-get-albums nil t)
  (setq tabulated-list-entries 'squeeze-albums-tabulated-list-entries)
  (tabbar-mode -1)
  (setq tabulated-list-use-header-line nil)
  (tabulated-list-init-header))

(defun squeeze-albums-artwork-callback (status artwork_track_id filename)
  (unless status
    (goto-char 1)
    (let ((end (search-forward "\n\n")))
      (delete-region 1 end)
      (write-file (format "%s%s/%s" squeeze-artwork-directory artwork_track_id filename))
      (kill-buffer))))

(defun squeeze-albums-tabulated-list-entry (x)
  (let* ((ati (squeeze-album-artwork_track_id x))
         (file (format "%s%s/cover_50x50_o.jpg" squeeze-artwork-directory ati)))
    (unless (file-exists-p file)
      (make-directory (format "%s%s" squeeze-artwork-directory ati) t)
      (let ((url (format "http://%s:%s/music/%s/cover_50x50_o.jpg"
                         squeeze-server-address squeeze-server-http-port ati)))
        (url-queue-retrieve url 'squeeze-albums-artwork-callback
                            (list ati "cover_50x50_o.jpg") t t)))
    (list x (vector (propertize (format "%s" ati)
                                'display `(when (file-exists-p ,file)
                                            image :type jpeg :file ,file))
                    (squeeze-album-album x)))))

(defun squeeze-albums-tabulated-list-entries ()
  (mapcar 'squeeze-albums-tabulated-list-entry (append squeeze-albums nil)))

(defun squeeze-complete-command-at-point ()
  (save-excursion
    (list (progn (backward-word) (point))
          (progn (forward-word) (point))
          (append
           (mapcar 'squeeze-player-playerid squeeze-players)
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
             )))))

(defun squeeze-read-server-parameters (address port)
  (let ((host (read-string "Host: " nil nil address))
        (port (read-number "Port: " port)))
    (cons host port)))

(defun squeeze (&optional address port)
  (interactive)
  (unless address (setq address squeeze-server-address))
  (unless port (setq port squeeze-server-port))
  (when current-prefix-arg
    (let ((parameters (squeeze-read-server-parameters address port)))
      (setq address (car parameters)
            port (cdr parameters))))
  (let ((buffer (make-comint-in-buffer "squeeze" nil (cons address port))))
    (switch-to-buffer buffer)
    (squeeze-mode)))

(defun squeeze-control (&optional address port)
  (interactive)
  (unless address (setq address squeeze-server-address))
  (unless port (setq port squeeze-server-port))
  (when current-prefix-arg
    (let ((parameters (squeeze-read-server-parameters address port)))
      (setq address (car parameters)
            port (cdr parameters))))
  (let ((current-prefix-arg nil))
    (squeeze address port))
  (let ((buffer (get-buffer-create "*squeeze-control*")))
    (switch-to-buffer buffer)
    (squeeze-control-listen)
    (squeeze-control-refresh)
    (squeeze-control-display-players)))

(defun squeeze-list-albums ()
  (interactive)
  (squeeze-get-albums)
  (let ((buffer (get-buffer-create "*squeeze-albums*")))
    (switch-to-buffer buffer)
    (squeeze-albums-mode)
    (tabulated-list-print)))

(defun squeeze-albums-load-album ()
  (interactive)
  (squeeze-send-string "%s playlistcontrol cmd:load album_id:%s"
                       squeeze-control-current-player
                       (squeeze-album-id (tabulated-list-get-id))))

(provide 'squeeze)
