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
  (add-hook 'comint-preoutput-filter-functions 'url-unhex-string nil t))

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
