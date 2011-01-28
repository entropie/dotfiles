(add-to-list 'load-path "~/.emacs.d/emms")

(setq emms-mode-line-icon-color "#d2691e")

(defvar mt-emms-mode-line-icon-stop
  `(image :type xpm :ascent center :data ,(concat "/* XPM */
static char *note[] = {
/* width height num_colors chars_per_pixel */
\"    10   11        2            1\",
/* colors */
\". c " emms-mode-line-icon-color  "\",
\"# c None s None\",
/* pixels */
\"##########\",
\"#........#\",
\"#........#\",
\"#........#\",
\"#........#\",
\"#........#\",
\"#........#\",
\"#........#\",
\"#........#\",
\"#........#\",
\"##########\"};")))

(defvar mt-emms-mode-line-icon-pause
  `(image :type xpm :ascent center :data ,(concat "/* XPM */
static char *note[] = {
/* width height num_colors chars_per_pixel */
\"    10   11        2            1\",
/* colors */
\". c " emms-mode-line-icon-color  "\",
\"# c None s None\",
/* pixels */
\"##########\",
\"##..##..##\",
\"##..##..##\",
\"##..##..##\",
\"##..##..##\",
\"##..##..##\",
\"##..##..##\",
\"##..##..##\",
\"##..##..##\",
\"##..##..##\",
\"##########\"};")))

(defvar mt-emms-mode-line-icon-start
  `(image :type xpm :ascent center :data ,(concat "/* XPM */
static char *note[] = {
/* width height num_colors chars_per_pixel */
\"    10   11        2            1\",
/* colors */
\". c " emms-mode-line-icon-color  "\",
\"# c None s None\",
/* pixels */
\"##########\",
\"##.#######\",
\"##..######\",
\"##...#####\",
\"##....####\",
\"##.....###\",
\"##....####\",
\"##...#####\",
\"##..######\",
\"##.#######\",
\"##########\"};")))

(defvar mt-emms-mode-line-icon-repeat-playlist
  `(image :type xpm :ascent center :data ,(concat "/* XPM */
static char *note[] = {
/* width height num_colors chars_per_pixel */
\"    10   11        2            1\",
/* colors */
\". c " emms-mode-line-icon-color  "\",
\"# c None s None\",
/* pixels */
\"###...####\",
\"#.......##\",
\"..#####..#\",
\"..######..\",
\"########..\",
\"########..\",
\"......##..\",
\"#....##..#\",
\"#........#\",
\"##.#.....#\",
\"##########\"};")))

(defvar mt-emms-mode-line-icon-repeat-track
  `(image :type xpm :ascent center :data ,(concat "/* XPM */
static char *note[] = {
/* width height num_colors chars_per_pixel */
\"    10   11        2            1\",
/* colors */
\". c " emms-mode-line-icon-color  "\",
\"# c None s None\",
/* pixels */
\"##########\",
\"##########\",
\"##########\",
\"##.####.##\",
\"#..####..#\",
\"..........\",
\"#..####..#\",
\"##.####.##\",
\"##########\",
\"##########\",
\"##########\"};")))


(defvar mt-emms-mode-line-icon-random
  `(image :type xpm :ascent center :data ,(concat "/* XPM */
static char *note[] = {
/* width height num_colors chars_per_pixel */
\"    10   11        2            1\",
/* colors */
\". c " emms-mode-line-icon-color  "\",
\"# c None s None\",
/* pixels */
\"###....###\",
\"##..##..##\",
\"#..####..#\",
\"#.#####..#\",
\"######..##\",
\"#####..###\",
\"####..####\",
\"####..####\",
\"##########\",
\"####..####\",
\"####..####\"};")))
;;}}}


(require 'emms-setup)
(require 'emms-info)
(require 'emms-info-libtag)
(require 'emms-info-mp3info)
(require 'emms-info-ogg)
(require 'emms-browser)
(require 'emms-player-mpd)
(require 'emms-player-mplayer)
(require 'emms-mode-line)
(require 'emms-mode-line-icon)
(require 'emms-playing-time)

(emms-devel)

(setq emms-info-functions '(emms-info-libtag))

(require 'url-util)
(defun mt-emms-info-file-name (track)
  "Use the file name as the track information. Use parent
directory name as artist and current file name as title. Add this
to the end of `emms-info-functions' for not rewrite exists
information when needed."
  (when (or (eq (emms-track-type track) 'file)
            (eq (emms-track-type track) 'url))
    (let ((name (url-unhex-string (emms-track-name track)))
          (artist (emms-track-get track 'info-artist))
          (title (emms-track-get track 'info-title)))
      (if (and (eq (emms-track-type track) 'url)
               (emms-track-get track 'coding))
          (setq name (decode-coding-string name (emms-track-get track 'coding))))
      (when (or (null artist)
                (string= artist "")
                (string-match "unknown" artist))
        (emms-track-set track 'info-artist
                        (file-name-nondirectory
                         (substring (file-name-directory name) 0 -1))))
      (when (or (null title)
                (string= title ""))
        (emms-track-set track 'info-title
                        (file-name-sans-extension
                         (file-name-nondirectory name)))))))
(setq emms-info-functions (delq 'emms-info-ogginfo emms-info-functions))
(add-to-list 'emms-info-functions 'mt-emms-info-file-name t)

(defun emms-mode-line-alter-titlebar ()
  "Update the titlebar with song info."
  (when emms-mode-line-titlebar-function
    (setq frame-title-format
      (list "" emms-mode-line-initial-titlebar
                " [" (funcall emms-mode-line-titlebar-function) " ]"))))

(defun mt-turn-off-covers-for-singles ()
  (if (string= emms-browser-current-filter-name "Singles")
      (setq emms-browser-covers nil)
    (setq emms-browser-covers 'my-emms-browser-covers)))

(defun mt-toggle-album-display ()
  (if (string= emms-browser-current-filter-name "Singles")
      (ad-activate 'emms-browser-next-mapping-type)
    (ad-deactivate 'emms-browser-next-mapping-type)))

(add-hook 'emms-browser-filter-changed-hook 'mt-turn-off-covers-for-singles)
(add-hook 'emms-browser-filter-changed-hook 'mt-toggle-album-display)

(defun mt-emms-mode-line-icon-function ()
  (concat " "
          emms-mode-line-icon-before-format
          (if emms-player-playing-p
              (if emms-player-paused-p
                  (propertize "P" 'display mt-emms-mode-line-icon-pause)
                (propertize "R" 'display mt-emms-mode-line-icon-start))
            (propertize "S" 'display mt-emms-mode-line-icon-stop))
          (emms-propertize "N" 'display emms-mode-line-icon-image-cache)
          (emms-mode-line-playlist-current)))

(setq emms-mode-line-mode-line-function 'mt-emms-mode-line-icon-function)

;;(setq emms-player-mplayer-command-name "/home/mit/bin/2mplayer")
(setq emms-player-mplayer-command-name "mplayer")
(setq emms-player-mplayer-parameters '("-slave" "-quiet" "-really-quiet" "-ao" "alsa:noblock:device=hw=0.0"))

(setq emms-mode-line-titlebar-function 'emms-mode-line-playlist-current)
(setq emms-stream-info-format-string "NS: %s")
(setq emms-stream-default-action "play")

(defun mt-emms-insert ()
  "Insert a Playing MPEG stream from ... string."
  (interactive)
  (emms-show 'insert))

(defun mt-emms-track-description (track)
  "Return a nice description of TRACK."
  (let ((desc (emms-track-simple-description track)))
    (if (string-match "^/home/ftp/pub/\\(.*\\)/\\(.*\\)" desc)
        (match-string 2 desc)
      desc)))

(emms-standard)
(emms-default-players)
(emms-mode-line-enable)
(emms-playing-time 1)

(setq emms-player-list (quote (emms-player-mplayer-playlist emms-player-mplayer)))

(setq emms-info-asynchronously t)
(setq emms-source-file-default-directory "/home/ftp/pub/music/")
(setq emms-mode-line-format " [ %s ]")
(setq emms-track-description-function 'mt-emms-track-description)

(add-hook 'emms-player-started-hook 'emms-show)

(setq emms-show-format "NP: %s")

(global-set-key (kbd "C-c m") 'nil)
(global-set-key (kbd "C-c m w") 'emms-show)
(global-set-key (kbd "C-c m n") 'emms-next)
(global-set-key (kbd "C-c m p") 'emms-previous)
(global-set-key (kbd "C-c m s") 'emms-stop)
(global-set-key (kbd "C-c m r") 'emms-random)
(global-set-key (kbd "C-c m t") 'mt-toggle-playing)
(global-set-key (kbd "C-c m P") 'emms-playlist-mode-go)
(global-set-key (kbd "C-c m m") 'emms-mode-line-toggle)


(define-key emms-playlist-mode-map (kbd "/") 'mt-search)
(add-hook 'emms-playlist-selection-changed-hook 'mt-focus-on-track)


(defadvice emms-pause (after mt-emms-update-mode-line)
  (emms-mode-line-alter)
  (force-mode-line-update))
(ad-activate 'emms-pause)


;; recenter based on the current track
(defun mt-focus-on-track ()
  (let ((w (get-buffer-window emms-playlist-buffer t)))
    (when w
      (with-selected-window w
        (emms-playlist-mode-center-current)
        (recenter '(4))))))

(defun mt-toggle-playing ()
  (interactive)
  (if emms-player-playing-p
      (emms-pause)
    (emms-start)))

(defun mt-add-dir ()
  (interactive)
  (call-interactively 'emms-add-directory-tree)
  (emms-playlist-mode-go))

(defun mt-playlist-go ()
  (interactive)
  (if (eq major-mode 'emms-playlist-mode)
      (bury-buffer)
    (emms-playlist-mode-go)))

(defun mt-search ()
  (interactive)
  (goto-char (point-min))
  (call-interactively 'isearch-forward))


(provide 'mt-emms)
