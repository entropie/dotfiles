;;; mpc.el --- A client for the Music Player Daemon

;; Copyright (C) 2006  Stefan Monnier

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords: multimedia

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This is an Emacs front end to the Music Player Daemon.

;; It mostly provides a browser inspired from Rhythmbox for your music
;; collection and also allows you to play the music you select.  The basic
;; interface is somewhat unusual in that it does not provide direct access
;; to MusicPD's playlist functionality.  Instead it uses MusicPD's playlist
;; internally to implement a queue of songs.  This means that it does not
;; always interact nicely with other clients that you may be using at the
;; same time.

;; I play albums rather than songs and thus don't have much need for
;; playlists, and it shows.  Playlist support exists, but is embryonic.

;; Bugs:

;; - when reaching end/start of song while ffwd/rewind, it may get wedged,
;;   signal an error, ... or when mpc-next/prev is called while ffwd/rewind.
;; - MPD errors are not reported to the user.
;; - The song's header-line doesn't slide with the text which H-scrolling.
;; - No way to create/modify playlists.
;; - how to deal with sorting and playlists: when browsing based on normal
;;   attributes, sorting should be done based on artist/album/... but when
;;   a playlist is selected, the sorting order should be the one of the
;;   playlist (and duplicates are possible).

;; Todo:

;; - check what happens if the playlist file holds absolute file names.
;; - deal with people who don't have tool-bars.
;; - even if nothing's happening, poll every once in a while, in case some
;;   other client started playback.
;; - at startup, if something's playing, select it in the mpc-song buffer.
;; - clicking on the album/author in mpc-status should select it.
;; - look for .folder.png (freedesktop) or folder.jpg (XP) as well.
;; - speed up ffwd/rewind progressively.
;; - fetch album covers and lyrics from the web.
;; - a mpc-status buffer which shows the volume button, the current song,
;;   and the cover picture.  Plus a way to show only this buffer (use it
;;   as a control-buffer from which you can show/hide the browser).
;; - improve mpc-song-mode's header-line column-headings so they can be
;;   dragged to resize.
;; - allow selecting several entries by drag-mouse.
;; - poll less often
;;   - do the time-ticking locally (and sync every once in a while)
;;   - look at the end of play time to make sure we notice the end
;;     as soon as possible
;; - do something more useful with the songs buffer.
;; - queue management (probably a queue buffer).
;; - better volume widget.
;; - add synthesized tags.
;;   e.g. pseudo-artist = artist + composer + performer.
;;   e.g. rewrite artist "Foo bar & baz" to "Foo bar".
;;   e.g. filename regexp -> compilation flag
;; - add hierarchy in tagbrowsers.  Then add tagbrowser for pseudo tag `file'.
;;   These will create problems in mpc-reorder since it's not clear
;;   how the sorting should be adjusted to work on a tree rather than
;;   a list (when the tree's leaves are split into the active and the
;;   non-active ones).
;; - window/buffer management.
;; - menubar, tooltips, ...
;; - add mpc-describe-song, mpc-describe-album, ...
;; - add import/export commands (especially export to an MP3 player).
;; - add a real notion of album (as opposed to just album-name):
;;   if all songs with same album-name have same artist -> it's an album
;;   else it's either several albums or a compilation album (or both),
;;   in which case we could use heuristics or user provided info:
;;   - if the user followed the 1-album = 1-dir idea, then we can group songs
;;     by their directory to create albums.
;;   - if a `compilation' flag is available, and if <=1 of the songs have it
;;     set, then we can group songs by their artist to create albums.
;;   - if two songs have the same track-nb and disk-nb, they're not in the
;;     same album.  So from the set of songs with identical album names, we
;;     can get a lower bound on the number of albums involved, and then see
;;     which of those may be non-compilations, etc...
;;   - use a special directory name for compilations.
;;   - ask the web ;-)

;;; Code:

;; Prefixes used in this code:
;; mpc-proc   : management of connection (in/out formatting, ...)
;; mpc-status : auto-updated status info
;; mpc-volume : stuff handling the volume widget
;; mpc-cmd    : mpdlib abstraction

;; UI-commands       : mpc-
;; internal          : mpc--

(require 'cl)

;;; Backward compatibility.
;; This code is meant for Emacs-CVS, so to get it to run on anything else,
;; we need to define some more things.

(unless (fboundp 'tool-bar-local-item)
  (defun tool-bar-local-item (icon def key map &rest props)
    (define-key-after map (vector key)
      `(menu-item ,(symbol-name key) ,def
                  :image ,(find-image
                           `((:type xpm :file ,(concat icon ".xpm"))))
                  ,@props))))

(unless (fboundp 'process-put)
  (defconst mpc-process-hash (make-hash-table :weakness 'key))
  (defun process-put (proc prop val)
    (let ((sym (gethash proc mpc-process-hash)))
      (unless sym
        (setq sym (puthash proc (make-symbol "mpc-proc-sym") mpc-process-hash)))
      (put sym prop val)))
  (defun process-get (proc prop)
    (let ((sym (gethash proc mpc-process-hash)))
      (when sym (get sym prop))))
  (defun process-plist (proc)
    (let ((sym (gethash proc mpc-process-hash)))
      (when sym (symbol-plist sym)))))
(unless (fboundp 'with-local-quit)
  (defmacro with-local-quit (&rest body)
    `(condition-case nil (let ((inhibit-quit nil)) ,@body)
       (quit (setq quit-flag t) nil))))
(unless (fboundp 'balance-windows-area)
  (defalias 'balance-windows-area 'balance-windows))
(unless (fboundp 'posn-object) (defalias 'posn-object 'ignore))
(unless (fboundp 'buffer-local-value)
  (defun buffer-local-value (var buf)
    (with-current-buffer buf (symbol-value var))))
      

;;; Main code starts here.

(defgroup mpc ()
  "A Client for the Music Player Daemon."
  :prefix "mpc-"
  :group 'multimedia
  :group 'applications)

(defcustom mpc-browser-tags '("genre" "artist" "album" "playlist")
  "Tags for which a browser buffer should be created by default."
  :type '(repeat string))

;;; Misc utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mpc-assq-all (key alist)
  (let ((res ()) val)
    (dolist (elem alist)
      (if (and (eq (car elem) key)
               (not (member (setq val (cdr elem)) res)))
          (push val res)))
    (nreverse res)))
  
(defun mpc-union (&rest lists)
  (let ((res (nreverse (pop lists))))
    (dolist (list lists)
      (let ((seen res))           ;Don't remove duplicates within each list.
        (dolist (elem list)
          (unless (member elem seen) (push elem res)))))
    (nreverse res)))

(defun mpc-intersection (l1 l2 &optional selectfun)
  "Return L1 after removing all elements not found in L2.
SELECTFUN if non-nil elements aren't compared directly, but instead they
are passed through SELECTFUN before comparison."
  (let ((res ()))
    (if selectfun (setq l2 (mapcar selectfun l2)))
    (dolist (elem l1)
      (when (member (if selectfun (funcall selectfun elem) elem) l2)
        (push elem res)))
    (nreverse res)))

(defun mpc-event-set-point (event)
  (condition-case nil (posn-set-point (event-end event))
    (error (condition-case nil (mouse-set-point event)
             (error nil)))))

(defun mpc-compare-strings (str1 str2 &optional ignore-case)
  "Compare strings STR1 and STR2.
Contrary to `compare-strings', this tries to get numbers sorted
numerically rather than lexicographically."
  (let ((res (compare-strings str1 nil nil str2 nil nil ignore-case)))
    (if (not (integerp res)) res
      (let ((index (1- (abs res))))
        (if (or (>= index (length str1)) (>= index (length str2)))
            res
          (let ((digit1 (memq (aref str1 index)
                              '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)))
                (digit2 (memq (aref str2 index)
                              '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))))
            (if digit1
                (if digit2
                    (if (< (truncate (string-to-number (substring str1 index)))
                           (truncate (string-to-number (substring str2 index))))
                        (- (abs res))
                      (abs res))
                  (- (abs res)))
              (if digit2 (abs res) res))))))))

;;; Support for the actual connection and MPD command execution ;;;;;;;;;;;;

(defcustom mpc-host
  (concat (or (getenv "MPD_HOST") "localhost")
          (if (getenv "MPD_PORT") (concat ":" (getenv "MPD_PORT"))))
  "Host (and port) where the Music Player Daemon is running.
The format is \"HOST\" or \"HOST:PORT\" where PORT defaults to 6600
and HOST default to localhost."
  :type 'string)

(defvar mpc-proc nil)

(defconst mpc--proc-end-re "^\\(?:OK\\(?: MPD .*\\)?\\|ACK \\(.*\\)\\)\n")

(put 'mpc-proc-error 'error-conditions '(mpc-proc-error error))
(put 'mpc-proc-error 'error-message "MPD error")

(defun mpc--debug (format &rest args)
  (if (get-buffer "*MPC-debug*")
      (with-current-buffer "*MPC-debug*"
        (goto-char (point-max))
        (insert-before-markers          ;So it scrolls.
         (replace-regexp-in-string "\n" "\n	"
                                   (apply 'format format args))
         "\n"))))

(defun mpc--proc-filter (proc string)
  (mpc--debug "Receive \"%s\"" string)
  (with-current-buffer (process-buffer proc)
    (if (process-get proc 'ready)
        (if nil ;; (string-match "\\`\\(OK\n\\)+\\'" string)
            ;; I haven't figured out yet why I get those extraneous OKs,
            ;; so I'll just ignore them for now.
            nil
          (delete-process proc)
          (set-process-buffer proc nil)
          (pop-to-buffer (clone-buffer))
          (error "MPD output while idle!?"))
      (save-excursion
        (let ((start (or (marker-position (process-mark proc)) (point-min))))
          (goto-char start)
          (insert string)
          (move-marker (process-mark proc) (point))
          (beginning-of-line)
          (when (and (< start (point))
                     (re-search-backward mpc--proc-end-re start t))
            (process-put proc 'ready t)
            (unless (eq (match-end 0) (point-max))
              (error "Unexpected trailing text"))
            (let ((error (match-string 1)))
              (delete-region (point) (point-max))
              (let ((callback (process-get proc 'callback)))
                (process-put proc 'callback nil)
                (if error (signal 'mpc-proc-error error))
                (funcall callback)))))))))

(defun mpc--proc-connect (host)
  (with-current-buffer (get-buffer-create " *mpc*")
    ;; (pop-to-buffer (current-buffer))
    (let (proc)
      (while (and (setq proc (get-buffer-process (current-buffer)))
                  (progn (debug) (delete-process proc)))))
    (erase-buffer)
    (let ((port 6600))
      (when (string-match ":[^.]+\\'" host)
        (setq port (substring host (1+ (match-beginning 0))))
        (setq host (substring host 0 (match-beginning 0)))
        (unless (string-match "[^[:digit:]]" port)
          (setq port (string-to-number port))))
      (let* ((coding-system-for-read 'utf-8-unix)
             (coding-system-for-write 'utf-8-unix)
             (proc (open-network-stream "MPC" (current-buffer) host port)))
        (when (processp mpc-proc)
          ;; Inherit the properties of the previous connection.
          (let ((plist (process-plist mpc-proc)))
            (while plist (process-put proc (pop plist) (pop plist)))))
        (mpc-proc-buffer proc 'mpd-commands (current-buffer))
        (process-put proc 'callback 'ignore)
        (process-put proc 'ready nil)
        (set-process-filter proc 'mpc--proc-filter)
        (set-process-sentinel proc 'ignore)
        (set-process-query-on-exit-flag proc nil)
        (mpc-proc-sync proc)
        proc))))

(defun mpc--proc-quote-string (s)
  (if (numberp s) (number-to-string s)
    (setq s (replace-regexp-in-string "[\"\\]" "\\\\\\&" s))
    (if (string-match " " s) (concat "\"" s "\"") s)))

(defun mpc--proc-alist-to-alists (alist)
  (let ((starter (caar alist))
        (alists ())
        tmp)
    (dolist (pair alist)
      (when (eq (car pair) starter)
        (if tmp (push (nreverse tmp) alists))
        (setq tmp ()))
      (push pair tmp))
    (if tmp (push (nreverse tmp) alists))
    (nreverse alists)))

(defun mpc-proc ()
  (or (and mpc-proc
           (buffer-live-p (process-buffer mpc-proc))
           (not (memq (process-status mpc-proc) '(closed)))
           mpc-proc)
      (setq mpc-proc (mpc--proc-connect mpc-host))))

(defun mpc-proc-sync (&optional proc)
  (unless proc (setq proc (mpc-proc)))
  (unwind-protect
      (progn
        (while (and (not (process-get proc 'ready))
                    (accept-process-output proc)))
        (if (process-get proc 'ready) (process-buffer proc)
          ;; (delete-process proc)
          (error "No response from MPD")))
    (unless (process-get proc 'ready)
      (message "Killing hung process")
      (delete-process proc))))

(defun mpc-proc-cmd (cmd &optional callback)
  (let ((proc (mpc-proc)))
    ;; Wait for any pending async command to terminate.
    (mpc-proc-sync proc)
    (process-put proc 'ready nil)
    (with-current-buffer (process-buffer proc)
      (erase-buffer)
      (mpc--debug "send \"%s\"" cmd)
      (process-send-string
       proc (concat (if (stringp cmd) cmd
                      (mapconcat 'mpc--proc-quote-string cmd " "))
                    "\n"))
      (process-put proc 'callback (or callback 'ignore)))
    ;; If `callback' is nil, we're executing synchronously.
    (unless callback (mpc-proc-sync proc))))

;; This function doesn't exist in Emacs-21.
;; (put 'mpc-proc-cmd-list 'byte-optimizer 'byte-optimize-pure-func)
(defun mpc-proc-cmd-list (cmds)
  (concat "command_list_begin\n"
          (mapconcat (lambda (cmd)
                       (if (stringp cmd) cmd
                         (mapconcat 'mpc--proc-quote-string cmd " ")))
                     cmds
                     "\n")
          "\ncommand_list_end"))

(defun mpc-proc-cmd-list-ok ()
  ;; To implement this, we'll need to tweak the process filter since we'd
  ;; then sometimes get "trailing" text after "OK\n".
  (error "Not implemented yet"))

(defun mpc-proc-buf-to-alist (&optional buf)
  (with-current-buffer (or buf (current-buffer))
    (let ((res ()))
      (goto-char (point-min))
      (while (re-search-forward "^\\([^:]+\\): \\(.*\\)\n" nil t)
        (push (cons (intern (match-string 1)) (match-string 2)) res))
      (nreverse res))))

(defun mpc-proc-buf-to-alists (buf)
  (mpc--proc-alist-to-alists (mpc-proc-buf-to-alist buf)))

(defun mpc-proc-cmd-to-alist (cmd)
  (mpc-proc-buf-to-alist (mpc-proc-cmd cmd)))

(defun mpc-proc-tag-string-to-sym (tag)
  (intern (capitalize tag)))

(defun mpc-proc-buffer (proc use &optional buffer)
  (let* ((bufs (process-get proc 'buffers))
         (buf (cdr (assoc use bufs))))
    (cond
     ((and buffer (buffer-live-p buf) (not (eq buffer buf)))
      (error "Duplicate MPC buffer for %s" use))
     (buffer
      (if buf
          (setcdr (assoc use bufs) buffer)
        (process-put proc 'buffers (cons (cons use buffer) bufs))))
     (t buf))))

;;; Support for regularly updated current status information ;;;;;;;;;;;;;;;

;; Exported elements:
;; `mpc-status' holds the uptodate data.
;; `mpc-status-callbacks' holds the registered callback functions.
;; `mpc-status-refresh' forces a refresh of the data.
;; `mpc-status-stop' stops the automatic updating.

(defvar mpc-status nil)
(defvar mpc-status-callbacks
  '((state  . mpc--status-timers-refresh)
    ;; (song   . mpc--queue-refresh)
    ;; (state  . mpc--queue-refresh)       ;To detect the end of the last song.
    (state  . mpc--faster-toggle-refresh) ;Only ffwd/rewind while play/pause.
    (volume . mpc-volume-refresh)
    (file   . mpc-songpointer-refresh)
    (updating_db . mpc-updated-db)
    (updating_db . mpc--status-timers-refresh)
    (t      . mpc-currentsong-refresh))
  "Alist associating properties to the functions that care about them.
Each entry has the form (PROP . FUN) where PROP can be t to mean
to call FUN for any change whatsoever.")

(defun mpc--status-callback ()
  (let ((old-status mpc-status))
    ;; Update the alist.
    (setq mpc-status (mpc-proc-buf-to-alist))
    (assert mpc-status)
    (unless (equal old-status mpc-status)
      ;; Run the relevant refresher functions.
      (dolist (pair mpc-status-callbacks)
        (when (or (eq t (car pair))
                  (not (equal (cdr (assq (car pair) old-status))
                              (cdr (assq (car pair) mpc-status)))))
          (funcall (cdr pair)))))))

(defvar mpc--status-timer nil)
(defun mpc--status-timer-start ()
  (add-hook 'pre-command-hook 'mpc--status-timer-stop)
  (unless mpc--status-timer
    (setq mpc--status-timer (run-with-timer 1 1 'mpc--status-timer-run))))
(defun mpc--status-timer-stop ()
  (when mpc--status-timer
    (cancel-timer mpc--status-timer)
    (setq mpc--status-timer nil)))
(defun mpc--status-timer-run ()
  (when (process-get (mpc-proc) 'ready)
    (condition-case nil
        (with-local-quit (mpc-status-refresh))
      (error nil))))

(defvar mpc--status-idle-timer nil)
(defun mpc--status-idle-timer-start ()
  (unless mpc--status-idle-timer
    (setq mpc--status-idle-timer
          (run-with-idle-timer 1 t 'mpc--status-idle-timer-run))
    ;; Typically, the idle timer is started from the mpc--status-callback,
    ;; which is run asynchronously while we're already idle (we typically
    ;; just started idling), so the timer itself will only be run the next
    ;; time we idle :-(
    ;; To work around that, we immediately start the repeat timer.
    (mpc--status-timer-start)))
(defun mpc--status-idle-timer-stop ()
  (when mpc--status-idle-timer
    (cancel-timer mpc--status-idle-timer)
    (setq mpc--status-idle-timer nil)))
(defun mpc--status-idle-timer-run ()
  (when (process-get (mpc-proc) 'ready)
    (condition-case nil
        (with-local-quit (mpc-status-refresh))
      (error nil)))
  (mpc--status-timer-start))

(defun mpc--status-timers-refresh ()
  "Start/stop the timers according to whether a song is playing."
  (if (or (member (cdr (assq 'state mpc-status)) '("play"))
          (cdr (assq 'updating_db mpc-status)))
      (mpc--status-idle-timer-start)
    (mpc--status-idle-timer-stop)
    (mpc--status-timer-stop)))

(defun mpc-status-refresh (&optional callback)
  "Refresh `mpc-status'."
  (lexical-let ((cb callback))
    (mpc-proc-cmd (mpc-proc-cmd-list '("status" "currentsong"))
                  (lambda ()
                    (mpc--status-callback)
                    (if cb (funcall cb))))))

(defun mpc-status-stop ()
  "Stop the autorefresh of `mpc-status'.
Any call to `mpc-status-refresh' may cause it to be restarted."
  (mpc--status-idle-timer-stop)
  (mpc--status-timer-stop))

;;; A thin layer above the raw protocol commands ;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defvar mpc-queue nil)
;; (defvar mpc-queue-back nil)

;; (defun mpc--queue-head ()
;;   (if (stringp (car mpc-queue)) (car mpc-queue) (cadar mpc-queue)))
;; (defun mpc--queue-pop ()
;;   (when mpc-queue                       ;Can be nil if out of sync.
;;     (let ((song (car mpc-queue)))
;;       (assert song)
;;       (push (if (and (consp song) (cddr song))
;;                 ;; The queue's first element is itself a list of
;;                 ;; songs, where the first element isn't itself a song
;;                 ;; but a description of the list.
;;                 (prog1 (cadr song) (setcdr song (cddr song)))
;;               (prog1 (if (consp song) (cadr song) song)
;;                 (setq mpc-queue (cdr mpc-queue))))
;;             mpc-queue-back)
;;       (assert (stringp (car mpc-queue-back))))))

;; (defun mpc--queue-refresh ()
;;   ;; Maintain the queue.
;;   (mpc--debug "mpc--queue-refresh")
;;   (let ((pos (cdr (or (assq 'Pos mpc-status) (assq 'song mpc-status)))))
;;     (cond
;;      ((null pos)
;;       (mpc-cmd-clear 'ignore))
;;      ((or (not (member pos '("0" nil)))
;;           ;; There's only one song in the playlist and we've stopped.
;;           ;; Maybe it's because of some external client that set the
;;           ;; playlist like that and/or manually stopped the playback, but
;;           ;; it's more likely that we've simply reached the end of
;;           ;; the song.  So remove it.
;;           (and (equal (assq 'state mpc-status) "stop")
;;                (equal (assq 'playlistlength mpc-status) "1")
;;                (setq pos "1")))
;;       ;; We're not playing the first song in the queue/playlist any
;;       ;; more, so update the queue.
;;       (dotimes (i (string-to-number pos)) (mpc--queue-pop))
;;       (mpc-proc-cmd (mpc-proc-cmd-list
;;                      (make-list (string-to-number pos) "delete 0"))
;;                     'ignore)
;;       (if (not (equal (cdr (assq 'file mpc-status))
;;                       (mpc--queue-head)))
;;           (message "MPC's queue is out of sync"))))))

(defun mpc-cmd-find (tag value)
  (cond
   ((equal tag "playlist")
    ;; Special case for pseudo-tag playlist.
    (condition-case err
        ;; (let* ((playlistfile (concat "/playlist/" value ".m3u"))
        ;;        (files (with-current-buffer
        ;;                   (mpc-cmd-download playlistfile)
        ;;                 (prog1 (split-string (buffer-string) "\n" t)
        ;;                   (kill-buffer (current-buffer)))))
        ;;        (cmds (mapcar (lambda (file) (list "listallinfo" file))
        ;;                      files)))
        ;;   (mpc-proc-buf-to-alists (mpc-proc-cmd (mpc-proc-cmd-list cmds))))
        (signal 'mpc-proc-error nil)
      ;; `download' is a local hack.
      (mpc-proc-error
       (let ((playlistlen (string-to-number (cdr (assq 'playlistlength (mpc-cmd-status)))))
             (cmds ()))
         (mpc-proc-cmd (list "load" value)) ;Append to the current playlist.
         (dotimes (i (- (string-to-number (cdr (assq 'playlistlength (mpc-cmd-status)))) playlistlen))
           (push (list "delete" playlistlen) cmds)
           (push (list "playlistinfo" playlistlen) cmds))
         (mpc-proc-buf-to-alists
          (mpc-proc-cmd (mpc-proc-cmd-list (cons (list "load" value)
                                                 cmds))))))))
   (t
    (condition-case err
        (mpc-proc-buf-to-alists (mpc-proc-cmd (list "find" tag value)))
      (mpc-proc-error
       ;; If `tag' is not one of the expected tags, MPD burps about not
       ;; having the relevant table.
       ;; FIXME: check the kind of error.
       (let ((res ()))
         (setq tag (mpc-proc-tag-string-to-sym tag))
         (setq value (cons tag value))
         (dolist (song (mpc-proc-buf-to-alists (mpc-proc-cmd "listallinfo")))
           (if (member value song) (push song res)))
         res))))))

(defun mpc-cmd-list (tag &optional other-tag value)
  (if (equal tag "playlist")
      ;; If `other-tag' is specified, we'll have to do more work.  For now,
      ;; just return everything, as if `other-tag' wasn't specified.
      (mpc-assq-all 'playlist (mpc-proc-cmd-to-alist "lsinfo"))
    (if (null other-tag)
        (condition-case nil
            (mapcar 'cdr (mpc-proc-cmd-to-alist (list "list" tag)))
          (mpc-proc-error
           ;; If `tag' is not one of the expected tags, MPD burps about not
           ;; having the relevant table.
           ;; FIXME: check the kind of error.
           (mpc-assq-all (mpc-proc-tag-string-to-sym tag)
                         (mpc-proc-cmd-to-alist "listallinfo"))))
      (condition-case nil
          (mapcar 'cdr (mpc-proc-cmd-to-alist (list "list" tag other-tag value)))
        (mpc-proc-error
         ;; DAMN!! the 3-arg form of `list' is new in 0.12 !!
         ;; FIXME: check the kind of error.
         (let ((other-songs (mpc-cmd-find other-tag value)))
           (mpc-assq-all (mpc-proc-tag-string-to-sym tag)
                         (apply 'nconc other-songs))))))))

(defun mpc-cmd-stop (&optional callback)
  (mpc-proc-cmd "stop" callback))

(defun mpc-cmd-clear (&optional callback)
  (mpc-proc-cmd "clear" callback)
  ;; (setq mpc-queue-back nil mpc-queue nil)
  )

(defun mpc-cmd-pause (&optional arg callback)
  "Pause or resume playback of the queue of songs."
  (lexical-let ((cb callback))
    (mpc-proc-cmd (list "pause" arg)
                  (lambda () (mpc-status-refresh) (if cb (funcall cb))))
    (unless callback (mpc-proc-sync))))

(defun mpc-cmd-status ()
  (mpc-proc-cmd-to-alist "status"))

(defun mpc-cmd-play ()
  (mpc-proc-cmd "play")
  (mpc-status-refresh))

(defun mpc-cmd-add (files &optional explanation)
  ;; (push (cons explanation files) mpc-queue)
  (mpc-proc-cmd (mpc-proc-cmd-list
                 (mapcar (lambda (file) (list "add" file)) files))))

(defun mpc-cmd-update (&optional arg callback)
  (lexical-let ((cb callback))
    (mpc-proc-cmd (if arg (list "update" arg) "update")
                  (lambda () (mpc-status-refresh) (if cb (funcall cb))))
    (unless callback (mpc-proc-sync))))

(defun mpc-cmd-download (file)
  (with-current-buffer (let ((default-enable-multibyte-characters nil))
                         (generate-new-buffer " *mpc download*"))
    (let* ((proc (mpc-proc))
           (stdbuf (process-buffer proc))
           (markpos (marker-position (process-mark proc)))
           (stdcoding (process-coding-system proc)))
      (unwind-protect
          (progn
            (set-process-buffer proc (current-buffer))
            (set-process-coding-system proc 'binary (cdr stdcoding))
            (set-marker (process-mark proc) (point))
            (mpc-proc-cmd (list "download" file)))
        (set-process-buffer proc stdbuf)
        (set-marker (process-mark proc) markpos stdbuf)
        (set-process-coding-system proc (car stdcoding) (cdr stdcoding)))
      ;; The command has completed, let's decode.
      (goto-char (point-max))
      (delete-char -1)                    ;Delete final newline.
      (while (re-search-backward "^>" nil t)
        (delete-char 1))
      (current-buffer))))

;;; Misc ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom mpc-mpd-music-directory nil
  "Location of MPD's music directory."
  :type '(choice (const nil) directory))

(defcustom mpc-data-directory
  (if (and (not (file-directory-p "~/.mpc"))
           (file-directory-p "~/.emacs.d"))
      "~/.emacs.d/mpc" "~/.mpc")
  "Directory where MPC.el stores auxiliary data."
  :type 'directory)

(defun mpc-data-directory ()
  (unless (file-directory-p mpc-data-directory)
    (make-directory mpc-data-directory))
  mpc-data-directory)

(defun mpc-file-local-copy (file)
  (if (and mpc-mpd-music-directory
           (file-exists-p (expand-file-name file mpc-mpd-music-directory)))
      (expand-file-name file mpc-mpd-music-directory)
    (let ((aux (expand-file-name (replace-regexp-in-string "[/]" "|" file)
                                 (mpc-data-directory))))
      (unless (file-exists-p aux)
        (condition-case nil
            (with-current-buffer (mpc-cmd-download file)
              (write-region (point-min) (point-max) aux))
          (mpc-proc-error (setq aux nil))))
      aux)))

;;; Formatter ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mpc-secs-to-time (secstr)
  (let ((secs (string-to-number secstr)))
    (format "%d:%02d" (/ secs 60) (% secs 60))))

(defvar mpc-tempfiles nil)
(defconst mpc-tempfiles-reftable (make-hash-table :weakness 'key))

(defun mpc-tempfiles-clean ()
  (let ((live ()))
    (maphash (lambda (k v) (push v live)) mpc-tempfiles-reftable)
    (dolist (f mpc-tempfiles)
      (unless (member f live) (ignore-errors (delete-file f))))
    (setq mpc-tempfiles live)))

(defun mpc-tempfiles-add (key file)
  (mpc-tempfiles-clean)
  (puthash key file mpc-tempfiles-reftable)
  (push file mpc-tempfiles))

(defun mpc-format (format-spec info)
  (let ((pos 0)
        (start (point))
        (col 0)
        (pred nil))
    (while (string-match "%\\(?:%\\|\\(-\\)?\\([0-9]+\\)?{\\([[:alpha:]][[:alnum:]]*\\)\\(?:-\\([^}]+\\)\\)?}\\)" format-spec pos)
      (let ((pre-text (substring format-spec pos (match-beginning 0))))
        (insert pre-text)
        (setq col (+ col (string-width pre-text))))
      (setq pos (match-end 0))
      (if (null (match-end 3)) (insert "%")
        (let* ((size (match-string 2 format-spec))
               (tag (intern (match-string 3 format-spec)))
               (post (match-string 4 format-spec))
               (right-align (match-end 1))
               (text
                (if (eq info 'self) (symbol-name tag)
                  (case tag
                    ((Time Duration)
                     (let ((time (cdr (or (assq 'time info) (assq 'Time info)))))
                       (setq pred (list nil)) ;Just assume it's never eq.
                       (when time
                         (mpc-secs-to-time (if (and (eq tag 'Duration)
                                                    (string-match ":" time))
                                               (substring time (match-end 0))
                                             time)))))
                    (Cover
                     (let* ((dir (file-name-directory (cdr (assq 'file info))))
                            (cover (concat dir "cover.jpg"))
                            (file (ignore-errors (mpc-file-local-copy cover)))
                            image)
                       (push `(equal ',dir (file-name-directory (cdr (assq 'file info)))) pred)
                       (if (null file)
                           ;; Make sure we return something on which we can
                           ;; place the `mpc-pred' property, as
                           ;; a negative-cache.  We could also use
                           ;; a default cover.
                           (progn (setq size nil) " ")
                         (if (null size) (setq image (create-image file))
                           (let ((tempfile (make-temp-file "mpc" nil ".jpg")))
                             (call-process "convert" nil nil nil
                                           "-scale" size file tempfile)
                             (setq image (create-image tempfile))
                             (mpc-tempfiles-add image tempfile)))
                         (setq size nil)
                         (propertize dir 'display image))))
                    (t (let ((val (cdr (assq tag info))))
                         (push `(equal ',val (cdr (assq ',tag info))) pred)
                         val)))))
               (space (when size
                        (setq size (string-to-number size))
                        (propertize " " 'display
                                    (list 'space :align-to (+ col size)))))
               (textwidth (if text (string-width text) 0))
               (postwidth (if post (string-width post) 0)))
          (when text
            (when size
              (cond
               ((> (+ postwidth textwidth) size)
                ;; This doesn't even obey double-width chars :-(
                (setq text (propertize
                            (if (zerop (- size postwidth 1))
                                (substring text 0 1)
                              (concat (substring text 0 (- size postwidth 1)) "…"))
                            'help-echo text)))
               ((and right-align (< (+ postwidth textwidth) (1- size)))
                (setq text (concat (propertize " " 'display
                                               (list 'space :align-to
                                                     (+ col (- size postwidth textwidth 1))))
                                   text)))))
            (insert text)
            (if post (insert post)))
          (if (null size) (setq col (+ col textwidth postwidth))
            (insert space)
            (setq col (+ col size))))))
    (put-text-property start (point) 'mpc-pred `(lambda (info) (and ,@(nreverse pred))))))
                  
;;; The actual UI code ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar mpc-separator nil)

(defmacro mpc-save-selection (&rest body)
  "Execute BODY and restore the selection afterwards."
  (declare (indent 0) (debug t))
  `(let ((selection (mpc-current-selection))
         (position (cons (buffer-substring-no-properties
                          (line-beginning-position) (line-end-position))
                         (current-column))))
     ,@body
     (mpc-restore-selection selection)
     (goto-char (point-min))
     (if (re-search-forward
          (concat "^" (regexp-quote (car position)) "$")
          (if (overlayp mpc-separator)
              (overlay-end mpc-separator))
          t)
         (move-to-column (cdr position)))
     (let ((win (get-buffer-window (current-buffer) 0)))
       (if win (set-window-point win (point))))))

(defvar mpc-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    ;; (define-key map "\e" 'mpc-stop)
    (define-key map "q" 'mpc-quit)
    (define-key map "\r" 'mpc-select)
    (define-key map [(shift return)] 'mpc-toggle-select)
    (define-key map [mouse-2] 'mpc-select)
    (define-key map [(shift mouse-2)] 'mpc-toggle-select)
    ;; Doesn't work because the first click changes the buffer, so the second
    ;; is applied elsewhere :-(
    ;; (define-key map [(double mouse-2)] 'mpc-play-at-point)
    (define-key map "p" 'mpc-pause)
    map))

(easy-menu-define mpc-mode-menu mpc-mode-map
  "Menu for MPC.el."
  '("MPC.el"
    ["Add new browser" mpc-tagbrowser]
    ["Update DB" mpc-update]
    ["Quit" mpc-quit]))

(defvar mpc-tool-bar-map
  (let ((map (make-sparse-keymap)))
    (tool-bar-local-item "mpc/prev" 'mpc-prev 'prev map
     :enable '(not (equal (cdr (assq 'state mpc-status)) "stop")))
    ;; FIXME: how can we bind it to the down-event?
    (tool-bar-local-item "mpc/rewind" 'mpc-rewind 'rewind map
     :enable '(not (equal (cdr (assq 'state mpc-status)) "stop"))
     :button '(:toggle . (and mpc--faster-toggle-timer
                             (not mpc--faster-toggle-forward))))
    ;; We could use a single toggle command for pause/play, with 2 different
    ;; icons depending on whether or not it's selected, but then it'd have
    ;; to be a toggle-button, thus displayed depressed in one of the
    ;; two states :-(
    (tool-bar-local-item "mpc/pause" 'mpc-pause 'pause map
     :visible '(equal (cdr (assq 'state mpc-status)) "play"))
    (tool-bar-local-item "mpc/play" 'mpc-play 'play map
     :visible '(not (equal (cdr (assq 'state mpc-status)) "play")))
    ;; FIXME: how can we bind it to the down-event?
    (tool-bar-local-item "mpc/ffwd" 'mpc-ffwd 'ffwd map
     :enable '(not (equal (cdr (assq 'state mpc-status)) "stop"))
     :button '(:toggle . (and mpc--faster-toggle-timer
                             mpc--faster-toggle-forward)))
    (tool-bar-local-item "mpc/next" 'mpc-next 'next map
     :enable '(not (equal (cdr (assq 'state mpc-status)) "stop")))
    (tool-bar-local-item "mpc/stop" 'mpc-stop 'stop map)
    map))

(define-derived-mode mpc-mode fundamental-mode "MPC"
  "Major mode for the features common to all buffers of MPC."
  (buffer-disable-undo)
  (set (make-local-variable 'tool-bar-map) mpc-tool-bar-map)
  (set (make-local-variable 'truncate-lines) t))

;;; The mpc-status-mode buffer

(define-derived-mode mpc-status-mode mpc-mode "MPC-Status"
  "Major mode to display MPC status info."
  ;; (set (make-local-variable 'mode-line-format) nil)
  (set (make-local-variable 'window-area-factor) 3)
  (set (make-local-variable 'header-line-format) '("MPC " mpc-volume)))

(defvar mpc-status-buffer-format
  '("%-6{Time}/%-6{Duration} %2{Disc--}%3{Track}" "%{Title}" "%{Album}" "%{Artist}" "%128{Cover}"))

(defun mpc-status-buffer-refresh ()
  (let ((buf (mpc-proc-buffer (mpc-proc) 'status)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (save-excursion
          (goto-char (point-min))
          (when (assq 'file mpc-status)
            (dolist (spec mpc-status-buffer-format)
              (let ((pred (get-text-property (point) 'mpc-pred)))
                (if (and pred (funcall pred mpc-status))
                    (forward-line)
                  (delete-region (point) (line-beginning-position 2))
                  (ignore-errors (mpc-format spec mpc-status))
                  (insert "\n"))))
            (unless (eobp) (delete-region (point) (point-max)))))))))

(defun mpc-status-buffer-show ()
  (interactive)
  (let* ((buf (mpc-proc-buffer (mpc-proc) 'status))
         (song-buf (mpc-proc-buffer (mpc-proc) 'song))
         (song-win (if song-buf (get-buffer-window song-buf 0))))
    (unless (buffer-live-p buf)
      (setq buf (get-buffer-create "*MPC-Status*"))
      (with-current-buffer buf
        (mpc-status-mode))
      (mpc-proc-buffer (mpc-proc) 'status buf))
    (if (null song-win) (pop-to-buffer buf)
      (let ((win (split-window song-win 20 t)))
        (set-window-dedicated-p song-win nil)
        (set-window-buffer song-win buf)
        (set-window-dedicated-p song-win 'soft)))))

(defvar mpc-tagbrowser-all-name (propertize "*ALL*" 'face 'italic))
(defvar mpc-tag nil)
(defvar mpc-select nil)

(define-derived-mode mpc-tagbrowser-mode mpc-mode "MPC-";; '("MPC-" mpc-tag)
  (set (make-local-variable 'mode-line-process) '("" mpc-tag))
  (set (make-local-variable 'mode-line-format) nil)
  (set (make-local-variable 'header-line-format) '("" mpc-tag "s"))
  )

(defun mpc-tagbrowser-refresh ()
  (mpc-save-selection
    (widen)
    (goto-char (point-min))
    (assert (looking-at (regexp-quote mpc-tagbrowser-all-name)))
    (forward-line 1)
    (let ((inhibit-read-only t))
      (delete-region (point) (point-max))
      (dolist (val (mpc-cmd-list mpc-tag)) (insert val "\n")))
    (set-buffer-modified-p nil))
  (mpc-reorder))

(defun mpc-updated-db ()
  (unless (assq 'updating_db mpc-status)
    (dolist (buf (process-get (mpc-proc) 'buffers))
      (setq buf (cdr buf))
      (when (and (buffer-local-value 'mpc-tag buf))
        (with-current-buffer buf (mpc-tagbrowser-refresh))))
    (mpc-song-refresh)))

(defun mpc-tagbrowser-buf (tag)
  (let ((buf (mpc-proc-buffer (mpc-proc) tag)))
    (if (buffer-live-p buf) buf
      (setq buf (get-buffer-create (format "*MPC %ss*" (capitalize tag))))
      (mpc-proc-buffer (mpc-proc) tag buf)
      (with-current-buffer buf
        (setq buffer-read-only nil)
        (erase-buffer)
        (mpc-tagbrowser-mode)
        (insert mpc-tagbrowser-all-name "\n")
        (setq buffer-read-only t)
        (forward-line -1)
        (mpc-make-select-overlay)
        (set (make-local-variable 'mpc-tag) tag)
        (mpc-tagbrowser-refresh)
        buf))))

(defun mpc-tagbrowser (tag)
  (interactive (list (read-string "Tag: ")))
  (let* ((newbuf (mpc-tagbrowser-buf tag))
         (win (get-buffer-window newbuf 0)))
    (if win (select-window win)
      (if (with-current-buffer (window-buffer (selected-window))
            (derived-mode-p 'mpc-tagbrowser-mode))
          (setq win (selected-window))
        ;; Find a tagbrowser-mode buffer.
        (let ((buffers (process-get (mpc-proc) 'buffers))
              buffer)
          (while
              (and buffers
                   (not (and (buffer-live-p (setq buffer (pop buffers)))
                             (with-current-buffer buffer
                               (derived-mode-p 'mpc-tagbrowser-mode))
                             (setq win (get-buffer-window buffer 0))))))))
      (if (not win) (pop-to-buffer newbuf)
        (setq win (split-window win nil 'horiz))
        (set-window-buffer win newbuf)
        (set-window-dedicated-p win 'soft)
        (select-window win)
        (balance-windows-area)))))
      

(defun mpc-current-selection ()
  (delq nil (mapcar (lambda (ol)
                      (unless (eq (point-min) (overlay-start ol))
                        (buffer-substring-no-properties
                         (overlay-start ol) (1- (overlay-end ol)))))
                    mpc-select)))

(defun mpc-make-select-overlay ()
  (let ((ol (make-overlay
             (line-beginning-position) (line-beginning-position 2))))
    (overlay-put ol 'mpc-select t)
    (overlay-put ol 'face 'region)
    (overlay-put ol 'evaporate t)
    (make-local-variable 'mpc-select)
    (push ol mpc-select)))

(defun mpc-select (&optional event)
  "Select the tag value at point."
  (interactive (list last-nonmenu-event))
  (assert mpc-select)
  (mpc-event-set-point event)
  (while (and (bolp) (eolp)) (forward-line -1))
  (move-overlay (car mpc-select)
                (line-beginning-position) (line-beginning-position 2))
  (mapc 'delete-overlay (cdr mpc-select))
  (setcdr mpc-select nil)
  (mpc-selection-refresh))

(defun mpc-other-buffers-constraints (&optional avoid-buf)
  (unless avoid-buf (setq avoid-buf (current-buffer)))
  (let ((constraints ())
        tag select)
    (dolist (buf (process-get (mpc-proc) 'buffers))
      (setq buf (cdr buf))
      (when (and (setq tag (buffer-local-value 'mpc-tag buf))
                 (not (eq buf avoid-buf))
                 (setq select
                       (with-current-buffer buf (mpc-current-selection))))
        (push (cons tag select) constraints)))
    constraints))

;; (defvar mpc-constraints nil)
(defun mpc-separator (active)
  ;; Place a separator mark.
  (unless mpc-separator
    (set (make-local-variable 'mpc-separator)
         (make-overlay (point) (point)))
    (overlay-put mpc-separator 'after-string
                 (propertize "\n"
                             'face '(:height 0.05 :inverse-video t))))
  (goto-char (point-min))
  (forward-line 1)
  (while (member (buffer-substring-no-properties
                  (line-beginning-position) (line-end-position))
                 active)
    (forward-line 1))
  (if (or (eobp) (null active))
      (delete-overlay mpc-separator)
    (move-overlay mpc-separator (1- (point)) (point))))

(defun mpc-sort (active)
  ;; Sort the active elements at the front.
  (let ((inhibit-read-only t))
    (condition-case nil
        (sort-subr nil 'forward-line 'end-of-line
                   nil nil
                   (lambda (s1 s2)
                     (setq s1 (buffer-substring-no-properties
                               (car s1) (cdr s1)))
                     (setq s2 (buffer-substring-no-properties
                               (car s2) (cdr s2)))
                     (cond
                      ((equal s1 mpc-tagbrowser-all-name) t)
                      ((equal s2 mpc-tagbrowser-all-name) nil)
                      ((member s1 active)
                       (if (member s2 active)
                           (let ((cmp (mpc-compare-strings s1 s2 t)))
                             (and (numberp cmp) (< cmp 0)))
                         t))
                      ((member s2 active) nil)
                      (t (let ((cmp (mpc-compare-strings s1 s2 t)))
                           (and (numberp cmp) (< cmp 0)))))))
      ;; The comparison predicate arg is new in Emacs-22.
      (wrong-number-of-arguments
        (sort-subr nil 'forward-line 'end-of-line
                   (lambda ()
                     (let ((name (buffer-substring-no-properties
                                  (point) (line-end-position))))
                       (cond
                        ((equal name mpc-tagbrowser-all-name) "")
                        ((member name active) (concat "1" name))
                        (t (concat "2" "name"))))))))))

(defun mpc-restore-selection (selection)
  ;; Restore the selection.  I.e. move the overlays back to their
  ;; corresponding location.  Actually which overlay is used for what
  ;; doesn't matter.
  (let ((ols mpc-select))
    (while selection
      ;; After an update, some elements may have disappeared.
      (goto-char (point-min))
      (when (re-search-forward
             (concat "^" (regexp-quote (pop selection)) "$") nil t)
        (move-overlay (pop ols)
                      (line-beginning-position) (line-beginning-position 2))))
    (when ols
      (mapcar 'delete-overlay ols)
      (if (eq ols mpc-select) ; Empty selection.
          (progn
            (goto-char (point-min))
            (assert (looking-at (regexp-quote mpc-tagbrowser-all-name)))
            (move-overlay (car ols) (point) (line-beginning-position 2))
            (setcdr mpc-select nil))
        ;; ols == (nthcdr (- (length mpc-select) (length ols)) mpc-select)
        (setcdr (nthcdr (- (length mpc-select) (length ols) 1) mpc-select)
                nil)))))
    
      

(defun mpc-reorder (&optional nodeactivate)
  "Return non-nil if a selection was deactivated."
  (mpc-save-selection
    (let ((constraints (mpc-other-buffers-constraints))
          (active 'all))
      ;; (unless (equal constraints mpc-constraints)
      ;;   (set (make-local-variable 'mpc-constraints) constraints)
      (dolist (cst constraints)
        (let ((vals (apply 'mpc-union
                           (mapcar (lambda (val)
                                     (mpc-cmd-list mpc-tag (car cst) val))
                                   (cdr cst)))))
          (setq active
                (if (listp active) (mpc-intersection active vals) vals))))
     
      (when (and (listp active) (not nodeactivate))
        ;; Remove the selections if they are all in conflict with
        ;; other constraints.
        (let ((deactivate t))
          (dolist (sel selection)
            (when (member sel active) (setq deactivate nil)))
          (when deactivate
            (mapc 'delete-overlay (cdr mpc-select))
            (setcdr mpc-select nil)
            ;; Variable declared/used by `mpc-save-selection'.
            (setq selection nil))))

      (goto-char (point-min))
      (mpc-sort (if (listp active) active))
      (mpc-separator (if (listp active) active)))))

(defun mpc-selection-refresh ()
  (if (null mpc-tag)
      nil
    (dolist (buf (process-get (mpc-proc) 'buffers))
      (setq buf (cdr buf))
      (when (and (buffer-local-value 'mpc-tag buf)
                 (not (eq buf (current-buffer))))
        (with-current-buffer buf (mpc-reorder))))
    (mpc-reorder) ;; 'nodeactivate
    (mpc-song-refresh)))

(defvar mpc-song-hashcons (make-hash-table :test 'equal :weakness t)
  "Make song file name objects unique via hash consing.
This is used so that they can be compared with `eq', which is needed for
`text-property-any'.")
(defun mpc-song-hashcons (name)
  (or (gethash name mpc-song-hashcons) (puthash name name mpc-song-hashcons)))
(defcustom mpc-song-format "%2{Disc--}%3{Track} %-6{Time} %40{Title} %20{Album} %20{Artist}"
  "Format used to display each song in the list of songs."
  :type 'string)


(defun mpc-song-refresh ()
  (let ((buf (mpc-proc-buffer (mpc-proc) 'song)))
    (when buf
      (with-current-buffer buf
        (let ((constraints (mpc-other-buffers-constraints))
              (dontsort nil)
              active)
          (if (null constraints)
              (setq dontsort t
                    active (mpc-proc-buf-to-alists
                            (mpc-proc-cmd "playlistinfo")))
            (dolist (cst constraints)
              ;; We don't do anything really special here for playlists,
              ;; because it's unclear what's a correct "union" of playlists.
              (let ((vals (apply 'mpc-union
                                 (mapcar (lambda (val)
                                           (mpc-cmd-find (car cst) val))
                                         (cdr cst)))))
                (setq active (if (null active)
                                 (progn
                                   (if (equal (car cst) "playlist")
                                       (setq dontsort t))
                                   vals)
                               (if (or dontsort
                                       ;; Try to preserve ordering and
                                       ;; repetitions from playlists.
                                       (not (equal (car cst) "playlist")))
                                   (mpc-intersection active vals
                                                     (lambda (x) (assq 'file x)))
                                 (setq dontsort t)
                                 (mpc-intersection vals active
                                                     (lambda (x) (assq 'file x)))))))))
          (erase-buffer)
          ;; Sorting songs is surprisingly difficult: when comparing two
          ;; songs with the same album name but different artist name, you
          ;; have to know whether these are two different albums (with the
          ;; same name) or a single album (typically a compilation).
          ;; I punt on it and just use file-name sorting, which does the
          ;; right thing if your library is properly arranged.
          (dolist (song (if dontsort active
                          (sort active
                                (lambda (song1 song2)
                                  (let ((cmp (mpc-compare-strings
                                              (cdr (assq 'file song1))
                                              (cdr (assq 'file song2)))))
                                    (and (integerp cmp) (< cmp 0)))))))
            (mpc-format mpc-song-format song)
            (insert "\n")
            (put-text-property
             (line-beginning-position 0) (line-beginning-position)
             'mpc-file (mpc-song-hashcons (cdr (assq 'file song))))
            ))
        )))
  (mpc-songpointer-refresh))

(defun mpc-toggle-select (&optional event)
  "Toggle the selection of the tag value at point."
  (interactive (list last-nonmenu-event))
  (assert mpc-select)
  (mpc-event-set-point event)
  (save-excursion
    (cond
     ;; The line is already selected: deselect it.
     ((get-char-property (point) 'mpc-select)
      (let ((ols nil))
        (dolist (ol mpc-select)
          (if (and (<= (overlay-start ol) (point))
                   (> (overlay-end ol) (point)))
              (delete-overlay ol)
            (push ol ols)))
        (assert (= (1+ (length ols)) (length mpc-select)))
        (if ols (setq mpc-select ols)
          ;; That was the last selected entry: select ALL.
          (goto-char (point-min))
          (assert (looking-at (regexp-quote mpc-tagbrowser-all-name)))
          (move-overlay (car mpc-select)
                        (point) (line-beginning-position 2)))))
     ;; We're trying to select *ALL* additionally to others.
     ((eq (line-beginning-position) (point-min))
      nil)
     ;; Nothing's selected just now: move the special *ALL* overlay.
     ((and (null (cdr mpc-select))
           (eq (overlay-start (car mpc-select)) (point-min)))
      ;; Nothing's selected just now.
      (move-overlay (car mpc-select)
                    (line-beginning-position) (line-beginning-position 2)))
     ;; Select the current line.
     (t (mpc-make-select-overlay))))
  (mpc-selection-refresh))

;;; Volume management ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar mpc-volume-map
  (let ((map (make-sparse-keymap)))
    (define-key map [down-mouse-1] 'mpc-volume-mouse-set)
    (define-key map [mouse-1] 'ignore)
    (define-key map [header-line down-mouse-1] 'mpc-volume-mouse-set)
    (define-key map [header-line mouse-1] 'ignore)
    (define-key map [mode-line down-mouse-1] 'mpc-volume-mouse-set)
    (define-key map [mode-line mouse-1] 'ignore)
    map))

(defvar mpc-volume nil) (put 'mpc-volume 'risky-local-variable t)

(defun mpc-volume-refresh ()
  ;; Maintain the volume.
  (setq mpc-volume
        (mpc-volume-widget
         (string-to-number (cdr (assq 'volume mpc-status))))))

(defun mpc-volume-mouse-set (&optional event)
  "Change volume setting."
  (interactive (list last-nonmenu-event))
  (let* ((posn (event-start event))
         (diff
          (if (memq (if (stringp (car-safe (posn-object posn)))
                        (aref (car (posn-object posn)) (cdr (posn-object posn)))
                      (with-current-buffer (window-buffer (posn-window posn))
                        (char-after (posn-point posn))))
                    '(?◁ ?<))
              -1 1))
         (newvol (+ (string-to-number (cdr (assq 'volume mpc-status))) diff)))
    (mpc-proc-cmd (list "setvol" newvol) 'mpc-status-refresh)))

(defun mpc-volume-widget (vol &optional size)
  (unless size (setq size 12.5))
  (let ((scaledvol (* (/ vol 100.0) size)))
    ;; (message "Volume sizes: %s - %s" (/ vol fact) (/ (- 100 vol) fact))
    (list (propertize "<" ;; "◁"
                      ;; 'face 'default
                      'keymap mpc-volume-map
                      'face '(:box (:line-width -2 :style pressed-button))
                      'mouse-face '(:box (:line-width -2 :style released-button)))
          " "
          (propertize "a"
                      'display (list 'space :width scaledvol)
                      'face '(:inverse-video t
                              :box (:line-width -2 :style released-button)))
          (propertize "a"
                      'display (list 'space :width (- size scaledvol))
                      'face '(:box (:line-width -2 :style released-button)))
          " "
          (propertize ">" ;; "▷"
                      ;; 'face 'default
                      'keymap mpc-volume-map
                      'face '(:box (:line-width -2 :style pressed-button))
                      'mouse-face '(:box (:line-width -2 :style released-button))))))

;;; MPC song mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar mpc-currentsong nil) (put 'mpc-currentsong 'risky-local-variable t)
(defvar mpc-song-format-description nil) (put 'mpc-song-format-description 'risky-local-variable t)

(defvar mpc-previous-window-config nil)


(define-derived-mode mpc-song-mode mpc-mode "MPC-song"
  (setq mpc-song-format-description
        (with-temp-buffer (mpc-format mpc-song-format 'self) (buffer-string)))
  (set (make-local-variable 'header-line-format)
       ;; '("MPC " mpc-volume " " mpc-currentsong)
       (list (propertize " " 'display '(space :align-to 0))
             'mpc-song-format-description))
  (set (make-local-variable 'mode-line-process)
       '(" " ;; mpc-volume " "
         mpc-currentsong)))

(defun mpc-songpointer-refresh ()
  (let ((buf (mpc-proc-buffer (mpc-proc) 'song)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((pos (text-property-any
                    (point-min) (point-max)
                    'mpc-file (mpc-song-hashcons
                               (cdr (assq 'file mpc-status))))))
          (unless (local-variable-p 'overlay-arrow-position)
            (set (make-local-variable 'overlay-arrow-position) (make-marker)))
          (move-marker overlay-arrow-position pos))))))

(defun mpc-currentsong-refresh ()
  ;; Maintain the currentsong.
  (mpc-status-buffer-refresh)
  (ignore-errors
    (setq mpc-currentsong
          (concat (if (assq 'updating_db mpc-status) "Updating-DB ")
                  (when (assq 'file mpc-status)
                    (concat " "
                            (mpc-secs-to-time (cdr (assq 'time mpc-status)))
                            " "
                            (cdr (assq 'Title mpc-status))
                            " ("
                            (cdr (assq 'Artist mpc-status))
                            " / "
                            (cdr (assq 'Album mpc-status))
                            ")")))))
  (force-mode-line-update t))

(defun mpc-song-buf ()
  (let ((buf (mpc-proc-buffer (mpc-proc) 'song)))
    (if (buffer-live-p buf) buf
      (with-current-buffer (setq buf (get-buffer-create "*MPC Songs*"))
        (mpc-proc-buffer (mpc-proc) 'song buf)
        (mpc-song-mode)
        buf))))

(defun mpc-update ()
  "Tell MPD to refresh its database."
  (interactive)
  (mpc-cmd-update))

(defun mpc-quit ()
  "Quit Music Player Daemon."
  (interactive)
  (let* ((proc mpc-proc)
         (bufs (mapcar 'cdr (if proc (process-get proc 'buffers))))
         (wins (mapcar (lambda (buf) (get-buffer-window buf 0)) bufs))
         (song-buf (mpc-song-buf))
         frames)
    ;; Collect all the frames where MPC buffers appear.
    (dolist (win wins)
      (when (and win (not (memq (window-frame win) frames)))
        (push (window-frame win) frames)))
    (if (and frames song-buf
             (with-current-buffer song-buf mpc-previous-window-config))
        (progn
          (select-frame (car frames))
          (set-window-configuration
           (with-current-buffer song-buf mpc-previous-window-config)))
      ;; Now delete the ones that show nothing else than MPC buffers.
      (dolist (frame frames)
        (let ((delete t))
          (dolist (win (window-list frame))
            (unless (memq (window-buffer win) bufs) (setq delete nil)))
          (if delete (ignore-errors (delete-frame frame))))))
    ;; Then kill the buffers.
    (mapc 'kill-buffer bufs)
    (mpc-status-stop)
    (if proc (delete-process proc))))
      
(defun mpc-stop ()
  "Stop playing the current queue of songs."
  (interactive)
  (mpc-cmd-stop)
  (mpc-cmd-clear)
  (mpc-status-refresh))

(defun mpc-pause ()
  "Pause playing."
  (interactive)
  (mpc-cmd-pause "1"))

(defun mpc-resume ()
  "Pause playing."
  (interactive)
  (mpc-cmd-pause "0"))

(defun mpc-play ()
  "Start playing whatever is selected."
  (interactive)
  (let (songs)
    (cond
     ((member (cdr (assq 'state (mpc-cmd-status))) '("pause"))
      (mpc-resume))
     ((setq songs
            (let ((buf (mpc-proc-buffer (mpc-proc) 'song)))
              (when (buffer-live-p buf)
                (with-current-buffer buf
                  (save-excursion
                    (goto-char (point-min))
                    (let ((files ())
                          file)
                      (while (not (eobp))
                        (setq file (get-text-property (point) 'mpc-file))
                        (forward-line 1)
                        (push file files))
                      (nreverse files)))))))
      (mpc-cmd-add songs (mpc-other-buffers-constraints 'allbufs))
      (if (member (cdr (assq 'state (mpc-cmd-status))) '("stop"))
          (mpc-cmd-play)))
     (t (error "Don't know what to play")))))

(defun mpc-next ()
  "Jump to the next song in the queue."
  (interactive)
  (mpc-proc-cmd "next")
  (mpc-status-refresh))

(defun mpc-prev ()
  "Jump to the beginning of the current song, or to the previous song."
  (interactive)
  (let ((time (cdr (assq 'time mpc-status))))
    ;; Here we rely on the fact that string-to-number silently ignores
    ;; everything after a non-digit char.
    (cond
     ;; Go back to the beginning of current song.
     ((and time (> (string-to-number time) 0))
      (mpc-proc-cmd (list "seekid" (cdr (assq 'songid mpc-status)) 0)))
     ;; We're at the beginning of the first song of the playlist.
     ;; Fetch the previous one from `mpc-queue-back'.
     ;; ((and (zerop (string-to-number (cdr (assq 'song mpc-status))))
     ;;       mpc-queue-back)
     ;;  ;; Because we use cmd-list rather than cmd-play, the queue is not
     ;;  ;; automatically updated.
     ;;  (let ((prev (pop mpc-queue-back)))
     ;;    (push prev mpc-queue)
     ;;    (mpc-proc-cmd
     ;;     (mpc-proc-cmd-list
     ;;      (list (list "add" prev)
     ;;            (list "move" (cdr (assq 'playlistlength mpc-status)) "0")
     ;;            "previous")))))
     ;; We're at the beginning of a song, but not the first one.
     (t (mpc-proc-cmd "previous")))
    (mpc-status-refresh)))

(defvar mpc-last-seek-time '(0 . 0))

(defun mpc--faster (event speedup step)
  "Fast forward."
  (interactive (list last-nonmenu-event))
  (let ((repeat-delay (/ (abs (float step)) speedup)))
    (if (not (memq 'down (event-modifiers event)))
        (let* ((currenttime (float-time))
               (last-time (- currenttime (car mpc-last-seek-time))))
          (if (< last-time (* 0.9 repeat-delay))
              nil ;; Trottle
            (let* ((status (if (< last-time 1.0)
                               mpc-status (mpc-cmd-status)))
                   (songid (cdr (assq 'songid status)))
                   (time (if songid
                             (if (< last-time 1.0)
                                 (cdr mpc-last-seek-time)
                               (string-to-number
                                (cdr (assq 'time status)))))))
              (setq mpc-last-seek-time
                    (cons currenttime (setq time (+ time step))))
              (mpc-proc-cmd (list "seekid" songid time)
                            'mpc-status-refresh))))
      (let ((status (mpc-cmd-status)))
        (lexical-let* ((songid (cdr (assq 'songid status)))
                       (step step)
                       (time (if songid (string-to-number
                                         (cdr (assq 'time status))))))
          (let ((timer (run-with-timer
                        t repeat-delay
                        (lambda ()
                          (mpc-proc-cmd (list "seekid" songid
                                              (setq time (+ time step)))
                                        'mpc-status-refresh)))))
            (while (mouse-movement-p
                    (event-basic-type (setq event (read-event)))))
            (cancel-timer timer)))))))

(defvar mpc--faster-toggle-timer nil)
(defun mpc--faster-stop ()
  (when mpc--faster-toggle-timer
    (cancel-timer mpc--faster-toggle-timer)
    (setq mpc--faster-toggle-timer nil)))

(defun mpc--faster-toggle-refresh ()
  (if (equal (cdr (assq 'state mpc-status)) "stop")
      (mpc--faster-stop)))

(defvar mpc--faster-toggle-forward nil)
(defvar mpc--faster-acceleration 0.5)
(defun mpc--faster-toggle (speedup step)
  (setq speedup (float speedup))
  (if mpc--faster-toggle-timer
      (mpc--faster-stop)
    (mpc-status-refresh) (mpc-proc-sync)
    (lexical-let* ((speedup speedup)
                   songid songtime songduration oldtime)
      (let ((fun
             (lambda ()
               (let ((newsongid (cdr (assq 'songid mpc-status))))
                 (cond
                  ((null newsongid) (mpc--faster-stop))
                  ((not (equal songid newsongid))
                   ;; We jumped to another song: reset.
                   (setq songid newsongid)
                   (setq songtime (string-to-number
                                   (cdr (assq 'time mpc-status))))
                   (setq songduration
                         (string-to-number
                          (let ((s (cdr (assq 'time mpc-status))))
                            (if (not (string-match ":" s))
                                (error "Unexpected time format %S" s)
                              (substring s (match-end 0))))))
                   (setq oldtime (float-time)))
                  ((and (>= songtime songduration) mpc--faster-toggle-forward)
                   ;; Skip to the next song.  Done automatically if we're
                   ;; playing.
                   nil)
                  ((and (<= songtime 0) (not mpc--faster-toggle-forward))
                   ;; Skip to the previous song.  Not implemented yet.
                   nil)
                  (t
                   (setq speedup (+ speedup mpc--faster-acceleration))
                   (let ((newstep
                          (truncate (* speedup (- (float-time) oldtime)))))
                     (if (<= newstep 1) (setq newstep 1))
                     (setq oldtime (+ oldtime (/ newstep speedup)))
                     (if (not mpc--faster-toggle-forward)
                         (setq newstep (- newstep)))
                     (setq songtime (min songduration (+ songtime newstep)))
                     (unless (>= songtime songduration)
                       (condition-case nil
                           (mpc-proc-cmd
                            (list "seekid" songid songtime)
                            'mpc-status-refresh)
                         (mpc-proc-error (mpc-status-refresh)))))))))))
        (setq mpc--faster-toggle-forward (> step 0))
        (funcall fun)                   ;Initialize values.
        (setq mpc--faster-toggle-timer
              (run-with-timer t 0.3 fun))))))



(defvar mpc-faster-speedup 8)

(defun mpc-ffwd (event)
  "Fast forward."
  (interactive (list last-nonmenu-event))
  ;; (mpc--faster event 4.0 1)
  (mpc--faster-toggle mpc-faster-speedup 1))
      
(defun mpc-rewind (event)
  "Fast rewind."
  (interactive (list last-nonmenu-event))
  ;; (mpc--faster event 4.0 -1)
  (mpc--faster-toggle mpc-faster-speedup -1))
      
      
(defun mpc-play-at-point (&optional event)
  (interactive (list last-nonmenu-event))
  (mpc-select event)
  (mpc-play))

;; (defun mpc-play-tagval ()
;;   "Play all the songs of the tag at point."
;;   (interactive)
;;   (let* ((val (buffer-substring (line-beginning-position) (line-end-position)))
;;          (songs (mapcar 'cdar
;;                         (mpc-proc-buf-to-alists
;;                          (mpc-proc-cmd (list "find" mpc-tag val))))))
;;     (mpc-cmd-add songs (cons mpc-tag val))
;;     (if (member (cdr (assq 'state (mpc-cmd-status))) '("stop"))
;;         (mpc-cmd-play))))

(defcustom mpc-frame-alist '((name . "MPC") (tool-bar-lines . 1))
  "Alist of frame parameters for the MPC frame."
  :type 'alist)

;;;###autoload
(defun mpc ()
  "Main entry point for MPC."
  (interactive)
  (let* ((song-buf (mpc-song-buf))
         (song-win (get-buffer-window song-buf 0)))
    (if song-win
        (select-window song-win)
      (if (or (window-dedicated-p (selected-window))
              (window-minibuffer-p))
          (ignore-errors (select-frame (make-frame mpc-frame-alist)))
        (with-current-buffer song-buf
          (set (make-local-variable 'mpc-previous-window-config)
               (current-window-configuration))))
      (let* ((win1 (selected-window))
             (win2 (split-window))
             (tags mpc-browser-tags))
        (unless tags (error "Need at least one entry in `mpc-browser-tags'"))
        (set-window-buffer win2 song-buf)
        (set-window-dedicated-p win2 'soft)
        (mpc-status-buffer-show)
        (set-window-buffer win1 (mpc-tagbrowser-buf (pop tags)))
        (set-window-dedicated-p win1 'soft)
        (while tags
          (set-window-buffer (setq win1 (split-window win1 nil 'horiz))
                             (mpc-tagbrowser-buf (pop tags)))
        (set-window-dedicated-p win1 'soft))))
    (balance-windows-area))
  (mpc-song-refresh)
  (mpc-status-refresh))

(provide 'mpc)

;; arch-tag: 4794b2f5-59e6-4f26-b695-650b3e002f37
;;; mpc.el ends here
