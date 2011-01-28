(require 'rcirc)


(setq rcirc-url-regexp 
      "\\(?:\\b\\(?:\\(?:\\(?:\\(?:\\(?:f\\(?:ile\\|tp\\)\\|gopher\\|https?\\|mailto\\|news\\|telnet\\|wais\\)://\\)\\|www\\.\\)[-a-zA-Z0-9_.]+[-a-zA-Z0-9_]+\\(?::[0-9]+\\)?\\)\\|\\(?:[-a-zA-Z0-9_.]+\\(?:\\.\\(?:com\\|net\\|org\\|org\\)\\)\\b\\)\\)\\(?:/[]-a-zA-Z0-9_='!?#$@~`%&*+|\\/:;.,{}[()]+[]-a-zA-Z0-9_=#$@~`%&*+|\\/:;{}[()]\\)?\\)")

(defun-rcirc-command op (nicks)
  "Send OP for `nicks'.
    Limitation: in its interactive form, you can only op one nick."
  (interactive (list (completing-read "Op nick: " (with-rcirc-server-buffer rcirc-nick-table))))
  (dolist (nick (split-string nicks " "))
    (rcirc-send-string process
                       (format "ChanServ OP %s %s" target nick))))

(defalias 'rcirc-cmd-opme
  '(lambda (&optional args process target)
     (interactive)
     (rcirc-cmd-op (or rcirc-nick rcirc-default-nick)))
  "Request a ChanServ OP on my current nick in the current channel.")

(defalias 'rcirc-cmd-deopme
  '(lambda (&optional args process target)
     (interactive)
     (rcirc-cmd-deop (or rcirc-nick rcirc-default-nick))))

(defalias 'rcirc-cmd-deop
  '(lambda (nicks &optional process target)
     (interactive (list (completing-read "De-op nick: " (with-rcirc-server-buffer rcirc-nick-table))))
     (let ((nicks (concat "-" (mapconcat 'identity (split-string nicks) " -"))))
       (rcirc-cmd-op nicks)))
  "Send DE-OP for `nicks'.
    Limitation: in its interactive form, you can only de-op one nick.")


(define-key rcirc-mode-map (kbd "C-c C-O") 'rcirc-cmd-opme)
(define-key rcirc-mode-map (kbd "C-c C-o") 'rcirc-omit-mode)
(define-key rcirc-mode-map (kbd "C-c C-k") 'kill-region)

(defun rcirc-handler-301 (process cmd sender args)
  "/away message handler.")

(defun rcirc-sort-name (buf)
  "Return server process and buffer name as a string."
  (with-current-buffer buf
    (downcase (concat (if rcirc-server-buffer
                          (buffer-name rcirc-server-buffer)
                        " ")
                      " "
                      (or rcirc-target "")))))

(defun rcirc-sort-buffers (a b)
  "Sort buffers A and B using `rcirc-sort-name'."
  (string< (rcirc-sort-name a)
           (rcirc-sort-name b)))

(defun rcirc-unfill ()
  (interactive)
  (save-excursion
    (goto-char rcirc-prompt-end-marker)
    (while (re-search-forward "\\s-+" nil t)
      (replace-match " "))))


(add-hook 'rcirc-print-hooks 'rcirc-late-fix-hook)

(defface rcirc-late-fix-face '((t (:underline t :foreground "Blue")))
  "Face for showing fixed words on the channel buffer.")

(defun rcirc-late-fix-apply (beg end string)
  (save-excursion
    (let ((overlay (make-overlay beg end (current-buffer) nil t)))
      (overlay-put overlay 'face 'rcirc-late-fix-face)
      (overlay-put overlay 'display string))))

(defun rcirc-late-fix-hook (process sender response target text)
  (save-excursion
    (when (string-equal response "PRIVMSG")
      (let (from to global matches)
        (when (or (and (string-match "^s/\\(.+\\)/\\(.+\\)/g" text)
                       (setq from (match-string 1 text)
                             to (match-string 2 text)
                             global t))
                  (and (string-match "^s/\\(.+\\)/\\([^/]+\\)" text)
                       (setq from (match-string 1 text)
                             to (match-string 2 text))))
          (set-buffer (rcirc-late-fix-matching-buffer target))
          (goto-char (point-max))
          (when (search-backward (concat "<" sender ">") nil t 2)
            (goto-char (match-end 0)) ;; skip nickname
            (while (search-forward from (point-at-eol) t) ;; make a list of the points from each match
              (setf matches (cons (list (match-beginning 0) (match-end 0)) matches)))
            (when (not (null matches)) ;; there was at least one match
              (if global               ;; global = replace all matches
                  (mapc '(lambda (x) (rcirc-late-fix-apply (car x) (cadr x) to)) matches)
                (rcirc-late-fix-apply (caar matches) (cadar matches) to)))))))))

(defun rcirc-late-fix-matching-buffer (name)
  "Find buffer (channel) that starts with NAME."
  (find-if '(lambda (x) (string-match (concat "^" name) x))
           (mapcar 'buffer-name (buffer-list))))




(add-hook 'rcirc-mode-hook 'mt-rcirc-setup)

(setq rcirc-debug-flag t)
(setq rcirc-time-format "%H%M%S  ")
(setq rcirc-default-nick "entropie")
(setq rcirc-default-user-name "mt")
(setq rcirc-default-user-full-name "Michael Trommer")
(setq rcirc-decode-coding-system 'undecided)


(setq rcirc-bright-nicks '("ChanServ" "NickServ" "fsbot" "lambdabot" "birny" "corundum" "rutlov" "Mathetes"))
(setq rcirc-dim-nicks '("typochur" "replikant" "vax" "Ionic`" "Bine" "mo2000" "d1g" "Mias" "dominikh" "muetze"))
(setq rcirc-ignore-list '("ro0^"))

(defadvice rcirc-format-response-string (after dim-entire-line)
  "Dim whole line for senders whose nick matches `rcirc-dim-nicks'."
  (when (and rcirc-dim-nicks sender
             (string-match (regexp-opt rcirc-dim-nicks 'words) sender))
    (setq ad-return-value (rcirc-facify ad-return-value 'rcirc-dim-nick))))
(ad-activate 'rcirc-format-response-string)

(setq rcirc-coding-system-alist (quote (
                                        ("#ccc" undecided . iso-latin-1)
                                        ("#gentoo.de" undecided . iso-latin-1)
                                        ("#linux.de" undecided . iso-latin-1)
                                        )))
(setq rcirc-server-alist 
      '(
        ("kommunism.us" :port "56666" :channels ("#ruby-de" "#ackro" "#rennfeuer" "#emacs"))
        ("78.46.106.73" :nick "entropy" :port "56666" :channels ("#linux.de" "#ccc"))))
        ;; ("irc.freenode.net" :channels ("#ruby-de" "#ackro" "#rennfeuer"))
        ;; ("irc.fu-berlin.de" :channels ("#ccc") :nick "entropy")
        ;; ("irc.coldfront.net" :channels ("#eve-dev" "#scrapheap") :nick "AnalphaBestie")))

;; (setq rcirc-authinfo
;;       '(("kommunism.us" server "entropie" "sekret")
;;         ("78.46.106.73" server "entropy"  "sekret")))     
(load-file "~/.rcirc-auth.el")

(add-to-list 'bs-configurations '("rcirc" nil nil nil (lambda (buf) (with-current-buffer buf (not (eq major-mode 'rcirc-mode)))) rcirc-sort-buffers))

(setq rcirc-colors '("lemon chiffon" "NavajoWhite" "cornflower blue" "azure" "slate grey"
                     "RoyalBlue" "SkyBlue" "DarkSeaGreen" "YellowGreen" "DarkKhaki" "gold"
                     "IndianRed" "peru" "beige" "wheat" "tan" "salmon" "orange" "HotPink"
                     "plum" "orchid" "thistle" "snow3" "seashell3" "cornsilk3" "ivory3"
                     "azure3" "green3" "gold3" "chocolate3" "DarkOrange3" "plum3" "plum4"
                     "grey17"))
(eval-after-load 'rcirc '(require 'rcirc-color))

(eval-after-load 'rcirc
  '(defun-rcirc-command encoding (arg)
     "Change the encoding coding system `rcirc-encode-coding-system' for the current buffer only."
     (interactive)
     (if (string= arg "")
         (rcirc-print process (rcirc-nick process) "NOTICE" target
                      (symbol-name rcirc-encode-coding-system))
       (set (make-local-variable 'rcirc-encode-coding-system)
            (intern-soft arg)))))

(eval-after-load 'rcirc
  '(when (not (boundp 'rcirc-nick-prefix-chars))
     (defvar rcirc-nick-prefix-chars "~&@%+")
     (defun rcirc-user-nick (user)
       "Return the nick from USER.  Remove any non-nick junk."
       (save-match-data
         (if (string-match (concat "^[" rcirc-nick-prefix-chars
                                   "]?\\([^! ]+\\)!?") (or user ""))
             (match-string 1 user)
           user)))))

(eval-after-load 'rcirc
  '(defun-rcirc-command sv (arg)
     "Boast about rcirc."
     (interactive "i")
     (rcirc-send-message process target
                         (concat "I use " rcirc-id-string))))

(eval-after-load 'rcirc
  '(defun-rcirc-command color (args)
     "Change one of the nick colors."
     (interactive)
     (setq args (split-string args))
     (rcirc-do-color (car args) (cadr args) process target)))


;;(add-hook 'rcirc-print-hooks 'rcirc-write-log)

;; (setq rcirc-log-directory "~/.log/irc")

;; (defun rcirc-write-log (process sender response target text)
;;   (when rcirc-log-directory
;;     (with-temp-buffer
;;       ;; Sometimes TARGET is a buffer :-(
;;       (when (bufferp target)
;;         (setq target (with-current-buffer buffer rcirc-target)))
;;       ;; Sometimes buffer is not anything at all!
;;       (unless (or (null target) (string= target ""))
;;         ;; Print the line into the temp buffer.
;;         (insert (format-time-string "%Y-%m-%d %H:%M "))
;;         (insert (format "%-16s " (rcirc-user-nick sender)))
;;         (unless (string= response "PRIVMSG")
;;           (insert "/" (downcase response) " "))
;;         (insert text "\n")
;;         ;; Append the line to the appropriate logfile.
;;         (write-region (point-min) (point-max)
;;                       (concat rcirc-log-directory "/" (downcase target))
;;                       t 'quietly)))))

(defun rcirc-commands ()
  "Return a list of defined IRC commands.
If a command called rcirc-cmd-foo exists, the IRC command /FOO
will be part of the list returned."
  (let ((commands))
    (mapatoms (lambda (sym)
                (let ((name (symbol-name sym)))
                  (when (and (commandp sym)
                             (string= (substring name 0 (min (length name) 10)) "rcirc-cmd-"))
                    (setq commands (cons (concat"/" (substring name 10))
                                         commands))))))
    commands))


(provide 'mt-rcirc)

;; (add-hook 'rcirc-print-hooks 'my-rcirc-print-hook)
;; (defun my-rcirc-print-hook (a b c d e)
;;   (when (and
;;          (string-match (rcirc-nick a) e)
;;          (not (string-match (concat "<" (rcirc-nick a) ">") e))) ; but, ignore my own messages 
;;     (message target)
;;     (shell-command-to-string (concat "echo '["target"] <" b ">: " e "' > ~/.osd"))))


