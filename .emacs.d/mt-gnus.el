;; -*- emacs-lisp -*-

;; You need this to be able to list all labels in gmail

(setq gnus-ignored-newsgroups "")

;; And this to configure gmail imap

(setq gnus-select-method '(nnimap "gmail"
                                  (nnimap-address "imap.gmail.com")
                                  (nnimap-server-port 993)
                                  (nnimap-stream ssl)))

;; My version of gnus in my Mac does not handle html messages
;; correctly (the one in the netbook does, I guess it is a different
;; version). The following will chose plaintext every time this is
;; possible.

(setq mm-discouraged-alternatives '("text/html" "text/richtext"))

;; Available SMTP accounts. The format is
;; type of connection - account in the from field - smtp server -
;; port - login name - password. You can leave the password field
;; as NIL and emacs will ask every time

(defvar smtp-accounts
  '(
    (ssl "mictro@gmail.com" "smtp.gmail.com"
         587 "mictro@gmail.com" "thDoNoCh,WeCh!")))

;; Now lets configure smtpmail.el with your name and functions to send
;; mail using your smtp accounts by changing the from field

(require 'smtpmail)
(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      mail-from-style nil user-full-name "Michael Trommer"
      smtpmail-debug-info t smtpmail-debug-verb t)

(defun set-smtp (mech server port user password)
  "Set related SMTP variables for supplied parameters."
  (setq smtpmail-smtp-server server smtpmail-smtp-service port
        smtpmail-auth-credentials (list (list server port user
                                              password)) smtpmail-auth-supported (list mech)
        smtpmail-starttls-credentials nil)
  (message "Setting SMTP server to `%s:%s' for user `%s'."
           server port user))

(defun set-smtp-ssl (server port user password &optional key
                            cert)
  "Set related SMTP and SSL variables for supplied parameters."
  (setq starttls-use-gnutls t
        starttls-gnutls-program "gnutls-cli"
        starttls-extra-arguments nil smtpmail-smtp-server server
        smtpmail-smtp-service port
        smtpmail-auth-credentials (list (list server port user
                                              password)) smtpmail-starttls-credentials (list (list
                                              server port key cert)))
  (message
   "Setting SMTP server to `%s:%s' for user `%s'. (SSL
enabled.)" server port user))

(defun change-smtp ()
  "Change the SMTP server according to the current from line."
  (save-excursion
    (loop with from = (save-restriction
                        (message-narrow-to-headers)
                        (message-fetch-field "from"))
          for (auth-mech address . auth-spec) in smtp-accounts
          when (string-match address from) do (cond
                                               ((memq auth-mech '(cram-md5 plain login))
                                                (return (apply 'set-smtp (cons auth-mech auth-spec))))
                                               ((eql auth-mech 'ssl)
                                                (return (apply 'set-smtp-ssl auth-spec)))
                                               (t (error "Unrecognized SMTP auth. mechanism:
`%s'." auth-mech))) finally (error "Cannot infer SMTP
information."))))

;; The previous function will complain if you fill the from field with
;; an account not present in smtp-accounts.

(defvar %smtpmail-via-smtp (symbol-function 'smtpmail-via-smtp))

(defun smtpmail-via-smtp (recipient smtpmail-text-buffer)
  (with-current-buffer smtpmail-text-buffer
    (change-smtp))
  (funcall (symbol-value '%smtpmail-via-smtp) recipient
           smtpmail-text-buffer))

;; This wraps send mail via smtp mail, to be able to send multiple
;; messages with smtpmail.










;; (require 'info)
;; (add-to-list 'Info-default-directory-list "~/.emacs.d/pgnus/texi/")

;; ;; useragent
;; (setq mail-user-agent 'gnus-user-agent)

;; (require 'gnus-cache)
;; (setq gnus-use-cache t)
;; (setq gnus-always-read-dribble-file t )
;; (setq nnmail-treat-duplicates 'delete)

;; (setq gnus-large-newsgroup 2000
;;       gnus-check-new-newsgroups nil
;;       gnus-nov-is-evil nil)

;; (setq gnus-save-all-headers t)

;; (setq user-mail-address "mictro@gmail.com")
;; (setq user-full-name "Michael Trommer")

;; (setq gnus-select-method '(nntp "news.t-online.de"))
;; (setq mail-sources nil)

;; (add-to-list
;;   'gnus-secondary-select-methods
;;   '(nnmaildir ""
;;               (directory "~/Maildir")
;;               (directory-files nnheader-directory-files-safe)
;;               (get-new-mail nil)))

;; ;; save mails
;; (setq gnus-message-archive-group 
;;       '((if (message-news-p) 
;;           "Sent-News" 
;;           "Sent-Mail")))

;; (gnus-add-configuration
;;   '(article
;;      (horizontal 1.0
;;                  (vertical 25
;;                            (group 1.0))
;;                  (vertical 1.0
;;                            (summary 0.25 point)
;;                            (article 1.0)))))
;; (gnus-add-configuration
;;   '(summary
;;      (horizontal 1.0
;;                  (vertical 25
;;                            (group 1.0))
;;                  (vertical 1.0
;;                            (summary 1.0 point)))))


;; (add-hook 'gnus-summary-mode-hook 'my-setup-hl-line)
;; (add-hook 'gnus-group-mode-hook 'my-setup-hl-line)
;; (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
;; (defun my-setup-hl-line ()
;;   (hl-line-mode 1)
;;   (setq cursor-type nil)) ; Comment this out, if you want the cursor to stay visible.

;; ;; Inline images?
;; (setq mm-attachment-override-types '("image/.*"))
;; (eval-after-load "gnus-sum" 
;;   '(add-to-list 
;;     'gnus-newsgroup-variables 
;;     '(mm-discouraged-alternatives 
;;       . '("text/html" "image/.*"))))
 
;; ; Display `text/html' parts in `nnrss' groups. 
;; (add-to-list 
;;  'gnus-parameters 
;;  '("\\`nnrss:" (mm-discouraged-alternatives nil)))

;; ;; and use w3m to do so
;; (setq mm-text-html-renderer 'w3m)

;; (setq gnus-ignored-from-addresses
;;       (mapconcat 'regexp-quote
;;        '("bandresen@gmail.com"
;;          "benny@in-ulm.de"
;;          "benjamin.andresen@ulm.ccc.de")
;;        "\\|"))

;; (setq gnus-visible-headers 
;;       (mapconcat 'regexp-quote
;;                  '("From:" "Newsgroups:" "Subject:" "Date:" 
;;                    "Organization:" "To:" "Cc:" "Followup-To" "Gnus-Warnings:"
;;                    "X-Sent:" "X-URL:" "User-Agent:" "X-Newsreader:"
;;                    "X-Mailer:" "Reply-To:" "X-Spam:" "X-Spam-Status:" "X-Now-Playing"
;;                    "X-Attachments" "X-Diagnostic")
;;                  "\\|"))

;; ; display stuff
;; (add-hook 'gnus-article-display-hook 'gnus-smiley-display nil)

;; ;; but the signature-sperator needs to be set, otherwise it will look ugly!
;; (setq gnus-signature-separator
;;       '("^-- $"         ; The standard
;;         "^-- *$"        ; A common mangling
;;         "^-------*$"    ; Many people just use a looong
;;         ; line of dashes.  Shame!
;;         "^ *--------*$" ; Double-shame!
;;         "^________*$"   ; Underscores are also popular
;;         "^========*$")) ; Pervert!

;; (setq gnus-group-sort-groups
;;       '(gnus-sort-by-rank gnus-sort-by-alphabet))

;; (setq gnus-group-line-format "%P %-4N %(%~(pad-right 8)G%)\n"
;;       gnus-topic-line-format "%i[ %0{%(%n (new: %a)%)%} ]\n")

;; (setq gnus-summary-line-format ":%U%R| %10,10&user-date; | %-20,20n | %B%s%-101=|%5,5L | %5,5k\n")

;; ;; Automatically poll for news every ten minutes (after two minute idle)
;; (require 'gnus-demon)
;; (setq gnus-use-demon t)
;; (gnus-demon-add-handler 'gnus-group-get-new-news 10 2)
;; (gnus-demon-init)
 
;; ;; I want to see some stuff permanently
;; (setq gnus-permanently-visible-groups 
;;       (mapconcat 'regexp-quote
;;                  '("Inbox"
;;                    "Sent-Mail"
;;                    "Sent-News")
;;                  "\\|"))


;; ;; (require 'gnus-alias)
;; ;; (setq gnus-alias-identity-alist
;; ;;       '(("gmail" "" "Benjamin Andresen <bandresen@gmail.com>" nil nil "" "")
;; ;;         ("bn" "" "Benjamin Andresen <benny@in-ulm.de>" nil nil "" "")
;; ;;         ("invalid" "" "Benjamin Andresen <invalid@invalid.invalid>" "black hole research and data storage" nil "" "")
;; ;;         ("ccc" "" "Benjamin Andresen <benjamin.andresen@ulm.ccc.de>" "Inner Party" nil "" "")))

;; ;(setq gnus-alias-default-identity "gmail")
;; ;(gnus-alias-init)
;; ;;(define-key message-mode-map "\C-c\C-p" 'gnus-alias-select-identity)

;; (setq gnus-posting-styles
;;       '((".*"
;;          (name "Klap"))

;;         ((message-news-p)
;;          (address "invalid@invalid.invalid"))

;;         ("nnmaildir:.*"
;;          (From (save-excursion
;;                  (set-buffer gnus-article-buffer)
;;                  (message-fetch-field "to"))))))

;; (setq gnus-auto-select-subject 'best)
;; (setq gnus-fetch-old-headers 'some)

;; ;; power speed awesomeness!
;; (gnus-compile)

;; ;; ;; -*- emacs-lisp -*-

;; ;; ;; ;; Set nnmail-expiry-wait-function
;; ;; ;; (load-file "~/.emacs.d/pgnus/lisp/expiry.el")
;; ;; ;; ;; Set nnmail-split-fancy
;; ;; ;; (load-file "~/.emacs.d/pgnus/lisp/lists.el")
;; ;; ;; ;; Set gnus-posting-styles
;; ;; ;; (load-file "~/.emacs.d/pgnus/lisp/posting-styles.el")

;; ;; ;;; Coding systems
;; ;; (setq mm-coding-system-priorities
;; ;;       '(mule-utf-8 iso-latin-1 iso-latin-9))

;; ;; ;;; Crypt-foo
;; ;; ;; (this would include pgg, if i wouldn't use the defaults)
;; ;; (setq gnus-message-replysign t
;; ;;       gnus-message-replyencrypt t
;; ;;       mm-verify-option 'always
;; ;;       mm-decrypt-option 'always)

;; ;; ;; Buttonize the different parts, please
;; ;; (setq gnus-buttonized-mime-types '("multipart/encrypted" "multipart/signed"))

;; ;; ;; But keep buttons for multiple parts
;; ;; (setq gnus-inhibit-mime-unbuttonizing t)

;; ;; ;;; Key bindings

;; ;; ;; (setq mail-sources '((directory :path "~/Maildir" :suffix ".spool")))
;; ;; ;; (setq mail-source-delete-incoming t)

;; ;; (setq
;; ;;  gnus-select-method '(nnmaildir "mymailbox" (directory "~/Maildir/"))
;; ;;  mail-sources '((maildir :path "~/Maildir/" :subdirs ("cur" "new")))
;; ;;  mail-source-delete-incoming t
;; ;; )
;; ;; (setq gnus-secondary-select-methods nil)
;; ;; (setq gnus-message-archive-group "nnmaildir+mymailbox:outbox")


;; ;; (add-hook 'gnus-summary-mode-hook
;; ;;           (lambda ()
;; ;;             (define-key gnus-summary-mode-map
;; ;;               [?U] 'gnus-summary-put-mark-as-unread)
;; ;;             (define-key gnus-summary-mode-map
;; ;;               [?s] 'gnus-summary-move-to-spambox)))
;; ;; (add-hook 'message-mode-hook
;; ;;           (lambda ()
;; ;;             (define-key message-mode-map
;; ;;               (kbd "C-c C-u") 'mt-kill-to-beginning-of-line)))

;; ;; (defun gnus-summary-move-to-spambox ()
;; ;;   "Move this article to the spambox."
;; ;;   (interactive)
;; ;;   (save-excursion
;; ;;     (gnus-summary-mark-as-expirable 1))
;; ;;   (gnus-summary-move-article 1 "nnml:spambox")
;; ;;   (forward-line 1))

;; ;; (add-hook 'gnus-group-mode-hook   'fc-dont-show-whitespace)
;; ;; (add-hook 'gnus-summary-mode-hook 'fc-dont-show-whitespace)
;; ;; (add-hook 'gnus-article-mode-hook 'fc-dont-show-whitespace)

;; ;; (defun fc-dont-show-whitespace ()
;; ;;   "Set `show-trailing-whitespace' to nil."
;; ;;   (setq show-trailing-whitespace nil))

;; ;; ;;; Main config

;; ;; (setq gnus-select-method '(nntp "news"))
;; ;; (setq gnus-secondary-select-methods
;; ;;       '((nnml ""
;; ;;               (nnml-directory "~/Mail/Lists")
;; ;;               (nnml-active-file "~/Mail/Lists/active")
;; ;;               (nnml-get-new-mail t))))
;; ;; (setq gnus-message-archive-method
;; ;;       '(nnfolder "archive"
;; ;; 		 (nnfolder-directory    "~/Mail/archive")
;; ;; 		 (nnfolder-active-file  "~/Mail/archive/active")
;; ;; 		 (nnfolder-get-new-mail nil)))
;; ;; (setq gnus-message-archive-group
;; ;;       '((if (message-news-p)
;; ;; 	    "misc-news"
;; ;; 	  (concat "mail." (format-time-string "%Y")))))

;; ;; (setq mail-sources '((file)))

;; ;; (add-hook 'message-header-setup-hook 'fc-fix-AW-subject)
;; ;; (defun fc-fix-AW-subject ()
;; ;;   (while (re-search-forward "^Subject: Re: \\(AW: \\)*" nil t)
;; ;;     (replace-match "Subject: Re: ")))

;; ;; (add-to-list 'mm-discouraged-alternatives "text/html")
;; ;; (add-to-list 'mm-discouraged-alternatives "image/")

;; ;; ;; Defeat Gnus' use of the Sender: header. I know what I am doing with
;; ;; ;; my From: header.
;; ;; (add-to-list 'message-syntax-checks '(sender . disabled))

;; ;; (setq nnmail-split-methods 'nnmail-split-fancy
;; ;;       nnmail-split-header-length-limit 4096
;; ;;       nnmail-use-long-file-names t
;; ;;       nnmail-crosspost nil)
;; ;; (setq gnus-add-to-list t
;; ;;       gnus-interactive-exit nil
;; ;;       gnus-save-newsrc-file nil
;; ;;       gnus-use-scoring t
;; ;;       ;; This is bogus when you q from a summary buffer: The *Group*
;; ;;       ;; buffer appears in another window.
;; ;;       ;; gnus-use-full-window nil
;; ;;       gnus-summary-default-score 0
;; ;;       gnus-summary-expunge-below -256
;; ;;       gnus-summary-make-false-roots 'dummy
;; ;;       gnus-score-expiry-days nil
;; ;;       gnus-home-score-file "~/Mail/gnus.SCORE"
;; ;;       gnus-agent-directory "~/Mail/agent/"
;; ;;       gnus-directory "~/Mail/News/"
;; ;;       gnus-article-save-directory "~/Mail/News"
;; ;;       gnus-cache-directory "~/Mail/News/cache"
;; ;;       gnus-cache-active-file "~/Mail/News/cache/active"
;; ;;       gnus-kill-files-direcotry "~/Mail/News"
;; ;;       nndraft-directory "~/Mail/drafts/"
;; ;;       gnus-default-article-saver 'gnus-summary-save-in-mail
;; ;;       gnus-save-killed-list nil
;; ;;       gnus-auto-expirable-newsgroups "^[^.]*$\\|mail"
;; ;;       gnus-ignored-mime-types '("text/x-vcard")
;; ;;       gnus-ignored-from-addresses "forcer\\|jorgen"
;; ;;       gnus-show-all-headers nil
;; ;;       gnus-treat-capitalize-sentences nil
;; ;;       gnus-treat-display-picons nil
;; ;;       gnus-treat-display-smileys nil
;; ;;       gnus-treat-display-x-face t
;; ;;       gnus-treat-emphasize nil
;; ;;       gnus-treat-fill-long-lines nil    ; 50000
;; ;;       gnus-treat-hide-signature nil
;; ;;       gnus-treat-overstrike nil
;; ;;       gnus-treat-play-sounds nil
;; ;;       gnus-treat-strip-banner nil
;; ;;       gnus-treat-strip-cr t
;; ;;       gnus-treat-strip-leading-blank-lines nil
;; ;;       gnus-treat-strip-multiple-blank-lines nil
;; ;;       gnus-treat-strip-pem nil
;; ;;       gnus-treat-strip-pgp nil
;; ;;       gnus-treat-strip-trailing-blank-lines nil
;; ;;       gnus-treat-translate nil)
;; ;; (setq gnus-visible-headers
;; ;;       '("^From:" "^Subject:" "^To:" "^Cc:" "^Resent-To:" "^Message-ID:"
;; ;;         "^Date:" "^Newsgroups:" "^Organization:" "Followup-To:"
;; ;;         "Reply-To:" "^X-Newsreader:" "^X-Mailer:"
;; ;;         "^X-Spam-Level:"))
;; ;; (setq gnus-sorted-header-list gnus-visible-headers)
;; ;; (setq gnus-extra-headers '(Newsgroups))
;; ;; (setq message-generate-headers-first t
;; ;;       message-insert-canlock nil
;; ;;       message-wash-forwarded-subjects t
;; ;;       message-make-forward-subject-function 'message-forward-subject-fwd
;; ;;       message-use-mail-followup-to 'use
;; ;;       message-subscribed-address-functions '(gnus-find-subscribed-addresses))
;; ;; ;; (setq spam-directory "~/Mail/spam"
;; ;; ;;       spam-mark-ham-unread-before-move-from-spam-group t
;; ;; ;;       spam-use-bogofilter-headers t
;; ;; ;;       spam-split-group "nnml:spambox"
;; ;; ;;       spam-junk-mailgroups '("nnml:spambox")
;; ;; ;;       gnus-spam-process-newsgroups
;; ;; ;;       '(("mail.*"
;; ;; ;;          (gnus-group-ham-exit-processor-bogofilter))
;; ;; ;;         ("spambox"
;; ;; ;;          (gnus-group-spam-exit-processor-bogofilter)))
;; ;; ;;       gnus-spam-newsgroup-contents '(("spambox"
;; ;; ;;                                       gnus-group-spam-classification-spam))
;; ;; ;;       spam-mark-only-unseen-as-spam t
;; ;; ;;       gnus-ham-process-destinations '(("spambox" "nnml:mail.misc"))
;; ;; ;;       gnus-spam-process-destinations '(("mail.*" "nnml:spambox"))
;; ;; ;;       gnus-parameter-ham-marks-alist '((".*"
;; ;; ;;                                         ((gnus-del-mark
;; ;; ;;                                           gnus-read-mark
;; ;; ;;                                           gnus-killed-mark
;; ;; ;;                                           gnus-kill-file-mark
;; ;; ;;                                           gnus-low-score-mark
;; ;; ;;                                           gnus-expirable-mark)))))
;; ;; (setq mail-source-delete-incoming t)

;; ;; ;; gnus-ifile config
;; ;; ; (require 'ifile-gnus)
;; ;; ; (setq ifile-classification-mode 'spam-filter-only)
;; ;; ; (setq ifile-spam-groups '("spambox" "nnml:spambox"))
;; ;; ; (setq ifile-primary-spam-group "spambox")
;; ;; ; 
;; ;; ; (defun gnus-article-classify ()
;; ;; ;   (interactive)
;; ;; ;   (let ((ifile-article-buffer (get-buffer "*Article*")))
;; ;; ;     (ifile-recommend)))

;; ;; (eval-after-load "gnus-art"
;; ;;   '(define-key gnus-mime-button-map (kbd "a") 'fc-gnus-darcs-apply-part))

;; ;; (defun fc-gnus-darcs-apply-part (repo)
;; ;;   "Apply the MIME part under point to a Darcs repository."
;; ;;   (interactive "DApply to Darcs repository: ")
;; ;;   (gnus-article-check-buffer)
;; ;;   (let ((data (get-text-property (point)
;; ;;                                  'gnus-data)))
;; ;;     (when data
;; ;;       (mm-with-unibyte-buffer
;; ;;         (mm-insert-part data)
;; ;;         (fc-send-region-to-command (point-min)
;; ;;                                    (point-max)
;; ;;                                    "darcs" "apply"
;; ;;                                    (format "--repodir=%s"
;; ;;                                            (expand-file-name repo))
;; ;;                                    "-a")))))

;; ;; (defun fc-send-region-to-command (beg end command &rest args)
;; ;;   "Call COMMAND with ARGS, and display output in a special buffer."
;; ;;   (let* ((coding-system-for-write 'binary)
;; ;;          (buf (with-current-buffer
;; ;;                   (get-buffer-create "*Shell Command Output*")
;; ;;                 (setq buffer-read-only nil)
;; ;;                 (erase-buffer)
;; ;;                 (current-buffer)))
;; ;;          (exit-status (apply 'call-process-region
;; ;;                              beg end
;; ;;                              command
;; ;;                              nil buf nil
;; ;;                              args)))
;; ;;     (with-current-buffer buf
;; ;;       (setq mode-line-process
;; ;;             (cond ((null exit-status)
;; ;;                    " - Error")
;; ;;                   ((stringp exit-status)
;; ;;                    (format " - Signal [%s]" exit-status))
;; ;;                   ((not (equal 0 exit-status))
;; ;;                    (format " - Exit [%d]" exit-status)))))
;; ;;     (if (with-current-buffer buf (> (point-max)
;; ;;                                     (point-min)))
;; ;;         ;; There's some output, display it
;; ;;         (display-message-or-buffer buf)
;; ;;       ;; No output; error?
;; ;;       (cond ((null exit-status)
;; ;;              (message "(Command failed with error)"))
;; ;;             ((equal 0 exit-status)
;; ;;              (message "(Command succeeded with no output)"))
;; ;;             ((stringp exit-status)
;; ;;              (message "(Command killed by signal %s)"
;; ;;                       exit-status))
;; ;;             (t
;; ;;              (message "(Command failed with code %d and no output)"
;; ;;                       exit-status output))))))
