;;; my-mew.el --- my mew

(add-to-list 'load-path "/usr/share/emacs/site-lisp/mew")
(require 'bbdb)

(bbdb-initialize)
(setq bbdb-north-american-phone-numbers nil)
(setq bbdb-default-country "Germany")

(autoload 'bbdb         "bbdb-com" "Insidious Big Brother Database" t)
(autoload 'bbdb-name    "bbdb-com" "Insidious Big Brother Database" t)
(autoload 'bbdb-company "bbdb-com" "Insidious Big Brother Database" t)
(autoload 'bbdb-net     "bbdb-com" "Insidious Big Brother Database" t)
(autoload 'bbdb-notes   "bbdb-com" "Insidious Big Brother Database" t)

(setq bbdb-use-pop-up nil)
(setq bbdb/mail-auto-create-p nil)
(setq signature-use-bbdb t)
(setq bbdb-north-american-phone-numbers-p nil)
(add-hook 'bbdb-notice-hook 'bbdb-auto-notes-hook)
(setq bbdb-auto-notes-alist '(("X-ML-Name" (".*$" ML 0))))
(setq bbdb-complete-name-allow-cycling t)
(setq bbdb-expand-mail-aliases t)
(setq bbdb-use-alternate-names t)
(setq bbdb-complete-name-allow-cycling)
(setq bbdb-display-layout 'one-line)

;;     To actually define the aliases which are stored in the BBDB, call the
;;     function `bbdb-define-all-aliases' from your `mail-setup-hook' (or
;;     `message-setup-hook' if you use Message mode coming with Gnus).  This
;;     will search the database, and call `define-mail-alias' to define each of
;;     the resulting aliases.

;(add-hook 'message-setup-hook 'bbdb-define-all-aliases)




;(add-hook 'bbdb-define-all-aliases 'mt-mail-mail-setup)
(require 'mew)
(require 'bbdb-hooks)
(require 'bbdb-mew)
;(autoload 'bbdb-insinuate-mew      "bbdb-mew"   "Hook BBDB into Mew")


(setq mew-summary-form
      '(type (5 date) "  " (-4 size) " " (14 from) " " t (50 subj) " | " (0 body)))

(if (boundp 'mail-user-agent)
    (setq mail-user-agent 'mew-user-agent))
(if (fboundp 'define-mail-user-agent)
    (define-mail-user-agent
      'mew-user-agent
      'mew-user-agent-compose
      'mew-draft-send-message
      'mew-draft-kill
      'mew-send-hook))

(setq mew-mailbox-type 'maildir)

;; ----------------------------------------------- ;;
;; insert signature
(setq mew-signature-insert-last t)
(setq mew-signature-as-lastpart t)
;(add-hook 'mew-before-cite-hook 'mew-header-goto-body)
;(add-hook 'mew-draft-mode-newdraft-hook 'my-mew-draft-append-signature)
(defun my-mew-draft-append-signature ()
  (let ((mew-signature-insert-last t))
    (mew-draft-insert-signature)))
;; ----------------------------------------------- ;;

(setq mew-addrbook-file "~/.mew-addr")

;; look and feel
;;(setq mew-decode-broken nil)
(setq mew-window-use-full t)
(setq mew-demo nil)
(setq mew-use-biff t)
(setq mew-underline-lines-use t)
(setq mew-use-fancy-thread t)
(setq mew-use-fancy-highlight-body t)
(setq mew-fancy-highlight-body-prefix-width 10)
(setq mew-highlight-body-regex-comment "^[;#?%]+.*")
(setq mew-prog-imls-arg-list '("--thread=yes" "--indent=2"))
(setq mew-use-highlight-cursor-line t)
(setq mew-highlight-cursor-line-face 'underline)

(setq mew-summary-form
      '(type (5 date) " " (14 from) " " t (30 subj) "|" (0 body)))
(setq mew-sort-default-key "x-date-count")

;; ----------------------------------------------- ;;
;; Auto-refile
(defvar mew-refile-guess-control
  '(mew-refile-guess-by-alist
    mew-refile-ctrl-auto-boundary
    mew-refile-ctrl-throw
    mew-refile-guess-by-from
    mew-refile-ctrl-throw
    mew-refile-guess-by-thread
    mew-refile-ctrl-throw
    mew-refile-guess-by-default)
  )

;; 'body --> reply before citation
;; 'end  --> reply after citation
(setq mew-summary-reply-with-citation-position 'body)
(setq mew-cite-hook 'sc-cite-original)
(setq sc-preferred-header-style 5)
(setq sc-auto-fill-region-p t)

(provide 'mt-mew)
