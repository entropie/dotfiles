(when (featurep 'xemacs)
  (error "This .emacs file does not work with XEmacs."))

(when (file-directory-p "~/.emacs.d")
  (add-to-list 'load-path "~/.emacs.d"))

(when (file-directory-p "~/.emacs.d/mew")
  (add-to-list 'load-path "~/.emacs.d/mew"))
(when (file-directory-p "~/.emacs.d/howm")
  (add-to-list 'load-path "~/.emacs.d/howm"))
(when (file-directory-p "~/.emacs.d/epg/")
  (add-to-list 'load-path "~/.emacs.d/epg/"))
(when (file-directory-p "~/.emacs.d/nxml/")
  (add-to-list 'load-path "~/.emacs.d/nxml/"))
(when (file-directory-p "~/.emacs.d/haskell-mode/")
  (add-to-list 'load-path "~/.emacs.d/haskell-mode/"))
(when (file-directory-p "~/.emacs.d/emacs-w3m/")
  (add-to-list 'load-path "~/.emacs.d/emacs-w3m/"))
(when (file-directory-p "~/.emacs.d/magit/")
  (add-to-list 'load-path "~/.emacs.d/magit/"))
(when (file-directory-p "~/.emacs.d/color-theme/")
  (add-to-list 'load-path "~/.emacs.d/color-theme/"))
(when (file-directory-p "~/.emacs.d/ruby-mode/")
  (add-to-list 'load-path "~/.emacs.d/ruby-mode/"))
(when (file-directory-p "~/.emacs.d/emacs-goodies/")
  (add-to-list 'load-path "~/.emacs.d/emacs-goodies//"))


;;(require 'magit)

(require 'paredit)
(defun lisp-enable-paredit-hook () (paredit-mode 1))
(require 'clojure-mode)
(add-hook 'clojure-mode-hook 'lisp-enable-paredit-hook)


;; (add-to-list 'load-path
;;              "~/.emacs.d/yasnipped")
;; (require 'yasnippet)
;; (yas/initialize)
;; (yas/load-directory "~/.emacs.d/yasnipped/snippets")


(add-to-list 'load-path "~/.emacs.d/flog/")

(require 'rainbow-mode)

(when (file-exists-p "~/.private.el")
  (load-file "~/.private.el"))

(add-to-list 'vc-handled-backends 'GIT)

(set-default 'truncate-1lines t)
(setq 
 nopaste-facility   "~/bin/gist"
 mt-is-default-font t
 ;;mt-default-font     "-*-terminus-medium-*-*-*-20-*-*-*-*-*-*-*"
 mt-default-font     "-*-Monaco-normal-r-*-*-12-102-120-120-c-*-iso8859-1"
 mt-other-font       "-*-unifont-*-*-*-*-16-*-*-*-*-*-*-*"
 mt-rcirc-font       "-*-helvetica-medium-r-*-*-17-*-*-*-*-*-*-*"
 )

(require 'filladapt)
(require 'mt-howm)
(require 'haskell-mode)
(require 'mt-functions)


(require 'cheat)
(require 'bitlbee)
(setq bitlbee-executable nil)

;; (autoload 'mldonkey "mt-mldonkey.el" "mldonkey" t)
;; (autoload 'setnu-mode "setnu.el" "setnu" t)

(setq frame-title-format '("" invocation-name ": %b") icon-title-format "%b")


(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)
(setq auto-mode-alist
   (cons '("\\.markdown" . markdown-mode) auto-mode-alist))
(add-hook 'markdown-mode-hook 'mt-markdown-setup)

(global-set-key (kbd "M-?") (lambda () (interactive) (insert "\\")))
(global-set-key (kbd "M-7") (lambda () (interactive) (insert "{")))
(global-set-key (kbd "M-8") (lambda () (interactive) (insert "[")))
(global-set-key (kbd "M-9") (lambda () (interactive) (insert "]")))
(global-set-key (kbd "M-0") (lambda () (interactive) (insert "}")))
(global-set-key (kbd "M-1") (lambda () (interactive) (insert "|")))
(global-set-key (kbd "M-2") (lambda () (interactive) (insert "@")))
(global-set-key (kbd "M-3") (lambda () (interactive) (insert "~")))
(global-set-key (kbd "M-4") (lambda () (interactive) (insert "<")))
(global-set-key (kbd "M-5") (lambda () (interactive) (insert ">")))
;; (prefer-coding-system 'latin-1)
;; (if (not (assoc "UTF-8" language-info-alist))
;;     (set-language-environment "latin-1")
;;   (set-language-environment "utf-8")
;;   (set-keyboard-coding-system 'utf-8)
;;   (set-terminal-coding-system 'utf-8)
;;   (prefer-coding-system 'utf-8))

(defalias 'yes-or-no-p 'y-or-n-p)

(setq-default 
 display-time-load-average   nil
 display-time-interval       30
 display-time-use-mail-icon  nil
 require-final-newline 1
 indent-tabs-mode nil
 default-major-mode 'text-mode
 even-window-heights nil
 resize-mini-windows nil
 sentence-end-double-space nil
 display-time-24hr-format t
 browse-url-browser-function 'mt-choose-browser
 default-tab-width 8
 scroll-preserve-screen-position 'keep
 user-mail-address "mictro@gmail.com"
 user-full-name "Michael Trommer"
 inhibit-startup-message t
 diff-switches "-c"
 comment-style 'extra-line
 case-fold-search t
 read-file-name-completion-ignore-case t
 completion-ignore-case t
 cursor-in-non-selected-windows nil
 x-stretch-cursor t
 mouse-yank-at-point t
 mouse-highlight 1
 highlight-parentheses-mode 1
 )
(add-hook 'before-save-hook 'time-stamp)


(if (file-directory-p "~/.backup")
    (setq backup-directory-alist '(("." . "~/.backup")))
  (message "Directory does not exist: ~/.backup"))

(setq dabbrev-case-fold-search nil
      confirm-kill-emacs 'yes-or-no-p)


;; Backups 
(setq backup-by-copying t
      delete-old-versions t
      version-control t
      kept-new-versions 300
      kept-old-versions 200)
(set-frame-parameter (selected-frame) 'active-alpha 0.54)
(setq mac-transparency-alpha 84)


(setq w3m-default-save-directory "~/Downloads"
     kept-old-versions 200)

(require 'w3m)
(require 'url)
(require 'fit-frame)
(setq fit-frame-min-height 100)
(setq fit-frame-min-width 100)
;;(require 'autofit-frame)
;;(add-hook 'after-make-frame-functions 'fit-frame)

(global-set-key (kbd "C-c i") 'mt-increment-number-at-point)
(global-set-key (kbd "C-c M") 'mt-show-message-buffer)
(global-set-key (kbd "C-%") 'mt-match-paren)

(global-set-key (kbd "C-c l") 'mt-goto-last-edit)

(global-set-key (kbd "C-c TAB") 'indent-relative)
(global-set-key (kbd "C-c C-f") 'find-file-root)
(global-set-key (kbd "C-x n") nil)
(global-set-key (kbd "C-x n '") (lambda () (interactive) (insert "’")))
(global-set-key (kbd "C-x n p") (lambda () (interactive) (insert "平和")) )
(global-set-key (kbd "C-x n k") (lambda () (interactive) (insert "ĸ")) )
(global-set-key (kbd "C-x n g") (lambda () (interactive) (insert "&gassi;")) ) ;
(global-set-key (kbd "C-x n h") (lambda () (interactive) (insert "♥")) )
(global-set-key (kbd "C-x n m") (lambda () (interactive) (insert "훗")) )

(global-set-key (kbd "C-c , ,") 'howm-menu)
(global-set-key (kbd "C-c , G") 'howm-refresh)
(global-set-key (kbd "C-c e") 'mt-eshell-here)

(define-key isearch-mode-map (kbd "C-o") 'mt-isearch-occur)

(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-c C-k") 'kill-region)

(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "M-m ") 'minimap-toggle)
(global-set-key (kbd "M-M ") 'minimap-kill)


(require 'bs)
(setq bs-configurations (butlast bs-configurations 2))
(add-to-list 'bs-configurations
             '("files" nil nil nil bs-visits-non-file bs-sort-buffer-interns-are-last))
(add-to-list 'bs-configurations
             '("ruby" nil nil nil (lambda (buf) (with-current-buffer buf (not (memq major-mode '(ruby-mode))))) nil))
(add-to-list 'bs-configurations 
             '("dired" nil nil nil (lambda (buf) (with-current-buffer buf (not (eq major-mode 'dired-mode)))) nil))
(add-to-list 'bs-configurations
             '("mail" nil nil nil (lambda (buf) (with-current-buffer buf (not (memq major-mode '(mew-draft-mode mew-message-mode mew-summary-mode))))) nil))


(global-set-key (kbd "C-x C-b") 'bs-show)
;;(global-set-key (kbd "C-c O") (lambda () (interactive) (make-frame-on-display ":0.0")))
(global-set-key (kbd "C-c C-u") 'mt-kill-to-beginning-of-line)
(global-set-key (kbd "C-c d")   'mt-insert-date)
(global-set-key (kbd "C-c D")   'mt-insert-any-date)

(require 'mt-encryption)
(require 'mt-rcirc)
(global-set-key (kbd "M-.") 'hippie-expand)
(global-set-key (kbd "C-x I") 'mt-indent-buffer)
(global-set-key (kbd "M-RET") 'comment-indent-new-line)
(global-set-key (kbd "C-x t") 'mt-transpose-windows)
(global-set-key (kbd "C-x j") 'join-line)
(global-set-key (kbd "C-c g") 'goto-line)
(global-set-key (kbd "C-c G") 'goto-char)
(global-set-key (kbd "<f1>")  'mt-occur) ; grep buffers
(global-set-key (kbd "C-c b") 'browse-url)
(global-set-key (kbd "C-c G") 'mt-google)
(global-set-key (kbd "C-c B") 'browse-url-at-point)

(global-set-key (kbd "C-t")   nil)

(autoload 'bm-toggle "bm" "bm" t)
(autoload 'highline-mode "highline" nil t)
(autoload 'highline-local-mode "highline" nil t)

(global-set-key (kbd "M-,")   'ispell-complete-word)
(global-set-key (kbd "C-t d") 'mt-dict-cc)
(global-set-key (kbd "C-t D") 'google-define)
(global-set-key (kbd "C-t t") 'bm-toggle)

(global-set-key (kbd "C-t n") 'bm-next)
(global-set-key (kbd "C-t p") 'bm-previous)
(global-set-key (kbd "C-t P") 'mt-nopaste-region)
(global-set-key (kbd "C-t c") 'bm-remove-all)
(global-set-key (kbd "C-t i") 'irc)
(global-set-key (kbd "C-t C") 'cheat)

(global-set-key (kbd "C-t h") 'highline-local-mode)
(global-set-key (kbd "C-t H") 'highlight-changes-mode)
(global-set-key (kbd "C-t C-l") 'ielm)


(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c C") 'mt-switch-dictionarry)
(global-set-key (kbd "C-x i") 'mt-insert-userid)
(global-set-key (kbd "C-x S") 'mt-insert-signature)


(global-set-key (kbd "C-x E") 'mt-insert-excuse)
(global-set-key (kbd "C-x p") 'mt-insert-mpd-np)

(global-set-key (kbd "C-x a") 'abbrev-mode)
(global-set-key (kbd "C-x F") 'mt-toggle-font)
(global-set-key (kbd "C-x U") 'mt-toggle-utf8-latin1)

(require 'hideshow)
(define-key hs-minor-mode-map (kbd "C-c C-e") 'hs-show-block)
(define-key hs-minor-mode-map (kbd "C-c C-c") 'hs-toggle-hiding)
(define-key hs-minor-mode-map (kbd "C-c C-x") 'hs-hide-block)
(define-key hs-minor-mode-map (kbd "C-c C-t") 'hide-body)
(define-key hs-minor-mode-map (kbd "C-c C-a") 'hs-show-all)
(define-key hs-minor-mode-map (kbd "C-c C-h") 'hs-hide-all)
(define-key hs-minor-mode-map (kbd "C-c C-l") 'hs-hide-level)

(global-set-key (kbd "M-_") 'hippie-expand)
(global-set-key (kbd "C-x *") 'isearch-current-symbol)
(global-set-key (kbd "C-x c") 'mt-line-comment-and-duplicate)
(global-set-key (kbd "C-x C-y") nil)
(global-set-key (kbd "C-c C-y") 'escreen-goto-last-screen)


(iswitchb-mode 1)
(when (and (featurep 'tool-bar)  window-system)
  (tool-bar-mode -1))
(when (fboundp 'blink-cursor-mode)
  (blink-cursor-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

(setq display-time-day-and-date nil
      display-time-use-mail-icon t
      )
(setq mark-even-if-inactive t)
(random t)
(setq show-paren-delay 0
      show-paren-style 'parenthesis
      hl-paren-colors '("LawnGreen" "SpringGreen"
                        "chartreuse", "YellowGreen"))



;;(setq-default ispell-dictionary "german8")

(setq ispell-program-name "/opt/local/bin/aspell")
(eval-after-load "ispell"
  '(add-to-list 'ispell-dictionary-alist
                '("german8"
                  "[a-zA-ZäöüßÄÖÜ]" "[^a-zA-ZäöüßÄÖÜ]" "[']" t
                  ("-C" "-d" "german")
                  "~latin1" iso-8859-1)))

(eval-after-load "flyspell"
  ;;'(define-key flyspell-mode-map "\M-\t" 'ispell-word)
  '(add-to-list 'ispell-dictionary-alist
                '("german8"
                  "[a-zA-ZäöüßÄÖÜ]" "[^a-zA-ZäöüßÄÖÜ]" "[']" t
                  ("-C" "-d" "german")
                  "~latin1" iso-8859-1)))

(add-hook 'eshell-mode-hook
          '(lambda () (define-key eshell-mode-map "\C-a" 'mt-eshell-maybe-bol)))

(add-to-list 'auto-mode-alist '("\\.ht$" . nxml-mode))


(require 'textile-mode)
(add-to-list 'auto-mode-alist '("\\.textile\\'" . textile-mode))

(add-hook 'nxml-mode-hook 'mt-html-setup)
(autoload 'nxml-mode "nxml/autostart.el" "nxml/autostart.el" t)
(define-abbrev-table 'global-abbrev-table '(
                                            ("alpha" "α" nil 0)
                                            ("beta" "β" nil 0)
                                            ("gamma" "γ" nil 0)
                                            ("theta" "θ" nil 0)
                                            ("kwiki" "http://wiki.kommunism.us/" nil 0)
                                            ))

(set-register ?e '(file . "~/.emacs"))


(require 'mt-unicode)

(if (boundp 'mail-user-agent)
    (setq mail-user-agent 'mew-user-agent))
(if (fboundp 'define-mail-user-agent)
    (define-mail-user-agent
      'mew-user-agent
      'mew-user-agent-compose
      'mew-draft-send-message
      'mew-draft-kill
      'mew-send-hook))
(autoload 'mew "mt-mew" "autoload mew." t)
(autoload 'compose-mail "mt-mew" "autoload mew." t)
(autoload 'mew-user-agent-compose "mew" nil t)

;;; Haskell
(add-to-list 'auto-mode-alist '("\\.[hg]s$"   . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.hi$"      . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.l[hg]s$"  . literate-haskell-mode))
(autoload 'haskell-mode "haskell-mode"
  "Major mode for editing Haskell scripts" t)
(autoload 'literate-haskell-mode "haskell-mode"
  "Major mode for editing literate Haskell scripts" t)
(autoload 'run-ghci "haskell-ghci"
  "Go to the *ghci* buffer" t nil)
;;(set-variable 'haskell-program-name "ghci")
(defalias 'run-haskell (quote switch-to-haskell))
(autoload (quote switch-to-haskell) "inf-haskell"
  "Show the inferior-haskell buffer.  Start the process if needed." t nil)

(add-hook 'haskell-mode-hook 'mt-haskell-setup)

(autoload 'highlight-parentheses-mode "highlight-parentheses"
  "highlight parentheses mode" t)



(require 'fillcode)
(autoload 'fillcode "fillcode" "fillcode" t)
(setq mudel-truncate-buffer-size (* 256 1024))

(mouse-avoidance-mode 'exile)
      
(setq pcomplete-cycle-completions nil)
                                  
(setq shell-prompt-pattern "^[^#$%\n]*[#$%*>] *")


(setq tramp-default-method "ssh")
(when (file-directory-p "~/.tramp-auto-save-directory")
  (setq tramp-auto-save-directory "~/.tramp-auto-save-directory"))

(autoload 'w3m-browse-url "w3m" "Ask emacs-w3m to browse URL." t)

(add-hook 'w3m-mode-hook 'mt-w3m-setup)
(add-hook 'w3m-form-input-textarea-mode-hook 'mt-remove-cr)

;; This might help in saving cookies
(eval-after-load "w3m"
  '(progn
     (add-hook 'kill-emacs-hook
               (lambda ()
                 (w3m-quit t)))))

(if (display-mouse-p) (mouse-avoidance-mode 'animate))

(setq hippie-expand-try-functions-list '(try-expand-dabbrev-visible
                                         try-complete-file-name
                                         try-expand-dabbrev-from-kill
                                         try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-complete-file-name))

(global-set-key [insert] (function (lambda () (interactive) (message "Sorry, overwrite mode has been disabled forever."))))
(global-set-key [insertchar] (function (lambda () (interactive) (message "Sorry, overwrite mode has been disabled forever."))))
(global-set-key (kbd "C-z") (function (lambda () (interactive) (message "Sorry, no icons..."))))


(autoload 'yaml-mode "/home/mit/.emacs.d/yaml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))

(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
(autoload 'css-mode "css-mode" "CSS mode" t)
(setq cssm-indent-function #'cssm-c-style-indenter)

(autoload 'wdired "wdired" "wdired" t)


(require 'mt-dired)
(require 'google-define)

;; (require 'gist)


(require 'mt-ruby)
(autoload 'ri "~/.emacs.d/ri-ruby.el" nil t)
(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby" "Set local key defs for inf-ruby in ruby-mode")
(setq ri-ruby-script "~/.emacs.d/ri-emacs.rb")
(autoload 'ruby-electric-mode "ruby-electric" "ruby-electric")

(require 'snippet)

(define-abbrev-table 'mt-ruby-mode-abbrev-table '())
(define-abbrev-table 'mt-rcirc-mode-abbrev-table '())

;; (snippet-with-abbrev-table 'mt-rcirc-mode-abbrev-table
;;                            ("cop" . "/mode #ccc +o $${nick}")
;;                            ("cdop" . "/mode #ccc -o $${nick}"))


;;(add-hook 'ruby-mode-hook 'my-ruby-compile-hook) 

(autoload 'ruby-mode "ruby-mode" "Ruby mode" t)
(autoload 'toggle-buffer "toggle" "toggle" t)
(autoload 'toggle-style "toggle" "toggle" t)
(add-to-list 'auto-mode-alist '("\\.rbx?$" . ruby-mode))
(add-hook 'howm-view-contents-mode-hook 'mt-howmc-setup)
(add-hook 'ruby-mode-hook 'mt-ruby-setup)
(add-hook 'yaml-mode-hook 'mt-yaml-setup)



(autoload 'dylan-mode "dylan-mode" "Major mode for Dylan source" t)
(setq auto-mode-alist
      (cons '("\\.dylan$" . dylan-mode) auto-mode-alist))
(add-hook 'dylan-mode-hook 'mt-dylan-setup)


(setq auto-mode-alist
      (append '(("\\.php$" . php-mode)
                ("\\.c$"   . c-mode)
                ("\\.htm$" . nxml-mode)
                ("\\.html$". nxml-mode)
                ("\\.rb$"  . ruby-mode)
                ("\\.rbx$" . ruby-mode)
                ("\\.hs$"  . haskell-mode)
                ("\\.rd$"  . rd-mode)
                ("\\.rdoc$"  . rd-mode)
                ("\\.clj$"  . clojure-mode)
                ("\\.howm$" . howm-mode))
              auto-mode-alist))


;;; various minor modes
(dolist (i '((auto-image-file-mode 1)
             (global-auto-revert-mode 1)
             (line-number-mode -1)
             (display-time-mode 1)
             (column-number-mode 1)
             (show-paren-mode 1)
             (winner-mode 1)
             (tooltip-mode -1)
             (size-indication-mode 1)
             (transient-mark-mode 0)
             (global-font-lock-mode 1)
             (auto-compression-mode 1)
             ))
  (when (fboundp (car i))
    (funcall (car i) (cdr i))))

(autoload 'autoinsert "autoinsert" "Automaticaly headers for files")
(add-hook 'find-file-hooks 'auto-insert)
(setq auto-insert-directory (expand-file-name "~/.emacs.d/autoinsert/"))
(setq auto-insert-query nil)
(define-auto-insert "\\.rb\\'"         "ruby")


(require 'color-theme)
(load "color-theme-bmate.el")
(load "color-theme-nox.el")

(require 'modeline-posn)
(require 'minimap)

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (set-variable 'color-theme-is-global nil)
            (select-frame frame)
            (when window-system
              (color-theme-bmate))
            (when (not window-system)
              (color-theme-nox))))

(when (not window-system)
  (message "no X")
  (load "color-theme-nox")
  (setq server-name "mit")
  (when (or (not (boundp 'server-process))
            (not (eq (process-status server-process)
                     'listen)))
    (server-start)))


(when window-system
  (set-face-font 'default mt-default-font)
  (color-theme-initialize)
  (color-theme-bmate)
  (setq favorite-color-themes
        '((color-theme-bmate)
          (color-theme-dd)
          (color-theme-pop)))
  (setq x-select-enable-clipboard t
        interprogram-paste-function 'x-cut-buffer-or-selection-value))

  

(require 'cursor-chg)
(change-cursor-mode 1)         ; On for overwrite/read-only/input mode
(toggle-cursor-type-when-idle 5)        ; On when idle
(setq curchg-default-cursor-type 'bar)
(setq curchg-default-cursor-color "#FF8000")
(setq curchg-input-method-cursor-color "yellow")
(require 'saveplace)
(setq-default save-place t)


;;; escreen
(setq escreen-prefix-char (kbd "C-c a")
      escreen-new-screen-default-buffer "Code")
(require 'escreen)
(escreen-install)

(font-lock-add-keywords 
 'ruby-mode
 '(("\\<\\(FIXME\\|TODO\\):" 1 font-lock-warning-face prepend)))


(put 'narrow-to-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'downcase-region 'disabled nil)

(autoload 'js2-mode "js2" nil t) 
(add-to-list 'auto-mode-alist'("\\.js$" . js2-mode))

(setq js2-basic-offset 2) 
(setq js2-bounce-indent-p t) 
(setq js2-use-font-lock-faces t)

(setq js2-allow-keywords-as-property-names nil)
(setq js2-auto-indent-p nil)
(setq js2-basic-offset 0)
(setq js2-bounce-indent-p t)
(setq js2-cleanup-whitespace t)
(setq js2-enter-indents-newline nil)
(setq js2-global-externs (quote ("algjs" "$" "goog" "YAHOO" "jQuery")))
(setq js2-highlight-level 3)
(setq js2-idle-timer-delay 0.2)
(setq js2-indent-on-enter-key nil)
(setq js2-mirror-mode nil)


(autoload 'rd-mode "rd-mode" nil t)

(setq w3m-coding-system 'utf-8
      w3m-file-coding-system 'utf-8
      w3m-file-name-coding-system 'utf-8
      w3m-input-coding-system 'utf-8
      w3m-output-coding-system 'utf-8
      w3m-terminal-coding-system 'utf-8)

(autoload 'haml-mode "haml-mode" nil t)
(autoload 'sass-mode "sass-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))
(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))
(add-hook 'sass-mode-hook 'mt-sass-setup)
(add-hook 'haml-mode-hook 'mt-haml-setup)


(autoload 'unicode-helper-mode "unicode-helper-mode" nil t)


;; show char name, used by C-u C-x = 
(let ((x "~/.emacs.d/UnicodeData.txt"))
  (when (file-exists-p x)
    (setq describe-char-unicodedata-file x)))

(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "|")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(paren-match-face (quote paren-face-match-light))
 '(paren-sexp-mode t)
 '(safe-local-variable-values (quote ((ruby-indent-level . 2)))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
