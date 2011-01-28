
;; emacs-w3m is in ~/elisp
(setq w3m-icon-directory "~/emacs.d/emacs-w3m/icons")
(require 'w3m-load)

;; env settings for w3m

(setq w3m-coding-system 'utf-8
      w3m-language 'utf-8
      w3m-file-coding-system 'utf-8
      w3m-file-name-coding-system 'utf-8
      w3m-input-coding-system 'utf-8
      w3m-output-coding-system 'utf-8
      w3m-terminal-coding-system 'utf-8)

;;w3m default browser
(setq browse-url-browser-function 'w3m-browse-url)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)

(global-set-key (kbd "<f7> j") 'webjump) 


 (defun my-w3m-switch-to-buffer (arg)
   "Select the ARG'th w3m buffer."
   (interactive "p")
   (let (buf)
     (if (= arg 0)
	 (setq arg 10)
       (setq arg (1- arg)))
     (if (and (> arg -1) (setq buf (nth arg (w3m-list-buffers))))
	 (progn
	   (switch-to-buffer buf)
	   (run-hooks 'w3m-select-buffer-hook)
	   (w3m-select-buffer-update))
       (message "No such buffer!"))))
 (add-hook 'w3m-mode-hook
	   (lambda ()
	     (dolist (bufnum '(0 1 2 3 4 5 6 7 8 9))
	       (let* ((bufstr (number-to-string bufnum))
		      (funcname (concat "my-w3m-switch-to-buffer-" bufstr)))
		 (eval `(defun ,(intern funcname) ()
			  (interactive)
			  (my-w3m-switch-to-buffer ,bufnum)))
		 (define-key w3m-mode-map bufstr
		   (intern funcname))))))

;;webjump sites
(setq webjump-sites
      '(("pythonlib" .  "http://docs.python.org/lib/genindex.html")
        ("pythondoc" . "http://docs.python.org/index.html")
        ("gentooforumfr" .  "http://www.gentoo.fr/forum/index.php")
        ("gentoowikifr" . "http://fr.gentoo-wiki.com/Accueil")
        ("gentoosearch" . "http://cosearch.googlepages.com/GentooSearchFrancais.html")
        ("delicious" . "http://del.icio.us/thiedlecques")
        ("gentooman" . "http://www.gentoo.org/doc/fr/handbook/handbook-amd64.xml?full=1#book_part1")
        ("emacsmanfr" . "http://www.linux-france.org/article/appli/emacs/manuel/html/index.html")
        ("emacswiki" . "http://www.emacswiki.org/cgi-bin/wiki/SiteMap")
        ("wikipedia" . "http://fr.wikipedia.org/wiki/Accueil")
        ("planner" . "http://wjsullivan.net/static/doc/planner/html_node/index.html")
        ("elispcode" . "http://www.emacswiki.org/cgi-bin/wiki/Cat%c3%a9gorieCode")
        ("elisp-reference-manual" . "http://www.gnu.org/software/emacs/elisp/html_node/index.html")
        ("developpez-faq" . "http://python.developpez.com/faq/")
        ))

;;enable cookies in w3m
(setq w3m-use-cookies t)
(setq w3m-cookie-accept-bad-cookies t)
(setq browse-url-netscape-program '"firefox")

(setq w3m-use-tab t)
(setq w3m-use-tab-menubar t)

(defun mt-w3m-setup-keymap ()
  "Use my heavily customized map."
  (interactive)
  ;; Undefine this key and use the advice instead so that my ido doesn't get
  ;; overridden
  (define-key w3m-mode-map (kbd "C-x b") nil)
  (define-key w3m-mode-map "C" 'w3m-print-this-url)
  (define-key w3m-mode-map "A" 'w3m-bookmark-add-current-url)
  (define-key w3m-mode-map "w" 'w3m-download-with-wget)
  (define-key w3m-mode-map "d" 'w3m-download-with-wget)
  (define-key w3m-mode-map "D" 'w3m-download-this-url)
  ;; Do not override my ever so handy rcirc binding
  (define-key w3m-mode-map (kbd "C-c C-SPC") nil)
  (define-key w3m-mode-map "s" 'w3m-search)
  (define-key w3m-mode-map "h" 'w3m-history)
  (define-key w3m-mode-map "t" 'w3m-scroll-down-or-previous-url)
  (define-key w3m-mode-map "n" 'w3m-scroll-up-or-next-url)
  (define-key w3m-mode-map (kbd "C-c b") 'iswitchb-buffer)
  ;; I don't often w3m to edit pages, so I'm borrowing o and e (right
  ;; below , / . for tab navigation) for page navigation instead.
  (define-key w3m-mode-map "o" 'w3m-view-previous-page)
  (define-key w3m-mode-map "e" 'w3m-view-next-page)
  ;; i is a more useful mnemonic for toggling images
  (define-key w3m-mode-map "i" 'w3m-toggle-inline-image)
  (define-key w3m-mode-map "I" 'w3m-toggle-inline-images)
  ;; and X for closing the buffer
  (define-key w3m-mode-map "X" 'w3m-delete-buffer)
  (define-key w3m-mode-map "x" 'w3m-delete-buffer)
  (define-key w3m-mode-map "z" 'w3m-delete-buffer)
  ;; and b for bookmarks
  (define-key w3m-mode-map "b" 'w3m-bookmark-view)
  ;; Browse in new sessions by default
  (define-key w3m-mode-map (kbd "RET") 'w3m-view-this-url-new-session)
  (define-key w3m-mode-map [(shift return)] 'w3m-view-this-url)
  (define-key w3m-mode-map "g" 'w3m-goto-url)
  (define-key w3m-mode-map "G" 'w3m-goto-url-new-session)
  ;; f for forward? I want to be able to follow links without removing
  ;; most of my fingers from home row. My fingers are too short to hit
  ;; Enter.
  (define-key w3m-mode-map "f" 'w3m-view-this-url-new-session)
  (define-key w3m-mode-map "F" 'w3m-view-this-url)
  ;; Use cursor keys to scroll
  (define-key w3m-mode-map [(left)] 'backward-char)
  (define-key w3m-mode-map [(right)] 'forward-char)
  (define-key w3m-mode-map [(shift left)] 'w3m-shift-right)
  (define-key w3m-mode-map [(shift right)] 'w3m-shift-left)
  ;; Which means I can now use , and . to switch pages
  (define-key w3m-mode-map "." 'w3m-next-buffer)
  (define-key w3m-mode-map "," 'w3m-previous-buffer)
  )


(add-hook 'w3m-mode-hook 'mt-w3m-setup-keymap)
(mt-w3m-setup-keymap)

 (defun w3m-new-tab ()
   (interactive)
   (w3m-copy-buffer nil nil nil t))
 (defun w3m-browse-url-new-tab (url &optional new-session)
   (interactive)
   (w3m-new-tab))

(defun mt-w3m-download-with-wget (loc)
  (interactive "DSave to: ")
  (let ((url (or (w3m-anchor) (w3m-image))))
    (if url
        (let ((proc (start-process "wget" (format "*wget %s*" url)
                                   "wget" "--passive-ftp" "-nv" 
                                   "-P" (expand-file-name loc) url)))
          (with-current-buffer (process-buffer proc)
            (erase-buffer))
          (set-process-sentinel proc (lambda (proc str)
                                       (message "wget download done"))))
      (message "Nothing to get"))))
(defun mt-w3m-download-with-curl (loc)
  (define-key w3m-mode-map "c"
    (lambda (dir)
      (interactive "DSave to: ")
      (cd dir)
      (start-process "curl" "*curl*" "curl.exe" "-O" "-s" (w3m-anchor)))))

(defun mt-w3m-browse-current-buffer ()
  (interactive)
  (let ((filename (concat (make-temp-file "w3m-") ".html")))
    (unwind-protect
        (progn
          (write-region (point-min) (point-max) filename)
          (w3m-find-file filename))
      (delete-file filename))))

