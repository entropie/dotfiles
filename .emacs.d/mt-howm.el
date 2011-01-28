(require 'howm)

(defvar my-howm-buffer-name-limit 20)
(defvar my-howm-buffer-name-total-limit my-howm-buffer-name-limit)

(defun my-howm-truncate-string (string limit &optional dots-str)
  "Truncate STRING if it is longer than LIMIT.
For example, \"1234567...\" is returned if string is \"123456789012\"
and limit is 10.
When DOTS is non-nil, it is used instead of \"...\"."
  (let ((dots (or dots-str "...")))
    (when (> (length dots) limit)
      (setq dots (substring dots 0 limit)))
    (if (> (length string) limit)
        (concat (substring string 0 (- limit (length dots)))
                dots)
      string)))

(defun my-howm-mode-set-buffer-name ()
  "Set the buffer name to the title at the top of the file."
  (when (and howm-mode (buffer-file-name))
    (save-excursion
      (goto-char 0)
      (let ((titles nil))
        (while (re-search-forward howm-view-title-regexp nil t)
          (setq titles
                (cons (match-string-no-properties howm-view-title-regexp-pos)
                      titles))) 
        (let ((name (mapconcat  
                     (lambda (s)
                       (my-howm-truncate-string s
                                                my-howm-buffer-name-limit))
                     (reverse (howm-cl-remove-if (lambda (s) (string= s ""))
                                                 titles))
                     "/")))
          (when (not (string= name ""))  ;; exclude "no title" case
            (rename-buffer (my-howm-truncate-string
                            name
                            my-howm-buffer-name-total-limit)
                       t)))))))

(add-hook 'howm-mode-hook 'my-howm-mode-set-buffer-name)
(add-hook 'after-save-hook 'my-howm-mode-set-buffer-name)


(setq howm-directory "~/.howm")
(define-key howm-mode-map (kbd "C-c , x") 'mt-kill-file-and-buffer)

(setq howm-content-from-region t)   
(setq howm-title-from-search t)

(setq howm-list-recent-title t) 
(setq howm-list-all-title t)
(setq howm-list-title t) 
(setq howm-list-normalizer 'howm-view-sort-by-reverse-date)
(setq howm-list-prefer-word t)
(setq howm-list-prefer-wiki t)
(setq howm-message-time t)
(setq howm-view-use-grep t)
(setq howm-schedule-menu-types "[!@+-]")
(setq howm-schedule-types "[!@+-]")
(setq howm-todo-menu-types "[!+-]")
(setq howm-todo-types "[!+-]")
(defvar howm-todo-priority-defer-laziness 30)
(defvar howm-todo-priority-defer-init -14)
(setq howm-reminder-font-lock-keywords
        `(
        (,(howm-reminder-regexp "[-]?") (0 howm-reminder-normal-face prepend))
        (,(howm-reminder-regexp "[+]") (0 howm-reminder-todo-face prepend))
        (,(howm-reminder-regexp "[~]") (0 howm-reminder-defer-face prepend))
        (,(howm-reminder-regexp "[!]") (0 howm-reminder-deadline-face prepend))
        (,(howm-reminder-regexp "[@]") (0 howm-reminder-schedule-face prepend))
        (,(howm-reminder-regexp "[.]") (0 howm-reminder-done-face prepend))
        ))

;; _underline_ and *emphasize*
(setq howm-user-font-lock-keywords
      '(("\\(^\\|\\s-\\)\\(_\\w+?_\\)\\(\\s-\\|$\\)"
         2 'underline prepend)
        ("\\(^\\|\\s-\\)\\([*]\\w+?[*]\\)\\(\\s-\\|$\\)"
         2 'font-lock-comment-face prepend)))

(setq howm-view-summary-persistent nil)
(setq howm-view-contents-persistent nil)
(setq howm-view-keep-one-window t)
(setq howm-view-summary-name "*howm S:%s*")
(setq howm-view-contents-name "*howm C:%s*")

(provide 'mt-howm)
