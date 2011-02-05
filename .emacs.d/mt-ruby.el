(defun mt-ruby-xmp-region (arg)
  "Pipe the region through Ruby's xmp utility and replace the region 
with the result. If prefix is set dont replace the region but add the result after a #"
  (interactive "P")
  (save-excursion
    (shell-command-on-region (region-beginning) (region-end)
                             (concat 
                              "ruby -r ~/bin/xmp.rb -n -e 'xmp($_, \"" 
                              (if arg "%l # => %r" "%r") "\n\")'") t)
    (backward-delete-char 1))
    (goto-char (point-at-eol)))


(defun mt-ruby-xmp-buffer ()
  (interactive)
  (save-excursion
    (shell-command-on-region 1 (point-max) (mt-ruby-xmp-command) t t))
  (goto-char (point-max)))

(defun mt-ruby-xmp-command ()
  (cond ((save-excursion
           (goto-char 1)
           (search-forward "< Test::Unit::TestCase" nil t))
         "ruby -S xmpfilter.rb --unittest")
        ((save-excursion
           (goto-char 1)
           (re-search-forward "^context.+do$" nil t))
         "ruby -S ~/bin/xmpfilter.rb --spec")
        (t
         "ruby -S ~/bin/xmpfilter.rb")))

(defun mt-ruby-eval-buffer () (interactive)
  "Evaluate the buffer with ruby."
  (shell-command-on-region (point-min) (point-max) "ruby"))

(defun mt-ruby-compile (&optional arg)
  "Evaluate the buffer with ruby."
  (interactive "P")
  (if arg 
      (if (equal arg '(4))
          (compile "rake specdoc")
        (message arg))
    (compile "rake spec")))


(defun mt-ruby-setup ()
  "Setup a ruby buffer."
  (let ((ruby-mode-hs-info
         '(ruby-mode
           "class\\|module\\|def\\|begin\\|do"
           "end"
           "#"
           ruby-move-to-block
           nil
           )))
    (if (not (member ruby-mode-hs-info hs-special-modes-alist))
        (setq hs-special-modes-alist
              (cons ruby-mode-hs-info hs-special-modes-alist))))
  (unicode-helper-mode 1)
  (hs-minor-mode 1)
  (hs-hide-initial-comment-block)
  (highlight-changes-mode 0)
  (setq ruby-program-name "irb --inf-ruby-mode")
  (highlight-parentheses-mode 1)
  (setq compile-command  "rake spec")
  (inf-ruby-keys)
  (line-number-mode 1)
  (ruby-electric-mode 1)
  (setq local-abbrev-table mt-ruby-mode-abbrev-table)
  (setq outline-regexp "#{{{]\\|###autoload\\|(")
  (setq ruby-electric-no-matching-char-in-word nil)
  (setq my-ruby-compilation-arguments "rake spec")
  (set (make-local-variable 'auto-hscroll-mode) nil)
  (bs-config-clear)
  (filladapt-mode 1)
  (setq bs-default-configuration "ruby")
  (mapc
   (lambda (mapping)
     (apply #'define-key ruby-mode-map mapping))
   `(
     (,(kbd "RET") ruby-reindent-then-newline-and-indent)
     (,(kbd "TAB") mt-indent-or-complete)
     (,(kbd "C-x x") mt-ruby-xmp-region)
     (,(kbd "C-x R") mt-ruby-eval-buffer)
     (,(kbd "C-c RET") mt-ruby-compile)
     (,(kbd "C-x t") mt-ruby-xmp-buffer)
     (,(kbd "C-c p") toggle-buffer)
     (,(kbd "C-c s") toggle-style)
     (,(kbd "C-h i") ri)
     (,(kbd "C-c C-e") hs-show-block)
     (,(kbd "C-c C-c") hs-toggle-hiding)
     (,(kbd "C-c C-a") hs-show-all)
     (,(kbd "C-c C-h") hs-hide-all)
     (,(kbd "C-c C-l") hs-hide-level))))

(provide 'mt-ruby)
