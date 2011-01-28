(defun mt-dired-sort-hook ()
  "Dired sort hook to list directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header  
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max))))
  (and (featurep 'xemacs)
       (fboundp 'dired-insert-set-properties)
       (dired-insert-set-properties (point-min) (point-max)))
  (set-buffer-modified-p nil))

(add-hook 'dired-after-readin-hook 'mt-dired-sort-hook)

(defun mt-dired-sort ()
  "Sort dired listings with directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header 
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
    (set-buffer-modified-p nil)))

(defadvice dired-readin
  (after dired-after-updating-hook first () activate)
  "Sort dired listings with directories first before adding marks."
  (mt-dired-sort))


(defun mt-dired-setup ()
  "Setup a direded buffer,"
  (bs-config-clear)
  (setq bs-default-configuration "dired")
  (setq dired-backup-overwrite nil)
  (setq dired-omit-files-p t)
  (setq dired-omit-files "^#\\|^\\.")
 (mapc
   (lambda (mapping)
     (apply #'define-key dired-mode-map mapping))
   `((,(kbd "C-x p") emms-play-dired)
     (,(kbd "C-x e") emms-add-dired)
     (,(kbd "^")  (lambda nil (interactive) (joc-dired-single-buffer "..")))
     (,(kbd "RET") joc-dired-single-buffer)
     (,(kbd "C-s")   dired-isearch-forward)
     (,(kbd "C-r")   dired-isearch-backward)
     (,(kbd "ESC C-s")   dired-isearch-forward-regexp)
     (,(kbd "ESC C-r")   dired-isearch-backward-regexp )
     (,(kbd "C-x r") wdired-change-to-wdired-mode)
     )))


(require 'dired)
(require 'dired-x)
(require 'dired-isearch)
(require 'dired-single)
(add-hook 'dired-mode-hook 'mt-dired-setup)
(add-hook 'text-mode-hook  'mt-text-setup)


(provide 'mt-dired)
