(require 'desktop)

;; (setq 
;;  mt-is-default-font t
;;  mt-default-font "-xos4-terminus-medium-r-normal--20-200-72-72-c-100-iso10646-1"
;;  mt-other-font   "-*-ormyn e-*-r-*-*-17-*-*-*-*-*-*-*")


;;(set-face-font 'default mt-default-font)

(setq desktop-dirname (expand-file-name "~/")
      desktop-save t
      desktop-globals-to-save
      '((extended-command-history . 50)
        (file-name-history	  . 50)
        (grep-history		  . 50)
        (minibuffer-history	  . 50)
        (query-replace-history  . 50)
        (read-expression-history  . 50)
        (regexp-history	  . 50)
        (search-ring		  . 50)
        (shell-command-history  . 50)))
