(require 'mldonkey)

(setq mldonkey-host "tie")
(setq mldonkey-port 4000)

(setq mldonkey-console-use-color t)

;; (setq mldonkey-user "USER")
;; (setq mldonkey-pass "PASSWD")

;; (add-hook 'mldonkey-console-hook 'mldonkey-console-auth)
;; (add-hook 'mldonkey-motd-hook 'mldonkey-auth)

(setq mldonkey-show-days t)

(setq mldonkey-show-network t)

(setq mldonkey-show-finished-md4 t)
(add-to-list 'mldonkey-vd-filename-filters 'mldonkey-vd-filename-remove-p20)

(add-hook 'mldonkey-pause-hook 'mldonkey-vd)
(add-hook 'mldonkey-resume-hook 'mldonkey-vd)
(add-hook 'mldonkey-commit-hook 'mldonkey-vd)
(add-hook 'mldonkey-recover-temp-hook 'mldonkey-vd)

;; use higline-local-mode
(require 'highline)
(add-hook 'mldonkey-mode-hook 'highline-local-mode)
