;;; mt-encryption.el --- cryption

;; Copyright (C) 2008  Michael Trommer

;; Author: Michael Trommer <mictro@gmail.com>
;; Keywords: tools

(require 'epa)
(require 'epa-file)

(epa-file-enable)
(setq epg-user-id "1D2998A7101B8546") ;;101B8546")


(provide 'mt-encryption)
