;;; mew-unix.el -- MIME content type for UNIX

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Dec  4, 1997

;;; Code:

;;;
;;; Office
;;;

(defvar mew-prog-application/msword "wvHtml")
(defvar mew-prog-application/msexcel "xlhtml")
(defvar mew-prog-application/mspowerpoint "ppthtml")
(defvar mew-prog-application/rtf "rtf2html")
(defvar mew-prog-msword-ext "ooffice")
(defvar mew-prog-msexcel-ext "ooffice")
(defvar mew-prog-mspowerpoint-ext "ooffice")

;;;
;;; Text/Html, Application/Xml
;;;

(defvar mew-format-html "%s.html")
(defvar mew-format-xml  "%s.xml")

(defvar mew-prog-text/html           'mew-mime-text/html-w3m) ;; See w3m.el
(defvar mew-prog-text/html-ext       '("mozilla" ("-remote" "openFile(%s)") t))

(defvar mew-prog-text/xml            'mew-mime-text/html-w3m) ;; See w3m.el
(defvar mew-prog-text/xml-ext        '("mozilla" ("-remote" "openFile(%s)") t))

(defvar mew-prog-application/xml     nil)
(defvar mew-prog-application/xml-ext '("mozilla" ("-remote" "openFile(%s)") t))


;;;
;;; Image
;;; 

(defvar mew-prog-image/*         'mew-mime-image/*)
(defvar mew-prog-image/*-ext     '("display" ("-geometry" "+0+0")))

;;;
;;; MIME setting
;;;

(defvar mew-prog-plain    'mew-mime-text/plain)
(defvar mew-prog-html     '(mew-mime-text/html mew-mime-text/html-ext))
(defvar mew-prog-xml      '(mew-mime-text/xml  mew-mime-text/xml-ext))
(defvar mew-prog-enriched 'mew-mime-text/enriched)
(defvar mew-prog-text     'mew-mime-text/plain)
(defvar mew-prog-audio
  `(,shell-file-name (,shell-command-switch "cat - > /dev/audio") nil))
(defvar mew-prog-audio2
  `(,shell-file-name (,shell-command-switch "cat < /dev/audio") nil))
(defvar mew-prog-image           '(mew-mime-image/* mew-mime-image/*-ext))
(defvar mew-prog-iges nil)
(defvar mew-prog-vrml nil)
(defvar mew-prog-mesh nil)
(defvar mew-prog-video           '("xine" ("--auto-play") t))
(defvar mew-prog-rfc822          'mew-mime-message/rfc822)
(defvar mew-prog-rfc822-headers  'mew-mime-text/rfc822-headers)
(defvar mew-prog-external-body   '(mew-mime-external-body mew-mime-external-body-ext))
(defvar mew-prog-delivery-status 'mew-mime-text/plain)
(defvar mew-prog-postscript      '("gv" ("-geometry" "+0+0") t))
(defvar mew-prog-pdf             '("xpdf" ("-geometry" "+0+0") t))
(defvar mew-prog-xml2            '(mew-mime-application/xml
				   mew-mime-application/xml-ext))
(defvar mew-prog-pgp-keys        '(mew-mime-pgp-keys mew-mime-pgp-keys-ext))
(defvar mew-prog-oasys           nil)
(defvar mew-prog-octet-stream    'mew-mime-application/octet-stream)

(defvar mew-prog-msword  `(mew-mime-application/msword  ,mew-prog-msword-ext))
(defvar mew-prog-msexcel `(mew-mime-application/msexcel ,mew-prog-msexcel-ext))
(defvar mew-prog-mspowerpoint `(mew-mime-application/mspowerpoint ,mew-prog-mspowerpoint-ext))
(defvar mew-prog-ooffice         "ooffice")
(defvar mew-prog-visio           'mew-mime-application/octet-stream)
(defvar mew-prog-mstnef          '(mew-mime-application-ms-tnef
				   mew-mime-application-ms-tnef-ext))
(defvar mew-prog-rtf             '(mew-mime-application/rtf
				   mew-mime-application/rtf-ext))
;;;
;;; Misc
;;;

(defvar mew-default-external-program nil)
(defvar mew-dir-list-function 'mew-dir-list-with-link-count)

(defun mew-set-file-type (file) ())

(provide 'mew-unix)

;;; Copyright Notice:

;; Copyright (C) 1996-2006 Mew developing team.
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. Neither the name of the team nor the names of its contributors
;;    may be used to endorse or promote products derived from this software
;;    without specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE TEAM AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;; PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE TEAM OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
;; OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
;; IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; mew-unix.el ends here
