;;; mew-varsx.el --- Variables depends on other variables

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: May 10, 2006

;;; Code:

(setq mew-basic-folders
  (list mew-inbox-folder mew-draft-folder mew-queue-folder mew-postq-folder))

(defcustom mew-scan-fields
  (list "Folder:" "Filename:" 
	mew-subj: mew-date: mew-from: mew-to: mew-cc:
	mew-ct: mew-cte: mew-x-mew-uidl:
	mew-message-id: mew-in-reply-to: mew-references: mew-x-mew-ref:
	mew-spam: "Body")
  "*A list which specifies mewl's output.
The first element MUST be \"Folder:\".
The second element MUST be \"Filename:\".
Both \"In-Reply-To:\" and \"References:\" MUST be included for thread.
The last MUST be \"Body\"."
  :group 'mew-summary
  :type '(cons (const "Folder:")
               (cons (const "Filename:")
                     (repeat string))))

;; strict valid
(setq mew-regex-message-files  (format  "^[1-9][0-9]*\\(%s\\)?$" (regexp-quote mew-suffix)))
;; strict invalid
(setq mew-regex-message-files4 (format "^0[1-9][0-9]*\\(%s\\)?$" (regexp-quote mew-suffix)))
;; strict all
(setq mew-regex-message-files3 (format "^\\([0-9]+\\)\\(%s\\)?$" (regexp-quote mew-suffix)))
;; mewl and grep 
(setq mew-regex-message-files2 (format "^\\([0-9]+\\)\\(%s\\)?" (regexp-quote mew-suffix)))
;; search
(setq mew-regex-message-files5 (format  "\\([0-9]+\\)\\(%s\\)?$" (regexp-quote mew-suffix)))

(provide 'mew-varsx)

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

;;; mew-varsx.el ends here
