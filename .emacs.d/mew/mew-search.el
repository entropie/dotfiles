;;; mew-search.el --- Index Search

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Aug 24, 2005

;;; Code:

(require 'mew)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Variables
;;;

(defvar mew-prog-spotlight "mdfind")
(defvar mew-prog-wds       "wdsgrep.exe")
(defvar mew-prog-google    "gdgrep.rb")
(defvar mew-prog-namazu    "namazu")

(defvar mew-search-switch
  `((spotlight "Spotlight" ,mew-prog-spotlight
     mew-search-with-spotlight mew-search-virtual-with-spotlight
     mew-spotlight-index-folder mew-spotlight-index-all
     mew-pick-canonicalize-pattern-spotlight
     nil nil)
    (wds "WDS" ,mew-prog-wds
     mew-search-with-wds mew-search-virtual-with-wds
     mew-wds-index-folder mew-wds-index-all
     mew-pick-canonicalize-pattern-wds
     mew-wds-register mew-wds-unregister)
    (google "Google" ,mew-prog-google
     mew-search-with-google mew-search-virtual-with-google
     mew-google-index-folder mew-google-index-all
     mew-pick-canonicalize-pattern-google
     mew-google-register mew-google-unregister)
    (namazu "Namazu" ,mew-prog-namazu
     mew-search-with-namazu nil
     mew-nmz-index-folder nil
     mew-pick-canonicalize-pattern-namazu
     nil nil)))

(defun mew-search-get-list (func)
  (let ((sw mew-search-switch)
	ent ret)
    (while sw
      (setq ent (car sw))
      (setq sw (cdr sw))
      (if (mew-which-exec (mew-search-get-prog ent))
	  (setq ret (cons (funcall func ent) ret))))
    (nreverse ret)))

(defun mew-search-get-ent (method)
  (assoc method mew-search-switch))

(defun mew-search-get-key (ent)
  (nth 0 ent))

(defun mew-search-get-name (ent)
  (nth 1 ent))

(defun mew-search-get-prog (ent)
  (nth 2 ent))

(defun mew-search-get-func-search (ent)
  (nth 3 ent))
(defun mew-search-get-func-virtual (ent)
  (nth 4 ent))

(defun mew-search-get-func-index-folder (ent)
  (nth 5 ent))

(defun mew-search-get-func-index-all (ent)
  (nth 6 ent))

(defun mew-search-get-func-canonicalize-pattern (ent)
  (nth 7 ent))

(defun mew-search-get-func-register (ent)
  (nth 8 ent))

(defun mew-search-get-func-unregister (ent)
  (nth 9 ent))

(defvar mew-search-method (car (mew-search-get-list 'mew-search-get-key)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Launcher
;;;

(defun mew-summary-search-change-method ()
  "Change a search method."
  (interactive)
  (let* ((names (mew-search-get-list 'mew-search-get-name))
	 (ent (mew-search-get-ent mew-search-method))
	 (default (mew-search-get-name ent))
	 name)
    (if (not names)
	(message "No search method")
      (setq names (mapcar (lambda (x) (list x)) names))
      (setq name (completing-read (format "Search method (%s): " default) names nil t))
      (if (string= name "") (setq name default))
      (setq mew-search-method (nth 0 (mew-assoc-equal name mew-search-switch 1))))))

(defun mew-summary-search ()
  "Pick messages according to a specified pick pattern 
with a search method. Then put the '*' mark onto them. "
  (interactive)
  (if (not mew-search-method)
      (message "No search method")
    (let* ((ent (mew-search-get-ent mew-search-method))
	   (func (mew-search-get-func-search ent))
	   (name (mew-search-get-name ent))
	   (canon-func (mew-search-get-func-canonicalize-pattern ent))
	   (folder (mew-pickable-folder))
	   pattern msgs)
      (mew-pickable
       (if (not (fboundp func))
	   (message "This command cannot be used")
	 (setq pattern (mew-input-pick-pattern (concat name " pick")))
	 (when (and canon-func (fboundp canon-func))
	   (setq pattern (funcall canon-func pattern)))
	 (setq msgs (funcall func pattern folder))
	 (mew-summary-pick-ls folder msgs))))))

(defun mew-summary-search-virtual (&optional ask-folder)
  "Making Virtual mode according to a specified pick pattern
with a search method."
  (interactive "P")
  (if (not mew-search-method)
      (message "No search method")
    (let* ((ent (mew-search-get-ent mew-search-method))
	   (func (mew-search-get-func-virtual ent))
	   (name (mew-search-get-name ent))
	   (canon-func (mew-search-get-func-canonicalize-pattern ent))
	   vfolder opattern pattern dfunc file opts rttl file-rttl flds)
      (if (not (fboundp func))
	  (message "This command cannot be used")
	(if ask-folder
	    (setq flds (mew-input-folders (mew-summary-folder-name))))
	(setq opattern (mew-input-pick-pattern (concat name " virtual")))
	(when (and canon-func (fboundp canon-func))
	  (setq pattern (funcall canon-func opattern)))
	(setq vfolder (mew-folder-to-virtual opattern))
	(mew-summary-switch-to-folder vfolder)
	(mew-sinfo-set-find-key opattern)
	(make-local-variable 'mew-summary-form-mark-delete)
	(setq mew-summary-form-mark-delete nil)
	(make-local-variable 'mew-summary-form-mark-spam)
	(setq mew-summary-form-mark-spam nil)
	(when (mew-summary-exclusive-p)
	  (with-temp-buffer
	    (mew-set-buffer-multibyte t)
	    (mew-piolet
	     mew-cs-text-for-read mew-cs-text-for-write
	     (setq file-rttl (funcall func pattern flds)))))
	(mew-set '(file rttl) file-rttl)
	(setq dfunc `(lambda () (mew-delete-file ,file)))
	(setq opts (list "-i" file))
	(mew-local-retrieve 'vir opts dfunc nil nil rttl)))))

(defun mew-summary-make-index-folder ()
  "Make index for this folder."
  (interactive)
  (mew-summary-only
   (if (not mew-search-method)
       (message "No search method")
     (let* ((ent (mew-search-get-ent mew-search-method))
	    (func (mew-search-get-func-index-folder ent))
	    (folder (mew-summary-folder-name 'ext)))
       (if (not (fboundp func))
	   (message "This command cannot be used")
	 (funcall func folder))))))

(defun mew-summary-make-index-all ()
  "Make index for all folders."
  (interactive)
  (if (not mew-search-method)
      (message "No search method")
    (let* ((ent (mew-search-get-ent mew-search-method))
	   (func (mew-search-get-func-index-all ent)))
      (if (fboundp func)
	  (funcall func)
	(message "This command cannot be used")))))

(defun mew-summary-search-register ()
  (interactive)
  (if (not mew-search-method)
      (message "No search method")
    (let* ((ent (mew-search-get-ent mew-search-method))
	   (func (mew-search-get-func-register ent)))
       (if (not (fboundp func))
	   (message "This command cannot be used")
	 (funcall func)))))

(defun mew-summary-search-unregister ()
  (interactive)
  (if (not mew-search-method)
      (message "No search method")
    (let* ((ent (mew-search-get-ent mew-search-method))
	   (func (mew-search-get-func-unregister ent)))
       (if (not (fboundp func))
	   (message "This command cannot be used")
	 (funcall func)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Spotlight
;;;

(defun mew-assoc-match-list (key alist nth)
  (let ((case-fold-search t) a n ret)
    (while alist
      (setq a (car alist))
      (setq n (nth nth a))
      (if (or (and (stringp n) (string-match key n))
	      (equal n key) (eq n t))
	  (setq ret (cons a ret)))
      (setq alist (cdr alist)))
    ret)) ;; not reversed

(defun mew-expand-wildcard-folder (wlist)
  (let (case fld ent ret flds regex alist)
    (while wlist
      (setq ent (car wlist))
      (setq wlist (cdr wlist))
      (setq case (mew-case:folder-case ent))
      (setq fld (mew-case:folder-folder ent))
      (if (not (string-match "\\*$" fld))
	  (setq ret (cons ent ret))
	(setq regex (substring fld 0 -1))
	(setq regex (concat "^" (regexp-quote regex)))
	(cond
	 ((mew-folder-popp fld)
	  (setq alist (mew-pop-folder-alist)))
	 ((mew-folder-nntpp fld)
	  (setq alist (mew-nntp-folder-alist case)))
	 ((mew-folder-imapp fld)
	  (setq alist (mew-imap-folder-alist case)))
	 (t
	  (setq alist (mew-local-folder-alist))))
	(setq flds (mew-assoc-match-list regex alist 0))
	(setq flds (mapcar (lambda (x) (mew-case-folder case (nth 0 x))) flds))
	(setq ret (nconc flds ret))))
    (nreverse ret)))

(defun mew-search-spotlight (pattern path)
  (setq pattern (mew-cs-encode-string pattern 'utf-8))
  (let* ((ent (mew-search-get-ent mew-search-method))
	 (prog (mew-search-get-prog ent)))
    (mew-plet
     (mew-alet
      (call-process prog nil t nil "-onlyin" path pattern)))))

(defun mew-search-with-spotlight (pattern folder)
  (let ((path (mew-expand-folder folder))
	msgs)
    (with-temp-buffer
      (mew-set-buffer-multibyte t)
      (mew-search-spotlight pattern path)
      (goto-char (point-min))
      (while (re-search-forward mew-regex-message-files5 nil t)
	(setq msgs (cons (mew-match-string 1) msgs))
	(forward-line))
      (setq msgs (nreverse msgs))
      (setq msgs (sort (mapcar 'string-to-number msgs) '<))
      (mapcar 'number-to-string msgs))))

(defun mew-search-virtual-with-spotlight (pattern flds)
  (let* ((mpath (mew-expand-folder mew-folder-local))
	 (mail-regex (regexp-quote (file-name-as-directory mpath)))
	 (regex (format "^%s%s\\([0-9]+\\)\\(%s\\)?$" mail-regex (file-name-as-directory "\\(.*\\)") mew-suffix))
	 (file (mew-make-temp-name))
	 (prev "") (rttl 0) (n 1) crnt path)
    (unless flds (setq flds (list mew-folder-local)))
    (setq flds (mew-expand-wildcard-folder flds))
    (while flds
      (setq path (mew-expand-folder (car flds)))
      (setq flds (cdr flds))
      (mew-search-spotlight pattern path)
      (goto-char (point-min))
      (while (looking-at regex)
	(setq rttl (1+ rttl))
	(setq crnt (match-string 1))
	(delete-region (match-beginning 0) (match-beginning 2))
	(when (not (string= crnt prev))
	  (beginning-of-line)
	  (insert "CD:" mew-folder-local crnt "\n"))
	(setq prev crnt)
	(forward-line))
      (mew-frwlet
       mew-cs-text-for-read mew-cs-text-for-write
       (write-region (point-min) (point-max) file (> n 1) 'no-msg))
      (mew-erase-buffer)
      (setq n (1+ n)))
    (list file rttl)))

(defun mew-spotlight-index (dir)
  (let* ((default-directory dir)
	 (msgs (mew-dir-messages dir mew-regex-message-files3 'full))
	 dirent subdir)
    (while msgs
      (mew-set-file-type (car msgs))
      (setq msgs (cdr msgs)))
    (when (/= (mew-file-get-links dir) 2)
      (setq dirent (directory-files "." nil mew-regex-folder-candidate))
      (while dirent
	(setq subdir (car dirent))
	(setq dirent (cdr dirent))
	(when (and (file-directory-p subdir)
		   (not (file-symlink-p subdir)))
	  (mew-spotlight-index (expand-file-name subdir dir)))))))

(defun mew-spotlight-index-folder (folder)
  "Making spotlight index for this folder."
  (interactive)
  (let* ((dir (mew-expand-folder folder))
	 (msgs (mew-dir-messages dir mew-regex-message-files3 'full)))
    (message "Spotlight indexing for %s..." folder)
    (while msgs
      (mew-set-file-type (car msgs))
      (setq msgs (cdr msgs)))
    (message "Spotlight indexing for %s...done" folder)))

(defun mew-spotlight-index-all ()
  "Making spotlight index for all messages."
  (interactive)
  (when (y-or-n-p "Make Spotlight index for all folders? ")
    (message "Spotlight indexing for all folders...")
    (let ((flds '("+" "+#imap" "+#pop" "+#nntp"))
	  dir)
      (while flds
	(setq dir (mew-expand-folder (car flds)))
	(setq flds (cdr flds))
	(if (file-directory-p dir) (mew-spotlight-index dir))))
    (message "Spotlight indexing for all folders...done")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Google Desktop
;;;

(defun mew-search-google (pattern path)
  (setq pattern (mew-cs-encode-string pattern 'shift_jis))
  (let* ((ent (mew-search-get-ent mew-search-method))
	 (prog (mew-search-get-prog ent)))
    (mew-plet
     (mew-alet
      (call-process prog nil t nil "-m" "-p" path "-s" "-q" pattern)))))

(defun mew-search-with-google (pattern folder)
  (let* ((path (mew-expand-folder folder))
	 msgs)
    (with-temp-buffer
      (mew-set-buffer-multibyte t)
      (mew-search-google pattern path)
      (goto-char (point-min))
      (while (re-search-forward mew-regex-message-files5 nil t)
	(setq msgs (cons (mew-match-string 1) msgs))
	(forward-line))
      (setq msgs (nreverse msgs))
      (setq msgs (sort (mapcar 'string-to-number msgs) '<))
      (mapcar 'number-to-string msgs))))

;; xxx flds are not used at this moment
(defun mew-search-virtual-with-google (pattern flds)
  (let* ((path (mew-expand-folder mew-folder-local))
	 (mail-regex (regexp-quote (file-name-as-directory path)))
	 (regex (concat "^" mail-regex "\\(.*\\)/" "\\([0-9]+\\)"))
	 (file (mew-make-temp-name))
	 (prev "") (rttl 0)
	 crnt)
    (mew-search-google pattern path)
    (goto-char (point-min))
    (while (looking-at regex)
      (setq rttl (1+ rttl))
      (setq crnt (match-string 1))
      (delete-region (match-beginning 0) (match-beginning 2))
      (when (not (string= crnt prev))
	(beginning-of-line)
	(insert "CD:" mew-folder-local crnt "\n"))
      (setq prev crnt)
      (forward-line))
    (mew-frwlet
     mew-cs-text-for-read mew-cs-text-for-write
     (write-region (point-min) (point-max) file nil 'no-msg))
    (list file rttl)))

(defun mew-google-index-folder (folder)
  "Make Google index for this folder."
  (interactive)
  (let* ((ent (mew-search-get-ent mew-search-method))
	 (prog (mew-search-get-prog ent)))
    (start-process prog nil prog "-m" "-i" (mew-expand-folder folder))
    (message "Google indexing for %s in background..." folder)))

(defun mew-google-index-all ()
  "Make Google index for all folders."
  (interactive)
  (let* ((ent (mew-search-get-ent mew-search-method))
	 (prog (mew-search-get-prog ent)))
    (start-process prog nil prog "-m" "-r" "-i" (mew-expand-folder "+"))
    (message "Google indexing for all folders in background...")))

(defun mew-google-register ()
  "Register Google component"
  (interactive)
  (message "Registering Google component...")
  (call-process mew-prog-google nil nil nil "-R")
  (message "Registering Google component...done"))

(defun mew-google-unregister ()
  "Unregister Google component"
  (interactive)
  (message "Unregistering Google component...")
  (call-process mew-prog-google nil nil nil "-U")
  (message "Unregistering Google component...done"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; WDS Desktop
;;;

(defun mew-search-wds (pattern path)
  (setq pattern (mew-cs-encode-string pattern default-file-name-coding-system))
  (let* ((ent (mew-search-get-ent mew-search-method))
	 (prog (mew-search-get-prog ent)))
    (mew-plet
     (mew-alet
      (call-process prog nil t nil "-e" mew-suffix "-p" path "-s" pattern)))))

(defun mew-search-with-wds (pattern folder)
  (let* ((path (mew-expand-folder folder))
	 msgs)
    (with-temp-buffer
      (mew-set-buffer-multibyte t)
      (mew-search-wds pattern path)
      (goto-char (point-min))
      (while (re-search-forward mew-regex-message-files5 nil t)
	(setq msgs (cons (mew-match-string 1) msgs))
	(forward-line))
      (setq msgs (nreverse msgs))
      (setq msgs (sort (mapcar 'string-to-number msgs) '<))
      (mapcar 'number-to-string msgs))))

;; xxx flds are not used at this moment
(defun mew-search-virtual-with-wds (pattern flds)
  (let* ((path (mew-expand-folder mew-folder-local))
	 (mail-regex (regexp-quote (file-name-as-directory path)))
	 (regex (concat "^" mail-regex "\\(.*\\)/" "\\([0-9]+\\)"))
	 (file (mew-make-temp-name))
	 (prev "") (rttl 0)
	 crnt)
    (mew-search-wds pattern path)
    (goto-char (point-min))
    (while (looking-at regex)
      (setq rttl (1+ rttl))
      (setq crnt (match-string 1))
      (delete-region (match-beginning 0) (match-beginning 2))
      (when (not (string= crnt prev))
	(beginning-of-line)
	(insert "CD:" mew-folder-local crnt "\n"))
      (setq prev crnt)
      (forward-line))
    (mew-frwlet
     mew-cs-text-for-read mew-cs-text-for-write
     (write-region (point-min) (point-max) file nil 'no-msg))
    (list file rttl)))

(defun mew-wds-index-folder (folder)
  "Make WDS index for all folders."
  (interactive)
  (mew-wds-index-all))

(defun mew-wds-index-all ()
  "Make WDS index for all folders."
  (interactive)
  (let* ((ent (mew-search-get-ent mew-search-method))
	 (prog (mew-search-get-prog ent)))
    (start-process prog nil prog "-R" (format "%s=%s" mew-suffix ".eml"))
    (message "WDS indexing for all folders in background...")))

(defun mew-wds-register ()
  "Register '.mew' to WDS"
  (interactive)
  (message "Registering suffix '%s' to WDS..." mew-suffix)
  (call-process mew-prog-wds nil nil nil "-R" (format "%s=%s" mew-suffix ".eml"))
  (message "Registering suffix '%s' to WDS...done" mew-suffix))

(defun mew-wds-unregister ()
  "Unregister '.mew' from WDS"
  (interactive)
  (message "This command cannot be used"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Namazu
;;;

(defun mew-search-with-namazu (pattern folder)
  (mew-nmz-multi-pick (list (mew-nmz-expand-folder folder)) pattern nil 'single))

(defun mew-nmz-index-folder (folder)
  (mew-nmz-mknmz folder nil 'msg))

(provide 'mew-search)

;;; Copyright Notice:

;; Copyright (C) 2005-2006 Mew developing team.
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

;;; mew-search.el ends here
