;;; mew-virtual.el --- Virtual mode for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Oct  2, 1996

;;; Code:

(require 'mew)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Virtual info
;;;

(defvar mew-vinfo-list
  '("func" "lra" "top" "db" "column" 
    "parent-folder" ;; Thread only
    "flds")) ;; Virtual (not Thread) only

(mew-blinfo-defun 'mew-vinfo mew-vinfo-list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Virtual mode
;;;

(defun mew-virtual-mode ()
  "Mew Virtual mode:: major mode to visualize messages in a virtual folder.
For more information, see the document of 'mew-summary-mode'."
  (interactive)
  (setq major-mode 'mew-virtual-mode)
  (setq mode-line-buffer-identification mew-mode-line-id)
  (use-local-map mew-summary-mode-map)
  (setq buffer-read-only t)
  (setq truncate-lines t)
  ;;
  (make-local-variable 'tab-width)
  (make-local-variable 'search-invisible)
  (setq search-invisible nil)
  (cond
   (mew-gemacs-p
    (unless (mew-thread-p)
      (jit-lock-register 'mew-summary-cook-region)))
   (t
    (make-local-hook 'window-scroll-functions)
    (add-hook 'window-scroll-functions 'mew-summary-cook-window nil 'local)))
  (mew-sinfo-set-disp-msg t)
  ;;
  (mew-summary-mode-name mew-mode-name-virtual)
  (mew-summary-setup-mode-line)
  (mew-summary-setup-decoration)
  (mew-highlight-cursor-line)
  (run-hooks 'mew-virtual-mode-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Making Virtual mode
;;;

(defun mew-summary-virtual (&optional regionp)
  "Making Virtual mode according to a specified pick pattern.
'mewl' or 'grep' is called as a picking command. If called with
'\\[universal-argument]', the target is the region."
  (interactive "P")
  (mew-pickable
   (mew-summary-with-mewl
    (let* ((folder (mew-pickable-folder))
	   (msgs (mew-summary-pick-msgs folder regionp))
	   (prompt (format "%s/%s virtual" mew-prog-mewl mew-prog-grep))
	   (prog mew-prog-grep)
	   (opts mew-prog-grep-opts)
	   mew-inherit-pick-mewlp
	   grepp pattern prog-opts-pat
	   pfolder vfolder lra)
      (if (not msgs)
	  (message "No message")
	(setq pattern (mew-input-pick-pattern prompt))
	(cond
	 ((string= pattern "")
	  (setq prog-opts-pat (mew-input-pick-command prog opts))
	  (mew-set '(prog opts pattern) prog-opts-pat)
	  (setq grepp t))
	 (t
	  (setq pattern (mew-pick-canonicalize-pattern pattern))
	  (unless mew-inherit-pick-mewlp (setq grepp t))))
	(if (and grepp (not (mew-which-exec prog)))
	    (message "'%s' not found" prog)
	  (setq pfolder (mew-expand-folder2 folder))
	  (setq lra (list (cons pfolder folder)))
	  (setq vfolder (mew-folder-to-virtual folder))
	  (mew-summary-switch-to-folder vfolder)
	  (when (mew-summary-exclusive-p)
	    (mew-vinfo-set-flds (list folder))
	    (cond
	     (grepp
	      (mew-sinfo-set-find-key pattern)
	      (message "Picking messages in %s..." folder)
	      (mew-summary-virtual-with-grep prog opts pattern folder msgs pfolder lra))
	     (t
	      (mew-sinfo-set-find-key nil)
	      (message "Picking messages in %s..." folder)
	      (mew-summary-virtual-with-mewl pattern folder msgs pfolder lra))))))))))

(defun mew-summary-virtual-with-mewl (pattern folder src-msgs pfolder lra)
  "Create Virtual mode with 'mewl'"
  (let ((opts (list "-a" "-p" pattern "-b" mew-mail-path))
	(range (mew-summary-pick-range src-msgs)))
    (if range
	(setq opts (nconc opts (list pfolder range)))
      (setq opts (nconc opts (list pfolder))))
    (mew-local-retrieve 'vir opts nil lra)))

(defun mew-summary-virtual-with-grep (prog opts pattern folder msgs pfolder lra)
  "Create Virtual mode with 'grep'"
  (interactive)
  (let ((file-rttl (mew-summary-virtual-with-grep1 prog opts pattern pfolder msgs))
	file rttl func args)
    (mew-set '(file rttl) file-rttl)
    (setq func `(lambda () (mew-delete-file ,file)))
    (setq args (list "-i" file))
    (mew-local-retrieve 'vir args func lra nil rttl)))

(defun mew-summary-virtual-with-grep1 (prog opts pattern folder msgs)
  (let ((dir (mew-expand-folder folder))
	(file (mew-make-temp-name))
	(rttl 0) nxt)
    (if (= (length msgs) 1) (setq msgs (cons null-device msgs)))
    (if pattern (setq pattern (mew-cs-encode-arg pattern)))
    (with-temp-buffer
      (mew-set-buffer-multibyte t)
      (cd dir)
      (mew-piolet
       mew-cs-text-for-read mew-cs-text-for-write
       (mew-alet ;; xxx
	(while msgs
	  (goto-char (point-max))
	  (setq nxt (nthcdr mew-prog-grep-max-msgs msgs))
	  (if nxt (setcdr (nthcdr (1- mew-prog-grep-max-msgs) msgs) nil))
	  (apply 'call-process prog nil t nil
		 (append opts (and pattern (list pattern)) msgs))
	  (setq msgs nxt)))
       (setq msgs nil)
       (goto-char (point-min))
       (while (re-search-forward mew-regex-message-files2 nil t)
	 (setq msgs (cons (mew-match-string 1) msgs))
	 (forward-line))
       (setq msgs (mew-uniq-list msgs))
       (setq msgs (mapcar 'string-to-number msgs))
       (setq msgs (sort msgs '<))
       (setq msgs (mapcar 'number-to-string msgs)))
      (mew-erase-buffer)
      (setq rttl (length msgs))
      (insert "CD: " folder "\n")
      (mapcar (lambda (x) (insert x "\n")) msgs)
      (mew-frwlet
       mew-cs-text-for-read mew-cs-text-for-write
       (write-region (point-min) (point-max) file nil 'no-msg))
      (list file rttl))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; mark virtual
;;;

(defun mew-summary-mark-virtual (&optional ask-mark)
  "Making Virtual mode for messages marked with '*'.
If called with '\\[universal-argument]', you can specify a target mark."
  (interactive "P")
  (if (not (mew-pickable))
      (message "This command cannot be used in this folder")
    (let* ((folder (mew-pickable-folder))
	   (vfolder (mew-folder-to-virtual folder))
	   (mark mew-mark-review)
	   (start (point))
	   beg line med regex)
      (if ask-mark (setq mark (mew-input-mark)))
      (setq regex (mew-mark-regex mark))
      (mew-summary-switch-to-folder vfolder)
      (mew-erase-buffer)
      (mew-vinfo-set-flds (list folder))
      (set-buffer folder)
      (goto-char (point-min))
      (while (re-search-forward regex nil t)
	(beginning-of-line)
	(setq beg (point))
	(forward-line)
	;; This must be buffer-substring
	(setq line (buffer-substring beg (point)))
	(save-excursion
	  (set-buffer vfolder)
	  (mew-elet
	   (insert line)
	   (save-excursion
	     (when (and (search-backward "\r")
			(setq med (point))
			(looking-at mew-regex-sumsyn-short))
	       (goto-char (match-beginning 1))
	       (insert folder)
	       (put-text-property med (point) 'invisible t))))))
      (goto-char start)
      (set-buffer vfolder)
      (mew-summary-set-count-line)
      ;; Unmarking in both Summary and Thread
      (if (char-equal mark mew-mark-review)
	  (mew-mark-undo-mark mark 'no-msg 'virtual-only)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Old
;;;

(defun mew-summary-virtual-old ()
  "Obsoleted command."
  (interactive)
  (mew-message-for-summary "This command was obsoleted. Use '\\[mew-summary-virtual]' to make Virtual mode"))

(provide 'mew-virtual)

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

;;; mew-virtual.el ends here
