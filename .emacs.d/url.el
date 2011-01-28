;;; url.el --- Uniform Resource Locator retrieval tool

;; Copyright (C) 1996, 1997, 1998, 1999, 2001, 2004,
;;   2005, 2006  Free Software Foundation, Inc.

;; Author: Bill Perry <wmperry@gnu.org>
;; Keywords: comm, data, processes, hypermedia

;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Registered URI schemes: http://www.iana.org/assignments/uri-schemes

;;; Code:

(eval-when-compile (require 'cl))

(eval-when-compile
  (require 'mm-decode)
  (require 'mm-view))

(require 'mailcap)
(require 'url-vars)
(require 'url-cookie)
(require 'url-history)
(require 'url-expand)
(require 'url-privacy)
(require 'url-methods)
(require 'url-proxy)
(require 'url-parse)
(require 'url-util)

;; Fixme: customize? convert-standard-filename?
(defvar url-configuration-directory
  (cond
   ((file-directory-p "~/.url") "~/.url")
   ((file-directory-p "~/.emacs.d") "~/.emacs.d/url")
   (t "~/.url")))

(defun url-do-setup ()
  "Setup the url package.
This is to avoid conflict with user settings if URL is dumped with
Emacs."
  (unless url-setup-done

    ;; Make OS/2 happy
    ;;(push '("http" "80") tcp-binary-process-input-services)

    (mailcap-parse-mailcaps)
    (mailcap-parse-mimetypes)

    ;; Register all the authentication schemes we can handle
    (url-register-auth-scheme "basic" nil 4)
    (url-register-auth-scheme "digest" nil 7)

    (setq url-cookie-file
	  (or url-cookie-file
	      (expand-file-name "cookies" url-configuration-directory)))

    (setq url-history-file
	  (or url-history-file
	      (expand-file-name "history" url-configuration-directory)))

    ;; Parse the global history file if it exists, so that it can be used
    ;; for URL completion, etc.
    (url-history-parse-history)
    (url-history-setup-save-timer)

    ;; Ditto for cookies
    (url-cookie-setup-save-timer)
    (url-cookie-parse-file url-cookie-file)

    ;; Read in proxy gateways
    (let ((noproxy (and (not (assoc "no_proxy" url-proxy-services))
			(or (getenv "NO_PROXY")
			    (getenv "no_PROXY")
			    (getenv "no_proxy")))))
      (if noproxy
	  (setq url-proxy-services
		(cons (cons "no_proxy"
			    (concat "\\("
				    (mapconcat
				     (lambda (x)
				       (cond
					((= x ?,) "\\|")
					((= x ? ) "")
					((= x ?.) (regexp-quote "."))
					((= x ?*) ".*")
					((= x ??) ".")
					(t (char-to-string x))))
				     noproxy "") "\\)"))
		      url-proxy-services))))

    (url-setup-privacy-info)
    (run-hooks 'url-load-hook)
    (setq url-setup-done t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Retrieval functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defun url-retrieve (url callback &optional cbargs)
  "Retrieve URL asynchronously and call CALLBACK with CBARGS when finished.
URL is either a string or a parsed URL.

CALLBACK is called when the object has been completely retrieved, with
the current buffer containing the object, and any MIME headers associated
with it.  Normally it gets the arguments in the list CBARGS.
However, if what we find is a redirect, CALLBACK is given
two additional args, `:redirect' and the redirected URL,
followed by CBARGS.

Return the buffer URL will load into, or nil if the process has
already completed."
  (url-do-setup)
  (url-gc-dead-buffers)
  (if (stringp url)
       (set-text-properties 0 (length url) nil url))
  (if (not (vectorp url))
      (setq url (url-generic-parse-url url)))
  (if (not (functionp callback))
      (error "Must provide a callback function to url-retrieve"))
  (unless (url-type url)
    (error "Bad url: %s" (url-recreate-url url)))
  (let ((loader (url-scheme-get-property (url-type url) 'loader))
	(url-using-proxy (if (url-host url)
			     (url-find-proxy-for-url url (url-host url))))
	(buffer nil)
	(asynch (url-scheme-get-property (url-type url) 'asynchronous-p)))
    (if url-using-proxy
	(setq asynch t
	      loader 'url-proxy))
    (if asynch
	(setq buffer (funcall loader url callback cbargs))
      (setq buffer (funcall loader url))
      (if buffer
	  (with-current-buffer buffer
	    (apply callback cbargs))))
    (if url-history-track
	(url-history-update-url url (current-time)))
    buffer))

;;;###autoload
(defun url-retrieve-synchronously (url)
  "Retrieve URL synchronously.
Return the buffer containing the data, or nil if there are no data
associated with it (the case for dired, info, or mailto URLs that need
no further processing).  URL is either a string or a parsed URL."
  (url-do-setup)

  (lexical-let ((retrieval-done nil)
		(asynch-buffer nil))
    (setq asynch-buffer
	  (url-retrieve url (lambda (&rest ignored)
			      (url-debug 'retrieval "Synchronous fetching done (%S)" (current-buffer))
			      (setq retrieval-done t
				    asynch-buffer (current-buffer)))))
    (if (null asynch-buffer)
        ;; We do not need to do anything, it was a mailto or something
        ;; similar that takes processing completely outside of the URL
        ;; package.
        nil
      (let ((proc (get-buffer-process asynch-buffer)))
	;; If the access method was synchronous, `retrieval-done' should
	;; hopefully already be set to t.  If it is nil, and `proc' is also
	;; nil, it implies that the async process is not running in
	;; asynch-buffer.  This happens e.g. for FTP files.  In such a case
	;; url-file.el should probably set something like a `url-process'
	;; buffer-local variable so we can find the exact process that we
	;; should be waiting for.  In the mean time, we'll just wait for any
	;; process output.
	(while (not retrieval-done)
	  (url-debug 'retrieval
		     "Spinning in url-retrieve-synchronously: %S (%S)"
		     retrieval-done asynch-buffer)
	  (if (and proc (memq (process-status proc)
                              '(closed exit signal failed))
                   ;; Make sure another process hasn't been started, as can
                   ;; happen with http redirections.
		   (eq proc (or (get-buffer-process asynch-buffer) proc)))
	      ;; FIXME: It's not clear whether url-retrieve's callback is
	      ;; guaranteed to be called or not.  It seems that url-http
	      ;; decides sometimes consciously not to call it, so it's not
	      ;; clear that it's a bug, but even then we need to decide how
	      ;; url-http can then warn us that the download has completed.
              ;; In the mean time, we use this here workaround.
              (setq retrieval-done t)
            ;; We used to use `sit-for' here, but in some cases it wouldn't
            ;; work because apparently pending keyboard input would always
            ;; interrupt it before it got a chance to handle process input.
            ;; `sleep-for' was tried but it lead to other forms of
            ;; hanging.  --Stef
            (unless (or (accept-process-output proc) (null proc))
              ;; accept-process-output returned nil, maybe because the process
              ;; exited (and may have been replaced with another).
              (setq proc (get-buffer-process asynch-buffer))))))
      asynch-buffer)))

(defun url-mm-callback (&rest ignored)
  (let ((handle (mm-dissect-buffer t)))
    (url-mark-buffer-as-dead (current-buffer))
    (with-current-buffer
        (generate-new-buffer (url-recreate-url url-current-object))
      (if (eq (mm-display-part handle) 'external)
	  (progn
	    (set-process-sentinel
	     ;; Fixme: this shouldn't have to know the form of the
	     ;; undisplayer produced by `mm-display-part'.
	     (get-buffer-process (cdr (mm-handle-undisplayer handle)))
	     `(lambda (proc event)
		(mm-destroy-parts (quote ,handle))))
	    (message "Viewing externally")
	    (kill-buffer (current-buffer)))
	(display-buffer (current-buffer))
	(add-hook 'kill-buffer-hook
		  `(lambda () (mm-destroy-parts ',handle))
		  nil
		  t)))))

(defun url-mm-url (url)
  "Retrieve URL and pass to the appropriate viewing application."
  ;; These requires could advantageously be moved to url-mm-callback or
  ;; turned into autoloads, but I suspect that it would introduce some bugs
  ;; because loading those files from a process sentinel or filter may
  ;; result in some undesirable carner cases.
  (require 'mm-decode)
  (require 'mm-view)
  (url-retrieve url 'url-mm-callback nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellaneous
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar url-dead-buffer-list nil)

(defun url-mark-buffer-as-dead (buff)
  (push buff url-dead-buffer-list))

(defun url-gc-dead-buffers ()
  (let ((buff))
    (while (setq buff (pop url-dead-buffer-list))
      (if (buffer-live-p buff)
	  (kill-buffer buff)))))

(cond
 ((fboundp 'display-warning)
  (defalias 'url-warn 'display-warning))
 ((fboundp 'warn)
  (defun url-warn (class message &optional level)
    (warn "(%s/%s) %s" class (or level 'warning) message)))
 (t
  (defun url-warn (class message &optional level)
    (with-current-buffer (get-buffer-create "*URL-WARNINGS*")
      (goto-char (point-max))
      (save-excursion
	(insert (format "(%s/%s) %s\n" class (or level 'warning) message)))
      (display-buffer (current-buffer))))))

(provide 'url)

;; arch-tag: bc182f1f-d187-4f10-b961-47af2066579a
;;; url.el ends here
