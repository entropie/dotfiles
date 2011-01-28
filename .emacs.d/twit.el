;;; twit.el --- interface with twitter.com

;; Copyright (c) 2007 Theron Tlax
;; Time-stamp: <2007-03-19 18:33:17 thorne>
;; Author: thorne <thorne@timbral.net>
;; Created: 2007.3.16
;; Keywords: comm
;; Favorite Poet: E. E. Cummings

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation version 2.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; For a copy of the GNU General Public License, search the Internet,
;; or write to the Free Software Foundation, Inc., 59 Temple Place,
;; Suite 330, Boston, MA 02111-1307 USA

;;; Commentary:

;; This is the beginnings of a library for interfacing with
;; twitter.com from Emacs.  It is also (more importantly) some
;; interactive functions that use that library.  It's a hack, of
;; course; RMS i am not.  Maybe one of you real programmers would
;; like to clean it up?

;; This uses Twitter's XML-based api, not the JSON one because i
;; would like to avoid making the user install third-party libraries
;; to use it.

;;  Use:

;;			      FOR POSTING

;; There are three main interactive functions:

;;   M-x twit-post RET will prompt for you to type your post directly
;;   in the minibuffer.

;;   M-x twit-post-region RET will post the region and

;;   M-x twit-post-buffer RET will post the entire contents of the
;;   current buffer.

;;   M-x twit-mode RET, if you want to bother, just binds the
;;   interactive functions to some keys.  Do C-h f RET twit-mode RET
;;   for more info.

;; But remember that your posts can't be longer than 140 characters
;; long.  All of these functions will also prompt you for your user
;; name (usually the email address you signed up to twitter with)
;; and password the first time in a given Emacs session.  Note that
;; twitter uses `Basic Authentication' for user authentication,
;; which translates to, basically none.  It's not secure for
;; anything more than casual attacks.

;;			     FOR READING

;; This is a work in progress.  Just stubs.  I have to figure out
;; how to make some use out of `xml-parse-fragment'.  Until then,
;; `twit-list-followers' is incredibly stupid, but works.

;;			     FOR HACKING

;; See `twit-post-function', which is the backend for posting, and
;; `twit-parse-xml' which grabs an xml file from HTTP and turns it
;; into a list structure (using `xml-parse-fragment').  This is a work
;; in progress.

;; Installing:

;; There's not much to it.  It you want it always there and ready, you
;; can add something to your .emacs file like:

;;  (load-file "/path/to/twit-post.el")

;; or get fancier, to the extent you want and know how (autoloading,
;; keybinding, etc).

;; Notes:

;; `twit-user' gets my vote for variable name of the year.  Ditto
;; `twit-mode' for mode names.

;;; History:

;; 2007-3-16 theron tlax <thorne@timbral.net>
;; * 0.0.1 -- Initial release.  Posting only.
;; 2007-3-17 ''
;; * 0.0.2 -- Near-total rewrite; better documentation; use standard
;;            Emacs xml and url packages; minor mode; a little
;;            abstraction; some stubs for the reading functions.
;; * 0.0.3 -- Doc and other minor changes.

;; Bugs:

;; * Posts with semicolons are being silently truncated.  I don't
;;   know why.

;;   `twit-list-followers' may not work if it is the first thing you
;;   do.

;; Report bugs to me at the listed email address.  Additionally,
;; report the absence of bugs if you are using a system not in the
;; list below of systems tested at least minimally:

;; Twit 0.0.2 / Emacs 22.0.93.1 / windows-nt
;; Twit 0.0.2 / Emacs 23.0.51.1 / gnu/linux
;; Twit 0.0.3 / Emacs 22..92.1 / gnu/linux

;;; To do:

;; Finish reading, and then add a timer for auto-update.

 
;;; Code:

(require 'xml)
(require 'url)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar twit-version-number "0.0.3")

;; Most of this will be used in the yet-to-be-written twitter
;; reading functions.
(defvar twit-base-url "http://twitter.com")
(defconst twit-update-url 
  (concat twit-base-url "/statuses/update.xml"))
(defconst twit-puplic-timeline-file
  (concat twit-base-url "/statuses/public_timeline.xml"))
(defconst twit-friend-timeline-file
  (concat twit-base-url "/statuses/friends_timeline.xml"))
(defconst twit-followers-file
  (concat twit-base-url "/statuses/followers.xml"))
(defconst twit-friend-list-file
  (concat twit-base-url "/statuses/friends.xml"))

(defconst twit-success-msg 
  "Post sent (no guarantees, though)")
(defconst twit-too-long-msg 
  "Post not sent because length exceeds 140 characters")

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General purpose library to wrap twitter.com's api
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun twit-parse-xml (url)
  "Retrieve file at URL and parse with `xml-parse-fragment'.
Emacs' url package will prompt for authentication info if required."
  (let ((result nil))
    (save-window-excursion
      (set-buffer (url-retrieve-synchronously url))
      (goto-char (point-min))
      (setq result (xml-parse-fragment))
      (kill-buffer (current-buffer)))
    result))

(defun twit-post-function (url post)
  (let ((url-request-method "POST")
	(url-request-data (concat "status=" post)))
    (url-retrieve url (lambda (arg) (kill-buffer (current-buffer))))))

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helpers for the interactive functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun twit-query-for-post ()
  "Query for a Twitter.com post text in the minibuffer."
  (read-string "Post (140 char max): "))

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main interactive functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun twit-post ()
  "Send a post to twitter.com.
Prompt the first time for password and username \(unless
`twit-user' and/or `twit-pass' is set\) and for the text of the
post; thereafter just for post text.  Posts must be <= 140 chars
long."
  (interactive)
  (let* ((post (twit-query-for-post)))
    (if (> (length post) 140)
	(error twit-too-long-msg)
      (if (twit-post-function twit-update-url post)
	  (message twit-success-msg)))))

;;;###autoload
(defun twit-post-region (start end)
  "Send text in the region as a post to twitter.com.
Uses `twit-post-function' to do the dirty work and to obtain
needed user and password information.  Posts must be <= 140 chars
long."
  (interactive "r")
  (let ((post (buffer-substring start end)))
    (if (> (length post) 140)
	(error twit-too-long-error)
      (if (twit-post-function twit-update-url post)
	  (message twit-success-msg)))))

;;;###autoload
(defun twit-post-buffer ()
  "Post the entire contents of the current buffer to twitter.com.
Uses `twit-post-function' to do the dirty work and to obtain
needed user and password information.  Posts must be <= 140 chars
long."
  (interactive)
  (let ((post (buffer-substring (point-min) (point-max))))
    (if (> (length post) 140)
	(error twit-too-long-error)
      (if (twit-post-function twit-update-url post)
	  (message twit-success-msg)))))

;;;###autoload
(defun twit-list-followers ()
  "Display a list of all your twitter.com followers' names."
(interactive)
(pop-to-buffer (get-buffer-create "*Twit-followers*"))
(kill-region (point-min) (point-max))
(loop for name in 
      (loop for name in
	    (loop for user in 
		  (xml-get-children
		   (cadr (twit-parse-xml twit-followers-file)) 'user)
		  collect (sixth user))
	    collect (third name))
      do (insert (concat name "\n"))))

;;; Added by Jonathan Arkell
;;;###autoload
(defun twit-show-recent-tweets ()
  "Display a list of the most recent twewets from your followwers."
  (interactive)
  (pop-to-buffer (get-buffer-create "*Twit-recent*"))
  (kill-region (point-min) (point-max))
  (loop for status-node 
		in (xml-get-children (cadr (twit-parse-xml twit-friend-timeline-file)) 'status)
		do (insert
			(concat (twit-user-node-in-friend-timeline-xml status-node)
					" : " 
					(twit-status-node-in-friend-timeline-xml status-node)
					"\n"))))

(defun twit-user-node-in-friend-timeline-xml (node)
  (third (eighth (tenth node))))

(defun twit-status-node-in-friend-timeline-xml (node)
  (third (eighth node)))


;;;###autoload
(define-minor-mode twit-mode 
  "Toggle twit-mode.
Globally binds some keys to Twit's interactive functions.

With no argument, this command toggles the mode. 
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

\\{twit-mode-map}" nil
" Twit" 
'(("\C-c\C-tp" . twit-post)
  ("\C-c\C-tr" . twit-post-region)
  ("\C-c\C-tb" . twit-post-buffer)
  ("\C-c\C-tf" . twit-list-followers))
 :global t
 :group 'twit
 :version twit-version-number)

(provide 'twit)

;;; twit.el ends here
