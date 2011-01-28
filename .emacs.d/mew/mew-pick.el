;;; mew-pick.el --- Picking up messages for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Oct  2, 1996

;;; Code:

(require 'mew)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Pick commands
;;;

(defun mew-summary-pick-msgs (folder regionp)
  (let* ((dir (mew-expand-folder folder))
	 (default-directory dir) ;; buffer local
	 beg end region msg msgs)
    (if (mew-mark-active-p) (setq regionp t))
    (cond
     (regionp
      (setq region (mew-summary-get-region))
      (setq beg (car region))
      (setq end (cdr region))
      (save-excursion
	(goto-char beg)
	(while (re-search-forward mew-regex-msg-or-part end t) ;; for thread
	  (setq msg (mew-summary-message-number))
	  (setq msg (mew-msg-get-filename msg))
	  (setq msgs (cons msg msgs))
	  (forward-line)))
      (setq msgs (delq nil msgs))
      (setq msgs (nreverse msgs)))
     (t
      (setq msgs (mew-dir-messages ".")) ;; all messages
      (setq msgs (delq nil msgs))))))

(defun mew-summary-pick (&optional regionp)
  "Pick messages according to a specified pick pattern.
Then put the '*' mark onto them. 'mewl' or 'grep' is called as a
picking command. If called with '\\[universal-argument]', the
target is the region."
  (interactive "P")
  (mew-pickable
   (mew-summary-with-mewl
    (let* ((folder (mew-pickable-folder))
	   (msgs (mew-summary-pick-msgs folder regionp))
	   (prompt (format "%s/%s pick" mew-prog-mewl mew-prog-grep))
	   (prog mew-prog-grep)
	   (opts mew-prog-grep-opts)
	   mew-inherit-pick-mewlp
	   grepp pattern prog-opts-pat)
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
	  (cond
	   (grepp
	    (mew-sinfo-set-find-key pattern)
	    (message "Picking messages in %s..." folder)
	    (setq msgs (mew-summary-pick-with-grep prog opts pattern folder msgs))
	    (message "Picking messages in %s...done" folder))
	   (t
	    (mew-sinfo-set-find-key nil) ;; force to ask a user
	    (message "Picking messages in %s..." folder)
	    (setq msgs (mew-summary-pick-with-mewl pattern folder msgs))
	    (message "Picking messages in %s...done" folder)))
	  (mew-summary-pick-ls folder msgs)))))))
  
(defun mew-summary-pick-ls (folder msgs)
  (let ((preline 0) (msgsback msgs)
	threadmsgs total linenum n)
    (if (null msgs)
	(message "No message to be marked")
      (setq n (length msgs))
      (if (= n 1)
	  (message "Marking 1 message...")
	(message "Marking %d messages..." n))
      (save-excursion
	(cond
	 ((mew-thread-p)
	  ;; Thread mode
	  (setq threadmsgs (mew-summary-thread-get-msglst
			    (mew-vinfo-get-top) 'separator))
	  (setq total (length threadmsgs))
	  (goto-char (point-min))
	  (while (and msgs (not (eobp)))
	    (setq linenum (member (car msgs) threadmsgs))
	    (if (null linenum)
		(setq msgsback (delete (car msgs) msgsback))
	      (setq linenum (- total (length linenum)))
	      (forward-line (- linenum preline))
	      (setq preline linenum)
	      (mew-summary-mark-as mew-mark-review))
	    (setq msgs (cdr msgs)))
	  (set-buffer-modified-p nil))
	 ((mew-virtual-p)
	  ;; Virtual mode
	  (goto-char (point-min))
	  (setq msgsback nil)
	  (while (and msgs (not (eobp)))
	    (when (re-search-forward (mew-regex-sumsyn-msg (car msgs)) nil t)
	      (setq msgsback (cons (car msgs) msgsback))
	      (mew-summary-mark-as mew-mark-review)
	      (forward-line))
	    (setq msgs (cdr msgs)))
	  (setq msgsback (nreverse msgsback))
	  (set-buffer-modified-p nil)))
	;; Summary mode (physical)
	(when (get-buffer folder)
	  (set-buffer folder)
	  (save-excursion
	    (goto-char (point-min))
	    (setq msgs msgsback)
	    (while (and msgs (not (eobp)))
	      (when (re-search-forward (mew-regex-sumsyn-msg (car msgs)) nil t)
		(mew-summary-mark-as mew-mark-review)
		(forward-line))
	      (setq msgs (cdr msgs)))
	    (set-buffer-modified-p nil))))
      (if (= n 1)
	  (message "Marking 1 message...done")
	(message "Marking %d messages...done" n)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Pick internal
;;;

(defun mew-summary-pick-range (src-msgs)
  (when src-msgs
    (let* ((min (string-to-number (car src-msgs)))
	   (max min)
	   i)
      (setq src-msgs (cdr src-msgs))
      (while src-msgs
	(setq i (string-to-number (car src-msgs)))
	(if (< i min) (setq min i))
	(if (> i max) (setq max i))
	(setq src-msgs (cdr src-msgs)))
      (format "%d-%d" min max))))

(defun mew-summary-pick-with-mewl (pattern folder src-msgs)
  "A function to pick messages matching PATTERN with 'mewl'"
  (let ((pfolder (mew-expand-folder2 folder))
	(range (mew-summary-pick-range src-msgs))
	(opts (list "-b" mew-mail-path
		    "-l" (number-to-string mew-scan-max-field-length)
		    "-x" mew-suffix
		    "-p" pattern))
	msgs)
    (if range
	(setq opts (nconc opts (list pfolder range)))
      (setq opts (nconc opts (list pfolder))))
    (with-temp-buffer
      (mew-set-buffer-multibyte t)
      (mew-piolet
       mew-cs-text-for-read mew-cs-text-for-write
       (apply 'call-process mew-prog-mewl nil t nil opts))
      (goto-char (point-min))
      (while (re-search-forward mew-regex-message-files2 nil t)
	(setq msgs (cons (mew-match-string 1) msgs))
	(forward-line)))
    (nreverse msgs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Grep
;;;

(defun mew-summary-pick-with-grep (prog opts pattern folder src-msgs)
  "A function to pick messages matching PATTERN."
  (let* ((dir (mew-expand-folder folder))
	 (default-directory dir) ;; buffer local
	 msgs nxt)
    ;; no sort here
    (if (= (length src-msgs) 1) (setq src-msgs (cons null-device src-msgs)))
    (if pattern (setq pattern (mew-cs-encode-arg pattern)))
    (with-temp-buffer
      (mew-set-buffer-multibyte t)
      (cd dir)
      (mew-piolet
       mew-cs-text-for-read mew-cs-text-for-write
       (while src-msgs
	 (goto-char (point-max))
	 (setq nxt (nthcdr mew-prog-grep-max-msgs src-msgs))
	 (if nxt (setcdr (nthcdr (1- mew-prog-grep-max-msgs) src-msgs) nil))
	 (apply 'call-process prog nil t nil
		(append opts (and pattern (list pattern)) src-msgs))
	 (setq src-msgs nxt)))
      (goto-char (point-min))
      (while (re-search-forward mew-regex-message-files2 nil t)
	(setq msgs (cons (mew-match-string 1) msgs))
	(forward-line)))
    (setq msgs (mew-uniq-list msgs))
    (setq msgs (mapcar 'string-to-number msgs))
    (setq msgs (sort msgs '<))
    (mapcar 'number-to-string msgs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Displaying keyword
;;;

(defun mew-summary-find-keyword-down (&optional arg)
  "Display a message in the forward direction, find a keyword 
and highlight it. 
In Summary mode, the target is messages marked with '*'.
In Virtual mode, the target is all messages in a Virtual folder.
The keyword is stored in a buffer local variable in Summary mode. 
If no keyword is set to the variable, this command first asks you 
for a keyword. 
If you want to change the stored keyword, execute this command with '\\[universal-argument]'."
  (interactive "P")
  (let* ((fld (mew-summary-folder-name))
	 (msg (mew-summary-message-number))
	 (fid (mew-frame-id))
	 (ofld (mew-current-get-fld fid))
	 (omsg (mew-current-get-msg fid))
	 (cwin (get-buffer-window (current-buffer)))
	 (mbuf (mew-buffer-message))
	 (mwin (get-buffer-window mbuf))
	 (key (mew-sinfo-get-find-key))
	 (mark (mew-summary-get-mark))
	 (virtualp (mew-virtual-p))
	 (search t)
	 end top)
    (when (or arg (not (stringp key)))
      (setq key (read-string
		 "Keyword: " (or (car mew-input-pick-pattern-hist) key)))
      (mew-sinfo-set-find-key key))
    (cond
     ((and (or virtualp (equal mew-mark-review mark))
	   (or (not (string= fld ofld)) (not (string= msg omsg))))
      (mew-summary-display-asis)
      (setq top t))
     ((or (null mwin)
	  (not (or virtualp (equal mew-mark-review mark)))
	  (or (not (string= fld ofld)) (not (string= msg omsg)))
	  (save-excursion (set-buffer mbuf) (eobp)))
      (cond
       (virtualp
	(forward-line)
	(mew-summary-display-asis)
	(setq top t))
       ((mew-summary-down-mark mew-mark-review)
	(mew-summary-display-asis)
	(setq top t))
       (t
	(setq search nil)))))
    (setq mwin (get-buffer-window mbuf))
    (if (not search)
	(message "No more marked messages")
      (select-window mwin)
      (unwind-protect
	  (progn
	    (if top (goto-char (point-min)))
	    (if (setq end (re-search-forward key nil t))
		(progn
		  (isearch-highlight (- end (length key)) end)
		  (recenter (/ (window-height) 2)))
	      (goto-char (point-max))
	      (message "Keyword '%s' is not found" key)
	      (recenter -1)))
	(select-window cwin)))))

(defun mew-summary-find-keyword-up (&optional arg)
  "Display a message in the backward direction, find a keyword 
and highlight it. 
In Summary mode, the target is messages marked with '*'.
In Virtual mode, the target is all messages in a Virtual folder.
The keyword is stored in a buffer local variable in Summary mode. 
If no keyword is set to the variable, this command first asks you 
for a keyword. 
If you want to change the stored keyword, execute this command with '\\[universal-argument]'."
  (interactive "P")
  (let* ((fld (mew-summary-folder-name))
	 (msg (mew-summary-message-number))
	 (fid (mew-frame-id))
	 (ofld (mew-current-get-fld fid))
	 (omsg (mew-current-get-msg fid))
	 (cwin (get-buffer-window (current-buffer)))
	 (mbuf (mew-buffer-message))
	 (mwin (get-buffer-window mbuf))
	 (key (mew-sinfo-get-find-key))
	 (mark (mew-summary-get-mark))
	 (virtualp (mew-virtual-p))
	 (search t)
	 beg bottom)
    (when (or arg (not (stringp key)))
      (setq key (read-string
		 "Keyword: " (or (car mew-input-pick-pattern-hist) key)))
      (mew-sinfo-set-find-key key))
    (cond
     ((and (or virtualp (equal mew-mark-review mark))
	   (or (not (string= fld ofld)) (not (string= msg omsg))))
      (mew-summary-display-asis)
      (setq bottom t))
     ((or (null mwin)
	  (not (or virtualp (equal mew-mark-review mark)))
	  (or (not (string= fld ofld)) (not (string= msg omsg)))
	  (save-excursion (set-buffer mbuf) (bobp)))
      (cond
       (virtualp
	(forward-line -1)
	(mew-summary-display-asis)
	(setq bottom t))
       ((mew-summary-down-mark mew-mark-review)
	(mew-summary-display-asis)
	(setq bottom t))
       (t
	(setq search nil)))))
    (setq mwin (get-buffer-window mbuf))
    (if (not search)
	(message "No more marked messages")
      (select-window mwin)
      (unwind-protect
	  (progn
	    (if bottom (goto-char (point-max)))
	    (if (setq beg (re-search-backward key nil t))
		(progn
		  (isearch-highlight beg (+ beg (length key)))
		  (recenter (/ (window-height) 2)))
	      (goto-char (point-min))
	      (message "Keyword '%s' is not found" key)
	      (recenter 0)))
	(select-window cwin)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Pick macro
;;;

(defvar mew-pick-macro-alist nil)

(defun mew-pick-define-macro (str1 str2)
  "Define pick macro."
  (interactive (list (read-string "Pick pattern: ")
		     (read-string "Macro body: ")))
  ;; macro-pattern is a string including no #, or
  ;; a string in a form FIELD=#1 #2 #3...#n.
  ;; #1 can be replaced by #.
  (let (args body asc value)
    (while (string-match "\\(#[0-9]*\\)[, ]*" str1)
      (setq args (cons (intern (match-string 1 str1)) args))
      (setq str1 (replace-match "" nil t str1)))
    (while (string-match "#[0-9]*" str2)
      (setq body (cons (substring str2 0 (match-beginning 0)) body))
      (setq body (cons (intern (match-string 0 str2)) body))
      (setq str2 (substring str2 (match-end 0))))
    (setq body (cons str2 body))
    (setq asc (assoc str1 mew-pick-macro-alist))
    (setq value (cons (nreverse args) (nreverse body)))
    (if asc
	(setcdr asc value)
      (setq mew-pick-macro-alist
	    (cons (cons str1 value) mew-pick-macro-alist)))))
    
(defun mew-pick-macro-expand (name args)
  (let ((asc (assoc name mew-pick-macro-alist))
	alist args2 body body-copy assq)
    (if (not asc)
	name
      (setq args2 (nth 1 asc))
      (setq body (nthcdr 2 asc))
      (while (and args args2)
	(setq alist (cons (cons (car args2) (car args)) alist))
	(setq args (cdr args))
	(setq args2 (cdr args2)))
      (while body
	(if (stringp (car body))
	    (setq body-copy (cons (car body) body-copy))
	  (setq assq (assq (car body) alist))
	  (if assq (setq body-copy (cons (cdr assq) body-copy))))
	(setq body (cdr body)))
      (concat "("
	      (mew-pick-macro-expand-string
	       (apply 'concat (nreverse body-copy)))
	      ")"))))

(defun mew-pick-macro-expand-string (str)
  (if (string= str "") 
      ""
    (let ((first (string-to-char str))
	  asc key rest eq-flag val args)
      (if (memq first (list ?\( ?\! ?\& ?\| ?= mew-sp ?\)))
	  (concat (char-to-string first)
		  (mew-pick-macro-expand-string (substring str 1)))
	(if (string-match "=\\| \\|)\\|&\\||" str)
	    (if (string= (match-string 0 str) "=")
		(progn
		  (setq eq-flag t)
		  (setq key (substring str 0 (match-end 0)))
		  (setq rest (substring str (match-end 0))))
	      (setq key (substring str 0 (match-beginning 0)))
	      (setq rest (substring str (match-beginning 0))))
	  (setq key str)
	  (setq rest ""))
	(setq asc (assoc key mew-pick-macro-alist))
	(cond
	 (asc
	  (setq args (nth 1 asc))
	  (while args
	    (if (string-match ",\\| \\|)\\|&\\||" rest)
		(progn
		  (setq val (cons (substring rest 0 (match-beginning 0)) val))
		  (setq rest (substring rest (match-beginning 0))))
	      (setq val (cons rest val))
	      (setq rest ""))
	    (setq args (cdr args)))
	  (concat
	   (mew-pick-macro-expand key (nreverse val))
	   (mew-pick-macro-expand-string rest)))
	 (eq-flag
	  (if (string-match " \\|)\\|&\\||" rest)
	      (progn
		(setq val (substring rest 0 (match-beginning 0)))
		(setq rest (substring rest (match-beginning 0))))
	    (setq val rest)
	    (setq rest ""))
	  (concat key val (mew-pick-macro-expand-string rest)))
	 (t
	  (concat key (mew-pick-macro-expand-string rest))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Lex and Parser
;;;

(defmacro mew-pick-lex-cut ()
  `(when (and start (< start i))
     (let ((word (substring pattern start i)) key op val)
       (if (string-match "^\\([-a-z0-9]+\\)\\(!?==?\\)\\(.+\\)$" word)
	   (progn
	     (setq key (mew-match-string 1 word))
	     (setq op  (mew-match-string 2 word))
	     (setq val (mew-match-string 3 word))
	     (setq val (mew-remove-single-quote val))
	     (setq ret (cons (list op key val) ret)))
	 (setq word (mew-remove-single-quote word))
	 (setq ret (cons word ret)))
       (setq start nil))))

(defun mew-pick-lex (pattern)
  (let ((len (length pattern))
	(i 0) ret start c dq)
    (while (< i len)
      (setq c (aref pattern i))
      (cond
       (dq
	(if (or (char-equal c ?\") (char-equal c ?')) (setq dq nil)))
       ((or (char-equal c ?\") (char-equal c ?'))
	(unless start (setq start i))
	(setq dq t))
       ((char-equal c ?\()
	(mew-pick-lex-cut)
	(setq ret (cons 'open ret)))
       ((char-equal c ?\))
	(mew-pick-lex-cut)
	(setq ret (cons 'close ret)))
       ((char-equal c ?&)
	(mew-pick-lex-cut)
	(setq ret (cons 'and ret)))
       ((char-equal c ?|)
	(mew-pick-lex-cut)
	(setq ret (cons 'or ret)))
       ((and (char-equal c ?!)
	     (not (char-equal (aref pattern (1+ i)) ?=)))
	(mew-pick-lex-cut)
	(setq ret (cons 'not ret)))
       ((char-equal c mew-sp)
	(mew-pick-lex-cut))
       (t
	(unless start (setq start i))))
      (setq i (1+ i)))
    (mew-pick-lex-cut)
    (nreverse ret)))

(defun mew-pick-parse (mew-inherit-pick-tokens)
  (let (mew-inherit-pick-ret)
    (mew-pick-parse1)
    (nreverse mew-inherit-pick-ret)))

(defun mew-pick-parse1 ()
  (when mew-inherit-pick-tokens
    (mew-pick-parse-elements)
    (while mew-inherit-pick-tokens
      (mew-pick-parse-and/or))))

(defun mew-pick-parse-elements ()
  (let ((cur (car mew-inherit-pick-tokens)))
    (setq mew-inherit-pick-tokens (cdr mew-inherit-pick-tokens))
    (setq mew-inherit-pick-mewlp t)
    (cond
     ((stringp cur)
      (setq mew-inherit-pick-mewlp nil)
      (setq mew-inherit-pick-ret (cons cur mew-inherit-pick-ret)))
     ((listp cur)
      (setq mew-inherit-pick-ret (cons cur mew-inherit-pick-ret)))
     ((eq cur 'open)
      (mew-pick-parse-open/close))
     ((eq cur 'close)
      (error "')' alone"))
     ((eq cur 'not)
      (mew-pick-parse-not))
     ((eq cur 'and)
      (error "'&' alone"))
     ((eq cur 'or)
      (error "'|' alone")))))

(defun mew-pick-parse-and/or ()
  (setq mew-inherit-pick-mewlp t)
  (let ((cur (car mew-inherit-pick-tokens)))
    (cond
     ((or (stringp cur) (listp cur))
      (unless mew-inherit-pick-omit-and
	(setq mew-inherit-pick-ret (cons 'and mew-inherit-pick-ret)))
      (mew-pick-parse-elements))
     ((eq cur 'open)
      (unless mew-inherit-pick-omit-and
	(setq mew-inherit-pick-ret (cons 'and mew-inherit-pick-ret)))
      (setq mew-inherit-pick-tokens (cdr mew-inherit-pick-tokens))
      (mew-pick-parse-open/close))
     ((eq cur 'close)
      (error "')' alone"))
     ((eq cur 'not)
      (unless mew-inherit-pick-omit-and
	(setq mew-inherit-pick-ret (cons 'and mew-inherit-pick-ret)))
      (setq mew-inherit-pick-tokens (cdr mew-inherit-pick-tokens))
      (mew-pick-parse-not))
     ((eq cur 'and)
      (unless mew-inherit-pick-omit-and
	(setq mew-inherit-pick-ret (cons 'and mew-inherit-pick-ret)))
      (setq mew-inherit-pick-tokens (cdr mew-inherit-pick-tokens))
      (mew-pick-parse-elements))
     ((eq cur 'or)
      (setq mew-inherit-pick-ret (cons 'or mew-inherit-pick-ret))
      (setq mew-inherit-pick-tokens (cdr mew-inherit-pick-tokens))
      (mew-pick-parse-elements)))))

(defun mew-pick-parse-not ()
  (if (eq (car mew-inherit-pick-ret) 'not)
      (setq mew-inherit-pick-ret (cdr mew-inherit-pick-ret))
    (setq mew-inherit-pick-ret (cons 'not mew-inherit-pick-ret)))
  (mew-pick-parse-elements))

(defun mew-pick-parse-open/close ()
  (let ((sub mew-inherit-pick-tokens)
	(level 0)
	cur prv pprv found)
    (catch 'loop
      (while mew-inherit-pick-tokens
	(setq pprv prv)
	(setq prv mew-inherit-pick-tokens)
	(setq cur (car mew-inherit-pick-tokens))
	(setq mew-inherit-pick-tokens (cdr mew-inherit-pick-tokens))
	(cond
	 ((eq cur 'open)
	  (setq level (1+ level)))
	 ((eq cur 'close)
	  (if (/= level 0)
	      (setq level (1- level))
	    (if (not pprv) (error "'( )' is empty"))
	    (setcdr pprv nil)
	    (let ((mew-inherit-pick-tokens sub))
	      (setq mew-inherit-pick-ret (cons 'open mew-inherit-pick-ret))
	      (mew-pick-parse1)
	      (setq mew-inherit-pick-ret (cons 'close mew-inherit-pick-ret)))
	    (throw 'loop (setq found t)))))))
    (unless found (error "')' alone"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Canonicalizer
;;;

(defun mew-pick-native-text (prefix token)
  (let (suffix func)
    (cond
     ((eq token 'and)
      (setq suffix "and"))
     ((eq token 'or)
      (setq suffix "or"))
     ((eq token 'open)
      (setq suffix "open"))
     ((eq token 'close)
      (setq suffix "close"))
     ((eq token 'not)
      (setq suffix "not"))
     ((stringp token)
      (setq suffix "key"))
     ((listp token)
      (setq suffix "kyvl")))
    (when suffix
      (setq func (intern (concat prefix suffix)))
      (if (fboundp func) (funcall func token)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; mewl/grep
;;;

(defun mew-pick-canonicalize-pattern (pattern)
  (mapconcat
   'mew-pick-native-text-mewl
   (mew-pick-parse (mew-pick-lex pattern))
   " "))

(defun mew-pick-native-text-mewl (token)
  (mew-pick-native-text "mew-pick-pattern-mewl-" token))

(defun mew-pick-pattern-mewl-and   (sym) "&")
(defun mew-pick-pattern-mewl-or    (sym) "|")
(defun mew-pick-pattern-mewl-open  (sym) "(")
(defun mew-pick-pattern-mewl-close (sym) ")")
(defun mew-pick-pattern-mewl-not   (sym) "!")
(defun mew-pick-pattern-mewl-key   (key) key)
(defun mew-pick-pattern-mewl-kyvl  (kyvl)
  (format "%s%s%s" (nth 1 kyvl) (nth 0 kyvl) (nth 2 kyvl)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Namazu
;;;

(defun mew-pick-canonicalize-pattern-namazu (pattern)
  (let ((mew-inherit-pick-omit-and t))
    (mapconcat
     'mew-pick-native-text-namazu
     (mew-pick-parse (mew-pick-lex pattern))
     " ")))

(defun mew-pick-native-text-namazu (token)
  (mew-pick-native-text "mew-pick-pattern-namazu-" token))

(defun mew-pick-pattern-namazu-and   (sym) "and")
(defun mew-pick-pattern-namazu-or    (sym) "or")
(defun mew-pick-pattern-namazu-open  (sym) "(")
(defun mew-pick-pattern-namazu-close (sym) ")")
(defun mew-pick-pattern-namazu-not   (sym) "not")
(defun mew-pick-pattern-namazu-key   (key) key)
(defun mew-pick-pattern-namazu-kyvl  (kyvl)
  (when (string= (nth 0 kyvl) "=")
    (format "+%s:%s" (nth 1 kyvl) (nth 2 kyvl))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Google
;;;

(defun mew-pick-canonicalize-pattern-google (pattern)
  (let* ((mew-inherit-pick-omit-and t)
	 (tokens (mew-pick-parse (mew-pick-lex pattern)))
	 tkn ret)
    (while tokens
      (setq tkn (car tokens))
      (setq tokens (cdr tokens))
      (if (eq tkn 'not)
	  (if (stringp (car tokens))
	      (progn
		(setq ret (cons (concat "-" (car tokens)) ret))
		(setq tokens (cdr tokens)))
	    nil) ;; xxx
	(setq ret (cons tkn ret))))
    (setq tokens (nreverse ret))
    (mapconcat
     'mew-pick-native-text-google
     tokens
     " ")))

(defun mew-pick-native-text-google (token)
  (mew-pick-native-text "mew-pick-pattern-google-" token))

(defun mew-pick-pattern-google-and   (sym) "and")
(defun mew-pick-pattern-google-or    (sym) "or")
(defun mew-pick-pattern-google-open  (sym) "'(' is not supported")
(defun mew-pick-pattern-google-close (sym) "')' is not supported")
(defun mew-pick-pattern-google-not   (sym) "-")
(defun mew-pick-pattern-google-key   (key) key)
(defun mew-pick-pattern-google-kyvl  (kyvl)
  (let ((op (nth 0 kyvl)))
    (error "'%s' is not supported" op)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; WDS
;;;

(defun mew-pick-canonicalize-pattern-wds (pattern)
  (let* ((mew-inherit-pick-omit-and t)
	 (tokens (mew-pick-parse (mew-pick-lex pattern)))
	 tkn ret)
    (while tokens
      (setq tkn (car tokens))
      (setq tokens (cdr tokens))
      (if (eq tkn 'not)
	  (if (stringp (car tokens))
	      (progn
		(setq ret (cons (concat "-" (car tokens)) ret))
		(setq tokens (cdr tokens)))
	    nil) ;; xxx
	(setq ret (cons tkn ret))))
    (setq tokens (nreverse ret))
    (mapconcat
     'mew-pick-native-text-wds
     tokens
     " ")))

(defun mew-pick-native-text-wds (token)
  (mew-pick-native-text "mew-pick-pattern-wds-" token))

(defun mew-pick-pattern-wds-and   (sym) "and")
(defun mew-pick-pattern-wds-or    (sym) "or")
(defun mew-pick-pattern-wds-open  (sym) "'(' is not supported")
(defun mew-pick-pattern-wds-close (sym) "')' is not supported")
(defun mew-pick-pattern-wds-not   (sym) "-")
(defun mew-pick-pattern-wds-key   (key) key)
(defun mew-pick-pattern-wds-kyvl  (kyvl)
  (let ((op (nth 0 kyvl)))
    (error "'%s' is not supported" op)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Spotlight
;;;

(defun mew-pick-canonicalize-pattern-spotlight (pattern)
  (let* ((tokens (mew-pick-parse (mew-pick-lex pattern)))
;;	 tkn ret
	 str)
;;     (while tokens
;;       (setq tkn (car tokens))
;;       (setq tokens (cdr tokens))
;;       (if (eq tkn 'not)
;; 	  (if (stringp (car tokens))
;; 	      (progn
;; 		(setq ret (cons (list "!=" "body" (car tokens)) ret))
;; 		(setq tokens (cdr tokens)))
;; 	    nil) ;; xxx
;; 	(setq ret (cons tkn ret))))
;;     (setq tokens (nreverse ret))
    (setq str (mapconcat
	       'mew-pick-native-text-spotlight
	       tokens
	       " "))
    (concat "kMDItemContentType == \"mew\"w && " str)))

(defun mew-pick-native-text-spotlight (token)
  (mew-pick-native-text "mew-pick-pattern-spotlight-" token))

(defun mew-pick-pattern-spotlight-and   (sym) "&&")
(defun mew-pick-pattern-spotlight-or    (sym) "||")
(defun mew-pick-pattern-spotlight-open  (sym) "(")
(defun mew-pick-pattern-spotlight-close (sym) ")")
(defun mew-pick-pattern-spotlight-not   (sym) (error "'!' is not supported"))
(defun mew-pick-pattern-spotlight-key   (key)
  (format "kMDItemTextContent == \"%s\"wc" key))
(defun mew-pick-pattern-spotlight-kyvl  (kyvl)
  (let ((op (nth 0 kyvl))
	(ky (nth 1 kyvl))
	(vl (nth 2 kyvl))
	kmd)
    (cond
     ((string= ky "subject")
      (setq kmd "kMDItemTitle"))
     ((string= ky "from")
      (setq kmd "kMDItemAuthors"))
     ((string= ky "body")
      (setq kmd "kMDItemTextContent"))
     (t
      (error "'%s' is not supported" ky)))
    (cond
     ((string= op "=")
      (format "%s == \"%s\"wc" kmd vl))
     ((string= op "==")
      (format "%s == \"%s\"w" kmd vl))
;;      ((string= op "!=")
;;       (format "%s != \"%s\"wc" kmd vl))
;;      ((string= op "!==")
;;       (format "%s != \"%s\"c" kmd vl))
     (t
      (error "'%s' is not supported" op)))))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Old
;;;

(defun mew-summary-grep-old ()
  "Obsoleted command."
  (interactive)
  (mew-message-for-summary "This command was obsoleted. Use '\\[mew-summary-pick]'"))


(provide 'mew-pick)

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

;;; mew-pick.el ends here
