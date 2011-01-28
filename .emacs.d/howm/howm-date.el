;;; howm-date.el --- Wiki-like note-taking tool
;;; Copyright (c) 2002, 2003, 2004, 2005, 2006
;;;   by HIRAOKA Kazuyuki <khi@users.sourceforge.jp>
;;; $Id: howm-date.el,v 1.26 2006/01/06 15:35:46 hira Exp $
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; The GNU General Public License is available by anonymouse ftp from
;;; prep.ai.mit.edu in pub/gnu/COPYING.  Alternately, you can write to
;;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139,
;;; USA.
;;--------------------------------------------------------------------

(require 'howm-common)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; insert & action-lock

(defvar howm-insert-date-pass-through nil)

(defun howm-insert-date ()
  (interactive)
  (let ((date (format-time-string howm-date-format)))
    (insert (format howm-insert-date-format date))
    (howm-action-lock-date date t howm-insert-date-future)))

(defun howm-insert-dtime ()
  (interactive)
  (insert (format-time-string howm-dtime-format)))

;; Sorry for ugly behavior around "new" to keep backward compatibility.
(defun howm-action-lock-date (date &optional new future-p)
  (let* ((pass-through (and new howm-insert-date-pass-through))
         (prompt (howm-action-lock-date-prompt date new pass-through))
         (immediate-chars (if pass-through "" "."))
         (c (howm-read-string prompt immediate-chars "+-~0123456789"
                              pass-through pass-through)))
    (cond
     ((null c) nil) ;; pass through
     ((string= c "")
      (if new
          t
        (howm-action-lock-date-search date)))
     ((string-match "^[-+][0-9]+$" c)
      (howm-action-lock-date-shift (string-to-number c) date))
     ((string-match "^[0-9]+$" c)
      (howm-action-lock-date-set c date future-p))
     ((string-match "^~\\([0-9]+\\)$" c)
      (howm-action-lock-date-repeat (match-string-no-properties 1 c) date))
     ((string-match "^[.]$" c)
      (howm-action-lock-date-set (format-time-string "%Y%m%d") date))
     ((and (string-match "^[-+~]$" c) pass-through)
      (insert c))
     (t (error (format "Can't understand %s." c))))))

(defun howm-action-lock-date-prompt (date new pass-through)
  (let* ((dow (howm-datestr-day-of-week date))
         (common-help "+num(shift), yymmdd(set), ~yymmdd(repeat)")
         (today-help ", .(today)")
         (help (cond ((and new pass-through)
                      common-help)
                     ((and new (not pass-through))
                      (concat "RET(ok), " common-help today-help))
                     ((not new)
                      (concat "RET(list), " common-help today-help))
                     (t
                      (error "Can't happen.")))))
    (format "[%s] %s: " dow help)))

(defun howm-action-lock-date-search (date)
  (howm-set-command 'howm-action-lock-date-search)
  (prog1
      (howm-search date t)
    (howm-action-lock-forward-escape)))

(defun howm-search-today ()
  (interactive)
  (howm-search-past 0))

(defun howm-search-past (&optional days-before)
  (interactive "P")
  (let* ((n (or days-before 0))
         (today (format-time-string howm-date-format))
         (target (howm-datestr-shift today 0 0 (- n))))
    (howm-action-lock-date-search target)))

(defun howm-action-lock-date-shift (n date)
  (howm-datestr-replace (howm-datestr-shift date 0 0 n)))

(defun howm-action-lock-date-set (val date &optional future-p)
  (howm-datestr-replace (howm-datestr-expand val date future-p)))

(defvar howm-action-lock-date-repeat-max 200)
(defun howm-action-lock-date-repeat (until date)
  (let ((every (read-from-minibuffer "Every? [RET(all), num(days), w(week), m(month), y(year)] ")))
    (let ((max-d (howm-datestr-expand until date t))
          (offset-y (if (string= every "y") 1 0))
          (offset-m (if (string= every "m") 1 0))
          (offset-d (or (cdr (assoc every '(("y" . 0) ("m" . 0) ("w" . 7))))
                        (max (string-to-number every) 1))))
      (let ((d date)
            (i 0)
            (check t))
        (catch 'too-many
          (while (progn
                   (setq d (howm-datestr-shift d offset-y offset-m offset-d))
                   (howm-datestr<= d max-d))
            (when (and check (>= i howm-action-lock-date-repeat-max))
              (if (y-or-n-p (format "More than %d lines. Continue? " i))
                  (setq check nil)
                (throw 'too-many nil)))
            (howm-duplicate-line)
            (howm-datestr-replace d)
            (setq i (+ i 1))))))))

(defun howm-make-datestr (y m d)
  (let ((ti (encode-time 0 0 0 d m y)))
    (format-time-string howm-date-format ti)))

(defun howm-datestr-parse (date)
  (string-match howm-date-regexp date)
  (mapcar (lambda (pos)
            (string-to-number (match-string-no-properties pos date)))
          (list howm-date-regexp-year-pos
                howm-date-regexp-month-pos
                howm-date-regexp-day-pos)))

(defun howm-datestr-to-time (date)
  (let* ((ymd (howm-datestr-parse date))
         (y (car ymd))
         (m (second ymd))
         (d (third ymd)))
    (encode-time 0 0 0 d m y)))

(defun howm-datestr-day-of-week (date)
  (format-time-string "%a" (howm-datestr-to-time date)))

(defun howm-datestr-expand (date base &optional future-p)
  (if future-p
      (howm-datestr-expand-future date base)
    (howm-datestr-expand-general date base future-p)))

(defun howm-datestr-expand-future (date base)
  (let ((raw (howm-datestr-expand-general date base nil))
        (future (howm-datestr-expand-general date base t)))
    (when (not (string= raw future))
      (message "Future date"))
    future))

(defun howm-datestr-expand-general (date base &optional future-p)
  (let* ((base-ymd (howm-datestr-parse base))
         (nval (format "%8s" date))
         (given-ymd-str (mapcar (lambda (r)
                                  (substring nval (car r) (second r)))
                                '((0 4) (4 6) (6 8))))
         (ys (car given-ymd-str))
         (ms (second given-ymd-str))
         (ds (third given-ymd-str)))
     (when (string-match "^ +0+$" ys)
       (setq ys "2000"))
     (let* ((given-ymd (mapcar #'string-to-number (list ys ms ds)))
            (carry nil) ;; to force future date
            (dmy (mapcar* (lambda (ox nx)
                         (when future-p
                           (when (and carry (= nx 0))
                             (setq ox (+ ox 1)))
                           (setq carry
                                 (cond ((= nx 0) nil)
                                       ((= nx ox) carry)
                                       ((< nx ox) t)
                                       (t nil))))
                         (if (= nx 0) ox nx))
                       (reverse base-ymd) (reverse given-ymd)))
         (d (car dmy))
         (m (second dmy))
         (y (third dmy)))
       (howm-make-datestr (if (<= y 99) (+ y 2000) y) m d))))

(defun howm-datestr-shift (date y m d)
  (let* ((ymd (howm-datestr-parse date))
         (oy (car ymd))
         (om (second ymd))
         (od (third ymd)))
    (howm-make-datestr (+ oy y) (+ om m) (+ od d))))

(defun howm-datestr<= (date1 date2)
  (or (string< date1 date2)
      (string= date1 date2)))

(defun howm-datestr-replace (date)
  (let ((p (point)))
    (while (not (looking-at howm-date-regexp))
      (backward-char))
    (replace-match date t t)
    (goto-char p)))

(defun howm-duplicate-line ()
  (let ((c (current-column))
        (s (buffer-substring (line-beginning-position) (line-end-position))))
    (end-of-line)
    (insert "\n" s)
    (move-to-column c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; search for next/previous date

(defvar howm-date-current nil)
(make-variable-buffer-local 'howm-date-current)

(defadvice howm-action-lock-date-search (around remember-date (date) activate)
  ad-do-it
  (setq howm-date-current date))

(defvar howm-date-forward-ymd-msg "Searching %s...")
(defvar howm-date-forward-ymd-limit 35)
(defun howm-date-forward-ymd (y m d)
  (when (not howm-date-current)
    (error "Not in date search."))
  (let* ((new-date (howm-datestr-shift howm-date-current y m d))
         (b (current-buffer))
         (step (if (> (+ y m d) 0) +1 -1))
         (c 0))
    (when (catch :found
            (while (progn
                   (when (howm-action-lock-date-search new-date)
                     (throw :found t))
                   (< c howm-date-forward-ymd-limit))
            (setq new-date (howm-datestr-shift new-date 0 0 step))
            (setq c (1+ c))
            (when howm-date-forward-ymd-msg
              (format howm-date-forward-ymd-msg new-date)))
          (error "Not found within %d days." howm-date-forward-ymd-limit))
      (when (not (eq (current-buffer) b))
        (with-current-buffer b
          (howm-view-kill-buffer)))
      (riffle-summary-check t))))

(defmacro howm-date-defun-f/b (func y m d)
  `(defun ,func (&optional k)
     (interactive "P")
     (let ((n (or k 1)))
       (howm-date-forward-ymd ,y ,m ,d))))

(howm-date-defun-f/b howm-date-forward       0 0 n)
(howm-date-defun-f/b howm-date-forward-month 0 n 0)
(howm-date-defun-f/b howm-date-forward-year  n 0 0)
(howm-date-defun-f/b howm-date-backward       0 0 (- n))
(howm-date-defun-f/b howm-date-backward-month 0 (- n) 0)
(howm-date-defun-f/b howm-date-backward-year  (- n) 0 0)

(let ((m howm-view-summary-mode-map))
  (define-key m "+" 'howm-date-forward)
  (define-key m "-" 'howm-date-backward)
  (define-key m ")" 'howm-date-forward)
  (define-key m "(" 'howm-date-backward)
  (define-key m "}" 'howm-date-forward-month)
  (define-key m "{" 'howm-date-backward-month)
  (define-key m "]" 'howm-date-forward-year)
  (define-key m "[" 'howm-date-backward-year)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide 'howm-date)

;;; howm-date.el ends here
