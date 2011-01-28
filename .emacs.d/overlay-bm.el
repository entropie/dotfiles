(defun filter (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
        (if val (push val acc))))
    (nreverse acc)))

(defun setOverlayBookmark ()
  (interactive)
  (let*
      ((pnt (line-beginning-position))
       (ovrly (make-overlay pnt (line-end-position))))
    (overlay-put ovrly 'face 'highlight)
    (overlay-put ovrly 'pointHistory t)
    ovrly))

(defun overlayBookmarkFilterFunc (ovrly) (and (overlay-get ovrly 'pointHistory) ovrly))
(defun overlayBookmarkFilterStartsFunc (ovrly) (and (overlay-get ovrly 'pointHistory) (overlay-start ovrly)))

(defun nextOverlayBookmark ()
  (interactive)
  (let*
      (
       (pnt (line-end-position))
       (ovrly_starts (or (filter 'overlayBookmarkFilterStartsFunc (overlays-in pnt (point-max)))
                         (filter 'overlayBookmarkFilterStartsFunc (overlays-in (point-min) pnt)))))
    (if ovrly_starts
        (goto-char (reduce (lambda (a b) (if (< a b) a b)) ovrly_starts))
      (message "no items in history"))))


(defun clearOverlayBookmarks ()
  (interactive)
  (let 
      ((ovrlys (filter 'overlayBookmarkFilterFunc (overlays-in (point-min) (point-max)))))
    (mapcar 'delete-overlay ovrlys)
    (message "cleared point history")))

(defun toggleOverlayBookmark ()
  (interactive)
  (let 
      ((ovrlys (filter 'overlayBookmarkFilterFunc (overlays-in (line-beginning-position) (line-end-position)))))
    (if ovrlys
        (mapcar 'delete-overlay ovrlys)
      (setOverlayBookmark))))

(defun overlayBookmarkRefresh ()
  "stretches the overlays from the beginning to end of a line"
  (interactive)
  (let 
      ((ovrlys (filter 'overlayBookmarkFilterFunc (overlays-in (point-min) (point-max))))
       (lf (lambda (tmp) (save-excursion
                           (goto-char (overlay-start tmp))
                           (move-overlay tmp (overlay-start tmp) (line-end-position)))))
       )
    (if ovrlys
        (mapcar lf ovrlys)
      (setPointHistory))))

(global-set-key [C-f12] 'clearOverlayBookmarks)
(global-set-key [f12] 'nextOverlayBookmark)
(global-set-key [M-f12] 'toggleOverlayBookmark)
