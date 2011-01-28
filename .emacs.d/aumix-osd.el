;;; aumix-osd.el --- aumix elisp front-end
;; Copyright (C) 2006 Fabio Viola

;; Author:     Fabio Viola <fabioviola@salug.it>
;; URL:        http://www.salug.it/~fabioviola/aumix-osd.el
;; Updated:    2006-12-10
;; Keywords:   multimedia
;; Created:    2006-12-10

;; This file is not part of GNU Emacs.
;; This file is released under the GNU General Public License.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; aumix-osd.el is an improved version of aumix.el that uses osd.el
;; to print volume level on screen. 
;; You can edit osd preferences customizing osd-* variables.
;; If you don't need to see volume on screen use aumix.el, you find it
;; on http://www.salug.it/fabioviola/aumix.el

; needed library

(require 'osd)

; variables

(defgroup aumix-osd nil
  "Aumix frontend for Emacs"
  :prefix "aumix-osd-")

(defcustom aumix-exec "aumix"
  "The aumix executable name; if you want you can add the path
before it")


(defcustom aumix-loved-level "75"
  "The preferred volume for the main channel. Using 
aumix-to-loved-level you can switch you mixer to that volume")

; functions

(defun aumix-osd-mute ()
  "It mutes the volume"
  (interactive)
  (start-process aumix-exec nil aumix-exec "-v0")
  (osd-broadcast-string "Muto")
  :group 'aumix)

(defun aumix-osd-raise (level)
  "It raises the volume to the chosen level"
  (interactive "sHow much should I raise it? [1-100] ")
  (start-process aumix-exec nil aumix-exec (concat "-v+" level))
  :group 'aumix)

(defun aumix-osd-lower (level)
  "It lowers the volume to the chosen level"
  (interactive "sHow much should I lower it? [1-100] ")
  (start-process aumix-exec nil aumix-exec (concat "-v-" level))
  (osd-broadcast-string (concat "Volume " level "%"))
  :group 'aumix)

(defun aumix-osd-switch-to-level (level)
  "It raises the volume to the chosen level"
  (interactive "sWhich volume do you prefer? [0-100] ")
  (start-process aumix-exec nil aumix-exec "-v" level)
  (osd-broadcast-string (concat "Volume " level "%"))
  :group 'aumix)

(defun aumix-osd-max ()
  "It raises the volume to the max level"
  (interactive)
  (start-process aumix-exec nil aumix-exec "-v" "100")
  (osd-broadcast-string "Max volume")
  :group 'aumix)

(defun aumix-osd-to-loved-level ()
  "It takes the volume to the level stored in aumix-loved-level"
  (interactive)
  (start-process aumix-exec nil aumix-exec "-v" aumix-loved-level)
  (osd-broadcast-string (concat "Volume " aumix-loved-level "%"))
  :group 'aumix)

(provide 'aumix)

;;; aumix.el ends here