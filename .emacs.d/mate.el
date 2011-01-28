(require 'color-theme)

(defun color-theme-mate ()
  ""
  (interactive)
  (color-theme-install
   '(color-theme-mate
     (
      (background-color . "seashell")
      (background-mode . light)
      (border-color . "red")
      (cursor-color . "cornflowerblue")
      (foreground-color . "black")
      (mouse-color . "slateblue"))
     ((help-highlight-face . underline)
      (list-matching-lines-face . bold)
      (view-highlight-face . highlight)
      (widget-mouse-face . highlight))
     (default ((t (:stipple nil :background "ivory" :foreground "black"
                            :inverse-video nil :box nil :strike-through nil :overline nil
                            :underline nil :slant normal :weight normal :height 100 :width normal))))
     (bold ((t (:bold t :weight bold))))
     (bold-italic ((t (:italic t :bold t :slant italic :weight bold))))
     (border ((t (:background "black"))))
     (cursor ((t (:background "slateblue" :foreground "black"))))
     (fixed-pitch ((t (:family "terminus"))))
     (font-lock-builtin-face ((t (:foreground "black"))))
     (font-lock-comment-face ((t (:italic t :background "linen" :foreground "steelblue"
                                          :slant italic))))
     (font-lock-constant-face ((t (:foreground "seagreen" :background "aliceblue"))))
     (font-lock-doc-face ((t (:background "lemonChiffon"))))
     (font-lock-function-name-face ((t (:bold t :underline t :weight
                                              bold :foreground "darkslateblue"))))
     (font-lock-keyword-face ((t (:foreground "MediumVioletRed"))))
     (font-lock-string-face ((t (:background "AntiqueWhite"))))
     (font-lock-type-face ((t ( :foreground "deeppink"))))
     (font-lock-variable-name-face ((t (:foreground "black"))))
     (font-lock-warning-face ((t (:bold t :foreground "Red" :weight
                                        bold))))
     (fringe ((t (:background "grey95"))))
     (header-line ((t (:bold t :weight bold :underline t :background
                             "grey90" :foreground "grey20" :box nil))))
     (highlight ((t (:background "mistyRose" :foreground "black"))))
     (isearch ((t (:background "magenta4" :foreground
                               "lightskyblue1"))))
     (isearch-lazy-highlight-face ((t (:background "paleturquoise"))))
     (italic ((t (:italic t :slant italic))))
     (menu ((t (nil))))
     (mode-line ((t (:bold t :background "mistyRose" :foreground "navy"
                           :underline t :weight bold))))
     (mouse ((t (:background "slateblue"))))
     (region ((t (:background "lavender" :foreground "black"))))
     (scroll-bar ((t (nil))))
     (secondary-selection ((t (:background "yellow"))))
     (tool-bar ((t (:background "grey75" :foreground "black" :box
                                (:line-width 1 :style released-button)))))
     (trailing-whitespace ((t (:background "indianred"))))
     (underline ((t (:underline t))))
     (variable-pitch ((t (:family "helv"))))
     (widget-button-face ((t (:bold t :weight bold))))
     (widget-button-pressed-face ((t (:foreground "red"))))
     (widget-documentation-face ((t (:foreground "dark green"))))
     (widget-field-face ((t (:background "gray85"))))
     (widget-inactive-face ((t (:foreground "dim gray"))))
     (circe-originator-face ((t (:foreground "darkslateblue" :bold t :weight bold))))
     (circe-my-message-face ((t (:foreground "HotPink" :bold t :weight bold))))
     (circe-highlight-nick-face ((t (:foreground "magenta" :bold t :weight bold))))
     (circe-server-face ((t (:foreground "MediumPurple"))))

     
     (widget-single-line-field-face ((t (:background "gray85")))))))

(add-to-list 'color-themes '(color-theme-mate \"mate\" \"ackro\"))

(message "a")
