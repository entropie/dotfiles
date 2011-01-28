(require 'color-theme)

(defun color-theme-bmate ()
  " sasas"
  (interactive)
  (color-theme-install
   '(color-theme-bmate
     (
      (background-color . "DodgerBlue4")
      (border-color . "black")
      (background-mode . dark)
      (cursor-color . "Plum1")
      (foreground-color . "#eeaeee")
      (mouse-color . "yellow1")
      (top-toolbar-shadow-color . "#e5e5e0e0e1e1")
      )
     (default ((t (nil))))
     (blue ((t (:foreground "blue"))))
     (bold ((t (:bold t))))
     (bold-italic ((t (nil))))
     (border-glyph ((t (nil))))
     (fringe ((t (:background "DodgerBlue4" .color "yellow"))))
     (font-lock-builtin-face ((t (:foreground "tomato1"))))
     (font-lock-comment-face ((t (:bold t :foreground "salmon" :height 0.8))))
     (font-lock-comment-delimiter-face ((t (:bold t :foreground "coral" :heigh 0.8))))
     (font-lock-constant-face ((t (:bold t :foreground "grey14" ))))
     (font-lock-doc-string-face ((t (:foreground "#00C000"))))
     (font-lock-doc-face ((t (:foreground "SkyBlue1" :background "RoyalBlue4"))))
     (font-lock-function-name-face ((t (:bold nil :foreground "MediumOrchid2" :height 1.2))))
     (font-lock-keyword-face ((t (:bold t :foreground "#ffa500" :height 1.1))))
     (font-lock-other-emphasized-face ((t (:bold t :foreground "gold1"))))
     (font-lock-other-type-face ((t (:bold nil :foreground "gold1"))))
     (font-lock-preprocessor-face ((t (:foreground "plum"))))
     (font-lock-reference-face ((t (:bold t :foreground "orangered"))))
     (font-lock-string-face ((t (:foreground "plum" :underline "MediumOrchid4"))))
     (font-lock-type-face ((t (:bold t :foreground "ivory2" :height 1.1))))
     (font-lock-variable-name-face ((t (:bold t :foreground "#66cd00" :underline "MediumOrchid4"))))
     (font-lock-warning-face ((t (:foreground "tomato"))))
     (green ((t (:foreground "green"))))
     (gui-button-face ((t (:background "grey75" :foreground "black"))))
     (gui-element ((t (:size "nil" :background "#e7e3d6" :foreground" #000000"))))
     (highlight ((t (:background "firebrick4" :foreground "yellow1"))))
     (isearch ((t (:bold t :background "pale turquoise" :foreground "blue"))))
     (hilit-chg ((t (:bold t :background "pale turquoise" :foreground "blue"))))
     (italic ((t (nil))))
     (lazy-highlight-face ((t (:bold t :foreground "dark magenta"))))
     (left-margin ((t (nil))))
     (list-mode-item-selected ((t (:bold t :background "gray68" :foreground "yellow"))))
     ;;:overline "CadetBlue" :underline "turquoise4"
     (modeline ((t (:background "seagreen" :foreground "DarkSeaGreen" :height 0.8))))
     (mode-line-inactive ((t (:italic t :background "DeepSkyBlue4" :foreground "cadetblue" :box (:line-width -1 :color "SeaGreen") :slant oblique :weight light :height 0.8))))
     (modeline-buffer-id ((t (:background "DarkGreen" :foreground "PaleGreen"))))
     (modeline-mousable ((t (:background "red" :foreground "darkblue"))))
     (modeline-mousable-minor-mode ((t (:background "sienna" :foreground "darkblue"))))
     (paren-blink-off ((t (:foreground "DodgerBlue4"))))
     (paren-match ((t (:background "red" :foreground "yellow"))))
     (paren-mismatch ((t (:background "green"))))
     (pointer ((t (:background "white"))))
     (primary-selection ((t (:bold t :background "medium sea green"))))
     (red ((t (:foreground "red"))))
     (region ((t (:background "MediumSeaGreen" :foreground "yellow"))))
     (right-margin ((t (nil))))
     (secondary-selection ((t (:background "gray91" :foreground "sienna3"))))
     (show-paren-match-face ((t (:background "#87E8FD" :foreground "blue"))))
     (show-paren-mismatch-face ((t (:background "#FD87BB" :foreground "blue"))))
     (show-trailing-whitespace ((t (:background "red" :foreground "blue"))))
     (text-cursor ((t (:background "tan" :foreground "DodgerBlue4"))))
     (toolbar ((t (:background "#e7e3d6" :foreground "#000000"))))
     (underline ((t (:underline t))))
     (vertical-divider ((t (:background "red" :foreground "yellow"))))
     (widget-button-face ((t (:bold t))))
     (widget-button-pressed-face ((t (:foreground "red"))))
     (widget-documentation-face ((t (:foreground "dark green"))))
     (widget-field-face ((t (:background "gray85"))))
     (widget-inactive-face ((t (:foreground "dim gray"))))
     (widget-single-line-field-face ((t (:background "gray85"))))
     (yellow ((t (:foreground "yellow"))))
     (circe-originator-face ((t (:foreground "OliveDrab2"))))
     (circe-my-message-face ((t (:foreground "SpringGreen3"))))
     (circe-highlight-nick-face ((t (:foreground "orchid2"))))
     (circe-server-face ((t (:foreground "thistle4"))))
     (circe-prompt-face ((t (:foreground "yellow1" :height 1.0))))
     (lui-button-face ((t (:foreground "yellow1"))))
     (lui-time-stamp-face ((t (:foreground "CadetBlue"))))
     (rcirc-timestamp ((t (:foreground "#4a708b"))))
     (rcirc-server ((t (:foreground "CadetBlue"))))
     (rcirc-prompt ((t (:foreground "#ffa500"))))
     (rcirc-bright-nick ((t (:foreground "#cdbe70" :underline nil))))
     (rcirc-dim-nick ((t (:foreground "#cd5b45"))))
     (rcirc-nick-in-message ((t (:foreground "MediumOrchid2"))))
     (rcirc-nick-in-message-full-line ((t (:foreground "#ffc125"))))
     (dired-warning ((t (:foreground "Orange"))))
     (dired-directory ((t (:foreground "LightBlue"))))
     (dired-filename ((t (:foreground "PaleGreen"))))
     
     (comint-highlight-prompt ((t (:foreground "grey10"))))
     (compilation-error ((t (:foreground "grey10"))))
     (compilation-line-number  ((t (:foreground "grey10"))))
     (eshell-ls-archive-face ((t (:bold t :foreground "IndianRed"))))
     (eshell-ls-backup-face ((t (:foreground "Grey"))))
     (eshell-ls-clutter-face ((t (:foreground "DimGray"))))
     (eshell-ls-directory-face ((t (:bold t :foreground "MediumSlateBlue"))))
     (eshell-ls-executable-face ((t (:foreground "Coral"))))
     (eshell-ls-missing-face ((t (:foreground "black"))))
     (eshell-ls-picture-face ((t (:foreground "Violet")))) ; non-standard face
     (eshell-ls-product-face ((t (:foreground "LightSalmon"))))
     (eshell-ls-readonly-face ((t (:foreground "Aquamarine"))))
     (eshell-ls-special-face ((t (:foreground "Gold"))))
     (eshell-ls-symlink-face ((t (:foreground "White"))))
     (eshell-ls-text-face ((t (:foreground "medium aquamarine")))) ; non-standard face
     (eshell-ls-todo-face ((t (:bold t :foreground "aquamarine")))) ; non-standard face
     (eshell-ls-unreadable-face ((t (:foreground "DimGray"))))
     (eshell-prompt-face ((t (:foreground "powder blue"))))
     (flyspell-duplicate ((t (:foreground "powder blue"))))
     (flyspell-incorrect ((t (:foreground "#ee30a7"))))
     (zmacs-region ((t (:background "white" :foreground "midnightblue")))))))

(add-to-list 'color-themes '(color-theme-mate \"mate\" \"ackro\"))

