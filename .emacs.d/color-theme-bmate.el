(require 'color-theme)

(defun color-theme-bmate ()
  "Sasas dsadsad sadsad sadsad sad sad sadsad sadasd"
  (interactive)
  (color-theme-install
   '(color-theme-bmate
     (
      (background-color . "#1C3661")
      (foreground-color . "#BACCBE")
      (border-color . "black")
      (background-mode . dark)
      (cursor-color . "#FF9100")
      (mouse-color . "#FF9100")
      (top-toolbar-shadow-color . "#e5e5e0e0e1e1")
      )
     (default ((t (nil))))
     (blue ((t (:foreground "blue"))))
     (bold ((t (:bold t))))
     (bold-italic ((t (:bold t :italic t))))
     (border-glyph ((t (nil))))
     (fringe ((t (:background "#1C3661"))))
     ;; lasdsad sadlsadls adsald sald
     ;; dsadasd 
     (font-lock-builtin-face ((t (:foreground "#84FF57" :bold t))))
     (font-lock-comment-face ((t (:bold nil :foreground "#607487" :height 0.8 :background nil))))
     (font-lock-comment-delimiter-face ((t (:bold nil :foreground "#607487" :height 0.8 :background nil))))
     (font-lock-constant-face ((t (:bold t :foreground "#41BF91" ))))
     (font-lock-doc-string-face ((t (:foreground "#00C000"))))
     (font-lock-doc-face ((t (:foreground "SkyBlue1" :background "RoyalBlue4"))))
     (font-lock-function-name-face ((t (:bold t :foreground "#FA7000" :background nil :underline nil))))
     (font-lock-keyword-face ((t (:bold t :foreground "#9076D6"))))
     (font-lock-other-emphasized-face ((t (:bold t :foreground "gold1"))))
     (font-lock-other-type-face ((t (:bold nil :foreground "gold1"))))
     (font-lock-preprocessor-face ((t (:foreground "plum"))))
     (font-lock-reference-face ((t (:bold t :foreground "orangered"))))
     (font-lock-string-face ((t (:foreground "#fff" :background "#1c4d61"))))
     (font-lock-type-face ((t (:bold t :foreground "#fff" :underline nil))))
     (font-lock-variable-name-face ((t (:bold nil :foreground "#66cd00" :background "#1c4d61"))))
     (font-lock-warning-face ((t (:background "#8C2624" :foreground "#000"))))
     (green ((t (:foreground "green"))))
     (gui-button-face ((t (:background "grey75" :foreground "black"))))
     (gui-element ((t (:size "nil" :background "#e7e3d6" :foreground "#000000"))))
     (highlight ((t (:background "firebrick4" :foreground "yellow1"))))
     (bm-face ((t (:background "blue" :foreground nil))))
     (isearch ((t (:bold t :background "pale turquoise" :foreground "blue"))))
     (italic ((t (nil))))
     (left-margin ((t (nil))))
     (list-mode-item-selected ((t (:bold t :background "#000" :foreground "yellow"))))
     (modeline ((t (:background "seagreen" :foreground "#000" :height 0.8))))
     (mode-line-inactive ((t (:italic t :background "#1C3661" :foreground "cadetblue" :box (:line-width -1 :color "#1C3661") :slant oblique :weight light :height 0.8))))
     (modeline-buffer-id ((t (:background "#2156ad" :foreground "#fff"))))
     (modeline-mousable ((t (:background "red" :foreground "darkblue"))))
     (modeline-mousable-minor-mode ((t (:background "sienna" :foreground "darkblue"))))
     (pointer ((t (:background "white"))))
     (red ((t (:foreground "red"))))
     (primary-selection ((t (:background "#2156ad"))))
     (region ((t (:background "#2156ad" :foreground nil))))
     (right-margin ((t (nil))))
     (secondary-selection ((t (:background "gray91" :foreground "sienna3"))))
     (show-paren-match-face ((t (:background "#8aad29" :foreground "#000"))))
     (show-paren-mismatch-face ((t (:background "#6e3328" :foreground nil))))
     (show-trailing-whitespace ((t (:background nil :underline "red"))))
     (trailing-whitespace ((t (:background nil :underline "red"))))
     (underline ((t (:underline t))))
     (widget-button-face ((t (:bold t))))
     (widget-button-pressed-face ((t (:foreground "red"))))
     (widget-documentation-face ((t (:foreground "dark green"))))
     (widget-field-face ((t (:background "gray85"))))
     (widget-inactive-face ((t (:foreground "dim gray"))))
     (widget-single-line-field-face ((t (:background "gray85"))))
     (yellow ((t (:foreground "yellow"))))
     (lui-button-face ((t (:foreground "yellow1"))))
     (lui-time-stamp-face ((t (:foreground "CadetBlue"))))

     (rcirc-timestamp ((t (:foreground "#fff" :height 0.7))))
     (rcirc-server ((t (:foreground "CadetBlue"))))
     (rcirc-prompt ((t (:foreground "#ffa500"))))
     (rcirc-bright-nick ((t (:foreground "#cdbe70" :underline nil))))
     (rcirc-dim-nick ((t (:foreground "#808080"))))
     (rcirc-nick-in-message ((t (:foreground "#ffc125"))))
     (rcirc-my-nick ((t (:foreground "#ffc125" :underline "#ffc125"))))
     (rcirc-nick-in-message-full-line ((t (:foreground "#ffc125"))))
     (dired-warning ((t (:foreground "Orange"))))
     (dired-directory ((t (:foreground "LightBlue"))))
     (dired-filename ((t (:foreground "PaleGreen"))))

     (howm-menu-key-face ((t (:foreground "orange"))))
     (howm-menu-list-face ((t (:foreground "white"))))
     (howm-mode-keyword-face ((t (:background "dodgerblue4" :foreground "cyan" :underline nil :slant normal))))
     (howm-mode-ref-face ((t (:italic t :foreground "greenyellow" :slant italic))))
     (howm-mode-title-face ((t (:bold t :background "dodgerblue4" :foreground "cyan" :underline nil :weight bold :height 1.5))))
     (howm-mode-wiki-face ((t (:background "dodgerblue4" :foreground "cyan" :underline nil))))
     (howm-reminder-deadline-face ((t (:background "red" :foreground "yellow" :underline nil :weight normal))))
     (howm-reminder-defer-face ((t (:background "purple4" :foreground "magenta" :weight normal))))
     (howm-reminder-done-face ((t (:foreground "gray"))))
     (howm-reminder-normal-face ((t (:background "darkgreen" :foreground "yellow" :underline nil :weight normal))))
     (howm-reminder-schedule-face ((t (:background "darkorange2" :foreground "yellow" :slant normal :weight normal))))
     (howm-reminder-separator-face ((t (:foreground "black"))))
     (howm-reminder-today-face ((t (:bold t :box nil :weight bold))))
     (howm-reminder-todo-face ((t (:background "dodgerblue4" :foreground "cyan" :underline nil :weight normal))))
     (howm-reminder-tomorrow-face ((t (:background "pink" :foreground "black"))))
     (howm-view-empty-face ((t (nil))))
     (howm-view-hilit-face ((t (:foreground "red"))))
     (howm-view-name-face ((t (nil))))

     
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

