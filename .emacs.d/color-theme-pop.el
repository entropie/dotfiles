(defun color-theme-pop ()
  "Color theme based on a little package weed."
  (interactive)
  (color-theme-install
   '(color-theme-pop
     (
      (foreground-color . "#EBE2C9")
      (background-color . "#0a0a19")
      (cursor-color . "#ff8833")
      (background-mode . dark)
      (mouse-color . "#ff8833"))
     (region ((t (:background "#0a0a0a"))))
     (show-paren-match-face ((t (:background "#31846A" :bold nil :box nil))))
     (paren-face-match-light ((t (:background "#3c552c" :bold nil :box nil))))
     (show-paren-mismatch-face ((t (:background "#FBC17C" :foreground "#D64325" :bold t :box nil))))
     (fringe ((t (:background "#0a0a19"))))
     (isearch ((t (:bold t :foreground "#ff4e00" :background "#cd8b60"))))
     (modeline ((t (:foreground "#f0ffff" :background "#3d3d3d" :height 0.8 ))))
     (mode-line-inactive ((t (:italic t :foreground "#80A1BF" :background "black" :box (:line-width -1 :color "black") :slant oblique :height 0.8))))
     (modeline-buffer-id ((t (:foreground "orange" :background "#141618"))))
     (font-lock-builtin-face ((t (:foreground "#71babd"))))
     (bm-face ((t (:foreground "#ff8c00" :background "#483d8b"))))
     (font-lock-comment-face ((t (:foreground "#7FBC41" :background "#00181e"))))
     (font-lock-comment-delimiter-face ((t (:foreground "#7FBC41" :background "#00181e"))))
     (font-lock-constant-face ((t (:foreground "#90D5BD" :bold t))))
     (font-lock-doc-string-face ((t (:foreground "#3798DC"))))
     (font-lock-doc-face ((t (:foreground "#eead0e"))))
     (font-lock-function-name-face ((t (:foreground "#eeffbb" :bold t))))
     (font-lock-keyword-face ((t (:bold t :foreground "#ff7f00" :bold nil))))
     (font-lock-preprocessor-face ((t (:foreground "309090"))))
     (font-lock-reference-face ((t (:bold t :foreground "#808bed"))))
     (font-lock-string-face ((t (:foreground "#ffcd8b" :background "#142225"))))
     (font-lock-type-face ((t (:foreground "#A4E525" :underline "#4682b4" :height 1.0))))
     (font-lock-variable-name-face ((t (:foreground "silver" :background "#4f0150" ))))
     (action-lock-face ((t (:foreground "#DFFF00" :underline "#225566" :height 1.0 :background "#1864B5"))))
     
     (js2-keyword-face ((t (:bold t :foreground "#ff7f00" :bold nil))))
     (js2-function-name-face ((t (:foreground "#eeffbb" :bold t))))
     (js2-string-face ((t (:foreground "#ffcd8b" :background "#006699"))))
     (js2-builtin-face ((t (:foreground "#A4E525" :underline "#4682b4" :height 1.0))))
     (js2-variable-name-face ((t (:foreground "#66ddff" :background "#225566" ))))

     (font-lock-warning-face ((t (:foreground "#cd9b9b" :background "#8b2500"))))
     (howm-menu-key-face ((t (:foreground "orange"))))
     (howm-menu-list-face ((t (:foreground "white"))))
     (howm-mode-keyword-face ((t (:background "dodgerblue4" :foreground "cyan" :underline nil :slant normal))))
     (howm-mode-ref-face ((t (:italic t :foreground "greenyellow" :slant italic))))
     (howm-mode-title-face ((t (:bold t :background "dodgerblue4" :foreground "cyan" :box (:line-width 8 :color "dodgerblue4") :underline nil :weight bold))))
     (howm-mode-wiki-face ((t (:background "dodgerblue4" :foreground "cyan" :underline nil))))
     (howm-reminder-deadline-face ((t (:background "#c71585" :foreground "yellow" :underline nil :weight normal))))
     (howm-reminder-defer-face ((t (:background "purple4" :foreground "magenta" :weight normal))))
     (howm-reminder-done-face ((t (:foreground "gray"))))
     (howm-reminder-normal-face ((t (:background "darkgreen" :foreground "yellow" :underline nil :weight normal))))
     (howm-reminder-schedule-face ((t (:background "#2e8b57" :foreground "yellow" :slant normal :weight normal))))
     (howm-reminder-separator-face ((t (:foreground "black"))))
     (howm-reminder-today-face ((t (:bold t :box nil :weight bold))))
     (howm-reminder-todo-face ((t (:background "dodgerblue4" :foreground "cyan" :underline nil :weight normal))))
     (howm-reminder-tomorrow-face ((t (:background "pink" :foreground "black"))))
     (howm-view-empty-face ((t (nil))))
     (howm-view-hilit-face ((t (:foreground "#00bfff" :height 1.2))))
     (howm-view-name-face ((t (nil))))
     (info-header-node ((t (:bold t :weight bold))))
     (info-header-xref ((t (:bold t :foreground "sky blue" :weight bold))))
     (info-menu-5 ((t (:underline t))))
     (info-menu-6 ((t (nil))))
     (info-menu-header ((t (:bold t :weight bold :family "helv"))))
     (info-menu-star ((t (:underline t))))
     (info-node ((t (:bold t :weight bold))))
     (info-title-1 ((t (:bold t :weight bold :height 1.728 :family "helv"))))
     (info-title-2 ((t (:bold t :weight bold :height 1.44 :family "helv"))))
     (info-title-3 ((t (:bold t :weight bold :height 1.2 :family "helv"))))
     (info-title-4 ((t (:bold t :weight bold :family "helv"))))
     (info-xref ((t (:bold t :foreground "gold" :weight bold))))
     (info-xref-visited ((t (:bold t :foreground "darkgoldenrod" :weight bold))))
     (isearch ((t (:background "slate blue"))))
     (isearch-lazy-highlight ((t (nil))))
     (isearch-secondary ((t (:foreground "red3"))))
     (iswitchb-current-match ((t (nil))))
     (iswitchb-invalid-regexp ((t (nil))))
     (iswitchb-single-match ((t (nil))))
     (iswitchb-virtual-matches ((t (nil))))
     (emms-playlist-selected-face ((t (:foreground "#ffd700"))))
     (emacs-wiki-bad-link-face ((t (:bold t :foreground "IndianRed1" :weight bold))))
     (emacs-wiki-header-1 ((t (:bold t :weight bold :height 1.728 :family "helv"))))
     (emacs-wiki-header-2 ((t (:bold t :weight bold :height 1.44 :family "helv"))))
     (emacs-wiki-header-3 ((t (:bold t :weight bold :height 1.2 :family "helv"))))
     (emacs-wiki-header-4 ((t (:bold t :weight bold :family "helv"))))
     (emacs-wiki-header-5 ((t (:bold t :weight bold))))
     (emacs-wiki-header-6 ((t (nil))))
     (emacs-wiki-link-face ((t (:bold t :foreground "khaki" :weight bold))))
     (emacs-wiki-verbatim-face ((t (:foreground "gray"))))
     (emms-playlist-track-face ((t (:foreground "darkkhaki"))))
     (emms-playlist-selected-face ((t (:bold t :foreground "gold2" :weight bold))))
     (emms-pbi-mark-marked ((t (nil))))
     (emms-pbi-mark-marked-face ((t (:italic t :background "gray30" :foreground "slate gray" :slant italic))))
     (emms-pbi-song ((t (nil))))
     (emms-pbi-song-face ((t (:foreground "darkkhaki"))))
     (emms-stream-name-face ((t (nil))))
     (emms-stream-url-face ((t (nil))))
     (hi-black-b ((t (:bold t :weight bold))))
     (hi-black-hb ((t (:bold t :weight bold :height 1.67 :family "helv"))))
     (hi-blue ((t (:background "light blue"))))
     (hi-blue-b ((t (:bold t :foreground "blue" :weight bold))))
     (hi-green ((t (:background "green"))))
     (hi-green-b ((t (:bold t :foreground "green" :weight bold))))
     (hi-pink ((t (:background "pink"))))
     (hi-red-b ((t (:bold t :foreground "red" :weight bold))))
     (hi-yellow ((t (:background "yellow"))))
     (highlight ((t (:background "dark slate blue" :foreground "light blue"))))
     (highlight-changes-delete-face ((t (:foreground "gray85" :underline t))))
     (highlight-changes-face ((t (:foreground "gray85"))))
     (highlight-current-line ((t (nil))))
     (highlight-current-line-face ((t (nil))))
     (highline-face ((t (:background "DeepSkyBlue4"))))
     (highline-vertical-face ((t (:background "lightcyan"))))
     (rcirc-server ((t (:foreground "#5cacee" :height 0.7))))
     (rcirc-text ((t (:foreground "#2e2e42"))))
     (rcirc-timestamp ((t (:foreground "#4f94cd" :height 0.7))))
     (rcirc-prompt ((t (:foreground "#ccee00"))))
     (rcirc-nick-in-message-full-line ((t (:foreground "#a3cfff" :weight normal))))
     (rcirc-dim-nick ((t (:foreground "#677482"))))
     (rcirc-other-nick ((t (:foreground "#00bfff"))))
     (rcirc-server-prefix ((t (:foreground "#454545"))))
     (rcirc-bright-nick ((t (:foreground "#da70d6"))))
     (rcirc-my-nick ((t (:foreground "#FD8000"))))
     (rcirc-track-nick ((t (:foreground "#FD8000" :background "#305A85" :bold nil))))
     (circe-originator-face ((t (:foreground "#8b7d6b"))))
     (circe-my-message-face ((t (:foreground "#eec900"))))
     (circe-highlight-nick-face ((t (:foreground "#eec900"))))
     (circe-server-face ((t (:foreground "#5cacee"))))
     (circe-prompt-face ((t (:foreground "#eec900" :height 1.0))))
     (lui-button-face ((t (:foreground "yellow1"))))
     (lui-time-stamp-face ((t (:foreground "#8b8878"))))
     (flyspell-incorrect ((t (:background "#505983" :foreground "#EE5050" :box nil :underline nil :weight normal :strike-through nil))))
     (flyspell-duplicate ((t (:foreground nil :underline "#900000" :weight normal))))
     (mew-face-header-key ((t (:foreground "#6d300f"))))
     (mew-face-header-important ((t (:bold t :foreground "#3CD142"))))
     (mew-face-header-to ((t (:foreground "#8a89a4"))))
     (mew-face-header-from ((t (:foreground "#bfbed4"))))
     (mew-face-header-marginal ((t (:foreground "#2e2e42"))))
     (mew-face-header-date ((t (:foreground "#5c5b77"))))
     (mew-face-header-private ((t (:foreground "red4"))))
     (mew-face-header-subject ((t (:bold t :foreground "#C5FDCF"))))
     (mew-face-body-url ((t (:foreground "#80A1BF" :underline "#80A1BF"))))
     (mew-face-body-cite1  ((t (:foreground "#46464b"))))
     (comint-highlight-prompt ((t (:foreground "#7F8F98" :bold t))))
     (dired-directory ((t (:bold t :foreground "khaki" :weight bold))))
     (dired-face-boring ((t (:foreground "Gray65"))))
     (dired-face-directory ((t (:bold t :foreground "sky blue" :weight bold))))
     (dired-face-executable ((t (:foreground "green yellow"))))
     (dired-face-flagged ((t (:foreground "tomato"))))
     (dired-face-header ((t (:background "grey75" :foreground "gray30"))))
     (dired-face-marked ((t (:italic t :foreground "slate gray" :slant italic))))
     (dired-face-permissions ((t (:foreground "aquamarine"))))
     (dired-face-setuid ((t (:foreground "gray85"))))
     (dired-face-socket ((t (:foreground "gray85"))))
     (dired-face-symlink ((t (:foreground "cyan"))))
     (dired-flagged ((t (:foreground "red"))))
     (dired-header ((t (:bold t :weight bold :height 1.44 :family "helv"))))
     (dired-ignored ((t (:foreground "gray50"))))
     (dired-mark ((t (:foreground "yellow" :background "red" :bold t :weight bold))))
     (dired-marked ((t (:italic t :foreground "slate gray" :slant italic))))
     (dired-symlink ((t (:foreground "IndianRed1"))))
     (dired-warning ((t (nil))))
     (modelinepos-column-warning ((t (:background "#305A85" :foreground "#ff0000"))))
     (modelinepos-info ((t (:foreground "#d2691e" :background "#305A85" ))))
     (term-black ((t (:foreground "black"))))
     (term-blackbg ((t (:background "black"))))
     (term-blue ((t (:foreground "blue"))))
     (term-blue-bold-face ((t (nil))))
     (term-blue-face ((t (nil))))
     (term-blue-inv-face ((t (nil))))
     (term-blue-ul-face ((t (nil))))
     (term-bluebg ((t (:background "blue"))))
     (term-bold ((t (:bold t :weight bold))))
     (term-cyan ((t (:foreground "cyan"))))
     (term-cyan-bold-face ((t (nil))))
     (term-cyan-face ((t (nil))))
     (term-cyan-inv-face ((t (nil))))
     (term-cyan-ul-face ((t (nil))))
     (term-cyanbg ((t (:background "cyan"))))
     (term-default ((t (:background "gray80" :foreground "gray30" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "outline-lucida console"))))
     (term-default-bg ((t (nil))))
     (term-default-bg-inv ((t (nil))))
     (term-default-bold-face ((t (nil))))
     (term-default-face ((t (nil))))
     (term-default-fg ((t (nil))))
     (term-default-fg-inv ((t (nil))))
     (term-default-inv-face ((t (nil))))
     (term-default-ul-face ((t (nil))))
     (term-green ((t (:foreground "green"))))
     (term-green-bold-face ((t (nil))))
     (term-green-face ((t (nil))))
     (term-green-inv-face ((t (nil))))
     (term-green-ul-face ((t (nil))))
     (term-greenbg ((t (:background "green"))))
     (term-invisible ((t (nil))))
     (term-invisible-inv ((t (nil))))
     (term-magenta ((t (:foreground "magenta"))))
     (term-magenta-bold-face ((t (nil))))
     (term-magenta-face ((t (nil))))
     (term-magenta-inv-face ((t (nil))))
     (term-magenta-ul-face ((t (nil))))
     (term-magentabg ((t (:background "magenta"))))
     (term-red ((t (:foreground "red"))))
     (term-red-bold-face ((t (nil))))
     (term-red-face ((t (nil))))
     (term-red-inv-face ((t (nil))))
     (term-red-ul-face ((t (nil))))
     (term-redbg ((t (:background "red"))))
     (term-underline ((t (:underline t))))
     (term-white ((t (:foreground "white"))))
     (term-white-bold-face ((t (nil))))
     (term-white-face ((t (nil))))
     (term-white-inv-face ((t (nil))))
     (term-white-ul-face ((t (nil))))
     (term-whitebg ((t (:background "white"))))
     (term-yellow ((t (:foreground "yellow"))))
     (term-yellow-bold-face ((t (nil))))
     (term-yellow-face ((t (nil))))
     (term-yellow-inv-face ((t (nil))))
     (term-yellow-ul-face ((t (nil))))
     (term-yellowbg ((t (:background "yellow"))))
)))
(provide 'color-theme-pop)