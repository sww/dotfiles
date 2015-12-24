(require 'flycheck)

(set-face-attribute 'flycheck-error nil :foreground "OrangeRed" :underline "Red")
(set-face-attribute 'flycheck-warning nil :foreground nil :underline "lightpink2")

;; Highlight the whole line on warning or error.
(setq flycheck-highlighting-mode 'lines)

(setq flycheck-display-errors-delay 0.15)
