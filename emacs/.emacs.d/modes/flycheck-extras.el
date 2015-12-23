(require 'flycheck)

(set-face-attribute 'flycheck-error nil :foreground "OrangeRed" :underline "Red")
(set-face-attribute 'flycheck-warning nil :foreground nil :underline "lightpink2")

(setq flycheck-display-errors-delay 0.15)
