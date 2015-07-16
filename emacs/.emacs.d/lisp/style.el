;; Settings for the look and feel of emacs.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load-theme 'solarized t)

(setq-default indent-tabs-mode nil)
(setq default-tab-width 1)
(setq tab-stop-list '(4 8 12 16))
(setq mac-command-modifier 'meta)
(setq default-abbrev-mode t)

(column-number-mode 1)
(line-number-mode 1)
(global-linum-mode)

(require 'powerline)
(powerline-default-theme)

;; Custom colors for flymake.
(custom-set-faces
  '(flymake-errline ((((class color)) (:underline "Red"))))
  '(flymake-warnline ((((class color)) (:underline "LightPink2")))))

;; Change this last since the other themes probably change the cursor color.
(set-cursor-color "orange")
