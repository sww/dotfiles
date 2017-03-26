;;; package -- Summary
;;; Commentary:
;;; Code:

(package-initialize)

;; Set the path from $PATH.
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Sets the dotfiles emacs dir.
(defvar emacsdir
  (file-name-directory
    (replace-regexp-in-string
       (regexp-quote "\n") ""
       (shell-command-to-string
       "ls -al ~/.emacs.d/init.el | awk '{print $NF}'"))))

(load-file (concat (file-name-as-directory emacsdir) "util.el"))

;; Setup MELPA.
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t))

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; Global-ish configs -- anything that affects other packages or global emacs settings.

(delete-selection-mode) ;; For being able to C-d selected lines.
(setq delete-active-region 0)

(setq make-backup-files nil) ;; Don't leave ~ backup files everywhere.

(setq ring-bell-function 'ignore) ;; No visual nor audio bell.

(setq initial-scratch-message "") ;; Empty scratch buffer.
(setq inhibit-startup-message t) ;; No welcome buffer.

(icomplete-mode t) ;; Suggestions.
;; Set the max height of the matches in the minibuffer.
(setq icomplete-prospects-height 1)
;; Separator character between suggestions.
(setq icomplete-separator "  ")

;; Settings for the look and feel of emacs.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(package-selected-packages
   (quote
    (icomplete-mode go-mode ace-window counsel ivy swiper avy zygospore which-key use-package powerline multi-term markdown-mode magit jsx-mode jinja2-mode jedi ido-vertical-mode hlinum flycheck exec-path-from-shell dumb-jump color-theme-solarized beacon base16-theme)))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))

(load-theme 'solarized t)

(setq-default indent-tabs-mode nil)
(setq tab-width 1)
(setq tab-stop-list '(4 8 12 16))
(setq mac-command-modifier 'meta)

(setq-default abbrev-mode t)
(setq abbrev-file-name (concat (file-name-as-directory emacsdir) "abbrevs.el"))

(column-number-mode 1)
(line-number-mode 1)
(global-linum-mode)
(show-paren-mode 1)

(require 'powerline)
(powerline-default-theme)

;; Custom colors for flymake.
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-errline ((((class color)) (:foreground "OrangeRed" :underline "Red"))))
 '(flymake-warnline ((((class color)) (:foreground nil :underline "LightPink2")))))

;; Change this last since the other themes probably change the cursor color.
(set-cursor-color "orange")

;; Show the buffer's full file path in the title.
(setq frame-title-format '((buffer-file-name "%f"
                           (dired-directory dired-directory "%b"))))

(global-set-key [f8] 'comment-region)
(global-set-key [f9] 'uncomment-region)
(global-set-key [f10] nil) ;; Prevent the default action -- mainly for OSX.
(global-set-key [C-tab] 'other-window)
(global-set-key "\M-p" 'next-buffer)
(global-set-key "\M-n" 'previous-buffer)
(global-set-key "\M-o" 'mode-line-other-buffer)
(global-set-key "\C-u" 'kill-before)
(global-set-key "\M-^" 'replace-string)
(global-set-key (kbd "\C-x B") 'buffer-menu)
(global-set-key (kbd "\C-x K") 'close-and-kill-this-pane)
(global-set-key (kbd "\C-x \C-k") 'kill-this-buffer)
(global-set-key (kbd "\C-c u") 'universal-argument)

;; http://emacsredux.com/blog/2013/03/30/go-back-to-previous-window/
;; Go backwards with C-x O when there are more than two windows split.
(global-set-key (kbd "C-x O") (lambda ()
                                (interactive)
                                (other-window -1)))

(global-set-key (kbd "C-x j") 'switch-to-previous-buffer)
(define-key minibuffer-local-map [M-backspace] 'backward-delete-word)

;; HTML.
(defun my-html-mode-hooks ()
  "Set ups for how I like html mode."
  (auto-fill-mode 0) ;; Prevents auto indenting.
)
(add-hook 'html-mode-hook 'my-html-mode-hooks)

;; Python.
(add-hook 'python-mode-hook 'which-function-mode)

;; Package stuff.

(use-package ace-window
  :bind (("M-g b" . ace-window))
  :ensure)

(use-package auto-complete
  :config
  (setq-default ac-sources '(ac-source-words-in-buffer))
  :ensure)

(use-package avy
  :bind (("M-g c" . avy-goto-char-timer)
         ("M-g l" . avy-goto-line)
         ("M-g w" . avy-goto-word-0))
  :ensure)

(use-package counsel
  :bind
  (("C-x C-d" . counsel-git))
  (("M-x" . counsel-M-x))
  :ensure)

(use-package dumb-jump
  :config
  (setq dumb-jump-selector 'ivy)
  :bind (("C-M-o" . dumb-jump-go-other-window)
         ("C-M-p" . dumb-jump-back)
         ("C-M-q" . dumb-jump-quick-look)
         ("C-M-g" . dumb-jump-go))
  :ensure)

(use-package flycheck
  :init
  (add-hook 'go-mode-hook 'flycheck-mode)
  (add-hook 'python-mode-hook 'flycheck-mode)
  :config
  (set-face-attribute 'flycheck-error nil :foreground "OrangeRed" :underline "Red")
  (set-face-attribute 'flycheck-warning nil :foreground nil :underline "lightpink2")
  (setq flycheck-highlighting-mode 'lines)
  (setq flycheck-display-errors-delay 0.15)
  :ensure)

(use-package go-mode
  :init
  (add-hook 'before-save-hook #'gofmt-before-save)
  (add-hook 'go-mode-hook 'which-function-mode)
  :config
  (setq gofmt-command "goimports")
  (setq gofmt-args nil)
  :ensure)

(use-package ivy
  :bind
  (("C-x C-b" . ivy-switch-buffer))
  :ensure)

(use-package jedi
  :init
  (add-hook 'python-mode-hook 'jedi:setup)
  (add-hook 'python-mode-hook 'jedi:ac-setup)
  :config
  (setq jedi:complete-on-dot t)
  (setq jedi:get-in-function-call-delay 1500) ;; Delay in showing the tooltip.
  (setq jedi:environment-virtualenv
        (list "virtualenv" "--system-site-packages"))
  :bind (("M-." . jedi:complete))
  :ensure)

(use-package magit
  :bind
  (("C-x g" . magit-status))
  :ensure)

(use-package swiper
  :bind (("C-s" . swiper))
  :bind (("M-s s" . swiper-all))
  :ensure)

(use-package which-key
  :ensure)

;;; init.el ends here
