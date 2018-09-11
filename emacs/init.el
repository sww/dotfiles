;;; package -- Summary
;;; Commentary:
;;; Code:

(package-initialize)

;; Set the path from $PATH.
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH"))

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

(setq echo-keystrokes 0.01) ;; Interval for echoing the keystroke in the minibuffer.

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
(setq mac-command-modifier 'meta)

(setq-default abbrev-mode t)
(setq abbrev-file-name (concat (file-name-as-directory emacsdir) "abbrevs.el"))

(column-number-mode 1)
(line-number-mode 1)
(show-paren-mode 1)
(if (version< emacs-version "26.1")
    (global-linum-mode)
  (global-display-line-numbers-mode))

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

;; Do not add the deleted word to the kill ring if in the minibuffer.
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

(use-package avy
  :bind (("M-g c" . avy-goto-char-timer)
         ("M-g l" . avy-goto-line)
         ("M-g w" . avy-goto-word-0))
  :ensure)

(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (global-company-mode)
  (setq company-show-numbers t)
  (custom-set-faces
   '(company-tooltip-selection ((((class color)) (:foreground "ivory1" :background "SandyBrown"))))
   '(company-tooltip-common-selection ((((class color)) (:foreground "ivory1" :background "SandyBrown")))))
  :bind (:map company-active-map
              ("<tab>" . company-complete-common-or-cycle)
              ("<S-tab>" . company-select-previous)
              ("TAB" . company-complete-common-or-cycle)
              ("S-TAB" . company-select-previous))
  :ensure)

(use-package company-go
  :ensure)

(use-package company-jedi
  :config
  (defun my/python-mode-hook ()
    (add-to-list 'company-backends 'company-jedi))
  (add-hook 'python-mode-hook 'my/python-mode-hook)
  :bind (("M-." . company-jedi))
  :ensure)

(use-package company-statistics
  :config
  (add-hook 'after-init-hook 'company-statistics-mode)
  :ensure)

(use-package counsel
  :config
  (fset 'counsel-grepper (cond ((executable-find "rg") 'counsel-rg)
                               ((executable-find "ag") 'counsel-ag)
                               ('counsel-grep)))
  :bind (("C-x C-d" . counsel-git)
         ("M-x" . counsel-M-x)
         ("M-s f" . counsel-grepper))
  :ensure)

(use-package dumb-jump
  :config
  (setq dumb-jump-selector 'ivy)
  :bind (("C-M-o" . dumb-jump-go-other-window)
         ("C-M-p" . dumb-jump-back)
         ("C-M-q" . dumb-jump-quick-look)
         ("C-M-g" . dumb-jump-go))
  :ensure)

(use-package elpy
  :init
  (package-initialize)
  (elpy-enable)
  :config
  ;; elpy enables flymake by default.
  (setq elpy-modules (remove 'elpy-module-flymake elpy-modules))
  :ensure)

(use-package free-keys
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
  :init
  (ivy-mode 1)
  :bind (("C-x C-b" . ivy-switch-buffer)
         ("C-c r" . ivy-resume))
  :ensure)

(use-package magit
  :bind (("C-x g" . magit-status))
  :config
  ;; Display the magit buffer in the current buffer.
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (setq magit-save-repository-buffers nil) ;; Don't ask to save buffers.
  ;; Set the order of sections in the magit-status buffer.
  (setq magit-status-sections-hook
        '(magit-insert-status-headers
          magit-insert-merge-log
          magit-insert-rebase-sequence
          magit-insert-am-sequence
          magit-insert-sequencer-sequence
          magit-insert-bisect-output
          magit-insert-bisect-rest
          magit-insert-bisect-log
          magit-insert-unstaged-changes
          magit-insert-staged-changes
          magit-insert-untracked-files
          magit-insert-stashes
          magit-insert-unpulled-from-upstream
          magit-insert-unpulled-from-pushremote
          magit-insert-unpushed-to-upstream-or-recent
          magit-insert-unpushed-to-pushremote))
  :ensure)

(use-package multiple-cursors
  :bind
  (("C-S-c C-S-c" . 'mc/edit-lines)
   ("C->" . 'mc/mark-next-like-this)
   ("C-<" . 'mc/mark-previous-like-this)
   ("C-c C-<" . 'mc/mark-all-like-this)
   ("C-S-<mouse-1>" . 'mc/add-cursor-on-click))
  :ensure)

(use-package quake
  :load-path "lisp/quake"
  :bind (("C-`" . quake))
  :defer t)

(use-package swiper
  :bind (("C-s" . swiper)
         ("M-s s" . swiper-all))
  :ensure)

(use-package which-key
  :ensure)

(use-package yasnippet
  :ensure)

;;; init.el ends here
