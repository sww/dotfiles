;;; package -- Summary
;;; Commentary:
;;; Code:

(when (< emacs-major-version 27)
  (package-initialize))

;; Sets the dotfiles emacs dir.
(defvar emacsdir
  (file-name-directory
   (replace-regexp-in-string
    (regexp-quote "\n") ""
    (shell-command-to-string
     "ls -al ~/.emacs.d/init.el | awk '{print $NF}'"))))

(load-file (concat (file-name-as-directory emacsdir) "util.el"))

;; Load any local configs in the `emacsdir/local.el` file.
(let ((local-config-file (concat (file-name-as-directory emacsdir) "local.el")))
  (when (file-exists-p local-config-file)
    (load-file local-config-file)))

;; Setup MELPA.
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "https://melpa.org/packages/")
   t)
  ;; Fixes failed package downloads (from https://emacs.stackexchange.com/a/56067).
  (custom-set-variables '(gnutls-algorithm-priority "normal:-vers-tls1.3")))

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;; Install use-package if not already installed.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Global-ish configs -- anything that affects other packages or global emacs settings.

;; Have the custom variables live in <user-emacs-directory>/custom.el.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(delete-selection-mode) ;; For being able to C-d selected lines.
(setq delete-active-region 0)

(setq make-backup-files nil) ;; Don't leave ~ backup files everywhere.

(setq ring-bell-function 'ignore) ;; No visual nor audio bell.

(setq initial-scratch-message "") ;; Empty scratch buffer.
(setq inhibit-startup-message t) ;; No welcome buffer.

(setq echo-keystrokes 0.01) ;; Interval for echoing the keystroke in the minibuffer.

;; Settings for the look and feel of emacs.

(blink-cursor-mode nil)
(column-number-mode t)
(delete-selection-mode t)
(menu-bar-mode nil)
(scroll-bar-mode nil)
(tool-bar-mode nil)

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

;; Allow invoking of other commands if in the minibuffer.
(setq enable-recursive-minibuffers t)

;; Show the buffer's full file path in the title.
(setq frame-title-format '((buffer-file-name "%f" (dired-directory dired-directory "%b"))))

(global-set-key [f8] 'comment-region)
(global-set-key [f9] 'uncomment-region)
(global-set-key (kbd "\C-c c") 'comment-or-uncomment-region)
(global-set-key [f10] nil) ;; Prevent the default action -- mainly for macOS.
(global-set-key [C-tab] 'other-window)
(global-set-key "\M-p" 'next-buffer)
(global-set-key "\M-n" 'previous-buffer)
(global-set-key "\M-o" 'mode-line-other-buffer)
(global-set-key "\C-u" 'kill-before)
(global-set-key "\M-^" 'replace-string)
(global-set-key (kbd "\C-x B") 'ibuffer)
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
  ;; Prevents auto indenting.
  (auto-fill-mode 0))
(add-hook 'html-mode-hook 'my-html-mode-hooks)

;; Python.
(add-hook 'python-mode-hook 'which-function-mode)
;; Set the indent level to only be one on newlines.
(set-variable 'python-indent-def-block-scale 1)

;; Package stuff.

(use-package ace-window
  :bind (("M-g b" . ace-window))
  :commands ace-window
  :defer t)

(use-package all-the-icons-ibuffer
  :config (all-the-icons-ibuffer-mode 1)
  :ensure)

(use-package all-the-icons-ivy-rich
  :config (all-the-icons-ivy-rich-mode 1)
  :ensure)

(use-package avy
  :commands (avy-goto-char-timer avy-goto-line avy-goto-word-0)
  :config
  (setq avy-style 'words)
  :bind (("M-g c" . avy-goto-char-timer)
         ("M-g l" . avy-goto-line)
         ("M-g w" . avy-goto-word-0))
  :defer t)

(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-show-numbers t)
  (setq company-idle-delay 1)  ;; In seconds.
  (custom-set-faces
   '(company-tooltip-selection ((((class color)) (:foreground "ivory1" :background "SandyBrown"))))
   '(company-tooltip-common-selection ((((class color)) (:foreground "ivory1" :background "SandyBrown")))))
  :bind (("C-." . company-complete)
         :map company-active-map
         ("<tab>" . company-complete-common-or-cycle)
         ("<S-tab>" . company-select-previous)
         ("TAB" . company-complete-common-or-cycle)
         ("S-TAB" . company-select-previous))
  :defer t)

(use-package company-box
  :after (company)
  :commands
  (company-box-mode)
  :hook (company-mode . company-box-mode)
  :defer t)

(use-package company-prescient
  :after (company)
  :config
  (company-prescient-mode)
  :hook (company-mode)
  :defer t)

(use-package company-quickhelp
  :after (company)
  :config
  (eval-after-load 'company
    '(define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin))
  :hook (company-mode)
  :defer t)

(use-package company-statistics
  :after (company)
  :commands
  (company-statistics-mode)
  :config
  (add-hook 'after-init-hook 'company-statistics-mode)
  :defer t)

(use-package counsel
  :commands (counsel-git counsel-M-x counsel-grepper)
  :config
  (fset 'counsel-grepper (cond ((executable-find "rg") 'counsel-rg)
                               ((executable-find "ag") 'counsel-ag)
                               ('counsel-grep)))
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
  :bind (("C-x C-d" . counsel-git)
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("M-s f" . counsel-grepper))
  :defer t)

(use-package dap-mode
  :after lsp-mode
  :commands (dap-mode dap-ui-mode dap-tooltip-mode)
  :config
  (dap-mode t)
  (dap-ui-mode t)
  (dap-tooltip-mode t)
  (require 'dap-dlv-go)
  (require 'dap-python)
  ;; Show the hydra debug commands when stopped on a breakpoint.
  :hook (dap-stopped . (lambda (arg) (call-interactively #'dap-hydra)))
  :defer t)

(use-package dockerfile-mode
  :defer t)

(use-package doom-modeline
  :config
  (doom-modeline-mode 1)
  (setq doom-modeline-env-enable-python t)
  (setq doom-modeline-env-enable-go t)
  (setq doom-modeline-height 20)
  (setq doom-modeline-buffer-file-name-style 'buffer-name)
  :ensure)

(use-package doom-themes
  ;; doom-one is the preferred dark theme.
  :ensure)

(use-package dumb-jump
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  :commands (dumb-jump-go-other-window dumb-jump-quick-look xref-pop-marker-stack xref-find-definitions)
  :config
  (setq dumb-jump-selector 'ivy)
  :bind (("C-M-o" . dumb-jump-go-other-window)
         ("C-M-p" . xref-pop-marker-stack)
         ("C-M-," . dumb-jump-quick-look)
         ("C-M-g" . xref-find-definitions))
  :defer t)

(use-package eat
  :config
  (setq eat-term-name "xterm-256color")
  :defer)

(use-package exec-path-from-shell
  :config
  ;; Set the path from $PATH.
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "GOPATH"))
  :ensure)

(use-package flycheck
  :init
  (add-hook 'go-mode-hook 'flycheck-mode)
  (add-hook 'python-mode-hook 'flycheck-mode)
  :commands (flycheck-mode)
  :config
  (custom-set-faces
   '(flycheck-warning ((t (:underline (:color "cyan" :style line)))))
   '(flycheck-error ((t (:underline (:color "coral3" :style wave))))))
  (setq flycheck-highlighting-mode 'lines)
  (setq flycheck-display-errors-delay 0.15)
  :defer t)

(use-package flyspell
  :config
  ;; Enable spell checking for comments and strings.
  (add-hook 'prog-mode-hook #'flyspell-prog-mode)
  (custom-set-faces
   '(flyspell-duplicate ((t (:weight bold :underline (:color "purple" :style line)))))
   '(flyspell-incorrect ((t (:slant italic :underline (:color "LightBlue3" :style line)))))))

(use-package free-keys
  :commands (free-keys free-keys-mode)
  :defer t)

(use-package go-mode
  :config
  (add-hook 'go-mode-hook 'which-function-mode)
  ;; Set up before-save hooks to format buffer and add/delete imports.
  ;; Make sure you don't have other gofmt/goimports hooks enabled.
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
  (setq gofmt-command "goimports")
  (setq gofmt-args nil)
  :defer t)

(use-package ivy
  :bind (("C-x C-b" . ivy-switch-buffer)
         ("C-c v" . ivy-push-view)
         ("C-c V" . ivy-pop-view)
         ("C-c r" . ivy-resume))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  :ensure)

(use-package ivy-posframe
  :after (ivy)
  :init
  (ivy-posframe-mode 1)
  :config
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display)))
  (setq ivy-posframe-parameters
        '((left-fringe . 8)
          (right-fringe . 8)))

  (set-face-attribute 'ivy-posframe nil :foreground "black" :background "papaya whip")
  (set-face-attribute 'ivy-posframe-cursor nil :foreground "orange" :background "orange")

  ;; Make the width of the posframe fixed.
  ;; From https://github.com/tumashu/ivy-posframe/issues/105#issuecomment-750370286.
  (defun my-ivy-posframe-get-size ()
    "Set the ivy-posframe size according to the current frame."
    (let ((height (or ivy-posframe-height (or ivy-height 10)))
          (width (min (or ivy-posframe-width 200) (round (* 0.95 (frame-width))))))
      (list :height height :width width :min-height height :min-width width)))

  (setq ivy-posframe-size-function 'my-ivy-posframe-get-size)
  :ensure)

(use-package ivy-prescient
  :after (ivy)
  :config
  (ivy-prescient-mode)
  :hook (ivy-mode)
  :defer t)

(use-package ivy-rich
  :after (:and ivy counsel)
  :init
  (setq ivy-rich-parse-remote-buffer nil)
  :config
  (ivy-rich-mode 1)
  (setq ivy-rich-path-style 'abbrev)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  ;; Taken from https://justin.abrah.ms/dotfiles/emacs.html, with some minor modifications.
  (setq ivy-rich-display-transformers-list
        (plist-put ivy-rich-display-transformers-list
                   'ivy-switch-buffer
                   '(:columns
                     ((all-the-icons-ivy-rich-buffer-icon)
                      (ivy-rich-candidate (:width 20))
                      (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right)); return the buffer indicators
                      (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))          ; return the major mode info
                      (ivy-rich-switch-buffer-project (:width 15 :face success))             ; return project name using `projectile'
                      (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))  ; return file path relative to project root or `default-directory' if project is nil
                     :predicate
                     (lambda (cand)
                       (if-let ((buffer (get-buffer cand)))
                           ;; Don't mess with EXWM buffers
                           (with-current-buffer buffer
                             (not (derived-mode-p 'exwm-mode))))))))

  ;; Just show the icon, name, and file size for the counsel-git results.
  (ivy-rich-set-columns
   'counsel-git
   '((all-the-icons-ivy-rich-file-icon)
     (all-the-icons-ivy-rich-file-name (:width 0.85))
     (all-the-icons-ivy-rich-file-size
      (:width 0.1 :face all-the-icons-ivy-rich-size-face :align right))))
  :hook (ivy-mode)
  :defer t)

(use-package lsp-ivy
  :after (:and ivy lsp-mode)
  :hook (ivy-mode)
  :defer t)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (go-mode . lsp-deferred)
  :config
  ;; Set the styles for flycheck's unnecessary and deprecated warnings.
  (setq lsp-diagnostics-attributes
        '((unnecessary :underline (:color "DeepSkyBlue" :style line))
          (deprecated :strike-through t)))
  :ensure)

(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred)))
  :defer t)

(use-package lsp-ui
  :commands lsp-ui-mode
  :defer t)

(use-package magit
  :bind (("C-x g" . magit-status))
  :commands (magit-status)
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
  (add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)
  :defer t)

(use-package multiple-cursors
  :bind
  (("C-S-c C-S-c" . 'mc/edit-lines)
   ("C->" . 'mc/mark-next-like-this)
   ("C-<" . 'mc/mark-previous-like-this)
   ("C-c C-<" . 'mc/mark-all-like-this)
   ("C-S-<mouse-1>" . 'mc/add-cursor-on-click))
  :commands (mc/edit-lines mc/mark-next-like-this mc/mark-previous-like-this mc/mark-all-like-this mc/add-cursor-on-click)
  :defer t)

(use-package org
  :config
  ;; Don't truncate long lines -- enable word wrapping.
  (setq org-startup-truncated nil)
  :defer t)

(use-package quake
  :load-path "lisp/quake"
  :bind (("C-`" . quake))
  :commands (quake)
  :defer t)

(use-package solarized-theme
  :init
  (load-theme 'solarized-light t)
  ;; Change this last since the theme will change the cursor color.
  (set-cursor-color "orange")
  :ensure)

(use-package swiper
  :bind (("C-s" . swiper)
         ("M-s s" . swiper-all))
  :commands (swiper swiper-all)
  :defer t)

(use-package windmove
  :config
  (windmove-default-keybindings)
  :defer t)

(use-package which-key
  :defer t)

(use-package yaml-mode
  :defer t)

(use-package yasnippet
  :config
  (yas-global-mode 1)
  :defer t)

;;; init.el ends here
