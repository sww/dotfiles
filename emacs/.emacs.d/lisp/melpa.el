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

;; Set the packages to be installed on load.
(setq package-list
      '(auto-complete exec-path-from-shell flycheck color-theme-solarized jinja2-mode powerline jedi))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; Install the packages.
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))
