;; Will need Common Lisp for some functions.
(require 'cl)

(add-to-list 'load-path "~/.emacs.d/lisp/") ;; Custom settings.
(add-to-list 'load-path "~/.emacs.d/modes/") ;; Extra stuff to load for certain modes.

(load-library "melpa")
(load-library "global")
(load-library "style")
(load-library "keys")
(load-library "abbrevs")
(load-library "functions")

(load-library "auto-complete-extras")
(load-library "jedi-extras")
(load-library "python-extras")
(load-library "html-extras")
(load-library "flycheck-extras")
(load-library "helm-extras")
(load-library "dumb-jump-extras")
(load-library "go-extras")

;; So emacs picks up my $PATH env var.
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
