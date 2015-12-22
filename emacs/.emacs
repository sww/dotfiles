;; Will need Common Lisp for some functions.
(require 'cl)

(add-to-list 'load-path "~/.emacs.d/lisp/") ;; Custom settings.
(add-to-list 'load-path "~/.emacs.d/modes/") ;; Extra stuff to load for certain modes.

;; Load packages installed from MELPA.
(load-library "melpa")

(load-library "style")
(load-library "keys")
(load-library "abbrevs")

(load-library "auto-complete-extras")
(load-library "jedi-extras")
(load-library "python-extras")

(load-library "html")

(cua-mode) ;; For being able to C-d selected lines.

(eval-after-load 'flymake '(require 'flymake-cursor))

(setq make-backup-files nil) ;; Don't leave ~ backup files everywhere.
