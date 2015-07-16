;; Will need Common Lisp for some functions.
(require 'cl)

(add-to-list 'load-path "~/.emacs.d/lisp/") ;; Custom settings.
(add-to-list 'load-path "~/.emacs.d/modes/") ;; Extra stuff to load for certain modes.
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-color-theme-solarized")

;; Load packages installed from MELPA.
(load-library "melpa")

(load-library "style")
(load-library "keys")
(load-library "abbrevs")

(load-library "jedi")

(load-library "python-extras")

(load-library "html")

(cua-mode) ;; For being able to C-d selected lines.
(ido-mode) ;; For listing open buffers when C-x b'ing.

(eval-after-load 'flymake '(require 'flymake-cursor))
