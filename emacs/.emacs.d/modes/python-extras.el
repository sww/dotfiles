;; Additional settings for Python mode.

(require 'flymake-python-pyflakes)
(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)
(setq flymake-python-pyflakes-executable "flake8")
(add-to-list 'exec-path "/Library/Frameworks/Python.framework/Versions/3.4/bin/")
(setq flymake-python-pyflakes-extra-arguments '("--ignore=E501")) ;; E501 = line too long.
(setq flymake-cursor-error-display-delay 0)

(add-hook 'python-mode-hook 'which-function-mode)

;; Jedi auto completion.
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

(add-hook 'python-mode-hook 'paragraph-keys)
(defun paragraph-keys ()
  (define-key python-mode-map (kbd "\M-f") 'forward-paragraph)
  (define-key python-mode-map (kbd "\M-b") 'backward-paragraph))