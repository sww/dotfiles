;; Additional settings for Python mode.

(add-hook 'python-mode-hook 'flycheck-mode)
(add-hook 'python-mode-hook 'which-function-mode)
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook 'paragraph-keys)
(defun paragraph-keys ()
  (define-key python-mode-map (kbd "\M-f") 'forward-paragraph)
  (define-key python-mode-map (kbd "\M-b") 'backward-paragraph))
