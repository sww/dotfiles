;; For jedi auto complete for Python.

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
