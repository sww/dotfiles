;; For jedi auto complete for Python.

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
(setq jedi:get-in-function-call-delay 50) ;; Delay in showing the tool tip.
