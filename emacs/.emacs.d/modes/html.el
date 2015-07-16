(defun my-html-mode-hooks ()
  "Set ups for how I like html mode."
  (auto-fill-mode 0)                    ; auto fill mode disabled
  )
(add-hook 'html-mode-hook 'my-html-mode-hooks)
