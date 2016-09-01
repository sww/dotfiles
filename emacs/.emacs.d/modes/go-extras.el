;; Additional settings for go-mode.

(add-hook 'before-save-hook #'gofmt-before-save)
(setq gofmt-args (list "-s"))

(add-hook 'go-mode-hook 'flycheck-mode)
