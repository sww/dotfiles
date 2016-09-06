;; Additional settings for go-mode.

(setq gofmt-command "goimports")
(add-hook 'before-save-hook #'gofmt-before-save)
(setq gofmt-args nil)

(add-hook 'go-mode-hook 'flycheck-mode)
(add-hook 'go-mode-hook 'which-function-mode)
