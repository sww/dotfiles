(require 'helm)
(require 'helm-ls-git)

;; Turn on autoresize-mode to be able to set the max height.
(helm-autoresize-mode 1)

;; Set the max helm height to 50% of the buffer.
(setq helm-autoresize-max-height 50)
