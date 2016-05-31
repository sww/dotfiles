;; Global-ish configs -- anything that affects other packages or global emacs settings.

(delete-selection-mode) ;; For being able to C-d selected lines.

(setq make-backup-files nil) ;; Don't leave ~ backup files everywhere.

(setq ring-bell-function 'ignore) ;; No visual nor audio bell.

(icomplete-mode 1) ;; Suggestions.

(dumb-jump-mode)
