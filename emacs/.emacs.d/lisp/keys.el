;; Keyboard shortcut.

(global-set-key [f8] 'comment-region)
(global-set-key [f9] 'uncomment-region)
(global-set-key [f10] nil) ;; Prevent the default action -- mainly for OSX.
(global-set-key [C-tab] 'other-window)
(global-set-key "\M-p" 'next-buffer)
(global-set-key "\M-n" 'previous-buffer)
(global-set-key "\M-o" 'mode-line-other-buffer)
(global-set-key "\C-u" 'kill-before)
(global-set-key "\M-^" 'replace-string)
(global-set-key "\C-x\C-b" 'helm-mini)
(global-set-key (kbd "\C-x B") 'buffer-menu)
(global-set-key (kbd "\C-x g") 'magit-status)
(global-set-key (kbd "\C-x \C-d") 'helm-browse-project)
(global-set-key (kbd "\C-x K") 'close-and-kill-this-pane)
(global-set-key (kbd "\C-c u") 'universal-argument)

(setq delete-active-region 0)

(defun kill-before ()
  (interactive)
  (kill-line 0)
)

(defun nm ()
  (interactive)
  (insert "if __name__ == '__main__':\n    "))

(defun pypdb ()
  (interactive)
  (insert "import pdb; pdb.set_trace()"))

(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

;; http://emacsredux.com/blog/2013/03/30/go-back-to-previous-window/
;; Go backwards with C-x O when there are more than two windows split.
(global-set-key (kbd "C-x O") (lambda ()
                                (interactive)
                                (other-window -1)))


;; https://www.emacswiki.org/emacs/SwitchingBuffers#toc5.
(defun switch-to-previous-buffer ()
      (interactive)
      (switch-to-buffer (other-buffer (current-buffer) 1)))

(global-set-key (kbd "C-x j") 'switch-to-previous-buffer)
