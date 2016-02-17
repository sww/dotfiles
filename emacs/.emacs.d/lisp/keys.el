;; Keyboard shortcut.

(global-set-key [f5] 'goto-line)
(global-set-key [f8] 'comment-region)
(global-set-key [f9] 'uncomment-region)
(global-set-key [C-tab] 'other-window)
(global-set-key "\M-p" 'next-buffer)
(global-set-key "\M-n" 'previous-buffer)
(global-set-key "\M-o" 'mode-line-other-buffer)
(global-set-key "\C-u" 'kill-before)
(global-set-key "\M-^" 'replace-string)
(global-set-key "\C-x\C-b" 'buffer-menu)

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

;; From https://sites.google.com/site/steveyegge2/my-dot-emacs-file.
(defun swap-windows ()
 "If you have 2 windows, it swaps them." (interactive) (cond ((not (= (count-windows) 2)) (message "You need exactly 2 windows to do this."))
 (t
 (let* ((w1 (first (window-list)))
	 (w2 (second (window-list)))
	 (b1 (window-buffer w1))
	 (b2 (window-buffer w2))
	 (s1 (window-start w1))
	 (s2 (window-start w2)))
 (set-window-buffer w1 b2)
 (set-window-buffer w2 b1)
 (set-window-start w1 s2)
 (set-window-start w2 s1)))))

;; http://emacsredux.com/blog/2013/03/30/go-back-to-previous-window/
;; Go backwards with C-x O when there are more than two windows split.
(global-set-key (kbd "C-x O") (lambda ()
                                (interactive)
                                (other-window -1)))
