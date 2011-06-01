(setq-default indent-tabs-mode nil)
(setq default-tab-width 1)
(setq tab-stop-list '(4 8 12 16))
(setq mac-command-modifier 'meta)
(setq default-abbrev-mode t)

(global-set-key [f5] 'goto-line)
(global-set-key [f8] 'comment-region)
(global-set-key [f9] 'uncomment-region)
(global-set-key [C-tab] 'other-window)
(global-set-key "\M-p" 'next-buffer)
(global-set-key "\M-n" 'previous-buffer)
(global-set-key "\C-u" 'kill-before)
(global-set-key "\M-^" 'replace-string)

(menu-bar-mode nil)

(defun kill-before ()
  (interactive)
  (kill-line 0)
)

;;(setq default-frame-alist
;; (setq initial-frame-alist
;;             '(
;;               (top . 0) (left . 0)
;;               (width . 80) (height . 43) ; Use 80 x 43 at resolution 1024 x 768
;;               (cursor-color . "Orange")
;;               (cursor-type . box)
;;               (foreground-color . "Gray")
;;               (background-color . "Black")
;;               (vertical-scroll-bars . right)))

;; (setq default-frame-alist
;;             '(
;;               (top . 0) (left . 0)
;;               (width . 80) (height . 43) ; Use 80 x 43 at resolution 1024 x 768
;;               (cursor-color . "Orange")
;;               (cursor-type . box)
;;               (foreground-color . "Gray")
;;               (background-color . "Black")
;;               (vertical-scroll-bars . right)))

(scroll-bar-mode nil) ; No scroll bars

;; Set the default font
;;(set-default-font
;;     "-outline-Consolas-normal-r-normal-normal-13-97-96-96-c-*-iso8859-1")

;; More colors
(set-face-foreground 'font-lock-comment-face "Lime Green")

;; Set a title
(setq frame-title-format '((buffer-file-name "%f"
                                  (dired-directory dired-directory "%b"))))

;; Do not create backup files ending in ~
(setq make-backup-files nil)

(setq load-path (cons "~/.emacs.d" load-path))
(column-number-mode 1)
(line-number-mode 1)
;;(require 'linum)
;;(global-linum-mode)

(defun donuts ()
  (interactive)
  (print "Mmmm, donuts."))

(defun nm ()
  (interactive)
  (insert "if __name__ == '__main__':\n"))

(defun pdb ()
  (interactive)
  (insert "import pdb; pdb.set_trace()"))

(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(defun writeroom ()
  "Switches to a WriteRoom-like fullscreen style"
  (interactive)	
  (when (featurep 'aquamacs)
    ;; switch to white on black
    (color-theme-initialize)
    (color-theme-clarity)
    ;; switch to Garamond 12pt
    (aquamacs-autoface-mode 0)
    (set-frame-font "-apple-garamond-medium-r-normal--12-360-72-72-m-360-iso10646-1")
    ;; switch to fullscreen mode
    (aquamacs-toggle-full-frame)))

(when (load "flymake" t) 
  (defun flymake-pyflakes-init () 
    (let* ((temp-file (flymake-init-create-temp-buffer-copy 
                       'flymake-create-temp-inplace)) 
           (local-file (file-relative-name 
                        temp-file 
                        (file-name-directory buffer-file-name)))) 
      (list "pyflakes" (list local-file)))) 
  
  (add-to-list 'flymake-allowed-file-name-masks 
               '("\\.py\\'" flymake-pyflakes-init))) 

(add-hook 'find-file-hook 'flymake-find-file-hook)

'(flymake-errline ((((class color)) (:background "LightPink" :foreground "black"))))
'(flymake-warnline ((((class color)) (:background "LightBlue2" :foreground "black"))))

(load-library "flymake-cursor.el")
;; (add-to-list 'load-path ".emacs.d/emacs-for-python/")
;; (require 'epy-setup)
;; (require 'epy-python)
