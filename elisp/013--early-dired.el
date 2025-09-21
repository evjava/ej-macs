;;;; ----- dired
(setq dired-recursive-copies (quote always))
(setq dired-dwim-target t) 
; call split-window-vertically, then go to another dired dir. Now, when you press C to copy, the other dir in the split pane will be default destination. Same for R (rename; move).
(require 'dired-x)
(put 'dired-find-alternate-file 'disabled nil)
(require 'ls-lisp)
(setq ls-lisp-use-insert-directory-program nil)
(setq dired-listing-switches "-aD")
(setq dired-omit-files (concat dired-omit-files "\\|\\.i$"))
(setq tramp-default-method "ssh")
(setq dired-guess-shell-alist-user '(("\\.zip\\'" "unzipp")))

(defun ej/dired-dd-loader-hook ()
  (load "dired-x")
  (when window-system
    (require 'dired-dd)
    (require 'dired-dd-mew)
    (require 'dired-dd-insert-fname)
    (require 'dired-dd-insert-file)))

(add-hook 'dired-load-hook 'ej/dired-dd-loader-hook)

(defun ej/remote-switches-hook ()
  (when (file-remote-p default-directory)
    (setq dired-actual-switches "-al")))

(add-hook 'dired-before-readin-hook 'ej/remote-switches-hook)

;; dired: async
(use-package async
  :config
  (dired-async-mode 0))
(defadvice load-theme (after run-after-load-theme-hook activate)
  ;; load-theme for some weird reason changes dired-async-mode
  (setq dired-async-mode nil))
;; (debug-on-variable-change 'dired-async-mode)

;; dired: ext
(defun ej/open-in-external-app (fpath)
  (pcase system-type
    ('gnu/linux  (let ((process-connection-type nil)) (start-process "" nil "xdg-open" fpath)))
    ('windows-nt (w32-shell-execute "open" (s-replace "/" "\\" fpath t t)))
    ('darwin     (let ((process-connection-type nil)) (start-process "" nil "open" fpath)))
    (_ (message "Unrecognized system-type: %s" system-type))))

(defun ej/open-in-external-app-dired ()
  " Open the current file or dired marked files in external app. 
    Works in Microsoft Windows, Mac OS X, Linux. "
  (interactive)
  (let* ((files (cond
                 ((equal major-mode 'dired-mode) (dired-get-marked-files))
                 (t (list (buffer-file-name)))))
         (accept (or
                  (<= (length files) 5)
                  (y-or-n-p "Open more than 5 files?"))))
    (when accept
      (--map (ej/open-in-external-app it) files))))

(defun ej/open-in-external-app-at-point ()
  (interactive)
  (let* ((fpath (thing-at-point 'filename)))
    (if (null fpath) (message "File not found: %s" fpath)
      (message "Going to open: %s" fpath)
      (ej/open-in-external-app fpath))))

(global-set-key (kbd "<f10>") 'ej/open-in-external-app-dired)
(global-set-key (kbd "S-<f10>") 'ej/open-in-external-app-at-point)

;;;; ----- files: hooks
(defun ej/find-file-check-make-large-file-read-only-hook ()
  "If a file is over a given size, make the buffer read only."
  (cl-flet ((pdfp () (s-suffix-p ".pdf" (buffer-file-name))))
    (when (and (> (buffer-size) (* 10 1024 1024)) (not (pdfp)))
      (setq buffer-read-only t)
      (buffer-disable-undo)
      (fundamental-mode)
      )))
(add-hook 'find-file-hook 'ej/find-file-check-make-large-file-read-only-hook)
(defun ej/advice--dired--find-file--no-djvu (proc ff-function file)
  (if (s-ends-with? ".djvu" file)
      (message "Don't open djvu-files from dired")
    (funcall proc ff-function file)))
(advice-add 'dired--find-file :around #'ej/advice--dired--find-file--no-djvu)

(defun ej/open-edownloads (&optional file-idx)
  (interactive "p")
  (find-file downloads-dir)
  (ej/select-nth-file file-idx))

(global-set-key (kbd "C-S-e") 'ej/open-edownloads)
(global-set-key (kbd "C-S-c e") 'ej/select-nth-file)
