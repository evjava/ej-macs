(defun load-ignore-error (file)
  (condition-case nil
      (load file)
    (error (message "Failed to load: %s" file))))

(defun load-org-ignore-error (org-file)
  (condition-case nil (org-babel-load-file org-file) (error nil)))

(defun make-directory-ignore-error (dir)
  (condition-case nil (make-directory dir) (error nil)))

(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun ej/write-string-to-file (string file)
  (interactive "sEnter the string: \nFFile to save to: ")
  (with-temp-file file
    (insert string)))

(defun launch-separate-emacs-in-terminal ()
  (suspend-emacs "fg ; emacs -nw"))
(defun launch-separate-emacs-under-x ()
  (call-process "sh" nil nil nil "-c" "emacs &"))

(defun ej/file-exists-p (file)
  (and (not (null file)) (file-exists-p file)))

(defun ej/restart-emacs ()
  (interactive)
  (when (yes-or-no-p "Restart emacs?")
    (let ((kill-emacs-hook
           (append 
            kill-emacs-hook 
            (list (if (display-graphic-p)
                      #'launch-separate-emacs-under-x
                    #'launch-separate-emacs-in-terminal)))))
      (save-buffers-kill-emacs))))

(defun ej/restart-server()
  (interactive)
  (server-force-delete)
  (server-start))

(defun ej/tab-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer))))

(defun ej/duplicate-line ()
  (interactive)
  (save-excursion
    (let* ((line (thing-at-point 'line)))
      (end-of-line)
      (if (looking-at "\n")
          (forward-line 1)
        (insert "\n"))
      (insert line)))
  (next-line 1))

(defun ej/kill-line (&optional mode)
  "Kill current line saving position from beginning of line."
  (interactive "p")
  (message "mode: %d" mode)
  (ej/kill-line-helper mode nil))

(defun ej/save-kill-line (&optional mode)
  "Save kill current line saving position from beginning of line."
  (interactive "p")
  (ej/kill-line-helper mode t))

(defun ej/save-kill-line2 ()
  "like C-k but save"
  (interactive)
  (save-excursion
    (let* ((cur (point))
           (_ (end-of-line 1)))
       (kill-ring-save cur (point))))
  (message "copied: \"%s\"" (current-kill 0)))

(defun ej/kill-line-helper (mode save-p)
  "Kill current line saving position from beginning of line."
  (interactive)
  (let ((pos (point)))
    (move-beginning-of-line 1)
    (let ((indent (- pos (point))))
      (kill-line mode)
      (if save-p (yank))
      (if (= (point) (point-max))  (previous-line))
      (let ((new_pos (point)))
        (end-of-line)
        (if (> (point) (+ new_pos indent))
            (progn 
              (move-beginning-of-line 1)
              (forward-char indent)))))))

(defun ej/run-last-command ()
  (interactive)
  (repeat nil))

(defun ej/remove-duplicate-lines()
  (interactive)
  (beginning-of-buffer)
  (replace-regexp "\\([^\n]+\n\\)\\1+" "\\1"))

(defun ej/copy-all ()
    "Copy entire buffer to clipboard"
    (interactive)
    (clipboard-kill-ring-save (point-min) (point-max))
    (message "Copy done."))

(defun ej/copy-fname-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun ej/copy-buffer-file-name (&optional mode)
  " improvement: C-u fname should copy file at point in dired
    time: 21.02.19:[19:07..19:16]
    resolution: wontfix. Found that <C-0 w> in dired-mode copy absolute path
    time: 06.03.19:[14:42..14:49]: C-0 fname -> copy short file name
  "
  (interactive "p")
  (let* ((file-name (buffer-file-name))
         (prepared-file-name 
          (if (eq mode 0)
              (car (last (split-string file-name "/")))
            file-name)))
     (kill-new prepared-file-name)))
(defalias 'fname 'ej/copy-buffer-file-name)
(defalias 'fname 'ej/copy-fname-to-clipboard)

