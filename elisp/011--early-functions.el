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
  (if (yes-or-no-p "Restart emacs?")
      (let ((kill-emacs-hook (append 
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
