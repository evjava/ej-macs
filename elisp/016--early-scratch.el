
(defun ej/get-scratch-buffers ()
  "Return a list of scratch buffers in /tmp."
  (let ((buffers (directory-files "/tmp" t "scratch-\\([0-9]+\\)")))
    (mapcar #'file-name-nondirectory buffers)))

(defun ej/switch-scratch-cycle (&optional inc)
  "Cycle between scratch buffers. INC can be -1 (prev) or 1 (next). Defaults to 1."
  (interactive "p")
  (let* ((inc (or inc 1)))
    (if (eq inc 4) (ej/create-new-scratch)
      (let* ((scratch-buffers (ej/get-scratch-buffers)))
        (if (not scratch-buffers) (find-file "/tmp/scratch-1"))
        (let* ((sorted-scratch-buffers (sort scratch-buffers #'string<))
               (len (length sorted-scratch-buffers))
               (cur-buf (buffer-name))
               (idx (cl-position cur-buf sorted-scratch-buffers :test #'string=))
               (next-idx (if idx (mod (+ idx inc) len) 0))
               (next-buf (nth next-idx sorted-scratch-buffers)))
          (message "Switching to buffer: %s" next-buf)
          (switch-to-buffer next-buf))))))

(defun ej/create-new-scratch ()
  "Create a new scratch buffer."
  (interactive)
  (let* ((scratch-buffers (ej/get-scratch-buffers))
         (next-number (if scratch-buffers
                          (1+ (or (extract-int (car (last scratch-buffers))) 0)) 1))
         (new-scratch-file (format "/tmp/scratch-%d" next-number)))
    (with-temp-buffer
      (write-file new-scratch-file))
    (find-file new-scratch-file)))

(global-set-key (kbd "M-s-s") (lambda () (interactive) (switch-to-buffer "*scratch*")))
(global-set-key (kbd "s-`") 'ej/switch-scratch-cycle)
(global-set-key (kbd "M-s-`") (lambda () (interactive) (ej/switch-scratch-cycle -1)))
