;;; shell: pager for stdout
(use-package shell
  :config
  (setenv "PAGER" "cat")
  (setq system-uses-terminfo nil)
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
  (setq ansi-color-names-vector
        ["black" "tomato" "PaleGreen2" "gold1"
         "DeepSkyBlue1" "MediumOrchid1" "cyan" "white"]))

;;; shell: shortcuts
(setq ej/shell-cnt 6)
(defun ej/shell-1-or-else (&optional new-shell) 
  (interactive "P")
  (if (null new-shell) (shell "*shell*<1>")
    (let ((new-shell-buffer-name (format "*shell*<%d>" ej/shell-cnt)))
      (setq ej/shell-cnt (1+ ej/shell-cnt))
      (shell new-shell-buffer-name))))
(defun extract-int (str)
  (when (string-match "\\([0-9]+\\)" str)
    (string-to-number (match-string 1 str))))
(defun ej/get-shell-buffers ()
  (let* ((buffers (-map #'buffer-name (buffer-list)))
         (shell-buffers (--filter (string-match "\\*shell\\*<[0-9]+>$" it) buffers))
         ) shell-buffers))
(defun ej/switch-to-prev-shell ()
  (interactive)
  (-if-let (shell-buffers (ej/get-shell-buffers))
      (let* ((sorted-shell-buffers (-sort (-on #'< #'extract-int) shell-buffers))
             (len (length sorted-shell-buffers))
             (cur-buf (buffer-name))
             (idx-0 (-elem-index cur-buf sorted-shell-buffers))
             (idx (or idx-0 0))
             (next-idx (mod (+ len (1- idx)) len))
             (next-buf (elt sorted-shell-buffers next-idx)))
        (switch-to-buffer next-buf))
    (message "No *shell*<N> buffers found!")))

(global-set-key (kbd "s-n") 'ej/shell-1-or-else)
(global-set-key (kbd "s-m") (lambda () (interactive) (shell "*shell*<2>")))
(global-set-key (kbd "s-,") (lambda () (interactive) (shell "*shell*<3>")))
(global-set-key (kbd "s-.") (lambda () (interactive) (shell "*shell*<4>")))
(global-set-key (kbd "s-/") (lambda () (interactive) (shell "*shell*<5>")))
(global-set-key (kbd "M-s-n") 'ej/switch-to-prev-shell)
(defun ej/bash-history ()
  (interactive)
  (helm
   :sources (helm-build-sync-source "Bash history"
              :candidates (seq-reverse (vc--read-lines "~/.bash_history"))
              :action #'insert
              :fuzzy-match t)
   :buffer "*Bash history*"))
(defun ej/rename-shell (new-shell-name)
  "Rename the current shell buffer to *shell*<NEW-SHELL-NAME>.
If a buffer with that name already exists, ask for confirmation before renaming."
  (interactive "sEnter new shell name: ")
  (let* ((new-name (format "*shell*<%s>" new-shell-name))
         (buffer (get-buffer new-name)))
    (if buffer
        (if (not (y-or-n-p (format "Buffer '%s' already exists. Kill it? " new-name)))
            (message "Cancelled!")
          (kill-buffer buffer)
          (rename-buffer new-name))
      (rename-buffer new-name))))
(define-key shell-mode-map (kbd "C-c s-r") #'ej/rename-shell)
(define-key shell-mode-map (kbd "M-s-r") #'ej/bash-history)
(define-key shell-mode-map (kbd "s-t") #'toggle-truncate-lines)

;;; shell: completion
(define-key shell-mode-map (kbd "<tab>") #'comint-dynamic-complete-filename)

;;;; ----- old emacs behavior
(add-to-list 'display-buffer-alist '("^\\*shell\\*.*$" . (display-buffer-same-window)))

