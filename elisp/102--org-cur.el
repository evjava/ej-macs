;; `curs-root` should be defined

(defun get-file-modification-time (file-path)
  "Get the modification time of a file."
  (let ((attributes (file-attributes file-path)))
    (nth 5 attributes)))

(defun ej/suggest-cur ()
  (interactive)
  (let* ((curs-files (directory-files-recursively curs-root "."))
         (candidates-0 (--filter (s-suffix? ".org" it) curs-files))
         (candidates (--sort (not (time-less-p (get-file-modification-time it) (get-file-modification-time other))) candidates-0))
         )
    (helm :sources (helm-build-sync-source "curs"
                     :candidates candidates
                     :action '(lambda (f) (interactive) (find-file f))
                     :fuzzy-match t)
          :buffer "*Select cur*")))
(global-set-key (kbd "C-x C-s-n") 'ej/suggest-cur)

(defun ej/new-note (mode)
  "insert note header, time: 29.11.18-14:46:07"
  (interactive "p")
  ;; todo assert org
  (beginning-of-buffer)
  (while (looking-at "[\n#]")
    (end-of-line)
    (forward-char))
  (insert "\n")
  (previous-line)
  (insert "** log\n")
  (ej/insert-time 4)
  (insert (cond ((eq mode 1) "\ntags: ")
                ((eq mode 4) "\ntitle: ")
                (t "\n"))))

(defun ej/cur-insert-identity ()
  (interactive)
  (if (not (s-starts-with? "**" (thing-at-point 'line)))
      (message "Move point on org-header")
    (let* ((_ (search-forward "\ndate: "))
           (date-start (point))
           (_ (end-of-line))
           (date-end (point))
           (date (buffer-substring-no-properties date-start date-end))
           (_ (newline))
           (uuid (org-id-uuid))
           (identity (s-concat date "$$" uuid)))
      (insert (format "identity: %s" identity))
      (kill-new identity)
      (message "Copied to clipboard: %s" identity))))

(use-package dyno
  :load-path "non-elpa"
  :config
  (setq *dyno-path* dyno-path)
  (global-set-key (kbd "s-x") #'dyno-context-action)
  (use-package dyno--org-ql
    :load-path "non-elpa"
    )
  (setq dyno-search-notes-backend #'dyno-search-notes-backend--org-ql)
  (setq dyno-suggest-tags-backend #'dyno-suggest-tags--org-ql)
  (setq dyno-reload-backend #'dyno-reload--org-ql)
  )

(define-key org-mode-map (kbd "C-s-n") 'ej/new-note)

(defun ej/org-mode-hook--dyno ()
  (when (is-dyno-file)
    (dyno-connect-session)))
(add-hook 'org-mode-hook 'ej/org-mode-hook--dyno)
