;;;; dyno ~ dynamic-org

(defvar *dyno-path* nil
  "Path to dynamic org-files"
  )

(setq *cur-dyno-file-pref* "dyno-")
(setq *cur-dyno-file-suf* ".org")
(setq *cur-dyno-header-suf* "----------")
(setq *dyno-last-searches-alist* nil)
(setq *dyno-search-state-empty* (list :search "" :search-tags '()))
(setq *dyno-log-enabled* t)

(defun dyno-replace-sexp-at-point (new-sexp)
  (dyno-message "new-sexp: %s" new-sexp)
  (when (dyno-string-at-point)
    (backward-kill-sexp))
  (insert new-sexp))

(defun dyno-message (format-string &rest args)
  (when *dyno-log-enabled*
    (apply #'message (cons format-string args))))

(defun dyno-count ()
	(condition-case err
			(let* ((files (directory-files *dyno-path*))
						 (org-files (--filter (s-ends-with? ".org" it) files))
             (res (length org-files))
						 ) res)
		(error 1)
		))

(defun dyno-generate-file-name ()
	(let* ((file-num (format "%05d" (dyno-count)))
				 (short-fname (concat "dyno-" file-num ".org"))
				 (long-fname (concat *dyno-path* "/" short-fname)))
		long-fname))

(defun dyno-connect-session ()
  (interactive)
  (unless (is-dyno-file)
    (error (format "Not dyno file: %s" (buffer-file-name))))
  (let* ((ss (or (dyno-search-state-read-buf) *dyno-search-state-empty*)))
    (remove-hook 'after-change-functions 'dyno-maybe-reload)
    (erase-buffer)
    (insert (dyno-search-state-serialize ss))
    (goto-line 1)
    (end-of-line)
    (add-hook 'after-change-functions 'dyno-maybe-reload nil t)
    (dyno-maybe-reload)
    (dyno-message "Connected to %s" (buffer-file-name))))

(defun dyno-create-session ()
  (interactive)
  (let* ((long-fname (dyno-generate-file-name)))
    (find-file long-fname)
    (when (is-dyno-file long-fname)
      (dyno-connect-session)
      (dyno-maybe-reload))))
    
(defun is-dyno-file ()
  (let* ((b-fpath (buffer-file-name)))
    (if (null b-fpath) nil
      (let* ((b-fname (file-name-nondirectory (buffer-file-name)))
             (is-ok (and
                     (s-starts-with? *cur-dyno-file-pref* b-fname)
                     (s-ends-with? *cur-dyno-file-suf* b-fname)))
             ) is-ok))))

(defun dyno-check-pref-and-drop (line pref)
  (unless (s-starts-with? pref line)
    (error "Malformed line in search-state-buf: expected pref: \"%s\", found: \"%s\"" pref line))
  (substring line (length pref)))

(defun dyno-search-state-parse (search-state-str)
  (let* ((lines (s-split "\n" search-state-str))
         (cd-search      (dyno-check-pref-and-drop (elt lines 0) "# search: "))
         (cd-search-tags-str (dyno-check-pref-and-drop (elt lines 1) "# search-tags: "))
         (cd-search-tags (-map #'s-trim (s-split "," cd-search-tags-str)))
         (_ (unless (equal (elt lines 2) *cur-dyno-header-suf*) (error "malformed search-state-buf-suf: %s" (elt lines 2))))
         (res (list :search cd-search :search-tags cd-search-tags))) res))

(defun dyno-search-state-read-buf ()
  (condition-case nil
      (save-excursion
        (beginning-of-buffer)
        (search-forward *cur-dyno-header-suf*)
        (let* ((search-state-buf (buffer-substring-no-properties 1 (point)))
               (_ (format "buf::::::" search-state-buf))
               (search-state (dyno-search-state-parse search-state-buf))
               ) search-state))
    (error nil)))

(defun dyno-search-state-serialize (search-state)
  (format "# search: %s\n# search-tags: %s\n%s"
          (plist-get search-state :search)
          (s-join ", " (plist-get search-state :search-tags))
          *cur-dyno-header-suf*))

(defun dyno-reload-inner (search-state pos)
  (save-excursion
    (goto-char (point-min))
    (search-forward *cur-dyno-header-suf*)
    (when (< pos (point))
      (delete-region (point) (point-max))
      (dyno-message "ss: %s" search-state)
      (when (not (equal *dyno-search-state-empty* search-state))
        (let* ((backend-res (funcall dyno-search-items-backend search-state)))
          (message "length(backend-res): %d" (length backend-res))
          (insert "\n")
          (insert backend-res)
          (save-buffer))))))

(defun dyno-maybe-reload (&optional beg end smth)
  (interactive)
  (dyno-message "(#dyno-maybe-reload %s %s %s)" beg end smth)
  (dyno-message "inhibit-modification-hooks: %s" inhibit-modification-hooks)
  (let* ((old-search-state (plist-get *dyno-last-searches-alist* (buffer-file-name)))
         (cur-search-state (or (dyno-search-state-read-buf) *dyno-search-state-empty*)))
    (dyno-message "old-search: %s" old-search-state)
    (dyno-message "cur-search: %s" cur-search-state)
    (when (or (null old-search-state) (not (equal old-search-state cur-search-state)))
      (plist-put *dyno-last-searches-alist* (buffer-file-name) cur-search-state)
      (dyno-reload-inner cur-search-state (or end (point))))))
         
(defun dyno-string-at-point ()
  (when (looking-at "[\s\n]")
    (sexp-at-point)))

;; todo fix: how to make this function more universal? Customizable or something... 
(defun dyno-parse-identity-at-point ()
  "parse identity like [cur:14.11.22-14:40:47$$8859fbc...]"
  (let* ((sexp (sexp-at-point))
         (id-full (when (equal (type-of sexp) 'vector) (symbol-name (elt sexp 0))))
         (_ (unless (s-starts-with? "cur:" id-full) (error "wrong id: %s" id-full)))
         (id (substring id-full 4))
         ) id))

(defun dyno-file-and-position (id)
  (let* ((cmd (format "grep -rn 'identity: %s' %s" (s-replace "$" "\\$" id) org-directory))
         (cmd-out (shell-command-to-string cmd)))
    (when cmd-out
      (let* ((parts (s-split ":" cmd-out))
             (fname (car parts))
             (line (string-to-number (cadr parts))))
        (cons fname line)))))

(defun dyno-context-action ()
  (interactive)
  (let* ((ln (thing-at-point 'line)))
    (cond
     ((or (s-starts-with? "tags:" ln)
          (s-starts-with? "# search-tags: " ln))
      (let* ((tps (dyno-string-at-point))
             (tag-pref (when tps (format "%s" tps)))
             (tags (funcall dyno-suggest-tags-backend tag-pref)))
        (if (stringp tags)
            (message tags)
          (helm :sources (helm-build-sync-source "tags"
                           :candidates tags
                           :action #'dyno-replace-sexp-at-point)))))
     ((dyno-parse-identity-at-point)
      (let* ((id (dyno-parse-identity-at-point))
             (fname-and-line (dyno-file-and-position id)))
        (when fname-and-line
          (let* ((fname (car fname-and-line))
                 (line (cdr fname-and-line)))
            (g fname :line line)))))
     (t (message "No context actions available")))
    ))

(defun dyno-is-dynamic-buffer (buffer)
  (let* ((b-name (buffer-file-name buffer))
         (res (and (stringp b-name) (s-contains? "/dyno-" b-name)))
         ) res))

(defun dyno-reload-dynamic-buffers ()
  (let* ((buffers (buffer-list))
         (f-buffers (-filter #'dyno-is-dynamic-buffer buffers))
         )
    (message "Going to reload buffers: %s" f-buffers)
    (cl-loop
     for buf in f-buffers
     do (with-current-buffer buf
          (ej/reopen)))))

(defun dyno-reload ()
  (interactive)
  (funcall dyno-reload-backend)
  (dyno-reload-dynamic-buffers))

;; dummy

(defun dyno-search-items--dummy (search-state)
  (let* ((res-fmt "Backend not installed!\nSearch-state: %S")
         (res (format res-fmt search-state))
         ) res))

(defun dyno-suggest-tags-backend--dummy (&optional tag-search)
  (let* ((res-fmt "Backend not installed! Tag-search: %S")
         (res (format res-fmt tag-search))
         ) res))

(defun dyno-reload-backend--dummy ()
  (let* ((res "Backend not installed!")
         ) res))


(defun dyno-search-items-example (search-state)
  (let* ((org-items (cl-loop
                     with search = (plist-get search-state :search)
                     for i from 1 to 10
                     for org-title = (format "%s: %s" search i)
                     for org-text = (format "text... %s" i)
                     for org-item = (format "** %s\ndate: 15.11.22\nbla-bla-bla%s" org-title org-text)
                     collect org-item))
         (res (s-join "\n" org-items))
         ) res))

;; dummy functions, should be redefined
(setq dyno-search-items-backend #'dyno-search-items--dummy)
(setq dyno-reload-backend #'dyno-reload-backend--dummy)
(setq dyno-suggest-tags-backend #'dyno-suggest-tags-backend--dummy)

(provide 'dyno)
