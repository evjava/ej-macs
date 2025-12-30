;;;; dyno ~ dynamic-org

(require 'timeout)

;;; dyno-vars

(defvar *dyno-path* nil "Path to dynamic org-files")
(setq *cur-dyno-file-pref* "dyno-")
(setq *cur-dyno-file-suf* ".org")
(setq *cur-dyno-header-suf* "# ─────────────")
(setq *dyno-last-searches-alist* nil)
(setq *dyno-log-enabled* t)
(setq *dyno-write-logs-to-file* nil)
(setq *dyno-debounce* 0.2)
(defvar *dyno-reload-timer* nil "Timer for debouncing the reload function.")

;;; dyno-misc

(defun dyno-message (format-string &rest args)
  (when *dyno-log-enabled*
    (if (null *dyno-write-logs-to-file*)
        (apply #'message (cons format-string args))
      (let* ((log (apply #'format (cons format-string args))))
        (with-temp-buffer
          (insert log)
          (insert "\n")
          (write-region (point-min) (point-max) *dyno-write-logs-to-file* 'append))))))

;;; dyno-files

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

;;; dyno-sync

(defun dyno-connect-session ()
  (interactive)
  (unless (is-dyno-file)
    (error (format "Not dyno file: %s" (buffer-file-name))))
  (let* ((ss (or (dyno-search-state-read-buf) dyno-search-state-empty)))
    (dyno-message "Found search-state: %s" ss)
    (remove-hook 'after-change-functions 'dyno-maybe-reload)
    (erase-buffer)
    (insert (format "%s\n%s" ss *cur-dyno-header-suf*))
    (add-hook 'after-change-functions 'dyno-maybe-reload-debounced nil t)
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

(defun dyno-search-state-read-buf ()
  (dyno-message "#dyno-search-state-read-buf")
  (condition-case nil
      (save-excursion
        (goto-char (point-min))
        (search-forward *cur-dyno-header-suf*)
        (let* ((search-state-end (- (point) (length *cur-dyno-header-suf*)))
               (search-state (s-trim (buffer-substring-no-properties 1 search-state-end))))
          (message "search-state: %s" search-state)
          search-state))
        (error nil)))

(defun dyno-get-out-start ()
  (save-excursion
    (goto-char (point-min))
    (condition-case nil
        (when (search-forward *cur-dyno-header-suf* nil t)
          (let* ((pos (match-end 0)))
            (dyno-message "#dyno-get-out-start, position -> %d" pos)
            pos))
      (error nil))))
  

(defun dyno-reload-inner (search-state pos)
  (dyno-message "#dyno-reload-inner <%s> <%d>" search-state pos)
  (let* ((match-pos (dyno-get-out-start)))
    (if (null match-pos)
       (progn
         (dyno-message "Not found header...")
         (erase-buffer)
         (insert dyno-search-state-empty)
         (insert "\n")
         (insert *cur-dyno-header-suf*))
      (save-excursion
        (dyno-message "Going to update generated org part")
        (goto-char (point-min))
        (when (search-forward *cur-dyno-header-suf* nil t)
          (when (or (null pos) (<= pos match-pos))
            (delete-region match-pos (point-max))
            (dyno-message "search-state: %s" search-state)
            (when (not (equal dyno-search-state-empty search-state))
              (let* ((start-time (float-time (current-time)))
                     (backend-res (funcall dyno-search-items-backend search-state))
                     (end-time (float-time (current-time)))
                     (elapsed-time (- end-time start-time))
                     )
                (dyno-message "length(backend-res): %d, ready in %.6f seconds" (length backend-res) elapsed-time)
                (insert "\n")
                (insert backend-res)
                (save-buffer)))))))))
      

(defun dyno-maybe-reload-debounced (&optional beg end smth)
  (interactive)
  (dyno-message "(#dyno-maybe-reload-debounced %s %s %s)" beg end smth)
  (when *dyno-reload-timer*
    (cancel-timer *dyno-reload-timer*))
  (setq *dyno-reload-timer* (run-at-time *dyno-debounce* nil 'dyno-maybe-reload beg end smth))
  )

(defun dyno-maybe-reload (&optional beg end smth)
  (interactive)
  (when *dyno-reload-timer*
    (cancel-timer *dyno-reload-timer*))
  (dyno-message "(#dyno-maybe-reload %s %s %s)" beg end smth)
  (dyno-message "inhibit-modification-hooks: %s" inhibit-modification-hooks)
  (let* ((old-search-state (plist-get *dyno-last-searches-alist* (buffer-file-name)))
         (cur-search-state (or (dyno-search-state-read-buf) dyno-search-state-empty)))
    (dyno-message "old-search: %s" old-search-state)
    (dyno-message "cur-search: %s" cur-search-state)
    (when (or (null old-search-state) (not (equal old-search-state cur-search-state)))
      (plist-put *dyno-last-searches-alist* (buffer-file-name) cur-search-state)
      (dyno-reload-inner cur-search-state (or end (point)))))
  )
         
(defun dyno-is-dynamic-buffer (buffer)
  (let* ((b-name (buffer-file-name buffer))
         (res (and
               (stringp b-name)
               (s-contains? "/dyno-" b-name)
               (s-ends-with? ".org" b-name))
              )
         ) res))

(defun dyno-reload-dynamic-buffers ()
  (let* ((buffers (buffer-list))
         (f-buffers (-filter #'dyno-is-dynamic-buffer buffers))
         )
    (message "Going to reload buffers: %s" f-buffers)
    (cl-loop
     for buf in f-buffers
     do (with-current-buffer buf
          (dyno-maybe-reload)))))

(defun dyno-reload ()
  (interactive)
  (funcall dyno-reload-backend)
  (message "reloaded!")
  (dyno-reload-dynamic-buffers))

;;; dyno-actions

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

(defun dyno-string-at-point ()
  (when (looking-at "[\s\n]")
    (sexp-at-point)))

(defun dyno-replace-sexp-at-point (new-sexp)
  (dyno-message "new-sexp: %s" new-sexp)
  (when (dyno-string-at-point)
    (backward-kill-sexp))
  (insert new-sexp))

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

;; dyno-dummy

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
                     with text = search-state
                     for i from 1 to 10
                     for org-title = (format "%s: %s" text i)
                     for org-text = (format "text... %s" i)
                     for org-item = (format "** %s\ndate: 15.11.22\nbla-bla-bla%s" org-title org-text)
                     collect org-item))
         (res (s-join "\n" org-items))
         ) res))

(setq dyno-search-state-empty--dummy "# search: ")

;; dummy functions, should be redefined
(setq dyno-search-items-backend #'dyno-search-items--dummy)
;; (setq dyno-search-items-backend #'dyno-search-items-example)
(setq dyno-reload-backend #'dyno-reload-backend--dummy)
(setq dyno-search-state-empty dyno-search-state-empty--dummy)
(setq dyno-suggest-tags-backend #'dyno-suggest-tags-backend--dummy)

(provide 'dyno)
