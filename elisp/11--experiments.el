;; Automates preview of LaTeX fragments (text shown when point on preview)
;; Kinda buggy. Also bad previews sometimes.
(use-package org-fragtog
  :config
  (add-hook 'org-mode-hook 'org-fragtog-mode))

;; Profile the startup time of Emacs in the background.
(use-package esup
  :ensure t
  ;; To use MELPA Stable use ":pin melpa-stable",
  :commands (esup))
(setq esup-depth 0)

;; org-latex-preview via lua
(setq luamagick '(luamagick :programs ("lualatex" "convert")
       :description "pdf > png"
       :message "you need to install lualatex and imagemagick."
       :use-xcolor t
       :image-input-type "pdf"
       :image-output-type "png"
       :image-size-adjust (1.0 . 1.0)
       :latex-compiler ("lualatex -interaction nonstopmode -output-directory %o %f")
       :image-converter ("convert -density %D -trim -antialias %f -quality 100 %O")))

(add-to-list 'org-preview-latex-process-alist luamagick)

(setq org-preview-latex-default-process 'luamagick)

;; showing timestamp in *Messages*
;;  https://emacs.stackexchange.com/questions/32150/how-to-add-a-timestamp-to-each-entry-in-emacs-messages-buffer
(defun sh/current-time-microseconds ()
  "Return the current time formatted to include microseconds."
  (let* ((nowtime (current-time))
         (now-ms (nth 2 nowtime)))
    ; (concat (format-time-string "[%Y-%m-%dT%H:%M" nowtime) (format ".%d]" now-ms))))
    (format-time-string "[%Y-%m-%dT%H:%M" nowtime)))
(defun sh/ad-timestamp-message (FORMAT-STRING &rest args)
  "Advice to run before `message' that prepends a timestamp to each message.
Activate this advice with:
(advice-add 'message :before 'sh/ad-timestamp-message)"
  (unless (string-equal FORMAT-STRING "%s%s")
    (let ((deactivate-mark nil)
          (inhibit-read-only t))
      (with-current-buffer "*Messages*"
        (goto-char (point-max))
        (if (not (bolp))
          (newline))
        (insert (sh/current-time-microseconds) " ")))))
; (advice-add 'message :before 'sh/ad-timestamp-message)

;; org-roam
(use-package org-roam
  :hook 
  (after-init . org-roam-mode)
  :config
  (setq org-roam-directory org-roam-dir)
  (:bind (:map org-roam-mode-map
               ("C-c n l" . 'org-roam)
               ("C-c n l" . 'org-roam)
               ("C-c n f" . 'org-roam-find-file)
               ("C-c n b" . 'org-roam-switch-to-buffer)
               ("C-c n g" . 'org-roam-graph-show)
               :map org-mode-map 
               ("C-c n i" . 'org-roam-insert))))

;; fb2
(defun fb2-mode-view()
  (vc-toggle-read-only)
  (interactive)
  (sgml-mode)
  (sgml-tags-invisible 0))
(defun fb2-mode-edit()
  (vc-toggle-read-only nil)
  (interactive)
  (sgml-mode)
  (sgml-tags-invisible 0))
(add-to-list 'auto-mode-alist '(".fb2$" . fb2-mode-view))

;; delete-current-file
(defun xah-delete-current-file (&optional @no-backup-p)
  "Delete current buffer/file.
   If buffer is a file, makes a backup~, else, push file content to `kill-ring'.
   
   If buffer is dired, go up a dir and mark it for delete  (by `dired-flag-file-deletion').
    (press 【x】 to call `dired-do-flagged-delete'  to actually delete it)
   
   This commands calls `xah-delete-current-file-make-backup' or
    `xah-delete-current-file-copy-to-kill-ring'.
   
   If next buffer is dired, refresh it.
   
   URL `http://ergoemacs.org/emacs/elisp_delete-current-file.html'
   Version 2017-08-27"
  (interactive "P")
  (if (eq major-mode 'dired-mode)
      (progn (dired-up-directory)
             (dired-flag-file-deletion 1)
             (revert-buffer))
    (progn
      (if (buffer-file-name)
          (xah-delete-current-file-make-backup @no-backup-p)
        (xah-delete-current-file-copy-to-kill-ring)))
    (when (eq major-mode 'dired-mode)
      (revert-buffer))))

;; all-the-icons
(use-package all-the-icons-dired
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

;; toc-org
(use-package toc-org
  :config
  ; (add-hook 'markdown-mode-hook 'toc-org-mode)
  ; (define-key (null markdown-mode-map) (kbd "\C-c\C-o") 'toc-org-markdown-follow-thing-at-point)
)
