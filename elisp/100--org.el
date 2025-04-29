(setq org-export-coding-system 'utf-8)
(setq calendar-week-start-day 1)
(setq org-log-done nil)
(setq org-image-actual-width '(500))
(setq org-duration-format 'h:mm)
(setq
 org-duration-units
 (list
  (cons "min" 1)
  (cons "h"   60)
  (cons "d"   (* 100500 1440))
  (cons "w"   10080)
  (cons "m"   43200)
  (cons "y"   525960.0)))
(require 'org-id)

(global-set-key (kbd "C-c c") 'org-capture)

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-agenda-window-setup 'current-window)
(setq org-agenda-skip-scheduled-if-done t)

(defun ej/toggle-org-html-export-on-save ()
  " https://www.reddit.com/r/emacs/comments/4golh1/how_to_auto_export_html_when_saving_in_orgmode/ "
  (interactive)
  (if (memq 'org-html-export-to-html after-save-hook)
      (progn
        (remove-hook 'after-save-hook 'org-html-export-to-html t)
        (message "Disabled org html export on save for current buffer..."))
    (add-hook 'after-save-hook 'org-html-export-to-html nil t)
    (message "Enabled org html export on save for current buffer...")))

(defun ej/org-inline-css-hook (exporter)
  "Insert custom inline css"
  (when (eq exporter 'html)
    (let* ((dir (ignore-errors (file-name-directory (buffer-file-name))))
           (path (concat dir "style.css"))
           (homestyle (or (null dir) (null (file-exists-p path))))
           (final (if homestyle org-html-export-css-file path))) ;; <- set your own style file path
      (setq org-html-head-include-default-style nil)
      (setq
       org-html-head
       (concat
        "<style type=\"text/css\">\n"
        "<!--/*--><![CDATA[/*><!--*/\n"
        (with-temp-buffer
          (insert-file-contents final)
          (buffer-string))
        "/*]]>*/-->\n"
        "</style>\n")))))
;; (add-hook 'org-export-before-processing-hook 'ej/org-inline-css-hook)

(defun ej/add-line-item-helper (is-sub indent-pow-4)
  (let* ((CHAR-NUMBER (string-to-char " "))
         (CHAR-MULT 2)
         (current-line (buffer-substring-no-properties (line-beginning-position) (point)))
         (has-box (cl-search "[" current-line))
         (beg-of-line (line-beginning-position))
         (indent-level (- (save-excursion (back-to-indentation) (point)) beg-of-line)))
    (newline)
    (insert-char ?\s indent-level)
    (let* ((base-indent (* CHAR-MULT (truncate (log indent-pow-4 4))))
           (indent (if is-sub base-indent (+ CHAR-MULT base-indent))))
      (if is-sub
          (insert-char CHAR-NUMBER indent)
        (backward-delete-char-untabify indent)))
    (insert (if (not has-box) "- " "- [ ] "))))

(defun ej/add-line-item (&optional mode)
  (interactive "p")
  (ej/add-line-item-helper t mode))

(defun ej/add-line-item-reverse (&optional mode)
  (interactive "p")
  (ej/add-line-item-helper nil mode))

(defun ej/tbl-export (name)
  "Search for table named `NAME` and export."
  (interactive "s")
  (outline-show-all)
  (let ((case-fold-search t))
    (if (search-forward-regexp (concat "#\\+NAME: +" name) nil t)
    (progn
      (next-line)
      (org-table-export (format "%s.csv" name) "orgtbl-to-csv")))))

;; ej/copy-cell
(fset 'ej/copy-cell [?\M-a ?\C-  ?\M-e ?\M-w tab])
(global-set-key (kbd "<C-f2>") 'ej/copy-cell)

(setq ORG-BASE-PREF "#+begin_src ")
(setq ORG-BASE-SUFF "#+end_src")
(setq MD-BASE-PREF "```")
(setq MD-BASE-SUFF "```")

(defun ej/wrap-src (lang)
  (let* ((snippet (s-trim (buffer-substring-no-properties (mark) (point))))
         (pref (if (equal major-mode 'org-mode) ORG-BASE-PREF MD-BASE-PREF))
         (suff (if (equal major-mode 'org-mode) ORG-BASE-SUFF MD-BASE-SUFF))
         (mb-newline (if (zerop (current-column)) "\n" ""))
         (block (s-concat pref lang "\n" snippet "\n" suff mb-newline)))
    (kill-region (mark) (point))
    (insert block)))

(use-package language-detection)

(defun ej/detect-lang (snippet)
  (let* ((detected-lang
          (condition-case nil
              (symbol-name (language-detection-string snippet))
            (error nil))))
    (pcase detected-lang
      ("awk" "elisp")
      ("lisp" "elisp")
      ("emacslisp" "elisp")
      ("json" "js")
      (_ detected-lang))))

(defun ej/wrap-src-interactive ()
  (interactive)
  (let* ((snippet (buffer-substring-no-properties (mark) (point)))
         (detected-lang (ej/detect-lang snippet))
         (prompt (format "Which language? (default: [%s]) " detected-lang))
         (lang-read (read-from-minibuffer prompt))
         (lang (if (empty lang-read) detected-lang lang-read)))
    (ej/wrap-src lang)))

(defun ej/async-shell-command-no-window (command)
  (let ((display-buffer-alist 
         (list (cons 
                "\\*Async Shell Command\\*.*"
                (cons #'display-buffer-no-window nil)))))
    (async-shell-command command)))

(setq CLIP-CMD-FMT "xclip -selection clipboard -t TARGETS -o | grep image/png && xclip -selection clipboard -t image/png -o > %s")
(defun ej/org-clip-to-file (&optional arg)
  (interactive)
  (if t ;; (s-contains? "image/png" (shell-command ))
      (let* ((dir (file-name-directory (buffer-file-name)))
             (_ (message "all files: %s" (directory-files (concat dir "./images/") nil nil)))
             (files (directory-files (concat dir "./images/") nil ".png$"))
             (files-cnt (length files))
             (_ (message "files-cnt: %s\narg: %s" files-cnt arg))
             (short-fname (if t ;; (equal arg 1)
                        (let* ((new-fname-0 (format "screenshot-%0d.png" (1+ files-cnt))))
                          (if (not (-contains? files new-fname-0))
                              new-fname-0
                            (s-replace new-fname-0 ".png" "--.png"))) nil))
;;                          (concat (car (interactive "senter new image name: ")) ".png")))
             (_ (message "short-fname: %s" short-fname))
             (fname (concat "./images/" short-fname)))
        (async-shell-command-no-window (format CLIP-CMD-FMT fname))
        (if (eq major-mode 'org-mode)
            (insert (format "[[file:%s]]" fname))
          (progn
            (message "Saved image to file: %s, saved to yank" fname)
            (kill-new fname))))
    (message "No image content in clipboard!")))

(require 'calendar)
(defun kisaragi/english-dow (&optional time zone abbreviated)
  "
  Return ABBREVIATED name of the day of week at TIME and ZONE.
  If TIME or ZONE is nil, use `current-time' or `current-time-zone'.
  "
  (unless time (setq time (current-time)))
  (unless zone (setq zone (current-time-zone)))
  (calendar-day-name
   (pcase-let ((`(,_ ,_ ,_ ,d ,m ,y . ,_)
                (decode-time time zone)))
     (list m d y))
   abbreviated))

(defun kisaragi/advice-format-time-string (func format &optional time zone)
  "
  Pass FORMAT, TIME, and ZONE to FUNC.
  Replace \"%A\" in FORMAT with English day of week of today,
  \"%a\" with the abbreviated version.
  "
  (let* ((format (s-replace "%a" (kisaragi/english-dow time zone t) format))
         (format (s-replace "%A" (kisaragi/english-dow time zone nil) format)))
    (funcall func format time zone)))
(advice-add 'format-time-string :around #'kisaragi/advice-format-time-string)

(use-package org-download
  :defer t
  :config
  (add-hook 'dired-mode-hook 'org-download-enable)
  )

(defun ej/org-hook ()
  ;; don't need margins after new-line
  (electric-indent-local-mode -1)
  (visual-line-mode 1)

  (local-set-key (kbd "s-t") 'visual-line-mode)
  (local-set-key (kbd "<C-return>") 'ej/add-line-item)
  (local-set-key (kbd "<C-S-return>") 'ej/add-line-item-reverse)
  (local-set-key (kbd "C-x w") 'ej/wrap-src-interactive)
  (local-set-key (kbd "C-M-x") 'eval-defun)
  (local-set-key (kbd "M-s-b") 'ej/boldify)
  )
(add-hook 'org-mode-hook 'ej/org-hook)

(defun ej/activate-org-latex ()
  (interactive)
  (load-el "elisp/101--org-latex.el"))
