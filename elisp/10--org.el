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

(use-package cdlatex
  :defer t
  :config
  (add-hook 'org-mode-hook 'turn-on-org-cdlatex))

(use-package auctex
  :defer t)

(setq org-preview-latex-image-directory (concat org-latex-preview-dir "/"))
;; `t` leads to strange behavior with headings... :(
(setq org-hide-emphasis-markers nil)

(defun ej/org-latex-hook ()
  (local-set-key (kbd "M-s-j") 'org-latex-preview)

  ;; cdlatex
  (define-key org-cdlatex-mode-map "_" nil)
  (define-key org-cdlatex-mode-map "^" nil)
  (define-key org-cdlatex-mode-map "'" nil)
  (define-key org-cdlatex-mode-map "`" nil)
)
(add-hook 'org-mode-hook 'ej/org-latex-hook)

;; (use-package ox-bibtex
;;   :config
;;   (setq bibtex-dialect 'biblatex)
;;   ;; open pdf with system pdf viewer (works on mac)
;;   ;; todo fix
;;   (setq bibtex-completion-pdf-open-function
;;         (lambda (fpath) (start-process "open" "*open*" "open" fpath))))
;;   
;; (use-package helm-bibtex)


(defun ej/bib-pref (file) 
  (expand-file-name file bibliography-dir))
(setq reftex-default-bibliography (ej/bib-pref "references.bib"))

;; see org-ref for use of these variables
(setq org-ref-bibliography-notes (ej/bib-pref "notes.org")
      org-ref-default-bibliography reftex-default-bibliography
      org-ref-pdf-directory (ej/bib-pref "bibtex-pdfs")
      bibtex-completion-bibliography reftex-default-bibliography
      bibtex-completion-library-path (ej/bib-pref "bibtex-pdfs")
      bibtex-completion-notes-path (ej/bib-pref "helm-bibtex-notes")
)

(use-package org-ref
  :defer t
  :config
  (require 'org-ref-pdf)
  (require 'org-ref-url-utils)
  (require 'org-id)
  (require 'org-ref-wos)
  (require 'org-ref-scopus)
  (require 'org-ref-pubmed)
)

(setq org-latex-hyperref-template "\\hypersetup{
 pdfauthor={%a},
 pdftitle={%t},
 pdfkeywords={%k},
 pdfsubject={%d},
 pdfcreator={%c}, 
 pdflang={%L},
 colorlinks=true,
 linkcolor=blue,
 linkbordercolor=red,
 urlbordercolor=cyan,
 unicode=true
}
")

(setq org-latex-pdf-process
      '("pdflatex -interaction nonstopmode -output-directory %o %f"
        "bibtex %b"
				;;; "biber %b"
        "pdflatex -interaction nonstopmode -output-directory %o %f"
        "pdflatex -interaction nonstopmode -output-directory %o %f"))
(setq org-latex-to-pdf-process org-latex-pdf-process)
(setq org-latex-compiler "latex")

(defun ej/activate-org-latex-pdf-process-with-bibliography ()
  (interactive)
  (setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f")))

(defun ej/export-to-pdf-or-beamer ()
  (interactive)
  (let* ((is-slides (condition-case nil
                        (save-excursion
                          (beginning-of-buffer)
                          (search-forward "\n#+BEAMER_THEME"))
                      (error nil))))
    (if is-slides
        (org-beamer-export-to-pdf)
      (org-latex-export-to-pdf))))
(define-key org-mode-map (kbd "C-<f11>") 'ej/export-to-pdf-or-beamer)

(defun ej/activate-xelatex ()
  (interactive)
  (setq org-latex-to-pdf-process
        '("xelatex -interaction nonstopmode %f"
          "xelatex -interaction nonstopmode %f"))
  (setq org-latex-pdf-process
        '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f" ))
  (setq org-latex-compiler "xelatex"))
;; (ej/activate-xelatex)

(add-to-list 'org-latex-packages-alist '("" "listingsutf8"))

(defun ej/activate-minted ()
  (interactive)
  (require 'ox-latex)
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (add-to-list 'org-latex-minted-langs '(python "python"))
  (setq org-latex-listings 'minted)
  (setq org-latex-pdf-process
    '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
      "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
      "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  
  ;; don't sure next 2 lines needed
  (setq org-src-preserve-indentation t)
  (setq org-latex-minted-options '(("tabsize" "2")))
  ;; (setq org-latex-minted-options '(("obeytabs" "true")))

  ;; don't work :(
  ;; (let ((minted-extra "\n\\usemintedstyle{default}\n\\setminted{fontsize=\\small}"))
  ;;  (setq org-format-latex-header (concat org-format-latex-header minted-extra)))
)
;; (ej/activate-minted)

(setq org-export-html-postamble nil)
(setq org-html-postamble-format nil)
(setq org-html-postamble nil)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((C . t)
   (gnuplot . t)
   (python . t)
   (emacs-lisp . t)
   (mscgen . t)
   (R . t)
   (latex . t)
   (org . t)
   (lisp . t)
   (shell . t)
   (dot . t)
   ;; (coq . t)
   ))

(defun ej/disable-confirm-babel-evaluate ()
  (interactive)
  (setq org-confirm-babel-evaluate nil))

(add-to-list 'org-latex-packages-alist '("" "tikz" t))
(eval-after-load "preview"
  '(add-to-list 'preview-default-preamble "\\PreviewEnvironment{tikzpicture}" t))
; (setq org-preview-latex-default-process 'imagemagick)
(setq org-preview-latex-default-process 'dvipng)
(defun ej/switch-preview-latex-default-process ()
  (interactive)
  (setq org-preview-latex-default-process 
    (if (eq org-preview-latex-default-process 'dvipng) 'imagemagick 'dvipng))
  (message (format "updated org-preview-latex: %s" org-preview-latex-default-process)))

;; (plist-put org-format-latex-options :scale 2.0)

(defun ej/refresh-latex-preview ()
  (interactive)
  (delete-directory org-latex-preview-dir t t))

(defun update-org-latex-fragments ()
  (org-latex-preview 4)
  (plist-put org-format-latex-options :scale text-scale-mode-amount)
  (org-latex-preview 16))

(defun ej/fix-org-format-options-scale()
  (interactive)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 4.0)))
(ej/fix-org-format-options-scale)

(defun ej/delete-latex-previews ()
  (interactive)
  (delete-directory org-preview-latex-image-directory t))

(defun remove-top-files (dir files count)
  (let* ((sub-files (-slice files 0 (min count (length files)))))
    (--map (delete-file (expand-file-name it dir)) sub-files)))

(setq fregexp "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)")
(defun get-files-sorted-by-update (dir)
  (let ((files (sort (directory-files-and-attributes dir nil fregexp)
    #'(lambda (x y) (not (time-less-p (nth 6 x) (nth 6 y)))))))
    (--map (car it) files)))

(defun remove-last-files (dir count)
  (remove-top-files dir (get-files-sorted-by-update dir) count))

(defun ej/remove-last-ltximg-latex-preview (&optional mode)
  (interactive "p")
  (message (format "mode::::::: %d" mode))
  (let ((cnt (cadr (assoc mode '((nil 1) (1 1) (4 4) (16 16))))))
    (remove-last-files (concat org-latex-preview-dir "/") cnt)
    (message (format "removed %d files" cnt)))
  t)

(setq var-change-layout nil)
(defun ej/insert-latex-section ()
  (interactive)
  (if (not (or (bolp) (eq (preceding-char) ?\s)))
      (insert " "))
  (let ((outside (null (org-inside-LaTeX-fragment-p))))
    (insert (if outside "\\( " "\\)"))
    (when (or (and outside (ej/is-current-layout-ru)) (and (not outside) var-change-layout))
      (setq var-change-layout (not var-change-layout))
      (ej/switch-layout))))

(defun ej/is-current-layout-ru ()
  (interactive)
  (equal (shell-command-to-string "xkblayout-state print %s") "ru"))
(defun ej/switch-layout ()
  (shell-command-to-string "xkblayout-state set +1"))

(define-key global-map (kbd "C-s-j") 'ej/insert-latex-section)

(unless (boundp 'org-latex-classes)
  (setq org-latex-classes nil))

(add-to-list 'org-latex-classes
                '("report"
                  "\\documentclass{report}"
                  ("\\chapter{%s}" . "\\chapter*{%s}")
                  ("\\section{%s}" . "\\section*{%s}")
                  ("\\subsection{%s}" . "\\subsection*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

(add-to-list 'org-latex-classes
                '("compact-report"
               "\\documentclass[a4paper,11pt]{extarticle}"
                  ("\\subsection{%s}" . "\\section*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

(add-to-list 'org-latex-classes
                '("compact-report2"
               "\\documentclass[a4paper,11pt]{extarticle}"
                  ("\\section{%s}" . "\\chapter*{%s}")
                  ("\\subsection{%s}" . "\\subsection*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

(add-to-list 'org-latex-classes
                '("compact-report3"
               "\\documentclass[a4paper,11pt]{extarticle}"
                  ("\\section{%s}" . "\\chapter*{%s}")
                  ("\\subsection{%s}" . "\\subsection*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

(add-to-list 'org-latex-classes
                '("compact-report3-landscape"
               "\\documentclass[a4paper,landscape,11pt]{extarticle}"
                  ("\\section{%s}" . "\\chapter*{%s}")
                  ("\\subsection{%s}" . "\\subsection*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

(add-to-list 'org-latex-classes
						 '("a6-report"
							 "\\documentclass[15pt]{article}"
                  ("\\section{%s}" . "\\chapter*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

(add-to-list 'org-latex-classes
               '("beamer"
                 "\\documentclass[presentation]{beamer}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

(add-to-list 'org-latex-classes
             '("article"
               "\\documentclass{article}"
               ("\\section{%s}" . "\\section*{%s}")))

(add-to-list 'org-latex-classes
                '("springer-article"
                  "\\documentclass[runningheads]{llncs}"
                  ("\\section{%s}" . "\\section*{%s}")
                  ("\\nosubsection{%s}" . "\\nosubsection*{%s}")))

(add-to-list 'org-latex-classes
             '("extarticle"
               "\\documentclass[a4paper,14pt]{extarticle}"
               ("\\section{%s}" . "\\section*{%s}")))

(add-to-list 'org-latex-classes
             '("extarticle2"
               "\\documentclass[a4paper,14pt]{extarticle}"
                  ("\\subsection{%s}" . "\\section*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

(add-to-list 'org-latex-classes
             '("extarticle3"
               "\\documentclass[a4paper,14pt]{extarticle}"
                  ("\\section{%s}" . "\\section*{%s}")
                  ("\\subsection{%s}" . "\\subsection*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

(add-to-list 'org-latex-classes
             '("extarticle4"
               "\\documentclass[a4paper,14pt]{extarticle}"
                  ("\\section{%s}" . "\\section*{%s}")
                  ("\\subsection{%s}" . "\\subsection*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

(add-to-list 'org-latex-classes
             '("diplom"
               "\\documentclass[a4paper,14pt,fleqn]{extreport}"
               ;; "\\documentclass[a4paper,12pt,fleqn]{article}"
               ("\\section{%s}" . "\\section*{%s}")))

(add-to-list 'org-latex-classes
             '("diplom2"
               "\\documentclass[a4paper,14pt,fleqn]{extreport}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ))

(add-to-list 'org-latex-classes
             '("diplom3"
               "\\documentclass[a4paper,14pt,fleqn]{extreport}"
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ))

(add-to-list 'org-latex-classes
             '("diplom-report"
               "\\documentclass[12pt]{amsart}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ))

(add-to-list 'org-latex-classes
             '("book"
               "\\documentclass{book}"
               ("\\part{%s}" . "\\part*{%s}")
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

;;;; 
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
      ; (message (format "org html head: %s" org-html-head))
      
      (setq org-html-head (concat
                           "<style type=\"text/css\">\n"
                           "<!--/*--><![CDATA[/*><!--*/\n"
                           (with-temp-buffer
                             (insert-file-contents final)
                             (buffer-string))
                           "/*]]>*/-->\n"
                           "</style>\n")))))

(defun ej/toggle-org-pdf-export-on-save ()
  " https://www.reddit.com/r/emacs/comments/4golh1/how_to_auto_export_html_when_saving_in_orgmode/ "
  (interactive)
  (if (memq 'org-latex-export-to-pdf after-save-hook)
      (progn
        (remove-hook 'after-save-hook 'org-latex-export-to-pdf t)
        (message "Disabled org html export on save for current buffer..."))
    (add-hook 'after-save-hook 'org-latex-export-to-pdf nil t)
    (message "Enabled org html export on save for current buffer...")))

;; (add-hook 'org-export-before-processing-hook 'ej/org-inline-css-hook)

(defun ej/add-line-item-helper (is-sub indent-pow-4)
  (let* ((CHAR-NUMBER (string-to-char " "))
         (CHAR-MULT 2)
         (current-line (buffer-substring-no-properties (line-beginning-position) (point)))
         (has-box (or (cl-search "[X]" current-line) (cl-search "[ ]" current-line)))
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

(defun ej/detect-lang (snippet)
  (let* ((detected-lang (symbol-name (language-detection-string snippet))))
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

(defun ej/insert-kotlin-before-after ()
  (interactive)
  (insert "\n*** before\n#+begin_src kotlin\n\n#+end_src\n*** after\n#+begin_src kotlin\n\n#+end_src"))

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
  "Return ABBREVIATED name of the day of week at TIME and ZONE.

If TIME or ZONE is nil, use `current-time' or `current-time-zone'."
  (unless time (setq time (current-time)))
  (unless zone (setq zone (current-time-zone)))
  (calendar-day-name
   (pcase-let ((`(,_ ,_ ,_ ,d ,m ,y . ,_)
                (decode-time time zone)))
     (list m d y))
   abbreviated))

(defun kisaragi/advice-format-time-string (func format &optional time zone)
  "Pass FORMAT, TIME, and ZONE to FUNC.

Replace \"%A\" in FORMAT with English day of week of today,
\"%a\" with the abbreviated version."
  (let* ((format (s-replace "%a" (kisaragi/english-dow time zone t) format))
         (format (s-replace "%A" (kisaragi/english-dow time zone nil) format)))
    (funcall func format time zone)))

(advice-add 'format-time-string :around #'kisaragi/advice-format-time-string)

(use-package org-download
  :defer t
  :config
  (add-hook 'dired-mode-hook 'org-download-enable)
  )

(defun env-inserter (env is-org)
  (let (
      (start-fmt (if is-org "#+begin_%s\n" "\\begin{%s}\n"))
      (end-fmt (if is-org "\n#+end_%s\n" "\n\\end{%s}\n")))
    (insert (format start-fmt env))
    (save-excursion (insert (format end-fmt env)))))

(defhydra ej/org-insert-env (:foreign-keys warn :columns 1 :exit t)
  "Insert..."
  ("p" (env-inserter "proof" t) "proof")
  ("t" (env-inserter "theorem" t) "theorem")
  ("d" (env-inserter "definition" t) "definition")
  ("l" (env-inserter "lemma" t) "lemma")
  ("s" (env-inserter "statement" t) "statement")
  ("e" (env-inserter "equation*" nil) "equation*")
  ("p" (env-inserter "proof" t) "proof")
  ("c" (env-inserter "corollary" t) "corollary")
  ("w" (env-inserter "wtf" t) "wtf")
  ("b" (insert "#+BEAMER: \\pause\n") "BEAMER pause") 
  ("<ESC>" nil "quit")
)

(defun ins-helper (before after)
  (interactive)
  (insert before)
  (save-excursion (insert after)))

(defun ins-env (env)
  (interactive)
  (let (
      (start (format "\\begin{%s}\n  " env))
      (end (format "\n\\end{%s}" env)))
    (ins-helper start end)))
(defhydra ej/tex-insert-env (:foreign-keys warn :columns 1 :exit t)
  "Insert..."
  ("n" (ins-helper "\\begin{enumerate}\n  \\item " "\n\\end{enumerate}") "num-list (enumerate)")
  ("l" (ins-helper "\\begin{itemize}\n  \\item " "\n\\end{itemize}") "list (itemize)")
  ("L" (ins-env "lemma") "lemma")
  ("b" (ins-helper "\\textbf{" "}") "bold")
  ("c" (ins-env "corollary") "corollary")
  ("i" (ins-helper "\\textit{" "}") "italic (textit)")
  ("d" (ins-env "definition") "definition")
  ("x" (ins-env "example") "example")
  ("X" (ins-env "examples") "examples")
  ("e" (ins-env "equation") "equation")
  ("E" (ins-env "exercise") "equation")
  ("r" (ins-env "remark") "remark")
  ("s" (ins-env "statement") "statement")
  ("p" (ins-env "proof") "proof")
  ("P" (ins-env "proposition") "proposition")
  ("t" (ins-helper "\\[ \\begin{tikzcd}\n" "\n\\end{tikzcd} \\]") "tikzcd")
  ("T" (ins-env "theorem") "theorem")
  ("o" (insert "^{op}") "^{op}")
  ("m" (insert "^{-1}") "^{-1}")
  ("D" (insert "\\cD") "\\cD")
  ("C" (insert "\\cC") "\\cC")
  ("u" (ins-helper "\\url{" "}"))
  ("<ESC>" nil "quit")
)

(defun ej/tex-expand-sequence ()
  (interactive)
  (let* ((pos (point))
         (_ (re-search-backward "_[01]"))
         (end (point))
         (_ (backward-sexp))
         (item (buffer-substring-no-properties (point) end))
         (_ (message "your item: %s" item))
         (_ (goto-char pos)))
    (insert (format ", \\dots, %s_" item))))

(setq ej/latex-headers
      (if (ej/file-exists-p latex-headers-file)
          (vc--read-lines latex-headers-file) '()))

(defun ej/add-def (header)
  (save-excursion
    (search-backward "#+LATEX_HEADER:")
    (end-of-line 1)
    (newline)
    (insert header))
  (message "added header: %s" header))

(defun ej/introduce-latex-def ()
  (interactive)
  (helm
   :sources  (helm-build-sync-source "Add LATEX-HEADER"
               :candidates ej/latex-headers
               :action 'ej/add-def
               :fuzzy-match t)
   :buffer "*helm suggestion latex header*"))

(defun ej/sat-latexify-0 (formula)
  (let ((replaces '(("or" "\\\\lor")
                    ("and" "\\\\land")
                    ("not(\\(.\\))" "\\\\ov{\\1}"))))
    (--reduce-from (s-replace-regexp (car it) (cadr it) acc) formula replaces)))

(defun ej/sat-latexify ()
  " converts formulas to texified formulas:
  'x and (z or not(x))' >> 'x \\land (z \\lor \\ov{x})' "
  (interactive)
  (search-backward "\\(")
  (forward-char 3)
  (let* ((start (point))
         (_ (end-of-line))
         (end (point))
         (_ (kill-region start end))
         (latexified (ej/sat-latexify-0 (last-killed))))
    (insert latexified)))

(defhydra ej/hydra-org (:foreign-keys warn :columns 1 :exit t)
  " Org LaTeX helpers "
  ("e" ej/tex-expand-sequence "expand tex sequence")
  ("h" ej/introduce-latex-def "introduce latex header")
  ("s" ej/sat-latexify "latexify SAT formula")
  ("r" ej/remove-last-ltximg-latex-preview "Remove last preview")
  ("R" (ej/remove-last-ltximg-latex-preview 16) "Remove last 16 previews")
  ("i" ej/org-insert-env/body "insert env ORG hydra")
  ("I" ej/tex-insert-env/body "insert env TEX hydra")
  ("l" (switch-to-buffer "*Org PDF LaTeX Output*") "*Org PDF LaTeX Output*" :exit t)
  ("d" org-ref-bibtex-new-entry/doi-insert-bibtex-and-exit "insert by DOI")
  ("t" (insert "^{\\theta}") "insert theta")
  ("<ESC>" nil "exit"))

(defun ej/org-hook ()
  (local-set-key (kbd "s-t") 'visual-line-mode)
  (local-set-key (kbd "<C-return>") 'ej/add-line-item)
  (local-set-key (kbd "<C-S-return>") 'ej/add-line-item-reverse)
  (local-set-key (kbd "C-x w") 'ej/wrap-src-interactive)

  (local-set-key (kbd "C-M-x") 'eval-defun)
  (local-set-key (kbd "C-M-x") 'eval-defun)

  ;; don't need margins after new-line
  (electric-indent-local-mode -1)
  (visual-line-mode 1)

  (local-set-key (kbd "C-s-u") 'ej/hydra-org/body)
  (add-hook 'text-scale-mode-hook 'update-org-latex-fragments nil t)
  )
(add-hook 'org-mode-hook 'ej/org-hook)

