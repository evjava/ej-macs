;;;; ----- base
(defun ej/user-dir (dir)
  (let ((cmd (concat "xdg-user-dir " dir)))
    (s-trim (shell-command-to-string cmd))))
(setq downloads-dir (ej/user-dir "DOWNLOAD"))
(setq music-dir (ej/user-dir "MUSIC"))

(setq org-html-export-css-file (locate-user-emacs-file "resources/style.css"))
(setq init-conf-file (locate-user-emacs-file "init.el"))
(setq cstm-conf-file (locate-user-emacs-file "custom.el"))
(defun ej/config-opener (&optional arg)
  (interactive "p")
  (let* ((f (cond ((eq arg 4)  init-conf-file)
                  ((eq arg 16) priv-conf-file)
                  ((eq arg 64) cstm-conf-file)
                  (t           init-conf-file))))
    (find-file f)))
(global-set-key (kbd "<S-f3>") 'ej/config-opener)

;;;; ----- local
(setq emacs-local-dir (locate-user-emacs-file ".local"))
(unless (file-exists-p emacs-local-dir)
  (make-directory emacs-local-dir))
(defun ej/emacs-local-path (name) 
  (expand-file-name name emacs-local-dir))

(setq last-theme-file (ej/emacs-local-path "last-theme.cfg"))
(setq emms-dir (ej/emacs-local-path "emms"))
(setq projectile-cache-file (ej/emacs-local-path "projectile.cache"))
(setq bookmark-default-file (ej/emacs-local-path "emacs_bookmarks"))
(setq recentf-save-file (ej/emacs-local-path "recentf"))
(setq org-latex-preview-dir (ej/emacs-local-path "ltximg"))

;;;; ----- external
(defvar bibliography-dir nil  "bibliography for org-ref")
(defvar python-dir "/usr/bin/python3" "python dir path")
(defvar telega-path nil "telega path")
(defvar telega-contrib-path nil "telega path contrib")
(defvar org-roam-dir nil "Org Roam directory")
(defvar priv-conf-file nil "Private config path")
(defvar nav-nav-dir nil "nav-nav path")
(defvar iove-dir nil "iove path")
(defvar nav-items-file nil "path to nav-nav-file")
(defvar latex-headers-file nil "latex headers file for completions")
(defvar asmtools-jar-path nil "path to amstools.jar")

(setq private-emacs-config-dir "~/.emacs.d.private")
(load-ignore-error (expand-file-name "private-paths.el" private-emacs-config-dir))
