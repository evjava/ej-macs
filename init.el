;;;; ----- base
(setq gc-cons-threshold (* 4000 1024 1024))
(defun ej/emacs-path (name) 
  (expand-file-name name user-emacs-directory))

;;;; ----- packages
(require 'package)
(package-initialize)
(let ((repos '(("org" . "https://orgmode.org/elpa/")
               ("melpa" . "http://melpa.org/packages/")
               ("melpa-stable" . "http://stable.melpa.org/packages/")
               )))
  (mapc (lambda (repo) (add-to-list 'package-archives repo)) repos))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile 
  (require 'use-package))
(setq use-package-always-ensure t
      use-package-verbose t
      package-enable-at-startup t)
(use-package session
  :config
  (add-hook 'after-init-hook 'session-initialize)
  (session-initialize)
  (savehist-mode 1)
)

;;;; ----- custom
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;;;; ----- env
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;;;; ----- load-path
(add-to-list 'load-path (ej/emacs-path "elisp"))
(add-to-list 'load-path (ej/emacs-path "elpa"))
(add-to-list 'load-path (ej/emacs-path "non-elpa"))
(let ((default-directory (ej/emacs-path "elpa")))
  (normal-top-level-add-subdirs-to-load-path))

;;;; ----- ej-macs.org
(require 'org)
(defun ej/load-config (name)
  "Load the Emacs Lisp file tangled from an Org file called NAME
   and placed in the user emacs directory, if that tangled code is
   still actual.  Otherwise, resort to `org-babel-load-file'."
  (let* ((org-file (expand-file-name name user-emacs-directory))
         (elisp-file (concat (file-name-sans-extension org-file) ".el")))
    (when (file-exists-p org-file)
      (if (file-newer-than-file-p elisp-file org-file)
          (load-file elisp-file)
        (org-babel-load-file org-file)))))

(ej/load-config "elisp/ej-macs.org")

