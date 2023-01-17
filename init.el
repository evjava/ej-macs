;;;; ----- GC
;; (setq gc-cons-threshold (* 4000 1024 1024))
(setq old-threshold gc-cons-threshold)
(setq gc-cons-threshold most-positive-fixnum)

;;;; ----- packages
(require 'package)
(package-initialize)
(let ((repos '(("melpa" . "http://melpa.org/packages/")
               ("melpa-stable" . "http://stable.melpa.org/packages/"))))
  (mapc (lambda (repo) (add-to-list 'package-archives repo)) repos))

;;;; ----- use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t
      use-package-verbose t
      package-enable-at-startup t)

;;;; ----- load-path
(add-to-list 'load-path (locate-user-emacs-file "elisp"))
(add-to-list 'load-path (locate-user-emacs-file "elpa"))
(add-to-list 'load-path (locate-user-emacs-file "non-elpa"))
(let ((default-directory (locate-user-emacs-file "elpa")))
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

(setq gc-cons-threshold old-threshold)
(message "Emacs started in %s" (emacs-init-time))
