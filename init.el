;;;; ----- GC
(setq old-threshold gc-cons-threshold)
(setq gc-cons-threshold most-positive-fixnum)

;;;; ----- load-path
(add-to-list 'load-path (locate-user-emacs-file "elisp"))
(add-to-list 'load-path (locate-user-emacs-file "elpa"))
(add-to-list 'load-path (locate-user-emacs-file "non-elpa"))
(let ((default-directory (locate-user-emacs-file "elpa")))
  (when (file-exists-p default-directory)
    (normal-top-level-add-subdirs-to-load-path)))

;;;; ----- parts
(defun load-el (rel-path)
  (load-file (expand-file-name rel-path user-emacs-directory)))
(load-el "elisp/010--early.el")
(load-el "elisp/020--paths.el")
(load-el "elisp/030--nav-nav.el")
(load-el "elisp/040--helm-hydra.el")
(load-el "elisp/050--ui.el")
(load-el "elisp/060--interactive.el")
(load-el "elisp/070--dev.el")
(load-el "elisp/071--elisp.el")
(load-el "elisp/072--python.el")
(load-el "elisp/080--modules.el")
(load-el "elisp/090--functions.el")
(load-el "elisp/100--org.el")
(load-el "elisp/101--org-latex.el")

(when priv-conf-file (load-file priv-conf-file))

;;;; ----- GC
(setq gc-cons-threshold old-threshold)
;;;; ----- DONE
(message "Emacs started in %s" (emacs-init-time))
