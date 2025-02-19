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
(load-el "elisp/01--early.el")
(load-el "elisp/02--paths.el")
(load-el "elisp/03--nav-nav.el")
(load-el "elisp/04--helm-hydra.el")
(load-el "elisp/05--ui.el")
(load-el "elisp/06--interactive.el")
(load-el "elisp/07--dev.el")
(load-el "elisp/08--modules.el")
(load-el "elisp/09--functions.el")
(load-el "elisp/100--org.el")
(when priv-conf-file (load-file priv-conf-file))

;;;; ----- GC
(setq gc-cons-threshold old-threshold)
;;;; ----- DONE
(message "Emacs started in %s" (emacs-init-time))
