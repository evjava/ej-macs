(setq gc-cons-threshold-old gc-cons-threshold gc-cons-threshold most-positive-fixnum) ;; gc trick
(dolist (dir '("elisp" "elpa" "non-elpa")) (add-to-list 'load-path (locate-user-emacs-file dir))) ;; load-path
(let ((default-directory (locate-user-emacs-file "elpa1")))
  (when (file-exists-p default-directory) (normal-top-level-add-subdirs-to-load-path)))

(defun load-el (rel-path) (load-file (expand-file-name rel-path user-emacs-directory)))
(load-el "elisp/010--early-functions.el")
(load-el "elisp/011--early-shortcuts.el")
(load-el "elisp/012--early-packages-and-server.el")
(load-el "elisp/013--early-vars.el")
(load-el "elisp/014--early-dired.el")
(load-el "elisp/015--early-shell.el")
(load-el "elisp/020--paths.el")
(load-el "elisp/030--nav-nav.el")
(load-el "elisp/040--helm-hydra.el")
(load-el "elisp/050--ui.el")
(load-el "elisp/060--interactive.el")
(load-el "elisp/070--dev.el")
(load-el "elisp/071--elisp.el")
(load-el "elisp/072--python.el")
(load-el "elisp/080--modules.el")
(load-el "elisp/081--modules--llm.el")
(load-el "elisp/090--functions.el")
(load-el "elisp/100--org.el")
(load-el "elisp/101--org-latex.el")
(when priv-conf-file (load-file priv-conf-file))

(setq gc-cons-threshold gc-cons-threshold-old)  ;; gc trick rollback
(message "Emacs started in %s" (emacs-init-time))
