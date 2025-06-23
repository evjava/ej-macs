;; helm
(use-package helm
  :bind (("s-h" . 'helm-command-prefix)
         ("M-x" . 'helm-M-x)
         ("<f3>" . 'helm-find-files)
         ("s-b" . 'helm-mini)
         ("C-x C-r" . 'helm-recentf)
         ("M-y" . 'helm-show-kill-ring)
         ("s-/" . helm-lisp-completion-at-point))
  :config
  (helm-mode 1)
  (global-unset-key (kbd "C-x c"))
  (setq 
   helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
   helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
   helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
   helm-scroll-amount                    20 ; scroll 8 lines other window using M-<next>/M-<prior>
   helm-ff-file-name-history-use-recentf t
   helm-echo-input-in-header-line t
   helm-autoresize-min-height 20
   helm-autoresize-max-height 40
   )

   (setq helm-recentf-fuzzy-match t
     helm-locate-fuzzy-match t
     helm-M-x-fuzzy-match t
     helm-buffers-fuzzy-matching t
     helm-semantic-fuzzy-match t
     helm-apropos-fuzzy-match t
     helm-imenu-fuzzy-match t
     helm-lisp-fuzzy-completion t
     helm-completion-in-region-fuzzy-match t)

  (when (executable-find "curl") (setq helm-google-suggest-use-curl-p t))
  (helm-autoresize-mode 1)

  (setq helm-mini-default-sources '(helm-source-buffers-list
                                  helm-source-recentf
                                  helm-source-bookmarks
                                  helm-source-buffer-not-found))
  (use-package helm-projectile :bind ("s-p h" . 'helm-projectile))
  (use-package helm-swoop :defer t :bind ("C-c C-g" . 'helm-swoop))
  (use-package helm-descbinds)
  (use-package helm-ls-git)
  (use-package helm-dash)
  (use-package helm-switch-shell :defer t)
  (use-package helm-system-packages :defer t)
  (use-package helm-org-rifle :defer t)
  (use-package helm-rg :defer t)
  )
(require 'helm)

;; hydra
(use-package hydra)
(use-package pretty-hydra)
