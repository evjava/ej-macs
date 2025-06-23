(use-package nav-nav
  :after (hydra)
  :load-path "non-elpa"
  :config
  (setq nav-nav-is-switch-layout t)
  (setq nav-nav-file nav-items-file)
  (global-set-key (kbd "s-s") 'nav-nav))
  
