(defun reverse-im-translate-region-2 (start end)
  " wrapper for reverse-im-translate-region "
  (interactive "r")
  (reverse-im-translate-region start end t))

(use-package reverse-im
 :ensure t
 :bind
 ("<f12>" . #'reverse-im-translate-region-2)
 :config
 (reverse-im-activate "russian-computer"))

(use-package desktop
  :config
  (desktop-save-mode 1)
  (setq desktop-path (list emacs-local-dir))
  (setq desktop-save t)
  (setq desktop-save-mode t)
  (setq desktop-load-locked-desktop t)
  :hook
  (after-init . desktop-read)
  (after-init . desktop-save-mode)
)

(use-package session
  :config
  (add-hook 'after-init-hook 'session-initialize)
  (session-initialize)
  (savehist-mode 1))

(use-package doom-modeline
  :ensure t
  :init 
  (setq doom-modeline-height 4)
  (doom-modeline-mode 1)
)
(use-package nerd-icons
  :if (display-graphic-p)
  :config
  (let* ((nerd-fonts-path (expand-file-name "~/.local/share/fonts/NFM.ttf")))
    (unless (file-exists-p nerd-fonts-path)
      (nerd-icons-install-fonts 'yes))))

(use-package multiple-cursors
  :defer t
  :bind
  ; multiple-cursors
  ("C-S-c C-S-c" . 'mc/edit-lines)
  ("C-S-c C-S-a" . 'mc/vertical-align-with-space)
  ("C->" . 'mc/mark-next-like-this)
  ("C-<" . 'mc/mark-previous-like-this)
  ("C-c C-<" . 'mc/mark-all-like-this)
  )

(use-package visual-regexp
  :bind
  ("C-c r" . 'vr/replace)
  ("C-c q" . 'vr/query-replace)
  ("C-c m" . 'vr/mc-mark))

(use-package projectile
  :ensure t
  :pin melpa-stable
  :bind (:map projectile-mode-map
              ("s-p" . 'projectile-command-map)
              ("s-з" . 'projectile-command-map)
              ("C-c p" . 'projectile-command-map))
  :config
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'native)
  (add-to-list 'projectile-globally-ignored-directories "^postgres-data$")
  (projectile-mode +1))

(use-package dired-subtree
  :bind
  (:map dired-mode-map ("i" . 'dired-subtree-toggle))
  :config
  (ej/set-theme-dired-subtree))

(use-package yafolding
  :defer t
  :bind
  ("<C-S-return>" . nil)
  ("<C-M-return>" . nil)
  ("<C-return>" . nil)
  ("C-c <C-M-return>" . 'yafolding-toggle-all)
  ("C-c <C-S-return>" . 'yafolding-hide-parent-element)

  ("C-c <C-return>" . 'yafolding-toggle-element)
  )

(use-package recentf
  :init
  (recentf-mode 1)
  (setq recentf-max-menu-items 400)
  (setq recentf-max-saved-items 400)
)

(use-package which-key
  :config
  (setq which-key-show-early-on-C-h t)
  (setq which-key-idle-delay 0.5)
  (setq which-key-idle-secondary-delay 0.05)
  (which-key-mode))

(use-package helpful
  :bind (("C-h f"   . #'helpful-callable)
         ("C-h v"   . #'helpful-variable)
         ("C-h k"   . #'helpful-key)
         ("C-c C-d" . #'helpful-at-point)
         ("C-h F"   . #'helpful-function)
         ("C-h C"   . #'helpful-command) ;; describe-coding-system >> interactive-functions 
         ))

(use-package kotlin-mode
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\.kt$" . kotlin-mode))
  )

(use-package google-translate
  :ensure t
  :bind (("s-f" . ej/translate-yank))
  :config
  (require 'google-translate-default-ui)
  (setq google-translate-backend-method 'curl)
  (defun google-translate--get-b-d1 ()
    " fix from https://github.com/atykhonov/google-translate/issues/52 "
    ;; TKK='427110.1469889687'
    (list 427110 1469889687))
  ;; hotfix from https://github.com/atykhonov/google-translate/issues/98
  (defun google-translate-json-suggestion (jj)
    (let ((info (aref jj 7)))
      (if (and info (> (length info) 0))
          (aref info 1)
        nil)))

  (setq google-translate-default-source-language "en")
  (setq google-translate-default-target-language "ru")
  )
(require 'google-translate)

(defun ej/translate-yank (start end)
  " Translates region. en>ru if a..z found, else ru>en "
  (interactive "r")
  (let* ((text (buffer-substring-no-properties start end))
         (source (if (s-match "[a-zA-Z]" text) "en" "ru"))
         (target (if (equal "en" source) "ru" "en")))
    (google-translate-translate source target text)
    (other-window 1)))

(defun open-emms-or-play-directory-tree-if-empty (&optional directory)
  (interactive 
   (if emms-playlist-mode-open-playlists nil 
     ;; copy from emms-source-file.el
     (list
      (emms-read-directory-name "Play directory tree: "
                                emms-source-file-default-directory
                                emms-source-file-default-directory
                                t)))))

(use-package emms
  :defer t
  :config
  (setq emms-directory emms-dir
        emms-player-list '(emms-player-vlc emms-player-vlc-playlist emms-player-mpg321 emms-player-ogg123 emms-player-mplayer-playlist emms-player-mplayer)
        emms-playlist-buffer-name "*Music*"
        emms-source-file-default-directory music-dir)
  (require 'emms-setup)
  (emms-all)
  (emms-default-players)
  (require 'emms-info-libtag)
  (setq emms-info-functions '(emms-info-libtag))
  ;; todo maybe add emms-from-youtube after https://www.emacswiki.org/emacs/EMMS#toc14
  ;; todo streams after
  ;; read from here: https://www.gnu.org/software/emms/manual/#User-Variables
  :bind (
         ;; ("<f11>" . 'emms)
         ("S-<f11>" . 'emms-play-directory-tree)
         ("C-<f8>" . 'emms-pause)
         ("C-<f9>" . 'emms-previous)
         ("C-<f10>" . 'emms-next)
         ("C-S-<f9>" . 'emms-seek-backward)
         ("C-S-<f10>" . 'emms-seek-forward)
         ("C-s-e" . 'emms)))

;; (ignore-errors 
;;   (use-package saveplace
;;           :custom (ej/emacs-local-path "places")
;;           :config (save-place-mode 1)))

(use-package pdf-tools
  :ensure t
  :config
  (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo")
  (add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))
  (setq-default pdf-view-display-size 'fit-page)

  ;; (use-package saveplace-pdf-view)
  ;; (save-place-mode 1)
  (add-to-list 'debug-ignored-errors "No such page")
  (message "pdf-tools configured")
  (add-hook 'pdf-view-mode-hook 'pdf-isearch-minor-mode)
  :bind
  (:map pdf-view-mode-map ("C-s" . isearch-forward))
  (:map pdf-view-mode-map ("C-s-a" . pdf-annot-add-highlight-markup-annotation))
  (:map pdf-view-mode-map ("C-v" . pdf-view-scroll-up-or-next-page))
  (:map pdf-view-mode-map ("M-v" . pdf-view-scroll-down-or-previous-page))
)
;; loading pdf-tools only on first pdf open
(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-tools-install))

(defun ej/hook-yaml ()
  (outline-minor-mode)
  (local-set-key (kbd "C-s-c") #'outline-cycle)
  (local-set-key (kbd "C-S-s-c") #'outline-cycle-buffer)
  (setq outline-regexp "^\s*- name: "))

(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode))
  (add-hook 'yaml-mode-hook 'ej/hook-yaml))

(use-package telega
  :defer t
  :load-path telega-path
  :commands (telega)
  :bind-keymap ("C-c t" . telega-prefix-map)
  :custom
  (telega-use-docker t)
  (telega-chat-input-markups '("markdown2" nil "markdown1")))

(use-package telega-mnz
  :defer t
  :after telega
  :load-path telega-contrib-path
  :config
  (add-hook 'telega-load-hook 'global-telega-mnz-mode)
  :custom
  (global-telega-mnz-mode t))

(use-package emojify
  :defer t
  ;; :hook (after-init . global-emojify-mode)
  :config
  (setq emojify-emojis-dir (ej/emacs-local-path "emojis")) 
  :init
)
;; (use-package company
;;   :init
;;   (company-mode)
;; )
(setq telega-emoji-company-backend 'telega-company-emoji)

(defun ej/telega-chat-mode-emoji ()
  (set (make-local-variable 'company-backends)
       (append (list telega-emoji-company-backend
                   'telega-company-username
                   'telega-company-hashtag)
             (when (telega-chat-bot-p telega-chatbuf--chat)
               '(telega-company-botcmd))))
  (company-mode 1)
  (emojify-mode 1))

(add-hook 'telega-chat-mode-hook 'ej/telega-chat-mode-emoji)

(use-package avy
  :defer t
  :bind (("C-;" . 'avy-goto-char-timer)))

(use-package winner
  :defer t
  :custom
  (winner-mode t))

(use-package erefactor
  :defer t
  :config
  (define-key emacs-lisp-mode-map (kbd "<S-f6>") 'erefactor-rename-symbol-in-buffer)
  ;; :bind (:map emacs-lisp-mode-map ("<S-f6>" . erefactor-rename-symbol-in-buffer)))
  (define-key emacs-lisp-mode-map "\C-c\C-v" erefactor-map))

(use-package dumb-jump)

(use-package keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
  (setq keyfreq-excluded-commands
        '(org-self-insert-command self-insert-command next-line previous-line isearch-printing-char
                                  backward-word forward-word forward-char backward-char other-window
                                  save-buffer move-end-of-line org-delete-backward-char set-mark-command
                                  isearch-forward forward-sexp dired-next-line scroll-up-command org-cycle
                                  dired-previous-line backward-delete-char-untabify move-beginning-of-line
                                  mwheel-scroll ignore))
  )

(use-package trashed :defer t)
(use-package diff-hl :defer t)

(defun ej/configure-iove ()
  )

(if (ej/file-exists-p iove-dir)
    (use-package iove
      :requires python
      :load-path iove-dir
      :config (ej/configure-iove))
  (message "Can not load `iove`")
  )
(global-set-key (kbd "C-M-S-<return>") 'iove/annotate)
