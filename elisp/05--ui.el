;; modes
(global-whitespace-mode -1)
(global-hl-line-mode -1) ;; highlight: подсветка строки с курсором, (hl-line-mode 1)
(transient-mark-mode -1) ; отключение выделение текста между меткой и точкой
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; font
(set-face-attribute 'default nil :font (font-spec :family "Monospace" :size 27))

;; themes
;; https://emacs.stackexchange.com/questions/24088/make-a-function-to-toggle-themes
(setq *ej/theme-dark* 'tron-legacy)
(setq *ej/theme-light* 'leuven)
(setq *ej/current-theme* nil)
(setq *ej/theme-location* last-theme-file)

(defun ej/set-dark-theme ()
  (interactive)
  (message "setting dark theme...")
	(use-package tron-legacy-theme
    :custom
    (tron-legacy-theme-softer-bg t)
		:config
		(load-theme 'tron-legacy t)
    (set-face-attribute 'helm-selection nil
                        :background "#3d5666" :foreground "white"))
  )

(defun ej/set-theme-dired-subtree ()
  (let* ((color-code-start (if (eq *ej/current-theme* *ej/theme-dark*) 0 255))
         (color-code-shift (if (eq *ej/current-theme* *ej/theme-dark*) 10 -10)))
    (cl-loop
     with cur-shift = color-code-start
     for ii from 1 to 6
     do (setq cur-shift (+ cur-shift color-code-shift))
     for cs-hex = (format "%02x" cur-shift)
     for rgb = (concat "#" (s-repeat 3 cs-hex))
     for attr = (intern (format "dired-subtree-depth-%d-face" ii))
     do (set-face-attribute attr nil :background rgb))))
  
(defun ej/set-light-theme ()
  (interactive)
  (message "setting light theme...")
  (load-theme 'leuven t))
;; disable other themes before loading new one
(defadvice load-theme (before theme-dont-propagate activate)
  "Disable theme before loading new one."
  (mapc #'disable-theme custom-enabled-themes))

(defun ej/set-theme (theme)
  (if (eq theme *ej/theme-dark*)
      (ej/set-dark-theme)
    (ej/set-light-theme))
  (setq *ej/current-theme* theme)
  (ej/write-string-to-file (format "%s" theme) *ej/theme-location*)
  (ej/sync-cache-dir)
)
(defun ej/swap-cache-dir ()
  " returns 'dark or 'light  "
  (let* ((imgs-dir (ej/emacs-local-path "ltximg"))
         (dark-dir (ej/emacs-local-path "ltximg_dark"))
         (light-dir (ej/emacs-local-path "ltximg_light"))
         (is-dark (file-exists-p light-dir))
         (is-light (file-exists-p dark-dir)))
    (if (or is-dark is-light)
        (if is-dark
            (progn
              (rename-file imgs-dir dark-dir)
              (rename-file light-dir imgs-dir)
              'light)
          (progn
            (rename-file imgs-dir light-dir)
            (rename-file dark-dir imgs-dir)
            'dark))
      (progn
        (make-directory-ignore-error light-dir)
        (make-directory-ignore-error imgs-dir)
        'dark))))

(defun ej/sync-cache-dir ()
  (cl-flet ((sync-once ()
              (eq (equal (ej/swap-cache-dir) 'dark)
                  (eq *ej/current-theme* *ej/theme-dark*))))
    (cl-loop until (sync-once))))

(defun ej/toggle-theme ()
  (interactive)
  (let ((theme (if (eq *ej/current-theme* *ej/theme-dark*) 
                   *ej/theme-light*
                 *ej/theme-dark*)))
    (ej/set-theme theme)))
;; (ej/toggle-theme)

(defun load-theme-on-start ()
	(let ((theme (if (not (file-exists-p *ej/theme-location*))
									 *ej/theme-dark*
								 (read (get-string-from-file *ej/theme-location*)))))
		(ej/set-theme theme)))
(load-theme-on-start)
