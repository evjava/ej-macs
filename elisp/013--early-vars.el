;;;; ----- custom
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)
(setq x-select-enable-clipboard-manager nil)

;;;; ----- variables
(defvar rectangular-region-mode nil)
(defvar regexp-string nil)
(defvar replace-string nil)
(put 'narrow-to-page 'disabled nil)
(setq-default auto-save-defaults t)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(defun ej/user-host ()
  (let* ((user (s-trim (shell-command-to-string "echo $USER")))
         (host-pretty (s-trim (shell-command-to-string "hostnamectl --pretty")))
         (host (if (= 0 (length host-pretty)) (system-name) host-pretty)))
    (format "%s@%s" user host)))
(setq *user-host* (ej/user-host))
(setq frame-title-format '((:eval (concat *user-host* " " "%b"))))

;; ----- yanking
(defconst debian-emacs-flavor 'emacs26
  "A symbol representing the particular debian flavor of emacs running.
 Something like 'emacs20, 'xemacs20, etc.")
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
;; (setq interprogram-paste-function 'x-selection-value)
(defalias 'x-cut-buffer-or-selection-value 'x-selection-value)
(defalias 'incf 'cl-incf)
(setq x-select-enable-clipboard-manager nil)

;;;; ----- behavior
;; remove the prompt for killing emacsclient buffers
(setq use-short-answers t) ; ask emacs be laconic and brief
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)
(put 'narrow-to-region 'disabled nil)

;;;; ----- utf-8 everywhere
(set-language-environment "UTF-8")
(set-language-environment-coding-systems 'utf-8)
(set-language-environment-input-method 'utf-8)
(set-language-environment-nonascii-translation 'utf-8)
(set-language-environment-charset 'utf-8)
(set-language-environment-unibyte 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-display-table-and-terminal-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; settings these vars below breaks archive-mode...
;; (setq default-buffer-file-coding-system 'utf-8)
;; (setq default-file-name-coding-system 'utf-8)
;; (setq selection-coding-system 'utf-8)
;; (setq coding-system-for-read 'utf-8)
;; (setq coding-system-for-write 'utf-8)

;; https://stackoverflow.com/questions/2706527/make-emacs-stop-asking-active-processes-exist-kill-them-and-exit-anyway
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (cl-letf (((symbol-function #'process-list) (lambda ())))
    ad-do-it))
