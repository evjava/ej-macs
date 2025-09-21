;;;; ----- packages
(require 'package)
(package-initialize)
(let ((repos '(("melpa" . "http://melpa.org/packages/")
               ("melpa-stable" . "http://stable.melpa.org/packages/")
               ("gnu-elpa" . "https://elpa.gnu.org/packages/")
               )))
  (mapc (lambda (repo) (add-to-list 'package-archives repo)) repos))

;;;; ----- use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t
      use-package-verbose t
      package-enable-at-startup t)

;;;; ----- custom
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)
(setq x-select-enable-clipboard-manager nil)

;;;; ----- quelpa
(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))
(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)

;;;; ----- early packages
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))
(use-package cl-lib)
(use-package s)
(use-package quick-yes :load-path "non-elpa")
(use-package multiple-cursors
  :defer t
  :bind
  ; multiple-cursors
  ("C-S-c C-S-c" . 'mc/edit-lines)
  ("C-S-c C-S-a" . 'mc/vertical-align-with-space)
  ("C->" . 'mc/mark-next-like-this)
  ("C-<" . 'mc/mark-previous-like-this)
  ("C-c C-<" . 'mc/mark-all-like-this)
  :custom
  (mc--read-char nil)
  (mc--read-quoted-char nil)
  )

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
(setq x-select-enable-clipboard-manager nil)

;;;; ----- behavior
;; remove the prompt for killing emacsclient buffers
(setq use-short-answers t) ; ask emacs be laconic and brief
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)
(put 'narrow-to-region 'disabled nil)

;;;; ----- server
(server-start)

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
(setq
 default-buffer-file-coding-system 'utf-8
 default-file-name-coding-system 'utf-8
 selection-coding-system 'utf-8
 coding-system-for-read 'utf-8
 coding-system-for-write 'utf-8)

;; https://stackoverflow.com/questions/2706527/make-emacs-stop-asking-active-processes-exist-kill-them-and-exit-anyway
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (cl-letf (((symbol-function #'process-list) (lambda ())))
    ad-do-it))
