(dolist (dir '("elisp" "elpa" "non-elpa"))
  (add-to-list 'load-path (locate-user-emacs-file dir))) ;; load-path

(let ((default-directory (locate-user-emacs-file "elpa")))
  (when (file-exists-p default-directory) (normal-top-level-add-subdirs-to-load-path)))
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

;;;; ----- server
(server-start)
