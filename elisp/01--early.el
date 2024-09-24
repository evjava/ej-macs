;;;; ----- packages
(require 'package)
(package-initialize)
(let ((repos '(("melpa" . "http://melpa.org/packages/")
               ("melpa-stable" . "http://stable.melpa.org/packages/"))))
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

;;;; ----- early packages
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))
(use-package cl-lib)
(use-package s)
(use-package quick-yes :load-path "non-elpa")

;;;; ----- variables
(defvar mc--read-char nil)
(defvar mc--read-quoted-char nil)
(defvar rectangular-region-mode nil)
(defvar regexp-string nil)
(defvar replace-string nil)
(put 'narrow-to-page 'disabled nil)
(setq-default auto-save-defaults t)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(setq 
 frame-title-format 
 '((:eval 
    (concat 
     (if (buffer-file-name) (abbreviate-file-name (buffer-file-name)) "%b")
     " - emacs@"
     (system-name)))))

(defconst debian-emacs-flavor 'emacs26
  "A symbol representing the particular debian flavor of emacs running.
 Something like 'emacs20, 'xemacs20, etc.")

(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

;;;; ----- old emacs behavior
(add-to-list 'display-buffer-alist '("^\\*shell\\*.*$" . (display-buffer-same-window)))

;;;; ----- behavior
;; remove the prompt for killing emacsclient buffers
(setq use-short-answers t) ; ask emacs be laconic and brief
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)

(put 'narrow-to-region 'disabled nil)

;; (setq interprogram-paste-function 'x-selection-value)
(defalias 'x-cut-buffer-or-selection-value 'x-selection-value)

;;;; ----- early functions
(defun load-ignore-error (file)
  (condition-case nil
      (load file)
    (error (message "Failed to load: %s" file))))

(defun load-org-ignore-error (org-file)
  (condition-case nil (org-babel-load-file org-file) (error nil)))

(defun make-directory-ignore-error (dir)
  (condition-case nil (make-directory dir) (error nil)))

(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun ej/write-string-to-file (string file)
  (interactive "sEnter the string: \nFFile to save to: ")
  (with-temp-file file
    (insert string)))

(defun launch-separate-emacs-in-terminal ()
  (suspend-emacs "fg ; emacs -nw"))
(defun launch-separate-emacs-under-x ()
  (call-process "sh" nil nil nil "-c" "emacs &"))

(defun ej/file-exists-p (file)
  (and (not (null file)) (file-exists-p file)))

(defun ej/restart-emacs ()
  (interactive)
  (if (yes-or-no-p "Restart emacs?")
      (let ((kill-emacs-hook (append 
                              kill-emacs-hook 
                              (list (if (display-graphic-p)
                                        #'launch-separate-emacs-under-x
                                      #'launch-separate-emacs-in-terminal)))))
        (save-buffers-kill-emacs))))
(global-set-key (kbd "C-c C-M-r") 'ej/restart-emacs)

;;;; ----- server
(server-start)
(defun ej/restart-server()
  (interactive)
  (server-force-delete)
  (server-start))

;;;; ----- early shortcuts
(global-set-key (kbd "<f2>") 'save-buffer)
(global-set-key (kbd "<f1><f2>") 'save-buffer)
(global-set-key (kbd "<f3>") 'find-file)
(global-set-key (kbd "<ESC><f3>") 'sudo-edit)
(global-set-key (kbd "C-x b") 'ido-switch-buffer)
(global-set-key (kbd "<f5>") 'calc-grab-region)
(global-set-key (kbd "<f6>") 'package-install)
(global-set-key (kbd "<C-f6>") 'list-packages)
(global-set-key (kbd "<f8>") 'kill-this-buffer)
(global-set-key (kbd "<f9>") 'sort-lines)
(global-set-key (kbd "M-j") 'dabbrev-expand)

(global-set-key (kbd "<C-kp-5>") 'replace-string)
(global-set-key (kbd "<ESC><C-kp-5>") 'query-replace)
(global-set-key (kbd "<C-kp-6>") 'replace-regexp)
(global-set-key (kbd "<ESC><C-kp-6>") 'query-replace-regexp)

(global-set-key (kbd "s-a") 'replace-string)
(global-set-key (kbd "M-s-a") 'replace-regexp)

;; modes
(global-set-key (kbd "<C-kp-1>") 'shell-mode)
(global-set-key (kbd "<C-kp-4>") 'emacs-lisp-mode)
(global-set-key (kbd "<C-kp-7>") 'text-mode)
(global-set-key (kbd "<C-kp-8>") 'org-mode)
(global-set-key (kbd "<C-kp-9>") 'python-mode)

(global-set-key (kbd "s-t") 'toggle-word-wrap)

(global-set-key (kbd "C-z") nil) ;; nil

;; macroses
(global-set-key (kbd "<C-f3>") 'kmacro-start-macro-or-insert-counter)
(global-set-key (kbd "<C-f4>") 'kmacro-end-macro)
(global-set-key (kbd "<C-f5>") 'kmacro-end-and-call-macro)
(global-set-key (kbd "M-n") 'kmacro-start-macro-or-insert-counter)
(global-set-key (kbd "M-o") 'kmacro-end-and-call-macro)

;; other-window
(global-set-key (kbd "<s-tab>") 'other-window)
(global-set-key (kbd "C-x o") 'other-window)
(global-set-key (kbd "<S-s-iso-lefttab>") (lambda () (interactive) (other-window -1)))
;; splits
(global-set-key (kbd "s-q") 'delete-other-windows)
(global-set-key (kbd "s-w") 'split-window-vertically)
(global-set-key (kbd "s-e") 'split-window-horizontally)

;; switch to near buffer
(defun ej/tab-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer))))
(global-set-key (kbd "C-`") 'ej/tab-to-previous-buffer)

;; delete matching/non-matching lines
(global-set-key (kbd "C-x m") 'delete-matching-lines)
(global-set-key (kbd "C-x M") 'delete-non-matching-lines)

;; run last command
(global-set-key (kbd "<C-f1>") 'ej/run-last-command)

;; buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; editor options
(global-set-key (kbd "s-k") 'ej/kill-line)
(global-set-key (kbd "s-M-k") 'ej/save-kill-line)
(global-set-key (kbd "C-s-k") 'ej/save-kill-line2)
(global-set-key (kbd "s-u") 'ej/duplicate-line)
(global-set-key (kbd "s-r") 'ej/kill-rectangle)
(global-set-key (kbd "s-y") 'ej/insert-rectangle)
(global-set-key (kbd "C-x s-r") 'string-insert-rectangle)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "<ESC> M-%") 'query-replace-regexp)

(global-set-key (kbd "<ESC><f5>") (lambda () (interactive) (revert-buffer t t)))
(global-set-key (kbd "C-x s-g") (lambda () (interactive) (revert-buffer t t)))

(fset 'ej/open-current-directory [f3 ?\C-d])
(global-set-key (kbd "s-i") 'dired-jump)

(global-set-key (kbd "C-c s-a") 'ej/copy-all)

(fset 'ej/remove-previous-command
   [?\C-b ?\C-a ?\C-  ?\C-c ?\C-p ?\C-n ?\C-a ?\C-w ?\C-o ?. ?. ?. ?\C-f ?\C-e])
(global-set-key (kbd "C-c s-d") 'ej/remove-previous-command)

(global-set-key (kbd "s-c") 'ej/calculator-mode)

(global-set-key (kbd "C-S-s-d") 'ej/go-to-dir-reflexia)
(global-set-key (kbd "s-g") 'find-file-at-point)
(global-set-key (kbd "C-s-d") 'shell-command-on-buffer)

(global-set-key (kbd "M-SPC") (lambda () (interactive)))
(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "C-s-f") 'ej/copy-fname-to-clipboard)
(global-set-key (kbd "C-x s-i") 'indent-relative)
(global-set-key (kbd "C-x s-w") 'ido-switch-buffer)

;;;; ----- locations
(defun ej/open-edownloads (&optional file-idx)
  (interactive "p")
  (find-file downloads-dir)
  (ej/select-nth-file file-idx))

(global-set-key (kbd "C-S-e") 'ej/open-edownloads)
(global-set-key (kbd "C-S-c e") 'ej/select-nth-file)

(defun ej/key-to-buffer (key buffer-name)
  (global-set-key key `(lambda () (interactive) (switch-to-buffer ,buffer-name))))

(ej/key-to-buffer (kbd "M-s-s") "*scratch*")
(ej/key-to-buffer (kbd "s-`") "*trash-buffer*")
(ej/key-to-buffer (kbd "s-ё") "*trash-buffer*")

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

;;;; ----- dired
(setq dired-recursive-copies (quote always))
(setq dired-dwim-target t) 
; call split-window-vertically, then go to another dired dir. Now, when you press C to copy, the other dir in the split pane will be default destination. Same for R (rename; move).
(require 'dired-x)
(put 'dired-find-alternate-file 'disabled nil)
(require 'ls-lisp)
(setq ls-lisp-use-insert-directory-program nil)
(setq dired-listing-switches "-aD")
(setq dired-omit-files (concat dired-omit-files "\\|\\.i$"))
(setq tramp-default-method "ssh")

(defun ej/hook-dired-dd-loader ()
  (load "dired-x")
  (when window-system
    (require 'dired-dd)
    (require 'dired-dd-mew)
    (require 'dired-dd-insert-fname)
    (require 'dired-dd-insert-file)))

(add-hook 'dired-load-hook 'ej/hook-dired-dd-loader)

(defun ej/hook-remote-switches ()
  (when (file-remote-p default-directory)
    (setq dired-actual-switches "-al")))

(add-hook 'dired-before-readin-hook 'ej/hook-remote-switches)

;; dired: async
(use-package async
  :config
  (dired-async-mode 0))
(defadvice load-theme (after run-after-load-theme-hook activate)
  ;; load-theme for some weird reason changes dired-async-mode
  (setq dired-async-mode nil))
;; (debug-on-variable-change 'dired-async-mode)

;; dired: ext
(defun ej/open-in-external-app (fpath)
  (pcase system-type
    ('gnu/linux  (let ((process-connection-type nil)) (start-process "" nil "xdg-open" fpath)))
    ('windows-nt (w32-shell-execute "open" (s-replace "/" "\\" fpath t t)))
    ('darwin     (let ((process-connection-type nil)) (start-process "" nil "open" fpath)))
    (_ (message "Unrecognized system-type: %s" system-type))))

(defun ej/open-in-external-app-dired ()
  " Open the current file or dired marked files in external app. 
    Works in Microsoft Windows, Mac OS X, Linux. "
  (interactive)
  (let* ((files (cond
                 ((equal major-mode 'dired-mode) (dired-get-marked-files))
                 (t (list (buffer-file-name)))))
         (accept (or
                  (<= (length files) 5)
                  (y-or-n-p "Open more than 5 files?"))))
    (when accept
      (--map (ej/open-in-external-app it) files))))

(defun ej/open-in-external-app-at-point ()
  (interactive)
  (let* ((fpath (thing-at-point 'filename)))
    (if (null fpath) (message "File not found: %s" fpath)
      (message "Going to open: %s" fpath)
      (ej/open-in-external-app fpath))))

(global-set-key (kbd "<f10>") 'ej/open-in-external-app-dired)
(global-set-key (kbd "S-<f10>") 'ej/open-in-external-app-at-point)

;; dired: add-date
(defun ej/dired-file-name-add-date ()
  " TODO fix: add query, add revert (maybe?), don't rename already renamed "
  (interactive)
  (let* ((full-name (dired-get-filename))
         (path-name (file-name-directory full-name))
         (short-name (file-name-nondirectory full-name))
         (file-attrs (file-attributes full-name))
         (dt (format-time-string "%Y-%m-%d" (nth 5 file-attrs)))
         (upd-short-name (format "%s--%s" dt short-name))
         (upd-full-name (s-concat path-name upd-short-name)))
    (message "Renamed %s >> %s" short-name upd-short-name)
    (rename-file full-name upd-full-name)
    (revert-buffer)
    (beginning-of-buffer)
    (search-forward upd-short-name)))
(define-key dired-mode-map (kbd "b") #'ej/dired-file-name-add-date)

;;;; ----- files: hooks
(defun ej/find-file-check-make-large-file-read-only-hook ()
  "If a file is over a given size, make the buffer read only."
  (cl-flet ((pdfp () (s-suffix-p ".pdf" (buffer-file-name))))
    (when (and (> (buffer-size) (* 10 1024 1024)) (not (pdfp)))
      (setq buffer-read-only t)
      (buffer-disable-undo)
      (fundamental-mode)
      )))
(add-hook 'find-file-hook 'ej/find-file-check-make-large-file-read-only-hook)
(defun ej/dired--find-file--no-djvu (proc ff-function file)
  (if (s-ends-with? ".djvu" file)
      (message "Don't open djvu-files from dired")
    (funcall proc ff-function file)))
(advice-add 'dired--find-file :around #'ej/dired--find-file--no-djvu)

;;;; ----- shell
;;; shell: pager for stdout
(use-package shell
  :config
  (setenv "PAGER" "cat")
  (setq system-uses-terminfo nil)
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
  (setq ansi-color-names-vector
        ["black" "tomato" "PaleGreen2" "gold1"
         "DeepSkyBlue1" "MediumOrchid1" "cyan" "white"]))

;;; shell: shortcuts
(setq ej/shell-cnt 6)
(defun ej/shell-1-or-else (&optional new-shell) 
  (interactive "P")
  (if (null new-shell) (shell "*shell*<1>")
    (let ((new-shell-buffer-name (format "*shell*<%d>" ej/shell-cnt)))
      (setq ej/shell-cnt (1+ ej/shell-cnt))
      (shell new-shell-buffer-name))))
(defun extract-int (str)
  (when (string-match "\\([0-9]+\\)" str)
    (string-to-number (match-string 1 str))))
(defun ej/get-shell-buffers ()
  (let* ((buffers (-map #'buffer-name (buffer-list)))
         (shell-buffers (--filter (string-match "\\*shell\\*<[0-9]+>$" it) buffers))
         ) shell-buffers))
(defun ej/switch-to-prev-shell ()
  (interactive)
  (-if-let (shell-buffers (ej/get-shell-buffers))
      (let* ((sorted-shell-buffers (-sort (-on #'< #'extract-int) shell-buffers))
             (len (length sorted-shell-buffers))
             (cur-buf (buffer-name))
             (idx-0 (-elem-index cur-buf sorted-shell-buffers))
             (idx (or idx-0 0))
             (next-idx (mod (+ len (1- idx)) len))
             (next-buf (elt sorted-shell-buffers next-idx)))
        (switch-to-buffer next-buf))
    (message "No *shell*<N> buffers found!")))
(defun rename-shell (new-shell-name)
  (interactive "senter new shell name: ")
  (rename-buffer (format "*shell*<%s>" new-shell-name)))

(defun ej/run-other-window ()
  (interactive)
  (save-buffer)
  (let* ((is-py (s-ends-with? ".py" (buffer-file-name))))
    (other-window 1)
    (if (not (equal major-mode 'shell-mode))
        (message "Other window isn't shell!")
      (goto-char (point-max))
      (while (ej/is-inside-program)
        (comint-send-eof)
        (sit-for 0.2))
      (if (not is-py) (comint-previous-input 1)
        (while (ej/is-not-interesting-command)
          (comint-previous-input 1)))
      (comint-send-input)
      (other-window -1))))

(global-set-key (kbd "s-n") 'ej/shell-1-or-else)
(global-set-key (kbd "s-m") (lambda () (interactive) (shell "*shell*<2>")))
(global-set-key (kbd "s-,") (lambda () (interactive) (shell "*shell*<3>")))
(global-set-key (kbd "s-.") (lambda () (interactive) (shell "*shell*<4>")))
(global-set-key (kbd "s-/") (lambda () (interactive) (shell "*shell*<5>")))
(global-set-key (kbd "M-s-n") 'ej/switch-to-prev-shell)
(global-set-key (kbd "s-j") 'ej/run-other-window)
(defun ej/bash-history ()
  (interactive)
  (helm
   :sources (helm-build-sync-source "Bash history"
              :candidates (seq-reverse (vc--read-lines "~/.bash_history"))
              :action #'insert
              :fuzzy-match t)
   :buffer "*Bash history*"))
(defun ej/rename-shell (new-shell-name)
  (interactive "senter new shell name: ")
  (rename-buffer (format "*shell*<%s>" new-shell-name)))
(define-key shell-mode-map (kbd "C-c s-r") #'ej/rename-shell)
(define-key shell-mode-map (kbd "M-s-r") #'ej/bash-history)
(define-key shell-mode-map (kbd "s-t") #'toggle-truncate-lines)

;; https://stackoverflow.com/questions/2706527/make-emacs-stop-asking-active-processes-exist-kill-them-and-exit-anyway
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (cl-letf (((symbol-function #'process-list) (lambda ())))
    ad-do-it))

;;; shell: completion
(use-package pcmpl-args
 :bind (:map shell-mode-map ("<tab>" . pcomplete)))
