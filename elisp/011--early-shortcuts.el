(global-set-key (kbd "<f2>") 'save-buffer)
(global-set-key (kbd "<f1><f2>") 'save-buffer)
(global-set-key (kbd "<f3>") 'find-file)
(global-set-key (kbd "<ESC><f3>") 'sudo-edit)
(global-set-key (kbd "C-x b") 'ido-switch-buffer)
(global-set-key (kbd "<f5>") 'calc-grab-region)
(global-set-key (kbd "<f6>") 'package-install)
(global-set-key (kbd "<C-f6>") 'list-packages)
;; workaround for emacs-30
(global-set-key (kbd "<f8>") '(lambda () (interactive) (kill-this-buffer)))
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
(global-set-key (kbd "s-r") 'kill-rectangle)
(global-set-key (kbd "s-y") 'yank-rectangle)
(global-set-key (kbd "C-x s-r") 'string-insert-rectangle)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "<ESC> M-%") 'query-replace-regexp)

(global-set-key (kbd "<ESC><f5>") (lambda () (interactive) (revert-buffer t t)))
(global-set-key (kbd "C-x s-g") (lambda () (interactive) (revert-buffer t t)))

(global-set-key (kbd "s-i") 'dired-jump)
(global-set-key (kbd "C-c s-a") 'ej/copy-all)

(global-set-key (kbd "s-g") 'find-file-at-point)
(global-set-key (kbd "C-s-d") 'shell-command-on-buffer)

(global-set-key (kbd "M-SPC") (lambda () (interactive)))
(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "C-s-f") 'ej/copy-fname-to-clipboard)
(global-set-key (kbd "C-x s-i") 'indent-relative)
(global-set-key (kbd "C-x s-w") 'ido-switch-buffer)

;;;; ----- locations
(global-set-key (kbd "M-s-s") (lambda () (interactive) (switch-to-buffer "*scratch*")))
(global-set-key (kbd "s-`") (lambda () (interactive) (switch-to-buffer "*trash-buffer*")))
(global-set-key (kbd "s-ё") (lambda () (interactive) (switch-to-buffer "*trash-buffer*")))

;; ej/..
(global-set-key (kbd "C-`") 'ej/tab-to-previous-buffer)
(global-set-key (kbd "C-c C-M-r") 'ej/restart-emacs)
