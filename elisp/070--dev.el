(defun ej/prog-mode-hook ()
  )
(add-hook 'prog-mode-hook 'ej/prog-mode-hook)

(use-package diff-hl
  :config
  (add-hook 'org-mode-hook 'diff-hl-mode)
  (add-hook 'prog-mode-hook 'diff-hl-mode))

(setq-default c-basic-offset 2 c-default-style "linux")
(setq-default tab-width 2 indent-tabs-mode nil)
;;; turn on syntax highlighting
(global-font-lock-mode 1)

(setq inferior-lisp-program "/usr/bin/clisp")

;; for bash
(add-to-list 'auto-mode-alist '("\\.bash_aliases$" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.bash_path$" . shell-script-mode))

(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))
(add-to-list 'auto-mode-alist '("\\.g4$" . antlr-mode))
(add-to-list 'auto-mode-alist '("\\..codespellrc" . conf-unix-mode))

;; for dabbrev-expand
(setq dabbrev-abbrev-skip-leading-regexp "[='/$]")

;;; use groovy-mode when file ends in .groovy or has #!/bin/groovy at start
(autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t)
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-to-list 'auto-mode-alist '("\.gant$" . groovy-mode))
(add-to-list 'auto-mode-alist '("\.gradle$" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))

;;; make Groovy mode electric by default.
(defun ej/hook-groovy ()
  (require 'groovy-electric)
  (groovy-electric-mode))
(add-hook 'groovy-mode-hook 'ej/hook-groovy)

(autoload 'js2-mode "js2" nil t)
; (require 'nodejs-repl)
(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))
(add-to-list 'auto-mode-alist '("\\.jsonl$" . js-mode))
(add-hook 'js2-mode-hook 'ac-js2-mode)
(setq js2-highlight-level 3)
(setq js-indent-level 2)

(defun ej/c-mode-common-hook ()
 " https://stackoverflow.com/questions/663588/emacs-c-mode-incorrect-indentation " 
 ;; my customizations for all of c-mode, c++-mode, objc-mode, java-mode
 (c-set-offset 'substatement-open 0)
 ;; other customizations can go here

 (setq c++-tab-always-indent t)
 (setq c-basic-offset 4)                  ;; Default is 2
 (setq c-indent-level 4)                  ;; Default is 2

 (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))
 (setq tab-width 4)
 (setq indent-tabs-mode t)  ; use spaces only if nil
 (local-set-key (kbd "C-s-i") #'ej/hydra-cpp/body)
 )

(add-hook 'c-mode-common-hook 'ej/c-mode-common-hook)

(defun ej/insert-dbg ()
  (interactive)
  (insert "std::cout << \"DBG: ")
  (save-excursion
    (insert "\" << std::endl;")))

(defun ej/toggle-comment-and-next-line ()
  (interactive)
  (back-to-indentation)
  (if (looking-at "// ")
      (delete-char 3)
    (insert "// "))
  (next-line 1))

(defhydra ej/hydra-cpp (:foreign-keys warn :columns 1 :exit t)
  " C++ helpers "
  ("i" ej/insert-dbg "insert debug")
  ("g" dumb-jump-go "jump-go")
  ("b" dumb-jump-back "jump-back")
  ("/" ej/toggle-comment-and-next-line "//" :exit nil)
  ("d" (search-forward "DBG") "next DBG" :exit nil)
  )

(defun ej/haskell-reload ()
  (interactive)
  (with-current-buffer "*shell*<1>"
    (end-of-buffer)
    (insert ":reload")
    (comint-send-input)))

(defun ej/haskell-find-first-error ()
  (interactive)
  (with-current-buffer "*shell*<1>"
    (search-backward ":reload")
    (search-forward "error:")
    (backward-sexp 3)
    (let* ((line (string-to-number (thing-at-point 'word)))
           (_ (progn (forward-sexp 1) (forward-char 1)))
           (pos (string-to-number (thing-at-point 'word))))
      (cons line pos))))

(defun ej/haskell-jump-first-error ()
  (interactive)
  (let* ((line-pos (ej/haskell-find-first-error)))
    (goto-line (car line-pos))
    (beginning-of-line)
    (forward-char (1- (cdr line-pos)))))
  
(defhydra ej/hydra-haskell (:foreign-keys warn :columns 1 :exit t)
  " Haskell helpers "
  ("h" ej/haskell-reload "reload")
  ("e" ej/haskell-jump-first-error "jump first error")
)

(use-package haskell-mode
  :defer t
  :config
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  ;; (remove-hook 'haskell-mode-hook 'ej/haskell-mode-hook)
  (local-set-key (kbd "C-s-h") 'ej/hydra-haskell/body))

(use-package slime
  :defer t
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl"))

(defhydra ej/java-interactive (:exit t)
  ("v" vc-annotate "vc-annotate")
  ("g" ej/jump-go "dumb-jump-go wrapper")
	("b" dumb-jump-back "dumb-jump-back")
  )
(defun ej/java-hook ()
  (local-set-key (kbd "s-l") 'ej/java-interactive/body))
(add-hook 'java-mode-hook 'ej/java-hook)
(defvar tmp-java-dir "/tmp/java")

(defun ej/next-temp-java-file ()
  (when (not (file-exists-p tmp-java-dir))
    (make-directory tmp-java-dir))
  (let* ((files (directory-files tmp-java-dir))
         (j-files (ej/files-with-ext tmp-java-dir ".java"))
         (new-j-name (format "%02d.java" (length j-files))))
    (expand-file-name new-j-name tmp-java-dir)))

(defun ej/src-code-with-asmtools ()
  (interactive)
  (let* ((_ (org-edit-src-code))
         (code (ej/copy-buffer))
         (_ (org-edit-src-abort))
         (j-fname (ej/next-temp-java-file))
         (c-files-old (ej/files-with-ext tmp-java-dir ".class"))
         (_ (mapcar #'delete-file-quite c-files-old))
         (_ (ej/write-string-to-file code j-fname))
         (mode (completing-read "Choose mode" '("jdis" "jdec")))
         (_ (shell-command-to-string (format "javac %s" j-fname)))
         (c-files-upd (ej/files-with-ext tmp-java-dir ".class"))
         (c-selected (if (= (length c-files-upd) 1)
                         (car c-files-upd)
                       (completing-read "Choose class" c-files-upd)))
         (asm-cmd (format "java -jar %s %s %s" asmtools-jar-path mode c-selected))
         (res (shell-command-to-string asm-cmd)))
    (message "b-name: %s" (buffer-name))
    (if (one-window-p) (split-window-right))
    (other-window 1)
    (switch-to-buffer "*asm-buffer*")
    (message "b-name: %s" (buffer-name))
    (erase-buffer)
    (insert res)
    (other-window 1)
    (message "Done!")))

(use-package proof-general
  :defer t
	:custom
	(proof-splash-enable nil))

(use-package dockerfile-mode
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\Dockerfile.*$" . dockerfile-mode))
)

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

(use-package markdown-mode
  :config
  (setq markdown-fontify-code-blocks-natively t)
  :bind ("C-x w" . ej/wrap-src-interactive))

(use-package protobuf-mode
  ;; https://emacs.stackexchange.com/a/82664
  :hook (protobuf-mode . init:protobuf-mode-hook)
  :config
  (defconst init:protobuf-style
    '((c-basic-offset . 2)
      (indent-tabs-mode . nil)))
  (defun init:protobuf-mode-hook ()
    (c-add-style "init-protobuf-style" init:protobuf-style t)))
