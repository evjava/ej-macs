(defun ej/prog-mode-hook ()
  (local-set-key (kbd "<C-M-return>") 'ej/run-other-window))
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

(setq org-babel-python-command python-dir)
(setq python-shell-interpreter python-dir)

(defun ej/hook-python-vars ()
  (setq indent-tabs-mode nil)
  (setq python-indent 4)
  (setq python-indent-def-block-scale 1)
  (setq tab-width 2))

(add-hook 'python-mode-hook 'ej/hook-python-vars)

(defun show-chunk-overlay (line text)
  (let* ((_ (goto-line line))
         (_ (end-of-line))
         (text-display (format "\n%s\n" text))
         (ov (make-overlay (point) (1+ (point)))))
    (overlay-put ov 'display text-display)
    (overlay-put ov 'face `(:foreground "red"))))

(defun ej/parse-flake8-line (line)
  " returns `(pos-x pos-y text) "
  (let* ((parts (s-split-up-to ":" line 3))
         (pos-x (string-to-number (elt parts 1)))
         (pos-y (string-to-number (elt parts 2)))
         (text (s-trim (elt parts 3)))
         (res (list pos-x pos-y text))
         ) res))

(defun ej/pline-to-text (pline)
  (let* ((parsed pline)
         (pos-x (car parsed))
         (pos-y (cadr parsed))
         (text (caddr parsed)))
    (format "%s⤷ %s" (s-repeat (1- pos-y) " ") text)))

(defun ej/run-test-function ()
  (interactive)
  (save-current-buffer)
  (eval-buffer)
  (with-current-buffer "dassist_checker.py"
    (ej/annotate-py-with-flake8)))

(defun ej/inline-flake8-chunk (chunk)
  (-let* ((pos-x (car chunk))
          (parsed-lines (cdr chunk))
          (texts (--map (ej/pline-to-text it) parsed-lines))
          (text (s-join "\n" texts)))
    (show-chunk-overlay pos-x text)))

(setq QUALIFIED-PREF "Unable to find qualified")

(defun ej/locate-flake8 ()
  (s-trim (shell-command-to-string "which flake8")))

(defun ej/flake8-parsed-lines (file-name)
  (let* ((cmd (format "%s %s" (ej/locate-flake8) file-name))
         (out (s-trim (shell-command-to-string cmd)))
         (lines-0 (s-split "\n" out))
         (lines (--remove (or (= 0 (length it)) (s-starts-with? QUALIFIED-PREF it)) lines-0))
         (parsed-lines (--map (ej/parse-flake8-line it) lines))
         ) parsed-lines))

(defun ej/goto-line-column (pos-x pos-y)
  (goto-char (point-min))
  (forward-line (1- pos-x))
  (beginning-of-line)
  (forward-char (1- pos-y))
  )

(defun ej/annotate-py-with-flake8 (&optional goto-first)
  (interactive)
  (save-buffer)
  (let* ((parsed-lines (ej/flake8-parsed-lines (buffer-file-name)))
         (chunks (--group-by (car it) parsed-lines))
         (pline-0 (car parsed-lines))
         (pos-x (unless (= 0 (length pline-0)) (car pline-0)))
         (pos-y (unless (= 0 (length pline-0)) (cadr pline-0)))
         )
    (ej/remove-overlays)
    (if (not parsed-lines)
        (message "No style problems found")
      (when parsed-lines
        (save-excursion
          (cl-loop
           for chunk in chunks
           do (message "chunk: %S" chunk)
           do (ej/inline-flake8-chunk chunk)))
        (message "goto: %s" pos-x)
        (when goto-first
          (ej/goto-line-column pos-x pos-y))))))

(defun ej/flake8-warn-keys-for-cur-line ()
  (interactive)
  (let* ((parsed-lines (ej/flake8-parsed-lines (buffer-file-name)))
         (cur-line-pos (line-number-at-pos))
         (cur-parsed-lines (--filter (eq cur-line-pos (car it)) parsed-lines))
         (keys (--map (car (s-split-up-to " " (caddr it) 1)) cur-parsed-lines))
         ) keys))

(defun ej/noqa-fix (&optional noca-comment-other)
  (interactive)
  (-when-let* ((noqa-comment (or noca-comment-other "todo-fix"))
               (keys (-uniq (ej/flake8-warn-keys-for-cur-line)))
               (keys-joined (s-join ", " keys))
               (noqa-comment (format "  # noqa: %s: %s" keys-joined noqa-comment)))
    (when noqa-comment
      (ej/remove-overlays)
      (end-of-line)
      (insert noqa-comment)
      (beginning-of-line)
      (ej/annotate-py-with-flake8))))

(defun elpy-rgrep-symbol-after (&rest args)
  (other-window 1)
  (forward-line 4))

(use-package elpy
  :defer t
  :config
  (advice-add 'elpy-rgrep-symbol :after #'elpy-rgrep-symbol-after)
  (elpy-enable))

(setq 
 ej/python-snippets
 '(
   "from dataclasses import dataclass"
   "raise AttributeError("
   "if __name__ == '__main__':\n\tfire.Fire("
   "import fire"
   "logger = logger.getLogger(__name__)"
   ))

(defun ej/get-full-py-module ()
  (let* ((fpath (buffer-file-name))
         (root-path (projectile-acquire-root))
         (r-path (substring fpath (length root-path) -3))
         (res (s-replace "/" "." r-path))
         ) res))

(defun ej/copy-full-py-module ()
  (interactive)
  (let* ((module-name (ej/get-full-py-module)))
    (kill-new module-name)
    (message "Copied: %s" module-name)))

(defun ej/eval-line ()
  (interactive)
  (let* ((line (thing-at-point 'line)))
    (end-of-line 1)
    (forward-char 1)
    (other-window 1)
    (insert (s-trim line))
    (comint-send-input)
    (other-window -1)))

(defun ej/add-python-snippet ()
  (interactive)
  (helm
   :sources  (helm-build-sync-source "Add Python snippet"
               :candidates ej/python-snippets
               :action 'insert
               :fuzzy-match t)
   :buffer "*helm suggestion latex header*"))

(defun ej/patch-f-string ()
  (interactive)
  (save-excursion
    (backward-char 1)
    (re-search-backward "['\"]")
    (insert "f")))
  
(defun ej/highlight-sexp-at-point (color)
  (interactive)
  (let* ((sexp (sexp-at-point))
         (pattern (format "\\_<%s\\_>" sexp)))
    (highlight-regexp pattern color)))

(defun ej/collapse-to-line (content)
  "Collapse multi-line CONTENT into a single line while preserving proper spacing."
  (let* ((replaces
          '(("[ \t]*\n[ \t]*" " ")
            ("[ \t]*([ \t]*" "(")
            ("[ \t]*,[ \t]*" ", ")
            ("[ \t]*)[ \t]*" ")")
            (",)" ")")))
         (res (--reduce-from
               (replace-regexp-in-string (car it) (cadr it) acc)
               content
               replaces))
         ) res))
(setq content "    return A(\n        x=x,\n        y=y,\n    )")
(setq output-expected "    return A(x=x, y=y)")
(cl-assert (equal (ej/collapse-to-line content) output-expected))

(defun ej/get-start-of-line-pos (point) (interactive) (save-excursion (goto-char point) (beginning-of-line 1) (point)))
(defun ej/end-of-line-pos (point) (interactive) (save-excursion (goto-char point) (end-of-line 1) (point)))

(defun ej/collapse-to-line-interactive ()
  (interactive)
  (let* ((p1 (1- (point)))
         (a (ej/get-start-of-line-pos (min (mark) p1)))
         (b (1+ (ej/end-of-line-pos (max (mark) p1))))
         (_ (kill-region a b))
         (content (current-kill 0))
         (content-fix (ej/collapse-to-line content))
         )
    (insert content-fix)
    (insert "\n")))

(defun ej/auto-fix-this-file ()
  (interactive)
  (save-buffer)
  (let* ((fpath buffer-file-name)
         (pre-commands (list
                        "black"
                        ;; https://lyz-code.github.io/autoimport/
                        "autoimport"))
         (commands (--map (format "%s %s" it fpath) pre-commands)))
    (cl-loop
     for cmd in commands
     do (shell-command-to-string cmd))
    ))


;; (projectile-project-root ))
;;     (projectile-ripgrep pattern)
;;     (other-window 1)
;;     (sit-for 0.4)
;;     (search-forward pattern)
;;     (compile-goto-error)
;;     ))

(defun ej/find-class-symbol ()
  (interactive)
  (let* ((symbol (thing-at-point 'word))
         (pattern (format "class %s" symbol))
         (dir "/home/tagin/1/repo/agi-med-llm")
         (cmd (format "rg --type-add 'pyy:*.py' -t pyy '%s' %s --glob '!sci'" pattern dir))
         (output (s-trim (shell-command-to-string cmd)))
         ;; todo fix: if many found, just open projectile-ripgrep
         (output-parts (s-split ":" output))
         (fpath (car output-parts)))
    (g fpath :str pattern)))

;; (projectile-project-root ))
;;     (projectile-ripgrep pattern)
;;     (other-window 1)
;;     (sit-for 0.4)
;;     (search-forward pattern)
;;     (compile-goto-error)
;;     ))

(pretty-hydra-define ej/python-interactive (:foreign-keys warn :exit t :quit-key "q")
	(
   "Annotations"
	 (
    ("s-i" iove/annotate)
    ("f" (ej/annotate-py-with-flake8 t) "flake-8")
    ("F" ej/annotate-py-with-flake8 "flake-8")
    ("n" ej/noqa-fix "noqa-fix")
    ("N" (ej/noqa-fix "intended") "noqa-fix-intended")
    ("v" vc-annotate "vc-annotate")
    ("Y" (ej/highlight-sexp-at-point 'hi-yellow) "highlight yellow")
    ("R" (ej/highlight-sexp-at-point 'hi-pink) "highlight red")
		)
	 
	 "Code actions"
	 (("g" ej/jump-go "dumb-jump-go wrapper")
		("b" dumb-jump-back "dumb-jump-back")
    ("?" elpy-rgrep-symbol "find-symbol")
    ("7" (message "TODO implement") "todo implement find class symbol")
		(";" ej/comment-and-next-line "comment and next line" :exit nil)
		("TAB" ej/indent-and-next-line "Indent and next line" :exit nil)
		("<C-tab>" ej/indent-until-end-of-sexp "Indent until end of sexp")
    ("i" ej/add-python-snippet "completions")
		("w" ej/copy-sexp-at-point "copy last sexp")
    ("s-j" ej/run-other-window "run other window")
    ("'" ej/patch-f-string "string -> f-string")
    ("<" ej/collapse-to-line-interactive "collapse")
    ("a" ej/auto-fix-this-file "auto-fix this file")
		)
	 
	 "Templates"
	 (
    ("c" (insert "from collections import defaultdict, Counter") "import defaultdict and Counter")
    ("p" (insert "breakpoint()") "breakpoint")
    ("P" (insert "from pathlib import Path") "from pathlib import Path")
    ("m" (insert "def main():\n    pass\n\nif __name__ == '__main__':\n    main()") "if __name__ == '__main__'")
    ("M" (insert "async def main():\n    pass\n\nif __name__ == '__main__':\n    asyncio.run(main())") "if __name__ == '__main__' (async)")
    ("E" (insert "ensure_ascii=False, indent=2)") "ensure_ascii=False, indent=2")
    )
	 
	 "Etc"
	 (
    ("d" ej/remove-overlays "remove overlays")
    ("/" ej/eval-line "eval line" :exit nil)
    ("y" ej/copy-full-py-module "copy-full-py-module")
    ("<ESC>" nil "exit")
		)
	 )
  )

(defun ej/py-interactive-hook ()
  (interactive)
  (local-set-key (kbd "s-j") 'ej/python-interactive/body)
  (local-set-key (kbd "<C-M-return>") 'ej/run-other-window)
  (local-set-key (kbd "M-s-g") 'ej/jump-go)
  (local-set-key (kbd "M-s-b") 'dumb-jump-back)
  (local-set-key (kbd "C-s-p") 'ej/patch-f-string)
  )
(add-hook 'python-mode-hook 'ej/py-interactive-hook)

(defun avi-kill-line-save (&optional arg)
      "Copy to the kill ring from point to the end of the current line.
    With a prefix argument, copy that many lines from point. Negative
    arguments copy lines backward. With zero argument, copies the
    text before point to the beginning of the current line."
      (interactive "p")
      (save-excursion
        (copy-region-as-kill
         (point)
         (progn (if arg (forward-visible-line arg)
                  (end-of-visible-line))
                (point)))))

; todo improve or research some normal python-mode
(defun ej/copy-python-section()
  " 3:00 - 03:15, 00:12 -  "
  (interactive)
  (let ((continue t))
    (while continue
      (avi-kill-line-save)
      (next-line)
      (setq continue (equal (following-char) ?\ ))
      (other-window 1)
      (yank)
      (if (not continue)
        (comint-send-input)
        (progn
          (open-line 1)
          (next-line)))
      (other-window -1))))

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

(defmacro msg (&rest vars)
  " for debug purposes "
  `(progn
     (mapc
      (lambda (v)
        (condition-case nil
            (message "DBG: %s is <%S>" v (eval v))
          (error (message "DBG: %s not exists..." v))))
      (list ,@vars))
     nil))

(defun ej/replace-last-sexp (new-sexp)
  (kill-sexp -1)
  (insert (format "%S" new-sexp)))

(defun ej/eval-replace (mode)
  (interactive "p")
  (let ((value (eval (elisp--preceding-sexp))))
    (if (eq mode 1) (kill-sexp -1))
    (save-excursion
      (insert (format "%S" value)))))

(defun ej/cur-sexp ()
  (interactive)
  (read (thing-at-point 'sexp)))

(defun ej/setq-let ()
  (interactive)
  (save-excursion
    (let* ((sexp-unq (read (thing-at-point 'sexp)))
           (var-unq (car sexp-unq))
           (val-unq (cadr sexp-unq))
           (ev-val-unq (eval val-unq))
           (tp (type-of ev-val-unq))
           (_  (eval `(setq ,var-unq ev-val-unq)))
           (_ (message "%s >> %s :: %s" var-unq ev-val-unq tp))
           (msg-display (format "%s :: %s" ev-val-unq tp))
           )
      (eros--eval-overlay msg-display (point))
      (list var-unq ev-val-unq))))

(defun ej/get-let-value ()
	(interactive)
  (save-excursion
    (let* ((sexp-unq (read (thing-at-point 'sexp)))
           (var-unq (car sexp-unq))
           (val-unq (cadr sexp-unq))
           (ev-val-unq (eval val-unq))
           (tp (type-of ev-val-unq))
           (_  (eval `(setq ,var-unq ev-val-unq)))
           )
      (list var-unq ev-val-unq))))

(defun ej/setq-last-sexp (var-unq)
  (interactive "senter var name: ")
  (save-excursion
    (let* ((sexp-unq (ej/cur-sexp))
           (setq-form `(setq ,(read var-unq) ',sexp-unq)))
      (msg 'setq-form)
      (eval setq-form)
      (message "%s >> %s" var-unq (symbol-value var-unq)))))

(defun ej/setq-killed (var)
  (interactive "senter var name: ")
  (let* ((var-symbol (read var))
         (kill (substring-no-properties (current-kill 0))))
    (eval `(setq ,var-symbol ,kill))
    (message "%s >> %s" var (symbol-value var-symbol))))

(defun ej/message-last-sexp ()
  (interactive)
  (let* ((sexp (ej/cur-sexp)))
    (message "sexp: %s >> %s" sexp (eval sexp))))

(defun ej/copy-sexp-at-point ()
  (interactive)
  (kill-new (thing-at-point 'sexp)))

(defun ej/setq-forward-lets ()
  (interactive)
  (ej/setq-let)
  (condition-case nil 
      (progn
        (forward-sexp)
        (ej/setq-forward-lets))
    (error nil)))

(setq ej/elisp-prettifier nil)
(defun ej/show-val (val)
  (let* ((prettified-val (and ej/elisp-prettifier (funcall ej/elisp-prettifier val))))
    (cond
     (prettified-val prettified-val)
     ((listp val)    (format "[len=%d] %S" (length val) val))
     (t              (format "%S" val)))))

(defvar annotate-color "#5dbb63")
(defun ej/let-annotate-hard ()
	(interactive)
	(save-excursion
	(let* ((l-var-val (ej/get-let-value))
				 (l-var (car l-var-val))
				 (l-val (ej/show-val (cadr l-var-val)))
				 (_ (progn (end-of-line) (forward-char 1)))
				 (poz-a (point))
         (_ (back-to-indentation))
				 ;; (_ (progn (backward-sexp) (next-line 1)))
				 (indent (current-column))
				 (ind-s (s-repeat indent " "))
				 (pref (s-concat (s-repeat (- indent 2) " ") "=>"))
				 (poz-b (point))
				 (ov (make-overlay poz-a poz-b))
				 (_ (condition-case nil (forward-sexp) (error nil)))
				 (_ (overlay-put ov 'face `(:foreground ,annotate-color)))
         (pretty-l-val (if (< (length l-val) 1000) l-val (substring-no-properties l-val 0 1000)))
				 (_ (overlay-put ov 'display (format "%s %S\n%s" pref pretty-l-val ind-s))))
		nil)))

(defun ej/forward-sexp-if-can ()
  (condition-case nil
      (progn
        (forward-sexp 1)
        t)
    (error nil)))

(defun ej/setq-forward-lets-hard ()
  (interactive)
  (set-mark (point))
  (condition-case nil
      ;; todo fix
      (ej/defun-annotate-args)
    (error nil))
  (ej/let-annotate-hard)
  (while (ej/forward-sexp-if-can)
		(ej/let-annotate-hard)
    ))

(defun ej/eval-last-sexp-forward ()
  (interactive)
  (let* ((res (eval (ej/cur-sexp))))
    (forward-sexp)
    (message "Evaluated: %s" res)))

(defun ej/defun-assign ()
  (interactive)
  (let* ((sexp (read (thing-at-point 'sexp)))
         (fun-def (symbol-function (car sexp)))
         (_ (message "fun-def: %s" fun-def))
         (fun-args (cl-remove '&optional (cadr fun-def)))
         (assignments-99 (->> (-zip-fill nil fun-args (cdr sexp))
                           (mapcar #'-cons-to-list)
                           (-flatten-n 1)))
         (expr `(setq ,@assignments-99)))
    (eval expr)
    (message "evaluated: %s" expr)))

(defun ej/defun-and-args ()
  (save-excursion
    (beginning-of-defun)
    (let* ((fun-sexp (sexp-at-point))
           (args (-remove-item '&optional (caddr fun-sexp)))
           (fun-name (cadr fun-sexp))
           (res (cons fun-name args))
           ) res)))

(defun ej/defun-annotate-args ()
  (interactive)
  (save-excursion
    (beginning-of-defun)
    (let* ((fun-sexp (sexp-at-point))
           (args (-remove-item '&optional (caddr fun-sexp)))
           (_ (progn (search-forward " (" nil nil 1) (backward-char)))
           (poz-a (point))
           (_ (forward-sexp))
           (poz-b (point))
           (ov (make-overlay poz-a poz-b))
           (_ (overlay-put ov 'face `(:foreground ,annotate-color)))
           (max-len-arg (number-to-string (-max (--map (length (symbol-name it)) args))))
           (fmt (s-concat "  (%" max-len-arg "s => %s)"))
           (args-vals (--map (format fmt it (ej/show-val (eval it))) args))
           (args-info (s-join "\n" args-vals))
           (_ (overlay-put ov 'display (format "(\n%s\n)" args-info)))
           ) nil)))

(defun ej/remove-overlays ()
  (interactive)
  (remove-overlays))

(defun ej/s-prefix (str drop-last)
  (substring str 0 (- (length str) drop-last)))

; brg-util-test.el
(defun ej/toggle-el-test ()
  (interactive)
  (let* ((bfn buffer-file-name)
         (is-test (s-suffix? "test.el" bfn))
         (bfn-new (if is-test
                      (s-replace "-test.el" ".el" bfn)
                    (s-replace ".el" "-test.el" bfn))))
    (switch-to-buffer bfn-new)))

(defun ej/indent-and-next-line ()
  (interactive)
  (indent-for-tab-command)
  (next-line 1))

(defun ej/indent-until-end-of-sexp ()
  (interactive)
  (back-to-indentation)
  (let* ((_ (forward-sexp 1))
         (sexp-end-line (line-number-at-pos))
         (_ (backward-sexp 1)))
    (while (< (line-number-at-pos) sexp-end-line)
      (ej/indent-and-next-line))
		(indent-for-tab-command)
    (end-of-line)))

(defun ej/comment-and-next-line ()
  (interactive)
  (beginning-of-line 1)
  (insert ";;")
  (next-line 1))

(defun ej/jump-go ()
  (interactive)
  (let* ((sexp (->> (thing-at-point 'sexp) (read))))
    (cond
     ((symbolp sexp) (dumb-jump-go))
     ((consp sexp) (progn
                     (backward-sexp 1)
                     (forward-char 1)
                     (forward-sexp 1)
                     (dumb-jump-go)))
     (t (error "not supported")))))

(defun ej/insert-map-on-first ()
  " debug --map helper "
  (interactive)
  (let* ((sexp (sexp-at-point))
         (_ (when (not (equal (car sexp) '--map))
              (error "only --map forms supported")))
         (form (cadr sexp))
         (list (caddr sexp))
         (res (cl-subst (list 'elt list 0) 'it form))
         )
    (newline 1 t)
    (insert (format ";; %s" res))))

(defun ej/insert-ert-template (defun-name)
  (interactive (let* ((default-candidate (last-killed))
                      (user-input (read-string (format "Enter defun name: (default: [%s]) " default-candidate)))
                      (res (if (= 0 (length user-input)) default-candidate user-input))
                      ) (list res)))
  (message "(ej/insert-ert-template %s)" defun-name)
	(insert (format "(ert-deftest test-%s ()\n" defun-name))
	(insert "  (should (equal (" defun-name))

(defun ej/insert-message-defun-call ()
  (interactive)
  (let* ((f-name-args (ej/defun-and-args))
         (f-name (car f-name-args))
         (f-args (cdr f-name-args))
         (pretty-args (s-join " " (--map (format "%S" it) f-args)))
         (pretty-args-fmt (s-join " " (-repeat (length f-args) "%s")))
         (res (format "(message \"(%s %s)\" %s)" f-name pretty-args-fmt pretty-args))
         (_ (insert res))
         ) t))

(defun ej/rerun-setq-let-in-defun ()
  (interactive)
  (save-excursion
    (save-excursion
      (ej/remove-overlays)
      (beginning-of-defun)
      (search-forward "let*")
      (end-of-line)
      (ej/setq-forward-lets-hard))
    (ej/setq-forward-lets-hard)))

;; :title "Emacs Lisp interactive stuff"
(pretty-hydra-define ej/elisp-interactive (:foreign-keys warn :exit t :quit-key "q")
	("Annotations"
	 (("l" ej/setq-let "setq last let")
		("L" ej/setq-forward-lets "setq last let and forward" :exit t)
		("i" ej/setq-forward-lets-hard "setq lets hard")
		("!" ej/rerun-setq-let-in-defun "rerun setq-let in current defun")
		("a" ej/defun-assign "assign to arguments of defun")
		("n" ej/defun-annotate-args "annotate args")
		)
	 
	 "Code actions"
	 (("g" ej/jump-go "dumb-jump-go wrapper")
		("b" dumb-jump-back "dumb-jump-back")
		(";" ej/comment-and-next-line "comment and next line" :exit nil)
		("TAB" ej/indent-and-next-line "Indent and next line" :exit nil)
		("<C-tab>" ej/indent-until-end-of-sexp "Indent until end of sexp" :exit t)
		("w" ej/copy-sexp-at-point "copy last sexp")
    ("j" ej/run-test-function "TEST")
		)
	 
	 "Eval/set"
	 (("e" eros-eval-last-sexp "eval last sexp")
		("E" ej/eval-last-sexp-forward "eval last sexp and forward" :exit nil)
		("r" ej/eval-replace "replace last sexp")
		("s" ej/setq-last-sexp "setq last sexp")
		("k" ej/setq-killed "setq killed"))

	 "Templates"
	 (("<f2>" ej/insert-map-on-first "insert map on first")
		("<f3>" ej/insert-message-defun-call "insert message defun call")
		("<f9>" ej/insert-ert-template "insert ert template"))
	 
	 "Etc"
	 (("o" eval-buffer "eval-buffer")
		("p" (ert t) "ert")
		("<f5>" trace-function "trace-function")
		("<ESC>" nil "exit")
		("d" ej/remove-overlays "remove overlays")
		("C-n" (next-line) "exit"))
	 )
  )
;;  ("m" ej/message-last-sexp "message last sexp")
;;  ("T" ej/toggle-el-test "toggle .el or -test.el")
(global-set-key (kbd "C-x s-e") 'ej/eval-replace)
(global-set-key (kbd "C-x C-S-e") 'ej/eval-replace)
(defun ej/elisp-hook ()
  (local-set-key (kbd "s-j") 'ej/elisp-interactive/body)
  (local-set-key (kbd "M-s-t") 'transpose-sexps))
(add-hook 'emacs-lisp-mode-hook 'ej/elisp-hook)

(use-package treepy 
  :defer t)

(use-package eros
  :defer t
  :config
  (eros-mode 1))

(defhydra ej/java-interactive (:exit t)
  ("v" vc-annotate "vc-annotate")
  ("g" ej/jump-go "dumb-jump-go wrapper")
	("b" dumb-jump-back "dumb-jump-back")
  )
(defun ej/java-hook ()
  (local-set-key (kbd "s-l") 'ej/java-interactive/body))

(add-hook 'java-mode-hook 'ej/java-hook)

(defvar tmp-java-dir "/tmp/java")

(defun ej/files-with-suf (dir suf)
  (let* ((names (directory-files dir))
         (f-names (--filter (string-suffix-p suf it) names)))
    (--map (expand-file-name it dir) f-names)))
         
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
