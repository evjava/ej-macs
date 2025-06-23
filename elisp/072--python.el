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
  (let* ((root-path (elpy-project-find-python-root))
         (pyproject-toml-path (s-concat root-path "pyproject.toml"))
         (f8 (ej/locate-flake8))
         (cmd-pref (if (not (file-exists-p pyproject-toml-path)) (ej/locate-flake8)
                     (format "%s --toml-config %s" f8 pyproject-toml-path)))
         (cmd (format "%s %s" cmd-pref file-name))
         (_ (message "command: %s" cmd))
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

(defun ej/get-prev-line-var-name ()
  (save-excursion
    (previous-line 1)
    (back-to-indentation)
    (substring-no-properties (thing-at-point 'sexp))))

(defun ej/insert-debug-var ()
  (interactive)
  (let* ((var-name (ej/get-prev-line-var-name)))
    (insert (format "debug(f\"{%s=}\")" var-name))))

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
    ("D" ej/insert-debug-var "insert debug(var)")
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

