(setq org-babel-python-command python-dir)
(setq python-shell-interpreter python-dir)

(defun ej/hook-python-vars ()
  (setq indent-tabs-mode nil)
  (setq python-indent 4)
  (setq python-indent-def-block-scale 1)
  (setq tab-width 2)
  (setq fill-column 120)
  )

(add-hook 'python-mode-hook 'ej/hook-python-vars)

(defun show-chunk-overlay (line text)
  (let* ((_ (goto-line line))
         (_ (end-of-line))
         (text-display (format "\n%s\n" text))
         (ov (make-overlay (point) (1+ (point)))))
    (overlay-put ov 'display text-display)
    (overlay-put ov 'face `(:foreground "red"))))

(defun ej/prettify-ruff-json-line (ruff-json-line)
  (let* ((loc (gethash "location" ruff-json-line))
         (pos-x (gethash "row" loc))
         (pos-y (gethash "column" loc))
         (text (gethash "message" ruff-json-line))
         (code (gethash "code" ruff-json-line))
         (arrow "⤷")
         (ln (format "%s%s %s: %s" (s-repeat (1- pos-y) " ") arrow code text))
         ) ln))

(defun ej/inline-ruff-json-line (ruff-json-line)
  (let* ((ln-pretty (ej/prettify-ruff-json-line ruff-json-line))
         (pos-x (gethash "row" (gethash "location" ruff-json-line))))
    (show-chunk-overlay pos-x ln-pretty)))

(defun ej/locate-ruff ()
  (s-trim (shell-command-to-string "which ruff")))

(defun ej/get-ruff-command (fname)
  (let* ((ruff (ej/locate-ruff))
         (cmd (format "%s check --output-format json %s" ruff fname))
         )
    (message "Ruff command: %s" cmd)
    cmd))

(defun ej/postfix-f401 (ruff-json)
  " filtering all F401 ( import-error ) if other errors present "
  (let* (
         (ruff-json-patched (vconcat (seq-remove (lambda (entry) (equal (gethash "code" entry) "F401")) ruff-json)))
         (res (if (= 0 (length ruff-json-patched)) ruff-json
                ruff-json-patched))
         ) res))

(defun ej/get-ruff-json (fname)
  (let* ((cmd (ej/get-ruff-command fname))
         (ruff-out (shell-command-to-string cmd))
         (ruff-json (json-parse-string ruff-out))
         ) ruff-json))

(defun ej/get-line (ruff-json-line)
  (let* ((pos-x (gethash "row" (gethash "location" ruff-json-line)))
         ) pos-x))

(defun ej/annotate-py-with-ruff (&optional goto-first)
  (interactive)
  (save-buffer)
  (let* ((fname (buffer-file-name))
         (ruff-json-before (ej/get-ruff-json fname))
         (ruff-json (ej/postfix-f401 ruff-json-before))
         )
    (ej/remove-overlays)
    (if (= 0 (length ruff-json))
        (message "No problems found")
      (save-excursion
        (cl-loop
         for ruff-json-line across ruff-json
         do (ej/inline-ruff-json-line ruff-json-line)))
      (when goto-first
        (let* ((ruff-json-line-0 (aref ruff-json 0))
               (pos-x (ej/get-line ruff-json-line-0))
               (pos-y (gethash "column" (gethash "location" ruff-json-line-0))))
          (ej/goto-line-column pos-x pos-y))))))

(defun ej/ruff-warn-keys-for-cur-line-inner (fname cur-line-pos)
  (let* ((ruff-json (ej/get-ruff-json fname))
         (cur-ruff-json (cl-loop for entry across ruff-json if (eq cur-line-pos (ej/get-line entry)) collect entry))
         (cur-keys (--map (gethash "code" it) cur-ruff-json))
         ) cur-keys))

(setq fname "/home/tagin/1/repo/agi/services/chat-manager/src/tracks/t_hosted_tracks.py")
(setq cur-line-pos 106)

(defun ej/ruff-warn-keys-for-cur-line ()
  (interactive)
  (let* ((fname (buffer-file-name))
         (cur-line-pos (line-number-at-pos))
         (res (ej/ruff-warn-keys-for-cur-line-inner fname cur-line-pos))
         ) res))

(defun ej/noqa-fix (&optional noca-comment-other)
  (interactive)
  (-when-let* ((noqa-comment (or noca-comment-other "todo-fix"))
               (keys (-uniq (ej/ruff-warn-keys-for-cur-line)))
               (keys-joined (s-join ", " keys))
               (noqa-comment (format "  # noqa: %s: %s" keys-joined noqa-comment)))
    (when noqa-comment
      (ej/remove-overlays)
      (end-of-line)
      (insert noqa-comment)
      (beginning-of-line)
      (ej/annotate-py-with-ruff))))

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
         (ruff (ej/locate-ruff))
         ;; todo fix
         (pre-commands (list
                        (format "%s check --fix" ruff)
                        (format "%s format" ruff)
                        ;; https://lyz-code.github.io/autoimport/
                        (format "autoimport --config-file %s" autoimport-config-path)
                        ;; todo fix
                        (format "%s format" ruff)
                        (format "%s check --select I --fix" ruff)
                        ))
         (commands (--map (format "%s %s" it fpath) pre-commands)))
    (cl-loop
     for cmd in commands
     do (shell-command-to-string cmd))
    ))

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

(defun ej/get-prev-line-var-name ()
  (save-excursion
    (previous-line 1)
    (back-to-indentation)
    (substring-no-properties (thing-at-point 'symbol))))

(defun ej/insert-debug-var ()
  (interactive)
  (let* ((var-name (ej/get-prev-line-var-name)))
    (insert (format "debug(f\"{%s=}\")" var-name))))

(pretty-hydra-define ej/python-interactive (:foreign-keys warn :exit t :quit-key "q")
	(
   "Annotations"
	 (
    ("s-i" iove/annotate)
    ("l" (ej/annotate-py-with-ruff t) "linter+go")
    ("L" ej/annotate-py-with-ruff "linter")
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

(defun ej/parse-filepath-and-line (input-string)
  "Parse the file path and line number from the INPUT-STRING.
The expected format is 'File ./path/to/file., line N, in function'."
  (let ((regex "File \"\\([^\"]+\\)\", line \\([0-9]+\\)"))
    (when (string-match regex input-string)
      (let ((file (match-string 1 input-string))
            (line (match-string 2 input-string)))
        (list file (string-to-number line))))))

;; todo fix: generalize to any file
(defvar ej--last-line-pos nil "remembered last line")
(defvar ej--line-to-copy nil "todo fix")

(defun ej/goto-line (ln)
  (goto-char (point-min))
  (forward-line ln))

(defun ej/goto-error-line-from-traceback-in-other-window ()
  "Navigate to the error line indicated in the traceback, in another window."
  (interactive)
  (let ((current-window (selected-window)))
    (other-window 1)
    (setq ej--line-to-copy nil)
      (save-excursion
        (if (not (eq last-command 'ej/goto-error-line-from-traceback-in-other-window))
            (setq ej--last-line-pos nil)
          (if (null ej--last-line-pos)
              (goto-char (point-max))  ; Start searching from the end of the buffer
            (ej/goto-line (1- ej--last-line-pos)))
          (message "Continuation of search at %d" ej--last-line-pos)
          )

        
        (if (search-backward "File " nil t)
            (progn
              (setq
               ej--line-to-copy
               (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
              (message "Copied line: %s" ej--line-to-copy)
              (setq ej--last-line-pos (line-number-at-pos))))

        ;; (message "No traceback error line found.")
        )
      
      (select-window current-window)
      (let* ((fpath-line (ej/parse-filepath-and-line ej--line-to-copy))
             (fpath (car fpath-line))
             (line (cadr fpath-line)))
        (ej/find-file-goto-line fpath :line line)
        (recenter-top-bottom))))

(global-set-key (kbd "C-s-e") #'ej/goto-error-line-from-traceback-in-other-window)
