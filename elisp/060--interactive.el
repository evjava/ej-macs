;; shell: interactive
(defun insert-send (text)
  (interactive)
  (goto-char (point-max))
  (insert text)
  (comint-send-input))

(defun ej/output-lines (cmd)
  (let* ((output (s-trim (shell-command-to-string cmd)))
         (res (if (empty output) '()
                (s-split "\n" output)))
         ) res))

(defun ej/git-modified-files ()
  (let* ((fs1 (ej/output-lines "git diff --name-only --cached"))
         (fs2 (ej/output-lines "git diff --name-only"))
         (res (-concat fs1 fs2))
         ) res))

(defun ej/git-untracked-files ()
  (ej/output-lines "git status -s | awk '/^\\?\\?/ {print $2}'"))

(setq ej/branches-cmd "git branch --list --sort=-committerdate")
(setq ej/handy-letters-git "asfjklghqwertyuiopvbn")

(defun ej/switch-branch ()
  (interactive)
  (let* ((branches-output (shell-command-to-string ej/branches-cmd))
         (branches-0 (s-split "\n" (s-trim branches-output)))
         (branches-1 (--filter (not (s-starts-with? "*" it)) branches-0))
         (branches (-map #'s-trim branches-1))
         (branches-no-dev-master (--remove (-contains? '("master" "develop") it) branches))
         (pairs-no-dev-master (-zip (string-to-list ej/handy-letters-git) branches-no-dev-master))
         (pairs (cl-concatenate
                 'list
                 (if (-contains? branches "develop") '((?d . "develop")))
                 (if (-contains? branches "master") '((?m . "master")))
                 pairs-no-dev-master))
         (sexp (--map `(,(format "%c" (car it)) (insert-send ,(format "git co %s" (cdr it))) ,(cdr it)) pairs))
         (hydra `(defhydra hydra-switch-branch (:exit t :columns 1 :foreign-keys warn)
                   "Hydra switch branch"
                   ,@sexp
                   ("\t\tq" nil "quit")))
         )
    (call-interactively (eval hydra))))

(defun ej/start-new-commit ()
  (interactive)
  (let* ((branch (s-trim (shell-command-to-string "git branch --show-current"))))
    (insert (format "git ci -am '%s: " branch))))

(defun ej/get-previous-cmd-output ()
  (interactive)
  (save-excursion
    (let* ((_ (forward-line -2))
           (end (point))
           (_ (comint-previous-prompt 1))
           (_ (end-of-line 1))
           (_ (forward-char 1))
           (start (point))
           (res (buffer-substring-no-properties start end))
           ) res)))
           


(defun ej/parse-commands-from-history (cmd-output)
  (let* ((lines (s-split "\n" cmd-output))
         (commands (cl-loop
                    for ln in lines
                    if (and (/= 0 (length ln)) (not (equal ?\ (elt ln 0))))
                    for ln-parts = (s-split-up-to " " (s-trim ln) 1)
                    for (cnt . cmd) = ln-parts
                    if (is-integer cnt)
                    collect (s-trim (car cmd))))
         (res (cl-remove-duplicates commands :test #'equal))
         ) res))

(setq ej/handy-letters "asdfghjkl;")
(setq ej/max-commands (length ej/handy-letters))

;; todo generalize
(defun ej/make-hydra-from-lines (commands)
  (let* ((pairs (-zip (string-to-list ej/handy-letters) commands))
         (sexp (--map `(,(format "%c" (car it)) (insert-send ,(cdr it)) ,(cdr it)) pairs))
         (hydra `(defhydra hydra-run-command (:exit t :columns 1 :foreign-keys warn)
                   "run command"
                   ,@sexp
                   ("\t\tq" nil "quit")))
         )
    hydra))
  

(defun ej/parse-push-commands (prev-output)
  (when (s-contains? "has no upstream branch." prev-output)
    (when (string-match "git push --set-upstream.*" prev-output)
      (list (match-string-no-properties 0 prev-output)))))

(defun ej/parse-updating (prev-output)
  (let* ((pat (rx "Updating " (= 7 (any "a-z" "0-9")) ".." (= 7 (any "a-z" "0-9")))))
    (when (string-match pat prev-output)
      (let* ((match (match-string-no-properties 0 prev-output))
             (commits-range (cadr (s-split " " match)))
             (res (list (format "git diff %s" commits-range)))
             ) res))))

(defun ej/suggest-context-commands ()
  (interactive)
  (let* ((prev-output (ej/get-previous-cmd-output))
         (commands (or
                    (ej/parse-commands-from-history prev-output)
                    (ej/parse-push-commands prev-output)
                    (ej/parse-updating prev-output)
                    ))
         (commands-cut (seq-subseq commands 0 (min (length commands) ej/max-commands)))
         (hydra (ej/make-hydra-from-lines commands-cut))
         )
    (call-interactively (eval hydra))))

(defun ej/convert-files-to-loop ()
  (interactive)
  (kill-line 0)
  (insert "for fp in $(")
  (insert (last-killed))
  (insert "); do ")
  (save-excursion
    (insert "; done")))

(defun ej/get-prompt ()
  (save-excursion
    (let* ((p (point))
           (_ (backward-char 1))
           (_ (move-beginning-of-line 1))
           (sub (buffer-substring-no-properties (point) p))
           (res (s-trim sub))
           ) res)))

(defun ej/make-tramp-path (prompt)
  (let* ((prompt-fix (string-trim-right prompt " [$]"))
         (parts (s-split "[@:]" prompt-fix))
         (user (elt parts 0))
         (home (format "/home/%s" user))
         (host (downcase (elt parts 1)))
         (path (elt parts 2))
         (path-fix (s-replace "~" home path))
         (res (format "/ssh:%s:%s" host path-fix))
         ) res))

;; (equal (ej/make-tramp-path "user@HOST:~/some-path $") "/ssh:host:/home/user/some-path")


(defun ej/start-tramp ()
  (interactive)
  (let* ((prompt (ej/get-prompt))
         (tramp-path (ej/make-tramp-path prompt))
         )
    (find-file tramp-path)))



(defun ej/get-dir-from-previous-buffer ()
  (with-current-buffer (other-buffer (current-buffer))
    (if (buffer-file-name)
      (file-name-directory (buffer-file-name))
    default-directory)))

(defun ej/find-nearest-by-pattern (pattern)
  (save-window-excursion
    (let ((starting-window (selected-window)))
      (other-window 1)
      (cl-loop for window = (selected-window) then (other-window 1)
               until (or (null window) (eq window starting-window))
               for buffer = (window-buffer window)
               when (s-matches? pattern (buffer-name buffer))
               return buffer
               finally return nil))))

(defun ej/insert-nearest-playground ()
  (interactive)
  (let* ((playground-buffer (ej/find-nearest-by-pattern "playground.*.py")))
    (if (null playground-buffer)
        (message "Nearest `playground*` not found!")
      (insert (format "python %s" playground-buffer)))))


(pretty-hydra-define ej/shell-helper (:foreign-keys warn :exit t :quit-key "q")
  (
   "cd"
   (
    ("!" (insert-send (format "cd %s" (thing-at-point 'existing-filename))) "cd to dir at point")
    ("<" (insert-send "cd -") "cd -")
    ("r" (insert-send (format "cd %s" (shell-command-to-string "git root"))) "cd git root")
    ("$" (insert-send (format "cd %s" (ej/get-dir-from-previous-buffer))) "cd dir in prev buffer")
    )

   "Tools"
   (
    ("l" (insert-send "alias l=\"ls -al\"") "alias l=\"ls -al\"")
    ("s" (insert "chmod +x *.sh") "chmod +x *.sh")
    ("j" (insert "python -m json.tool --no-ensure-ascii") "python prettify via json.tool")
    ("s-i" (ej/suggest-context-commands) "suggest commands from context")
    ("f" (ej/convert-files-to-loop) "convert files output to loop")
    (">" elpy-rgrep-symbol "find-symbol")
    ("/" (insert "fd -e py -x rg -H ") "fd -e py -x rg -H")
    ("t" (ej/start-tramp) "tramp")
    )

   "Git"
   (
    ("m" (insert (completing-read "Choose" (ej/git-modified-files))) "choose modified git file")
    ;; ("L" (insert-send "git branch --list --sort=-committerdate") "insert `git branch --list`")
    ("?" (insert (completing-read "Choose" (ej/git-untracked-files))) "choose untracked git file")
    ("{" (insert-send "git checkout -") "git co -")
    ("+" ej/start-new-commit "start new commit")
    ("b" (ej/switch-branch) "switch branch")
    )
   
   "Python"
   (
    ("p" (insert-send "import pandas as pd") "import pandas as pd")
    ("P" (insert-send "from pathlib import Path") "from pathlib import Path")
    ("c" (insert-send "from collections import Counter, defaultdict") "Counter & defaultdict")
    ("L" (ej/insert-nearest-playground) "python playground_*")
    )
   ))

(defun ej/get-fpath (&optional firstp)
  (let* ((fpath
          (save-excursion
            (if (not firstp) (forward-line -3)
              (comint-previous-prompt 1)
              (forward-line 1))
            (end-of-line)
            (thing-at-point 'existing-filename)))
         ) fpath))

(defun ej/cd-dir-from-stdout (&optional firstp)
  (let* ((fpath (ej/get-fpath firstp))
         (dir (if (file-directory-p fpath) fpath (file-name-directory fpath))))
    (insert (format "cd %s" dir))
    (comint-send-input)))

(defun ej/cd-dir-from-stdout-first () (interactive) (ej/cd-dir-from-stdout t))
(defun ej/cd-dir-from-stdout-last () (interactive) (ej/cd-dir-from-stdout nil))
(defun ej/open-file-first () (interactive) (find-file (ej/get-fpath t)))

(defun ej/shell-hook ()
  (interactive)
  (local-set-key (kbd "C-c s-r") 'ej/rename-shell)
  (local-set-key (kbd "s-j") 'ej/shell-helper/body)
  (local-set-key (kbd "C-s-j") 'ej/cd-dir-from-stdout-first)
  (local-set-key (kbd "C-s-o") 'ej/open-file-first)
  (local-set-key (kbd "M-s-j") 'ej/cd-dir-from-stdout-last)
  (local-set-key (kbd "s-<backspace>") '(lambda () (interactive) (insert-send "cd -")))
  )
(add-hook 'shell-mode-hook 'ej/shell-hook)

(defun ej/insert-src-and-tests-subdirs ()
  (interactive)
  (let* ((dir (dired-current-directory))
         (dir-name (file-name-nondirectory (directory-file-name dir)))
         (module-name (s-replace "-" "_" dir-name))
         (src-dir (format "package/src/%s" module-name)))
    (dired-insert-subdir "tests")
    (dired-insert-subdir src-dir)))

;; dired
(defhydra ej/dired-interactive (:exit t :columns 1)
  " Dired commands "
  ("b" ej/dired-file-name-add-date "add date prefix")
  ("t" ej/toggle-empty-dir-file "toggle empty dir <-> file")
  ("i" dired-insert-subdir "dired-insert-subdir")
  ("l" ej/insert-src-and-tests-subdirs "dired-insert-subdir: tests, src")
  )

(defun ej/dired-hook ()
  (interactive)
  (local-set-key (kbd "r") 'ej/dired-interactive/body)
  )
(add-hook 'dired-mode-hook 'ej/dired-hook)

;; dired: add-date
;; (define-key dired-mode-map (kbd "b") #'ej/dired-file-name-add-date)

