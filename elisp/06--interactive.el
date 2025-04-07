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

(defun ej/modified-git-files ()
  (let* ((fs1 (ej/output-lines "git diff --name-only --cached"))
         (fs2 (ej/output-lines "git diff --name-only"))
         (res (-concat fs1 fs2))
         ) res))

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
  

(defun ej/suggest-context-commands ()
  (interactive)
  (let* ((prev-output (ej/get-previous-cmd-output))
         (commands (ej/parse-commands-from-history prev-output))
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

(pretty-hydra-define ej/shell-helper (:foreign-keys warn :exit t :quit-key "q")
  (
   "Tools"
   (
    ("l" (insert-send "alias l=\"ls -al\"") "alias l=\"ls -al\"")
    ("s" (insert "chmod +x *.sh") "chmod +x *.sh")
    ("j" (insert "python -m json.tool --no-ensure-ascii") "python prettify via json.tool")
    ("s-i" (ej/suggest-context-commands) "suggest commands from context")
    ("f" (ej/convert-files-to-loop) "convert files output to loop")
    ("?" elpy-rgrep-symbol "find-symbol")
    ("!" (insert-send (format "cd %s" (thing-at-point 'existing-filename))) "cd to dir at point")
    ("<" (insert-send "cd -") "cd -")
    ("/" (insert "fd -e py -x rg -H ") "fd -e py -x rg -H")
    )

   "Git"
   (
    ("m" (insert (completing-read "Choose" (ej/modified-git-files))) "choose modified git file")
    ("L" (insert-send "git branch --list --sort=-committerdate") "insert `git branch --list`")
    ("{" (insert-send "git checkout -") "git co -")
    ("+" ej/start-new-commit "start new commit")
    ("b" (ej/switch-branch) "switch branch")
    ("r" (insert-send (format "cd %s" (shell-command-to-string "git root"))) "cd git root")
    )
   
   "Python"
   (
    ("p" (insert-send "import pandas as pd") "import pandas as pd")
    ("P" (insert-send "from pathlib import Path") "from pathlib import Path")
    ("c" (insert-send "from collections import Counter, defaultdict") "Counter & defaultdict")
    )
   ))

(defun ej/cd-dir-from-stdout (firstp)
  (let* ((last-fpath
          (save-excursion
            (if (not firstp) (forward-line -3)
              (comint-previous-prompt 1)
              (forward-line 1))
            (thing-at-point 'existing-filename)))
         (last-dir
           (if (file-directory-p last-fpath)
               last-fpath
             (file-name-directory last-fpath))))
    (insert (format "cd %s" last-dir))
    (comint-send-input)))

(defun ej/cd-dir-from-stdout-first () (interactive) (ej/cd-dir-from-stdout t))
(defun ej/cd-dir-from-stdout-last () (interactive) (ej/cd-dir-from-stdout nil))

(defun ej/shell-hook ()
  (interactive)
  (local-set-key (kbd "C-c s-r") 'rename-shell)
  (local-set-key (kbd "s-j") 'ej/shell-helper/body)
  (local-set-key (kbd "C-s-j") 'ej/cd-dir-from-stdout)
  (local-set-key (kbd "C-s-j") 'ej/cd-dir-from-stdout-first)
  (local-set-key (kbd "M-s-j") 'ej/cd-dir-from-stdout-last)
  )
(add-hook 'shell-mode-hook 'ej/shell-hook)
