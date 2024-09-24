;; shell: interactive
(defun insert-send (text)
  (interactive)
  (insert text)
  (comint-send-input))

(defun ej/modified-git-files ()
  (s-split "\n" (shell-command-to-string "git ls-files -m")))

(setq ej/branches-cmd "git branch --list --sort=-committerdate")
(setq ej/handy-letters "asfjklghqwertyuiopvbn")

(defun ej/switch-branch ()
  (interactive)
  (let* ((branches-output (shell-command-to-string ej/branches-cmd))
         (branches-0 (s-split "\n" (s-trim branches-output)))
         (branches-1 (--filter (not (s-starts-with? "*" it)) branches-0))
         (branches (-map #'s-trim branches-1))
         (branches-no-dev-master (--remove (-contains? '("master" "develop") it) branches))
         (pairs-no-dev-master (-zip (string-to-list ej/handy-letters) branches-no-dev-master))
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

(defhydra ej/shell-helper (:columns 1 :foreign-keys warn :exit t)
  "shell helper"
  ("p" (insert-send "import pandas as pd") "import pandas as pd")
  ("P" (insert-send "from pathlib import Path") "from pathlib import Path")
  ("c" (insert-send "from collections import Counter, defaultdict") "Counter & defaultdict")
  ("l" (insert-send "alias l=\"ls -al\"") "alias l=\"ls -al\"")
  ("s" (insert "chmod +x *.sh") "chmod +x *.sh")
  ("g" (insert (completing-read "Choose" (ej/modified-git-files))) "choose modified git file")
  ("L" (insert-send "git branch --list --sort=-committerdate") "insert `git branch --list`")
  ("!" (insert-send "git co -") "git co -")
  ("+" ej/start-new-commit "start new commit")
  ("b" (ej/switch-branch) "switch branch")
  ("j" (insert "python -m json.tool --no-ensure-ascii") "python prettify via json.tool")
  ("?" elpy-rgrep-symbol "find-symbol")
  )

(defun ej/shell-hook ()
  (interactive)
  (local-set-key (kbd "C-c s-r") 'rename-shell)
  (local-set-key (kbd "s-j") 'ej/shell-helper/body)
  )
(add-hook 'shell-mode-hook 'ej/shell-hook)
