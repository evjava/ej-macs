;; shell: interactive
(defun insert-send (text)
  (interactive)
  (insert text)
  (comint-send-input))

(defun ej/modified-git-files ()
  (s-split "\n" (shell-command-to-string "git ls-files -m")))

(setq ej/branches-cmd "git branch --list --sort=-committerdate")
(setq ej/handy-letters "asdfjklghqwertyuiopvbnm")

(defun ej/switch-branch ()
  (interactive)
  (let* ((branches-output (shell-command-to-string ej/branches-cmd))
         (branches-0 (s-split "\n" (s-trim branches-output)))
         (branches-1 (--filter (not (s-starts-with? "*" it)) branches-0))
         (branches (-map #'s-trim branches-1))
         (pairs (-zip (string-to-list ej/handy-letters) branches))
         (sexp (--map `(,(format "%c" (car it)) (insert-send ,(format "git co %s" (cdr it))) ,(cdr it)) pairs))
         )
    (call-interactively (eval
                         `(defhydra hydra-switch-branch (:exit t :columns 1 :foreign-keys warn)
                            "Hydra switch branch"
                            ,@sexp
                            ("\t\tq" nil "quit"))))))

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
  ("b" (ej/switch-branch) "switch branch")
  ("j" (insert "python -m json.tool --no-ensure-ascii") "python prettify via json.tool")
  ("?" elpy-rgrep-symbol "find-symbol")
  )

(defun ej/shell-hook ()
  (interactive)
  (local-set-key (kbd "C-c s-r") 'rename-shell)
  (local-set-key (kbd "s-l") 'ej/shell-helper/body)
  )
(add-hook 'shell-mode-hook 'ej/shell-hook)
