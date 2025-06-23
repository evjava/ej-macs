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

