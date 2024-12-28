(make-variable-buffer-local (defvar ej/calc-last "" "last evaluated expression"))
(make-variable-buffer-local (defvar ej/calc-fullp nil "is read from beginning of line"))

(setq shell-math-cmd
      (if (= 0(length (shell-command-to-string "which r")))
          "awk \"BEGIN{printf %s}\" | sed 's/,/./g'"
        "r -e 'cat(%s)'"))
;; (setq shell-math-cmd "awk \"BEGIN{printf %s}\" | sed 's/,/./g'")

(defun string/starts-with (string prefix)
  (and (string-match (rx-to-string `(: bos ,prefix) t) string) t))

(setq SEARCH-START-CHARACTER ":")
(defun ej/calculator ()
  (interactive)
  (let* ((p (point))
         (s (- p (current-column)))
         (almost-expr (buffer-substring-no-properties p s))
         (indexof (cl-search SEARCH-START-CHARACTER almost-expr :start2 0))
         (expr (if (null indexof) almost-expr (substring almost-expr (1+ indexof) (length almost-expr))))
         (balanced-expr (ej/balance expr))
         (bash-expr (format shell-math-cmd balanced-expr))
         (evaluated (shell-command-to-string bash-expr))
         (output evaluated))
    (setq ej/calc-fullp (null indexof))
    (setq ej/calc-last output)
    (message output)))
(defun ej/balance (expr)
  (let* ((open (s-count-matches "(" expr))
         (close (s-count-matches ")" expr))
         (diff (- close open)))
    (if (eq diff 0) expr (concat (make-string (max 0 diff) ?\() expr (make-string (max 0 (- diff)) ?\))))))

(defun exit-calc-remove-expr-insert-evaluated ()
  (interactive)
  (ej/calculator-mode -1) 
  (if ej/calc-fullp (kill-line 0)
    (progn
      (let ((cur (point)))
        (re-search-backward SEARCH-START-CHARACTER nil nil 1)
        (kill-region (point) cur))))
  (insert ej/calc-last))

(defun exit-calc-insert-evaluated ()
  (interactive)
  (ej/calculator-mode -1)
  (save-excursion
    (insert ej/calc-last)))

(define-minor-mode ej/calculator-mode
  "my calculator"
  :keymap (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'exit-calc-insert-evaluated)
    (define-key map [(control return)] 'exit-calc-remove-expr-insert-evaluated)
    map)
  (if ej/calculator-mode
      (add-hook 'post-command-hook 'ej/calculator)
    (remove-hook 'post-command-hook 'ej/calculator)))

(defun avg (&rest args)
  (/ (* 1.0 (apply #'+ args)) (length args)))

(defun ej/copy-shrugman ()
  " do in shell: $ emacsclient --no-wait --eval '(ej/copy-shrugman)' "
  (interactive)
  (kill-new "¯\\_(ツ)_/¯"))

(load "ej-quick-copy")

;; working with links in buffer
(defun ej/link-nav (count link-mover link-callback)
  (dotimes (_ count) (funcall link-mover))
  (set-mark (point))
  (funcall link-callback (thing-at-point-url-at-point)))

(defun ej/link-copier (link)
  (when link
    (kill-new link)
    (message "Copied link: %s" link)))

(defun ej/copy-next-link (cnt) (interactive "p") (ej/link-nav cnt      'org-next-link     'ej/link-copier))
(defun ej/copy-prev-link (cnt) (interactive "p") (ej/link-nav cnt      'org-previous-link 'ej/link-copier))
(defun ej/open-next-link (cnt) (interactive "p") (ej/link-nav (1- cnt) 'org-next-link     'org-open-at-point))

(global-set-key (kbd "<f7>")   'ej/copy-next-link)
(global-set-key (kbd "<S-f7>") 'ej/copy-prev-link)
(global-set-key (kbd "<C-f7>") 'ej/open-next-link)

(defun ej/duplicate-line ()
  (interactive)
  (save-excursion
    (let* ((line (thing-at-point 'line)))
      (end-of-line)
      (if (looking-at "\n")
          (forward-line 1)
        (insert "\n"))
      (insert line)))
  (next-line 1))

(defun ej/kill-line (&optional mode)
  "Kill current line saving position from beginning of line."
  (interactive "p")
  (message "mode: %d" mode)
  (ej/kill-line-helper mode nil))

(defun ej/save-kill-line (&optional mode)
  "Save kill current line saving position from beginning of line."
  (interactive "p")
  (ej/kill-line-helper mode t))

(defun ej/save-kill-line2 ()
  "like C-k but save"
  (interactive)
  (save-excursion
    (let* ((cur (point))
           (_ (end-of-line 1)))
       (kill-ring-save cur (point))))
  (message "copied: \"%s\"" (current-kill 0)))

(defun ej/kill-line-helper (mode save-p)
  "Kill current line saving position from beginning of line."
  (interactive)
  (let ((pos (point)))
    (move-beginning-of-line 1)
    (let ((indent (- pos (point))))
      (kill-line mode)
      (if save-p (yank))
      (if (= (point) (point-max))  (previous-line))
      (let ((new_pos (point)))
        (end-of-line)
        (if (> (point) (+ new_pos indent))
            (progn 
              (move-beginning-of-line 1)
              (forward-char indent)))))))
(fset 'ej/kill-rectangle     "\C-xrk")
(fset 'ej/insert-rectangle   "\C-xry")

(defun ej/remove-duplicate-lines()
  (interactive)
  (beginning-of-buffer)
  (replace-regexp "\\([^\n]+\n\\)\\1+" "\\1"))
(global-set-key (kbd "<S-f9>") 'ej/remove-duplicate-lines)

(setq TIME-FORMATS '(
  (1  . "%H:%M")
  (2  . "%d.%m.%y")
  (3  . "upd: %d.%m.%y-%H:%M:%S. ")
  (4  . "date: %d.%m.%y-%H:%M:%S")
  (5 . "[%Y-%m-%d]")
  (6  . "%d.%m.%y-%H:%M:%S")
  (7 . "%Y-%m-%d")
  (8  . "%H:%M:%S")
  (9  . "[%Y-%m-%d %a %H:%M]")
  (10  . "resolution(%d.%m.%y-%H:%M): ")
  (11 . "%a <Dec> %d %H:%M:%S %Y")
  (16 . "[%Y-%m-%d %a]")
  (17  . "%d.%m.%y-%H:%M")
  (18  . ((lambda (time) (time-subtract time (seconds-to-time (* 3 3600)))) . "%Y-%m-%dT%H:%M:%SZ"))
  ))

(setq TIME-FORMAT-DEFAULT "%H:%M")

(defun ej/insert-time (&optional mode)
  (interactive "p")
  (if (eq mode 0)
      (ej/insert-time-hydra)
    (ej/insert-time-key mode)))

(defun ej/apply-time-maker (time-maker-object time)
  (let* ((time-maker-entry (if (stringp time-maker-object)
                               (cons (lambda (time) time) time-maker-object)
                             time-maker-object))
         (time-fmt (cdr time-maker-entry))
         (time-mdf (car time-maker-entry))
         (res (format-time-string time-fmt (funcall time-mdf (current-time))))
         ) res))

(defun ej/make-pretty-time (mode)
  (let* ((time-entry (assoc mode TIME-FORMATS))
         (time-maker-object (if (null time-entry) TIME-FORMAT-DEFAULT (cdr time-entry)))
         (res (ej/apply-time-maker time-maker-object (current-time)))
         ) res))

(defun ej/insert-time-key (&optional mode)
  (insert (ej/make-pretty-time mode)))

(defun ej/time-to-hydra-time (f time)
  (let ((key (car f))
        (time (ej/apply-time-maker (cdr f) time)))
    (list (format "\t\t%s" key) `(insert ,time) time)))

(defun ej/time-hydra-sexp ()
  (let* ((num-ch-subs '((10 . "a") (11 . "b") (16 . "s") (17 . "k") (18 . "l")))
         (hydra-time-formats (cl-sublis num-ch-subs TIME-FORMATS))
         (time (current-time))
         (sexp (--map (ej/time-to-hydra-time it time) hydra-time-formats))
         ) sexp))

(defun ej/insert-time-hydra ()
  (interactive)
  (let* ((sexp (ej/time-hydra-sexp)))
    (call-interactively (eval
                         `(defhydra hydra-insert-time (:exit t :columns 1 :foreign-keys warn)
                            "Hydra insert time"
                            ,@sexp
                            ("\t\tq" nil "quit"))))))

(global-set-key (kbd "s-o") 'ej/insert-time)

(defvar enumerate-line-num)

(defun enumerate-line (start end fmt)
  (string-rectangle-line start end (format fmt enumerate-line-num) t)
  (incf enumerate-line-num))
  
(defun enumerate-rectangle (start end &optional first-number)
"Replace the region-rectangle with numbers beginning at 1 and incrementing for each line.

You can use the universal argument to change the initial value.
For example, to start counting lines at zero:

C-u 0 M-x enumerate-rectangle"
  (interactive "*r\np")
  (setq enumerate-line-num first-number)
  (let (line0 lineN fmt)
    (save-excursion
      (goto-char start)
      (setq line0 (line-number-at-pos))
      (goto-char end)
      (setq lineN (line-number-at-pos)))
    (setq fmt (concatenate 'string 
                           "%" 
                           (format "%0d" (string-width (format "%0d" (+ enumerate-line-num (- lineN line0)))))
                           ".1d"))))

(defun get-by-key (key list)
  (interactive)
  (cdr (assoc key list)))

(defun empty (s)
  (= 0 (length s)))

(defun ej/find-file-goto-line (name &optional arg-type arg-val)
  (interactive)
  (find-file name)
  (pcase arg-type
   (:pos (goto-char arg-val))
   (:line (goto-line arg-val))
   (:str (search-forward arg-val)))
  (end-of-line))

(defalias 'g 'ej/find-file-goto-line)
(defalias 'ffap 'find-file-at-point)

(defun ej/find-file-goto-line-notes (name &optional arg-type arg-val)
  (interactive)
  (ej/find-file-goto-line name arg-type arg-val)
  (org-cycle 2))
(defalias 'gur 'ej/find-file-goto-line-notes)

(defun ej/dired-get-size ()
  " runs command $ du -sch SOME_FILE "
  (interactive)
  (let ((files (dired-get-marked-files)))
    (with-temp-buffer
      (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
      (message "Size of all marked files: %s"
               (progn 
                 (re-search-backward "\\(^[0-9.,]+[A-Za-z]+\\).*total$")
                 (match-string 1))))))
(define-key dired-mode-map (kbd "?") 'ej/dired-get-size)

(defun ej/select-nth-file (&optional file-idx)
  (interactive)
  (revert-buffer)
  (let* (
         (idx (if (null file-idx) 0 (1- file-idx)))
         (dir default-directory)
         (files (->> (directory-files-and-attributes dir nil nil t)
                     (--filter (file-regular-p (expand-file-name (car it) dir)))
                     (--sort (not (time-less-p (nth 6 it) (nth 6 other))))))
         (nth-edited (car (nth idx files))))
    (when nth-edited
      (message nth-edited)
      (beginning-of-buffer)
      (search-forward nth-edited))))

(setq tmp-name1 "/tmp/from-emacs-1")
(setq tmp-name2 "/tmp/from-emacs-2")
(setq tmp-name3-diff "/tmp/from-emacs-3.diff")
(setq tmp-name3-wdiff "/tmp/from-emacs-3.wdiff")

(defun ej/diff-helper (command fname-out)
  "19:00 - 19:11"
  (interactive)
  (delete-file-quite tmp-name1)
  (delete-file-quite tmp-name2)
  (save-excursion
    (let ((point-a (point))
          (_ (exchange-point-and-mark))
          (point-b (point)))
      (write-region point-a point-b tmp-name1 t)))
  (write-region (current-kill 0) nil tmp-name2 'append)
  (shell-command (format "%s %s %s > %s" command tmp-name1 tmp-name2 fname-out))
  (g fname-out))

(defun ej/diff ()
  (interactive)
  (ej/diff-helper "diff" tmp-name3-diff))
(global-set-key (kbd "C-M-=") #'ej/diff)

(defun ej/patch-wdiff (regexp color)
  (beginning-of-buffer)
  (while (re-search-forward regexp nil t)
    (let* ((start (match-beginning 0)))
      (kill-region start (point))
      (insert (propertize (current-kill 0) 'font-lock-face `(:foreground ,color))))))

(defun ej/colorize-wdiff ()
  (interactive)
  (ej/patch-wdiff "\\[-\\(.\\|\n\\)*?-]" "red")
  (ej/patch-wdiff "{\\+\\(.\\|\n\\)*?\\+}" "green"))

(defun ej/wdiff ()
  (interactive)
  (ej/diff-helper "wdiff" tmp-name3-wdiff)
  (ej/colorize-wdiff))

(require 'grep)
(defun search-all-buffers (regexp prefix)
  "Searches file-visiting buffers for occurence of REGEXP.  With
prefix > 1 (i.e., if you type C-u \\[search-all-buffers]),
searches all buffers."
  (interactive (list (grep-read-regexp)
                     current-prefix-arg))
  (message "Regexp is %s; prefix is %s" regexp prefix)
  (multi-occur
   (if (member prefix '(4 (4)))
       (buffer-list)
     (remove-if
      (lambda (b) (some (lambda (rx) (string-match rx  (file-name-nondirectory (buffer-file-name b)))) search-all-buffers-ignored-files))
      (remove-if-not 'buffer-file-name (buffer-list))))

   regexp))
(defcustom search-all-buffers-ignored-files (list (rx-to-string '(and bos (or ".bash_history" "TAGS") eos)))
  "Files to ignore when searching buffers via \\[search-all-buffers]."
  :type 'editable-list)

(defun xah-delete-current-file-make-backup (&optional @no-backup-p)
  "Delete current file, makes a backup~, closes the buffer.
   Backup filename is “‹name›~‹date time stamp›~”. Existing file of the same name is overwritten.
   If the file is not associated with buffer, the backup file name starts with “xx_”.
   When `universal-argument' is called first, don't create backup.
   URL `http://ergoemacs.org/emacs/elisp_delete-current-file.html'
   Version 2016-07-20"
  (interactive "P")
  (let* (
         ($fname (buffer-file-name))
         ($buffer-is-file-p $fname)
         ($backup-suffix (concat "~" (format-time-string "%Y%m%dT%H%M%S") "~")))
    (if $buffer-is-file-p
        (progn
          (save-buffer $fname)
          (when (not @no-backup-p)
            (copy-file
             $fname
             (concat $fname $backup-suffix)
             t))
          (delete-file $fname)
          (message "Deleted. Backup created at 「%s」." (concat $fname $backup-suffix)))
      (when (not @no-backup-p)
        (widen)
        (write-region (point-min) (point-max) (concat "xx" $backup-suffix))
        (message "Backup created at 「%s」." (concat "xx" $backup-suffix))))
    (kill-buffer (current-buffer))))

(defun delete-file-quite (file)
  (if (file-exists-p file) (delete-file file)))

(defun ej/copy-fname-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun ej/copy-buffer-file-name (&optional mode)
  " improvement: C-u fname should copy file at point in dired
    time: 21.02.19:[19:07..19:16]
    resolution: wontfix. Found that <C-0 w> in dired-mode copy absolute path
    time: 06.03.19:[14:42..14:49]: C-0 fname -> copy short file name
  "
  (interactive "p")
  (let* ((file-name (buffer-file-name))
         (prepared-file-name 
          (if (eq mode 0)
              (car (last (split-string file-name "/")))
            file-name)))
     (kill-new prepared-file-name)))
(defalias 'fname 'ej/copy-buffer-file-name)
(defalias 'fname 'ej/copy-fname-to-clipboard)

(defun ej/copy-word ()
  (interactive)
  (set-mark (point))
  (forward-word)
  (kill-ring-save (mark) (point))
  (forward-char))
(global-set-key (kbd "s-d") 'ej/copy-word)

(defun ej/copy-all ()
    "Copy entire buffer to clipboard"
    (interactive)
    (clipboard-kill-ring-save (point-min) (point-max))
    (message "Copy done."))

(defun ej/copy-region-to-temp ()
  (interactive)
  (exchange-point-and-mark)
  (setq begin (point))
  (exchange-point-and-mark)
  (setq myStr (buffer-substring-no-properties begin (point)))
  (setq fname "/tmp/tmp.tmp")
  (delete-file fname)
  (append-to-file begin (point) fname)
  (with-current-buffer "tmp.tmp"
        (when (and (buffer-file-name) (file-exists-p (buffer-file-name)) (not (buffer-modified-p)))
          (erase-buffer)
          (append-to-buffer))))

(defun ej/copy-big-word ()
  (interactive)
  (search-backward-regexp "[^a-zA-Z0-9-\./]")
  (forward-char)
  (setq begin (point))
  (search-forward-regexp "[^a-zA-Z0-9-\./]")
  (kill-ring-save begin (point)))

(defun ej/x11-yank-image-at-point-as-image ()
  "Yank the image at point to the X11 clipboard as image/png.
   https://emacs.stackexchange.com/questions/41016/how-can-i-yank-images-from-emacs
  "
  (interactive)
  (let ((image (get-text-property (point) 'display)))
    (if (eq (car image) 'image)
        (let ((data (plist-get (cdr image) ':data))
              (file (plist-get (cdr image) ':file)))
          (cond (data
                 (with-temp-buffer
                   (insert data)
                   (call-shell-region
                    (point-min) (point-max)
                    "xclip -i -selection clipboard -t image/png")))
                (file
                 (if (file-exists-p file)
                     (start-process
                      "xclip-proc" nil "xclip"
                      "-i" "-selection" "clipboard" "-t" "image/png"
                      "-quiet" (file-truename file))))
                (t 
                (message "The image seems to be malformed."))))
      (message "Point is not at an image."))))

(defun is-pdf-buffer () (s-ends-with-p ".pdf" (buffer-file-name)))

(defun ej/org-screenshot ()
  "Take a screenshot into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
  (interactive)
  (let ((movep (if (is-pdf-buffer) (progn (other-window 1) t) nil))
        (filename
         (concat
          (make-temp-name
           (concat (buffer-file-name)
                   "_"
                   (format-time-string "%Y%m%d_%H%M%S_")) ) ".png")))
    (call-process "import" nil nil nil filename)
    (insert (concat "[[" filename "]]\n\n"))
    (org-display-inline-images)
    (if movep (other-window 1))))

(defun ej/org-screenshot-from-clipboard ()
  " Creates time stamped file with image from clipboard and inserts it in org-buffer "
  (interactive)
  (let* ((fname
          (concat (make-temp-name
                   (concat (buffer-file-name)
                           "_"
                           (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
         (fname-full (expand-file-name fname))
         (cmd (format "xclip -selection clipboard -t image/png -o > %s" fname-full)))
    (shell-command cmd)
    (insert (concat "[[" fname "]]\n\n"))
    (org-display-inline-images)))

(global-set-key (kbd "<s-f3>") 'ej/org-screenshot)

(defun save-image-rotation ()
  " save in place rotated image with Image Magick "
  (interactive)
  (pcase (image-property (image--get-image) :rotation)
    ('nil (message "No rotation"))
    (rot (let* ((img (buffer-file-name))
                (cmd (format "convert %s -rotate %d" img rot)))
           (shell-command cmd)
           (message "Saved rotated image (%d)" rot)))))

(define-key image-mode-map (kbd "C-x C-s") #'save-image-rotation)

(defun ej/google-it (&optional input-seq)
  (interactive)
  (let* ((query (or
                 input-seq
                 (buffer-substring (mark-marker) (point))))
         (url (concat "https://www.google.ru/search?q=" query)))
    (browse-url url)))

(defun ej/anti-zap-to-char (arg char)
  "Zap to a character"
  (interactive "p\nc my Zap to char: ")
  (setq begin (point)) 
  (re-search-forward (format "[^%c]" char))
  (backward-char 1)
  (kill-region begin (point)))
(global-set-key (kbd "s-z") #'ej/anti-zap-to-char)

(defun ej/saved-zap-to-char (arg char)
  "Zap to a character"
  (interactive "p\nc my saved Zap to char: ")
  (setq begin (point))
  (search-forward (char-to-string char))
  (backward-char 1)
  (kill-ring-save begin (point)))

(defun ej/insert-macros ()
  (interactive)
  (name-last-kbd-macro 'a)
  (insert-kbd-macro 'a))

(fset 'ej/open-last-file
   [f3 ?\C-f ?\M-p return])

(defun ej/sum ()
  (interactive)
  (setq end0 (point))
  (exchange-point-and-mark)
  (setq start0 (point))
  (exchange-point-and-mark)
  (let* ((start (min start0 end0))
         (end (max start0 end0)))
        (goto-char start)
        (insert "(+ ")
        (goto-char (+ end 3))
        (insert ")")
        (ej/eval-replace)))

;; http://www.emacswiki.org/emacs/KillingBuffers
(defun ej/close-all-dirs ()
       "Kill all dired buffers. Also IbufferMode: simply type C-x C-b * / D yes RET."
       (interactive)
       (save-excursion
         (let ((count 0))
           (dolist (buffer (buffer-list))
             (set-buffer buffer)
             (when (equal major-mode 'dired-mode)
               (setq count (1+ count))
               (kill-buffer buffer)))
           (message "Killed %i dired buffer(s)." count))))

(defun ej/get-cur-dir ()
  (interactive)
  (save-excursion
    (re-search-backward (rx ":" (group (1+ any)) " $"))
    (buffer-substring-no-properties (match-beginning 1) (en (match-end 1)))))

(require 'url)
(defun insert-image-from-url (&optional url)
  (interactive)
  (unless url (setq url (url-get-url-at-point)))
  (unless url
    (error "Couldn't find URL."))
  (let ((buffer (url-retrieve-synchronously url)))
    (unwind-protect
         (let ((data (with-current-buffer buffer
                       (goto-char (point-min))
                       (search-forward "\n\n")
                       (buffer-substring (point) (point-max)))))
           (insert-image (create-image data nil t)))
      (kill-buffer buffer))))
(setq shr-max-image-proportion 0.3)

(defalias 'strip 's-trim)

;; https://stackoverflow.com/questions/15869131/emacs-shell-command-on-buffer
; todo fix. Now it doesn't work
(defun shell-command-on-buffer (command)
  (interactive "sShell command on buffer: ")
  (save-excursion
    (shell-command-on-region (point-min) (point-max) command)))

(defun ej/remove-new-lines ()
  (interactive)
  (replace-string "\n" " ")
  (move-beginning-of-line 1)
  (query-replace "- " ""))

(fset 'ej/run-last-command
   [?\M-x ?\M-p return])

(defun ej/jump-to(arg)
    (interactive)
    (search-forward arg))
(defalias 'mjt 'ej/jump-to)

(defun swap-buffers-in-windows ()
  "Put the buffer from the selected window in next window, and vice versa
   https://stackoverflow.com/a/1774949/14354364 "
  (interactive)
  (let* ((this (selected-window))
     (other (next-window))
     (this-buffer (window-buffer this))
     (other-buffer (window-buffer other)))
    (set-window-buffer other this-buffer)
    (set-window-buffer this other-buffer)
    )
  )

(defun sudo-edit (&optional arg)
  "Edit currently visited file as root. With a prefix ARG prompt for a file to visit.
   Will also prompt for a file to visit if current buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
           (if (frame-parameter nil 'fullscreen) nil 'fullboth)))

(defun ej/reopen ()
  (interactive)
  (let ((fn (buffer-file-name))
        (pnt (point)))
    (when (not (null fn))
      (kill-this-buffer)
      (find-file fn)
      (goto-char pnt))))

(defun last-killed ()
  (interactive)
  (substring-no-properties (car kill-ring)))

(defun last-killed-ext ()
  (interactive)
  (with-temp-buffer
    (yank)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun ej/search-next (c)
  (interactive "cEnter character: ")
  (let* ((cc (char-to-string c))
         (rcc (rx-to-string cc)))
    (if (looking-at rcc)
        (forward-char))
    (search-forward cc)
    (backward-char)))
(global-set-key (kbd "s-[") #'ej/search-next)

(defun ej/copy-buffer ()
  " copy buffer content "
  (buffer-substring-no-properties (point-min) (point-max)))

(defun ej/is-not-interesting-command ()
  (save-excursion
    (move-beginning-of-line 1)
    (let* ((cmd (thing-at-point 'line))
           (res (or
                 (null cmd)
                 (s-starts-with? "rg " cmd)
                 (< (length cmd) 3)))
           ) res)))

(defun ej/is-inside-program ()
  (save-excursion
    (backward-char 2)
    (not (equal "$" (thing-at-point 'char)))))

(defun ej/prog-mode-hook ()
  (local-set-key (kbd "C-<return>") #'ej/run-other-window))
(add-hook 'prog-mode-hook 'ej/prog-mode-hook)

(setq default-mode-line-format mode-line-format)
(setq default-frame-title-format frame-title-format)
(setq is-presentation-mode nil)

(defun ej/toggle-presentation-mode ()
  (interactive)
  (let* ((p-mode  (not is-presentation-mode))
         (m-line  (if p-mode nil default-mode-line-format))
         (f-title (if p-mode "emacs" default-frame-title-format)))
    (setq-default mode-line-format     m-line)
    (setq         mode-line-format     m-line)
    (setq         frame-title-format   f-title)
    (setq         is-presentation-mode p-mode)))

(defun ej/toggle-pdf-org ()
  (interactive)
  (let* ((bfn buffer-file-name)
         (fn-no-ext (file-name-sans-extension bfn))
         (fn-ext (file-name-extension bfn))
         (new-ext (if (equal "org" fn-ext) "pdf" "org"))
         (new-bfn (concat fn-no-ext "." new-ext))
         (buf (find-file-noselect new-bfn))
         )
    (switch-to-buffer buf)))

(defun ej/split-show-dired ()
  (interactive)
  (split-window-horizontally)
  (other-window 1)
  (dired-jump))

(defun ej/dired-in-other-window ()
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (other-window 1)
  (dired-jump))

(defalias 'async-cmd 'async-shell-command)
(defhydra ej/hydra-buffers-windows (:foreign-keys warn :columns 1)
  " Hydra navigation stuff "
  ("!" ej/reopen "reopen" :exit t)
  ("@" fname "copy file name" :exit t)
  ("2" ej/split-show-dired "split and dired" :exit t)
  ("3" ej/src-code-with-asmtools "open code block with asmtools" :exit t)
  ("e" split-window-horizontally "split horizontally")
  ("v" split-window-vertically "split vertically")
  ("o" other-window "other window")
  ("q" delete-other-windows "delete-other-windows")
  ("0" delete-window "delete-window")
  ("s" swap-buffers-in-windows "swap windows" :exit t)
  ("t" ej/toggle-pdf-org "toggle pdf <-> org" :exit t)
  ("T" ej/toggle-theme "toggle theme" :exit t)
  ("m" (switch-to-buffer "*Messages*") "*Messages*" :exit t)
  ("p" (switch-to-buffer "*Packages*") "*Packages*" :exit t)
  ("P" (switch-to-buffer "*Python*") "*Python*" :exit t)
  ("f" fname "Copy path to file of buffer" :exit t)
  ("c" ej/copy-all "Copy buffer" :exit t)
  ("<f1>" windresize "windresize" :exit t)
  ("<f8>" ej/dired-in-other-window "Dired in other window" :exit t)
  ("<ESC>" nil "quit")
)

(global-set-key (kbd "<M-f8>") 'ej/hydra-buffers-windows/body)
(suppress-keymap ej/hydra-buffers-windows/keymap)

(defun ej/title-by-url (url)
  (let* ((command (format "wget -qO- %s |  gawk -v IGNORECASE=1 -v RS='</title' 'RT{gsub(/.*<title[^>]*>/,\"\");print;exit}'" url))
         (res (s-trim (shell-command-to-string command)))
         ) res))

(use-package esxml
  :config
  (require 'esxml-query))

(defun ej/title-by-url (url)
  (let* ((root (with-current-buffer (url-retrieve-synchronously url)
                 (libxml-parse-html-region (point-min) (point-max))))
         (res (car (dom-children (esxml-query "title" root))))
         ) res))

(defun ej/yank-downloaded-title ()
  "insert in this buffer title for yanked url"
  (interactive)
  (let* ((url (--first (s-starts-with? "http" it) kill-ring))
         (url-title (ej/title-by-url url)))
    (insert (ej/title-by-url url))))

(defalias 'titlize 'ej/insert-title)

(defun ej/yank-double-slashes ()
  (interactive)
  (insert (s-replace "\\" "\\\\" (current-kill 0))))

(defun ej/yank-encode-wiki-links ()
  (interactive)
  (let* ((killed (substring-no-properties (current-kill 0)))
         (is-link (cl-search "://" killed))
         (patched (--> killed
                       (if is-link (decode-coding-string (url-unhex-string it) 'utf-8) it)
                       (s-replace " " "%20" it))))
    (insert patched)))

(defun ej/yank-title-as-filename ()
  (interactive)
  (let* ((killed (substring-no-properties (current-kill 0)))
         (patched (->> killed
                       (downcase)
                       (s-replace-regexp "[\s\n_–]" "-")
                       (s-replace-regexp "[,:*]" ""))))
    (insert patched)))

(defun ej/yank-link-to-wiki ()
  (interactive)
  (let* ((link (substring-no-properties (current-kill 0)))
         (title (--> link
                     (cadr (split-string it "/wiki/"))
                     (decode-coding-string (url-unhex-string it) 'utf-8)
                     (s-replace "_" " " it))))
    (insert (format "[[%s][wiki: %s]]" link title))))

(defun ej/yank-python-onelinefy ()
  (interactive)
  (let* ((killed (substring-no-properties (current-kill 0)))
         (patched (->> killed
                       (s-replace-regexp "\n *" "; ")
                       (s-replace ":;" ":")
                       (s-trim))))
     (insert (format "`%s`" patched))))

(defhydra ej/hydra-yank (:foreign-keys warn :columns 1 :exit t)
  " Yank wrappers "
  ("t" ej/yank-downloaded-title "download url title")
  ("/" ej/yank-double-slashes "fix slashes")
  ("w" ej/yank-encode-wiki-links "encode wiki links")
  ("f" ej/yank-title-as-filename "as filename")
  ("l" ej/yank-link-to-wiki "org-link to wiki")
  ("p" ej/yank-python-onelinefy "Python onelinefy")
  ("<ESC>" nil "exit"))
(global-set-key (kbd "C-s-y") 'ej/hydra-yank/body)

(defun ej/drop-brackets-if-has (str)
  (cl-flet ((start-end? (p s) (and (s-starts-with? p str) (s-ends-with? s str))))
    (if (or
         (start-end? "[" "]")
         (start-end? "(" ")")
         (start-end? "{" "}"))
        (substring str 1 (1- (length str)))
      str)))

(defun ej/rg-sexp-at-point ()
  (interactive)
  (let* ((sexp (thing-at-point 'sexp))
         (str (ej/drop-brackets-if-has str))
         (regexp (eval `(rx ,str)))
         (default-directory (projectile-project-root)))
    (helm-rg regexp t)))

(defun ej/unscreen-string ()
  (interactive)
  (let* ((text (buffer-substring-no-properties (mark) (point))))
    (switch-to-buffer "*unscreen*")
    (erase-buffer)
    (insert (read text))
    (goto-char (point-min))))

(require 'ffap)

(defun ej/open-go (filename line-number column-number token)
  (find-file filename)
  (cond
   ((and (not (null line-number)) (/= 0 line-number))
    (progn (goto-char (point-min))
           (forward-line (1- line-number))
           (forward-char column-number)))
   ((not (null token))
    (progn
      (goto-char (point-min))
      (condition-case nil
          (search-forward token)
        (error nil))))
   ))

(defun ej/ffap-guesser (arg)
  (with-temp-buffer
    (insert arg)
    (goto-char (point-max))
    (ffap-guesser)))
(defun ej/file-exists-p (fp)
  (and (not (null fp))
       (file-exists-p fp)))

(defun find-file-at-point-with-line (&optional filename)
  "Opens file at point and moves point to line specified next to file name."
  (interactive)
  (let* ((filename-guess (ffap-guesser))
         (filename (or filename (if current-prefix-arg (ffap-prompter) filename-guess)))
;;         (filename (when (and (not (null filename)) (s-contains? ":" filename))
;;                     (car (s-split ":" filename))))
         (filename-fix (when filename-guess
                         (or
                          (ej/ffap-guesser (car (s-split ":" filename-guess)))
                          (ej/ffap-guesser (s-replace "." "/" (car (s-split ":" filename-guess)))))))
         (line-number
          (and (or (looking-at ".* line \\(\[0-9\]+\\)")
                   (looking-at "[^:]*[\\(:]\\(\[0-9\]+\\)"))
               (string-to-number (match-string-no-properties 1))))
         (column-number
          (or 
           (and (looking-at "[^:]*:\[0-9\]+:\\(\[0-9\]+\\)")
                (string-to-number (match-string-no-properties 1)))
           (let 'column-number 0)))
          (token
           (and (looking-at "[^:\\n]*:\\(\\w+\\)")
                (match-string-no-properties 1)))
          )
    (cond ((ffap-url-p filename)
           (let (current-prefix-arg)
             (funcall ffap-url-fetcher filename)))
          ((ej/file-exists-p filename) (ej/open-go filename line-number column-number token))
          ((ej/file-exists-p filename-fix) (ej/open-go filename-fix line-number column-number token))
          ((and ffap-pass-wildcards-to-dired
                ffap-dired-wildcards
                (string-match ffap-dired-wildcards filename))
           (funcall ffap-directory-finder filename))
          ((and ffap-dired-wildcards
                (string-match ffap-dired-wildcards filename)
                find-file-wildcards
                ;; Check if it's find-file that supports wildcards arg
                (memq ffap-file-finder '(find-file find-alternate-file)))
           (funcall ffap-file-finder (expand-file-name filename) t))
          ((or (not find-file-not-found-functions)
               (ej/file-exists-p filename)
               (y-or-n-p "File does not exist, create buffer? "))
           (funcall ffap-file-finder
                    ;; expand-file-name fixes "~/~/.emacs" bug sent by CHUCKR.
                    (expand-file-name filename)))
          ;; User does not want to find a non-existent file:
          ((signal 'file-error (list "Opening file buffer"
                                     "no such file or directory"
                                     filename))))))

(global-set-key (kbd "s-g") #'find-file-at-point-with-line)

;; files
(defun remove-top-files (dir files count)
  (let* ((sub-files (-slice files 0 (min count (length files)))))
    (--map (delete-file (expand-file-name it dir)) sub-files)))

(setq fregexp "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)")
(defun get-files-sorted-by-update (dir)
  (let ((files (sort (directory-files-and-attributes dir nil fregexp)
    #'(lambda (x y) (not (time-less-p (nth 6 x) (nth 6 y)))))))
    (--map (car it) files)))

(defun remove-last-files (dir count)
  (remove-top-files dir (get-files-sorted-by-update dir) count))

;; layout
(defun ej/is-current-layout-ru ()
  (interactive)
  (equal (shell-command-to-string "xkblayout-state print %s") "ru"))
(defun ej/switch-layout ()
  (shell-command-to-string "xkblayout-state set +1"))

(defun is-integer (str)
  (condition-case nil
      (progn
        (cl-parse-integer str)
        t)
    (error nil)))

