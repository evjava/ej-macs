;;; wdiff-mode.el --- Major mode for wdiff-style output  -*- lexical-binding: t; -*-
(require 'hi-lock)   ; for `hi-red-b' and `hi-green-b'

(setq wdiff-del-regexp "\\[-\\(?:.\\|\n\\)*?-\\]")
(setq wdiff-ins-regexp "{+\\(?:.\\|\n\\)*?+}")
(defgroup wdiff nil "Major mode for wdiff-style output." :group 'text :prefix "wdiff-")
(defcustom wdiff-del-face 'hi-red-b "Face used for wdiff deletions (`[-...-]')." :type 'face :group 'wdiff)
(defcustom wdiff-ins-face 'hi-green-b "Face used for wdiff insertions (`{+...+}')." :type 'face :group 'wdiff)

(defvar wdiff-font-lock-keywords
  `((,wdiff-del-regexp 0 ',wdiff-del-face t)
    (,wdiff-ins-regexp 0 ',wdiff-ins-face t))
  "Font-lock keywords for `wdiff-mode'. Deletions (`[-...-]') and insertions (`{+...+}') are matched non-greedily across newlines.")

;;;###autoload
(define-derived-mode wdiff-mode prog-mode "wdiff"
  "Major mode for wdiff-style output.

wdiff marks deletions as `[-old text-]' and insertions as `{+new text+}'.
This mode highlights deletions with `wdiff-del-face' and insertions with `wdiff-ins-face'.
\\{wdiff-mode-map}"
  :group 'wdiff
  (setq-local font-lock-defaults '(wdiff-font-lock-keywords))
  (setq-local comment-start nil)
  (setq-local parse-sexp-ignore-comments t))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.wdiff\\'" . wdiff-mode))

;;; Org mode integration

(with-eval-after-load 'org
  ;; `#+begin_src wdiff`
  (add-to-list 'org-src-lang-modes '("wdiff" . wdiff))
  ;; Fontify src-block contents in the Org buffer itself.
  (setq org-src-fontify-natively t)
  ;; Make sure `C-c '' on a wdiff block edits it in `wdiff-mode'.
  (with-eval-after-load 'org-src
    (add-to-list
     'org-src-mode-hook
     (lambda () (when (derived-mode-p 'wdiff-mode) (setq-local org-src--allowed-indent 2))))))

;;; Commands

;;;###autoload
(defun wdiff-mode-colorize ()
  "Colorize wdiff markers in the current buffer using overlays.

This is an alternative to enabling `wdiff-mode' when you only want
the highlighting without changing the major mode.  It uses
`highlight-regexp' (see Info node `(emacs)Highlight Interactively')."
  (interactive)
  (highlight-regexp wdiff-del-regexp wdiff-del-face)
  (highlight-regexp wdiff-ins-regexp wdiff-ins-face))

(defun wdiff-mode-uncolorize ()
  "Remove highlighting added by `wdiff-mode-colorize'."
  (interactive)
  (unhighlight-regexp wdiff-del-regexp)
  (unhighlight-regexp wdiff-ins-regexp))

(provide 'wdiff-mode)
;;; wdiff-mode.el ends here
