(setq default-copy-candidates '(("sweat smile" . "😅") ("blush" . "😊") ("ghost" . "👻")))
(setq copy-candidates default-copy-candidates)

(defun ej/candidates-render ()
  (--map (format "%s: %s" (car it) (cdr it)) copy-candidates))

(defun ej/candidates-mapper (x)
  (cadr (s-split-up-to ": " x 1)))

(defun ej/quick-copy ()
  (interactive)
  (helm :sources (helm-build-sync-source "test"
                   :candidates #'ej/candidates-render
                   :fuzzy-match t
                   :action #'(lambda (x) (kill-new (ej/candidates-mapper x))))
        :buffer "*helm quick copy*"))

(defun ej/quick-copy-external ()
  (interactive)
  (let ((buffer (generate-new-buffer "Completions")))
    (switch-to-buffer-other-frame buffer)
    (ej/quick-copy)
    (delete-frame)
    (kill-buffer buffer)))
