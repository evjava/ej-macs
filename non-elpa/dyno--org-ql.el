(use-package org-ql)

(defun org-get-all-subtree ()
  "Get full ORG subtree "
  (save-excursion
    (org-back-to-heading t)
    (filter-buffer-substring (line-beginning-position 1) (org-end-of-subtree t))))

;; todo fix: support all

(setq search-state '(:search "" :search-tags ("python")))

(defun parse-tokens-from-search (search)
  (let* ((search-t (s-trim search))
         (search-tokens (if (= 0 (length search-t)) '()
                          (s-split " " search-t)))
         ) search-tokens))

(defun dyno-make-notes-query (search-state)
  " pure "
  (let* ((search (plist-get search-state :search))
         (search-tokens (parse-tokens-from-search search))
         (search-tokens-sorted (-sort (-on #'>= #'length) search-tokens))
         ;; todo fix API: pass list here
         (tags (plist-get search-state :search-tags))
         (tags-sorted (-sort (-on #'>= #'length) tags))
         (q-search-tokens (--map `(regexp ,it) search-tokens-sorted))
         (q-tags (if (empty tags-sorted) '()
                   (--map `(regexp ,(format "tags:.*%s" it)) tags-sorted)))
         (query-parts (cl-concatenate 'list q-tags q-search-tokens))
         (query (cons 'and query-parts))
         (_ (message "Query: %S" query))
         ) query))

(cl-assert (equal
         (dyno-make-notes-query '(:search "abc defg" :search-tags ("tag1" "tag22")))
         '(and (regexp "tags:.*tag22") (regexp "tags:.*tag1") (regexp "defg") (regexp "abc"))))

(defun dyno-get-cur-paths ()
  (let* ((all-paths (directory-files-recursively curs-root "."))
         (res (--filter (s-ends-with? ".org" it) all-paths))
         ) res))

(defun dyno-org-ql-wrapped (query)
  (org-ql-query
    :select '(org-get-all-subtree)
    :from (dyno-get-cur-paths)
    :where query))

(defun dyno-search-notes-backend--org-ql--inner (search-state)
  (let* ((search (plist-get search-state :search))
         (tags (plist-get search-state :search-tags))
         (res-p (cond
                 ((equal search ":all")
                  (let* ((query (dyno-make-notes-query search-state))
                         (elements (dyno-org-ql-wrapped query)))
                    (list
                     :header (format "Total: %d" (length elements))
                     :items elements)))
                 
                 ((equal search ":tags")
                  '(:header "TODO, :tags not implemented yet..."))
                 
                 ((and (< (length search) 4) (empty tags))
                  '(:header "Too short input..."))
                 
                 ((and (= 0 (length (s-trim search))) (empty tags))
                  '(:header "Empty input..."))
                 
                 (t
                  (let* ((query (dyno-make-notes-query search-state))
                         (elements (dyno-org-ql-wrapped query)))
                    (list
                     :header (format "Total: %d" (length elements))
                     :items elements)))
                 ))
         ) res-p))

;; (plist-get (dyno-search-notes-backend--org-ql--inner '(:search "" :search-tags ("python"))) :header)
;; (dyno-search-notes-backend--org-ql '(:search "" :search-tags ("python")))

(defun dyno-search-notes-backend--org-ql (search-state)
  (message "here: %S" search-state)
  (let* ((res-p (dyno-search-notes-backend--org-ql--inner search-state))
         (_ (message "Header: %S" (plist-get res-p :header)))
         (res (s-trim
               (s-join
                "\n"
                (list
                 (plist-get res-p :header)
                 (format "# sugg-tags: TODO")
                 (s-join "\n" (or (plist-get res-p :items) ""))))))
         ) res))

(defun dyno-suggest-tags--org-ql (tag-search)
  (list (format "%s" "TODO suggest-tags"))
  )
(defun dyno-reload--org-ql ()
  (message "Already done")
  )

(provide 'dyno--org-ql)
