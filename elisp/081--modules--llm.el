(use-package gptel
  :config
  (global-set-key (kbd "C-s-<return>") 'gptel)
  (setq gptel-default-mode 'org-mode)
  )

(when (not (null mcp-el-load-path))
  (require 'gptel-integrations)
  (use-package mcp
    :ensure t
    :load-path mcp-el-load-path
    :after gptel
    :custom (mcp-hub-servers
             `(("filesystem" . (:command "npx"
                                         :args ("-y" "@modelcontextprotocol/server-filesystem")
                                         :roots ,mcp--filesystem--roots))
               ("fetch" . (:command "uvx" :args ("mcp-server-fetch")))
               ("searxng" . (:command
                             "uvx"
                             :args ("searxng" "--instance-url" "http://localhost:8888/")
                             :env (:SEARXNG_BASE_URL "http://localhost:8888")
                             )
                )))
    :config (require 'mcp-hub)
    ;; todo configure later
    ;; :hook (after-init . mcp-hub-start-all-server)
    ))
