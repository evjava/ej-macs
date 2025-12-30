(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3"
    "DeepSkyBlue" "gray50"])
 '(async-shell-command-buffer 'new-buffer)
 '(auto-save-list-file-prefix "~/.emacs.d/.local/auto-save-list/.saves-")
 '(backup-directory-alist '((".*" . "~/.emacs.d/.local/autosave/")))
 '(blink-cursor-mode nil)
 '(browse-url-browser-function 'browse-url-chromium)
 '(browse-url-generic-program "chromium")
 '(c-basic-offset t)
 '(calendar-latitude 59.95)
 '(calendar-location-name "Saint-Petersburg")
 '(calendar-longitude 30.316667)
 '(calendar-time-zone 240)
 '(column-number-mode t)
 '(comint-password-prompt-regexp
   "\\(^ *\\|\\( SMB\\|'s\\|Bad\\|CVS\\|Enter\\(?: \\(?:\\(?:sam\\|th\\)e\\)\\)?\\|Kerberos\\|LDAP\\|New\\|Old\\|Repeat\\|UNIX\\|\\[sudo]\\|enter\\(?: \\(?:\\(?:sam\\|th\\)e\\)\\)?\\|login\\|new\\|old\\) +\\)\\(?:\\(?:adgangskode\\|contrase\\(?:\\(?:ny\\|ñ\\)a\\)\\|geslo\\|h\\(?:\\(?:asł\\|esl\\)o\\)\\|iphasiwedi\\|jelszó\\|l\\(?:ozinka\\|ösenord\\)\\|m\\(?:ot de passe\\|ật khẩu\\)\\|pa\\(?:rola\\|s\\(?:ahitza\\|s\\(?: phrase\\|code\\|ord\\|phrase\\|wor[dt]\\)\\|vorto\\)\\)\\|s\\(?:alasana\\|enha\\|laptažodis\\)\\|wachtwoord\\|лозинка\\|PASSWORD\\|.*Password.*\\|пароль\\|ססמה\\|كلمة السر\\|गुप्तशब्द\\|शब्दकूट\\|গুপ্তশব্দ\\|পাসওয়ার্ড\\|ਪਾਸਵਰਡ\\|પાસવર્ડ\\|ପ୍ରବେଶ ସଙ୍କେତ\\|கடவுச்சொல்\\|సంకేతపదము\\|ಗುಪ್ತಪದ\\|അടയാളവാക്ക്\\|රහස්පදය\\|ពាក្យសម្ងាត់\\|パスワード\\|密[码碼]\\|암호\\)\\|Response\\)\\(?:\\(?:, try\\)? *again\\|(empty for no passphrase)\\| (again)\\)?\\(?: for [^:：៖]+\\)?[:：៖]\\s *\\'")
 '(custom-safe-themes
   '("23c0dc923626f1186edf9ed406dad5358477434d635ea90012e93863531a97b3"
     "3f5f69bfa958dcf04066ab2661eb2698252c0e40b8e61104e3162e341cee1eb9"
     default))
 '(cv-basic-offset t)
 '(debug-on-error t)
 '(default-tab-width 2)
 '(delete-by-moving-to-trash t)
 '(delete-old-versions t)
 '(desktop-dirname "~/trash" t)
 '(dired-listing-switches "-aD")
 '(display-time-24hr-format t)
 '(display-time-day-and-date nil)
 '(display-time-mode t)
 '(file-name-shadow-mode t)
 '(global-auto-revert-mode t)
 '(global-hl-line-mode nil)
 '(helm-completion-style 'emacs)
 '(help-window-select t)
 '(hl-sexp-background-color "#33323e")
 '(horizontal-scroll-bar-mode nil)
 '(ignored-local-variable-values
   '((eval and buffer-file-name
           (not (eq major-mode 'package-recipe-mode))
           (or (require 'package-recipe-mode nil t)
               (let ((load-path (cons "../package-build" load-path)))
                 (require 'package-recipe-mode nil t)))
           (package-recipe-mode))
     (eval add-to-list 'imenu-generic-expression
           '("Sallet sources"
             "\\(^(sallet-defsource +\\)\\(\\_<.+?\\_>\\)" 2))
     (vc-prepare-patches-separately)
     (diff-add-log-use-relative-names . t)
     (vc-git-annotate-switches . "-w")))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-major-mode 'fundamental-mode)
 '(initial-scratch-message nil)
 '(interprogram-paste-function 'x-cut-buffer-or-selection-value t)
 '(kept-new-versions 5)
 '(kill-ring-max 500)
 '(large-file-warning-threshold 25000000)
 '(load-home-init-file t t)
 '(ls-lisp-dirs-first t)
 '(ls-lisp-format-time-list '("%y-%m-%d %H:%M" "%y-%m-%d %H:%M"))
 '(ls-lisp-use-localized-time-format t)
 '(ls-lisp-verbosity '(links))
 '(make-backup-files t)
 '(menu-bar-mode nil)
 '(org-adapt-indentation 'headline-data)
 '(org-clock-display-default-range 'untilnow)
 '(org-edit-src-content-indentation 0)
 '(org-export-backends '(ascii html icalendar latex md odt))
 '(org-startup-indented nil)
 '(org-tags-column 40)
 '(package-check-signature nil)
 '(package-selected-packages
   '(0blayout ace-jump ace-jump-mode ag aidermacs all-the-icons
              all-the-icons-dired ansible ansible-doc auctex
              auctex-latexmk avy buttercup cdlatex chess cliphist
              clojure-mode color-theme color-theme-buffer-local
              company company-emojify company-mode company-posframe
              consult dash dashboard devdocs diff-hl difflib
              dired-atool dired-subtree dirvish djvu docker
              dockerfile-mode doom-modeline dot-mode dracula-theme
              dumb-jump dumb-mode eat edraw-org eglot ein ekg elogcat
              elpy elquery elsa emacsql emacsql-sqlite emms
              emms-info-libtag emojify erefactor eros ess esup esxml
              esxml-query exec-path-from-shell
              exec-path-from-shell-initialize expand-region expreg
              exsml fireplace flx flymake-ruff gnuplot
              google-translate google-translate-default-ui gptel
              gradle-mode graphviz-dot-mode groovy-mode haskell-mode
              haskell-mode helm helm-ag helm-bibtex helm-dash
              helm-descbinds helm-fish-completion helm-ls-git helm-org
              helm-org-rifle helm-projectile helm-rg helm-switch-shell
              helm-swoop helm-system-packages helm-themes helpful
              hierarchy highlight-sexp hl-todo htmlize hydra hyperbole
              ialign iove ivy-emoji jinja2-mode json-mode julia-mode
              julia-shell jupyter keyfreq khoj kotlin-mode
              language-detection leaf leuven-theme lispy loop lsp
              lsp-mode magit major-mode-hydra markdown-mode maxima mcp
              multiple-cursors nav-nav nix-mode nyan-mode org
              org-contrib org-download org-fragtog org-mobile-sync
              org-noter-pdftools org-pdftools org-pdfview
              org-plus-contrib org-ql org-ref org-roam
              org-transclusion org-velocity ov ox-bibtex ox-tiddly
              pcmpl-args pdf-tools pdf-view php-mode plantuml-mode
              poly-markdown polymode pretty-hydra preview-it
              projectile-ripgrep proof-general protobuf-mode quelpa
              quelpa-use-package quick-yes rainbow-delimiters
              rainbow-identifiers reazon request reverse-im rg ripgrep
              ruff-format rust-mode s sallet saveplace-pdf-view
              scroll-on-jump session shx simple-httpd slime
              smartparens smex solidity-mode spinner straight
              sublimity suggest svg-lib swift-mode telega telega-mnz
              telega-stories tex timeout toc-org togetherly transient
              trashed treepy treesit tron-legacy-theme tuareg
              typescript-mode typopunct use-package uuidgen vertico
              visual-fill-column visual-regexp visual-regexp-steroids
              vline wgrep wgrep-helm which-key wiki-summary windresize
              yafolding yaml-mode zenburn-theme zzz-to-char))
 '(safe-local-variable-values
   '((eval setq-local vc-directory-exclusion-list
           (-concat '("_data" "_temp_store" "_store")
                    vc-directory-exclusion-list))
     (vc-git-annotate-switches . "-w") (encoding . cp1251)))
 '(scroll-bar-mode nil)
 '(scroll-step 1)
 '(select-enable-clipboard t)
 '(sentence-end "[.?!;…]\\($\\|\11\\| \\)[\12]*")
 '(session-use-package t nil (session))
 '(tab-width 2)
 '(tool-bar-mode nil)
 '(use-package-selected-packages
   '(visual-regexp-steroids hierarchy dired-subtree org-plus-contrib
                            org-pdfview yafolding ag projectile
                            reverse-im dashboard auctex org-virtual
                            zzz-to-char org-mobile-sync orgit org
                            yaml-mode visual-regexp use-package
                            pdf-tools multiple-cursors kotlin-mode
                            emms async))
 '(version-control t)
 '(world-clock-list
   '(("America/Los_Angeles" "Seattle") ("America/New_York" "Stamford")
     ("Europe/Warsaw" "Krakow") ("Europe/Moscow" "Saint-Petersburg")
     ("Europe/London" "London") ("Europe/Paris" "Paris")
     ("Asia/Calcutta" "Bangalore") ("Asia/Tokyo" "Tokyo")))
 '(x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
