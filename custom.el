(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
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
   '("23c0dc923626f1186edf9ed406dad5358477434d635ea90012e93863531a97b3" "3f5f69bfa958dcf04066ab2661eb2698252c0e40b8e61104e3162e341cee1eb9" default))
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
   '((eval add-to-list 'imenu-generic-expression
           '("Sallet sources" "\\(^(sallet-defsource +\\)\\(\\_<.+?\\_>\\)" 2))
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
   '(aidermacs gptel color-theme-buffer-local telega protobuf-mode dirvish php-mode vline pcmpl-args elpy iove docker fireplace devdocs ov emacsql-sqlite emacsql ekg hl-todo vertico all-the-icons rg rainbow-delimiters hyperbole avy dot-mode graphviz-dot-mode dockerfile-mode jinja2-mode ansible-doc ansible helm-rg julia-shell julia-mode edraw-org dired-atool consult eat google-translate simple-httpd lispy svg-lib esxml-query esxml exsml elquery ess khoj typescript-mode org-roam windresize flx sallet org-velocity ox-tiddly pdf-tools clojure-mode gradle-mode solidity-mode shx highlight-sexp major-mode-hydra pretty-hydra polymode proof-general org-download org-ref ox-bibtex org-plus-contrib org-contrib org auctex-latexmk 0blayout elogcat request preview-it lsp-mode lsp scroll-on-jump zenburn-theme rust-mode elsa reazon spinner loop helm-bibtex dumb-jump maxima ialign cliphist eros treepy slime nyan-mode keyfreq haskell-mode dumb-mode ein erefactor org-pdftools google-translate-default-ui visual-regexp s quick-yes exec-path-from-shell-initialize quelpa-use-package nav-nav quelpa straight exec-path-from-shell telega-mnz telega-stories dashboard saveplace-pdf-view helm-fish-completion all-the-icons-dired uuidgen helm-projectile jupyter trashed ace-jump-mode chess company-emojify emojify company-posframe company-mode language-detection company visual-fill-column rainbow-identifiers dash transient magit groovy-mode tuareg esup org-fragtog multiple-cursors helm-ag ace-jump suggest buttercup ivy-emoji hydra helm smartparens togetherly markdown-mode swift-mode org-noter-pdftools helm-org cdlatex helpful djvu color-theme tron-legacy-theme helm-system-packages wgrep-helm wgrep wiki-summary helm-org-rifle gnuplot helm-themes leuven-theme dracula-theme session difflib toc-org which-key helm-switch-shell helm-dash helm-ls-git helm-swoop helm-descbinds emms-info-libtag typopunct projectile-ripgrep ripgrep smex pdf-view doom-modeline tex sublimity diff-hl haskell-mode htmlize zzz-to-char yaml-mode yafolding visual-regexp-steroids use-package reverse-im org-pdfview org-mobile-sync kotlin-mode hierarchy emms dired-subtree auctex ag))
 '(safe-local-variable-values
   '((eval setq-local vc-directory-exclusion-list
           (-concat
            '("_data" "_temp_store" "_store")
            vc-directory-exclusion-list))
     (vc-git-annotate-switches . "-w")
     (encoding . cp1251)))
 '(scroll-bar-mode nil)
 '(scroll-step 1)
 '(select-enable-clipboard t)
 '(sentence-end "[.?!;…]\\($\\|\11\\| \\)[\12]*")
 '(session-use-package t nil (session))
 '(tab-width 2)
 '(tool-bar-mode nil)
 '(use-package-selected-packages
   '(visual-regexp-steroids hierarchy dired-subtree org-plus-contrib org-pdfview yafolding ag projectile reverse-im dashboard auctex org-virtual zzz-to-char org-mobile-sync orgit org yaml-mode visual-regexp use-package pdf-tools multiple-cursors kotlin-mode emms async))
 '(version-control t)
 '(world-clock-list
   '(("America/Los_Angeles" "Seattle")
     ("America/New_York" "Stamford")
     ("Europe/Warsaw" "Krakow")
     ("Europe/Moscow" "Saint-Petersburg")
     ("Europe/London" "London")
     ("Europe/Paris" "Paris")
     ("Asia/Calcutta" "Bangalore")
     ("Asia/Tokyo" "Tokyo")))
 '(x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
