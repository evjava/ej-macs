(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(auto-save-list-file-prefix "~/.emacs.d/.local/auto-save-list/.saves-")
 '(backup-directory-alist '((".*" . "~/.emacs.d/.local/autosave/")))
 '(blink-cursor-mode nil)
 '(browse-url-browser-function 'browse-url-generic)
 '(browse-url-generic-program "chromium-browser")
 '(c-basic-offset t)
 '(calendar-latitude 59.95)
 '(calendar-location-name "Saint-Petersburg")
 '(calendar-longitude 30.316667)
 '(calendar-time-zone 240)
 '(column-number-mode t)
 '(comint-password-prompt-regexp
   "\\(^ *\\|\\( SMB\\|'s\\|Bad\\|CVS\\|Enter\\(?: \\(?:\\(?:sam\\|th\\)e\\)\\)?\\|Kerberos\\|LDAP\\|New\\|Old\\|Repeat\\|UNIX\\|\\[sudo]\\|enter\\(?: \\(?:\\(?:sam\\|th\\)e\\)\\)?\\|login\\|new\\|old\\) +\\)\\(?:\\(?:adgangskode\\|contrase\\(?:\\(?:ny\\|ñ\\)a\\)\\|geslo\\|h\\(?:\\(?:asł\\|esl\\)o\\)\\|iphasiwedi\\|jelszó\\|l\\(?:ozinka\\|ösenord\\)\\|m\\(?:ot de passe\\|ật khẩu\\)\\|pa\\(?:rola\\|s\\(?:ahitza\\|s\\(?: phrase\\|code\\|ord\\|phrase\\|wor[dt]\\)\\|vorto\\)\\)\\|s\\(?:alasana\\|enha\\|laptažodis\\)\\|wachtwoord\\|лозинка\\|пароль\\|ססמה\\|كلمة السر\\|गुप्तशब्द\\|शब्दकूट\\|গুপ্তশব্দ\\|পাসওয়ার্ড\\|ਪਾਸਵਰਡ\\|પાસવર્ડ\\|ପ୍ରବେଶ ସଙ୍କେତ\\|கடவுச்சொல்\\|సంకేతపదము\\|ಗುಪ್ತಪದ\\|അടയാളവാക്ക്\\|රහස්පදය\\|ពាក្យសម្ងាត់\\|パスワード\\|密[码碼]\\|암호\\)\\|Response\\)\\(?:\\(?:, try\\)? *again\\|(empty for no passphrase)\\| (again)\\)?\\(?: for [^:：៖]+\\)?[:：៖]\\s *\\'")
 '(custom-safe-themes
   '("23c0dc923626f1186edf9ed406dad5358477434d635ea90012e93863531a97b3" "3f5f69bfa958dcf04066ab2661eb2698252c0e40b8e61104e3162e341cee1eb9" default))
 '(cv-basic-offset t)
 '(debug-on-error nil)
 '(default-tab-width 2)
 '(delete-by-moving-to-trash t)
 '(delete-old-versions t)
 '(desktop-dirname "~/trash" t)
 '(dired-async-mode 0 nil nil "Customized with use-package async")
 '(dired-listing-switches "-aD")
 '(display-time-24hr-format t)
 '(display-time-day-and-date nil)
 '(display-time-mode t)
 '(display-time-world-list
   '(("America/Los_Angeles" "Seattle")
     ("America/New_York" "Stamford")
     ("Europe/Warsaw" "Krakow")
     ("Europe/Moscow" "Saint-Petersburg")
     ("Europe/London" "London")
     ("Europe/Paris" "Paris")
     ("Asia/Calcutta" "Bangalore")
     ("Asia/Tokyo" "Tokyo")))
 '(file-name-shadow-mode t)
 '(global-auto-revert-mode t)
 '(global-hl-line-mode nil)
 '(helm-completion-style 'emacs)
 '(help-window-select t)
 '(hl-sexp-background-color "#33323e")
 '(horizontal-scroll-bar-mode nil)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
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
   '(ein erefactor org-pdftools org-ref pdf-tools google-translate-default-ui visual-regexp s quick-yes exec-path-from-shell-initialize quelpa-use-package nav-nav quelpa straight exec-path-from-shell telega-mnz telega-stories dashboard saveplace-pdf-view helm-fish-completion all-the-icons-dired uuidgen helm-projectile jupyter trashed ace-jump-mode chess company-emojify emojify company-posframe company-mode language-detection company visual-fill-column rainbow-identifiers dash transient magit groovy-mode lsp-mode tuareg esup org-fragtog multiple-cursors helm-ag ace-jump suggest buttercup ivy-emoji hydra helm smartparens togetherly markdown-mode swift-mode org-noter-pdftools helm-org cdlatex helpful djvu color-theme tron-legacy-theme helm-system-packages wgrep-helm wgrep wiki-summary helm-org-rifle gnuplot helm-themes leuven-theme dracula-theme session difflib toc-org which-key helm-switch-shell helm-dash helm-ls-git helm-swoop helm-descbinds emms-info-libtag typopunct google-translate projectile-ripgrep ripgrep smex pdf-view doom-modeline tex sublimity diff-hl zetteldeft haskell-mode htmlize zzz-to-char yaml-mode yafolding visual-regexp-steroids use-package reverse-im org-plus-contrib org-pdfview org-mobile-sync kotlin-mode hierarchy emms dired-subtree auctex ag))
 '(safe-local-variable-values '((encoding . cp1251)))
 '(scroll-bar-mode nil)
 '(scroll-step 1)
 '(select-enable-clipboard t)
 '(sentence-end "[.?!;…]\\($\\|	\\| \\)[
]*")
 '(session-use-package t nil (session))
 '(show-paren-mode t)
 '(tab-width 2)
 '(tool-bar-mode nil)
 '(tron-legacy-theme-softer-bg t nil nil "Customized with use-package tron-legacy-theme")
 '(use-package-selected-packages
   '(visual-regexp-steroids hierarchy dired-subtree org-plus-contrib org-pdfview yafolding ag projectile reverse-im dashboard auctex org-virtual zzz-to-char org-mobile-sync orgit org yaml-mode visual-regexp use-package pdf-tools multiple-cursors kotlin-mode emms async))
 '(version-control t)
 '(x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
