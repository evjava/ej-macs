;;;; for terminal

(quail-define-package
 "cyrillic-jcuken" "Cyrillic" "RU" nil
 "jcuken keyboard layout widely used in Russia (ISO 8859-5 encoding)"
 nil t t t t nil nil nil nil nil t)

(quail-define-rules
 ("`" ?ё) ("1" ?1) ("2"  ?2) ("3" ?3) ("4"  ?4) ("5" ?5) ("6" ?6) ("7" ?7) ("8" ?8) ("9" ?9) ("0"  ?0) ("-"   ?-) ("=" ?=) 
 ("q" ?й) ("w" ?ц) ("e"  ?у) ("r" ?к) ("t"  ?е) ("y" ?н) ("u" ?г) ("i" ?ш) ("o" ?щ) ("p" ?з) ("["  ?х) ("]"   ?ъ) 
 ("a" ?ф) ("s" ?ы) ("d"  ?в) ("f" ?а) ("g"  ?п) ("h" ?р) ("j" ?о) ("k" ?л) ("l" ?д) (";" ?ж) ("'"  ?э) ("\\" ?\\) 
 ("z" ?я) ("x" ?ч) ("c"  ?с) ("v" ?м) ("b"  ?и) ("n" ?т) ("m" ?ь) ("," ?б) ("." ?ю) ("/" ?.) 
 
 ("~" ?Ё) ("!" ?!) ("@" ?\") ("#" ?#) ("$" ?\;) ("%" ?%) ("^" ?:) ("&" ??) ("*" ?*) ("(" ?\() (")"  ?\)) ("_"   ?_) ("+" ?+) 
 ("Q" ?Й) ("W" ?Ц) ("E"  ?У) ("R" ?К) ("T"  ?Е) ("Y" ?Н) ("U" ?Г) ("I" ?Ш) ("O" ?Щ) ("P" ?З) ("{"  ?Х) ("}"   ?Ъ) 
 ("A" ?Ф) ("S" ?Ы) ("D"  ?В) ("F" ?А) ("G"  ?П) ("H" ?Р) ("J" ?О) ("K" ?Л) ("L" ?Д) (":" ?Ж) ("\"" ?Э) ("|"   ?/)
 ("Z" ?Я) ("X" ?Ч) ("C"  ?С) ("V" ?М) ("B"  ?И) ("N" ?Т) ("M" ?Ь) ("<" ?Б) (">" ?Ю) ("?" ?,))

(defun reverse-input-method (input-method)
  "Build the reverse mapping of single letters from INPUT-METHOD."
  (interactive
   (list (read-input-method-name "Use input method (default current): ")))
  (if (and input-method (symbolp input-method))
      (setq input-method (symbol-name input-method)))
  (let ((current current-input-method)
        (modifiers '(nil (control) (meta) (control meta))))
    (when input-method
      (activate-input-method input-method))
    (when (and current-input-method quail-keyboard-layout)
      (dolist (map (cdr (quail-map)))
        (let* ((to (car map))
               (from (quail-get-translation
                      (cadr map) (char-to-string to) 1)))
          (when (and (characterp from) (characterp to))
            (dolist (mod modifiers)
              (define-key (if mod input-decode-map local-function-key-map)
                (vector (append mod (list from)))
                (vector (append mod (list to)))))))))
    (when input-method
      (activate-input-method current))))
(reverse-input-method "cyrillic-jcuken")
(set-input-method 'cyrillic-jcuken)
