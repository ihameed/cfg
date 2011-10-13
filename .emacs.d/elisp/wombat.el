(defun font-lock-fontify-numbers ()
  "Use this function as a hook to fontify numbers as constant"
  (font-lock-add-keywords nil
      '(("\\b\\(0x[0-9a-fA-F]+\\)" 1 font-lock-constant-face) ; hexa
        ("\\b\\(-?[0-9]+\\.[0-9]+\\)" 1 font-lock-constant-face) ; float
        ("[\`^(\{\[,\+\-\*/\%=\s-]\\(-?[0-9]+U?L?L?\\)" 1
         font-lock-constant-face)))) ; int
(add-hook 'font-lock-mode-hook 'font-lock-fontify-numbers)

(set-background-color "#242424")
(set-foreground-color "#cccccc")
(set-cursor-color "#ecee90")
(set-face-foreground 'font-lock-comment-face "#c0bc6c")
(set-face-foreground 'font-lock-doc-face "#c0bc6c")
(set-face-foreground 'font-lock-constant-face "#e5786d")
(set-face-foreground 'font-lock-string-face "#95e454")
(set-face-foreground 'font-lock-variable-name-face "#cccccc")
(set-face-foreground 'font-lock-function-name-face "#cae682")
(set-face-foreground 'font-lock-type-face "#cae682")
(set-face-foreground 'font-lock-builtin-face "#87afff")
(set-face-foreground 'font-lock-keyword-face "#87afff")
(set-face-foreground 'font-lock-preprocessor-face "#e5786d")
(set-face-foreground 'font-lock-negation-char-face "#e7f6da")
(set-face-foreground 'link "#8ac6f2")
(set-face-foreground 'show-paren-match "#f6f3e8")
(set-face-background 'show-paren-match "#857b6f")
(set-face-foreground 'region "#f6f3e8")
(set-face-background 'region "#444444")
(set-face-foreground 'lazy-highlight "black")
(set-face-background 'lazy-highlight "yellow")
(global-hl-line-mode 1)
(set-face-background 'hl-line "#32322e")
