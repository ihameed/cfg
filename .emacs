(require 'cl)
(let ((my-lisp-dirs (expand-file-name "~/.emacs.d/elisp")))
  (if (file-directory-p my-lisp-dirs)
      (progn
        (byte-recompile-directory my-lisp-dirs)
        (setq my-lisp-dirs (nconc (list my-lisp-dirs)
                                  (directory-files my-lisp-dirs t nil nil)))
        (do ((dir my-lisp-dirs (cdr dir)))
            ((eq dir nil))
          (let ((dir (car dir)))
            (if (and (file-directory-p dir)
                     (not (string-match "/\\.\\{1,2\\}$" dir)))
                (setq load-path (cons dir load-path))))))))

(require 'undo-tree)
(global-undo-tree-mode)
(setq viper-mode t)
(require 'viper)
(require 'vimpulse)

(setq
 standard-indent 4
 scroll-step 1
 tab-always-indent nil
 initial-scratch-message nil
 inhibit-splash-screen t)

(custom-set-variables '(inhibit-startup-echo-area-message (user-real-login-name)))

(show-paren-mode t)

(set-background-color "#242424")
(set-foreground-color "#cccccc")
(set-cursor-color "#ecee90")
(set-face-foreground 'font-lock-comment-face "#c0bc6c")
(set-face-foreground 'font-lock-doc-face "#c0bc6c")
(set-face-foreground 'font-lock-constant-face "#e5786d")
(set-face-foreground 'font-lock-string-face "#95e454")
(set-face-foreground 'font-lock-variable-name-face "#cae682")
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

(show-paren-mode t)
(tool-bar-mode nil)
(menu-bar-mode nil)
(toggle-scroll-bar nil)
(transient-mark-mode nil)

(setq ring-bell-function (lambda ()))

(setq derp (if (eq system-type 'gnu/linux)
	       '(default ((t (:height 70 :family "ProFontWindows"))))
	     '(default ((t (:height 90 :family "ProFontWindows"))))))

(custom-set-faces
 derp
 '(mode-line ((t (:box (:line-width 2 :color "#242424"))))))
