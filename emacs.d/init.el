(if window-system
  (progn
    (fringe-mode 0)
    (scroll-bar-mode -1)
    (tool-bar-mode -1)))
(menu-bar-mode -1)
(transient-mark-mode nil)
(show-paren-mode t)
(global-hl-line-mode 1)
(ido-mode 1)
(column-number-mode 1)
(customize-set-variable 'indent-tabs-mode nil)

(defun tmp-path (dir) (concat "~/.emacs.d/tmp/" dir))
(defun mktmp (dir) (make-directory (tmp-path dir) t))

(mktmp "backup")

(setq
 mouse-wheel-scroll-amount '(3)
 mouse-wheel-progressive-speed nil
 scroll-step 1
 scroll-conservatively 1000000
 backup-directory-alist `(("." . ,(tmp-path "backup")))
 auto-save-default nil
 font-lock-maximum-size 4096000
 standard-indent 4
 scroll-step 1
 tab-always-indent nil
 initial-scratch-message nil
 inhibit-splash-screen t
 line-move-visual nil
 ring-bell-function (lambda ())
 mac-allow-anti-aliasing nil
 ido-enable-flex-matching t
 ido-everywhere t
 vc-follow-symlinks t)

(defun reload-config () (interactive) (load-file "~/.emacs.d/init.el"))

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


(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/solarized")

(setq el-get-dir "~/.emacs.d/el-get")
(if (not (file-exists-p el-get-dir)) (make-directory el-get-dir))
(add-to-list 'load-path (concat el-get-dir "/el-get"))

(unless (require 'el-get nil t)
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (goto-char (point-max))
     (eval-print-last-sexp))))

(setq my-packages '(auto-complete
                    column-marker
                    ethan-wspace
                    evil
                    fill-column-indicator
                    ghc-mod
                    haskell-mode
                    markdown-mode
                    tuareg-mode))

(el-get 'sync my-packages)
(el-get 'wait)

(evil-mode 1)
(require 'vicompat)

(setq-default
 ;line-spacing 1.5
 fci-rule-column 80)

(global-ethan-wspace-mode 1)
(global-visual-line-mode 1)

(custom-set-variables
 '(inhibit-startup-echo-area-message (user-real-login-name)))

(require 'auto-complete)
(require 'auto-complete-config)
(ac-flyspell-workaround)
;(ac-linum-workaround)
(global-auto-complete-mode t)
(setq
 ac-auto-show-menu t
 ac-auto-start t)

(add-hook 'after-change-major-mode-hook 'fci-mode)

;===============================================================================
(require 'rx)

(defun run-ghcmod (args)
  (shell-command-to-string (concat "ghc-mod " args)))

(defun ghcmod-parse-list (str) (split-string str))

(defun run-ghcmod-browse (module)
  (ghcmod-parse-list (run-ghcmod (concat "browse -o " module))))

(defun run-ghcmod-list ()
  (ghcmod-parse-list (run-ghcmod "list")))

(defun testies ()
  (interactive)
  (message (pp-to-string (run-ghcmod-list))))

;(defun ghcmod-import-prefix () nil)
(defun ghcmod-import-module-candidates () (run-ghcmod-list))

(setq ghcmod-import-module-prefix
      (rx line-start
          "import" (one-or-more space)
          (zero-or-one "qualified" (one-or-more space))
          (group (zero-or-more any))))

(setq ghcmod-import-module-rx
      (rx line-start
       ))

(setq ghcmod-multiline-start-rx
      (rx line-start
          "import" (one-or-more space)
          (zero-or-one "qualified" (one-or-more space))
          (zero-or-more any)
          "("))

(setq ghcmod-multiline-continue-rx
      (rx line-start (one-or-more space) ","))

(defun beginning-of-buffer-p ()
  (interactive)
  (eq (line-number-at-pos) 1))

(defun ghcmod-multiline-import-p ()
  (interactive)
  (save-excursion
    (catch 'ret
      (while (not (beginning-of-buffer-p))
      (beginning-of-line)
      (cond
       ((looking-at ghcmod-multiline-start-rx) (throw 'ret t))
       ((looking-at ghcmod-multiline-continue-rx) (forward-line -1))
       (t (throw 'ret nil)))))))

(defun ghcmod-multiline-import-prefix ()
  (if (ghcmod-multiline-import-p) (ac-prefix-symbol) (nil)))

(defun ghcmod-ghetto-context-sensitive-candidates ()
  (save-excursion
    (beginning-of-line)
    (cond
     ((looking-at ghcmod-import-module-prefix)
      (ghcmod-import-module-candidates))
     (t ("huttsack" "nuttsack"))
     )))

(ac-define-source ghcmod-generic
  `((candidates . ghcmod-ghetto-context-sensitive-candidates)
    (symbol . "s")))

(ac-define-source ghcmod-module
  `((candidates . (ghcmod-import-module-candidates))
    (prefix . ,ghcmod-import-module-prefix)
    (symbol . "m")))

(ac-define-source ghcmod-module-exports
  `((candidates . '("hoopy" "doopy"))
    (prefix . ghcmod-multiline-import-prefix)
    (symbol . "m")))
;===============================================================================

(require 'haskell-style)

(defun on-haskell-mode ()
  (interactive)
  (ghc-init)
  (message "hello")
  (flymake-mode)
  (haskell-style)
  (setq ac-sources '(;ac-source-ghc-mod
                     ;ac-source-ghcmod-module-exports
                     ;ac-source-ghcmod-module
                     ac-source-ghcmod-generic
                     ;ac-source-words-in-buffer
                     )))

(add-hook 'haskell-mode-hook 'on-haskell-mode)

(when window-system
  (setq derp (cond
               ((eq system-type 'darwin)
                '(default ((t (:height 90 :family "ProFontX")))))
               (t
                '(default ((t (:height 90 :family "ProFontWindows")))))))
  (custom-set-faces
   derp
   '(mode-line ((t (:box (:line-width 1 :color "#444444"))))))
  (load-theme 'solarized-light t))

(unless window-system
  (require 'colorscheme-wombat))

;(set-frame-height (selected-frame) 75)
;(set-frame-width (selected-frame) 150)

(if (file-exists-p "~/.emacs.d/local.el") (load "~/.emacs.d/local"))
