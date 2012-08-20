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

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil t)
  (url-retrieve
    "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
    (lambda (s) (end-of-buffer) (eval-print-last-sexp))))

(setq my-packages '(
		    auto-complete
		    evil
		    haskell-mode
		    markdown-mode
		    tuareg-mode
		    ))

(el-get 'sync my-packages)
(el-get 'wait)

(evil-mode 1)

;(require 'colorscheme-wombat)

(load-theme 'solarized-light t)

(setq font-lock-maximum-size 4096000)

(setq
 standard-indent 4
 scroll-step 1
 tab-always-indent nil
 initial-scratch-message nil
 inhibit-splash-screen t)

(setq line-move-visual nil)
;(setq-default line-spacing 1.5)
(global-visual-line-mode 1)
(column-number-mode 1)
(fringe-mode 0)

(custom-set-variables '(inhibit-startup-echo-area-message (user-real-login-name)))

(global-hl-line-mode 1)
(global-linum-mode 1)

(defvar linum-dynamic-format "")

(add-hook 'linum-before-numbering-hook
	  (lambda ()
	    (let ((width
		   (length (number-to-string
			    (count-lines (point-min) (point-max))))))
	      (setq linum-dynamic-format
		    (concat "%" (number-to-string width) "d ")))))
	
(setq linum-format
      (lambda (line-number)
	(propertize (format linum-dynamic-format line-number) 'face 'linum)))
				     

(show-paren-mode t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(if window-system (progn (scroll-bar-mode -1)))
(transient-mark-mode nil)
(setq mac-allow-anti-aliasing nil)

(setq ring-bell-function (lambda ()))

(setq derp (cond
             ((eq system-type 'darwin) '(default ((t (:height 90 :family "ProFontX")))))
             (t '(default ((t (:height 90 :family "ProFontWindows")))))))

(custom-set-faces
 derp
 '(mode-line ((t (:box (:line-width 1 :color "#444444"))))))

(if (file-exists-p "~/.emacs.d/local.el") (load "~/.emacs.d/local"))
