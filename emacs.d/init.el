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


(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/solarized")

(custom-set-variables
 '(inhibit-startup-echo-area-message (user-real-login-name)))
