(provide 'vicompat)

(defvar linum-dynamic-format "")
(defun max-linum-width ()
  (length (number-to-string (count-lines (point-min) (point-max)))))
(defun adjust-dynamic-format ()
  (setq linum-dynamic-format
        (concat "%" (number-to-string (max-linum-width)) "d ")))
(defun linum-format-line (line-number)
  (propertize (format linum-dynamic-format line-number) 'face 'linum))
(add-hook 'linum-before-numbering-hook 'adjust-dynamic-format)

(global-linum-mode 1)
(setq linum-format 'linum-format-line)



(require 'delsel) ;defines minibuffer-keyboard-quit

(global-set-key [escape] 'keyboard-quit)
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

(global-set-key (kbd "<f3>") 'ido-switch-buffer)


(setq ex-commands '(
  ("reload" . reload-config)
  ("ghci" . run-haskell)
  ("shell" . shell)
))

(defun bind-ex-command (commandspec)
  (destructuring-bind (cmd . f) commandspec
    (evil-ex-define-cmd cmd f)))

(mapc 'bind-ex-command ex-commands)
