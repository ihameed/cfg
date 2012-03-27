;;; .loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (camldebug) "camldebug" "tuareg-mode/camldebug.el"
;;;;;;  (20337 37412))
;;; Generated autoloads from tuareg-mode/camldebug.el

(defvar camldebug-command-name "ocamldebug" "\
Pathname for executing Caml debugger.")

(autoload 'camldebug "camldebug" "\
Run camldebug on program FILE in buffer *camldebug-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for camldebug.  If you wish to change this, use
the camldebug commands `cd DIR' and `directory'.

\(fn PATH)" t nil)

;;;***

;;;### (autoloads nil "evil-core" "evil/evil-core.el" (20337 37409))
;;; Generated autoloads from evil/evil-core.el
 (autoload 'evil-mode "evil")

;;;***

;;;### (autoloads (tuareg-mode) "tuareg" "tuareg-mode/tuareg.el"
;;;;;;  (20337 37412))
;;; Generated autoloads from tuareg-mode/tuareg.el
 (add-to-list 'auto-mode-alist '("\\.ml[ily]?\\'" . tuareg-mode))

(autoload 'tuareg-mode "tuareg" "\
Major mode for editing Caml code.

Dedicated to Emacs and XEmacs, version 21 and higher. Provides
automatic indentation and compilation interface. Performs font/color
highlighting using Font-Lock. It is designed for Objective Caml but
handles Objective Labl and Caml Light as well.

Report bugs, remarks and questions to Albert.Cohen@prism.uvsq.fr.

The Font-Lock minor-mode is used according to your customization
options. Within XEmacs (non-MULE versions only) you may also want to
use Sym-Lock:

\(if (and (boundp 'window-system) window-system)
    (when (string-match \"XEmacs\" emacs-version)
       	(if (not (and (boundp 'mule-x-win-initted) mule-x-win-initted))
            (require 'sym-lock))
       	(require 'font-lock)))

You have better byte-compile tuareg.el (and sym-lock.el if you use it)
because symbol highlighting is very time consuming.

For customization purposes, you should use `tuareg-mode-hook'
\(run for every file) or `tuareg-load-hook' (run once) and not patch
the mode itself. You should add to your configuration file something like:
  (add-hook 'tuareg-mode-hook
            (lambda ()
               ... ; your customization code
            ))
For example you can change the indentation of some keywords, the
`electric' flags, Font-Lock colors... Every customizable variable is
documented, use `C-h-v' or look at the mode's source code.

A special case is Sym-Lock customization: You may set
`tuareg-sym-lock-keywords' in your `.emacs' configuration file
to override default Sym-Lock patterns.

`custom-tuareg.el' is a sample customization file for standard changes.
You can append it to your `.emacs' or use it as a tutorial.

`M-x camldebug' FILE starts the Caml debugger camldebug on the executable
FILE, with input and output in an Emacs buffer named *camldebug-FILE*.

A Tuareg Interactive Mode to evaluate expressions in a toplevel is included.
Type `M-x tuareg-run-caml' or see special-keys below.

Some elementary rules have to be followed in order to get the best of
indentation facilities.
  - Because the `function' keyword has a special indentation (to handle
    case matches) use the `fun' keyword when no case match is performed.
  - In OCaml, `;;' is no longer necessary for correct indentation,
    except before top level phrases not introduced by `type', `val', `let'
    etc. (i.e., phrases used for their side-effects or to be executed
    in a top level.)
  - Long sequences of `and's may slow down indentation slightly, since
    some computations (few) require to go back to the beginning of the
    sequence. Some very long nested blocks may also lead to slow
    processing of `end's, `else's, `done's...
  - Multiline strings are handled properly, but the string concatenation `^'
    is preferred to break long strings (the C-j keystroke can help).

Known bugs:
  - When writting a line with mixed code and comments, avoid putting
    comments at the beginning or middle of the text. More precisely, 
    writing comments immediately after `=' or parentheses then writing
    some more code on the line leads to indentation errors. You may write
    `let x (* blah *) = blah' but should avoid `let x = (* blah *) blah'.

Special keys for Tuareg mode:\\{tuareg-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads (global-undo-tree-mode undo-tree-mode) "undo-tree"
;;;;;;  "undo-tree/undo-tree.el" (20337 37344))
;;; Generated autoloads from undo-tree/undo-tree.el

(autoload 'undo-tree-mode "undo-tree" "\
Toggle undo-tree mode.
With no argument, this command toggles the mode.
A positive prefix argument turns the mode on.
A negative prefix argument turns it off.

Undo-tree-mode replaces Emacs' standard undo feature with a more
powerful yet easier to use version, that treats the undo history
as what it is: a tree.

The following keys are available in `undo-tree-mode':

  \\{undo-tree-map}

Within the undo-tree visualizer, the following keys are available:

  \\{undo-tree-visualizer-map}

\(fn &optional ARG)" t nil)

(defvar global-undo-tree-mode nil "\
Non-nil if Global-Undo-Tree mode is enabled.
See the command `global-undo-tree-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-undo-tree-mode'.")

(custom-autoload 'global-undo-tree-mode "undo-tree" nil)

(autoload 'global-undo-tree-mode "undo-tree" "\
Toggle Undo-Tree mode in every possible buffer.
With prefix ARG, turn Global-Undo-Tree mode on if and only if ARG is positive.
Undo-Tree mode is enabled in all buffers where `turn-on-undo-tree-mode' would do it.
See `undo-tree-mode' for more information on Undo-Tree mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("el-get/el-get-install.el" "el-get/el-get.el"
;;;;;;  "evil/evil-commands.el" "evil/evil-common.el" "evil/evil-digraphs.el"
;;;;;;  "evil/evil-ex.el" "evil/evil-integration.el" "evil/evil-macros.el"
;;;;;;  "evil/evil-maps.el" "evil/evil-pkg.el" "evil/evil-repeat.el"
;;;;;;  "evil/evil-search.el" "evil/evil-states.el" "evil/evil-tests.el"
;;;;;;  "evil/evil-types.el" "evil/evil-vars.el" "evil/evil.el" "tuareg-mode/append-tuareg.el"
;;;;;;  "tuareg-mode/custom-tuareg.el" "tuareg-mode/sym-lock.el")
;;;;;;  (20337 37413 148800))

;;;***

(provide '.loaddefs)
;; Local Variables:
;; version-control: never
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; .loaddefs.el ends here
