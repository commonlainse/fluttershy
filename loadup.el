;; loadup.el  -*- lexical-binding: t; -*-

;; This code so far only deals with X11

;; Errors you'll find step by step
;; 0. You first should start by calling (x-create-frame) and you'll soon find out you'll have to call (x-open-connection) too
;; 1. no 'internal-timer-start-idle
;; 2. no 'internal-echo-keystrokes-prefix
;; 3. no latin script/charset
;; 4. no 'window--pixel-to-total
;; 5. unknown utf-8 coding system
;; 6. no command-execute
;; 7. emacs will send 'handle-move-frame' event
;; 8. Why is... everything black?
;; 8a. You must delete terminal-frame (delete-frame terminal-frame)
;; 8b. When Emacs starts it creates a frame, the terminal-frame. Everything is sent and received by this frame, so this new frame we created (x-create-frame) is actually dummy, it doesn't do anything. If the terminal-frame is killed, then that frame will behave appropiately. Is this brainfuck? Yes, remember Emacs' GUI is just a terminal emulator in disguise after all

;; What to do next?
;; - Define a global keymap. Use `use-global-map'
;; - Handle arguments
;; - There seems to be a segfault when certain things run interactively

;; Things to notice
;; - When running from temacs, (documentation) on any function will return nil.

;; Obligatory functions which the C code depends on

(load "emacs.el")

(set-buffer "*scratch*")
(setq buffer-undo-list t)

(internal-set-lisp-face-attribute 'default :background "white" t)
(internal-set-lisp-face-attribute 'default :foreground "black" t)

(put 'keyboard-translate-table 'char-table-extra-slots 0)
(setq x-resource-name "emacs")

(x-open-connection ":0" nil (eq initial-window-system 'x))
(x-create-frame '((name . "Fluttershy")
                  (minibuffer . t)
                  (parent-frame)
                  (undecorated)
                  (override-redirect)
                  (icon-name . "emacs")))

(delete-frame terminal-frame)
(setq terminal-frame nil)

;; Stupidly basic keybindings

(defalias 'define-key-range
  #'(lambda (keymap init end def)
      (if (< init end)
          (progn
            (define-key keymap (string init) def)
            (define-key-range keymap (+ init 1) end def)))))

(let ((map (make-keymap)))
  (define-key-range map ?0 ?9 #'self-insert-command)
  (define-key-range map ?A ?Z #'self-insert-command)
  (define-key-range map ?a ?z #'self-insert-command)
  (define-key map " " #'self-insert-command)
  (define-key map [left] #'backward-char)
  (define-key map [right] #'forward-char)
  (define-key map [backspace] #'delete-char)
  (use-global-map map))

;; Define

(defalias 'defun
  (cons 'macro #'(lambda (name args &rest body))))

(get-buffer-create "example")
(set-window-buffer (selected-window) "*Messages*")
;; (set-buffer "example")
(setq mode-line-format '("%b"))
(insert (format "%S" mode-line-format))

(provide 'loadup)
