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

(defalias 'for-each
  (cons 'macro
        #'(lambda (spec &rest body)
            (let ((i (make-symbol "iter")))
              (list 'let (list (list i (car (cdr spec))))
                    (list 'while i
                          (append (list 'let (list (list (car spec) (list 'car i)))) body)
                          (list 'setq i (list 'cdr i)))))))
  "Temporary alternative to Emacs Lisp's `dolist'")

;; Virtual cursors
(defvar cursors '())
(make-variable-buffer-local 'cursors)
(set-default 'cursor-type nil)

(internal-make-lisp-face 'visual-cursor)
(defalias 'set-visual-cursor-attributes
  #'(lambda ()
      (internal-set-lisp-face-attribute 'visual-cursor :background "black" 0)
      (internal-set-lisp-face-attribute 'visual-cursor :foreground "white" 0))
  "Temporary. Faces have to be set for every frame otherwise it's an invalid face reference")
(set-visual-cursor-attributes)

(defalias 'new-cursor
  #'(lambda (start &optional end mark)
      (let* ((overlay (make-overlay start (or end (+ start 1)) (current-buffer) t))
             (cursor (cons overlay mark)))
        (overlay-put overlay 'face '(visual-cursor))
        (setq cursors (cons cursor cursors))
        cursor)))

(defalias 'cursor-start
  #'(lambda (cursor)
      (overlay-start (car cursor))))

(defalias 'cursor-end
  #'(lambda (cursor)
      (overlay-end (car cursor))))

(defalias 'cursor-activated-p
  #'(lambda (cursor)
      (cdr cursor)))

;; Make window

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
(set-visual-cursor-attributes)

;; Stupidly basic keybindings

(defalias 'define-key-range
  #'(lambda (keymap init end def)
      (if (< init end)
          (progn
            (define-key keymap (string init) def)
            (define-key-range keymap (+ init 1) end def)))))

(defalias 'visual-self-insert
  #'(lambda ()
      (interactive)
      (save-excursion
        (let ((iter cursors))
          (while iter
            (goto-char (cursor-start (car iter)))
            (insert last-command-event)
            (setq iter (cdr iter)))))))

(defalias 'visual-backward-char
  #'(lambda ()
      (interactive)
      (for-each (cursor cursors)
                (move-overlay (car cursor) (1- (cursor-start cursor)) (1- (cursor-end cursor))))))

(defalias 'visual-forward-char
  #'(lambda ()
      (interactive)
      (for-each (cursor cursors)
                (move-overlay (car cursor) (1+ (cursor-start cursor)) (1+ (cursor-end cursor))))))

(defalias 'visual-delete-char
  #'(lambda ()
      (interactive)
      (for-each (cursor cursors)
                (save-excursion
                  (goto-char (cursor-start cursor))
                  (delete-char -1)))))

(let ((map (make-keymap)))
  (define-key-range map ?0 ?9 #'visual-self-insert)
  (define-key-range map ?A ?Z #'visual-self-insert)
  (define-key-range map ?a ?z #'visual-self-insert)
  (define-key map " " #'visual-self-insert)
  (define-key map [left] #'visual-backward-char)
  (define-key map [right] #'visual-forward-char)
  (define-key map [backspace] #'visual-delete-char)
  (use-global-map map))

;; Define

(provide 'loadup)
