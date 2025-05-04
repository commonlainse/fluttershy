;;; lang.el --- Language constructs for Elisp        -*- lexical-binding: t; -*-

;; Macros and functions that turn Elisp into an actual language
;;  Including a type system, looping constructs, structs and core concepts that
;; every package should adhere to.  Such as buffer or function categories.

;; Types with (: A B C) (stored in the symbol plist)
;; Not focus on lists too much
;; Default values with &optional (+ destructuring)
;; Powerful looping
;; Structs
;; Functions can belong in categories with (category) and (package)
;; Threading with (->)

(defalias 'lambda
  (cons 'macro
        #'(lambda (&rest body)
            (list 'function (cons 'lambda body)))))

;; todo: Doesn't support cycles
(defalias 'backquote
  (lambda (value)
    (cond
     ((atom value) ; `sym
      (list 'quote value))
     ((or (eq (car value) '\,)) ; (, ...)
      (car (cdr value)))
     ((eq (car value) '\`) ; (` ...)
      value)
     ((eq (car value) '\,@) ; (,@ ...)
      (car (cdr value)))
     ((and (consp (car value))
           (eq (car (car value)) '\,@)) ; ((,@ ...) ...)
      (list 'append (car (cdr (car value)))
            (backquote (cdr value))))
     (t ; (...)
      (let ((res (list nil))
            tail)
        (setq tail res)
        (while (and (consp value)
                    ;; v  No need to process lists in the form (,@ ...)
                    ;; v  As they're handled above by this same `cond'
                    (null (and (consp (car value))
                               (eq (car (car value)) '\,@)))
                    ;; v  To properly handle `(a . ,b)
                    ;; v  Which Elisp transforms to '(a \, b)
                    (null (eq (car value) '\,)))
          (setcar tail (list 'cons (backquote (car value)) nil))
          (setq tail (cdr (cdr (car tail))))
          (setq value (cdr value)))
        (setcar tail (backquote value))
        (car res))))))

(defalias '\` (cons 'macro #'backquote)
  "Same as quoting except preceding an element with a comma evaluates it

`(a b c) -> '(a b c)
`(a b ,(+ 2 2)) -> '(a b 4)
`(a b ,@c) -> '(a b ...c...)
`(a b ,@'(c d e)) -> (a b c d e)")

(defalias 'when
  (cons 'macro
        (lambda (cond &rest body)
          `(if ,cond (progn ,@body))))
  "Evaluates BODY if COND isn't nil")

(defalias 'defmacro
  (cons 'macro
        (lambda (name arglist &rest body)
          (let (docstring)
            (when (and (cdr body) (stringp (car body)))
              (setq docstring (car body))
              (setq body (cdr body)))
            `(defalias ',name
               (cons 'macro #'(lambda ,arglist ,@body))
               ,docstring))))
  "Define a macro called NAME")

(defmacro defun (name arglist &rest body)
  "Define a function called NAME"
  (let (docstring)
    (when (and (cdr body) (stringp (car body)))
      (setq docstring (car body))
      (setq body (cdr body)))
    `(defalias ',name
       #'(lambda ,arglist ,@body)
       ,docstring)))

(defmacro -> (init &rest body)
  "Move every statement to be the first argument of the next statement

(-> a 1+) => (-> (1+ a))
(-> a (+ b)) => (-> (+ a b))"
  (while body
    (let ((el (car body)))
      (when (symbolp el)
        (setq el (list el)))
      (setq init `(,(car el) ,init . ,(cdr el))))
    (setq body (cdr body)))
  init)
(provide 'lang)
;;; lang.el ends here
