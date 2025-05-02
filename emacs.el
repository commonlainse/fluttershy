;;; emacs.el --- Things absolutely necessary for Emacs to run  -*- lexical-binding: t; -*-

;;; Commentary:

;;  The C side of Emacs calls a lot of functions that are on the Lisp side
;;  If those functions aren't defined, Emacs will send an error or worse:
;; not start or segfault.

;;; Code:

;;; Miscelaneous functions called from the C code

(defalias 'internal-timer-start-idle #'(lambda nil nil))

(defalias 'internal-echo-keystrokes-prefix #'(lambda nil nil))

(defalias 'command-execute
  #'(lambda (cmd &optional record-flag keys special)
      (message "%S" cmd)
      ;; (message (format "%S" cmd))
      ;; ^ That sometimes causes a segfault???
      (call-interactively cmd record-flag keys))
  "Called from 'command_loop_1' in C and 'read_char' in C, both from keyboard.c")

(defalias 'undefined
  #'(lambda ()
      (interactive)
      (ding))
  "Gets called by 'command_loop_1' from keyboard.c")

(defalias 'ignore
  #'(lambda (&rest dontcare)
      (interactive)
      nil)
  "Used in various places in the C source code")

(defalias 'undo-auto--add-boundary
  #'(lambda ())
  "Gets called by 'command_loop_1' from keyboard.c")

(defalias 'undo-auto-amalgamate
  #'(lambda ())
  "Gets called by 'delete-char' and 'self-insert-command' from cmds.c")

(defalias 'undo-auto--undoable-change
  #'(lambda ())
  "Called by run_undoable_change in insdel.c which is called by many editing commands")

(defalias 'handle-shift-selection
  #'(lambda ())
  "Called by 'call-interactively' from callint.c")

(defalias 'make-lock-file-name
  #'(lambda (filename))
  "Called by `make_lock_file_name' `lock_file' `unlock_file' and `file-locked-p' in filelock.c")

(defalias 'function-documentation
  #'(lambda (function))
  "Called by `documentation' in doc.c")

(defalias 'substitute-command-keys
  #'(lambda (string &optional no-face include-menus)
      string)
  "Called by 'documentation-property' and 'documentation' in doc.c, 'help_echo_substitute_command_keys' and 'parse_menu_item' in keyboard.c, 'print_error_message' in print.c, and 'internal-describe-syntax-value' in syntax.c")

(defalias 'record-window-buffer
  #'(lambda (window))
  "Called by 'set-window-buffer' and 'set-window-configuration' in window.c")

(defalias 'kill-buffer--possibly-save
  #'(lambda (buffer) t)
  "Called by 'kill-buffer' in buffer.c")

;;; Functions that handle events

(defalias 'handle-move-frame
  #'(lambda (event)
      (interactive "e"))
  "Defined to be called in 'keys_of_keyboard' from C file keyboard.c")

(defalias 'handle-focus-in
  #'(lambda (event)
      (interactive "e"))
  "Defined to be called in 'keys_of_keyboard' from C file keyboard.c")

(defalias 'handle-focus-out
  #'(lambda (event)
      (interactive "e"))
  "Defined to be called in 'keys_of_keyboard' from C file keyboard.c")

(defalias 'handle-delete-frame
  #'(lambda (event)
      (interactive "e")
      (if (= (length (frame-list)) 1) ; delete-frame errors when trying to delete the sole frame of the emacs process.
          (kill-emacs 0)
        (delete-frame (car (nth 1 event)) t)))
  "Defined to be called in 'keys_of_keyboard' from C file keyboard.c")

(defalias 'run-hook-query-error-with-timeout #'run-hooks
  "Called by `kill-emacs' in 'emacs.c' when called interactively")

;;; Needed to create frames
(defalias 'vendor-specific-keysyms #'(lambda (vendor) nil))

(defalias 'window--pixel-to-total
  #'(lambda (frame horizontal)
      ; lazy
      ; (frame-char-height frame)
      t)
  "Called by 'window_pixel_to_total' on the C source code")

;;; Fontsets, chars and scripts

;; ENSURE LATIN ENCODING WORKS
;; IT'S THE DEFAULT ONE CHOSEN BY EMACS WHEN YOU CREATE A FRAME SO IT'S OBLIGATORY
;; stolen from international/characters.el
(define-category ?a "ASCII")
(define-category ?l "Latin")
(modify-category-entry '(32 . 127) ?a)
(modify-category-entry '(32 . 127) ?l)
(modify-category-entry '(#x80 . #x024F) ?l)

;;  it's important to define the raw-text coding system or emacs
;; segfaults when using `write-region'
;; stolen from international/mule-cmds.el
(define-coding-system-internal
  'raw-text
  ?t
  'raw-text
  nil
  nil
  nil
  nil  
  nil
  nil
  nil
  t
  '(:name "Raw text" :description "Text only with EOL")
  nil)

(define-coding-system-internal
  'ascii ; name
  ?a ; mnemonic
  'utf-8 ; coding-type
  '(ascii) ; charset-list
  t ; ascii-compatible-p
  nil ; decode-translation-table
  nil ; encode-translation-table
  nil ; post-read-conversion
  nil ; pre-write-conversion
  nil ; default-char
  nil ; for-unibyte
  '(:name "ASCII" :docstring "Felt like it") ; plist
  nil ; eol-type
  nil ; bom. only specific to utf-8
  )

(define-coding-system-internal
  'latin-1 ; name
  ?l ; mnemonic
  'utf-8 ; coding-type
  '(ascii) ; charset-list
  t ; ascii-compatible-p
  nil ; decode-translation-table
  nil ; encode-translation-table
  nil ; post-read-conversion
  nil ; pre-write-conversion
  nil ; default-char
  nil ; for-unibyte
  '(:name "Latin" :docstring "Felt like it") ; plist
  nil ; eol-type
  nil ; bom. only specific to utf-8
  )

;; stolen from international/charscript.el
(set-char-table-range char-script-table '(#x0000 . #x007F) 'latin)
(set-char-table-range char-script-table '(#x00A0 . #x024F) 'latin)
(set-char-table-range char-script-table '(#x1AB0 . #x1AFF) 'latin)
(set-char-table-range char-script-table '(#x1DC0 . #x1EFF) 'latin)
(set-char-table-range char-script-table '(#x2C60 . #x2C7F) 'latin)
(set-char-table-range char-script-table '(#xA700 . #xA7FF) 'latin)
(set-char-table-range char-script-table '(#xAB30 . #xAB6F) 'latin)
(set-char-table-range char-script-table '(#xFE20 . #xFE2F) 'latin)
(set-char-table-range char-script-table '(#x2C60 . #x2C7F) 'latin)
(set-char-table-range char-script-table '(#x10780 . #x107BF) 'latin)
(set-char-table-range char-script-table '(#x1DF00 . #x1DFFF) 'latin)
(set-char-table-extra-slot char-script-table 0 '(latin))

(setq script-representative-chars
      '((latin ?A ?Z ?a ?z #x00C0 #x0100 #x0180 #x1e00)))

;; stolen from international/fontset.el
(new-fontset
 "fontset-default"
 (list
  ;; for each script
  (list 'latin
        '(nil . "ISO8859-1")
	'(nil . "ISO8859-2")
	'(nil . "ISO8859-3")
	'(nil . "ISO8859-4")
	'(nil . "ISO8859-9")
	'(nil . "ISO8859-10")
	'(nil . "ISO8859-13")
	'(nil . "ISO8859-14")
	'(nil . "ISO8859-15")
	'(nil . "ISO8859-16")
	'(nil . "VISCII1.1-1")
	(font-spec :registry "iso10646-1" :script 'latin))

  ;; for each charset
  (list 'ascii '(nil . "ISO8859-1"))
  
  ;; Fallback fonts
  (list nil
        '(nil . "gb2312.1980")
	'(nil . "gbk-0")
	'(nil . "gb18030")
	'(nil . "jisx0208")
	'(nil . "ksc5601.1987")
	'(nil . "CNS11643.1992-1")
	'(nil . "CNS11643.1992-2")
	'(nil . "CNS11643.1992-3")
	'(nil . "CNS11643.1992-4")
	'(nil . "CNS11643.1992-5")
	'(nil . "CNS11643.1992-6")
	'(nil . "CNS11643.1992-7")
	'(nil . "big5")
	'(nil . "jisx0213.2000-1")
	'(nil . "jisx0213.2004-1")
	'(nil . "jisx0212"))))

;; SETUP UTF-8
;; stolen from international/mule.conf.el
(define-coding-system-internal
  'utf-8 ; name
  ?U ; mnemonic
  'utf-8 ; coding type
  '(unicode) ; charset list
  nil ; ascii compatible
  nil ; decode translation table
  nil ; encode translation table
  nil ; post read conversion
  nil ; pre-write-conversion
  nil ; default char
  nil ; for unibyte
  '(:mime-charset utf-8 :name utf-8 :docstring "UTF-8 (no signature (BOM))") ; plist
  nil ; eol-type
  nil ; bom
  )

(provide 'emacs)
