;;; -*- lexical-binding: t; -*-

;; Hosts a server at localhost port 8008, evaluating anything it receives
;; Temporary until the elisp editor is fully fleshed out

(defconst repl-port "8008")

(defalias 'repl-filter
  #'(lambda (process s)
      (let ((res (eval (read s) t)))
        (process-send-string process (format "%S" res)))))

(defalias 'repl-stop
  #'(lambda ()
      (delete-process "Crappy REPL")))

(defalias 'repl-start
  #'(lambda ()
      (make-network-process
       :name "Crappy REPL"
       :buffer nil
       :family 'ipv4 ; no need to deal with sockets shenanigans
       :service repl-port
       :host "0.0.0.0"
       :server t
       :filter #'repl-filter)
      (set-default 'kill-emacs-hook (cons #'repl-stop kill-emacs-hook))))

(provide 'repl)
;;; repl.el ends here
