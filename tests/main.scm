(import (scheme base)
        (scheme write)
        (conduit))

(define (check name value expected)
  (unless (equal? value expected)
    (display "failed: ")
    (write name)
    (display " expected ")
    (write expected)
    (display " got ")
    (write value)
    (newline)
    (error name "check failed")))

(define (read-all port)
  (let loop ((chars '()))
    (let ((char (read-char port)))
      (if (eof-object? char)
          (list->string (reverse chars))
          (loop (cons char chars))))))

(check 'implementation-symbol (symbol? conduit-implementation) #t)
(cond-expand
  ((or capy gauche guile chibi sagittarius mosh stklos kawa ironscheme)
   (check 'process-separate-ports? process-separate-ports? #t))
  (else
   (check 'process-separate-ports? process-separate-ports? #f)))

(let ((command (command-args (command-arg (make-command "printf") "%s") '("command-api"))))
  (check 'command? (command? command) #t)
  (check 'command-program (command-program command) "printf")
  (check 'command-arguments (command-arguments command) '("%s" "command-api"))
  (check 'command->list (command->list command) '("printf" "%s" "command-api")))

(cond-expand
  ((or capy gauche guile chibi sagittarius mosh stklos kawa ironscheme)
   (let ((command (command-args (command-arg (make-command "printf") "%s") '("command-api"))))
     (check 'command-output (process-output->string command) "command-api"))
   (check 'process-output (process-output->string '("sh" "-c" "printf conduit")) "conduit")
   (check 'shell-command-ok (shell-command '("sh" "-c" "exit 0")) 0)
   (check 'shell-command-fail (shell-command '("sh" "-c" "exit 7")) 7))
  (mit
   (check 'shell-command-ok (shell-command "exit 0") 0)
   (check 'shell-command-fail (shell-command "exit 7") 7))
  (else #t))

(cond-expand
  (gauche
   (check 'shell-command-stdio
          (process-output+error+status
            '("gosh" "-I" "src" "-r7" "-e"
              "(import (scheme base) (conduit)) (shell-command '(\"sh\" \"-c\" \"printf out; printf err >&2\"))"))
          '("out" "err" 0)))
  (else #t))

(cond-expand
  ((or capy gauche chibi sagittarius mosh stklos kawa ironscheme)
   (check 'process-error
          (process-error->string '("sh" "-c" "printf problem >&2"))
          "problem")
   (check 'process-output+error+status
          (process-output+error+status
            (command-args
              (make-command "sh")
              '("-c" "printf out; printf err >&2; exit 5")))
          '("out" "err" 5)))
  (else #t))

(cond-expand
  ((or capy gauche guile chibi sagittarius mosh stklos kawa ironscheme)
   (let ((process (spawn-process* '("sh" "-c" "printf port") #f #t #f)))
     (check 'process? (process? process) #t)
     (check 'process-port-output (read-char (process-output-port process)) #\p)
     (process-close-output process)
     (check 'process-wait (process-wait process) 0)
     (check 'process-exit-status (process-exit-status process) 0)))
  (else #t))

(cond-expand
  ((or capy gauche chibi sagittarius mosh stklos kawa ironscheme)
   (let ((process (spawn-process* '("sh" "-c" "printf err >&2") #f #f #t)))
     (check 'process-error-port-output (read-all (process-error-port process)) "err")
     (process-close-error process)
     (check 'process-error-port-wait (process-wait process) 0)))
  (else #t))

(cond-expand
  ((or capy gauche guile chibi sagittarius mosh stklos kawa ironscheme)
   (let ((process (spawn-process* '("sh" "-c" "read line; printf \"$line\"") #t #t #f)))
     (process-send-string process "roundtrip\n")
     (process-close-input process)
     (check 'process-roundtrip (read-all (process-output-port process)) "roundtrip")
     (check 'process-roundtrip-wait (process-wait process) 0)))
  (else #t))

(cond-expand
  ((or capy gauche guile chibi sagittarius mosh stklos kawa ironscheme)
   (let ((source (spawn-process* '("sh" "-c" "printf conduit") #f #t #f))
         (sink (spawn-process* '("tr" "a-z" "A-Z") #t #t #f)))
     (pipe-processes source sink)
     (check 'pipe-processes-source-wait (process-wait source) 0)
     (check 'pipe-processes-output (read-all (process-output-port sink)) "CONDUIT")
     (check 'pipe-processes-sink-wait (process-wait sink) 0))
   (check 'process-pipeline-output
          (process-pipeline-output->string
            '(("sh" "-c" "printf conduit")
              ("tr" "a-z" "A-Z")
              ("sed" "s/CON/PRO/")))
          "PRODUIT"))
  (else #t))

(display "conduit tests ok on ")
(write conduit-implementation)
(newline)
