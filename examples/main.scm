(import (scheme base)
        (scheme write)
        (conduit))

(define (show label value)
  (display label)
  (display ": ")
  (write value)
  (newline))

(display "implementation: ")
(write conduit-implementation)
(newline)

(define printf-command
  (command-args
    (make-command "printf")
    '("%s\n" "hello from a Command value")))

(show "command argv" (command->list printf-command))
(show "stdout" (process-output->string printf-command))
(show "shell status" (shell-command '("sh" "-c" "exit 3")))

(let ((process
        (spawn-process*
          (command-args
            (make-command "sh")
            '("-c" "read line; printf \"reply:%s\" \"$line\""))
          #t
          #t
          #f)))
  (process-send-string process "ping\n")
  (process-close-input process)
  (show "roundtrip" (read-port->string (process-output-port process)))
  (show "roundtrip status" (process-wait process)))

(if process-separate-ports?
    (begin
      (show "manual pipe"
            (let ((source (spawn-process* '("printf" "conduit") #f #t #f))
                  (sink (spawn-process* '("tr" "a-z" "A-Z") #t #t #f)))
              (pipe-processes source sink)
              (process-wait source)
              (let ((result (read-port->string (process-output-port sink))))
                (process-wait sink)
                result)))
      (show "pipeline"
            (process-pipeline-output->string
              (list
                (command-args (make-command "printf") '("conduit"))
                (command-args (make-command "tr") '("a-z" "A-Z"))
                (command-args (make-command "sed") '("s/CON/PRO/"))))))
    (begin
      (display "manual pipe: skipped, backend lacks separate process ports")
      (newline)
      (display "pipeline: skipped, backend lacks separate process ports")
      (newline)))
