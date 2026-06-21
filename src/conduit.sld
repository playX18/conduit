(define-library (conduit)
  (export
    conduit-implementation
    conduit-unsupported
    process-separate-ports?
    make-command
    command?
    command-program
    command-arguments
    command-arg
    command-args
    command->list
    make-process
    process?
    process-pid
    process-input-port
    process-output-port
    process-error-port
    process-exit-status
    spawn-process
    spawn-process*
    shell-command
    process-wait
    process-output->string
    process-error->string
    process-output+error+status
    read-port->string
    copy-port
    process-send-string
    process-close-input
    process-close-output
    process-close-error
    pipe-processes
    process-pipeline-output->string)
  (import (scheme base))
  (cond-expand
    (capy
      (import (conduit implementation capy)))
    (gauche
      (import (conduit implementation gauche)))
    (guile
      (import (conduit implementation guile)))
    (chibi
      (import (conduit implementation chibi)))
    (mit
      (import (conduit implementation mit)))
    (chez
      (import (conduit implementation chez)))
    (sagittarius
      (import (conduit implementation sagittarius)))
    (mosh
      (import (conduit implementation mosh)))
    (stklos
      (import (conduit implementation stklos)))
    (kawa
      (import (conduit implementation kawa)))
    (loko
      (import (conduit implementation loko)))
    (ironscheme
      (import (conduit implementation ironscheme)))
    (skint
      (import (conduit implementation skint)))
    (cyclone
      (import (conduit implementation cyclone)))
    (else
      (import (conduit implementation unsupported))))
  (begin
    (define-record-type <process>
      (make-process impl pid input output error status)
      process?
      (impl process-impl)
      (pid process-pid)
      (input process-input-port)
      (output process-output-port)
      (error process-error-port)
      (status process-exit-status process-exit-status-set!))

    (define-record-type <command>
      (make-command-record program arguments)
      command?
      (program command-program)
      (arguments command-arguments))

    (define conduit-implementation %conduit-implementation)
    (define process-separate-ports? %separate-process-ports?)

    (define (conduit-unsupported who)
      (error who "operation is not supported by this Scheme implementation"))

    (define (string-list? obj)
      (let loop ((obj obj))
        (cond
          ((null? obj) #t)
          ((and (pair? obj) (string? (car obj))) (loop (cdr obj)))
          (else #f))))

    (define (make-command program)
      (unless (string? program)
        (error 'make-command "expected program string" program))
      (make-command-record program '()))

    (define (command-arg command argument)
      (unless (command? command)
        (error 'command-arg "expected command" command))
      (unless (string? argument)
        (error 'command-arg "expected argument string" argument))
      (make-command-record
        (command-program command)
        (append (command-arguments command) (list argument))))

    (define (command-args command arguments)
      (unless (command? command)
        (error 'command-args "expected command" command))
      (unless (string-list? arguments)
        (error 'command-args "expected list of argument strings" arguments))
      (make-command-record
        (command-program command)
        (append (command-arguments command) arguments)))

    (define (command->list command)
      (unless (command? command)
        (error 'command->list "expected command" command))
      (cons (command-program command) (command-arguments command)))

    (define (normalize-command command)
      (cond
        ((string? command) command)
        ((command? command) (command->list command))
        ((and (pair? command) (string-list? command)) command)
        (else
          (error 'normalize-command
                 "expected shell string, command, or non-empty string list"
                 command))))

    (define (spawn-process command)
      (spawn-process* command #t #t #t))

    (define (spawn-process* command input? output? error?)
      (let ((impl (%spawn-process (normalize-command command) input? output? error?)))
        (make-process
          impl
          (%process-pid impl)
          (%process-input-port impl)
          (%process-output-port impl)
          (%process-error-port impl)
          #f)))

    (define (shell-command command)
      (%shell-command (normalize-command command)))

    (define (process-wait process)
      (unless (process? process)
        (error 'process-wait "expected process" process))
      (let ((status (%process-wait (process-impl process))))
        (process-exit-status-set! process status)
        status))

    (define (process-output->string command)
      (%process-output->string (normalize-command command)))

    (define (read-port->string port)
      (let loop ((chars '()))
        (let ((char (read-char port)))
          (if (eof-object? char)
              (list->string (reverse chars))
              (loop (cons char chars))))))

    (define (process-error->string command)
      (let ((process (spawn-process* (normalize-command command) #f #f #t)))
        (let ((error-port (process-error-port process)))
          (unless error-port
            (error 'process-error->string "process has no error port" process))
          (let ((text (read-port->string error-port)))
            (process-wait process)
            text))))

    (define (process-output+error+status command)
      (let ((process (spawn-process* (normalize-command command) #f #t #t)))
        (let ((output-port (process-output-port process))
              (error-port (process-error-port process)))
          (unless output-port
            (error 'process-output+error+status "process has no output port" process))
          (unless error-port
            (error 'process-output+error+status "process has no error port" process))
          (let ((output (read-port->string output-port))
                (error-output (read-port->string error-port)))
            (list output error-output (process-wait process))))))

    (define (copy-port input output)
      (let loop ()
        (let ((char (read-char input)))
          (if (eof-object? char)
              (begin
                (flush-output-port output)
                #t)
              (begin
                (write-char char output)
                (loop))))))

    (define (process-send-string process string)
      (unless (process? process)
        (error 'process-send-string "expected process" process))
      (unless (string? string)
        (error 'process-send-string "expected string" string))
      (let ((input (process-input-port process)))
        (unless input
          (error 'process-send-string "process has no input port" process))
        (let loop ((chars (string->list string)))
          (unless (null? chars)
            (write-char (car chars) input)
            (loop (cdr chars))))
        (flush-output-port input)
        #t))

    (define (process-close-input process)
      (unless (process? process)
        (error 'process-close-input "expected process" process))
      (let ((input (process-input-port process)))
        (unless input
          (error 'process-close-input "process has no input port" process))
        (close-output-port input)))

    (define (process-close-output process)
      (unless (process? process)
        (error 'process-close-output "expected process" process))
      (let ((output (process-output-port process)))
        (unless output
          (error 'process-close-output "process has no output port" process))
        (close-input-port output)))

    (define (process-close-error process)
      (unless (process? process)
        (error 'process-close-error "expected process" process))
      (let ((error-port (process-error-port process)))
        (unless error-port
          (error 'process-close-error "process has no error port" process))
        (close-input-port error-port)))

    (define (pipe-processes source sink)
      (unless (process? source)
        (error 'pipe-processes "expected source process" source))
      (unless process-separate-ports?
        (error 'pipe-processes "backend does not expose separate process ports" conduit-implementation))
      (unless (process? sink)
        (error 'pipe-processes "expected sink process" sink))
      (let ((output (process-output-port source))
            (input (process-input-port sink)))
        (unless output
          (error 'pipe-processes "source process has no output port" source))
        (unless input
          (error 'pipe-processes "sink process has no input port" sink))
        (copy-port output input)
        (process-close-input sink)
        sink))

    (define (process-pipeline-output->string commands)
      (unless process-separate-ports?
        (error 'process-pipeline-output->string
               "backend does not expose separate process ports"
               conduit-implementation))
      (unless (and (pair? commands) (not (null? commands)))
        (error 'process-pipeline-output->string "expected non-empty command list" commands))
      (let loop ((source (spawn-process* (normalize-command (car commands)) #f #t #f))
                 (rest (cdr commands)))
        (if (null? rest)
            (let ((output (read-port->string (process-output-port source))))
              (process-wait source)
              output)
            (let ((sink (spawn-process* (normalize-command (car rest)) #t #t #f)))
              (pipe-processes source sink)
              (process-wait source)
              (loop sink (cdr rest))))))))
