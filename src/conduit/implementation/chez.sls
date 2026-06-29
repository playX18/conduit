(library (conduit implementation chez)
  (export
    %conduit-implementation
    %separate-process-ports?
    %spawn-process
    %process-pid
    %process-input-port
    %process-output-port
    %process-error-port
    %shell-command
    %process-wait
    %process-output->string)
  (import
    (rnrs)
    (only (chezscheme)
      buffer-mode
      foreign-alloc
      foreign-free
      foreign-procedure
      foreign-ref
      ftype-sizeof
      load-shared-object
      machine-type
      native-transcoder
      open-process-ports
      system))

  (define %conduit-implementation 'chez)
  (define %separate-process-ports? #t)

  (define-record-type (impl-process make-impl-process impl-process?)
    (fields
      (immutable pid impl-process-pid)
      (immutable input impl-process-input)
      (immutable output impl-process-output)
      (immutable error impl-process-error)
      (mutable waited? impl-process-waited? impl-process-waited-set!)))

  (define waitpid #f)

  (define (load-libc)
    (case (machine-type)
      ((i3le ti3le a6le ta6le arm32le tarm32le arm64le tarm64le rv64le trv64le)
       (load-shared-object "libc.so.6"))
      ((i3osx ti3osx a6osx ta6osx arm64osx tarm64osx)
       (load-shared-object "libc.dylib"))
      (else
        (load-shared-object "libc.so"))))

  (define (shell-quote value)
    (let loop ((chars (string->list value)) (out "'"))
      (cond
        ((null? chars) (string-append out "'"))
        ((char=? (car chars) #\')
         (loop (cdr chars) (string-append out "'\\''")))
        (else
          (loop (cdr chars) (string-append out (string (car chars))))))))

  (define (command->shell-string command)
    (define (string-list? obj)
      (let loop ((obj obj))
        (cond
          ((null? obj) #t)
          ((and (pair? obj) (string? (car obj))) (loop (cdr obj)))
          (else #f))))
    (cond
      ((string? command) command)
      ((and (pair? command) (string-list? command))
       (let loop ((parts command) (out ""))
         (if (null? parts)
             out
             (loop (cdr parts)
                   (if (string=? out "")
                       (shell-quote (car parts))
                       (string-append out " " (shell-quote (car parts))))))))
      (else
        (assertion-violation
          'spawn-process
          "expected command string or non-empty list of strings"
          command))))

  (define (read-all port)
    (let loop ((chars '()))
      (let ((char (read-char port)))
        (if (eof-object? char)
            (list->string (reverse chars))
            (loop (cons char chars))))))

  (define (wait-status->exit-status status)
    (if (= (mod status 128) 0)
        (mod (div status 256) 256)
        (- (mod status 128))))

  (define (wait-for-pid pid)
    (let ((status-address (foreign-alloc (ftype-sizeof int))))
      (dynamic-wind
        (lambda () #t)
        (lambda ()
          (let ((result (waitpid pid status-address 0)))
            (when (= result -1)
              (assertion-violation 'process-wait "waitpid failed" pid))
            (foreign-ref 'int status-address 0)))
        (lambda ()
          (foreign-free status-address)))))

  (define (%spawn-process command input? output? error?)
    (let-values (((input output error pid)
                  (open-process-ports
                    (command->shell-string command)
                    (buffer-mode block)
                    (native-transcoder))))
      (make-impl-process
        pid
        (and input? input)
        (and output? output)
        (and error? error)
        #f)))

  (define (%process-pid process) (impl-process-pid process))
  (define (%process-input-port process) (impl-process-input process))
  (define (%process-output-port process) (impl-process-output process))
  (define (%process-error-port process) (impl-process-error process))

  (define (%shell-command command)
    (system (command->shell-string command)))

  (define (%process-wait process)
    (if (impl-process-waited? process)
        0
        (let ((status (wait-for-pid (impl-process-pid process))))
          (impl-process-waited-set! process #t)
          (wait-status->exit-status status))))

  (define (%process-output->string command)
    (let ((process (%spawn-process command #f #t #f)))
      (let ((output (read-all (%process-output-port process))))
        (%process-wait process)
        output)))

  (load-libc)
  (set! waitpid (foreign-procedure "waitpid" (int void* int) int)))
