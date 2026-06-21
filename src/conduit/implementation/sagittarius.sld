(define-library (conduit implementation sagittarius)
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
    (scheme base)
    (rnrs io ports)
    (rename (sagittarius process)
      (make-process sagittarius:make-process)
      (process-call sagittarius:process-call)
      (process-input-port sagittarius:process-input-port)
      (process-output-port sagittarius:process-output-port)
      (process-error-port sagittarius:process-error-port)
      (process-wait sagittarius:process-wait)
      (run sagittarius:run)))
  (begin
    (define %conduit-implementation 'sagittarius)
    (define %separate-process-ports? #t)

    (define-record-type <impl-process>
      (make-impl-process native pid input output error)
      impl-process?
      (native impl-process-native)
      (pid impl-process-pid)
      (input impl-process-input)
      (output impl-process-output)
      (error impl-process-error))

    (define (command-program+arguments command)
      (cond
        ((string? command) (values "sh" (list "-c" command)))
        ((and (pair? command) (string? (car command)))
         (values (car command) (cdr command)))
        (else
          (error 'spawn-process "expected command string or non-empty list of strings" command))))

    (define (textual-port port)
      (and port (transcoded-port port (native-transcoder))))

    (define (read-all port)
      (let loop ((chars '()))
        (let ((char (read-char port)))
          (if (eof-object? char)
              (list->string (reverse chars))
              (loop (cons char chars))))))

    (define (%spawn-process command input? output? error?)
      (call-with-values
        (lambda () (command-program+arguments command))
        (lambda (program arguments)
          (let* ((native (sagittarius:make-process program arguments))
                 (pid (sagittarius:process-call native)))
            (make-impl-process
              native
              pid
              (and input? (textual-port (sagittarius:process-input-port native)))
              (and output? (textual-port (sagittarius:process-output-port native)))
              (and error? (textual-port (sagittarius:process-error-port native))))))))

    (define (%process-pid process) (impl-process-pid process))
    (define (%process-input-port process) (impl-process-input process))
    (define (%process-output-port process) (impl-process-output process))
    (define (%process-error-port process) (impl-process-error process))

    (define (%shell-command command)
      (call-with-values
        (lambda () (command-program+arguments command))
        (lambda (program arguments)
          (apply sagittarius:run program arguments))))

    (define (%process-wait process)
      (sagittarius:process-wait (impl-process-native process)))

    (define (%process-output->string command)
      (let ((process (%spawn-process command #f #t #f)))
        (let ((output (read-all (%process-output-port process))))
          (%process-wait process)
          output)))))
