(define-library (conduit implementation stklos)
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
    STklos)
  (begin
    (define %conduit-implementation 'stklos)
    (define %separate-process-ports? #t)

    (define-record-type <impl-process>
      (make-impl-process native)
      impl-process?
      (native impl-process-native))

    (define (command->parts command)
      (cond
        ((string? command) (list "sh" "-c" command))
        ((and (pair? command) (string? (car command))) command)
        (else
          (error 'spawn-process "expected command string or non-empty list of strings" command))))

    (define (read-all port)
      (let loop ((chars '()))
        (let ((char (read-char port)))
          (if (eof-object? char)
              (list->string (reverse chars))
              (loop (cons char chars))))))

    (define (%spawn-process command input? output? error?)
      (make-impl-process
        (apply run-process
               (append
                 (command->parts command)
                 (append
                   (if input? (list :input :pipe) '())
                   (if output? (list :output :pipe) '())
                   (if error? (list :error :pipe) '()))))))

    (define (%process-pid process)
      (process-pid (impl-process-native process)))
    (define (%process-input-port process)
      (process-input (impl-process-native process)))
    (define (%process-output-port process)
      (process-output (impl-process-native process)))
    (define (%process-error-port process)
      (process-error (impl-process-native process)))

    (define (%shell-command command)
      (let ((process (%spawn-process command #f #f #f)))
        (%process-wait process)))

    (define (%process-wait process)
      (process-wait (impl-process-native process))
      (process-exit-status (impl-process-native process)))

    (define (%process-output->string command)
      (let ((process (%spawn-process command #f #t #f)))
        (let ((output (read-all (%process-output-port process))))
          (%process-wait process)
          output)))))
