(define-library (conduit implementation gauche)
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
    (gauche base)
    (gauche process))
  (begin
    (define %conduit-implementation 'gauche)
    (define %separate-process-ports? #t)

    (define-record-type <impl-process>
      (make-impl-process native)
      impl-process?
      (native impl-process-native))

    (define (command->argv command)
      (cond
        ((and (pair? command) (string? (car command))) command)
        ((string? command) (list "sh" "-c" command))
        (else
          (error 'spawn-process "expected command string or non-empty list of strings" command))))

    (define (read-all port)
      (let loop ((chars '()))
        (let ((char (read-char port)))
          (if (eof-object? char)
              (list->string (reverse chars))
              (loop (cons char chars))))))

    (define (exit-status status)
      (if (sys-wait-exited? status)
          (sys-wait-exit-status status)
          status))

    (define (%spawn-process command input? output? error?)
      (make-impl-process
        (run-process
          (command->argv command)
          :input (if input? :pipe :null)
          :output (if output? :pipe :null)
          :error (if error? :pipe :null))))

    (define (%process-pid process)
      (process-pid (impl-process-native process)))
    (define (%process-input-port process)
      (process-input (impl-process-native process)))
    (define (%process-output-port process)
      (process-output (impl-process-native process)))
    (define (%process-error-port process)
      (process-error (impl-process-native process)))

    (define (%shell-command command)
      (let ((process
              (run-process
                (command->argv command)
                :input :null
                :wait #t)))
        (exit-status (process-exit-status process))))

    (define (%process-wait process)
      (process-wait (impl-process-native process))
      (exit-status (process-exit-status (impl-process-native process))))

    (define (%process-output->string command)
      (let ((process (%spawn-process command #f #t #f)))
        (let ((output (read-all (%process-output-port process))))
          (%process-wait process)
          output)))))
