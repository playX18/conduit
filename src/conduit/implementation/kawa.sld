(define-library (conduit implementation kawa)
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
    (kawa base))
  (begin
    (define %conduit-implementation 'kawa)
    (define %separate-process-ports? #t)

    (define-record-type <impl-process>
      (make-impl-process native input output error)
      impl-process?
      (native impl-process-native)
      (input impl-process-input)
      (output impl-process-output)
      (error impl-process-error))

    (define (process-input-stream process)
      ((as java.lang.Process process):getInputStream))

    (define (process-output-stream process)
      ((as java.lang.Process process):getOutputStream))

    (define (process-error-stream process)
      ((as java.lang.Process process):getErrorStream))

    (define (input-stream->port stream)
      (gnu.kawa.io.BinaryInPort stream))

    (define (output-stream->port stream)
      (java.io.OutputStreamWriter stream))

    (define (read-all port)
      (let loop ((chars '()))
        (let ((char (read-char port)))
          (if (eof-object? char)
              (list->string (reverse chars))
              (loop (cons char chars))))))

    (define (run-command command input? output? error?)
      (let ((input-redirect (if input? 'pipe 'inherit))
            (output-redirect (if output? 'pipe 'inherit))
            (error-redirect (if error? 'pipe 'inherit)))
        (if (string? command)
            (run-process
              in-from: input-redirect
              out-to: output-redirect
              err-to: error-redirect
              shell: #t
              command)
            (run-process
              in-from: input-redirect
              out-to: output-redirect
              err-to: error-redirect
              command))))

    (define (%spawn-process command input? output? error?)
      (let ((native (run-command command input? output? error?)))
        (make-impl-process
          native
          (and input? (output-stream->port (process-output-stream native)))
          (and output? (input-stream->port (process-input-stream native)))
          (and error? (input-stream->port (process-error-stream native))))))

    (define (%process-pid process) #f)
    (define (%process-input-port process) (impl-process-input process))
    (define (%process-output-port process) (impl-process-output process))
    (define (%process-error-port process) (impl-process-error process))

    (define (%shell-command command)
      (process-exit-wait (run-command command #f #f #f)))

    (define (%process-wait process)
      (process-exit-wait (impl-process-native process)))

    (define (%process-output->string command)
      (let ((process (%spawn-process command #f #t #f)))
        (let ((output (read-all (%process-output-port process))))
          (%process-wait process)
          output)))))
