(define-library (conduit implementation mosh)
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
    (rename (mosh process)
      (pipe mosh:pipe)
      (spawn mosh:spawn)
      (waitpid mosh:waitpid)
      (call-process mosh:call-process)))
  (begin
    (define %conduit-implementation 'mosh)
    (define %separate-process-ports? #t)

    (define-record-type <impl-process>
      (make-impl-process pid input output error)
      impl-process?
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
          (call-with-values
            (lambda ()
              (if input?
                  (mosh:pipe)
                  (values #f #f)))
            (lambda (child-stdin parent-input)
              (call-with-values
                (lambda ()
                  (if output?
                      (mosh:pipe)
                      (values #f #f)))
                (lambda (parent-output child-stdout)
                  (call-with-values
                    (lambda ()
                      (if error?
                          (mosh:pipe)
                          (values #f #f)))
                    (lambda (parent-error child-stderr)
                      (call-with-values
                        (lambda ()
                          (mosh:spawn
                            program
                            arguments
                            (list child-stdin child-stdout child-stderr)))
                        (lambda (pid ignored-input ignored-output ignored-error)
                          (when child-stdin (close-input-port child-stdin))
                          (when child-stdout (close-output-port child-stdout))
                          (when child-stderr (close-output-port child-stderr))
                          (make-impl-process
                            pid
                            (textual-port parent-input)
                            (textual-port parent-output)
                            (textual-port parent-error)))))))))))))

    (define (%process-pid process) (impl-process-pid process))
    (define (%process-input-port process) (impl-process-input process))
    (define (%process-output-port process) (impl-process-output process))
    (define (%process-error-port process) (impl-process-error process))

    (define (%shell-command command)
      (%process-wait (%spawn-process command #f #f #f)))

    (define (%process-wait process)
      (call-with-values
        (lambda () (mosh:waitpid (impl-process-pid process)))
        (lambda (pid status signal)
          (or status signal))))

    (define (%process-output->string command)
      (let ((process (%spawn-process command #f #t #f)))
        (let ((output (read-all (%process-output-port process))))
          (%process-wait process)
          output)))))
