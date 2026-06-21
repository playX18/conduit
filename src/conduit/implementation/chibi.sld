(define-library (conduit implementation chibi)
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
    (rename (chibi process)
      (call-with-process-io chibi:call-with-process-io)
      (process->string chibi:process->string)
      (system chibi:system)
      (waitpid chibi:waitpid)))
  (begin
    (define %conduit-implementation 'chibi)
    (define %separate-process-ports? #t)

    (define-record-type <impl-process>
      (make-impl-process pid input output error waited?)
      impl-process?
      (pid impl-process-pid)
      (input impl-process-input)
      (output impl-process-output)
      (error impl-process-error)
      (waited? impl-process-waited? impl-process-waited?-set!))

    (define (unsupported who)
      (error who "operation is not supported by this Chibi build"))

    (define (status->exit status)
      (if (and (integer? status) (>= status 256))
          (quotient status 256)
          status))

    (define (%spawn-process command input? output? error?)
      (chibi:call-with-process-io
        command
        (lambda (pid input output error)
          (make-impl-process
            pid
            (and input? input)
            (and output? output)
            (and error? error)
            #f))))

    (define (%process-pid process) (impl-process-pid process))
    (define (%process-input-port process) (impl-process-input process))
    (define (%process-output-port process) (impl-process-output process))
    (define (%process-error-port process) (impl-process-error process))

    (define (%shell-command command)
      (let ((status (if (string? command)
                        (chibi:system command)
                        (apply chibi:system command))))
        (cond
          ((and (pair? status) (pair? (cdr status))) (status->exit (cadr status)))
          ((pair? status) (car status))
          (else (status->exit status)))))

    (define (%process-wait process)
      (if (impl-process-waited? process)
          0
          (let ((status (chibi:waitpid (impl-process-pid process) 0)))
            (impl-process-waited?-set! process #t)
            (cond
              ((and (pair? status) (pair? (cdr status))) (status->exit (cadr status)))
              ((pair? status) (car status))
              (else (status->exit status))))))

    (define (%process-output->string command)
      (chibi:process->string command))))
