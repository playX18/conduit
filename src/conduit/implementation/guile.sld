(define-library (conduit implementation guile)
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
    (ice-9 popen)
    (only (guile)
      close-port
      module-ref
      resolve-module
      system
      system*
      status:exit-val
      waitpid))
  (begin
    (define %conduit-implementation 'guile)
    (define %separate-process-ports? #t)

    (define-record-type <impl-process>
      (make-impl-process pid input output error waited?)
      impl-process?
      (pid impl-process-pid)
      (input impl-process-input impl-process-input-set!)
      (output impl-process-output impl-process-output-set!)
      (error impl-process-error)
      (waited? impl-process-waited? impl-process-waited?-set!))

    (define guile:open-process
      (module-ref (resolve-module '(ice-9 popen)) 'open-process))

    (define (read-all port)
      (let loop ((chars '()))
        (let ((char (read-char port)))
          (if (eof-object? char)
              (list->string (reverse chars))
              (loop (cons char chars))))))

    (define (exit-status status)
      (let ((value (status:exit-val status)))
        (if value value status)))

    (define (open-mode input? output?)
      (cond
        ((and input? output?) "r+")
        (output? "r")
        (input? "w")
        (else "r")))

    (define (close-input-if-open port)
      (when (and port (input-port-open? port))
        (close-port port)))

    (define (close-output-if-open port)
      (when (and port (output-port-open? port))
        (close-port port)))

    (define (%spawn-process command input? output? error?)
      (let ((mode (open-mode input? output?)))
        (call-with-values
          (lambda ()
            (if (string? command)
                (guile:open-process mode "/bin/sh" "-c" command)
                (apply guile:open-process mode command)))
          (lambda (read-port write-port pid)
            (make-impl-process
              pid
              (and input? write-port)
              (and output? read-port)
              #f
              #f)))))

    (define (%process-pid process) (impl-process-pid process))
    (define (%process-input-port process) (impl-process-input process))
    (define (%process-output-port process) (impl-process-output process))
    (define (%process-error-port process) (impl-process-error process))

    (define (%shell-command command)
      (if (string? command)
          (exit-status (system command))
          (exit-status (apply system* command))))

    (define (%process-wait process)
      (if (impl-process-waited? process)
          0
          (begin
            (when (impl-process-input process)
              (close-output-if-open (impl-process-input process))
              (impl-process-input-set! process #f))
            (when (impl-process-output process)
              (close-input-if-open (impl-process-output process))
              (impl-process-output-set! process #f))
            (impl-process-waited?-set! process #t)
            (exit-status (cdr (waitpid (impl-process-pid process)))))))

    (define (%process-output->string command)
      (let ((process (%spawn-process command #f #t #f)))
        (let ((output (read-all (%process-output-port process))))
          (%process-wait process)
          output)))))
