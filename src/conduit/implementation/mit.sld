(define-library (conduit implementation mit)
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
  (import (scheme base))
  (begin
    (define %conduit-implementation 'mit)
    (define %separate-process-ports? #f)

    (load-option 'synchronous-subprocess)

    (define (unsupported who)
      (error who "operation is not supported by this MIT Scheme backend"))

    (define (%spawn-process command input? output? error?)
      (unsupported 'spawn-process))
    (define (%process-pid process) #f)
    (define (%process-input-port process) #f)
    (define (%process-output-port process) #f)
    (define (%process-error-port process) #f)

    (define (%shell-command command)
      (run-shell-command
        (if (string? command)
            command
            (unsupported 'shell-command))))

    (define (%process-wait process)
      (unsupported 'process-wait))

    (define (%process-output->string command)
      (unsupported 'process-output->string))))
