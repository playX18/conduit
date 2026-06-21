(define-library (conduit implementation unsupported-runtime)
  (export
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
    (define %separate-process-ports? #f)

    (define (unsupported who)
      (error who "operation is not supported by this Scheme implementation"))

    (define (%spawn-process command input? output? error?)
      (unsupported 'spawn-process))
    (define (%process-pid process) #f)
    (define (%process-input-port process) #f)
    (define (%process-output-port process) #f)
    (define (%process-error-port process) #f)
    (define (%shell-command command) (unsupported 'shell-command))
    (define (%process-wait process) (unsupported 'process-wait))
    (define (%process-output->string command) (unsupported 'process-output->string))))
