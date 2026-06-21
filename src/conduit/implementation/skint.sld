(define-library (conduit implementation skint)
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
    (conduit implementation unsupported-runtime))
  (begin
    (define %conduit-implementation 'skint)))
