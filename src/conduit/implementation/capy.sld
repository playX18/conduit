(define-library (conduit implementation capy)
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
    (rename (core io process)
      (open-process-ports capy:open-process-ports)
      (process capy:process)
      (system capy:system)
      (process-wait capy:process-wait)))
  (begin
    (define %conduit-implementation 'capy)
    (define %separate-process-ports? #t)

    (define-record-type <impl-process>
      (make-impl-process pid input output error)
      impl-process?
      (pid impl-process-pid)
      (input impl-process-input)
      (output impl-process-output)
      (error impl-process-error))

    (define (shell-quote value)
      (let loop ((chars (string->list value)) (out "'"))
        (cond
          ((null? chars) (string-append out "'"))
          ((char=? (car chars) #\')
           (loop (cdr chars) (string-append out "'\\''")))
          (else
            (loop (cdr chars) (string-append out (string (car chars))))))))

    (define (command->shell-string command)
      (cond
        ((string? command) command)
        ((and (pair? command) (string? (car command)))
         (let loop ((parts command) (out ""))
           (if (null? parts)
               out
               (loop (cdr parts)
                     (if (string=? out "")
                         (shell-quote (car parts))
                         (string-append out " " (shell-quote (car parts))))))))
        (else
          (error 'spawn-process "expected command string or non-empty list of strings" command))))

    (define (status->exit status)
      (if (and (integer? status) (>= status 256))
          (quotient status 256)
          status))

    (define (read-all port)
      (let loop ((chars '()))
        (let ((char (read-char port)))
          (if (eof-object? char)
              (list->string (reverse chars))
              (loop (cons char chars))))))

    (define (%spawn-process command input? output? error?)
      (let ((cmd (command->shell-string command)))
        (if error?
            (let ((parts (capy:open-process-ports cmd)))
              (make-impl-process
                (list-ref parts 3)
                (and input? (list-ref parts 1))
                (and output? (list-ref parts 0))
                (list-ref parts 2)))
            (let ((parts (capy:process cmd)))
              (make-impl-process
                (list-ref parts 2)
                (and input? (list-ref parts 1))
                (and output? (list-ref parts 0))
                #f)))))

    (define (%process-pid process) (impl-process-pid process))
    (define (%process-input-port process) (impl-process-input process))
    (define (%process-output-port process) (impl-process-output process))
    (define (%process-error-port process) (impl-process-error process))
    (define (%shell-command command)
      (status->exit (capy:system (command->shell-string command))))
    (define (%process-wait process)
      (status->exit (capy:process-wait (impl-process-pid process))))

    (define (%process-output->string command)
      (let ((process (%spawn-process command #f #t #f)))
        (let ((out (read-all (%process-output-port process))))
          (%process-wait process)
          out)))))
