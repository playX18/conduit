(define-library (conduit implementation ironscheme)
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
    (ironscheme clr))
  (begin
    (clr-using System.Diagnostics)

    (define %conduit-implementation 'ironscheme)
    (define %separate-process-ports? #t)

    (define-record-type <impl-process>
      (make-impl-process native input output error)
      impl-process?
      (native impl-process-native)
      (input impl-process-input)
      (output impl-process-output)
      (error impl-process-error))

    (define (string-contains-any? string chars)
      (let loop ((rest (string->list string)))
        (cond
          ((null? rest) #f)
          ((let char-loop ((chars chars))
             (cond
               ((null? chars) #f)
               ((char=? (car rest) (car chars)) #t)
               (else (char-loop (cdr chars)))))
           #t)
          (else (loop (cdr rest))))))

    (define (quote-argument argument)
      (if (string-contains-any? argument '(#\space #\tab #\newline #\"))
          (list->string
            (reverse
              (let loop ((chars (string->list argument)) (out '(#\")))
                (cond
                  ((null? chars) (cons #\" out))
                  ((char=? (car chars) #\")
                   (loop (cdr chars) (cons #\" (cons #\\ out))))
                  ((char=? (car chars) #\\)
                   (loop (cdr chars) (cons #\\ (cons #\\ out))))
                  (else
                   (loop (cdr chars) (cons (car chars) out)))))))
          argument))

    (define (join-arguments arguments)
      (let loop ((arguments arguments) (out ""))
        (cond
          ((null? arguments) out)
          ((string=? out "")
           (loop (cdr arguments) (quote-argument (car arguments))))
          (else
           (loop
             (cdr arguments)
             (string-append out " " (quote-argument (car arguments))))))))

    (define (command-program+arguments command)
      (cond
        ((string? command)
         (values "/bin/sh" (join-arguments (list "-c" command))))
        ((and (pair? command) (string? (car command)))
         (values (car command) (join-arguments (cdr command))))
        (else
          (error 'spawn-process "expected command string or non-empty list of strings" command))))

    (define (new-process program arguments input? output? error?)
      (let* ((process (clr-new Process))
             (start-info (clr-prop-get Process StartInfo process)))
        (clr-prop-set! ProcessStartInfo FileName start-info program)
        (clr-prop-set! ProcessStartInfo Arguments start-info arguments)
        (clr-prop-set! ProcessStartInfo UseShellExecute start-info #f)
        (clr-prop-set! ProcessStartInfo CreateNoWindow start-info #t)
        (clr-prop-set! ProcessStartInfo RedirectStandardInput start-info input?)
        (clr-prop-set! ProcessStartInfo RedirectStandardOutput start-info output?)
        (clr-prop-set! ProcessStartInfo RedirectStandardError start-info error?)
        (clr-call Process Start process)
        process))

    (define (%spawn-process command input? output? error?)
      (call-with-values
        (lambda () (command-program+arguments command))
        (lambda (program arguments)
          (let ((native (new-process program arguments input? output? error?)))
            (make-impl-process
              native
              (and input? (clr-prop-get Process StandardInput native))
              (and output? (clr-prop-get Process StandardOutput native))
              (and error? (clr-prop-get Process StandardError native)))))))

    (define (%process-pid process)
      (clr-prop-get Process Id (impl-process-native process)))

    (define (%process-input-port process) (impl-process-input process))
    (define (%process-output-port process) (impl-process-output process))
    (define (%process-error-port process) (impl-process-error process))

    (define (%shell-command command)
      (let ((process (%spawn-process command #f #f #f)))
        (%process-wait process)))

    (define (%process-wait process)
      (clr-call Process WaitForExit (impl-process-native process))
      (clr-prop-get Process ExitCode (impl-process-native process)))

    (define (%process-output->string command)
      (let ((process (%spawn-process command #f #t #f)))
        (let loop ((chars '()))
          (let ((char (read-char (%process-output-port process))))
            (if (eof-object? char)
                (let ((output (list->string (reverse chars))))
                  (%process-wait process)
                  output)
                (loop (cons char chars)))))))))
