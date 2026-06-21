(import (scheme base)
        (scheme write)
        (conduit))

(let loop ((n 1000) (last ""))
  (if (= n 0)
      (begin
        (display "conduit bench ok")
        (newline))
      (loop (- n 1) (process-output->string '("sh" "-c" "printf x")))))
