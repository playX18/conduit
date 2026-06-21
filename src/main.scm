(import (scheme base)
        (scheme write)
        (conduit))

(display (process-output->string '("sh" "-c" "printf conduit")))
(newline)
