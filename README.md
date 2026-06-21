# conduit

`(conduit)` is an R7RS library for process creation and process communication
through ports.

## API

Process procedures:

- `(make-command program)` creates a portable command value.
- `(command? obj)`, `(command-program command)`, and
  `(command-arguments command)` inspect command values.
- `(command-arg command argument)` returns a command with one appended argument.
- `(command-args command arguments)` returns a command with appended arguments.
- `(command->list command)` lowers a command value to `(program arg ...)`.
- `(spawn-process command)` starts `command` with stdin, stdout, and stderr pipes.
- `(spawn-process* command input? output? error?)` starts `command`, creating only
  requested standard-stream pipes.
- `(process? obj)`, `(process-pid process)`, `(process-input-port process)`,
  `(process-output-port process)`, `(process-error-port process)`, and
  `(process-exit-status process)` inspect process records.
- `process-separate-ports?` is true when the backend exposes independent child
  stdin and stdout ports, which is enough for bidirectional communication and
  simple pipelines.
- `(process-wait process)` waits for completion and returns the exit status.
- `(shell-command command)` runs a command synchronously and returns the exit status.
- `(process-output->string command)` runs a command and captures stdout.
- `(process-error->string command)` runs a command and captures stderr when the
  backend exposes a separate error port.
- `(process-output+error+status command)` runs a command and returns
  `(stdout stderr status)` for small outputs when the backend exposes separate
  stdout and stderr ports. This helper reads stdout before stderr, so it is not a
  concurrent collector for commands that can fill both pipes.
- `(read-port->string port)` reads all characters from `port`.
- `(copy-port input output)` copies characters from `input` to `output` until EOF.
- `(process-send-string process string)` writes `string` to the process input port
  and flushes it.
- `(process-close-input process)` closes the process input port, signalling EOF to
  the child process.
- `(process-close-output process)` closes the process stdout port.
- `(process-close-error process)` closes the process stderr port.
- `(pipe-processes source sink)` copies `source` stdout to `sink` stdin and then
  closes `sink` stdin.
- `(process-pipeline-output->string commands)` runs a small-output pipeline and
  captures the final stdout. It requires `process-separate-ports?`.

`command` is a `Command` value, a shell command string, or a non-empty list of
strings. Backends that only expose shell-command process creation quote list
commands before handing them to the shell.

The `Command` API intentionally models only a program and argv for now. It does
not yet model current directory, environment mutation, stdin/stdout/stderr policy,
or platform-specific creation flags because those are not consistently available
across the supported R7RS implementations.
