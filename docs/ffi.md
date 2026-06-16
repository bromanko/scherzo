# Erlang FFI contracts

This document is the agent-visible contract for Scherzo's Erlang foreign-function interface (FFI) boundary. Gleam type-checks the `@external` declarations that call these modules, but it cannot verify the Erlang implementations. When changing any `src/*_ffi.erl` module or its closest Gleam wrapper, update this document, keep the Erlang wire errors finite, and map high-risk FFI failures to typed Gleam errors before the rest of the application sees them.

A function is described as blocking when it may wait for the operating system, filesystem, socket, or subprocess. A handle is owned by the caller that receives it and must be closed or cleaned up through the matching function in the same contract.

## `src/scherzo_port_ffi.erl`

The port FFI owns subprocesses used by hooks, command steps, version discovery, and pi RPC. The public Gleam wrapper is `src/scherzo/port.gleam`. Its public functions return `PortError`, not raw FFI strings.

A `Process` handle owns an Erlang port, a subprocess OS pid when available, stderr diagnostics storage, a child-pid tracking file, a launched-command status file, and the private temp directory containing those files. New handles have shape `{scherzo_process, Port, ErrPath, OsPid, ChildPidPath, TmpDir}`. Older five- and three-element handles are tolerated for cleanup and diagnostics so tests and rolling code paths remain safe.

`start/2` accepts a shell command string and working-directory string. It validates that the command is not blank and the working directory exists, creates a private directory under the OS temp root, creates stderr, child-pid, and status paths inside that directory, launches `/bin/bash` with a wrapper that records the child pid and later the launched command status, and returns immediately after the port is created. It returns `cwd_not_directory`, `invalid_command:<reason>`, `spawn_failed:<reason>`, or `unexpected_ffi_failure:<detail>`.

`start_with_env/3` is `start/2` plus an environment list. Environment entries must be string or binary key/value pairs and keys must be non-empty. It returns the same errors as `start/2` plus `invalid_env:<reason>`.

`start_argv/4` accepts an executable string, argument list, working-directory string, and environment list. It validates a non-empty executable, string or binary args, a directory working directory, and valid env entries. It should be preferred by new code because it avoids shell interpolation. It returns `cwd_not_directory`, `invalid_executable:<reason>`, `invalid_arg:<reason>`, `invalid_env:<reason>`, `spawn_failed:<reason>`, or `unexpected_ffi_failure:<detail>`.

`send_line/2` writes the supplied line plus one newline to subprocess stdin. It may block briefly on port backpressure. It returns success only if the port accepted the bytes. It returns `closed` for a closed or invalid port and `send_failed:<reason>` for other send failures.

`read_stdout_line/2` blocks until a complete stdout line, process exit, port closure, line-too-long condition, or timeout. It returns the line without the newline. Buffered bytes are preserved across calls. If the process exits with buffered bytes that do not end in a newline, those bytes are returned once before the exit status is returned. It returns `timeout`, `line_too_long:<max-bytes>`, `exit_status:<status>`, `closed`, or `unexpected_ffi_failure:<detail>`.

`read_diagnostics/1` returns captured stderr bytes as text. Before cleanup it reads the diagnostics file. After `await_exit/2` or `terminate/1`, it returns diagnostics cached before the temp directory was removed. It returns an empty string if no diagnostics were captured. It returns `diagnostics_failed:<reason>` only for real read failures.

`terminate/1` is idempotent best effort. It caches diagnostics, signals the tracked child process group and descendant PIDs with TERM then KILL as needed, closes the Erlang port, and removes the private temp directory. It preserves the process-tree cleanup behavior added before LIV-106. It returns `cleanup_failed:<reason>` only when local temp cleanup itself fails; signaling an already-gone process is not an error.

`await_exit/2` blocks up to the timeout for the launched command to complete and for residual descendants to be cleaned up. The launch wrapper records the direct command's exit status immediately after `wait`, then best-effort terminates any remaining process group descendants before the wrapper exits, so leftover helpers that inherited stdio do not keep command completion unreachable. `await_exit/2` treats that recorded launched-command status as authoritative when it is available, and retains a defensive Erlang-side cleanup path for cases where the direct child is gone before the Erlang port reports `{exit_status, Status}`. It is a status-only wait: unread stdout is intentionally discarded, including cached line-reader buffer state and queued target-port `{data, _}` messages. On success it caches diagnostics, removes the private temp directory, and returns the integer exit status. It returns `timeout` when the direct launched command is still running or residual cleanup cannot complete by the deadline, or `cleanup_failed:<reason>` if temp cleanup fails after the process has exited.

## `src/scherzo_state_ffi.erl`

The state FFI backs append-only ledger operations in `src/scherzo/state/ledger.gleam`.

`append_line/3` accepts a path, line content, and fsync flag. It appends exactly the line plus one newline, optionally syncs, and closes the file. It blocks on filesystem operations. Wire errors are phase-prefixed: `open:<reason>`, `write:<reason>`, `sync:<reason>`, `close:<reason>`, or `unexpected_ffi_failure:<detail>`. The caller owns parent-directory creation.

`append_lines/3` accepts a path, contents, and fsync flag. It appends exactly the supplied contents without adding a newline, optionally syncs, closes the file in all paths, and uses the same phase-prefixed errors as `append_line/3`.

`fold_lines/3` opens a path for reading, calls the supplied Gleam step function with accumulator, line text without a trailing newline, one-based line number, and an `is_last` flag, then closes the file. It blocks until the file is fully read, a read fails, or the callback fails. Wire errors are `open:<reason>`, `read:<reason>`, `step:<reason>`, `close:<reason>`, or `unexpected_ffi_failure:<detail>`.

`with_ledger_lock/2` uses a VM-global lock keyed by the ledger identity string, runs the operation exactly once while the lock is held, and returns the operation result. It blocks while waiting for the lock. Tests assert serialization rather than the lock implementation.

`system_time_millisecond/0` returns wall-clock milliseconds from the Erlang VM for ledger metadata and local artifacts. It is a simple value boundary and does not own resources.

## `src/scherzo_artifact_store_ffi.erl`

The artifact-store FFI is wrapped by `src/scherzo/state/artifact_store.gleam`.

`write_atomic/2` accepts a final artifact path and contents. It creates a unique temp file in the same directory as the final artifact, writes the bytes, syncs the temp file, closes it, renames it over the final path, syncs the parent directory when the platform supports that operation, and removes the temp file on any failure before rename. It blocks on filesystem operations. Wire errors identify the failed phase: `invalid_path:<reason>`, `open_temp:<reason>`, `write_temp:<reason>`, `sync_temp:<reason>`, `close_temp:<reason>`, `rename:<reason>`, `sync_parent:<reason>`, `cleanup_temp:<reason>`, or `unexpected_ffi_failure:<detail>`. Temp names must not be deterministic `final ++ ".tmp"` names because concurrent writers can collide.

## `src/scherzo_control_ffi.erl`

The control FFI backs the local loopback JSON control protocol in `src/scherzo/control/server.gleam`, `src/scherzo/control/client.gleam`, `src/scherzo/control/file.gleam`, and JSON encoding in `src/scherzo/control/protocol.gleam`.

`dynamic_to_json/1` converts a Gleam dynamic value to JSON. The current Erlang implementation delegates to the JSON encoder and may raise if the value is not encodable, so callers must keep the dynamic values produced by the protocol module and must treat encoder failures as protocol bugs rather than operator input.

`listen/2` accepts only `127.0.0.1` or `localhost` and a TCP port where zero means the OS chooses a free port. It owns a listener that must be closed by `close_listener/1`. It may block briefly while binding. Errors include `non_loopback_host_rejected`, `eaddrinuse`, `eacces`, and other atom or unexpected details.

`accept/1` blocks until a connection is accepted or the listener is closed. The caller owns the returned socket and must close it with `close_socket/1`. Errors include `closed` and socket reason atoms.

`connect/3` accepts only loopback hosts and blocks up to its timeout while connecting. The returned socket is caller-owned. Errors include `non_loopback_host_rejected`, `timeout`, `econnrefused`, `closed`, and other socket reason atoms.

`send_line/3` sends the supplied line plus one newline. The timeout argument is honored for this call by temporarily applying the socket send timeout and restoring the previous timeout afterwards when possible. Errors include `timeout`, `closed`, and socket reason atoms.

`recv_line/2` blocks up to its timeout and returns one line without the trailing newline. Errors include `timeout`, `closed`, `line_too_long`, and socket reason atoms.

`close_socket/1` and `close_listener/1` are idempotent best-effort cleanup functions and return `Nil`.

`bound_port/1` returns the actual listener port or zero for an invalid listener.

`generate_token/1` returns cryptographically random base64 bytes for positive byte counts and uses 32 bytes for invalid counts.

`chmod_private/1` sets owner-only permissions on the control file where the platform supports it. It returns atom reason strings on failure.

`getenv/1` returns a non-empty environment value or `not_found`.

## `src/scherzo_redaction_ffi.erl`

The redaction FFI is wrapped by `src/scherzo/session/redaction.gleam`. Redaction is safety-critical because failures can otherwise leak raw pi event JSON or secrets into logs and ledgers.

`redact_raw_json/3` accepts raw JSON text, a list of secret strings, and a maximum byte count. It parses JSON, replaces values under sensitive keys (`token`, `api_key`, `authorization`, or `secret` substrings), replaces configured secret string occurrences in ordinary string values, and returns `#(redacted_json, truncated)`. Malformed JSON returns an encoded safe placeholder rather than raw input. Truncation preserves a valid UTF-8 prefix.

`redact_raw_json_fail_closed/4` is the public safe entry point. It catches any lower-level redaction failure and returns the supplied failure placeholder, truncated to a safe maximum. It must never return raw input on malformed JSON or internal failure. This path intentionally does not expose typed operational errors to callers because callers need a safe string to log, not a reason that might tempt them to log raw data.

## `src/scherzo_lock_ffi.erl`

The lock FFI backs `src/scherzo/instance_lock.gleam`.

`acquire/2` accepts a lock-file path and body, creates the lock file exclusively, writes the body, and returns an opaque file handle owned by the caller. It returns `exists` when the lock already exists, `open:<reason>` for open failures, `write:<reason>` for body write failures, or `unexpected_ffi_failure:<detail>`. If writing fails after creation, it closes the handle and deletes the file best effort.

`release/2` accepts the handle and path, closes the handle, deletes the lock file, ignores repeated cleanup failures, and returns `Nil`. Stale lock detection and operator recovery belong to higher-level code.

## `src/scherzo_signal_ffi.erl`

The signal FFI is wrapped by `src/scherzo/signal.gleam`. It mutates VM-global SIGTERM handling, so ownership is global to the running Erlang VM.

Scherzo does not currently handle SIGINT directly in Gleam/OTP. The FFI below installs only a SIGTERM handler, and direct terminal Ctrl-C/SIGINT can bypass the daemon lifecycle cleanup that runs from that handler. Packaged foreground daemon startup therefore uses a shell launcher to translate interactive Ctrl-C into SIGTERM; `scherzo-start` is the deprecated compatibility alias for that same wrapper.

`install_sigterm/1` accepts a Gleam process subject, installs a SIGTERM handler, returns an opaque signal handle plus the OS process id as text, and sends exactly one stop message to the subject for the first SIGTERM handled by that installation. Errors are categorized by the wrapper as signal-server unavailable, install failed, handler verification failed, or unexpected FFI failure.

`cleanup_sigterm/1` removes the custom handler, restores the default handler when this installation replaced it, is idempotent, and returns `Nil`.

`init/1`, `handle_event/2`, `handle_call/2`, `handle_info/2`, `terminate/2`, and `code_change/3` are callback exports required by Erlang's `gen_event` behavior. They are not application API.

## `src/scherzo_config_ffi.erl`

The config/path FFI is low risk and owns no resources.

`getenv/1` returns `Ok(value)` for non-empty environment variables and `Error(Nil)` for missing or empty values.

`home/0` returns `Ok(HOME)` or `Error(Nil)` if no home directory is available.

`tmpdir/0` returns the OS temp directory, using `/tmp` when `TMPDIR` is absent. Tests must not assert host-specific absolute prefixes.

`dirname/1` returns the parent directory text using Erlang `filename:dirname/1`.

`absname/1` returns an absolute normalized path using Erlang `filename:absname/1`. Tests must not assert host-specific absolute prefixes beyond suffix or normalization behavior.

## `src/scherzo_main_ffi.erl`

`args/0` returns command-line arguments as strings. It owns no resources and does not block in normal use.

`halt/1` terminates the Erlang VM with the supplied exit code. It is intentionally effectful and must remain isolated at CLI exit boundaries.

## `src/scherzo_lifecycle_ffi.erl`

`safe_shutdown/2` calls a shutdown callback with a stop reason. It returns `Ok(Nil)` only when the callback returns that exact success shape. It returns `Error(Nil)` for callback failures, unexpected callback returns, and exceptions so shutdown code cannot crash the caller unexpectedly.

## `src/scherzo_time_ffi.erl`

`monotonic_ms/0` returns Erlang monotonic time in milliseconds for durations and timeout calculations. It is not a wall-clock timestamp and must not be persisted as real time.

`wall_clock_ms/0` returns Unix epoch milliseconds from Erlang system time for persisted timestamps and human-facing schedule identities.

## `src/scherzo_process_ext_ffi.erl`

`trap_exits/1` toggles process exit trapping for the calling Erlang process and returns the previous setting according to `erlang:process_flag/2`. It affects only the caller process.

## `src/scherzo_hash_ffi.erl`

`sha256_hex/1` accepts bytes or text, computes SHA-256, and returns a lowercase hexadecimal digest. It owns no resources and is deterministic.

## `src/scherzo_terminal_ffi.erl`

`stdout_supports_color/0` returns whether stdout appears to be a terminal with color support, honoring `NO_COLOR`, `CLICOLOR`, `FORCE_COLOR`, and `CLICOLOR_FORCE`. It fails closed to `False` on unexpected errors.

`terminal_columns/0` returns the terminal width, the positive `SCHERZO_ATTACH_COLUMNS` override, or zero when no width is known. Tests should avoid requiring a real terminal.

## Platform caveats

Scherzo supports macOS and Linux behavior for these FFI contracts. Tests must not assume identical process-table shape, signal timing, socket buffer size, permission metadata, or temp-directory path prefixes. Process cleanup assertions should use eventual polling with deadlines. Control socket tests should bind loopback and port zero. File tests should use test-created directories and should not inspect unrelated host temp files.
