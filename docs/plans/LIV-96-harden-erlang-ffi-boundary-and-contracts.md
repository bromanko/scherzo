# Harden Erlang FFI Boundary and Contracts

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

Scherzo relies on Erlang foreign-function interface modules for work that Gleam cannot safely express by itself: launching subprocesses, handling local sockets, writing ledgers and artifacts, reacting to signals, and redacting raw JSON. The FFI boundary is the place where Gleam's type guarantees stop. After this change, a maintainer or agent can modify these modules with an explicit contract for every public FFI function, a finite set of typed Gleam errors for high-risk boundaries, and regression tests that prove process cleanup, temp-file cleanup, ledger locking, atomic writes, and fail-closed redaction continue to work.

The operator-visible outcome is fewer operational surprises from agent-written maintenance. A failed subprocess launch should become a typed error such as `CwdNotDirectory` or `SpawnFailed`, not an arbitrary string. A terminated subprocess should not leave tracked helper processes or private temp files behind. A ledger append or artifact write should either complete atomically or leave clear, typed evidence of the failed phase. A redaction failure should still return a safe placeholder instead of raw sensitive JSON.

## Problem Framing and Constraints

The current repository contains several Erlang modules under `src/` with names ending in `_ffi.erl`. They are called from Gleam through `@external` functions. These modules do necessary unsafe work, but their contracts are mostly implicit in code. Several high-risk functions return `Result(_, String)` through Gleam wrappers, and many Erlang catch-all paths convert exceptions into formatted strings. That is hard for a future agent to handle correctly because the agent must infer which strings are stable, which resources it owns, whether a call blocks, and how cleanup is supposed to happen.

LIV-82 already addressed a concrete leaked-process class. This plan uses LIV-82 as context and does not redo the process-tree termination design unless a new regression test proves that the existing behavior is incomplete. The work here is broader contract hardening: document the boundary, make error results typed at the Gleam layer, add focused tests, and make the riskiest FFI resource ownership rules explicit.

The implementation must stay proportionate. It should not replace Erlang FFI with a new runtime abstraction, should not redesign Scherzo's daemon or control protocol, and should not introduce a large platform layer. The right-sized change is a documented contract, typed errors at the immediate Gleam wrapper boundary, small behavior changes for temp-file and atomic-write safety, and regression tests around the operational bugs this boundary can cause.

## Strategy Overview

The strategy is to work from highest risk to lowest risk. First, create an agent-visible FFI contract document and typed error design so later edits have a shared vocabulary. Second, harden `src/scherzo_port_ffi.erl` because it owns subprocesses, stderr files, child PID tracking, blocking reads, timeouts, and process-tree termination. Third, harden filesystem persistence boundaries in `src/scherzo_state_ffi.erl`, `src/scherzo_artifact_store_ffi.erl`, and `src/scherzo_lock_ffi.erl`. Fourth, harden local control socket and redaction boundaries. Fifth, document low-risk FFI modules without unnecessary behavior changes.

The main design choice is that Erlang FFI functions may continue to return Erlang terms that are convenient for the boundary, but public Gleam wrappers must not expose free-form FFI strings for high-risk operations. The Erlang side should return a finite documented set of error tags or tag-prefixed binaries. The Gleam wrapper should map those tags immediately into typed errors. Any truly unexpected formatted exception should be wrapped in an explicit `UnexpectedFfiFailure` variant so it cannot masquerade as a documented operational case.

The plan answers the temp-file question directly: port temp files should move into a private per-process temp directory, and that directory should be cleaned after `await_exit` and `terminate`. To preserve diagnostics for callers, `await_exit` and `terminate` should read and cache stderr before cleanup so `read_diagnostics` can still return diagnostics after the files are gone.

## Alternatives Considered

The simplest plausible alternative is documentation only. That would help agents understand the current boundary, but it would leave `Result(_, String)` APIs in place for subprocess, control socket, ledger, artifact, lock, and signal failures. Documentation alone would not make missing pattern matches visible to the compiler and would not prevent a future change from returning a new ad hoc string.

Another alternative is a large rewrite that removes Erlang FFI or replaces all FFI modules with one generic runtime service. That is disproportionate. The existing modules are small and map to real runtime responsibilities. Most low-risk modules, such as hashing and time, only need explicit documentation. The high-risk modules need targeted typed errors and tests, not a new subsystem.

A third alternative is changing Erlang functions to construct Gleam custom type values directly. That would make the wire representation between Erlang and Gleam more brittle for maintainers who are not familiar with generated Gleam runtime terms. This plan instead keeps the Erlang wire contract finite and documented, then maps it to typed Gleam errors in the closest wrapper module.

## Risks and Countermeasures

The first risk is breaking callers that currently expect `Result(_, String)`. The countermeasure is to update one boundary at a time, add conversion functions such as `port_error_to_string` only at display or logging edges, and keep compatibility helpers temporarily if a broad call-site update is too risky. The plan includes a clarification item about whether any downstream consumer outside this repository depends on the current string-returning public API.

The second risk is hiding a real subprocess exit status behind cleanup errors. The countermeasure is to define the port contract before coding: `await_exit` must collect exit status, cache diagnostics, attempt cleanup, and return a typed cleanup error only when cleanup failure is operationally important. Tests must verify the normal path removes temp directories and still makes diagnostics available.

The third risk is introducing platform-flaky process tests. Process trees, signal timing, and `ps` output differ between macOS and Linux. The countermeasure is to assert eventual behavior rather than exact process-table shape, use bounded polling with generous deadlines, avoid platform-specific path assertions, and run the new integration tests on both macOS and Linux before accepting the implementation.

The fourth risk is making persistence code more complex than necessary. The countermeasure is to keep `scherzo_state_ffi` ledger behavior mostly unchanged except for typed errors and tests, while changing `scherzo_artifact_store_ffi` only where there is a concrete atomicity issue: deterministic temp paths should become unique same-directory temp paths with cleanup on failure.

The fifth risk is weakening fail-closed redaction while trying to type errors. The countermeasure is that redaction should not expose operational errors to callers on the fail-closed path. If redaction cannot parse or process raw JSON safely, it must return the configured placeholder, truncated to the maximum byte count, and must not return raw input.

## Progress

- [x] (2026-05-06 00:00Z) Drafted this ExecPlan for LIV-96 after inspecting source-control status, FFI exports, Gleam external references, and the highest-risk Erlang FFI modules.
- [x] (2026-05-07 16:08Z) Created `docs/ffi.md` as the agent-visible FFI contract inventory and linked it from `docs/ARCHITECTURE.md`.
- [x] (2026-05-07 16:40Z) Hardened `src/scherzo_port_ffi.erl` and `src/scherzo/port.gleam` with finite raw error tags, typed `PortError`, private per-process temp directories, cached diagnostics after cleanup, and port lifecycle regression tests.
- [x] (2026-05-07 17:05Z) Hardened artifact, ledger, and lock persistence boundaries with typed wrapper errors, unique same-directory artifact temp files, phase-tagged ledger and lock errors, and persistence regression tests.
- [x] (2026-05-07 17:26Z) Hardened control and redaction coverage by honoring `send_line/3` send timeouts, mapping client transport failures to typed errors, adding raw control FFI tests, and adding a redaction fail-closed fallback test.
- [x] (2026-05-07 17:34Z) Documented stable low-risk FFI modules, added low-risk FFI smoke tests, and ran local formatting, test, and glinter validation on the primary development platform. Linux validation remains a reviewer or CI follow-up.
- [x] (2026-05-07 17:45Z) Applied review feedback by terminating subprocesses on generic non-timeout port read errors, closing control event stream sockets on remote `Closed`, preserving rejected control hosts in `NonLoopbackHostRejected`, and rerunning targeted formatting, full test, and glinter validation.

## Surprises & Discoveries

- Observation: `src/scherzo_port_ffi.erl` already tracks a port OS PID and a child PID file and has TERM then KILL grace periods for process-tree cleanup.
  Evidence: the module exports `terminate/1` and `await_exit/2`, reads a child PID file, enumerates descendants from a process table, and signals process groups and descendant PIDs.
- Observation: `src/scherzo_port_ffi.erl` currently creates separate stderr and child PID temp files under the OS temp root and does not put them in a private per-process directory.
  Evidence: `stderr_path/0` and `child_pid_path/0` call a shared `tmp_path/2` helper that joins the OS temp base with prefixed unique filenames.
- Observation: `src/scherzo_artifact_store_ffi.erl` writes through a deterministic temp path formed by appending `.tmp` to the final artifact path.
  Evidence: `write_atomic/2` assigns `Temp = Final ++ ".tmp"` before writing and renaming.
- Observation: `src/scherzo_control_ffi.erl` accepts a timeout argument in `send_line/3` but the function body ignored it before this implementation.
  Evidence: the original implementation named the third argument `_TimeoutMs` and called `gen_tcp:send/2` directly; the implemented version temporarily applies `{send_timeout, Timeout}` for the call and restores the previous socket option when possible.
- Observation: A raw accepted TCP socket used in a test closes when the short-lived accepting process exits, because the Erlang socket remains owned by its accepting process unless ownership is transferred.
  Evidence: an initial control FFI test received `Error("closed")` after passing an accepted socket from a spawned accept process back to the test process; the passing test now performs `recv_line` inside the accepting process instead.
- Observation: The local development platform completed the full Gleam test suite with 800 passing tests after the FFI hardening changes.
  Evidence: `direnv exec . gleam test` reported `800 passed, no failures` on 2026-05-07.
- Observation: Review feedback showed that typed error handling must also preserve cleanup on less common terminal paths, not only on normal exit and timeouts.
  Evidence: `src/scherzo/command_step.gleam` now calls `port.terminate` after generic `read_stdout_line` errors such as `LineTooLong`, and `src/scherzo/control/client.gleam` closes stream sockets when `recv_line` returns `Closed`.

## Decision Log

- Decision: Use a documentation-first, boundary-by-boundary hardening pass rather than a runtime rewrite.
  Rationale: The current FFI modules are small and purposeful; the operational risk comes from implicit contracts, string errors, and missing tests, not from the mere existence of FFI.
  Date: 2026-05-06
- Decision: Convert high-risk FFI errors to typed Gleam errors at the wrapper boundary while keeping Erlang wire terms finite and documented.
  Rationale: Gleam callers get compiler-visible cases, while Erlang modules remain maintainable without relying on generated Gleam custom-type representations.
  Date: 2026-05-06
- Decision: Move port stderr and child PID files into a private per-process temp directory and clean it after `await_exit` or `terminate`.
  Rationale: Private directories make ownership clear, avoid unrelated temp-file collisions, and give tests a concrete cleanup invariant.
  Date: 2026-05-06
- Decision: Treat LIV-82 process cleanup as prior context and add regression tests instead of redesigning it up front.
  Rationale: The ticket asks for a broader FFI contract pass and explicitly says not to duplicate completed process-leak work.
  Date: 2026-05-06
- Decision: Keep Erlang raw FFI returns as finite strings or phase-prefixed strings, then map them in the closest Gleam wrapper rather than constructing Gleam custom values directly in Erlang.
  Rationale: This preserves maintainable Erlang wire terms while making public high-risk wrappers typed for Gleam callers.
  Date: 2026-05-07
- Decision: Expose `port.temp_dir_for_test` as a narrow inspection helper backed by `scherzo_port_ffi:temp_dir_for_test/1` instead of exposing temp storage in normal subprocess APIs.
  Rationale: The port cleanup contract needs direct regression coverage, but production callers should continue treating temp storage as an implementation detail.
  Date: 2026-05-07
- Decision: Treat unsupported parent-directory sync in artifact writes as best effort while still returning `sync_parent` if an opened parent directory fails to sync.
  Rationale: Directory sync support differs by platform; preserving successful writes on unsupported platforms is safer than turning platform limitations into artifact write failures.
  Date: 2026-05-07
- Decision: Add typed signal installation errors in `src/scherzo/signal.gleam` while keeping the test injection hook string-based at the raw FFI seam.
  Rationale: The production application boundary should be typed, but tests still need to simulate exact raw FFI failures without constructing Erlang handler state.
  Date: 2026-05-07
- Decision: Treat generic port read errors and remote stream closure as resource cleanup edges before returning typed errors.
  Rationale: The FFI boundary hardening target includes resource ownership guarantees on every finite error tag, not just converting the tags to Gleam types.
  Date: 2026-05-07
- Decision: Preserve the attempted control host when mapping connect-time `non_loopback_host_rejected` failures.
  Rationale: Operators need the rejected value in diagnostics; the empty-host fallback remains only for unexpected non-connect contexts where no host is available.
  Date: 2026-05-07

## Outcomes & Retrospective

The implementation established `docs/ffi.md` as the durable contract for every Erlang FFI export, linked it from `docs/ARCHITECTURE.md`, and brought the highest-risk wrappers closer to the plan's typed-error target. Port, artifact, ledger, lock, signal, and client control transport failures now map to typed Gleam errors at their wrapper boundaries while raw Erlang returns remain finite and documented.

The highest-risk resource ownership changes are in place. Port stderr and child-pid files now live in private per-process temp directories. `await_exit/2` and `terminate/1` cache diagnostics before removing those directories, and tests prove diagnostics remain readable after cleanup. Artifact writes now use unique same-directory temp files and cleanup failure paths instead of deterministic `.tmp` paths.

Regression coverage now includes port launch errors, read timeout, line-too-long, process-tree termination, temp cleanup, diagnostics-after-cleanup, artifact atomic write and concurrent writer behavior, lock release idempotence, raw control socket send/receive/timeout/closed behavior, redaction fail-closed fallback, and low-risk hash/time/config/terminal smoke behavior. Local validation passed on the current development platform: formatting, the full Gleam test suite, and glinter's production gate all completed with zero failures or errors.

A review-feedback pass tightened the ownership and diagnostics outcomes: uncommon port read failures now terminate the subprocess after diagnostics capture, remote stream closure closes the client socket, and connect-time non-loopback rejection messages include the rejected host. After that pass, targeted formatting for the changed Gleam files passed, the full Gleam test suite still reported 800 passed, no failures, and glinter reported 0 errors with the existing 358-warning ratchet.

The main remaining validation gap is cross-platform execution. This workspace ran on the local primary platform only; Linux or the other supported platform should be covered by CI or reviewer validation before final acceptance of platform-sensitive subprocess, signal, socket, and filesystem behavior.

## Context and Orientation

This repository is a Gleam project that uses Erlang modules for runtime operations that Gleam delegates through `@external`. An FFI function is a function implemented outside Gleam but called as if it were a Gleam function. Because the compiler cannot verify the Erlang implementation, every FFI function needs an explicit human-readable contract.

The Erlang FFI modules live in `src/` and have names like `src/scherzo_port_ffi.erl`. Gleam wrapper modules live under `src/scherzo/` and contain `@external(erlang, "module", "function")` declarations. The wrapper is the correct place to hide raw Erlang wire details from the rest of the Gleam codebase.

The most important high-risk wrapper surfaces verified during drafting are `src/scherzo/port.gleam` for subprocesses, `src/scherzo/state/ledger.gleam` for ledgers, `src/scherzo/state/artifact_store.gleam` and related artifact modules for artifact writes, `src/scherzo/control/server.gleam`, `src/scherzo/control/client.gleam`, and `src/scherzo/control/file.gleam` for local control sockets and control files, `src/scherzo/session/redaction.gleam` for fail-closed redaction, `src/scherzo/instance_lock.gleam` for lock files, and `src/scherzo/signal.gleam` for SIGTERM handling.

The implementation should create `docs/ffi.md` as the agent-visible contract document. If `docs/ARCHITECTURE.md` exists when implementation begins, add a short repository-relative link from that file to `docs/ffi.md`; otherwise do not create architecture documentation solely for a link.

## Preconditions and Verified Facts

The working copy was clean when this plan was drafted. Source-control inspection used this command from the repository root:

    jj status --color=never

The command reported no working-copy changes and a current empty workspace commit. It also reported an unrelated bookmark conflict on `main`; this plan does not require resolving bookmarks because the workflow contract says not to manage workspaces.

The current Erlang FFI module inventory is as follows.

- `src/scherzo_main_ffi.erl` exports `args/0` and `halt/1`.
- `src/scherzo_lifecycle_ffi.erl` exports `safe_shutdown/2`.
- `src/scherzo_time_ffi.erl` exports `monotonic_ms/0`.
- `src/scherzo_process_ext_ffi.erl` exports `trap_exits/1`.
- `src/scherzo_redaction_ffi.erl` exports `redact_raw_json/3` and `redact_raw_json_fail_closed/4`.
- `src/scherzo_port_ffi.erl` exports `start/2`, `start_with_env/3`, `start_argv/4`, `send_line/2`, `read_stdout_line/2`, `read_diagnostics/1`, `terminate/1`, and `await_exit/2`.
- `src/scherzo_hash_ffi.erl` exports `sha256_hex/1`.
- `src/scherzo_state_ffi.erl` exports `append_line/3`, `append_lines/3`, `fold_lines/3`, `with_ledger_lock/2`, and `system_time_millisecond/0`.
- `src/scherzo_signal_ffi.erl` exports `install_sigterm/1`, `cleanup_sigterm/1`, and the `gen_event` callback functions `init/1`, `handle_event/2`, `handle_call/2`, `handle_info/2`, `terminate/2`, and `code_change/3`.
- `src/scherzo_terminal_ffi.erl` exports `stdout_supports_color/0` and `terminal_columns/0`.
- `src/scherzo_lock_ffi.erl` exports `acquire/2` and `release/2`.
- `src/scherzo_artifact_store_ffi.erl` exports `write_atomic/2`.
- `src/scherzo_config_ffi.erl` exports `getenv/1`, `home/0`, `tmpdir/0`, `dirname/1`, and `absname/1`.
- `src/scherzo_control_ffi.erl` exports `dynamic_to_json/1`, `listen/2`, `accept/1`, `connect/3`, `send_line/3`, `recv_line/2`, `close_socket/1`, `close_listener/1`, `bound_port/1`, `generate_token/1`, `chmod_private/1`, and `getenv/1`.

The current high-risk modules inspected for behavior were `src/scherzo_port_ffi.erl`, `src/scherzo_state_ffi.erl`, `src/scherzo_artifact_store_ffi.erl`, `src/scherzo_control_ffi.erl`, `src/scherzo_redaction_ffi.erl`, `src/scherzo_lock_ffi.erl`, `src/scherzo_signal_ffi.erl`, and `src/scherzo_config_ffi.erl`. Low-risk FFI modules still need to be re-read during implementation before documenting their exact behavior, but their exported function names are already inventoried above.

The expected validation commands use the repo-local environment. From the repository root, prefer:

    direnv exec . gleam format --check src test
    direnv exec . gleam test

If `direnv exec .` reports that `.envrc` is blocked, inspect `.envrc`, run `direnv allow .` from the repository root, and retry the same commands. Treat that as environment setup, not a test failure.

## Scope Boundaries

In scope: documenting every public Erlang FFI function; adding typed Gleam error types for high-risk FFI wrappers; changing port temp files to private per-process temp directories with cleanup; adding regression tests for subprocess lifecycle, timeout, line-too-long behavior, process-tree cleanup, malformed launch inputs, temp cleanup, ledger locking, artifact atomicity, and redaction failure behavior; and adding platform caveats for macOS and Linux.

Also in scope: small behavior changes that directly follow from the contract. These include unique same-directory artifact temp paths, cleanup of artifact temp files after failed writes, honoring or explicitly removing unused timeout semantics in control socket send operations, and validating subprocess launch inputs before they reach Erlang operations that would fail with formatted catch-all strings.

Out of scope: redesigning the Scherzo daemon, replacing the control protocol, changing Linear integration behavior, introducing a new process supervisor, changing LIV-82's process-tree algorithm unless regression tests fail, adding support for non-Erlang Gleam targets, or changing low-risk FFI modules beyond documentation and typed wrapper cleanup.

Stable enough for documentation only unless inspection reveals a mismatch: `src/scherzo_main_ffi.erl`, `src/scherzo_lifecycle_ffi.erl`, `src/scherzo_time_ffi.erl`, `src/scherzo_process_ext_ffi.erl`, `src/scherzo_hash_ffi.erl`, `src/scherzo_terminal_ffi.erl`, and most of `src/scherzo_config_ffi.erl`. These modules either return simple values, wrap a narrow VM primitive, or have low resource-ownership risk.

Needs behavior changes or typed-error work: `src/scherzo_port_ffi.erl`, `src/scherzo_artifact_store_ffi.erl`, `src/scherzo_control_ffi.erl`, `src/scherzo_state_ffi.erl`, `src/scherzo_lock_ffi.erl`, `src/scherzo_signal_ffi.erl`, and `src/scherzo_redaction_ffi.erl`. The state, lock, signal, and redaction modules may need fewer Erlang behavior changes than the port, artifact, and control modules, but they still need explicit wrapper contracts and tests.

## Desired FFI Contracts

The implementation must encode the following contracts in `docs/ffi.md`, in Gleam wrapper types, and in tests. The document should state inputs, outputs, documented error tags, ownership, cleanup, and blocking behavior for each function.

For `src/scherzo_port_ffi.erl`, a `Process` handle owns an Erlang port, stderr diagnostics storage, a child PID tracking file, and the private temp directory containing those files. `start/2` accepts a shell command string and working directory string. It validates that the working directory exists, opens a subprocess port, creates private diagnostics storage, and returns immediately after the port is created; it does not wait for the child command to finish. `start_with_env/3` adds an environment list and must reject malformed environment keys or values with a documented invalid-env error. `start_argv/4` accepts an executable string, argument list, working directory string, and environment list; it should be preferred by new code because it avoids shell interpolation. It must reject an empty executable, malformed args, malformed env, and non-directory working directories with finite errors.

For port writes, `send_line/2` writes the provided line plus one newline to the subprocess stdin. It may block briefly on port backpressure. It returns success only if the write was accepted by the port, and it returns typed errors for closed ports, invalid process handles, or send failures. For port reads, `read_stdout_line/2` blocks until a line, exit, closure, line-too-long condition, or timeout. It returns the line without the newline. It must preserve buffered data across calls, return a typed timeout on timeout, return a typed line-too-long error when the configured maximum is exceeded, return a typed process-exited error with the status when no buffered output remains after exit, and return a typed closed error when the port closes without status.

For port diagnostics, `read_diagnostics/1` returns captured stderr bytes as text. Before process cleanup it reads from the diagnostics file. After `await_exit/2` or `terminate/1`, it returns cached diagnostics captured before cleanup. It should not fail merely because cleanup already removed the temp directory. For port cleanup, `terminate/1` is idempotent best effort: it signals the tracked child process group and descendant PIDs, closes the port, waits as the existing LIV-82 behavior requires, caches diagnostics, and removes the private temp directory. `await_exit/2` blocks up to its timeout for the port exit and tracked OS process disappearance, returns the exit status on success, caches diagnostics, and removes the private temp directory. Normal cleanup must remove the temp directory; cleanup failure must be typed and visible without losing available diagnostics.

The documented port wire error tags should include at least `cwd_not_directory`, `invalid_command`, `invalid_executable`, `invalid_arg`, `invalid_env`, `spawn_failed`, `timeout`, `line_too_long`, `exit_status`, `closed`, `send_failed`, `diagnostics_failed`, `cleanup_failed`, and `unexpected_ffi_failure`. The public Gleam wrapper should expose a `PortError` type with variants matching those meanings, including an exit-status variant that carries the integer status and an unexpected variant that carries the original detail.

For `src/scherzo_state_ffi.erl`, `append_line/3` accepts a path, line content, and fsync flag, appends exactly the line plus one newline, closes the file, and optionally syncs before close. `append_lines/3` accepts a path, bytes or text contents, and fsync flag, appends exactly the provided contents without adding a newline, closes the file, and optionally syncs. Both functions block on filesystem operations and must return typed file errors that identify the phase: open, write, sync, or close. The caller owns path selection and parent-directory creation. These functions must not leave an open file handle after any error.

For ledger reads, `fold_lines/3` opens the path for reading, calls the provided step function with accumulator, line bytes without trailing newline, one-based line number, and an is-last flag, and closes the file in all success and failure paths. It blocks until the file is fully read or an error occurs. It returns typed errors for open, read, close, step callback failure, and unexpected FFI failure. For `with_ledger_lock/2`, the lock key is a ledger identity string. The function blocks while acquiring the VM-global ledger lock, runs the operation exactly once under that lock, and returns the operation result. Tests should verify serialization, not a specific lock implementation. `system_time_millisecond/0` returns wall-clock milliseconds and is documentation-only.

For `src/scherzo_artifact_store_ffi.erl`, `write_atomic/2` accepts a final artifact path and contents. It must create a unique temp file in the same directory as the final artifact, write bytes, sync the temp file, close it, rename it over the final path atomically, sync the parent directory when supported, and delete the temp file on any failure before rename. It blocks on filesystem operations. Its typed errors must identify `open_temp`, `write_temp`, `sync_temp`, `close_temp`, `rename`, `sync_parent`, `cleanup_temp`, `invalid_path`, and `unexpected_ffi_failure`. It must not use a deterministic final-path-plus-suffix temp name because concurrent writers can collide.

For `src/scherzo_control_ffi.erl`, `dynamic_to_json/1` converts a Gleam dynamic value to JSON and must document whether it can raise; if it can fail, the wrapper must expose a typed encode error rather than a catch-all string. `listen/2` accepts only loopback hosts and a port, where port zero means the OS chooses a port. It owns a listener that must be closed by `close_listener/1`. It may block briefly while binding and returns typed errors for non-loopback host, address in use, permission denied, and unexpected failure. `accept/1` blocks until a connection is accepted or the listener is closed, and the caller owns the returned socket. `connect/3` accepts only loopback hosts and blocks up to its timeout. `send_line/3` sends one line plus newline and must either honor the supplied timeout or the public contract must remove the unused timeout argument in a compatibility-safe way. `recv_line/2` blocks up to its timeout and returns a line without newline, or typed timeout, closed, line-too-long, or socket failure. `close_socket/1` and `close_listener/1` are idempotent and return `Nil`. `bound_port/1` returns the actual bound port or zero for an invalid listener. `generate_token/1` returns cryptographically random base64 bytes for positive byte counts and uses the documented default for invalid counts. `chmod_private/1` sets private owner-only permissions on the control file where the platform supports it. `getenv/1` returns not-found as a typed control-file environment error.

For `src/scherzo_redaction_ffi.erl`, `redact_raw_json/3` accepts raw JSON, a list of secret strings, and a maximum byte count. It returns redacted JSON text plus a boolean indicating truncation. It must redact sensitive key names and secret string occurrences, return a safe malformed-JSON placeholder on parse failure, and truncate to a valid UTF-8 prefix. `redact_raw_json_fail_closed/4` is the safe public entry point: on any error it returns the failure placeholder, truncated to a safe maximum, and must never return raw input. This module should not expose typed operational errors on the fail-closed path because callers need a safe string, not a reason to accidentally log raw data.

For `src/scherzo_lock_ffi.erl`, `acquire/2` accepts a lock-file path and body, creates the file exclusively, writes the body, and returns an opaque lock handle owned by the caller. It returns a typed already-held error for an existing lock, typed file errors for open or write failures, and must delete the file if writing the body fails after creation. `release/2` accepts the handle and path, closes the handle, deletes the lock file, ignores repeated cleanup failures, and returns `Nil`. The documentation must state that lock-file cleanup is best effort and that stale lock handling belongs to higher-level code if it exists.

For `src/scherzo_signal_ffi.erl`, `install_sigterm/1` accepts a Gleam process subject, installs a SIGTERM handler, returns an opaque signal handle plus the OS process identifier as text, and sends exactly one stop message to the subject for the first handled SIGTERM. It mutates VM signal handling, so ownership is global to the running VM, not local to one module. `cleanup_sigterm/1` removes the custom handler, restores the default handler when this installation replaced it, is idempotent, and returns `Nil`. The `gen_event` callbacks are public only because Erlang requires them for the handler; docs should label them callback exports, not application API.

For `src/scherzo_config_ffi.erl`, `getenv/1` returns a non-empty environment value or not-found, `home/0` returns the home directory or not-found, `tmpdir/0` returns the OS temp directory, `dirname/1` returns the parent directory text, and `absname/1` returns an absolute normalized path. This module is low risk, but docs must warn tests not to assert host-specific absolute path prefixes.

For `src/scherzo_main_ffi.erl`, `args/0` returns command-line args as strings and `halt/1` terminates the VM with the supplied exit code. `halt/1` is intentionally effectful and should stay isolated at the CLI boundary. For `src/scherzo_lifecycle_ffi.erl`, `safe_shutdown/2` should preserve its current wrapper contract and catch shutdown callback failures so shutdown code cannot crash the caller unexpectedly. For `src/scherzo_time_ffi.erl`, `monotonic_ms/0` returns monotonic milliseconds for durations and timeouts, not wall-clock timestamps. For `src/scherzo_process_ext_ffi.erl`, `trap_exits/1` toggles process exit trapping and returns the previous setting or current VM result as the wrapper currently expects. For `src/scherzo_hash_ffi.erl`, `sha256_hex/1` returns a lowercase SHA-256 hex digest for the provided bytes. For `src/scherzo_terminal_ffi.erl`, `stdout_supports_color/0` returns a boolean and `terminal_columns/0` returns the detected terminal width or a safe fallback.

## Milestones

Milestone 1 establishes the contract surface before behavior changes. At the end, `docs/ffi.md` exists, all public FFI functions are listed, high-risk wrappers have named typed-error targets in the plan of work, and the current test suite still passes. This comes first because every later code change needs a stable contract to implement against.

Milestone 2 hardens the port boundary. At the end, `src/scherzo/port.gleam` no longer exposes raw FFI strings for public subprocess operations, `src/scherzo_port_ffi.erl` uses private per-process temp directories, diagnostics remain readable after cleanup, subprocess lifecycle tests cover the LIV-82 regression area, and temp directories are removed after normal exit and termination.

Milestone 3 hardens ledger, lock, and artifact persistence. At the end, ledger append, fold, and locking errors are typed; artifact writes use unique same-directory temp files; failed artifact writes clean temp files; concurrent atomic write tests prove the final file is one complete payload; and ledger locking tests prove serialized operations.

Milestone 4 hardens control sockets and redaction. At the end, control socket operations expose typed errors, timeout behavior is documented and tested, loopback-only behavior is tested, line-too-long behavior is tested, and redaction tests prove malformed raw JSON and redaction crashes fail closed.

Milestone 5 completes low-risk documentation, platform validation, and cleanup. At the end, every FFI export in the inventory has a contract in `docs/ffi.md`, stable modules are documented as stable, platform caveats are captured, and the full formatting and test commands pass on the supported platform set.

## Plan of Work

Start by creating `docs/ffi.md`. Give it sections for every module listed in the inventory. For each public function, write the input contract, output contract, documented error tags or typed errors, ownership and cleanup rules, and blocking behavior. Include a short warning that Erlang FFI functions are not type-checked by Gleam and that agents must update this document when they change an FFI module or its wrapper.

Next, add typed error definitions near the wrapper that owns each domain. In `src/scherzo/port.gleam`, define `PortError` and make public port functions return `Result(_, PortError)` or an existing domain error that wraps `PortError`. Keep private `ffi_*` functions returning the raw Erlang wire result if that is the least invasive way to call Erlang. Add a `port_error_to_string` helper only for logging and CLI display edges. Follow the same pattern for `ControlError` in the control wrapper modules, `ArtifactWriteError` in the artifact-store wrapper, `LedgerFfiError` in the ledger wrapper, `InstanceLockError` in the lock wrapper, and `SignalError` in the signal wrapper. If an equivalent domain error type already exists in a wrapper, extend that type rather than creating a parallel one.

Then update `src/scherzo_port_ffi.erl`. Replace separate shared temp files with a private temp directory per process. Store stderr and child PID files inside that directory. Extend the Erlang process handle tuple in a backward-compatible way so existing handles can still be understood during a rolling code path inside tests. On `await_exit/2` and `terminate/1`, read diagnostics into the process dictionary keyed by the port or another stable handle key before removing the temp directory. Make cleanup idempotent. Preserve the LIV-82 termination behavior unless a new test fails.

Update the port wrapper in `src/scherzo/port.gleam` to map all documented raw error tags to `PortError`. Replace call sites that pattern-match or display raw strings. At call sites that need a human message, convert through `port_error_to_string`. Do not leak raw unexpected FFI strings except inside the `UnexpectedFfiFailure` variant.

Update persistence boundaries. In `src/scherzo_artifact_store_ffi.erl`, replace deterministic temp paths with unique temp paths in the same directory as the final artifact, delete temp files on any pre-rename error, and report the failed phase. In `src/scherzo/state/ledger.gleam` and `src/scherzo_state_ffi.erl`, map file open, write, sync, close, read, and callback exceptions into typed ledger errors while preserving the append and fold behavior. In `src/scherzo/instance_lock.gleam` and `src/scherzo_lock_ffi.erl`, map `exists` to a typed already-held error and preserve best-effort release cleanup.

Update control socket wrappers and `src/scherzo_control_ffi.erl`. Decide in code that `send_line/3` honors its timeout by setting socket send options for the call or by documenting that the timeout is fixed at socket creation and removing public reliance on the argument. The preferred behavior is to honor the provided timeout because the argument already exists in the FFI export. Map loopback rejection, timeout, closed, line-too-long, permission, and environment not-found cases to typed `ControlError` variants.

Update redaction tests and documentation. Keep `redact_raw_json_fail_closed/4` as the public safe path. If direct `redact_raw_json/3` is still used, document it as lower-level and ensure malformed JSON still returns the safe malformed placeholder. Add tests that fail if raw secrets appear in the output after malformed input, sensitive keys, secret strings, truncation, or an injected redaction failure.

Finally, re-read and document stable modules: `src/scherzo_main_ffi.erl`, `src/scherzo_lifecycle_ffi.erl`, `src/scherzo_time_ffi.erl`, `src/scherzo_process_ext_ffi.erl`, `src/scherzo_hash_ffi.erl`, `src/scherzo_terminal_ffi.erl`, and `src/scherzo_config_ffi.erl`. Do not change their behavior unless documentation reveals a concrete mismatch with callers or tests.

## Concrete Steps

1. From the repository root, run `jj status --color=never` and confirm the working copy only contains intentional changes for this implementation.
2. Re-read every `src/*ffi.erl` module listed in this plan and the matching Gleam wrappers before editing. If exports changed since this plan was written, update `docs/ffi.md` and the Progress section before proceeding.
3. Create `docs/ffi.md` with one section per FFI module and one bullet or paragraph per exported function. Include inputs, outputs, error tags or typed errors, ownership, cleanup, and blocking behavior.
4. If `docs/ARCHITECTURE.md` exists, add a short link to `docs/ffi.md`. If it does not exist, skip this step and record that in Surprises & Discoveries.
5. Run `direnv exec . gleam format --check src test` and `direnv exec . gleam test`. Expect formatting to pass and all existing tests to pass. Commit the documentation-only baseline after it is green.
6. In `src/scherzo/port.gleam`, add the `PortError` type and helper functions that map documented raw FFI error tags to `PortError` values and render `PortError` for display.
7. Add port tests in an existing port test file if one exists; otherwise create `test/scherzo/port_test.gleam`. First add tests for `cwd_not_directory`, read timeout, process exited status, closed port handling, and line-too-long behavior. Run `direnv exec . gleam test` and confirm these tests fail or cannot compile until the wrapper returns typed errors.
8. In `src/scherzo_port_ffi.erl`, add private per-process temp directory creation, put stderr and child PID files in that directory, cache diagnostics before cleanup, and remove the temp directory from `await_exit/2` and `terminate/1`.
9. In `src/scherzo/port.gleam`, update public functions and call sites to return or handle `PortError`. Use display conversion only at user-facing boundaries.
10. Add port lifecycle integration tests in `test/scherzo/port_test.gleam`: normal exit cleans temp storage, terminate cleans temp storage, diagnostics are still available after cleanup, timeout leaves the process terminable, a child process spawned by the command is gone after termination, and malformed executable, args, env, or cwd return typed errors.
11. Run `direnv exec . gleam format --check src test` and `direnv exec . gleam test`. Expect all port tests and existing tests to pass. Commit the port hardening slice.
12. Add artifact atomic-write tests in an existing artifact-store test file if one exists; otherwise create `test/scherzo/state/artifact_store_test.gleam`. Cover successful write, failed parent path, cleanup of unique temp files after failure, and two concurrent writers producing a final file that is exactly one complete payload.
13. Update `src/scherzo_artifact_store_ffi.erl` and its Gleam wrapper so write errors are typed by phase, temp files are unique in the same directory as the final artifact, and failure cleanup is best effort but tested on the normal failure paths.
14. Add ledger and lock tests in an existing ledger or lock test file if one exists; otherwise create `test/scherzo/state/ledger_ffi_test.gleam` and `test/scherzo/instance_lock_test.gleam`. Cover append with fsync false and true, fold line numbers and is-last flags, missing-file read error, callback exception mapping, lock serialization, existing lock error, and release idempotence.
15. Update `src/scherzo_state_ffi.erl`, `src/scherzo/state/ledger.gleam`, `src/scherzo_lock_ffi.erl`, and `src/scherzo/instance_lock.gleam` to return typed errors while preserving current behavior.
16. Run formatting and tests again. Commit the persistence hardening slice when green.
17. Add control tests in existing control test files if they exist; otherwise create `test/scherzo/control_ffi_test.gleam`. Cover loopback listen on port zero, non-loopback rejection, connect timeout to an unused loopback port, send and receive one line, receive timeout, receive closed, line-too-long, bound port, private chmod success on a temp fixture, token generation, and environment not found.
18. Update `src/scherzo_control_ffi.erl` and control Gleam wrappers to expose typed `ControlError` values and to honor the documented timeout behavior for `send_line/3`.
19. Add redaction tests in an existing redaction test file if one exists; otherwise create `test/scherzo/session/redaction_test.gleam`. Cover malformed raw JSON, sensitive key redaction, secret string redaction, truncation at a valid UTF-8 boundary, invalid maximum byte handling, and fail-closed fallback behavior.
20. Update `src/scherzo_redaction_ffi.erl` and `src/scherzo/session/redaction.gleam` only as needed to satisfy the fail-closed contract and tests. Do not expose raw redaction errors to callers on the fail-closed path.
21. Run formatting and tests again. Commit the control and redaction hardening slice when green.
22. Re-read stable low-risk FFI modules, finish their sections in `docs/ffi.md`, and add any small wrapper tests that are missing for simple deterministic behavior such as hashing, monotonic time increasing across a sleep, terminal fallback, and config not-found behavior.
23. Run `direnv exec . gleam format --check src test` and `direnv exec . gleam test` on the primary development platform.
24. Run the same validation on the other supported platform, macOS or Linux, before final acceptance. If only one platform is available locally, record the missing platform as a validation gap in Outcomes & Retrospective and leave CI or reviewer instructions in the PR.
25. Final commit after the full suite is green and `docs/ffi.md` matches the implemented contracts.

The suggested commit map is: commit 1 for `docs/ffi.md` and contract inventory; commit 2 for port typed errors, temp directories, cleanup, and tests; commit 3 for artifact, ledger, and lock typed errors and tests; commit 4 for control socket and redaction typed errors and tests; commit 5 for stable FFI documentation, platform notes, and final validation fixes.

## Testing and Falsifiability

Port tests should prove the highest-risk claims. In `test/scherzo/port_test.gleam`, start a command that prints one line and exits; assert that reading returns the exact line without a newline and that `await_exit` returns status zero. Start a command that exits with a nonzero status without output; assert that `read_stdout_line` returns the typed process-exited error carrying that status. Start a command that produces no output for longer than a short timeout; assert that `read_stdout_line` returns typed timeout and that `terminate` succeeds afterward. Start a command that writes a line larger than the documented maximum; assert typed line-too-long. Start a command that writes to stderr and exits; call `await_exit`, then `read_diagnostics`, and assert the stderr text is still available even though the temp directory was cleaned.

Port cleanup tests should expose the temp directory path only through a test helper or controlled inspection function, not through production public API. The test should assert that the private temp directory exists while the process is running and does not exist after `await_exit` or `terminate`. For the LIV-82 regression area, run a command that starts a child process that would otherwise outlive the shell wrapper, call `terminate`, and poll until both the tracked child and its process group are gone. The assertion should be eventual disappearance within a deadline, not an exact process-tree shape.

Malformed launch tests should call the public Gleam wrapper with a missing working directory, an empty executable for argv launch, an invalid environment entry if the wrapper can represent one, and an executable name that cannot be spawned. Each case should assert a specific `PortError` variant. If Gleam's type system prevents constructing malformed args or env through the public wrapper, add a narrow Erlang FFI unit test or a private test-only helper that calls the raw FFI with malformed terms, and document why this is testing the boundary rather than normal Gleam usage.

Artifact tests should use a temporary fixture directory created by the test and should never assert host-specific absolute path prefixes. A successful `write_atomic` test should assert the final file contains the exact bytes and no temp files remain. A failed-write test should use a final path whose parent does not exist or cannot be used and assert a typed phase error and no leftover temp file in the fixture directory. A concurrency test should spawn two writers with different payloads to the same final path and assert the final file equals one full payload or the other, never a mixture or partial file.

Ledger tests should use a temporary ledger file. Append tests should call `append_lines` with fsync false and true, then read the file and assert exact contents. Fold tests should write three lines and assert the step callback receives line numbers one, two, and three, with is-last false, false, and true. Error tests should fold a missing file and assert the typed open error. Callback tests should make the step callback fail or panic and assert the wrapper maps that to a typed callback failure while closing the file. Lock tests should start two operations using the same ledger key; the first operation should block on a subject or message until the second is waiting, then release. Assert the second operation does not enter until the first exits.

Control socket tests should listen on loopback with port zero, retrieve the bound port, connect a client, send a line, receive the same line without newline, and close both socket and listener idempotently. Negative tests should assert non-loopback host rejection, receive timeout, closed socket receive, and line-too-long. A send timeout test should be included only after the implementation defines how `send_line/3` honors the timeout; it should use a small timeout and a socket state that cannot accept data without relying on platform-specific buffer sizes if possible.

Redaction tests should call the public fail-closed wrapper. For malformed raw JSON containing a fake secret value, assert the output is the malformed placeholder and does not contain the secret. For valid JSON with keys containing `token`, `api_key`, `authorization`, and `secret`, assert the values are replaced with `[REDACTED]`. For valid JSON containing a secret string in a non-sensitive field, assert only the secret substring is replaced. For truncation, use a multi-byte character near the boundary and assert the output is valid UTF-8 and the truncated flag is true. For a simulated internal failure, pass inputs that previously caused a crash or add a test-only failure hook, then assert the failure placeholder is returned and raw input is absent.

Stable-module tests should be minimal. Hashing should assert a known SHA-256 digest for a small string. Time should assert `monotonic_ms` does not go backward across a short sleep. Config should assert a definitely missing environment variable returns not-found and `dirname` behaves consistently for repository-relative fixture paths. Terminal tests should avoid requiring a real terminal; they should assert only type and safe fallback behavior.

The plan is falsified if any public high-risk wrapper still returns raw `String` for FFI-originated errors after its slice, if normal port exit or termination leaves private temp directories behind, if diagnostics disappear after cleanup, if artifact concurrent writes can produce partial content, if ledger lock tests can overlap critical sections, or if redaction can return raw malformed input on failure.

## Validation and Acceptance

From the repository root, run:

    direnv exec . gleam format --check src test

Expected result: the command exits successfully without formatting diffs. Then run:

    direnv exec . gleam test

Expected result: all tests pass, including the new port, artifact, ledger, lock, control, redaction, and stable FFI tests. The exact pass count may change as tests are added; the important acceptance signal is a zero exit status and no failed tests.

Acceptance requires these observable behaviors:

- `docs/ffi.md` inventories every module and export listed in this plan and gives inputs, outputs, errors, ownership, cleanup, and blocking behavior.
- Public high-risk Gleam wrappers no longer expose free-form FFI strings as their primary error type.
- `scherzo_port_ffi` subprocesses keep stderr and child PID files in private per-process temp directories.
- A normally exited or terminated subprocess removes its private temp directory, while diagnostics remain readable through the wrapper.
- Timeout, line-too-long, nonzero exit status, malformed launch input, and closed subprocess states produce typed errors.
- Existing LIV-82 process-tree cleanup behavior has regression coverage and remains green.
- Artifact writes use unique same-directory temp files, clean normal failure leftovers, and never produce partial final files in tests.
- Ledger append, fold, and lock tests prove file closure, typed errors, and serialized operations.
- Control socket tests prove loopback-only binding, timeout, close, line-too-long, and token or permission behavior.
- Redaction tests prove malformed input and internal failure paths return safe placeholders and do not leak secrets.
- The full validation commands pass on both macOS and Linux, or any missing platform is explicitly recorded as a PR validation gap.

## Rollout, Recovery, and Idempotence

This is an internal hardening change. Roll it out in small commits as described in the commit map. Each commit must leave the repository formatted and with tests passing. If a slice fails late, revert that slice without reverting earlier documentation and test scaffolding that is still accurate.

The typed error migration should be additive within each wrapper before call sites are changed. Add the typed error and conversion helpers first, then update functions and callers. If a broad call-site update becomes too large, keep a temporary compatibility function that returns the old display string and mark it for removal in the same milestone. Do not let compatibility helpers become the main API.

Port temp cleanup must be idempotent. Calling `terminate` after `await_exit`, calling `read_diagnostics` after cleanup, or closing an already-closed control socket should not crash. Artifact temp cleanup must also be idempotent: deleting a missing temp file during error handling should not turn the original write error into a misleading cleanup error.

No persistent data migration is required. Ledgers and artifacts remain in their current format. The artifact write implementation changes how temp files are named before rename, but final artifact paths and contents remain unchanged. If implementation stops halfway through a persistence slice, the final paths are still ordinary files and the safe recovery is to delete only test-created temp fixture directories, then rerun the tests.

## Artifacts and Notes

The initial source-control fact used while drafting was:

    jj status --color=never
    The working copy has no changes.

Important drafting observations were that `src/scherzo_port_ffi.erl` already contains process-tree termination code from the LIV-82 area, `src/scherzo_artifact_store_ffi.erl` currently uses a deterministic temp suffix, and `src/scherzo_control_ffi.erl` currently ignores the timeout argument to `send_line/3`.

When adding tests that need temporary directories, use test-created directories from the project's existing test helpers if they exist. If no helper exists, add one in the relevant test module that asks the runtime for a temp directory and creates a unique child directory. Do not assert literal host-specific path prefixes in tests or docs.

## Interfaces and Dependencies

No new third-party dependency is required unless implementation discovers that the existing test helpers cannot create isolated temp directories or spawn short-lived processes. Prefer standard Gleam, Erlang, and existing repository helpers.

Use these typed-error shapes as the target, adjusting names only if an equivalent domain type already exists in the wrapper module:

    pub type PortError {
      CwdNotDirectory
      InvalidCommand(reason: String)
      InvalidExecutable(reason: String)
      InvalidArgument(reason: String)
      InvalidEnvironment(reason: String)
      SpawnFailed(reason: String)
      SendFailed(reason: String)
      ReadTimeout
      LineTooLong(max_bytes: Int)
      ProcessExited(status: Int)
      Closed
      DiagnosticsFailed(reason: String)
      CleanupFailed(reason: String)
      UnexpectedFfiFailure(function: String, detail: String)
    }

    pub type ArtifactWriteError {
      InvalidPath(reason: String)
      OpenTempFailed(reason: String)
      WriteTempFailed(reason: String)
      SyncTempFailed(reason: String)
      CloseTempFailed(reason: String)
      RenameFailed(reason: String)
      SyncParentFailed(reason: String)
      CleanupTempFailed(reason: String)
      UnexpectedFfiFailure(function: String, detail: String)
    }

    pub type LedgerFfiError {
      OpenFailed(reason: String)
      WriteFailed(reason: String)
      SyncFailed(reason: String)
      CloseFailed(reason: String)
      ReadFailed(reason: String)
      StepFailed(reason: String)
      LockFailed(reason: String)
      UnexpectedFfiFailure(function: String, detail: String)
    }

    pub type ControlError {
      NonLoopbackHostRejected(host: String)
      AddressInUse
      PermissionDenied(reason: String)
      Timeout
      Closed
      LineTooLong(max_bytes: Int)
      SendFailed(reason: String)
      ReceiveFailed(reason: String)
      ListenFailed(reason: String)
      ConnectFailed(reason: String)
      AcceptFailed(reason: String)
      TokenGenerationFailed(reason: String)
      EnvNotFound(name: String)
      UnexpectedFfiFailure(function: String, detail: String)
    }

    pub type InstanceLockError {
      AlreadyHeld
      OpenFailed(reason: String)
      WriteFailed(reason: String)
      UnexpectedFfiFailure(function: String, detail: String)
    }

    pub type SignalError {
      SignalServerUnavailable(reason: String)
      InstallFailed(reason: String)
      HandlerVerificationFailed(reason: String)
      UnexpectedFfiFailure(function: String, detail: String)
    }

Keep raw `ffi_*` declarations private. Public functions in wrapper modules should return typed errors or existing domain errors that contain these typed FFI errors. Display conversion functions should be pure and explicit, for example `port_error_to_string(error: PortError) -> String`, so user-facing strings are still available without making strings the error contract.

## Platform Caveats

The required supported behavior is the same on macOS and Linux, but tests must not assume identical process-table shape, signal timing, socket buffer size, or temp-directory path prefixes. Process cleanup tests should use eventual polling with deadlines. Control socket tests should bind only loopback and port zero where possible. File permission tests should assert the strongest portable behavior the repository already expects; if one platform reports permission metadata differently, document the difference in `docs/ffi.md` and assert the platform-specific branch explicitly.

Avoid tests that require a specific shell implementation beyond commands already required by Scherzo's existing subprocess code. If a test needs a helper command such as a sleeping child process or long-line emitter, prefer a small repository test fixture or a command available in the existing development environment. If a helper command is unavailable on one platform, make that a test fixture problem to fix, not a reason to weaken the FFI contract.

## Open Questions and Clarifications Needed

- [CLARIFY] Confirm whether any public API outside this repository depends on high-risk wrappers returning `Result(_, String)`. If external callers exist, keep compatibility functions for one release or one PR while moving internal code to typed errors.
- [CLARIFY] Confirm the required validation matrix for this repository: macOS only, Linux only, or both. This plan assumes both macOS and Linux behavior matter because the FFI uses subprocesses, signals, sockets, and filesystem semantics that can differ by platform.
- [CLARIFY] Confirm whether direct use of `redact_raw_json/3` should remain supported or whether all Gleam callers should use only `redact_raw_json_fail_closed/4`. The safer default is to keep the fail-closed wrapper as the public application API and document direct redaction as a lower-level FFI export.
