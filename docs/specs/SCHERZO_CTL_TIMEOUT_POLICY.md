# Scherzo Control Timeout Policy

Status: Draft v1

Purpose: Define the operator-facing timeout contract for `scherzoctl`, daemon-backed Scherzo control commands, and related top-level Scherzo CLI flows that start, inspect, or wait on control operations.

## Normative language

The key words `MUST`, `MUST NOT`, `REQUIRED`, `SHOULD`, `SHOULD NOT`, `RECOMMENDED`, `MAY`, and `OPTIONAL` in this document are to be interpreted as described in RFC 2119.

## 1. Core principle

Every timeout reported by a Scherzo control command MUST answer three operator questions:

1. Was the command accepted?
2. Which phase timed out?
3. What should the operator do next?

A Scherzo control timeout MUST NOT be an opaque `timed out` or generic transport failure when Scherzo can identify a more precise phase. Timeout responses SHOULD distinguish a command that was not accepted, a command that was accepted and is still running, and a command whose acceptance is unknown.

## 2. Scope

This policy applies to:

- `scripts/scherzoctl` and packaged `scherzoctl` equivalents,
- daemon-backed control commands invoked through top-level `scherzo ctl ...`,
- read commands such as `ps`, `session`, `events`, and `operation-status`,
- mutating operator commands such as retry, retry-step, recollect, publication retry, pause, resume, abort, prompt, UI response, cleanup, and future daemon-backed controls, and
- optional `--wait` behavior after an operation has been accepted.

This policy does not require every external workflow command to use the `scherzoctl` envelope. However, workflow command-step timeouts, external API timeouts, and launcher/bootstrap timeouts SHOULD be translated into the same phase vocabulary when they are reported through Scherzo operator surfaces.

## 3. Vocabulary

**Accepted** means the daemon has received the request and has either completed the read request, applied the mutation, rejected the mutation with a definitive protocol response, or durably recorded/queued an operation that can be rediscovered later.

**Not accepted** means the request did not reach the daemon or the daemon did not begin processing it. Examples include control-file discovery failure, connection failure before request send, and client-side validation failure.

**Unknown acceptance** means the client cannot prove whether the daemon received the request. Examples include a timeout after writing a mutating request but before receiving a response.

**Operation** means a long-running or asynchronous unit of control work identified by an `operation_id`, such as a retry-step repair, output recollection, publication retry, cleanup batch, or future run finalization.

**Read command** means a command whose primary effect is inspection and which SHOULD be safe to repeat, such as `ps`, `session`, `events`, or `operation-status`.

**Mutating command** means a command that can change daemon state, retained workflow state, tracker state, workspaces, artifacts, or queued operations.

## 4. Timeout phases

Every structured timeout MUST include one stable `phase` value from this section. New phases MAY be added, but existing phase strings MUST NOT be renamed without a compatibility plan.

| Phase | Meaning |
| --- | --- |
| `cli_bootstrap` | The command timed out before the Scherzo control command was running, such as wrapper startup, environment activation, package evaluation, or executable launch. |
| `control_file_discovery` | The command timed out while locating, reading, or validating the control file used to contact the daemon. |
| `daemon_connect` | The command timed out while opening the daemon control connection. |
| `request_round_trip` | The command timed out after starting a bounded client request and before receiving a complete daemon response. |
| `daemon_actor_query` | The daemon timed out while asking an internal actor or projection for read data. |
| `operation_admission` | The daemon timed out while deciding whether to accept, reject, apply, or queue a mutating operation. |
| `operation_wait` | The operator explicitly asked to wait for an already accepted operation, and that wait budget expired before terminal completion. |
| `command_step_watchdog` | A workflow command step exceeded its configured execution budget. |
| `external_api` | A required external service, such as Linear or GitHub, exceeded its operation budget. |

When multiple phases could describe a timeout, Scherzo SHOULD report the most specific phase that is known. For example, a read-model snapshot timeout inside the daemon SHOULD be `daemon_actor_query`, not `request_round_trip`, if the daemon can return a structured response.

## 5. JSON timeout shape

In `--json` mode, a timeout that prevents acceptance MUST use the normal JSON envelope for the command and MUST include an error object with at least these fields:

    {
      "ok": false,
      "error": {
        "code": "timeout",
        "phase": "daemon_connect",
        "timeout_ms": 5000,
        "accepted": false,
        "retryable": true,
        "message": "Timed out connecting to the Scherzo daemon.",
        "suggested_next_command": "scripts/scherzoctl ping --json --timeout 10s"
      }
    }

`error.code` MUST be `timeout` for timeout failures. `error.phase` MUST be one of the phase strings in this policy. `error.timeout_ms` MUST be the effective timeout budget in milliseconds when known. `error.accepted` MUST be one of `true`, `false`, or `unknown`. `error.retryable` MUST describe whether the exact command can be retried safely by an operator without additional inspection. `error.message` MUST be suitable for human display and MUST NOT contain secrets. `error.suggested_next_command` SHOULD be present when Scherzo can recommend a safe next inspection or recovery command.

If acceptance is unknown, the response MUST say so explicitly:

    {
      "ok": false,
      "error": {
        "code": "timeout",
        "phase": "request_round_trip",
        "timeout_ms": 5000,
        "accepted": "unknown",
        "retryable": false,
        "message": "Timed out waiting for the daemon response after sending the request.",
        "suggested_next_command": "scripts/scherzoctl operation-status <operation-id-or-request-id> --json"
      }
    }

If no operation id or request id exists for rediscovery, the suggested next command SHOULD be a safe read command such as `scripts/scherzoctl ps --json` or `scripts/scherzoctl events <session-id> --json` rather than a blind retry of a mutating command.

## 6. Admission versus completion

Mutating commands SHOULD separate admission from completion. By default, a long-running mutating command SHOULD return promptly after the daemon accepts or rejects the request. If the daemon accepts asynchronous work, the response MUST include an `operation_id` and an operation status such as `queued`, `running`, `applied`, `rejected`, `not_found`, or `not_allowed`.

A successful admission response SHOULD look like this shape:

    {
      "ok": true,
      "data": {
        "command": "retry-step",
        "status": "queued",
        "accepted": true,
        "operation_id": "retry-step:LIV-123-1700000000000-1:final_validate:1700000001000",
        "message": "Retry-step operation queued."
      }
    }

Long waits for accepted work MUST be opt-in. A command MAY support `--wait`, but `--wait` MUST use the `operation_wait` phase when its wait budget expires. A wait timeout for an accepted operation MUST NOT be reported as command failure if Scherzo can confirm the operation is still queued or running. It SHOULD return `ok: true` with explicit wait metadata:

    {
      "ok": true,
      "data": {
        "operation_id": "retry-step:LIV-123-1700000000000-1:final_validate:1700000001000",
        "status": "running",
        "accepted": true,
        "wait": {
          "timed_out": true,
          "phase": "operation_wait",
          "timeout_ms": 120000
        },
        "suggested_next_command": "scripts/scherzoctl operation-status retry-step:LIV-123-1700000000000-1:final_validate:1700000001000 --json --wait --timeout 2m"
      }
    }

Commands that are intentionally synchronous and bounded MAY complete without creating an operation id. If such a command can run longer than the default request budget, it SHOULD either become admission-first or require explicit `--wait` semantics.

## 7. Retry and idempotency policy

The CLI MAY retry automatically before a request reaches the daemon. This includes bounded retry/backoff for `daemon_connect` during daemon startup.

The CLI MUST NOT blindly retry a mutating command after the request may have reached the daemon unless the command is idempotent by construction or the request includes a stable client request id that the daemon can deduplicate. For unknown acceptance, Scherzo SHOULD guide the operator to rediscover state with `operation-status`, `ps`, `session`, `events`, or another read command.

Mutating commands that queue operations SHOULD use stable operation identifiers or client request identifiers so an operator can safely rediscover whether the command was accepted after a timeout, client crash, or terminal disconnect.

## 8. Read-query policy

Read commands SHOULD prefer bounded, stale-marked results over opaque timeout failure. If fresh data is unavailable but Scherzo has usable cached or projected data, the command SHOULD return `ok: true` and mark the data as stale:

    {
      "ok": true,
      "data": {
        "fresh": false,
        "stale_reason": "daemon_actor_query_timeout",
        "age_ms": 4200
      }
    }

A read command SHOULD return `ok: false` for timeout only when no usable data can be returned. If a read command returns stale data, it MUST make the stale state visible in JSON and SHOULD make it visible in human output.

Read commands MUST remain safe to repeat. Retrying a read command after a timeout SHOULD NOT mutate daemon, tracker, workspace, or artifact state.

## 9. CLI timeout controls

Scherzo control commands SHOULD expose a consistent timeout interface:

    scripts/scherzoctl ps --json --timeout 5s
    scripts/scherzoctl operation-status <operation-id> --json --wait --timeout 2m

`--timeout` SHOULD set the primary command budget. For non-waiting commands this is the bounded control request budget. For `--wait` commands this is the operation wait budget unless a command provides a more specific wait-timeout option.

The CLI SHOULD honor these environment defaults when the command line does not specify a timeout:

    SCHERZO_CTL_TIMEOUT=5s
    SCHERZO_CTL_WAIT_TIMEOUT=2m

Timeout durations SHOULD accept at least milliseconds, seconds, and minutes, for example `500ms`, `5s`, and `2m`. Invalid duration strings MUST fail before contacting the daemon and SHOULD be reported as client validation errors, not timeout errors.

## 10. JSON output rule

In `--json` mode, stdout MUST contain exactly one JSON document for the command result. Logs, environment activation messages, progress output, warnings, and diagnostics MUST go to stderr.

This requirement is part of the timeout policy because malformed stdout prevents operators and agents from reliably distinguishing transport timeouts, daemon responses, stale read results, and accepted operations. A command that cannot preserve JSON-only stdout SHOULD avoid advertising `--json` until it can.

JSON responses MUST NOT expose daemon tokens, API keys, or other secrets. Suggested commands MUST NOT include secret-bearing control-file contents or token values.

## 11. Human output rule

Human-readable output SHOULD use the same semantics as JSON output. When a timeout occurs, human output SHOULD name the phase, acceptance state, retryability, and next safe command. For example:

    Timed out connecting to the Scherzo daemon.
    Phase: daemon_connect
    Accepted: false
    Retryable: yes
    Next: scripts/scherzoctl ping --json --timeout 10s

Human output MUST NOT contradict the JSON envelope for the same command.

## 12. Observability expectations

Scherzo SHOULD record enough telemetry to diagnose recurring timeout phases. Implementations SHOULD prefer counters and timings such as:

- timeout count by `phase`, command, and retryability,
- control-file discovery duration,
- daemon-connect duration and retry count,
- request round-trip duration,
- daemon actor query duration,
- operation admission duration,
- operation wait duration, and
- command-step watchdog expirations.

Telemetry MUST NOT include secrets. Operator-facing diagnostics SHOULD redact control tokens, API keys, issue bodies, prompts, and other sensitive content.

## 13. Compatibility and rollout

This policy is intended to be implemented incrementally. Existing commands MAY temporarily return legacy timeout output while implementation work is in progress, but new or changed timeout behavior SHOULD move toward this policy.

Compatibility-sensitive changes include:

- changing stdout/stderr behavior for `--json`,
- adding `--timeout` parsing to existing commands,
- changing mutating commands from synchronous completion to admission-first responses,
- changing read commands to return stale-marked successful data instead of failure, and
- adding operation ids or client request ids to existing command responses.

Implementation plans SHOULD state which commands are brought into compliance in each milestone and SHOULD add tests that prevent regressions to opaque timeout failures.
