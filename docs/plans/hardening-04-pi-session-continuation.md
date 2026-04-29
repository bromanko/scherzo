# Hardening 04: Resume interrupted work from persisted pi sessions

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

After this change, Scherzo can continue an interrupted worker from the prior pi conversation history instead of always starting a fresh pi session. When a daemon restarts after a worker was interrupted, Scherzo marks the old Scherzo run as interrupted, validates that the workspace still exists and is safe, launches a new pi RPC process using the saved pi `sessionFile`, and sends an explicit recovery prompt telling pi to inspect the current workspace and continue without repeating completed work. The visible proof is a deterministic fake-pi recovery test where the first run records a `sessionFile`, the daemon restarts, and the recovery worker launches pi with that session file and receives a recovery prompt.

This phase resumes persisted pi conversation history. It does not resume the exact live in-flight model stream, tool call, Erlang port, worker process, queued operator prompt, pending UI request, or EventHub stream that existed before the crash. Those runtime objects are still lost when the BEAM process exits.

## Problem Framing and Constraints

The single-instance crash recovery plan can detect that a worker was running when Scherzo exited and can schedule a retry or park the issue. Without pi session continuation, that retry starts with a fresh pi context. The workspace may contain partial changes, and Linear may contain claim comments, but pi loses its prior conversation, decisions, tool outputs, and any context compaction it performed. That makes recovery less efficient and can cause repeated investigation or duplicate edits.

Pi already supports persisted sessions. Its README documents `pi --session <path|id>` and `pi --session-dir <path>`, and RPC `get_state` returns `sessionFile` and `sessionId`. Current Scherzo defaults to `pi --mode rpc --no-session`, so it deliberately disables this persistence. Current `pi_rpc.Session` stores `session_id` but not `session_file`, and `runner.PiUpdate`/EventHub expose pi session id but not the file path needed to resume deterministically.

The right-size change is to make pi session persistence an explicit Scherzo option and to store pi session file paths in the durable ledger. Recovery then starts a new worker that resumes the old pi session file and sends a recovery prompt. This is not automatic live reattachment; it is a new pi process using persisted conversation history.

## Strategy Overview

Add pi session persistence configuration. Keep the current behavior as the default initially: `session_persistence: disabled`, equivalent to using `--no-session`. Add `session_persistence: per_issue` to instruct Scherzo to use a Scherzo-owned session directory under `workspace.root/.scherzo-state/pi-sessions/` and to persist/resume pi sessions for each issue.

Change pi launch construction so Scherzo can build the final command from config plus runtime session options. Today `pi.command` is a shell string such as `pi --mode rpc --no-session`. This phase should avoid brittle string surgery by introducing either a command template or a launch options helper. The simplest compatible approach is to keep `pi.command` as the base command but add explicit validation: when `session_persistence: per_issue` is enabled, `pi.command` must not contain `--no-session`, and Scherzo appends `--session-dir <dir>` for new sessions or `--session <session-file>` for resumed sessions. If appending flags to an arbitrary shell string is too unsafe, revise the plan during implementation to add `pi.executable` and `pi.args` fields while preserving backward compatibility.

Extend `src/scherzo/agent/pi_rpc.gleam` to decode `sessionFile` from `get_state` and `get_session_stats`. Extend `pi_rpc.Session`, `runner.PiUpdate`, EventHub session summaries or events, and `runner.WorkerSuccess`/failure metadata as needed so the daemon can persist `session_id` and `session_file` for a run.

Extend the durable ledger with `PiSessionAttached(run_id, issue_id, session_id, session_file)` and `RunResumedFrom(new_run_id, previous_run_id, session_file)` records. During normal worker startup, after `get_state` succeeds, append `PiSessionAttached`. During crash recovery, if an interrupted run has a valid session file and the workspace still passes safety checks, dispatch a recovery run with `resume_session_file: Some(path)` and append `RunResumedFrom`.

The recovery run sends a distinct recovery prompt, not the original full task prompt. The prompt should say that Scherzo restarted, identify the Linear issue and previous run id, instruct pi to inspect the workspace state, avoid repeating completed work, and continue toward the task goal. The prompt should be configurable later, but this phase can use a fixed built-in template with issue fields.

## Alternatives Considered

One alternative is to keep fresh-session recovery only. That is simpler but wastes model/context and loses useful prior reasoning. It also makes interrupted long-running tasks harder to continue safely.

Another alternative is to rely on `pi -c` or most-recent session continuation. That is unsafe in Scherzo because multiple issues and workspaces can exist. Recovery must use the exact `sessionFile` recorded for the interrupted run, not a global recent session.

A third alternative is to store sessions in pi's default global `~/.pi/agent/sessions/` location and resume by `sessionId`. That makes cleanup and workspace-root portability harder. Scherzo should prefer a session directory under its own `.scherzo-state` so the durable ledger and session files belong to the same workspace root.

A fourth alternative is to fork rather than resume the prior session. Forking might be useful if the prior session ended in a bad branch, but for crash continuation the most direct behavior is to resume the session file. A future operator command can add fork-from-run if needed.

A fifth alternative is to resume the exact in-flight turn by reattaching to a still-running pi process. That is not feasible after BEAM process death in the current architecture. Erlang ports and worker command subjects are gone, and the JSONL stream cannot be recovered.

## Risks and Countermeasures

The main correctness risk is resuming a pi session whose workspace no longer matches the conversation. Countermeasure: before resuming, verify the recorded workspace path still exists, remains inside `workspace.root`, and passes the normal `before_run` hook. If verification fails, fall back according to config: either start fresh with a warning or park the issue for manual inspection.

The main safety risk is appending session flags to an arbitrary `pi.command` shell string incorrectly. Countermeasure: validate the base command when persistence is enabled and add tests for command construction. If robust escaping is not possible with the existing shell-string model, introduce structured `pi.command_args` or `pi.executable`/`pi.args` fields in this plan and keep the old `pi.command` path for disabled persistence.

The main duplication risk is sending the original task prompt again into a resumed pi session. Countermeasure: resumed runs use a built-in recovery prompt and must not resend the initial workflow prompt. Tests must inspect fake-pi transcript and assert the recovery prompt is present and the original full prompt is absent.

The main privacy risk is storing absolute session file paths or workspace details in Linear comments. Countermeasure: session file paths are stored only in the local ledger and control/EventHub metadata as needed. Handoff comments do not include session file paths.

The main operational risk is missing or corrupt session files. Countermeasure: add `pi.resume_missing_session_policy`, defaulting to `fresh_with_warning` or `park`. For the first safe version, prefer `park` if the session file was expected but missing, because a human can inspect the workspace before spending tokens on a fresh duplicate attempt. The implementation should pick one default and document it.

The main version compatibility risk is pi changing session file format or CLI flags. Countermeasure: use pi's documented `--session` and `--session-dir` options, extend fake-pi tests, and include an optional real-pi probe that starts a session, captures `sessionFile`, restarts pi with `--session <file>`, and confirms `get_state.sessionFile` matches.

## Progress

- [x] (2026-04-29 04:35Z) Drafted this plan after confirming pi documents persistent session files, `--session`, `--session-dir`, RPC `get_state.sessionFile`, and RPC `switch_session`.
- [ ] Add pi session persistence config and command-construction tests.
- [ ] Decode and propagate pi `sessionFile` through RPC, runner updates, EventHub, and durable ledger records.
- [ ] Persist pi session attachment records for runs.
- [ ] Resume interrupted recovery runs from recorded session files.
- [ ] Add fake-pi and optional real-pi validation for session continuation.
- [ ] Update README and hardening recovery docs.

## Surprises & Discoveries

- Observation: Pi RPC documentation lists `--no-session` and `--session-dir`, and pi README documents `--session <path|id>`.
  Evidence: `/nix/store/.../pi-coding-agent/docs/rpc.md` lists common RPC options including `--no-session` and `--session-dir`; the main pi README says `pi --session <path|id>` uses a specific session file or ID.

- Observation: Pi RPC `get_state` includes both `sessionId` and `sessionFile`.
  Evidence: `docs/rpc.md` shows `get_state` response data with `sessionFile` and `sessionId` fields.

- Observation: Current Scherzo stores only pi session id in the RPC session type.
  Evidence: `src/scherzo/agent/pi_rpc.gleam` defines `Session(..., session_id: Option(String), next_id: Int)` and `Data(session_id, tokens)` without `session_file`.

## Decision Log

- Decision: Resume persisted pi history, not live in-flight turns.
  Rationale: A BEAM restart loses Erlang ports, JSONL streams, worker command subjects, and EventHub live state. A new pi process can still use the saved session file.
  Date: 2026-04-29

- Decision: Keep pi session persistence opt-in for the first implementation.
  Rationale: The current default `--no-session` is explicit and predictable. Enabling persistence changes cleanup and recovery behavior and should be deliberate.
  Date: 2026-04-29

- Decision: Store pi sessions under Scherzo state rather than relying on global pi session discovery.
  Rationale: A Scherzo-owned session directory makes recovery deterministic for one workspace root and avoids accidentally resuming another project's recent session.
  Date: 2026-04-29

- Decision: Use a recovery prompt for resumed runs instead of resending the original issue prompt.
  Rationale: The resumed pi session already contains prior context. The recovery prompt should orient it to crash recovery and current workspace inspection, not restart the task from scratch.
  Date: 2026-04-29

## Outcomes & Retrospective

(To be filled at completion. Include final config names, chosen missing-session policy, command-construction approach, fake-pi transcript evidence, optional real-pi validation result, and final test count.)

## Context and Orientation

Scherzo launches pi through `src/scherzo/agent/pi_rpc.gleam` and orchestrates issue attempts through `src/scherzo/agent/runner.gleam`. The current example config uses `pi.command: "pi --mode rpc --no-session"`. The runner calls `pi_rpc.launch`, sends `set_session_name`, configures auto retry, calls `get_state`, and emits `pi_session_started` with the pi session id.

The session EventHub records Scherzo session ids and optional pi session ids but not pi session file paths. The hardening ledger from `hardening-02` and recovery logic from `hardening-03` can record local facts and plan interrupted-run retries. This plan adds the pi session file as a recoverable fact.

Pi sessions are JSONL files. Pi can start in RPC mode with a session directory or specific session file. RPC `get_state` reports `sessionFile` for persisted sessions. If `--no-session` is used, `sessionFile` may be absent.

## Preconditions and Verified Facts

Before implementing this plan:

- `docs/plans/hardening-01-graceful-daemon-lifecycle.md` is complete.
- `docs/plans/hardening-02-local-durable-state-ledger.md` is complete.
- `docs/plans/hardening-03-single-instance-crash-recovery.md` is complete.
- The ledger can record run starts/interruption and startup recovery can schedule interrupted-run retries.
- `direnv exec . gleam test` passes.

If hardening 03 is not complete, do not implement this plan first. Pi session continuation depends on the daemon knowing which run was interrupted and which workspace path belongs to it.

## Scope Boundaries

In scope: pi session persistence config; safe pi command construction for persisted sessions; decoding `sessionFile`; storing pi session attachment records in the ledger; recovery-run planning that can use `resume_session_file`; recovery prompt; workspace validation before resume; fake-pi tests; optional real-pi probe.

Out of scope: live process reattachment; preserving queued operator prompts across restart; preserving pending UI requests across restart; durable EventHub transcript archive; forking sessions; interactive session selector; distributed session sharing across workspace roots; posting session file paths to Linear.

## Milestones

Milestone 1 adds configuration and command construction. At the end, tests prove disabled persistence preserves the current command behavior and per-issue persistence builds commands with `--session-dir` for new runs and `--session <file>` for resumed runs.

Milestone 2 captures pi session files. At the end, pi RPC decodes `sessionFile`, runner updates include it, and a daemon test records `PiSessionAttached` in the ledger.

Milestone 3 resumes interrupted runs from session files. At the end, startup recovery can dispatch a recovery worker with a recorded `session_file` and the fake pi transcript shows `--session <file>` plus a recovery prompt.

Milestone 4 handles fallback and documentation. At the end, missing/corrupt session files follow the configured policy, README documents persisted pi sessions, and optional real-pi validation is recorded.

## Plan of Work

Extend `src/scherzo/domain.gleam` with pi session persistence types. A minimal shape is:

    pub type PiSessionPersistence {
      PiSessionDisabled
      PiSessionPerIssue
    }

    pub type MissingPiSessionPolicy {
      ParkOnMissingSession
      FreshSessionWithWarning
    }

Add fields to `PiConfig`: `session_persistence`, `session_dir`, and `missing_session_policy`. Defaults preserve current behavior: disabled persistence, no required session dir, and park or fresh fallback only matters when persistence is enabled.

Extend `src/scherzo/config.gleam` to parse:

    pi:
      session_persistence: disabled | per_issue
      session_dir: optional path
      missing_session_policy: park | fresh_with_warning

Resolve relative `session_dir` against the workspace root or workflow directory. Prefer resolving it under `workspace.root/.scherzo-state/pi-sessions` when omitted and persistence is enabled.

Create `src/scherzo/agent/pi_launch.gleam` or equivalent helper. It takes `PiConfig`, workspace root, issue identifier, and optional resume session file, then returns the shell command to launch. It validates that persistence-enabled commands do not include `--no-session` and appends either `--session-dir <dir>` or `--session <file>`. If appending flags to shell strings is unsafe, implement structured command fields and document the migration.

Extend `pi_rpc.Data` with `session_file: Option(String)` decoded from `sessionFile`. Extend `pi_rpc.Session` with `session_file: Option(String)`. Update `get_state` and `get_session_stats` paths to preserve this field. Add tests in `test/pi_rpc_test.gleam` for decoding `sessionFile`.

Extend `runner.PiUpdate` with `pi_session_file: Option(String)` or a richer metadata field. Emit it in `pi_session_started_update`. Update EventHub JSON only if the path is needed by local control; avoid exposing it in public user-facing output by default.

Extend `runner.WorkerSuccess` and `WorkerFailure` if necessary so daemon completion knows the last pi session file. Prefer recording the session file as soon as `get_state` succeeds, because a crash before worker completion is exactly the case this plan needs. The daemon should append `PiSessionAttached` on the `pi_session_started` update, not wait for success.

Extend ledger records with:

    PiSessionAttached(run_id, issue_id, session_id, session_file)
    RunResumedFrom(new_run_id, previous_run_id, session_file)

Add projection support that maps interrupted runs to their latest known pi session file.

Modify hardening recovery planning. When an interrupted run is retryable and has a valid session file, create a recovered dispatch request with `resume_session_file: Some(path)` and `resumed_from_run_id: Some(old_run_id)`. When no valid session file exists, use the configured missing-session policy.

Modify runner entrypoints so a resumed attempt receives a recovery prompt instead of the original workflow prompt. This may require adding a `RunKind` or `AttemptContext` argument to the agent runner dependency:

    FreshAttempt(attempt: Option(Int))
    ResumedAttempt(previous_run_id: String, session_file: String)

The recovery prompt should include issue identifier, title, previous run id, and instructions to inspect workspace state before continuing.

Update fake pi fixture to record launch arguments and to return a deterministic `sessionFile` from `get_state`. Add a mode where launching with `--session <file>` is required and fails otherwise.

## Concrete Steps

1. From the repository root, run `direnv exec . gleam test` and record the current pass count.

2. Add `test/pi_session_config_test.gleam`. Test defaults preserve disabled persistence, `per_issue` resolves a session directory under `.scherzo-state/pi-sessions`, `--no-session` is rejected when persistence is enabled, and invalid missing-session policy fails config.

3. Implement domain/config changes for pi session persistence.

4. Create `test/pi_launch_test.gleam`. Test disabled launch returns the current command unchanged, per-issue new launch appends `--session-dir`, resumed launch appends `--session <file>`, and paths with spaces are shell-escaped correctly.

5. Implement `src/scherzo/agent/pi_launch.gleam` and update runner/probe launch paths to use it where appropriate. Probe may continue to use non-persistent launch unless the plan decides probes should validate session persistence too.

6. Update `test/pi_rpc_test.gleam` so `get_state` and `get_session_stats` fake responses include `sessionFile`; assert decoded records and `pi_rpc.Session` contain it.

7. Modify `src/scherzo/agent/pi_rpc.gleam` to decode and preserve `sessionFile`.

8. Update `runner.PiUpdate` and tests so `pi_session_started` carries both session id and session file. Ensure redaction/logging does not expose the full path in Linear comments.

9. Add ledger record tests for `PiSessionAttached` and `RunResumedFrom`.

10. Modify daemon worker update handling to append `PiSessionAttached` when `pi_session_started` includes a session file and current run id is known.

11. Add recovery planner tests: interrupted run with session file produces resumed attempt; interrupted run with missing session file follows missing-session policy.

12. Modify daemon recovery dispatch so resumed attempts pass `resume_session_file` and `previous_run_id` into the runner dependency.

13. Add runner tests for resumed attempts. Assert the fake pi launch command includes `--session <file>`, the prompt is a recovery prompt, and the original full issue prompt is not sent.

14. Add fake-pi integration test for a crash/restart flow: first run records session file, recovery start launches with that file, and transcript proves continuation.

15. Add missing-session tests: remove the session file before recovery and assert Scherzo parks or fresh-starts according to config, with a clear log event.

16. Update README with `pi.session_persistence`, `pi.session_dir`, missing-session policy, and the distinction between persisted conversation continuation and live turn reattachment.

17. Run `direnv exec . gleam format`, `direnv exec . gleam format --check src test`, and `direnv exec . gleam test`. Record final pass count.

18. Optional real-pi validation: run a no-prompt or tiny fake/safe prompt workflow with persistence enabled, capture `sessionFile`, restart pi with `--session <file>`, call `get_state`, and verify the same session file/id is reported.

19. Commit the phase with a message such as `Resume interrupted workers from pi sessions`.

## Testing and Falsifiability

This plan is falsified if persistence-enabled runs still use `--no-session`, if Scherzo records only `sessionId` and not `sessionFile`, if recovery resends the original task prompt into a resumed session, if missing workspace/session files are ignored, if resume uses a global recent session instead of the recorded file, or if session file paths leak into Linear handoff comments.

Run from the repository root:

    direnv exec . gleam format --check src test
    direnv exec . gleam test

No deterministic test may require real pi. Use fake pi fixtures to assert launch arguments and session file behavior. Real-pi validation is optional and should be recorded in Outcomes.

## Validation and Acceptance

Accept this phase when:

- Pi session persistence is disabled by default and opt-in through config.
- Persistence-enabled launches use a Scherzo-owned session directory or explicit recorded session file.
- `get_state.sessionFile` is decoded and durably recorded for a run.
- Crash recovery can plan a resumed attempt from an interrupted run's session file.
- Resumed attempts use a recovery prompt and do not resend the original issue prompt.
- Missing/corrupt session files follow a documented policy.
- Session file paths are not posted to Linear comments.
- The full deterministic suite passes.

## Rollout, Recovery, and Idempotence

Roll out with persistence disabled by default. Enable it first on a private test project and workspace root. Existing runs without recorded session files continue to recover as fresh retries or parked issues according to hardening 03.

If session files are deleted manually, recovery follows the configured missing-session policy. If the policy parks, operators can inspect the workspace and unpark/retry manually. If the policy fresh-starts, Scherzo logs a warning and sends a fresh recovery prompt without prior pi history.

Resumed attempts are still at-least-once work. They may repeat some investigation, but they have prior conversation context and should inspect workspace state before acting.

## Artifacts and Notes

Example config:

    pi:
      command: "pi --mode rpc"
      session_persistence: per_issue
      session_dir: ".scherzo-state/pi-sessions"
      missing_session_policy: park

Example recovery prompt shape:

    Scherzo restarted while working on Linear issue LIV-9 in previous run LIV-9-1714320000000-1.
    You are being resumed from the prior pi session history.
    Inspect the current workspace before making changes. Do not repeat work that is already complete. Continue toward the issue goal and summarize what remains.

Example ledger records:

    PiSessionAttached(run_id="LIV-9-...", issue_id="...", session_id="abc123", session_file=".../.scherzo-state/pi-sessions/...jsonl")
    RunResumedFrom(new_run_id="LIV-9-...-recovery", previous_run_id="LIV-9-...", session_file="...")

## Interfaces and Dependencies

In `src/scherzo/domain.gleam`, add types equivalent to:

    pub type PiSessionPersistence {
      PiSessionDisabled
      PiSessionPerIssue
    }

    pub type MissingPiSessionPolicy {
      ParkOnMissingSession
      FreshSessionWithWarning
    }

Add fields to `PiConfig` equivalent to:

    session_persistence: PiSessionPersistence
    session_dir: Option(String)
    missing_session_policy: MissingPiSessionPolicy

In `src/scherzo/agent/pi_rpc.gleam`, extend session/data types equivalent to:

    pub type Session {
      Session(
        process: port.Process,
        command: String,
        cwd: String,
        session_id: Option(String),
        session_file: Option(String),
        next_id: Int,
      )
    }

In the runner or daemon dependency interface, add an attempt context equivalent to:

    pub type AttemptContext {
      FreshAttempt(attempt: Option(Int))
      ResumedAttempt(previous_run_id: String, session_file: String)
    }

No new package dependency should be required. This plan depends on the local ledger and recovery APIs from hardening plans 02 and 03.
