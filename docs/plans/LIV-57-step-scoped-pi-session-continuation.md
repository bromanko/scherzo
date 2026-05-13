# Step-scoped pi session continuation

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

Scherzo runs workflow steps from Linear issues. Agent steps launch pi, send a prompt, and rely on pi's local conversation state while the step is running. Today, if Scherzo or its host stops while an agent step is mid-flight, a later recovery path can only reason about the issue or run at a coarse level. It cannot safely reopen the exact pi session that belonged to the exact workflow step attempt, so the safest choices are to fail, park, or start over.

After this change, operators who explicitly opt in to pi session persistence can recover a crashed or interrupted agent step without resending the original prompt, but only for attempts that were started as continuation-capable and have a persisted step-scoped pi `sessionFile`. Scherzo will record whether each agent step attempt was continuation-capable at attempt start, then record the pi `sessionFile` returned by pi for the exact workflow step attempt, including the workflow run, workflow id, step id, workspace name, attempt index, workspace path, pi session id, and session file. When recovery policy says continuation is safe, Scherzo will relaunch pi from the recorded step workspace with `--session <recorded-session-file>` and will send a short recovery prompt that tells pi to continue from the already-opened transcript.

If session persistence was disabled for the original attempt, Scherzo has no durable transcript handle for that attempt. Recovery can still park, retry, or restart according to existing policies, but it cannot truthfully continue that interrupted step's pi session.

The visible outcome is that an interrupted continuation-capable workflow agent step can continue in its original step workspace with its original pi context, while unsafe recovery cases are parked instead of silently restarting from scratch.

## Problem Framing and Constraints

The operator problem is continuity and safety. A workflow step can leave useful work in its workspace and useful reasoning in pi's local transcript. If Scherzo restarts and sends the original prompt again, pi may duplicate changes, overwrite partial work, repeat external operations, or produce a result based on a changed Linear issue or workflow definition. If Scherzo guesses at a session file without tying it to the exact step attempt, it can resume the wrong context.

This plan solves step-scoped pi session persistence and continuation for workflow agent steps. A workflow agent step is a DAG step whose kind is `agent`; it launches pi and sends a prompt. A step attempt is one execution attempt of one workflow step. The attempt identity for this plan is `run_id`, `issue_id`, `issue_identifier`, `workflow_id`, `workflow_fingerprint`, `step_id`, `workspace_name`, `attempt_index`, and `workspace_path`. The current repository is still issue/run-scoped in the relevant durable state: `src/scherzo/state/record.gleam` has `RunStarted`, `RunFinished`, and `RunInterrupted`, `src/scherzo/state/recovery.gleam` recovers interrupted runs by issue, and `src/scherzo/workflow_run.gleam` does not pass a step-attempt context to `Dependencies.agent_step`. This plan therefore includes the missing step-attempt records, projection state, recovery action model, and runner integration as first-class early work. Do not rely on any other document, plan, or branch for the step-attempt design.

The `pi.session_persistence.enabled` config flag is rollout and privacy optionality, not semantic optionality. It controls whether new attempts are launched in a way that can persist a session, but it does not change the past capability of already-started attempts. `StepAttemptStarted` must therefore persist a `continuation_capable` boolean at attempt start. Recovery must read that stored field instead of inferring capability from current config, from the presence or absence of a later session fact, or from a missing session file after restart. An attempt recorded with `continuation_capable: False` is not eligible for true session continuation; an attempt recorded with `continuation_capable: True` that lacks exactly one usable session fact must park rather than silently becoming a fresh prompt restart.

A pi session file is the local file path returned by pi as `sessionFile` in the RPC `get_state` response. The file contains pi's local transcript and state. Scherzo must treat that file and any workspace path as sensitive local operator data. It may persist the path in the local Scherzo ledger because recovery needs it, but it must not upload the file, read transcript contents, or expose raw local session paths in Linear comments, operator summaries, daemon warnings, or attach JSON.

Backward compatibility with the obsolete issue-level pi session continuation design is explicitly out of scope. This plan does not migrate, read, or honor issue-level continuation records. It adds step-attempt facts to the current run-scoped state model and uses those facts for new continuation behavior only.

The scope is also constrained by command safety. The current tree stores `pi.command` as one shell string and starts it through the repository port wrapper. Appending `--session` to that arbitrary string would be unsafe because the string may contain environment assignments, shell syntax, quoting, or an explicit `--no-session`. This plan therefore introduces a structured argv launch path and requires it for session persistence and continuation.

## Strategy Overview

The chosen approach is to make pi session persistence opt-in, step-scoped, and validated before broad implementation. The first implementation milestone is a real-pi feasibility spike. It proves when a real pi RPC process reports `sessionFile`, whether reopening that file with `--session` from the same workspace succeeds, and whether the reopened process can continue from previous context. If the spike disproves any of those assumptions, the implementer must stop, update this ExecPlan with the observed behavior, and not build the later ledger and recovery machinery on a false premise.

After the spike, the plan adds a durable step-attempt foundation to the current run-scoped recovery model. Scherzo will record step-attempt start with a durable `continuation_capable` boolean, finish, interruption, continuation start, and pi-session-observed facts. These records are keyed by the complete attempt identity and projected into state that recovery can query after daemon restart.

When persistence is disabled for a fresh attempt, Scherzo keeps the current shell-string launch behavior and records `continuation_capable: False` on `StepAttemptStarted`. That fact means true session continuation is unavailable for the attempt; if a later recovery path restarts it, that action is a retry or restart from the original prompt, not continuation. When persistence is enabled for a fresh attempt, Scherzo records `continuation_capable: True`, requires a structured pi argv configuration, launches fresh pi sessions without `--no-session`, captures `sessionId` and `sessionFile` from `get_state` as soon as real-pi behavior proves they are available, and appends a step-attempt-scoped ledger fact. If current config disables session persistence during recovery, Scherzo treats that as an operator kill switch: it parks otherwise continuation-capable interrupted attempts with `recovery_session_persistence_disabled` instead of opening the recorded session or falling back to the original prompt.

Recovery is conservative. Static recovery planning produces a continuation request only when the recorded `continuation_capable` field is `True`, current config still permits session persistence, and all ledger and filesystem facts line up: the refreshed Linear issue has not drifted, the selected workflow and step definition still match the recorded workflow fingerprint, the recorded workspace exists and is safe, the session file exists, and exactly one matching session fact exists for the interrupted attempt. The actual pi reopen validation happens in the same process that will perform the continuation. Scherzo starts pi once with `--session <recorded-session-file>`, calls startup RPCs and `get_state`, verifies the reopened session file and cwd when pi reports them, and sends the recovery prompt only on that same already-validated process. It does not perform a separate launch solely for preflight, because opening the same pi session twice may mutate metadata, acquire locks, or otherwise disturb the transcript.

The implementation is proportionate because it reuses the workflow DAG runner, workspace runner, pi RPC client, state ledger, projection, and recovery planner that exist today, while adding only the step-attempt state needed to make recovery exact. It adds one structured launch path rather than replacing all process execution. It records metadata, not pi transcript contents. It includes operator-facing config documentation and redaction tests, while deferring retention dashboards and cleanup UX to a later plan.

## Alternatives Considered

The simplest alternative is to append ` --session <file>` to `config.pi.command` during recovery. That is rejected. `pi.command` is an arbitrary shell string and currently defaults to a no-session launch. It may include shell quoting, environment variables, wrappers, or flags that conflict with `--session`. Blind string concatenation would make recovery dependent on unvalidated shell syntax and could execute a different command than intended.

Another alternative is to keep issue-level session continuation and pick the latest session for an issue. That is rejected because workflows can run multiple agent steps, can fan out into multiple workspaces, and can retry individual steps. The latest issue session may belong to a different step or workspace. The acceptance criteria require attaching sessions to the exact attempt fields, so issue-level state is the wrong abstraction.

A third alternative is to always restart the step from the original prompt when recovery cannot prove safety. That is rejected for session-enabled attempts because it hides a dangerous fallback behind an apparently successful recovery. This plan parks instead. Operators may explicitly unpark and rerun later, but Scherzo will not automatically resend the original prompt for an interrupted session-enabled attempt.

## Risks and Countermeasures

The main feasibility risk is that real pi may not behave like fake pi. A real pi RPC launch might not return `sessionFile` before the first prompt, might return it under a different field, might create the file only after a turn, or might fail to reopen the captured file with `--session`. The countermeasure is Milestone 0: a narrow real-pi spike before broad code changes. The spike records when `sessionFile` first appears and proves protocol-level reopen behavior. If the observation differs from this plan, the implementer updates this plan and changes the capture point before continuing.

The main safety risk is resuming the wrong pi context. The countermeasure is to key session facts by the complete step attempt identity: `run_id`, `issue_id`, `issue_identifier`, `workflow_id`, `workflow_fingerprint`, `step_id`, `workspace_name`, `attempt_index`, and `workspace_path`, with `session_id` and `session_file` recorded as data on that key. Recovery must look up exactly one matching session fact for the interrupted attempt. Missing or ambiguous matches park recovery.

The main semantic risk is confusing the opt-in flag with the attempt's durable capability. The countermeasure is to record `continuation_capable` on `StepAttemptStarted` before pi launches. Recovery must use that recorded value: current config can disable execution as a kill switch, but it cannot make an old non-persistent attempt continuation-capable, and a missing session fact for a continuation-capable attempt is a safety failure that parks rather than downgrading to a prompt restart.

The main data-integrity risk is drift. The Linear issue, workflow label, workflow DAG, prompt reference, model settings, or step workspace declaration may change between the original attempt and recovery. The countermeasure is to store issue and workflow fingerprints on step-attempt start and compare them during recovery. If the refreshed issue fingerprint or current workflow fingerprint differs from the recorded value, recovery parks and does not launch pi.

The main filesystem risk is using a stale or unsafe workspace path. The countermeasure is to validate that the recorded workspace path is non-empty, resolves inside the configured workspace root, is a directory, is not the root itself, and is the deterministic path for the recorded `workflow_id`, `run_id`, and `workspace_name` where that deterministic check is available. Missing or unsafe workspaces park recovery before any pi process starts.

The main command-execution risk is shell injection or conflicting session flags. The countermeasure is to require structured argv for session persistence. Scherzo owns session flags. It rejects configured argv values that already contain `--session` or `--no-session` when persistence is enabled, and it passes the session file as its own argv item.

The main continuation risk is mutating or disturbing a pi session before recovery. A separate preflight launch could update session metadata, create locks, consume startup events, or make the later continuation differ from a single resume launch. The countermeasure is to combine reopen validation with continuation execution. The recovery executor launches pi once, validates `get_state`, and either sends the recovery prompt on that same process or terminates and parks before sending any prompt.

The main privacy risk is exposing pi transcripts or local session paths. The countermeasure is to persist only the local `session_file` path in the local ledger, never transcript contents. `src/scherzo/state/record.gleam` must redact `session_file` and `workspace_path` when records are rendered for diagnostics. `src/scherzo/state/recovery.gleam` and `src/scherzo/orchestrator/daemon.gleam` warnings must include reason codes and attempt identifiers but not raw session paths. `src/scherzo/session/json.gleam` summaries and attach/event JSON must not gain a `session_file` field. README and example config documentation must describe local sensitivity and retention boundaries. Retention cleanup controls remain out of scope, but leakage prevention is in scope and testable.

## Progress

- [x] (2026-05-04 00:00Z) Drafted this ExecPlan from LIV-57 and inspected the current repository surfaces needed for an accurate plan.
- [x] (2026-05-04 00:00Z) Incorporated adversarial review feedback by folding the missing step-attempt/recovery foundation into this plan, moving real-pi validation to the first milestone, replacing separate preflight with single-process reopen validation, and adding privacy/documentation test work.
- [x] (2026-05-04 15:20Z) Addressed PR review feedback by separating rollout optionality from true continuation semantics and requiring `continuation_capable` to be persisted on step-attempt start.
- [x] (2026-05-05 14:45Z) Ran the real-pi validation through the production structured launch path. `SCHERZO_REAL_PI_VALIDATION=1 direnv exec . gleam test` passed, proving a captured `sessionFile` could be reopened with `--session` from the recorded workspace and accept a recovery prompt.
- [x] (2026-05-05 14:45Z) Added durable step-attempt records, projection state, and recovery continuation request plumbing to the current run-scoped state model.
- [x] (2026-05-05 14:45Z) Added structured pi argv configuration and launch construction for session-enabled pi runs.
- [x] (2026-05-05 14:45Z) Decoded and captured pi `sessionFile` and recorded it against the exact step attempt.
- [x] (2026-05-05 14:45Z) Extended recovery planning to choose safe step-scoped continuation candidates and to park unsafe cases.
- [x] (2026-05-05 14:45Z) Executed continuation from the recorded workspace with `--session` and a recovery prompt on the same process used for reopen validation.
- [x] (2026-05-05 14:45Z) Added fake-pi, state, workflow-runner, command-construction, redaction, and documentation tests, including post-review coverage for argv/cwd logging, no original prompt resend, no prompt on resume-validation failure, and recovery failure code propagation.
- [x] (2026-05-05 14:45Z) Completed mandatory real-pi validation evidence and recorded concise results in this plan.

## Surprises & Discoveries

- Observation: The current `pi.command` configuration is a single shell string and the repository port wrapper starts it through a shell in `src/scherzo_port_ffi.erl`.
  Evidence: `src/scherzo/config.gleam` defaults `pi.command` to `pi --mode rpc --no-session`; `src/scherzo/pi/client.gleam` passes that string to `port.start`; `src/scherzo/port.gleam` exposes `start(command, cwd)`.
- Observation: At plan start, the workflow runner already prepared per-step workspaces and routed agent steps through `run_prompt_in_workspace`, but it did not yet create a durable step-attempt context.
  Evidence: `src/scherzo/workspace_run.gleam` defines `PreparedStepWorkspace`; `src/scherzo/workflow_run.gleam` prepares steps, renders agent prompts, applies per-step model settings, and now calls `Dependencies.agent_step` with prompt mode, attempt context, and session recording.
- Observation: At plan start, durable state had run-level interruption recovery, not step-attempt recovery.
  Evidence: `src/scherzo/state/record.gleam`, `src/scherzo/state/projection.gleam`, and `src/scherzo/state/recovery.gleam` now include step-attempt and step pi-session recovery state in addition to the earlier run records.
- Observation: At plan start, the fake pi fixture supported transcript capture but not launch argv or configurable `sessionFile` behavior.
  Evidence: `test/fixtures/fake_pi_rpc.sh` now records argv and cwd when `FAKE_PI_ARGV_LOG` is set, returns session files from `FAKE_PI_SESSION_FILE` or `--session`, and supports mismatch and `get_state` failure controls.
- Observation: Operator config documentation initially showed only `pi.command: "pi --mode rpc --no-session"`.
  Evidence: `README.md` and `examples/scherzo.yaml` now document `pi.argv`, `pi.session_persistence.enabled`, local transcript sensitivity, and retention boundaries.
- Observation: The post-review real-pi validation passed through the production structured argv path.
  Evidence: `SCHERZO_REAL_PI_VALIDATION=1 direnv exec . gleam test` passed with 749 tests and no failures after launching real pi, capturing a non-empty `sessionFile`, reopening it with `--session` from the same workspace, validating `get_state`, and sending a recovery prompt.
- Observation: Recovery resume-validation failure needed an explicit failure code that survives the workflow-runner artifact path.
  Evidence: Review found validation failures flowed back as ordinary worker failure. The implementation now emits `recovery_pi_resume_validation_failed`, stores it in the step artifact failure code, treats it as fatal regardless of step failure policy, and parks the issue in daemon failure handling.

## Decision Log

- Decision: This plan includes the step-attempt records, projection state, recovery continuation request, and workflow-runner plumbing instead of depending on any other document or branch.
  Rationale: The current tree is issue/run-scoped. A self-contained ExecPlan must be executable from the implementation branch without telling the implementer to stop and find other plans.
  Date: 2026-05-04
- Decision: Real-pi `sessionFile` capture and reopen behavior must be validated in Milestone 0 before broad ledger, projection, and recovery work proceeds.
  Rationale: If real pi cannot provide or reopen the session file as assumed, later architecture would implement the wrong behavior.
  Date: 2026-05-04
- Decision: Session-enabled launches must use structured argv, not shell-string appending.
  Rationale: The current shell command may contain arbitrary syntax and may include `--no-session`; a structured argv path lets Scherzo validate and own `--session` construction.
  Date: 2026-05-04
- Decision: Record pi sessions as step-attempt-scoped ledger facts, not as issue-level state.
  Rationale: Workflows can have multiple agent steps, retries, and workspaces. Recovery must resume the exact prior step attempt.
  Date: 2026-05-04
- Decision: Persist `continuation_capable` on `StepAttemptStarted` instead of deriving continuation capability during recovery.
  Rationale: The session-persistence flag is a rollout and privacy control, while true continuation requires a session file captured for the original attempt. Recovery must not infer historical capability from current config or from a missing session fact after restart.
  Date: 2026-05-04
- Decision: Recovery uses single-process reopen validation and continuation, not a separate pi preflight process.
  Rationale: Opening the same session twice may mutate session metadata or locking state. Starting once, validating, and sending the recovery prompt on that same process avoids disturbing the session before continuation.
  Date: 2026-05-04
- Decision: Recovery parks on current persistence kill-switch disablement, drift, missing workspace, missing session file, missing session fact, ambiguous session fact, or resume-validation failure instead of falling back to the original prompt.
  Rationale: Resending the original prompt can duplicate or corrupt work. A parked issue is visible and reversible, and a disabled current config is an operator instruction not to open local pi session files.
  Date: 2026-05-04
- Decision: A continuation reuses the interrupted attempt's `attempt_index` and records a continuation-started fact instead of starting a new attempt index.
  Rationale: The work being continued is the same logical attempt and the same pi transcript; new attempt indexes are reserved for true retries from a fresh prompt.
  Date: 2026-05-04
- Decision: Operator-facing config documentation, invalid-config messages, and privacy redaction tests are in scope; retention dashboards and cleanup controls are deferred.
  Rationale: Operators need enough documentation to enable the feature safely, and leakage prevention must be verifiable now. Durable retention policy and cleanup UX are a separate operator-control surface.
  Date: 2026-05-04
- Decision: Dynamic resume-validation failure uses the explicit reason code `recovery_pi_resume_validation_failed` and is fatal even when the recovered step has `on_failure: continue`.
  Rationale: Reopen validation happens before any recovery prompt is sent. If it fails, continuing downstream or retrying as an ordinary worker failure could hide an unsafe recovery path. The explicit code lets the daemon park the issue with the required reason.
  Date: 2026-05-05

## Outcomes & Retrospective

Post-review implementation completed the step-scoped continuation path and closed the main safety gaps from review. Fake-pi now exposes launch argv, cwd, configurable session files, and resume-validation mismatch controls, so tests can prove continuation launches use structured argv, run from the recorded workspace, send only the recovery prompt, and send no prompt when reopen validation fails. Resume-validation failure now propagates as `recovery_pi_resume_validation_failed`, is fatal even for `on_failure: continue`, and parks the issue instead of flowing through ordinary retry/failure handling. Operator docs now describe `pi.argv`, `pi.session_persistence`, local transcript sensitivity, and the current retention boundary.

Validation on 2026-05-05 passed with `direnv exec . gleam format --check src test`, `direnv exec . gleam test`, and `SCHERZO_REAL_PI_VALIDATION=1 direnv exec . gleam test`. The real-pi validation used the production structured argv path, captured a `sessionFile`, terminated the first process, reopened with `--session <captured-session-file>` from the same workspace, validated `get_state`, and successfully sent a recovery prompt. No transcript contents, session file contents, or machine-specific session paths are recorded here.

## Context and Orientation

This repository is a Gleam project targeting Erlang. The usual validation commands are run from the repository root with direnv, for example `direnv exec . gleam test` and `direnv exec . gleam format --check src test`.

The workflow DAG parser lives in `src/scherzo/workflow_dag.gleam`. A DAG is a directed acyclic graph: a list of workflow steps where each step may depend on earlier steps. `WorkflowStep` has an `id`, a `kind`, a `workspace`, a failure policy, and model settings. Agent steps are represented by `AgentStep(prompt)`. Command steps are represented by `CommandStep(run, timeout_ms)` and are not part of pi session continuation.

The workflow runner lives in `src/scherzo/workflow_run.gleam`. It prepares ready steps, executes ready batches, renders agent prompts with step artifact locals, applies model settings, and calls the `agent_step` dependency. The default agent dependency calls `src/scherzo/agent/run_attempt.gleam` through `run_prompt_in_workspace`, passing the exact prepared step workspace path. The current dependency signature does not include durable step-attempt context, so this plan extends it.

The per-step workspace helper lives in `src/scherzo/workspace_run.gleam`. `PreparedStepWorkspace` contains `workflow_id`, `run_id`, `run_root`, `workspace_name`, `path`, `source_workspace_name`, and `source_workspace_path`. The helper builds workflow run workspaces under the configured workspace root, grouping by workflow id, issue identifier, run id, and workspace name after sanitizing those components.

The pi RPC client lives in `src/scherzo/pi/client.gleam`. It currently launches pi, sends `set_session_name`, sends `set_auto_retry`, sends `get_state`, and stores `session_id` on its `Session`. The protocol decoder lives in `src/scherzo/pi/protocol.gleam`. It already decodes `sessionId` from `get_state` data; this plan extends it to decode `sessionFile` and the current working directory field when pi provides it.

The process wrapper lives in `src/scherzo/port.gleam` with Erlang FFI in `src/scherzo_port_ffi.erl`. It currently accepts a shell command string and a cwd. This is why this plan adds a structured argv path instead of mutating `pi.command` strings.

The state ledger and projection live in `src/scherzo/state/record.gleam` and `src/scherzo/state/projection.gleam`. The recovery planner lives in `src/scherzo/state/recovery.gleam`. The current tree has issue-level run records such as `RunStarted`, `RunFinished`, and `RunInterrupted`. This plan adds step-attempt records and recovery continuation requests directly to those current modules.

The daemon integration that appends ledger records and applies recovery plans lives in `src/scherzo/orchestrator/daemon.gleam`. The implementation must route new step-attempt records through the same local ledger append path used for existing run and outbox records. Operator summaries and attach/event JSON are represented under `src/scherzo/session/`, especially `src/scherzo/session/json.gleam`; those surfaces must not expose raw pi session file paths.

The fake pi test fixture lives in `test/fixtures/fake_pi_rpc.sh`. It reads JSON RPC lines from stdin, writes JSON RPC lines to stdout, records input lines through `FAKE_PI_TRANSCRIPT`, records launch argv and cwd through `FAKE_PI_ARGV_LOG`, returns fake session metadata from `get_state`, and supports session-file mismatch and `get_state` failure controls so tests can assert recovery prompt behavior.

Operator config documentation lives in `README.md` and the reusable example config `examples/scherzo.yaml`. This plan updates those docs during implementation so operators know how to enable structured argv and session persistence safely.

## Preconditions and Verified Facts

Before implementation starts, verify that the working tree is clean with `jj status --color=never` from the repository root. The expected status for a new implementation branch is no unrelated changes. Do not create, switch, finish, forget, or otherwise manage jj workspaces as part of this plan.

This plan is intentionally self-contained against the current repository shape. The current tree does not need pre-existing step-attempt modules or recovery actions. If another branch has already added equivalent step-attempt records or recovery continuation requests by the time this plan is implemented, normalize by reusing those concrete types only if they provide all fields and safety semantics listed here. Otherwise, implement the types in this plan and record the choice in the Decision Log.

The current verified repository facts this plan relies on are:

- `src/scherzo/config/types.gleam` defines `PiConfig` with `command`, timeout fields, `auto_retry`, UI request policy, compatibility probe, and rate-limit payload.
- `src/scherzo/config.gleam` defaults `pi.command` to `pi --mode rpc --no-session` and defaults compatibility probing to enabled.
- `src/scherzo/pi/client.gleam` launches pi from a cwd, sets the session name, sets auto-retry, calls `get_state`, and stores `session_id` on `Session`.
- `src/scherzo/pi/protocol.gleam` decodes RPC records and already has a data decoder shape that can be extended for new `get_state` fields.
- `src/scherzo/workflow_run.gleam` routes agent steps through a dependency function, which gives tests a seam for asserting prompt, workspace, and new attempt-context behavior.
- `src/scherzo/workspace_run.gleam` computes deterministic workflow-run workspace paths and has cleanup safety checks that can inform recovery workspace validation.
- `src/scherzo/state/record.gleam`, `src/scherzo/state/projection.gleam`, and `src/scherzo/state/recovery.gleam` currently model issue-level run recovery; this plan adds step-attempt state there.
- `src/scherzo/orchestrator/daemon.gleam` appends ledger records and must be updated to append step-attempt records emitted while workflow steps run and while continuation recovery executes.
- `README.md` and `examples/scherzo.yaml` are the repository-local operator config documentation surfaces to update for `pi.argv` and `pi.session_persistence`.
- `test/fixtures/fake_pi_rpc.sh` is the right fixture to extend for launch-argument, cwd, session-file, failure-mode, and prompt assertions.

## Scope Boundaries

In scope:

- Add durable step-attempt records, projection state, and recovery continuation request plumbing to the current run-scoped recovery model, including a persisted `continuation_capable` field on step-attempt start.
- Add opt-in configuration for pi session persistence.
- Persist `continuation_capable: True` only for attempts launched with session persistence enabled; attempts launched while persistence is disabled are not true continuation candidates.
- Add a structured argv launch path for pi when persistence is enabled.
- Decode `sessionFile` from pi RPC `get_state` responses and retain the existing `sessionId` behavior.
- Record step-attempt-scoped pi session facts with `run_id`, `issue_id`, `issue_identifier`, `workflow_id`, `workflow_fingerprint`, `step_id`, `workspace_name`, `attempt_index`, `workspace_path`, `session_id`, and `session_file`.
- Extend projection and recovery planning so interrupted agent step attempts can become continuation candidates only when the recorded `continuation_capable` field, issue, workflow, workspace, and session facts are safe.
- Launch recovery pi with `--session <recorded-session-file>` from the recorded step workspace, validate the reopened session on that same process, and send a recovery prompt instead of the original prompt.
- Add tests using fake pi for launch args, cwd, recovery prompt, no original prompt resend, exact attempt-scoped session recording, and resume-validation failure before prompt.
- Add mandatory real-pi feasibility and final validation milestones that prove a captured `sessionFile` can be reopened from the recorded workspace.
- Update `README.md` and `examples/scherzo.yaml` with the minimal opt-in config and safety notes.
- Add redaction and privacy tests for ledger record rendering, recovery warnings, session summaries, and operator-facing JSON surfaces.

Out of scope:

- Migration from obsolete issue-level pi session continuation records.
- Treating session-disabled historical attempts as true continuations after config is enabled later.
- Continuing command steps; command steps have no pi session.
- Reconstructing missing workspaces or copying work into a new workspace during recovery.
- Uploading pi transcripts or session files to Linear, artifacts, logs, or remote storage.
- Operator-facing retention dashboards, detailed recovery dashboards, or cleanup UX; those are deferred to a later operator-control plan.
- Changing the default behavior for users who do not opt in to session persistence.

## Milestones

Milestone 0 proves the real-pi premise before broad implementation. At the end of this milestone, a gated real-pi feasibility test or script has launched a real pi RPC process from a repository-local scratch workspace, observed when `get_state.sessionFile` becomes non-empty, relaunched pi with `--session` using structured argv, and confirmed protocol-level reopen behavior from the same workspace. The proof is a concise note in this plan that records the command shape, observed timing, and pass/fail result without private transcript contents or machine-specific paths. If the spike fails, stop and revise this plan before continuing.

Milestone 1 adds the durable step-attempt foundation that the current tree lacks. At the end of this milestone, the ledger can encode and decode step-attempt started, finished, interrupted, continuation-started, and pi-session-recorded facts; the started fact persists `continuation_capable`; projection can identify running or interrupted step attempts and preserve their capability; and recovery plans can carry continuation requests without the original prompt. The proof is state record and projection tests that distinguish two attempts for the same issue by step id and attempt index and assert their recorded capability is not recomputed from config.

Milestone 2 establishes safe launch construction. At the end of this milestone, the config layer can represent a structured pi argv command, validation rejects session persistence without structured argv, and the pi client can launch from a validated launch spec. Fresh non-persistent launches continue to use the existing shell command. The proof is unit tests that validate config parsing and launch-spec construction, including rejection of `--session` and `--no-session` in session-enabled argv, plus fake-pi argv/cwd tests.

Milestone 3 captures pi session metadata for fresh agent step attempts. At the end of this milestone, a fresh session-enabled agent step records `continuation_capable: True` on start and records `session_id` and `session_file` against the exact step attempt at the capture point proven by Milestone 0. A fresh session-disabled agent step records `continuation_capable: False` and no session observation. The proof is fake-pi tests showing the ledger or in-memory recorder receives the complete step attempt identity, capability, and session fields, and separate attempts do not overwrite each other.

Milestone 4 extends static recovery planning and workspace safety checks. At the end of this milestone, the recovery planner can identify an interrupted agent step attempt whose recorded `continuation_capable` field is true and whose session facts are safe, then either produce a continuation request or park the issue with a precise reason before pi is launched. Attempts recorded with `continuation_capable: False` are not true continuation candidates, and attempts recorded as capable park with `recovery_session_persistence_disabled` if current config has disabled the kill switch. The proof is state recovery tests for capability persistence, current-config toggles, issue drift, workflow drift, missing or ambiguous session facts, missing or unsafe workspace, and missing session file.

Milestone 5 executes continuation with single-process reopen validation. At the end of this milestone, a continuation request launches pi once with `--session <recorded-session-file>` in the recorded workspace, validates `get_state` on that process, and sends the recovery prompt, not the original prompt. If validation fails, it parks before sending any prompt. The proof is fake-pi integration coverage that records argv, cwd, validation failures, and prompt JSON lines.

Milestone 6 hardens privacy, docs, and existing behavior. At the end of this milestone, normal session-disabled workflow runs still use the existing shell command path, session-enabled fresh runs use structured argv without `--session`, recovery never falls back to the original prompt, operator docs show the minimal opt-in config, and operator-facing summaries do not leak session file paths or transcript contents. The proof is the full Gleam test suite, formatting, targeted redaction/documentation tests, and final real-pi validation evidence.

## Plan of Work

Start with the real-pi feasibility spike. Add the smallest structured argv process support needed for the spike in `src/scherzo/port.gleam` and `src/scherzo_port_ffi.erl`, or add it directly if the implementation prefers to keep the spike's support code as the production structured argv primitive. The function should be named `start_argv(executable, args, cwd, env)` and must pass argv items as separate data values, not by concatenating shell fragments. Add a gated test in `test/real_pi_session_validation_test.gleam` that is skipped unless `SCHERZO_REAL_PI_VALIDATION=1`. The test should launch real pi in RPC mode from `test/tmp/real-pi-session-validation`, call the current startup RPCs and `get_state`, send a first prompt with a unique token, call `get_state` again if needed, terminate, relaunch with `--session` and the captured session file as separate argv items, call `get_state`, and send a recovery prompt that does not repeat the token. Protocol-level pass criteria are non-empty `sessionFile`, successful relaunch, compatible reopened `sessionFile`, compatible cwd when pi reports cwd, and no prompt sent before reopen validation. Token recall is supporting evidence, not the only pass condition, because provider behavior can be nondeterministic. If the test shows that `sessionFile` is not available before the first prompt, update the later capture step to the earliest proven safe point and record that decision before continuing.

Add a step-attempt domain module, preferably `src/scherzo/workflow_attempt.gleam`, because the current tree does not have one. Define `StepAttemptContext` with `run_id`, `issue_id`, `issue_identifier`, `workflow_id`, `workflow_fingerprint`, `step_id`, `workspace_name`, `attempt_index`, `workspace_path`, and `continuation_capable`. Define `PiSessionObservation` with the same identity fields plus `session_id` and `session_file`. Define `StepAttemptKey` or a key helper that length-prefixes string components and includes the integer attempt index; do not build keys with a bare delimiter, and do not include `continuation_capable` in the key because it is capability metadata rather than identity. Define `AgentPromptMode` with `OriginalPrompt(String)` and `RecoveryPrompt(String)`. For the initial implementation, set `attempt_index` to `1` for the first execution of a step within a `run_id`; reserve higher indexes for future true step retries inside the same run. Set `continuation_capable` to `True` only when the attempt is launched with validated session persistence enabled, persist that value immediately, and never recompute it during recovery.

Define fingerprint helpers in `src/scherzo/workflow_attempt.gleam`. `issue_fingerprint(issue)` should reuse the existing park-release fingerprint helper if one exists; otherwise factor the current `IssueParkedV2.issue_fingerprint` construction out of `src/scherzo/state/recovery.gleam` so the same value is used for parking and step-attempt drift checks. `workflow_fingerprint(dag, workflow_id)` should produce a stable string from the workflow id and each step's id, kind, workspace reference, dependency ids, failure policy, model settings, and prompt reference or prompt template content hash. The exact encoding can be a stable JSON string or another deterministic string, but tests must prove that changing an agent prompt, workspace name, model setting, or dependency changes the fingerprint.

Add ledger records in `src/scherzo/state/record.gleam`. The required new record bodies are `StepAttemptStarted`, `StepAttemptFinished`, `StepAttemptInterrupted`, `StepAttemptContinuationStarted`, and `StepAttemptPiSessionRecorded`. Use JSON kinds `step_attempt_started`, `step_attempt_finished`, `step_attempt_interrupted`, `step_attempt_continuation_started`, and `step_attempt_pi_session_recorded`. Use snake-case field names. `StepAttemptStarted` must include the attempt identity, issue fingerprint, workflow fingerprint, `step_kind`, `prompt_mode` as `original` for fresh execution, and `continuation_capable`. `StepAttemptContinuationStarted` must include the same identity and `prompt_mode` as `recovery`. `StepAttemptFinished` must include the identity and a status string such as `succeeded` or `failed`. `StepAttemptInterrupted` must include the identity and a reason string. `StepAttemptPiSessionRecorded` must include the identity, `session_id`, and `session_file`. Add decode, encode, `kind`, and redaction support for each new record.

Update `src/scherzo/state/projection.gleam` so the projection keeps step-attempt status and pi session observations per exact attempt key. Add fields such as `step_attempts: Dict(String, StepAttemptStatus)` and `step_pi_sessions: Dict(String, PiSessionStatus)` to `Projection`. `StepAttemptStatus` should represent started, finished, interrupted, and continuation-started states with timestamps, identity fields, and the persisted `continuation_capable` value from `StepAttemptStarted`. The projected pi session value must preserve all identity fields, `session_id`, `session_file`, and `recorded_at_ms`. Projection must keep two attempts with the same issue and run but different `step_id` or `attempt_index` distinct.

Extend `src/scherzo/state/recovery.gleam` with continuation requests. Add a public type like `StepPiContinuationRequest` containing the exact attempt identity, `session_id`, `session_file`, `workspace_path`, `issue_fingerprint`, `workflow_fingerprint`, and rendered recovery prompt. Add a `pi_session_continuations: List(StepPiContinuationRequest)` field to `RecoveryPlan`. Static planning must never include the original prompt in this request. During planning, inspect interrupted or still-running step attempts from projection. For each interrupted agent attempt, first read the persisted `continuation_capable` value from the attempt status. If it is `False`, do not create a true continuation request and do not treat a missing session fact as surprising; any later run-level recovery is a retry or restart, not continuation. If it is `True`, require current config to keep session persistence enabled, compare the refreshed issue fingerprint and current workflow fingerprint with the recorded values, validate that exactly one matching session fact exists, validate the recorded workspace path and session file path, and either add a continuation request or add a park record with a specific reason.

Static recovery planning should not launch pi. The park reasons for static checks on continuation-capable attempts are:

- `recovery_session_persistence_disabled`
- `recovery_issue_drift`
- `recovery_workflow_drift`
- `recovery_workspace_missing`
- `recovery_workspace_unsafe`
- `recovery_session_file_missing`
- `recovery_session_fact_missing`
- `recovery_session_fact_ambiguous`

Use the existing park record shape if possible, such as `IssueParkedV2`, and add new reason constructors in `src/scherzo/orchestrator/reason.gleam` if that is where park reasons are modeled. Warnings should include the reason and attempt key, not raw session file paths.

Add workspace validation for recovery in `src/scherzo/workspace_run.gleam`, for example `validate_recorded_step_workspace(recorded_path, issue, workflow_id, run_id, workspace_name, orchestrator) -> Result(String, error.WorkspaceError)`. It must verify the recorded path resolves inside the configured workspace root, is a directory, is not the workspace root itself, and matches the deterministic path for the recorded identity when the deterministic path can be recomputed. Return the normalized safe path on success. Missing directories and unsafe paths must be distinct errors so the recovery planner can park with the correct reason.

Next, implement the structured pi launch and configuration surface. In `src/scherzo/config/types.gleam`, add a structured pi command type and a session persistence config. Use stable names so the rest of the plan can refer to them:

    pub type PiArgvCommand {
      PiArgvCommand(
        executable: String,
        args: List(String),
        env: List(#(String, String)),
      )
    }

    pub type PiSessionPersistenceConfig {
      PiSessionPersistenceConfig(
        enabled: Bool,
        recovery_prompt: String,
      )
    }

Extend `PiConfig` with `argv_command: Option(PiArgvCommand)` and `session_persistence: PiSessionPersistenceConfig`. Keep the existing `command: String` field. When `session_persistence.enabled` is false, the existing shell command remains the launch source for new attempts and those attempts record `continuation_capable: False`. When it is true, `argv_command` is required, new attempts record `continuation_capable: True`, and the shell command is not used for pi launches or probes.

In `src/scherzo/config.gleam`, parse optional YAML fields under `pi`: `argv` as a non-empty list of strings, `argv_env` as an optional map of string keys to string values, `session_persistence.enabled` as a boolean defaulting to false, and `session_persistence.recovery_prompt` as an optional string. If absent, use the default recovery prompt defined below. Validation rules are part of the design. If `session_persistence.enabled` is true, `argv` must be present, the executable must be non-empty after trimming, every arg must be a single argv item, and no argv item may equal `--session` or `--no-session`. Scherzo owns those flags. If these validations fail, return `InvalidConfig` with a message that starts with `pi.session_persistence` so operator errors are easy to diagnose.

Add a launch construction module, preferably `src/scherzo/pi/command.gleam`. Define `LaunchSpec` and `LaunchMode` as described in the Interfaces and Dependencies section. Expose `build_launch(config, mode)`. `FreshNoSession` uses `ShellLaunch(config.command)`. `FreshPersistent` uses `ArgvLaunch` from `argv_command` without adding `--session`. `ContinueSession` uses `ArgvLaunch` from `argv_command` with `--session` and `session_file` appended as two separate arguments. `ContinueSession` rejects empty session files and configs without `argv_command`.

Update `src/scherzo/pi/client.gleam` to accept a `LaunchSpec` on new code paths. Keep a compatibility wrapper if that minimizes churn, but route session-enabled fresh and continuation launches through `LaunchSpec`. Extend `Session` with `session_file: Option(String)`. After `get_state`, store both `session_id` and `session_file` from the decoded record. Update `src/scherzo/pi/protocol.gleam` to decode `data.sessionFile` into `RpcRecord.session_file: Option(String)` and `data.cwd` into `RpcRecord.cwd: Option(String)`. Keep decoding tolerant: missing fields produce `None`, malformed non-string fields produce `None`, and existing records still decode.

Update the runner seam in `src/scherzo/workflow_run.gleam`. Extend `Dependencies.agent_step` to receive `workflow_attempt.StepAttemptContext`, `workflow_attempt.AgentPromptMode`, and a session observation callback such as `fn(workflow_attempt.PiSessionObservation) -> Nil`. Fresh agent steps pass `OriginalPrompt(rendered_prompt)`. Continuation steps pass `RecoveryPrompt(rendered_recovery_prompt)`. The default dependency should call `src/scherzo/agent/run_attempt.gleam` with the mode and context. The agent runner must never receive the original prompt when executing a recovery continuation, so tests can prove no accidental fallback is possible.

Update `src/scherzo/agent/run_attempt.gleam`. For session-enabled fresh runs, launch with `FreshPersistent`; for session-disabled fresh runs, keep the legacy `FreshNoSession` shell launch and make sure the already-appended `StepAttemptStarted` records `continuation_capable: False`. For continuation runs, launch with `ContinueSession(recorded_session_file)`, validate the reopened `get_state` on the same process, and only then send the recovery prompt. If resume validation fails, return a failure that the recovery executor maps to park reason `recovery_pi_resume_validation_failed`; do not send any prompt. For fresh session-enabled runs, record `PiSessionObservation` at the capture point proven by Milestone 0. If both `session_id` and `session_file` are present and non-empty, call the recorder before the first prompt whenever the spike proved that is possible. If `sessionFile` appears only after the first prompt, record it at the earliest proven safe point and record that choice in this plan. If either field is missing when persistence is enabled, emit a visible session-unavailable fact or warning and keep the current step behavior according to existing failure policy; recovery for that continuation-capable attempt must later park because no session file is available. If persistence is disabled, do not record a session observation and do not describe later recovery as continuation.

Implement continuation execution in `src/scherzo/orchestrator/daemon.gleam` or the module that consumes `RecoveryPlan`. The executor receives `StepPiContinuationRequest`, records `StepAttemptContinuationStarted`, launches pi once in `workspace_path`, validates `get_state`, sends the recovery prompt, and finishes the same step attempt through the normal artifact and step-finished path. If launch, validation, timeout, or RPC startup fails, append `IssueParkedV2` with reason `recovery_pi_resume_validation_failed` and do not send a prompt.

Use this default recovery prompt when config does not override it:

    You are being resumed by Scherzo after an interrupted workflow agent step.
    Continue from the existing pi session context that was reopened for this step.
    Do not restart from scratch and do not assume the original prompt has been resent.
    Work in the current directory, which is the recorded workspace for this step.
    If the prior session context shows the step was already completed, summarize the completed work and stop.
    Otherwise, inspect the current workspace as needed, finish the same step, and provide a concise final response for Scherzo.

When rendering a configured recovery prompt, support only safe metadata placeholders from `StepAttemptContext`, such as `run_id`, `workflow_id`, `step_id`, `workspace_name`, and `attempt_index`. Do not include the original prompt as an available placeholder. If the configured recovery prompt fails to render, park with a specific recovery-render reason if one exists, otherwise `recovery_pi_resume_validation_failed`; do not use the original prompt as a fallback.

Update `test/fixtures/fake_pi_rpc.sh`. It must record launch argv and cwd when `FAKE_PI_ARGV_LOG` is set. It must return `sessionFile` from `get_state` when either `FAKE_PI_SESSION_FILE` is set or the process was launched with `--session <value>`. It must return `sessionId` from `FAKE_PI_SESSION_ID` when set, otherwise keep `fake-session`. It must support mismatch and failure modes for resume-validation tests, such as `FAKE_PI_SESSION_FILE_MISMATCH`, `FAKE_PI_CWD_MISMATCH`, or `FAKE_PI_GET_STATE_FAIL`. It must keep existing transcript behavior so prompt tests continue to inspect JSON RPC input lines.

Update privacy and documentation surfaces. In `src/scherzo/state/record.gleam`, ensure `redact_excerpts` or a new redaction helper replaces `session_file` and sensitive local workspace path values with placeholders before diagnostic display. In `src/scherzo/state/recovery.gleam` and `src/scherzo/orchestrator/daemon.gleam`, ensure warnings and park messages include reason codes and attempt ids but not raw session paths or transcript contents. In `src/scherzo/session/json.gleam`, do not add `session_file` to summaries or events; only `pi_session_id` may appear. Update `README.md` and `examples/scherzo.yaml` with a minimal opt-in config that uses `pi.argv: ["pi", "--mode", "rpc"]`, explains that `pi.command` remains the default for non-persistent runs, and warns that session files are local sensitive data.

## Concrete Steps

1. From the repository root, run `jj status --color=never`. Expect no unrelated changes before starting implementation.

2. Add or complete `start_argv(executable, args, cwd, env)` in `src/scherzo/port.gleam` and `src/scherzo_port_ffi.erl`. Keep `start(command, cwd)` unchanged.

3. Add a small fake process test, for example in `test/port_argv_test.gleam` or `test/pi_client_test.gleam`, proving an argument with spaces is received as one argv item and cwd is the requested repository-relative scratch directory.

4. Add `test/real_pi_session_validation_test.gleam` as a gated real-pi feasibility test. It must skip unless `SCHERZO_REAL_PI_VALIDATION=1` is set and print a clear skip message such as `real pi session validation skipped; set SCHERZO_REAL_PI_VALIDATION=1 to run`.

5. In the gated test, create a scratch workspace under `test/tmp/real-pi-session-validation`, launch real pi with structured argv equivalent to `pi --mode rpc`, call startup RPCs and `get_state`, send a first prompt containing a unique token, and call `get_state` again if the first response had no `sessionFile`.

6. In the same gated test, terminate the first pi process, relaunch real pi with the same argv plus `--session` and the captured session file as separate argv items, call `get_state`, assert the reopened session file is compatible, assert cwd is the scratch workspace when pi reports cwd, and send a recovery prompt that does not include the unique token. Record whether semantic token recall worked, but make protocol-level reopen the deterministic assertion.

7. Run the gated validation explicitly from the repository root with `SCHERZO_REAL_PI_VALIDATION=1 direnv exec . gleam test`. If credentials, rate limits, or local pi availability prevent running it, do not claim the implementation complete. If real pi does not return a usable `sessionFile` or cannot reopen it, stop and revise this plan before continuing.

8. Commit Milestone 0 after the structured argv primitive, fake argv test, and real-pi feasibility evidence are complete. Suggested commit message: `Spike real pi session reopen behavior`.

9. Add `src/scherzo/workflow_attempt.gleam` with `StepAttemptContext`, including `continuation_capable`, `PiSessionObservation`, `AgentPromptMode`, and a collision-safe step attempt key helper that does not include capability in the key.

10. In `src/scherzo/workflow_attempt.gleam`, add issue and workflow fingerprint helpers. Reuse or factor the current `IssueParkedV2` issue-fingerprint logic so park release and step recovery compare the same issue fingerprint.

11. Add unit tests in `test/workflow_attempt_test.gleam` proving the key helper does not collide when ids contain delimiter-like characters, `attempt_index` changes the key, and workflow fingerprint changes when a prompt, workspace name, model setting, or dependency changes.

12. In `src/scherzo/state/record.gleam`, add `StepAttemptStarted`, `StepAttemptFinished`, `StepAttemptInterrupted`, `StepAttemptContinuationStarted`, and `StepAttemptPiSessionRecorded` record bodies, JSON kinds, encode entries, decode fields, and redaction behavior. `StepAttemptStarted` must encode and decode `continuation_capable`.

13. Add record tests in `test/state_record_test.gleam`. Assert each new record round-trips through JSON and uses snake-case fields. Assert redacted display or redaction helper replaces `session_file` and sensitive workspace paths with placeholders.

14. In `src/scherzo/state/projection.gleam`, add `step_attempts` and `step_pi_sessions` projection fields and apply the new record bodies.

15. Add projection tests in `test/state_projection_test.gleam`. Assert two attempts with the same issue and run but different `step_id` or `attempt_index` keep distinct statuses, distinct `continuation_capable` values, and distinct session files. Assert `StepAttemptFinished` removes or supersedes the interrupted/running candidate without losing the persisted start capability in historical record rendering.

16. Extend `src/scherzo/state/recovery.gleam` with `StepPiContinuationRequest` and `RecoveryPlan.pi_session_continuations`. Add static recovery planning that reads the recorded `continuation_capable` field before considering session facts. Add park reasons `recovery_session_persistence_disabled`, `recovery_issue_drift`, `recovery_workflow_drift`, `recovery_workspace_missing`, `recovery_workspace_unsafe`, `recovery_session_file_missing`, `recovery_session_fact_missing`, and `recovery_session_fact_ambiguous` for continuation-capable attempts.

17. Add recovery tests in `test/state_recovery_test.gleam` for one success continuation request and one case for each static park reason. Add tests that a `continuation_capable: False` attempt does not become a continuation candidate after config is enabled later, and that a `continuation_capable: True` attempt parks when current config disables session persistence. Assert continuation requests do not contain the original prompt.

18. Run `direnv exec . gleam test`. Commit Milestone 1 after step-attempt record, projection, and static recovery tests pass. Suggested commit message: `Add durable workflow step attempt recovery state`.

19. In `src/scherzo/config/types.gleam`, add `PiArgvCommand` and `PiSessionPersistenceConfig`, then extend `PiConfig` with `argv_command` and `session_persistence`.

20. In `src/scherzo/config.gleam`, extend the default pi config with `argv_command: None` and `session_persistence.enabled: False`. Add the default recovery prompt string in one function so tests can assert it without duplicating text.

21. In `src/scherzo/config.gleam`, parse `pi.argv`, `pi.argv_env`, and `pi.session_persistence`. Add validation errors for missing argv, empty executable, and forbidden `--session` or `--no-session` when persistence is enabled.

22. Add config tests in `test/config_test.gleam`: disabled persistence preserves the current default command; enabled persistence with `argv: ["pi", "--mode", "rpc"]` succeeds; enabled persistence without argv fails; enabled persistence with `--no-session` fails; enabled persistence with `--session` fails; invalid errors start with `pi.session_persistence`.

23. Add `src/scherzo/pi/command.gleam` with `LaunchSpec`, `LaunchMode`, and `build_launch`. Add focused tests in `test/pi_command_test.gleam` for fresh shell, fresh persistent argv, continuation argv, empty session file rejection, forbidden flag validation, and a session file containing spaces remaining one argv item.

24. Update `src/scherzo/pi/protocol.gleam` to add `session_file` and `cwd` fields to `RpcRecord` and to decode `data.sessionFile` and `data.cwd`. Add tests in `test/pi_rpc_test.gleam` or `test/pi_client_test.gleam` that decode a `get_state` response with both fields and a response missing both fields.

25. Update `src/scherzo/pi/client.gleam` so new launch paths accept `LaunchSpec` and `Session` stores `session_file`. Keep a compatibility wrapper for callers that still launch from the legacy shell command while persistence is disabled.

26. Extend `test/fixtures/fake_pi_rpc.sh` with argv logging, cwd logging, configurable `sessionFile`, continuation launch detection, and get-state failure/mismatch modes.

27. Add pi client tests in `test/pi_client_test.gleam` that launch fake pi through structured argv from a repository-relative scratch cwd, assert the argv log contains each item separately, assert cwd equals the scratch workspace, and assert the client stores `session_file`.

28. Run `direnv exec . gleam test`. Commit Milestone 2 after config, command construction, protocol, port, fake-pi, and pi-client tests pass. Suggested commit message: `Add structured pi launch support for session persistence`.

29. Extend `src/scherzo/workflow_run.gleam` so `Dependencies.agent_step` receives `StepAttemptContext`, `AgentPromptMode`, and a `PiSessionObservation` callback. Compute `StepAttemptContext` from `run_id`, the current issue, workflow id, workflow fingerprint, step id, prepared workspace name, attempt index `1`, `workspace.path`, and the fresh attempt's validated `pi.session_persistence.enabled` value as `continuation_capable`.

30. Update `src/scherzo/orchestrator/daemon.gleam` or the workflow-run call site to append `StepAttemptStarted` with `continuation_capable` before each step is executed, `StepAttemptFinished` after success or failure, and `StepAttemptInterrupted` for running step attempts when daemon recovery observes a crash or restart before finish.

31. In `src/scherzo/agent/run_attempt.gleam`, record `PiSessionObservation` at the capture point proven by Milestone 0. If persistence is disabled, do not record a session observation; the persisted `continuation_capable: False` start fact is why recovery must not treat the missing session fact as recoverable continuation state.

32. Add tests in `test/agent_runner_test.gleam` or `test/workflow_run_test.gleam` proving a fake-pi fresh run records the exact attempt identity and session fields at the required ordering point. If Milestone 0 proved capture before first prompt, assert the observation callback fires before the prompt line appears in `FAKE_PI_TRANSCRIPT`.

33. Run `direnv exec . gleam test`. Commit Milestone 3 after session record, projection, and fresh-run recording tests pass. Suggested commit message: `Record pi session files per workflow step attempt`.

34. Add recovery workspace validation in `src/scherzo/workspace_run.gleam`. Tests must cover a valid recorded workspace, a missing recorded workspace, the workspace root itself, and a path derived at runtime that resolves outside the configured workspace root.

35. Ensure `src/scherzo/state/recovery.gleam` uses the recorded `continuation_capable` field, the current config kill switch, workspace validation, and session-file existence checks before adding `StepPiContinuationRequest`. The recovery planner must not launch pi.

36. Add or update `test/state_recovery_test.gleam` cases to assert non-capable attempts do not create continuation requests, current disabled persistence parks capable attempts with `recovery_session_persistence_disabled`, and workspace and session-file failures park with specific reasons and no continuation request.

37. Run `direnv exec . gleam test`. Commit Milestone 4 after static recovery planning tests pass. Suggested commit message: `Plan safe step-scoped pi session recovery`.

38. Implement continuation execution in `src/scherzo/orchestrator/daemon.gleam` or the recovery executor. The action must carry only the recorded attempt identity, workspace path, session id, session file, and recovery prompt. It must not include the original prompt.

39. In `src/scherzo/agent/run_attempt.gleam` or a small `src/scherzo/pi/reopen.gleam` helper, implement single-process reopen validation: build `ContinueSession(recorded_session_file)`, launch pi in `recorded_workspace_path`, call startup RPCs and `get_state`, verify returned `session_id` is non-empty, returned `sessionFile` is compatible with the recorded session file, and returned cwd is compatible with the recorded workspace when provided. Return a live `Session` on success.

40. Use the live validated `Session` to send the recovery prompt. If reopen validation fails, terminate the process if possible, append a park record with `recovery_pi_resume_validation_failed`, and assert no prompt was sent.

41. Add a fake-pi workflow recovery test, likely in `test/workflow_run_test.gleam` or a new `test/workflow_recovery_pi_session_test.gleam`. Arrange an interrupted agent step with original prompt text that contains `ORIGINAL_PROMPT_SHOULD_NOT_APPEAR`, a recorded session file, and a valid workspace. Execute the continuation. Assert the argv log has `--session` followed by the recorded session file, cwd is the recorded workspace, the transcript has exactly one prompt command, the prompt contains the recovery prompt marker, and the prompt does not contain `ORIGINAL_PROMPT_SHOULD_NOT_APPEAR`.

42. Add a fake-pi resume-validation failure test. Configure fake pi to report a mismatched session file or fail `get_state`. Assert Scherzo parks with `recovery_pi_resume_validation_failed` and the transcript contains no prompt command.

43. Add a second fake-pi recovery test with two interrupted attempts for the same issue but different `step_id` or `attempt_index`. Assert recovery chooses the session fact for the exact requested attempt and does not use the other session file.

44. Run `direnv exec . gleam test`. Commit Milestone 5 after continuation execution tests pass. Suggested commit message: `Resume interrupted agent steps with recorded pi sessions`.

45. Update `README.md` and `examples/scherzo.yaml` with the opt-in config example. The minimal documented config should use `pi.argv: ["pi", "--mode", "rpc"]` and `pi.session_persistence.enabled: true`; it must not include `--no-session` or an explicit `--session`.

46. Add documentation/config tests if the repository has snapshot or example validation tests. At minimum, add a test that parses the documented `examples/scherzo.yaml` shape or a fixture copied from the docs and asserts session persistence validates.

47. Add privacy tests for operator-facing surfaces: `test/state_record_test.gleam` for redacted ledger display, `test/state_recovery_test.gleam` for warnings without raw session paths, and `test/session_json_test.gleam` or equivalent for summaries/events that do not include `session_file`.

48. Run formatting from the repository root: `direnv exec . gleam format --check src test`. Expect no formatting changes required. If it reports formatting differences, run the formatter according to repository conventions, inspect the diff, and rerun the check.

49. Run the full test suite from the repository root: `direnv exec . gleam test`. Expect all tests to pass with zero failures.

50. Run the gated real-pi validation again with `SCHERZO_REAL_PI_VALIDATION=1 direnv exec . gleam test`. Confirm it still passes through the production structured launch and continuation path, not only the early spike path.

51. Update this ExecPlan's Progress, Surprises & Discoveries, Outcomes & Retrospective, and Artifacts and Notes with concise validation evidence. Do not paste full pi transcripts, session file contents, local machine-specific paths, secrets, Linear issue contents, or private source snippets.

52. Commit the final milestone after fake tests, formatting, full tests, docs/config tests, privacy tests, and real-pi validation pass. Suggested commit message: `Validate step-scoped pi session continuation`.

## Testing and Falsifiability

This plan is false if real pi cannot produce a usable `sessionFile` or cannot reopen it with `--session` from the recorded workspace. The gated `test/real_pi_session_validation_test.gleam` must run before broad implementation and again at the end. It should first assert protocol-level facts: captured `sessionFile` is non-empty, relaunch with `--session` succeeds, reopened `get_state.sessionFile` matches or is compatible, cwd is correct when reported, and no recovery prompt is sent before reopen validation succeeds. Semantic token recall is useful supporting evidence, but provider behavior, rate limits, or prompt interpretation must not be the only assertion.

This plan is false if Scherzo ever resends the original prompt during a session-enabled continuation. The fake-pi continuation test must make that falsifiable by putting a unique sentinel in the original prompt and asserting no JSON prompt sent to fake pi contains that sentinel. The recovery request type should also make this structurally hard by not carrying the original prompt.

This plan is false if Scherzo appends `--session` to a shell command string. The command-construction tests must instantiate a config with only `pi.command` and `session_persistence.enabled: true` and assert validation fails. Continuation launch tests must inspect fake-pi argv and prove `--session` and the session file are separate argv items.

This plan is false if sessions are issue-scoped instead of attempt-scoped. Projection tests must record two sessions with the same issue and run but different step ids or attempt indexes and assert both survive under distinct keys. Workflow recovery tests must select the exact matching session.

This plan is false if recovery infers continuation capability from current config or from a missing session fact instead of the persisted start record. State recovery tests must record an attempt with `continuation_capable: False`, enable persistence later, and assert no continuation request is produced. They must also record an attempt with `continuation_capable: True`, disable persistence later, and assert `recovery_session_persistence_disabled`, plus record a capable attempt with no session fact and assert `recovery_session_fact_missing` instead of a fresh restart.

This plan is false if unsafe recovery cases fall back to fresh execution. Recovery tests must assert that disabled current persistence for a capable attempt, issue drift, workflow drift, missing workspace, unsafe workspace, missing or ambiguous session fact, missing session file, and resume-validation failure produce park records and no fresh original-prompt run.

This plan is false if privacy promises are not verifiable. Redaction tests must prove diagnostic rendering of `StepAttemptPiSessionRecorded` does not include raw session file or sensitive workspace paths. Recovery warning tests must prove warnings contain reason codes and attempt identifiers, not raw session paths or transcript contents. Session summary JSON tests must prove `session_file` is not present.

Concrete tests to add or modify:

- `test/real_pi_session_validation_test.gleam`: gated real-pi feasibility and final validation, skipped unless explicitly enabled.
- `test/workflow_attempt_test.gleam`: attempt key, `continuation_capable` metadata, and issue/workflow fingerprint behavior.
- `test/config_test.gleam`: config defaults and validation for `pi.argv` and `pi.session_persistence`, including error prefixes.
- `test/pi_command_test.gleam`: launch-spec construction for fresh shell, fresh persistent argv, continuation argv, empty session file, forbidden flags, and session file argv item boundaries.
- `test/pi_rpc_test.gleam` or `test/pi_client_test.gleam`: protocol decoding for `sessionFile` and `cwd`.
- `test/pi_client_test.gleam`: structured argv launch against fake pi, cwd assertion, session file capture, and resume-validation success/failure.
- `test/state_record_test.gleam`: JSON round-trip for all step-attempt records, including `continuation_capable`, and redaction for session/workspace paths.
- `test/state_projection_test.gleam`: exact attempt key behavior, persisted capability, and latest-session projection.
- `test/agent_runner_test.gleam`: session observation happens at the Milestone 0-proven capture point for session-enabled fresh agent steps.
- `test/workflow_run_test.gleam` or `test/workflow_recovery_pi_session_test.gleam`: recovery launch args, cwd, recovery prompt, no original prompt resend, no prompt on validation failure, and exact attempt selection.
- `test/state_recovery_test.gleam`: park or continuation decisions for every static recovery safety branch, including persisted capability and current kill-switch behavior, plus warning redaction.
- `test/session_json_test.gleam` or the existing session JSON test file: summaries and events do not expose `session_file`.
- Documentation/config validation tests for the `README.md` or `examples/scherzo.yaml` session-persistence example if the repository has such tests; otherwise add a small config fixture test that mirrors the documented snippet.

Run commands from the repository root:

    direnv exec . gleam format --check src test
    direnv exec . gleam test
    SCHERZO_REAL_PI_VALIDATION=1 direnv exec . gleam test

Expected successful output is the normal Gleam compile and test output with zero failed tests. The exact test count may change as this plan is implemented, so the acceptance criterion is all tests pass, the new tests named above are included in the run, and the gated real-pi validation is either explicitly skipped when not requested or passes when requested. The implementation is not complete until the gated real-pi validation has passed at least once and concise evidence is recorded in this plan.

## Validation and Acceptance

The implementation is accepted only when all of the following are true:

Milestone 0 has recorded real-pi evidence before broad implementation proceeded. The evidence states when `sessionFile` first became available, whether `--session` reopen succeeded, and whether the final implementation still uses that proven capture point.

Every fresh agent step records a `step_attempt_started` fact with `continuation_capable` set from the validated session-persistence config at attempt start. This value is durable attempt history: enabling persistence later does not make a false value true, and disabling persistence later does not erase that the original attempt was capable.

A fresh session-enabled agent step records a `step_attempt_pi_session_recorded` fact with `run_id`, `issue_id`, `issue_identifier`, `workflow_id`, `workflow_fingerprint`, `step_id`, `workspace_name`, `attempt_index`, `workspace_path`, `session_id`, and `session_file`. The fact is attached to the exact step attempt, not merely to the issue.

A recovery candidate is produced only when the recorded `continuation_capable` field is true, current config still permits session persistence, the refreshed issue still matches the recorded issue fingerprint, the selected workflow id and workflow fingerprint still match the recorded attempt, the recorded workspace exists and passes safety checks, the recorded session file exists, and exactly one matching pi session fact exists.

Continuation execution launches pi once with `--session <recorded-session-file>` from the recorded workspace, validates `get_state` on that same process, and sends the recovery prompt only after validation succeeds.

If an interrupted agent attempt was recorded with `continuation_capable: False`, no true continuation candidate is produced; any later non-session recovery is a retry or restart, not continuation. If current persistence config is disabled for a capable attempt, if workflow or issue drift is detected, if the recorded workspace is missing or unsafe, if the session file is missing, if the session fact is missing or ambiguous, or if resume validation fails, recovery parks with a specific reason and does not launch a fresh pi run with the original prompt.

Command construction for continuation uses structured argv. Scherzo does not append flags to `pi.command` shell strings. Session-enabled config rejects argv values that already contain `--session` or `--no-session`.

Fake-pi tests prove launch args, cwd, recovery prompt content, absence of original prompt resend, no prompt on resume-validation failure, and attempt-scoped session recording.

The final real-pi validation proves that a captured `sessionFile` can be reopened with `--session` from the recorded workspace through the production structured launch path and that the reopened process has usable prior context at least by protocol-level session continuity, with semantic recall recorded as supporting evidence when available.

`README.md` and `examples/scherzo.yaml` document the minimal opt-in configuration and explain that `pi.command` remains the default path when persistence is disabled.

Privacy tests prove local session file paths and transcript contents are not exposed in diagnostic rendering, recovery warnings, session summaries, Linear comments, or attach/event JSON. Local ledger persistence of the raw path is allowed because it is repository-local recovery state.

## Rollout, Recovery, and Idempotence

Rollout is opt-in. The default `pi.session_persistence.enabled` is false, so existing operators continue to use `pi.command` and the existing `--no-session` default. Those fresh attempts record `continuation_capable: False`; enabling persistence later cannot turn those historical attempts into true continuation candidates. Enabling persistence requires structured `pi.argv`; invalid configs fail at startup with messages beginning `pi.session_persistence` rather than failing during recovery.

The change is additive to the ledger. New step-attempt and `step_attempt_pi_session_recorded` records do not require migrating old issue-level records. If the feature is disabled after records exist, those records remain harmless historical facts. Fresh non-persistent runs record `continuation_capable: False`, and recovery of already-capable interrupted attempts treats the disabled config as a kill switch that parks instead of opening the recorded session.

Recovery planning must be idempotent. If the planner has already parked an interrupted attempt for a specific recovery reason and issue fingerprint, rerunning recovery should not append duplicate park records for the same attempt and same issue fingerprint. If the planner has already emitted or started a continuation for an attempt, rerunning recovery should not start a second concurrent continuation. Add a dedupe key derived from the exact step attempt identity and recovery generation if the current recovery plan does not already have one.

Continuation execution is single-process. If reopen validation succeeds, Scherzo sends the recovery prompt on that process. If validation fails, Scherzo terminates the process if possible, parks with `recovery_pi_resume_validation_failed`, and records no step finish. A later recovery pass should see the park record and not start a duplicate continuation unless an operator explicitly unparks or reruns.

Rollback is straightforward. Disable `pi.session_persistence.enabled` in config and restart Scherzo. Fresh workflow runs return to the current non-persistent shell command behavior and record `continuation_capable: False`. Interrupted session-enabled attempts that cannot or should not be continued while the kill switch is off should remain parked with `recovery_session_persistence_disabled` or another specific reason; operators can explicitly decide whether to re-enable persistence, unpark, or rerun.

## Artifacts and Notes

A successful fake-pi continuation argv log should look like this, with repository-relative placeholders rather than local machine paths:

    cwd=<recorded-workspace-path>
    argv[0]=test/fixtures/fake_pi_rpc.sh
    argv[1]=--mode
    argv[2]=rpc
    argv[3]=--session
    argv[4]=<recorded-session-file>

A successful fake-pi recovery transcript should have one prompt command whose message starts with the recovery prompt marker. It must not contain the sentinel from the original prompt:

    prompt_count=1
    recovery_prompt_seen=true
    original_prompt_sentinel_seen=false

A failed resume-validation transcript should have no prompt commands:

    resume_validation=failed
    prompt_count=0
    park_reason=recovery_pi_resume_validation_failed

Fake-pi validation on 2026-05-05 covered structured argv and cwd logging, continuation `--session` argument boundaries, recovery prompt delivery, original-prompt absence, and no prompt on resume-validation failure. The argv log shape was:

    cwd=<recorded-workspace-path>
    argv[0]=test/fixtures/fake_pi_rpc.sh
    argv[1]=--mode
    argv[2]=rpc
    argv[3]=--session
    argv[4]=<recorded-session-file>

Real-pi validation on 2026-05-05 used the production structured command shape `pi --mode rpc` for the first process and `pi --mode rpc --session <captured-session-file>` for reopen. The first process reported a non-empty `sessionFile`; the reopened process reported compatible state from the same workspace and accepted a recovery prompt. Validation command:

    SCHERZO_REAL_PI_VALIDATION=1 direnv exec . gleam test

The command completed with 749 tests and no failures. No full transcript, session file contents, local path, secret, or Linear issue data is included here.

## Interfaces and Dependencies

In `src/scherzo/workflow_attempt.gleam`, define the attempt and session observation types:

    pub type StepAttemptContext {
      StepAttemptContext(
        run_id: String,
        issue_id: String,
        issue_identifier: String,
        workflow_id: String,
        workflow_fingerprint: String,
        step_id: String,
        workspace_name: String,
        attempt_index: Int,
        workspace_path: String,
        continuation_capable: Bool,
      )
    }

    pub type AgentPromptMode {
      OriginalPrompt(String)
      RecoveryPrompt(String)
    }

    pub type PiSessionObservation {
      PiSessionObservation(
        run_id: String,
        issue_id: String,
        issue_identifier: String,
        workflow_id: String,
        workflow_fingerprint: String,
        step_id: String,
        workspace_name: String,
        attempt_index: Int,
        workspace_path: String,
        session_id: String,
        session_file: String,
      )
    }

In `src/scherzo/config/types.gleam`, the end state must include:

    pub type PiArgvCommand {
      PiArgvCommand(
        executable: String,
        args: List(String),
        env: List(#(String, String)),
      )
    }

    pub type PiSessionPersistenceConfig {
      PiSessionPersistenceConfig(
        enabled: Bool,
        recovery_prompt: String,
      )
    }

    pub type PiConfig {
      PiConfig(
        command: String,
        argv_command: Option(PiArgvCommand),
        turn_timeout_ms: Int,
        read_timeout_ms: Int,
        stall_timeout_ms: Int,
        auto_retry: Bool,
        ui_request_policy: UiRequestPolicy,
        ui_request_timeout_ms: Int,
        compatibility_probe: Bool,
        rate_limit_payload: Option(String),
        session_persistence: PiSessionPersistenceConfig,
      )
    }

If adding fields in the middle of `PiConfig` causes excessive churn, place `argv_command` and `session_persistence` at the end. The behavior is more important than field order, but tests must be updated consistently.

In `src/scherzo/pi/command.gleam`, define:

    pub type LaunchSpec {
      ShellLaunch(command: String)
      ArgvLaunch(
        executable: String,
        args: List(String),
        env: List(#(String, String)),
      )
    }

    pub type LaunchMode {
      FreshNoSession
      FreshPersistent
      ContinueSession(session_file: String)
    }

    pub fn build_launch(
      pi: config_types.PiConfig,
      mode: LaunchMode,
    ) -> Result(LaunchSpec, error.ConfigError)

In `src/scherzo/port.gleam`, add:

    pub fn start_argv(
      executable: String,
      args: List(String),
      cwd: String,
      env: List(#(String, String)),
    ) -> Result(Port, error.PortError)

Use the existing port type and error conventions if their names differ; the required behavior is separate argv items and cwd support without caller-provided shell concatenation.

In `src/scherzo/pi/protocol.gleam`, extend `RpcRecord` with:

    session_file: Option(String)
    cwd: Option(String)

In `src/scherzo/pi/client.gleam`, extend `Session` with:

    session_file: Option(String)

and add launch/reopen functions equivalent to:

    pub fn launch_spec(
      spec: pi_command.LaunchSpec,
      cwd: String,
      session_name: String,
      auto_retry: Bool,
      read_timeout_ms: Int,
    ) -> Result(Session, error.PiRpcError)

    pub fn reopen_session_for_continuation(
      spec: pi_command.LaunchSpec,
      cwd: String,
      expected_session_file: String,
      read_timeout_ms: Int,
    ) -> Result(Session, error.PiRpcError)

`reopen_session_for_continuation` returns a live validated `Session`. The caller sends the recovery prompt on that same session. Do not implement it as a separate preflight that terminates before the real continuation.

In `src/scherzo/state/record.gleam`, add ledger bodies equivalent to:

    StepAttemptStarted(
      run_id: String,
      issue_id: String,
      issue_identifier: String,
      workflow_id: String,
      workflow_fingerprint: String,
      step_id: String,
      workspace_name: String,
      attempt_index: Int,
      workspace_path: String,
      issue_fingerprint: String,
      step_kind: String,
      prompt_mode: String,
      continuation_capable: Bool,
    )

    StepAttemptFinished(
      run_id: String,
      issue_id: String,
      workflow_id: String,
      step_id: String,
      workspace_name: String,
      attempt_index: Int,
      status: String,
    )

    StepAttemptInterrupted(
      run_id: String,
      issue_id: String,
      workflow_id: String,
      step_id: String,
      workspace_name: String,
      attempt_index: Int,
      reason: String,
    )

    StepAttemptContinuationStarted(
      run_id: String,
      issue_id: String,
      issue_identifier: String,
      workflow_id: String,
      workflow_fingerprint: String,
      step_id: String,
      workspace_name: String,
      attempt_index: Int,
      workspace_path: String,
      issue_fingerprint: String,
      session_id: String,
    )

    StepAttemptPiSessionRecorded(
      run_id: String,
      issue_id: String,
      issue_identifier: String,
      workflow_id: String,
      workflow_fingerprint: String,
      step_id: String,
      workspace_name: String,
      attempt_index: Int,
      workspace_path: String,
      session_id: String,
      session_file: String,
    )

In `src/scherzo/state/recovery.gleam`, add:

    pub type StepPiContinuationRequest {
      StepPiContinuationRequest(
        run_id: String,
        issue_id: String,
        issue_identifier: String,
        workflow_id: String,
        workflow_fingerprint: String,
        step_id: String,
        workspace_name: String,
        attempt_index: Int,
        workspace_path: String,
        session_id: String,
        session_file: String,
        recovery_prompt: String,
      )
    }

and extend `RecoveryPlan` with:

    pi_session_continuations: List(StepPiContinuationRequest)

This request must not include the original prompt.

In `src/scherzo/workspace_run.gleam`, add a helper equivalent to:

    pub fn validate_recorded_step_workspace(
      recorded_path: String,
      issue: tracker_issue.Issue,
      workflow_id: String,
      run_id: String,
      workspace_name: String,
      orchestrator: config_types.OrchestratorConfig,
    ) -> Result(String, error.WorkspaceError)

In `src/scherzo/workflow_run.gleam`, extend `Dependencies.agent_step` so it receives `workflow_attempt.StepAttemptContext`, `workflow_attempt.AgentPromptMode`, and a session observation callback. Fresh execution passes `OriginalPrompt(rendered_prompt)`. Continuation execution passes `RecoveryPrompt(recovery_prompt)` from a `StepPiContinuationRequest` and never carries the original prompt.

## Open Questions and Clarifications Needed

None.
