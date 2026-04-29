# Add an extensible doctor command for readiness checks

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

After this change, an operator can run one command, `direnv exec . gleam run -- doctor path/to/WORKFLOW.md`, to check whether a Scherzo workflow is ready for cautious real-board operation. The command prints one stable result per named readiness check, exits nonzero when any selected check fails, and keeps the existing one-off checks available for focused debugging. The initial checks cover workflow/config validation, local instance-lock availability, read-only Linear board contract validation, read-only Linear smoke, workspace hook validation, and no-prompt pi RPC probing.

The visible behavior is a per-check transcript such as `doctor_check_pass check=workflow-config`, `doctor_check_fail check=linear-smoke code=linear_api_status`, and a final `doctor_summary passed=5 failed=1 skipped=1 warned=0`. Operators can also run a subset, for example `direnv exec . gleam run -- doctor --check linear-smoke --check pi-probe examples/WORKFLOW.md`, without losing the detailed output those checks currently provide through `--linear-smoke`, `--linear-contract-check`, and `--pi-probe`.

This plan also cleans up the existing documentation trail. At plan-authoring time, `docs/TODO.md` does not contain a doctor-command item; the outstanding doctor item is in `docs/plans/real-board-readiness.md`. During implementation, search `docs/TODO.md` again and remove any stale doctor-command TODO if one has been added, and mark the `real-board-readiness` doctor progress item complete after the command is implemented and validated.

## Problem Framing and Constraints

Scherzo now has several separate readiness surfaces. `--linear-smoke` proves the Linear issue-read path, `--linear-contract-check` proves configured Linear states and labels exist on the board, and `--pi-probe` prepares a scratch workspace and launches pi RPC without sending a prompt. These checks are useful, but a new operator has to remember which ones to run, in which order, and how to interpret partial success. The current `README.md` operational rollout still lists separate commands. The unchecked doctor item in `docs/plans/real-board-readiness.md` exists because those one-off checks should be gathered behind an extensible readiness command before more checks are added.

The first implementation must not dispatch real work, claim Linear issues, post Linear comments, update Linear issue state, send a pi `prompt`, start daemon mode, or require a running local control server. It may perform the same bounded, explicit side effects as the existing probe commands: read workflow files, query Linear read-only metadata/issues, acquire and release the local instance lock, prepare and remove a scratch workspace, run trusted workspace hooks against that scratch workspace, and launch pi RPC commands that do not send a task prompt.

The command must keep checks independently observable. A combined command that simply calls existing service modes and stops at the first error would be less useful than the current separate commands. Doctor should report every selected check whose prerequisites are satisfied, and it should report skipped checks when an earlier prerequisite failed.

## Strategy Overview

Add a small doctor layer rather than replacing the existing service modes. A new module, `src/scherzo/doctor.gleam`, will define stable check names, check statuses, check results, dependency injection for tests, report formatting, and the ordered execution rules. A new service entry point in `src/scherzo/orchestrator/service.gleam` will load and resolve the workflow once, run the selected doctor checks with real dependencies, log one structured line per check, log a summary, and return `Error(StartupError("doctor_failed", ...))` when any selected check fails.

Add a new CLI subcommand rather than another top-level flag. The command shape is `gleam run -- doctor [options] [path-to-WORKFLOW.md]`. Supported options are `--check <name>` repeated to select checks and `--list-checks` to print available check names. With no `--check`, doctor runs the default readiness set in this order: `workflow-config`, `linear-contract`, `linear-smoke`, `instance-lock`, `workspace-hooks`, and `pi-probe`. The order is chosen to fail cheap and read-only checks before local lock and scratch-workspace checks. Existing `--linear-smoke`, `--linear-contract-check`, and `--pi-probe` remain unchanged for focused use and backward compatibility.

The workspace and pi checks share one scratch workspace in a doctor run. When either `workspace-hooks` or `pi-probe` is selected, doctor acquires the instance lock once, prepares a workspace named `SCHERZO-DOCTOR`, reports the workspace hook result, optionally runs the no-prompt pi probe in that prepared workspace, and then cleans up the scratch workspace and releases the lock. This avoids cloning or preparing the same repository twice while still reporting the workspace and pi checks separately. Cleanup failure is reported as a warning result named `workspace-cleanup` and included in the summary, but it does not hide a successful no-prompt pi probe.

## Alternatives Considered

One alternative is to make `--linear-smoke` call the other checks. That would overload a mode whose name promises a Linear issue-read smoke test. It would also make it harder to request a subset or add non-Linear checks later.

Another alternative is to add a top-level `--doctor` flag with only an optional workflow path. That matches the existing mode style, but it leaves no clean space for named checks. A `doctor` subcommand with options is a better long-term shape and mirrors the existing `ctl` subcommand pattern.

Another alternative is to have doctor call `service.start_linear_smoke`, `service.start_linear_contract_check`, and `service.start_pi_probe` directly. That would be small but wrong for operator output: each function returns only success or one startup error, and `start_pi_probe` prepares and cleans a scratch workspace internally. The new doctor layer should reuse lower-level pure helpers and clients so it can continue after independent failures, share prepared workspace state, and report per-check statuses.

Another alternative is to include local control API health checks in the first implementation. That is deferred. Doctor is for pre-dispatch readiness and should not require a running daemon. A future check can inspect a running daemon through `control.json`, ping the control server, query EventHub health, and report that separately.

## Risks and Countermeasures

The main safety risk is doctor accidentally dispatching work or mutating Linear. Countermeasure: doctor must not call `start_once`, `daemon.start`, handoff clients, Linear command clients, or any worker runner. The only pi operation is the existing probe sequence through `probe.probe`, which sends `set_session_name`, `set_auto_retry`, `get_state`, and `get_session_stats` but no `prompt`. Tests must inspect a fake-pi transcript and assert no `prompt` command appears.

The main usability risk is a failed early check hiding useful independent checks. Countermeasure: represent dependencies explicitly. If workflow loading fails, all workflow-dependent checks are skipped. If config resolves but Linear smoke fails, workspace and pi checks can still run if their prerequisites are satisfied. If the instance lock is unavailable, `workspace-hooks` and `pi-probe` are skipped, while already completed read-only Linear checks remain visible.

The main operational risk is running trusted hooks unexpectedly. Countermeasure: document that default doctor includes the same scratch workspace/hook side effects as `--pi-probe`, and make `--check` available for read-only subsets such as `--check workflow-config --check linear-smoke --check linear-contract`. The check result for `workspace-hooks` must include the scratch workspace path so an operator can inspect cleanup warnings.

The main compatibility risk is breaking current CLI parsing or changing existing readiness commands. Countermeasure: add `doctor` as a new `CliResult` branch, keep the current `RunMode` branches and existing flags unchanged, and extend `test/main_test.gleam` for the new parser cases.

The main code-duplication risk is copying Linear smoke and contract logic into a new module. Countermeasure: doctor should call `smoke.linear_read_smoke`, `linear_contract.check`, `workspace.prepare`, `workspace.cleanup_stored_path`, and `probe.probe` through injectable dependencies. The existing standalone service modes can stay as thin wrappers around their current behavior; they do not need to share all doctor machinery in the first implementation.

The main documentation risk is leaving the old TODO trail in place after implementation. Countermeasure: implementation steps explicitly search `docs/TODO.md` for doctor/readiness aggregation language, remove it if present, and mark the existing doctor progress item in `docs/plans/real-board-readiness.md` complete.

## Progress

- [x] (2026-04-29 00:00Z) Searched documentation for doctor-command references. `docs/TODO.md` has no doctor item in the current tree; `docs/plans/real-board-readiness.md` has one unchecked doctor progress item.
- [x] (2026-04-29 00:00Z) Reviewed current CLI and service surfaces: `src/scherzo/main.gleam` supports daemon, once, `--linear-smoke`, `--linear-contract-check`, `--pi-probe`, and `ctl`, but no `doctor` command.
- [x] (2026-04-29 00:00Z) Reviewed current readiness helpers: `src/scherzo/smoke.gleam`, `src/scherzo/linear_contract.gleam`, `src/scherzo/orchestrator/service.gleam`, `src/scherzo/workspace.gleam`, `src/scherzo/agent/probe.gleam`, and `src/scherzo/instance_lock.gleam`.
- [x] (2026-04-29 00:00Z) Ran baseline validation from the repository root with `direnv exec . gleam test`; it passed with `215 passed, no failures`.
- [ ] Add doctor result types, check selection, summary formatting, and pure tests.
- [ ] Add service-level doctor execution with fake Linear, lock, workspace, and pi dependencies.
- [ ] Add CLI parsing, usage text, and list-checks behavior.
- [ ] Update README, `docs/TODO.md` if needed, `docs/plans/real-board-readiness.md`, and this plan's retrospective after validation.

## Surprises & Discoveries

- Observation: The read-only Linear board contract check is already implemented in the current source tree even though the older plan progress checklist still shows it as pending.
  Evidence: `src/scherzo/main.gleam` has `LinearContractCheck`, `src/scherzo/orchestrator/service.gleam` has `start_linear_contract_check`, and `src/scherzo/linear_contract.gleam` defines contract diagnostics.

- Observation: Graceful SIGTERM lifecycle support is already implemented in the current source tree even though the older hardening plan progress checklist still shows it as pending.
  Evidence: `src/scherzo/orchestrator/service.gleam` imports `scherzo/lifecycle` and `scherzo/signal`, and daemon mode goes through `start_daemon_with_lifecycle` rather than `process.sleep_forever()`.

- Observation: `docs/TODO.md` does not currently contain a doctor-command TODO.
  Evidence: A grep for `doctor`, `readiness checks`, and `one command` found the doctor item only in `docs/plans/real-board-readiness.md`.

## Decision Log

- Decision: Implement doctor as a `doctor` subcommand, not as `--doctor`.
  Rationale: Doctor needs named checks and future options. A subcommand gives an extensible argument shape and follows the existing `ctl` subcommand precedent.
  Date: 2026-04-29

- Decision: Keep existing one-off modes unchanged.
  Rationale: Operators and tests may still want focused `--linear-smoke`, `--linear-contract-check`, or `--pi-probe` runs. Doctor aggregates them; it does not replace their CLI compatibility in this phase.
  Date: 2026-04-29

- Decision: Make the default doctor run include the no-prompt pi probe and workspace hooks.
  Rationale: The command's purpose is full readiness for cautious real-board operation. Operators who want read-only checks can use repeated `--check` options to select only read-only checks.
  Date: 2026-04-29

- Decision: Share one scratch workspace for `workspace-hooks` and `pi-probe` during a doctor run.
  Rationale: Preparing a real repository can be expensive and side-effectful. Sharing the workspace keeps output granular without running hooks twice.
  Date: 2026-04-29

- Decision: Report cleanup failure as a warning result rather than hiding the main check result.
  Rationale: This matches the existing `--pi-probe` posture, where cleanup failure is logged as `pi_probe_cleanup_failed` but the probe result still reflects whether pi RPC worked.
  Date: 2026-04-29

## Outcomes & Retrospective

(To be filled after implementation. Include the final check names, final output examples, final test count, whether `docs/TODO.md` contained anything to remove at implementation time, and whether the real-board-readiness progress item was marked complete.)

## Context and Orientation

Scherzo is a Gleam/Erlang daemon. Runtime source lives under `src/scherzo/`, tests live under `test/`, documentation lives under `README.md`, `examples/WORKFLOW.md`, and `docs/`, and validation runs from the repository root with `direnv exec . gleam test` and `direnv exec . gleam format --check src test`.

The CLI entry point is `src/scherzo/main.gleam`. It currently defines `RunMode` variants `Daemon`, `Once`, `LinearSmoke`, `LinearContractCheck`, and `PiProbe`, plus a separate `Control(List(String))` branch for `ctl`. Its `usage()` text documents the existing modes. Doctor should add a separate CLI branch so `doctor --check ...` options do not have to fit inside the current single-path `RunMode` shape.

The service layer is `src/scherzo/orchestrator/service.gleam`. It already exposes `start_linear_smoke`, `start_linear_contract_check`, and `start_pi_probe`. Linear smoke loads and resolves workflow config, creates `smoke.real_linear_reader`, calls `smoke.linear_read_smoke`, and logs `linear_smoke_ok`. Contract check loads and resolves config, creates `linear.real_contract_client`, calls `linear_contract.check`, logs `linear_contract_ok` or `linear_contract_diagnostic`, and exits nonzero on mismatch. Pi probe validates dispatch hooks, acquires the instance lock, prepares a scratch workspace, launches `probe.probe`, cleans up the workspace, and logs `pi_probe_ok`.

`src/scherzo/smoke.gleam` defines `LinearSmokeReader` and `linear_read_smoke`. `src/scherzo/linear_contract.gleam` defines remote board snapshot types, diagnostic variants, `check`, `is_ok`, `diagnostic_code`, and `format_report`. `src/scherzo/workspace.gleam` defines `prepare` and `cleanup_stored_path`, and `src/scherzo/instance_lock.gleam` defines `acquire` and `release`. `src/scherzo/agent/probe.gleam` performs the no-prompt pi RPC probe.

A doctor check in this plan means a named, independently reported readiness assertion. A passing check means that one assertion succeeded. A failing check means Scherzo should not be considered ready for the selected doctor run. A skipped check means a prerequisite was unavailable or failed, not that the underlying capability is healthy.

## Preconditions and Verified Facts

Before implementing this plan, the current baseline from the repository root is:

    direnv exec . gleam format --check src test
    direnv exec . gleam test
    direnv exec . gleam run -- --help

On 2026-04-29 while writing this plan, `direnv exec . gleam test` ended with `215 passed, no failures`.

Current repository facts this plan depends on:

- `src/scherzo/main.gleam` has no doctor command.
- `src/scherzo/main.gleam` already recognizes `--linear-contract-check` and `--pi-probe`.
- `src/scherzo/orchestrator/service.gleam` has `ContractCheckDependencies` for fake contract-reader tests.
- `test/orchestrator_service_test.gleam` already contains service tests for Linear contract success, mismatch, and fetch error.
- `test/main_test.gleam` asserts parser and usage behavior for existing modes.
- `config.validate_dispatch` currently requires either `hooks.after_create` or `hooks.before_run` before dispatch-like operations.
- `workspace.prepare` runs `hooks.after_create` for newly created workspaces, then `hooks.before_run` when configured.
- `probe.probe` can validate pi RPC without sending a task prompt.
- `docs/TODO.md` currently does not contain a doctor TODO, while `docs/plans/real-board-readiness.md` contains one unchecked doctor progress item.

If any of these facts differ when implementation begins, update this plan first so it remains self-contained.

## Scope Boundaries

In scope: a `doctor` CLI subcommand; stable named check selection; per-check pass/fail/skip/warn results; default readiness check set; workflow/config validation; instance-lock availability check; read-only Linear smoke; read-only Linear contract check; scratch workspace hook validation; no-prompt pi probe using the scratch workspace; structured stderr logs; deterministic tests; README updates; removal of any stale doctor item from `docs/TODO.md` if present; marking the real-board-readiness doctor item complete after implementation.

Out of scope: dispatching Linear issues; daemon startup; local control API health checks; EventHub health checks; web dashboard checks; Linear mutations; Linear command comment processing; handoff comments; durable ledger checks; network retries beyond current readers; deprecating the existing `--linear-smoke`, `--linear-contract-check`, or `--pi-probe` flags; JSON output mode for doctor. A future plan may add `doctor --json` and daemon health checks once the text/log output has stabilized.

## Milestones

Milestone 1 adds the doctor model and pure behavior. At the end, tests can select default checks, parse repeated `--check` names through a helper, format pass/fail/skip/warn results, compute summary counts, and compute the correct exit decision without touching Linear, filesystems, locks, or pi.

Milestone 2 adds service execution with injected dependencies. At the end, service tests can run doctor against fake workflow files, fake Linear smoke readers, fake contract clients, fake lock operations, fake workspace preparation, and fake pi probes. Tests prove that a Linear smoke failure does not hide later independent local checks, that a lock failure skips workspace and pi checks, and that pi probe never sends a prompt.

Milestone 3 wires the CLI. At the end, `src/scherzo/main.gleam` accepts `doctor`, `doctor --check <name>`, and `doctor --list-checks`; usage text documents the command; and parser tests cover valid and invalid doctor arguments.

Milestone 4 updates documentation and validates the full tree. At the end, README operational rollout uses doctor as the primary readiness command while preserving one-off commands for focused debugging, stale TODO documentation is removed or confirmed absent, the real-board-readiness progress item is marked complete, and the deterministic suite passes.

## Plan of Work

Create `src/scherzo/doctor.gleam`. Define check names as a public type equivalent to:

    pub type CheckName {
      WorkflowConfig
      LinearContract
      LinearSmoke
      InstanceLock
      WorkspaceHooks
      PiProbe
    }

Expose `check_name_to_string`, `parse_check_name`, `list_check_names`, and `default_checks`. The stable strings must be `workflow-config`, `linear-contract`, `linear-smoke`, `instance-lock`, `workspace-hooks`, and `pi-probe`.

In the same module, define result and summary types equivalent to:

    pub type CheckStatus { Pass Warn Fail Skip }

    pub type CheckResult {
      CheckResult(
        check: CheckName,
        status: CheckStatus,
        code: String,
        message: String,
        fields: List(#(String, String)),
      )
    }

    pub type Report { Report(results: List(CheckResult)) }

Add helpers `summary(report)`, `has_failures(report)`, `result_event(result)`, and `result_log_fields(result)`. Stable event names should be `doctor_check_pass`, `doctor_check_warn`, `doctor_check_fail`, `doctor_check_skip`, and `doctor_summary`. The summary fields should include `passed`, `warned`, `failed`, and `skipped`.

Add `DoctorOptions` in `src/scherzo/main.gleam` or a small CLI helper module. It should contain `path: Option(String)`, `checks: List(String)`, and `list_checks: Bool`. Keep raw check names in the CLI layer and let the service map unknown names to a usage error or startup error with a clear message. The main parser should support:

    gleam run -- doctor
    gleam run -- doctor examples/WORKFLOW.md
    gleam run -- doctor --check linear-smoke --check pi-probe examples/WORKFLOW.md
    gleam run -- doctor --list-checks

Reject unknown doctor options, missing check names after `--check`, duplicate positional workflow paths, and unknown top-level modes. Duplicated `--check` names should be de-duplicated in first-seen order by the doctor layer so a repeated flag does not run a check twice.

Add `DoctorDependencies` to `src/scherzo/orchestrator/service.gleam`, or put it in `src/scherzo/doctor.gleam` and re-export only what service needs. It should provide injectable functions for Linear smoke reader creation, Linear contract client creation, instance-lock acquire/release, workspace prepare/cleanup, pi probe, and logging. Production dependencies use `smoke.real_linear_reader`, `linear.real_contract_client`, `instance_lock.acquire`, `instance_lock.release`, `workspace.prepare`, `workspace.cleanup_stored_path`, `probe.probe`, and `log_stderr`.

Implement `service.start_doctor(options)` and `service.start_doctor_with_dependencies(options, dependencies)`. The function resolves selected checks first. If `--list-checks` is set, it prints or logs the check list and returns `Ok(Nil)` without loading a workflow. Otherwise it loads the workflow path with `workflow.load`, resolves config with `config.resolve`, and runs the selected checks in the canonical order, not in user-provided order. This keeps output stable and ensures prerequisites are handled consistently.

The `workflow-config` check should load, resolve, and run `config.validate_dispatch`. If the workflow file cannot be loaded or config cannot be resolved, report `workflow-config` as failed and skip all selected checks that require effective config. If `config.validate_dispatch` fails, report `workflow-config` as failed and skip `workspace-hooks` and `pi-probe`, because they depend on dispatch hook configuration. Linear read-only checks should also be skipped when config resolution fails because they need tracker config.

The `linear-contract` check should use the same behavior as `start_linear_contract_check_with_dependencies`: fetch the remote board through a `linear.ContractClient`, call `linear_contract.check`, pass when diagnostics are empty, and fail with one result per contract mismatch or one aggregate failed result plus diagnostic fields. To keep output compact, use one `doctor_check_fail check=linear-contract code=linear_contract_mismatch diagnostic_count=N` plus additional fields such as the first diagnostic code/message. The existing standalone `--linear-contract-check` continues to log every diagnostic.

The `linear-smoke` check should call `smoke.linear_read_smoke` and report `candidate_count`, `terminal_count`, and `refreshed_count` on success. On failure, map the tracker error through existing service error-code helpers so the check result has a stable code such as `linear_api_status`.

The `instance-lock` check should acquire the lock for the lock-required part of the doctor run when `instance-lock`, `workspace-hooks`, or `pi-probe` is selected. If acquiring the lock fails, report `instance-lock` as failed and skip `workspace-hooks` and `pi-probe`. If only `workspace-hooks` or `pi-probe` is selected and `instance-lock` was not explicitly selected, still acquire the lock and report an implicit `instance-lock` result so operators understand why a local probe did or did not run. Release the lock exactly once at the end of all lock-required checks.

The `workspace-hooks` check should prepare a scratch workspace with identifier `SCHERZO-DOCTOR` using `workspace.prepare("SCHERZO-DOCTOR", effective.workspace, effective.hooks)`. On success, report the prepared path and whether the workspace was created and populated. Keep the prepared workspace available for `pi-probe` if that check is also selected. If `workspace-hooks` is not selected but `pi-probe` is selected, still prepare the scratch workspace as a prerequisite and report a skipped or implicit workspace result only if the preparation fails; do not make pi probe appear to have failed because of pi when the real failure is workspace preparation.

The `pi-probe` check should run `probe.probe(effective.pi.command, prepared.path, effective.pi.read_timeout_ms)` against the prepared doctor workspace. On success, report the workspace path. On failure, report the pi RPC error code. The fake-pi test must assert the transcript includes `get_state` and `get_session_stats` and does not include `prompt`.

After workspace and pi checks, cleanup the prepared workspace with `workspace.cleanup_stored_path(effective.workspace.root, prepared.path, effective.hooks)`. If cleanup fails, append a warning result with check name `workspace-hooks` and code `workspace_cleanup_failed`, or add a separate internal warning result if the implementation chooses to add `workspace-cleanup` as a non-selectable check. Keep the summary warning count accurate.

Update `README.md`. In `Development` or `CLI modes`, document `doctor`, list default checks, give read-only subset examples, and say that `--linear-smoke`, `--linear-contract-check`, and `--pi-probe` remain available for focused debugging. In `Operational rollout`, make doctor the first recommended command after deterministic tests, then leave the individual commands as troubleshooting alternatives.

Update documentation cleanup. Search `docs/TODO.md` for `doctor`, `readiness`, and `smoke/probe` aggregation language. If a doctor TODO exists at implementation time, remove it because this plan and implementation supersede it. In the current tree there is no such `docs/TODO.md` entry, so record that no TODO removal was needed if the file is unchanged. In `docs/plans/real-board-readiness.md`, mark the doctor progress item complete after doctor is implemented and validated, and reference `docs/plans/doctor-command.md` in the progress entry.

## Concrete Steps

1. From the repository root, run `direnv exec . gleam test` and record the current pass count in this plan's Progress section. The plan-authoring baseline was `215 passed, no failures`.

2. Create `test/doctor_test.gleam`. Add `default_checks_are_stable_test` asserting `doctor.default_checks()` returns `workflow-config`, `linear-contract`, `linear-smoke`, `instance-lock`, `workspace-hooks`, and `pi-probe` in that order.

3. In `test/doctor_test.gleam`, add `parse_check_name_accepts_known_names_test` and `parse_check_name_rejects_unknown_names_test` for all stable check-name strings.

4. In `test/doctor_test.gleam`, add `summary_counts_result_statuses_test` with one pass, one warn, one fail, and one skip result. Assert the summary counts and `doctor.has_failures(report) == True`.

5. Implement `src/scherzo/doctor.gleam` with the check-name, result, report, and summary helpers until the pure doctor tests pass.

6. Extend `src/scherzo/orchestrator/service.gleam` with a `DoctorDependencies` type and production dependency constructor. Keep existing `Dependencies`, `DaemonLifecycleDependencies`, and `ContractCheckDependencies` unchanged.

7. Add `test/orchestrator_service_doctor_test.gleam`. Start with `doctor_workflow_config_success_test`: write a temporary workflow with a fake workspace root and hooks; run `service.start_doctor_with_dependencies` using fake dependencies; assert a `doctor_check_pass` log for `workflow-config` and a final `doctor_summary`.

8. Add `doctor_unknown_check_name_fails_before_loading_workflow_test`: pass a doctor option containing `--check no-such-check`; assert `StartupError("unknown_doctor_check", _)` or the chosen usage error and no fake workflow load side effects.

9. Add `doctor_linear_smoke_success_reports_counts_test`: fake `smoke.LinearSmokeReader` returns one candidate, zero terminal issues, and one refreshed issue; assert `doctor_check_pass check=linear-smoke candidate_count=1 terminal_count=0 refreshed_count=1`.

10. Add `doctor_linear_smoke_failure_does_not_skip_workspace_probe_test`: fake Linear smoke returns `error.LinearApiStatus(500)` while fake workspace and pi probe succeed. Assert the report contains a failed `linear-smoke` result and passed local probe results, and that the overall service returns `doctor_failed`.

11. Add `doctor_contract_mismatch_reports_failure_test`: fake `linear.ContractClient` returns a board missing the configured active state; assert a failed `linear-contract` result with code `linear_contract_mismatch` and a nonzero summary failure count.

12. Add `doctor_lock_failure_skips_workspace_and_pi_test`: fake lock acquire fails; assert `instance-lock` fails and `workspace-hooks` plus `pi-probe` are skipped without calling fake workspace preparation or fake pi probe.

13. Add `doctor_workspace_and_pi_share_one_prepared_workspace_test`: fake workspace prepare returns path `test/tmp/doctor/workspaces/SCHERZO-DOCTOR`; fake pi probe records that path; assert prepare is called once, pi receives the same path, and cleanup is called once after the probe.

14. Add `doctor_pi_probe_does_not_prompt_test`: use `test/fixtures/fake_pi_rpc.sh` and a real temporary transcript path, run only `--check pi-probe` with fake Linear checks omitted, and assert the transcript contains `get_state` and `get_session_stats` but not `prompt`.

15. Implement `service.start_doctor_with_dependencies` and any private helper functions until the service doctor tests pass. Use existing error-code mapping helpers where possible rather than inventing new strings for existing error classes.

16. Extend `src/scherzo/main.gleam`. Add a `DoctorOptions` type and a `Doctor(DoctorOptions)` branch to `CliResult`, or an equivalent shape. Add parser support for `doctor`, repeated `--check`, `--list-checks`, and one optional workflow path.

17. Update `test/main_test.gleam`. Assert `main.parse_args(["doctor"])`, `main.parse_args(["doctor", "WORKFLOW.md"])`, `main.parse_args(["doctor", "--check", "linear-smoke", "--check", "pi-probe", "WORKFLOW.md"])`, and `main.parse_args(["doctor", "--list-checks"])` return the expected doctor result.

18. Add main parser rejection tests for `doctor --unknown`, `doctor --check`, and `doctor one.md two.md`.

19. Update `main.usage()` and its tests so help text mentions `doctor`, `doctor --check`, the default check list, and the existing individual smoke/probe flags.

20. Wire `main.main` to call `service.start_doctor` for the new doctor branch. For `--list-checks`, either have `main` print the check list directly or have `service.start_doctor` log/print it; test the pure parser and at least one service path.

21. Update `README.md`. Add a `Doctor readiness checks` subsection under CLI modes and revise `Operational rollout` so doctor is the primary readiness command. Include examples for default full doctor and read-only subset doctor.

22. Search `docs/TODO.md` for `doctor`, `readiness`, `linear-smoke`, and `pi-probe`. If a doctor-command TODO exists, remove that bullet. If no such bullet exists, leave `docs/TODO.md` unchanged and record the fact in this plan's Outcomes.

23. Update `docs/plans/real-board-readiness.md`: mark the existing doctor progress item complete with the implementation date and mention that the work is now covered by `docs/plans/doctor-command.md`.

24. Update this plan's Progress, Surprises & Discoveries, Decision Log if any behavior changed, and Outcomes & Retrospective with final validation results.

25. Run `direnv exec . gleam format`.

26. Run `direnv exec . gleam format --check src test` and `direnv exec . gleam test`. Record the final pass count in this plan's Progress and Outcomes.

27. Run `direnv exec . gleam run -- doctor --list-checks` and record the expected output shape in this plan's Artifacts and Notes.

28. Optional credential-gated validation: with `LINEAR_API_KEY` and a private test workflow, run `direnv exec . gleam run -- doctor examples/WORKFLOW.md`. Confirm it prints per-check results, does not dispatch an issue, and fake or real pi transcript contains no prompt.

29. Commit the phase with a message such as `Add doctor readiness command`.

## Testing and Falsifiability

This plan is falsified if doctor can dispatch a worker, post or mutate Linear data, send a pi `prompt`, hide per-check output behind one aggregate error, fail to release the instance lock, fail to clean the scratch workspace on normal completion, or break the existing one-off readiness flags.

Pure tests in `test/doctor_test.gleam` must cover stable check names, parser helpers, result events, summary counts, failure detection, and first-seen de-duplication for repeated check names.

Service tests in `test/orchestrator_service_doctor_test.gleam` must cover successful workflow/config validation, Linear smoke success and failure, Linear contract success and mismatch, lock failure skipping local probes, workspace prepare failure, shared workspace use for workspace and pi checks, cleanup warning behavior, and fake-pi transcript proof that no prompt is sent.

CLI tests in `test/main_test.gleam` must cover doctor argument parsing, invalid doctor options, usage text, and preservation of existing modes.

Run from the repository root:

    direnv exec . gleam format --check src test
    direnv exec . gleam test

Before implementation, tests referencing `scherzo/doctor` and `main.parse_args(["doctor", ...])` should fail. After implementation, the full deterministic suite should pass and current `--linear-smoke`, `--linear-contract-check`, `--pi-probe`, and `ctl` parser tests should still pass.

## Validation and Acceptance

Accept the implementation when all of the following are true:

- `direnv exec . gleam run -- doctor --list-checks` prints or logs the stable check names.
- `direnv exec . gleam run -- doctor path/to/WORKFLOW.md` runs the default checks in stable order and emits one result per check plus `doctor_summary`.
- `direnv exec . gleam run -- doctor --check linear-smoke --check linear-contract path/to/WORKFLOW.md` runs only the selected read-only checks and does not acquire the instance lock.
- `pi-probe` doctor validation uses a scratch workspace and sends no `prompt` command to pi.
- A Linear smoke failure does not suppress independent local probe checks when their prerequisites are satisfied.
- A lock failure skips workspace and pi checks and releases any partially acquired resources.
- Existing standalone readiness flags still behave as before.
- README documents doctor as the primary readiness command.
- Any stale doctor TODO in `docs/TODO.md` is removed, or the retrospective records that none existed.
- The real-board-readiness doctor progress item is marked complete.
- The full deterministic test suite passes.

## Rollout, Recovery, and Idempotence

The doctor command is additive. If it behaves poorly, operators can continue using the existing `--linear-smoke`, `--linear-contract-check`, and `--pi-probe` modes while the issue is fixed. Doctor does not change daemon behavior, scheduler state, Linear issues, or handoff configuration.

Doctor runs are safe to repeat. Repeated read-only checks only query Linear. Repeated local checks may create and remove the same scratch workspace key `SCHERZO-DOCTOR`; `workspace.prepare` already handles existing workspace directories and population markers, and doctor cleanup should remove the scratch workspace at the end of each successful local-probe run. If cleanup fails, doctor reports a warning with the path so the operator can inspect and remove it manually.

Doctor must release the instance lock on every path after a successful acquire. If the process is killed with `kill -9` during doctor, the same stale-lock recovery rule as daemon mode applies: verify no Scherzo process remains active for that workspace root before manually removing `workspace.root/.scherzo-state/instance.lock`.

## Artifacts and Notes

Expected default command shape:

    direnv exec . gleam run -- doctor examples/WORKFLOW.md

Expected structured log shape on all-pass fake dependencies:

    level=info service=scherzo event=doctor_check_pass check=workflow-config code=ok message="workflow config is valid"
    level=info service=scherzo event=doctor_check_pass check=linear-contract code=ok project_slug=TEST team_count=1
    level=info service=scherzo event=doctor_check_pass check=linear-smoke code=ok candidate_count=1 terminal_count=0 refreshed_count=1
    level=info service=scherzo event=doctor_check_pass check=instance-lock code=ok workspace_root=.scherzo/workspaces
    level=info service=scherzo event=doctor_check_pass check=workspace-hooks code=ok workspace_path=.../SCHERZO-DOCTOR
    level=info service=scherzo event=doctor_check_pass check=pi-probe code=ok workspace_path=.../SCHERZO-DOCTOR
    level=info service=scherzo event=doctor_summary passed=6 warned=0 failed=0 skipped=0

Expected check list:

    workflow-config
    linear-contract
    linear-smoke
    instance-lock
    workspace-hooks
    pi-probe

The implementation may choose exact message wording, but check names, event names, status names, and summary field names should remain stable once tests are written.

## Interfaces and Dependencies

In `src/scherzo/doctor.gleam`, define public helpers equivalent to:

    pub type CheckName {
      WorkflowConfig
      LinearContract
      LinearSmoke
      InstanceLock
      WorkspaceHooks
      PiProbe
    }

    pub type CheckStatus { Pass Warn Fail Skip }

    pub type CheckResult {
      CheckResult(
        check: CheckName,
        status: CheckStatus,
        code: String,
        message: String,
        fields: List(#(String, String)),
      )
    }

    pub type Report { Report(results: List(CheckResult)) }

    pub fn default_checks() -> List(CheckName)
    pub fn list_check_names() -> List(String)
    pub fn parse_check_name(String) -> Result(CheckName, String)
    pub fn check_name_to_string(CheckName) -> String
    pub fn summary(Report) -> Summary
    pub fn has_failures(Report) -> Bool
    pub fn result_event(CheckResult) -> String
    pub fn result_log_fields(CheckResult) -> List(log.Field)

In `src/scherzo/main.gleam`, add a doctor CLI result shape equivalent to:

    pub type DoctorOptions {
      DoctorOptions(
        path: Option(String),
        checks: List(String),
        list_checks: Bool,
      )
    }

    pub type CliResult {
      Run(RunMode, Option(String))
      Control(List(String))
      Doctor(DoctorOptions)
      Help
    }

In `src/scherzo/orchestrator/service.gleam`, add production and test entry points equivalent to:

    pub fn start_doctor(options: main.DoctorOptions) -> Result(Nil, StartupError)

    pub fn start_doctor_with_dependencies(
      options: main.DoctorOptions,
      dependencies: DoctorDependencies,
    ) -> Result(doctor.Report, StartupError)

Avoid an import cycle between `main` and `service`. If using `main.DoctorOptions` in `service` would create a cycle, move the options type to `src/scherzo/doctor.gleam` and have `main` construct `doctor.Options` instead.

Production doctor dependencies should use existing modules:

- `workflow.load` and `config.resolve` for workflow/config.
- `config.validate_dispatch` for dispatch prerequisite validation.
- `smoke.real_linear_reader` and `smoke.linear_read_smoke` for read-only Linear smoke.
- `linear.real_contract_client` and `linear_contract.check` for read-only board contract validation.
- `instance_lock.acquire` and `instance_lock.release` for local lock status.
- `workspace.prepare` and `workspace.cleanup_stored_path` for scratch workspace hook validation.
- `probe.probe` for no-prompt pi RPC probing.
- `log_stderr` for structured logs with `config.resolved_secrets(effective)` redaction.
