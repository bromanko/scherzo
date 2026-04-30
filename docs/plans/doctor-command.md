# Add an extensible doctor command for readiness checks

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

After this change, an operator can run one command, `direnv exec . gleam run -- doctor path/to/scherzo.yaml`, to check whether a Scherzo YAML orchestrator config and its routed workflow DAGs are ready for cautious real-board operation. The command prints one stable result per named readiness check, exits nonzero when any selected check fails, and keeps the existing one-off checks available for focused debugging. The initial checks cover orchestrator config/DAG validation, local instance-lock availability, read-only Linear board contract validation, read-only Linear smoke, current workspace hook validation, and no-prompt pi RPC probing.

The visible behavior is a per-check transcript such as `doctor_check_pass check=workflow-config`, `doctor_check_fail check=linear-smoke code=linear_api_status`, and a final `doctor_summary passed=5 failed=1 skipped=1 warned=0`. Operators can also run a subset, for example `direnv exec . gleam run -- doctor --check linear-smoke --check pi-probe examples/scherzo.yaml`, without losing the detailed output those checks currently provide through `--linear-smoke`, `--linear-contract-check`, and `--pi-probe`.

This plan also cleans up the existing documentation trail. At plan-authoring time, `docs/TODO.md` does not contain a doctor-command item; the outstanding doctor item is in `docs/plans/real-board-readiness.md`. During implementation, search `docs/TODO.md` again and remove any stale doctor-command TODO if one has been added, and mark the `real-board-readiness` doctor progress item complete after the command is implemented and validated.

## Problem Framing and Constraints

Scherzo now has several separate readiness surfaces. `--linear-smoke` proves the Linear issue-read path, `--linear-contract-check` proves configured Linear states and labels exist on the board, and `--pi-probe` prepares a scratch workspace and launches pi RPC without sending a prompt. These checks are useful, but a new operator has to remember which ones to run, in which order, and how to interpret partial success. The current `README.md` quick start still lists separate validation commands. The unchecked doctor item in `docs/plans/real-board-readiness.md` exists because those one-off checks should be gathered behind an extensible readiness command before more checks are added.

The first implementation must not dispatch real work, claim Linear issues, post Linear comments, update Linear issue state, send a pi `prompt`, start daemon mode, or require a running local control server. It may perform the same bounded, explicit side effects as the existing probe commands: read the YAML orchestrator config, routed workflow DAG files, and prompt templates; query Linear read-only metadata/issues; acquire and release the local instance lock; prepare and remove a scratch workflow-run workspace; run trusted `workspace.hooks.create`, `workspace.hooks.before_step`, and `workspace.hooks.remove` snippets against that scratch workspace; and launch pi RPC commands that do not send a task prompt.

The command must keep checks independently observable. A combined command that simply calls existing service modes and stops at the first error would be less useful than the current separate commands. Doctor should report every selected check whose prerequisites are satisfied, and it should report skipped checks when an earlier prerequisite failed.

## Strategy Overview

Add a small doctor layer rather than replacing the existing service modes. A new module, `src/scherzo/doctor.gleam`, will define stable check names, check statuses, check results, dependency injection for tests, report formatting, and the ordered execution rules. A new service entry point in `src/scherzo/orchestrator/service.gleam` will load the runtime bundle once with `runtime_bundle.load`, run the selected doctor checks with real dependencies, log one structured line per check, log a summary, and return `Error(StartupError("doctor_failed", ...))` when any selected check fails.

Add a new CLI subcommand rather than another top-level flag. The command shape is `gleam run -- doctor [options] [path-to-scherzo.yaml]`. Supported options are `--check <name>` repeated to select checks and `--list-checks` to print available check names. With no `--check`, doctor runs the default readiness set in this order: `workflow-config`, `linear-contract`, `linear-smoke`, `instance-lock`, `workspace-hooks`, and `pi-probe`. The order is chosen to fail cheap and read-only checks before local lock and scratch-workspace checks. Existing `--linear-smoke`, `--linear-contract-check`, and `--pi-probe` remain unchanged for focused use and backward compatibility.

The workspace and pi checks share one scratch workflow-run workspace in a doctor run. When either `workspace-hooks` or `pi-probe` is selected, doctor acquires the instance lock once, prepares a synthetic step workspace for issue identifier `SCHERZO-DOCTOR` with `workspace_run.prepare_step`, reports the workspace hook result, optionally runs the no-prompt pi probe in that prepared workspace, and then cleans up the scratch run root with `workspace_run.cleanup_run` and releases the lock. This avoids cloning or preparing the same repository twice while still reporting the workspace and pi checks separately. Cleanup failure is reported as a warning result on `workspace-hooks` with code `workspace_cleanup_failed` and included in the summary, but it does not hide a successful no-prompt pi probe.

## Alternatives Considered

One alternative is to make `--linear-smoke` call the other checks. That would overload a mode whose name promises a Linear issue-read smoke test. It would also make it harder to request a subset or add non-Linear checks later.

Another alternative is to add a top-level `--doctor` flag with only an optional YAML config path. That matches the existing mode style, but it leaves no clean space for named checks. A `doctor` subcommand with options is a better long-term shape and mirrors the existing `ctl` subcommand pattern.

Another alternative is to have doctor call `service.start_linear_smoke`, `service.start_linear_contract_check`, and `service.start_pi_probe` directly. That would be small but wrong for operator output: each function returns only success or one startup error, and `start_pi_probe` prepares and cleans a scratch workspace internally. The new doctor layer should reuse lower-level pure helpers and clients so it can continue after independent failures, share prepared workspace state, and report per-check statuses.

Another alternative is to include local control API health checks in the first implementation. That is deferred. Doctor is for pre-dispatch readiness and should not require a running daemon. A future check can inspect a running daemon through `control.json`, ping the control server, query EventHub health, and report that separately.

## Risks and Countermeasures

The main safety risk is doctor accidentally dispatching work or mutating Linear. Countermeasure: doctor must not call `start_once`, `daemon.start`, handoff clients, Linear command clients, or any worker runner. The only pi operation is the existing probe sequence through `probe.probe`, which sends `set_session_name`, `set_auto_retry`, `get_state`, and `get_session_stats` but no `prompt`. Tests must inspect a fake-pi transcript and assert no `prompt` command appears.

The main usability risk is a failed early check hiding useful independent checks. Countermeasure: represent dependencies explicitly. If runtime bundle loading fails, all config-dependent checks are skipped. If the bundle loads but Linear smoke fails, workspace and pi checks can still run if their prerequisites are satisfied. If the instance lock is unavailable, `workspace-hooks` and `pi-probe` are skipped, while already completed read-only Linear checks remain visible.

The main operational risk is running trusted hooks unexpectedly. Countermeasure: document that default doctor includes the same scratch workflow-run workspace and `workspace.hooks` side effects as `--pi-probe`, and make `--check` available for read-only subsets such as `--check workflow-config --check linear-contract --check linear-smoke`. The check result for `workspace-hooks` must include the scratch workspace path and run root so an operator can inspect cleanup warnings.

The main compatibility risk is breaking current CLI parsing or changing existing readiness commands. Countermeasure: add `doctor` as a new `CliResult` branch, keep the current `RunMode` branches and existing flags unchanged, and extend `test/main_test.gleam` for the new parser cases.

The main code-duplication risk is copying Linear smoke, contract, or workspace-probe logic into a new module. Countermeasure: doctor should call `smoke.linear_read_smoke`, `linear_contract.check`, `workspace_run.prepare_step`, `workspace_run.cleanup_run`, and `probe.probe` through injectable dependencies. The existing standalone service modes can stay as thin wrappers around their current behavior; they do not need to share all doctor machinery in the first implementation.

The main documentation risk is leaving the old TODO trail in place after implementation. Countermeasure: implementation steps explicitly search `docs/TODO.md` for doctor/readiness aggregation language, remove it if present, and mark the existing doctor progress item in `docs/plans/real-board-readiness.md` complete.

## Progress

- [x] (2026-04-29 00:00Z) Searched documentation for doctor-command references. `docs/TODO.md` has no doctor item in the current tree; `docs/plans/real-board-readiness.md` has one unchecked doctor progress item.
- [x] (2026-04-29 00:00Z) Reviewed current CLI and service surfaces: `src/scherzo/main.gleam` supports daemon, once, `--linear-smoke`, `--linear-contract-check`, `--pi-probe`, and `ctl`, but no `doctor` command.
- [x] (2026-04-29 00:00Z) Reviewed current readiness helpers: `src/scherzo/smoke.gleam`, `src/scherzo/linear_contract.gleam`, `src/scherzo/orchestrator/service.gleam`, `src/scherzo/workspace.gleam`, `src/scherzo/agent/probe.gleam`, and `src/scherzo/instance_lock.gleam`.
- [x] (2026-04-29 00:00Z) Ran baseline validation from the repository root with `direnv exec . gleam test`; it passed with `215 passed, no failures`.
- [x] (2026-04-30 15:38Z) Re-reviewed this plan against the current tree. The plan is still relevant because `src/scherzo/main.gleam` and `src/scherzo/orchestrator/service.gleam` still have no doctor command, but the implementation instructions needed to be updated for the current YAML orchestrator/DAG runtime and `workspace_run` probe path.
- [x] (2026-04-30 15:38Z) Ran current validation from the repository root. `direnv exec . gleam test` was blocked by an unapproved `.envrc`; plain `gleam format --check src test`, `gleam test`, and `gleam run -- --help` succeeded. The test run reported `410 passed, no failures`, and help still showed no doctor command.
- [x] (2026-04-30 16:16Z) Added `src/scherzo/doctor.gleam` with stable check names, result/status/report/summary helpers, check parsing, first-seen de-duplication, and pure tests in `test/doctor_test.gleam`.
- [x] (2026-04-30 16:23Z) Added service-level doctor execution in `src/scherzo/orchestrator/service.gleam` with injected runtime bundle, Linear smoke, Linear contract, lock, workspace, cleanup, pi-probe, logging, and list-writer dependencies; added coverage in `test/orchestrator_service_doctor_test.gleam` for success, failures, skips, cleanup warnings, shared workspace use, and no-prompt fake-pi probing.
- [x] (2026-04-30 16:24Z) Added `doctor` CLI parsing, usage text, `--check`, `--list-checks`, and service wiring in `src/scherzo/main.gleam`; extended `test/main_test.gleam` for valid and invalid doctor arguments while preserving existing modes.
- [x] (2026-04-30 16:26Z) Updated `README.md`, confirmed `docs/TODO.md` still has no doctor/readiness aggregation TODO, marked the real-board-readiness doctor item complete, and ran final plain Gleam validation because `direnv exec .` remained blocked by the unapproved `.envrc` in this workspace. `gleam format`, `gleam format --check src test`, `gleam test`, and `gleam run -- doctor --list-checks` succeeded; the test run reported `426 passed, no failures`.
- [x] (2026-04-30 16:31Z) After `.envrc` was approved, reran deterministic validation through `direnv exec .`: `gleam format --check src test`, `gleam test`, `gleam run -- --help`, and `gleam run -- doctor --list-checks` all succeeded; the test run still reported `426 passed, no failures`.
- [x] (2026-04-30 16:32Z) Ran manual doctor CLI checks through `direnv exec .`. With the approved environment, `LINEAR_API_KEY` and `LINEAR_PROJECT_SLUG` were not set, so real Linear smoke/contract validation was initially not possible. Config-only checks passed for `.scherzo/scherzo.yaml` and `examples/scherzo.yaml` when supplied dummy local environment values. The unknown-check path returned `unknown_doctor_check`, workflow load failure skipped dependent checks, workspace-hook validation passed and cleaned the run root, and `--check pi-probe` launched the real `pi --mode rpc --no-session` probe successfully without sending a prompt.
- [x] (2026-04-30 16:36Z) Sourced real local credentials from `~/Code/scherzo/.env.local` without printing secrets and reran credential-gated validation through `direnv exec .`. The read-only doctor subset passed with `workflow-config`, `linear-contract`, and `linear-smoke`; the full default doctor run passed all six checks with `doctor_summary passed=6 warned=0 failed=0 skipped=0`; and the legacy `--linear-smoke`, `--linear-contract-check`, and `--pi-probe` modes all still passed. Local cleanup left no instance lock, and empty synthetic probe directories were removed manually after validation.

## Surprises & Discoveries

- Observation: The read-only Linear board contract check is already implemented in the current source tree even though the older plan progress checklist still shows it as pending.
  Evidence: `src/scherzo/main.gleam` has `LinearContractCheck`, `src/scherzo/orchestrator/service.gleam` has `start_linear_contract_check`, and `src/scherzo/linear_contract.gleam` defines contract diagnostics.

- Observation: Graceful SIGTERM lifecycle support is already implemented in the current source tree even though the older hardening plan progress checklist still shows it as pending.
  Evidence: `src/scherzo/orchestrator/service.gleam` imports `scherzo/lifecycle` and `scherzo/signal`, and daemon mode goes through `start_daemon_with_lifecycle` rather than `process.sleep_forever()`.

- Observation: `docs/TODO.md` does not currently contain a doctor-command TODO.
  Evidence: A grep for `doctor`, `readiness checks`, and `one command` found the doctor item only in `docs/plans/real-board-readiness.md`.

- Observation: Legacy Markdown runtime workflows have been removed since the original doctor plan was drafted. Doctor must accept a YAML orchestrator config path such as `.scherzo/scherzo.yaml` or `examples/scherzo.yaml`, not `WORKFLOW.md`.
  Evidence: `README.md` says legacy Markdown runtime workflows are no longer supported, `runtime_bundle.load` rejects non-`.yaml`/`.yml` paths, and `find` found no `WORKFLOW.md` file in the repository.

- Observation: The current no-prompt pi probe uses the YAML DAG workspace path, not the older `workspace.prepare` helper. Doctor should reuse `workspace_run.prepare_step` and `workspace_run.cleanup_run` so it exercises the same `workspace.hooks.create`, `workspace.hooks.before_step`, and `workspace.hooks.remove` behavior as `--pi-probe`.
  Evidence: `src/scherzo/orchestrator/service.gleam` implements `run_pi_probe_orchestrator` with `workspace_run.prepare_step`, `workflow_dag.WorkspaceRef(name: "main", from: None)`, `probe.probe`, and `workspace_run.cleanup_run`.

- Observation: `config.validate_dispatch` is a legacy top-level hook gate and is not part of the current startup or probe path. Calling it from doctor would incorrectly fail valid YAML orchestrator configs that use `workspace.hooks.create` and `workspace.hooks.before_step`.
  Evidence: `grep` finds `validate_dispatch` only in `src/scherzo/config.gleam` and `test/config_test.gleam`; `README.md`, `.scherzo/scherzo.yaml`, and `examples/scherzo.yaml` configure hooks under `workspace.hooks`.

- Observation: This workspace currently blocks `direnv exec .` until `.envrc` is allowed, but the plain Gleam commands still validate the current tree.
  Evidence: `direnv exec . gleam test` failed with "`.envrc` is blocked"; `gleam format --check src test`, `gleam test`, and `gleam run -- --help` succeeded, and `gleam test` reported `410 passed, no failures`.

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

- Decision: Share one scratch workflow-run workspace for `workspace-hooks` and `pi-probe` during a doctor run.
  Rationale: Preparing a real repository can be expensive and side-effectful. Sharing the workspace keeps output granular without running hooks twice.
  Date: 2026-04-29

- Decision: Report cleanup failure as a warning result rather than hiding the main check result.
  Rationale: This matches the existing `--pi-probe` posture, where cleanup failure is logged as `pi_probe_cleanup_failed` but the probe result still reflects whether pi RPC worked.
  Date: 2026-04-29

- Decision: Treat `runtime_bundle.load` as the YAML config and workflow-DAG validation boundary for doctor, and do not call `config.validate_dispatch`.
  Rationale: The current runtime is YAML orchestrator/DAG only. `runtime_bundle.load` validates the orchestrator config, routed workflow DAG files, and prompt files, while `config.validate_dispatch` checks legacy top-level hooks that current configs do not use.
  Date: 2026-04-30

- Decision: Implement `workspace-hooks` and `pi-probe` through `workspace_run.prepare_step` and `workspace_run.cleanup_run` rather than `workspace.prepare` and `workspace.cleanup_stored_path`.
  Rationale: The current service-level `--pi-probe` uses the workflow-run workspace machinery and `workspace.hooks.create`/`before_step`/`remove`. Doctor must exercise the same path to stay relevant.
  Date: 2026-04-30

- Decision: Report local cleanup failures as a non-fatal `workspace-hooks` warning with code `workspace_cleanup_failed`, not as a separate selectable readiness check.
  Rationale: Cleanup is a consequence of running the workspace/pi checks rather than an independent operator-selected prerequisite. Keeping it on `workspace-hooks` preserves a six-check public list while still making the warning visible in the summary.
  Date: 2026-04-30

## Outcomes & Retrospective

2026-04-30 implementation outcome: The doctor command is implemented as `gleam run -- doctor [options] [path-to-scherzo.yaml]`. The final public check names are `workflow-config`, `linear-contract`, `linear-smoke`, `instance-lock`, `workspace-hooks`, and `pi-probe`; `gleam run -- doctor --list-checks` prints those names one per line without loading a runtime bundle. The default doctor run reports structured events named `doctor_check_pass`, `doctor_check_warn`, `doctor_check_fail`, or `doctor_check_skip` for individual checks, followed by `doctor_summary passed=N warned=N failed=N skipped=N`. Existing `--linear-smoke`, `--linear-contract-check`, and `--pi-probe` modes remain in place.

The implementation added deterministic coverage in `test/doctor_test.gleam`, `test/orchestrator_service_doctor_test.gleam`, and `test/main_test.gleam`. The first final deterministic gate used plain Gleam commands because `.envrc` was blocked, then `.envrc` was approved and the deterministic gate was rerun through `direnv exec .`. Both runs reported `426 passed, no failures`, and `direnv exec . gleam run -- doctor --list-checks` printed the six stable names in order.

Additional manual doctor validation covered the config-only path, unknown-check usage error, workflow-load failure skip behavior, workspace-hook local validation, instance-lock acquire/release, and the real `pi --mode rpc --no-session` no-prompt probe path. Real Linear credentials were then sourced from the sibling checkout's ignored `~/Code/scherzo/.env.local`; the read-only doctor subset, full default doctor run, and legacy `--linear-smoke`/`--linear-contract-check` modes all passed against the configured board. The deterministic fake-pi transcript test and the real local pi probe both support the safety claim that doctor pi probing does not send a task prompt.

`docs/TODO.md` did not contain any doctor/readiness aggregation item at implementation time, so no TODO removal was needed. `docs/plans/real-board-readiness.md` was updated to mark its doctor progress item complete and point to this plan.

## Context and Orientation

Scherzo is a Gleam/Erlang daemon. Runtime source lives under `src/scherzo/`, tests live under `test/`, documentation lives under `README.md`, `.scherzo/scherzo.yaml`, `examples/scherzo.yaml`, `examples/workflows/`, and `docs/`, and validation normally runs from the repository root with `direnv exec . gleam test` and `direnv exec . gleam format --check src test`. If `direnv` is blocked in a disposable workspace, plain `gleam test` and `gleam format --check src test` are acceptable for deterministic validation after noting the deviation in this plan.

The CLI entry point is `src/scherzo/main.gleam`. It defines `RunMode` variants `Daemon`, `Once`, `LinearSmoke`, `LinearContractCheck`, and `PiProbe`, plus `Control(List(String))` for `ctl` and `Doctor(doctor.Options)` for the readiness command. Its `usage()` text documents YAML orchestrator config paths such as `.scherzo/scherzo.yaml`, the `doctor` subcommand, repeated `doctor --check <name>` selection, and `doctor --list-checks`.

The service layer is `src/scherzo/orchestrator/service.gleam`. It exposes `start_linear_smoke`, `start_linear_contract_check`, `start_pi_probe`, `start_doctor`, `start_doctor_with_dependencies`, and `build_doctor_report_with_dependencies`. Linear smoke loads a `runtime_bundle.RuntimeBundle`, creates `smoke.real_linear_reader`, calls `smoke.linear_read_smoke`, and logs `linear_smoke_ok`. Contract check loads a runtime bundle, creates `linear.real_contract_client`, calls `linear_contract.check`, logs `linear_contract_ok` or `linear_contract_diagnostic`, and exits nonzero on mismatch. Pi probe loads a runtime bundle, acquires the instance lock, prepares a synthetic scratch step workspace through `workspace_run.prepare_step`, launches `probe.probe`, cleans up the run root with `workspace_run.cleanup_run`, releases the lock, and logs `pi_probe_ok`. Doctor uses the same lower-level helpers through `DoctorDependencies` so tests can inject fake Linear, lock, workspace, cleanup, pi, and logging behavior.

`src/scherzo/runtime_bundle.gleam` defines `RuntimeBundle` and `load`, which select a YAML config path, parse the orchestrator config, load every routed workflow DAG, and inline prompt-template files. `src/scherzo/smoke.gleam` defines `LinearSmokeReader` and `linear_read_smoke`. `src/scherzo/linear_contract.gleam` defines remote board snapshot types, diagnostic variants, `check`, `is_ok`, `diagnostic_code`, and `format_report`. `src/scherzo/workspace_run.gleam` defines `prepare_step` and `cleanup_run` for current YAML DAG workspaces, and `src/scherzo/instance_lock.gleam` defines `acquire` and `release`. `src/scherzo/agent/probe.gleam` performs the no-prompt pi RPC probe.

A doctor check in this plan means a named, independently reported readiness assertion. A passing `workflow-config` check means the YAML orchestrator config, routed workflow DAG files, and prompt templates loaded successfully. A passing check for another name means that one assertion succeeded. A failing check means Scherzo should not be considered ready for the selected doctor run. A skipped check means a prerequisite was unavailable or failed, not that the underlying capability is healthy.

## Preconditions and Verified Facts

Before implementing this plan, the preferred baseline from the repository root is:

    direnv exec . gleam format --check src test
    direnv exec . gleam test
    direnv exec . gleam run -- --help

If `direnv exec .` is blocked because `.envrc` has not been approved in the current workspace, either run `direnv allow` after reviewing `.envrc`, or record the reason and use the plain commands:

    gleam format --check src test
    gleam test
    gleam run -- --help

On 2026-04-29 while writing this plan, `direnv exec . gleam test` ended with `215 passed, no failures`. On 2026-04-30 during relevance review, `direnv exec . gleam test` was blocked by `.envrc`, while `gleam format --check src test`, `gleam test`, and `gleam run -- --help` succeeded; the test run ended with `410 passed, no failures`.

Current repository facts after implementation:

- `src/scherzo/main.gleam` recognizes `doctor`, repeated `doctor --check <name>`, `doctor --list-checks`, `--linear-contract-check`, and `--pi-probe`.
- `src/scherzo/main.gleam` help text describes YAML orchestrator configs such as `.scherzo/scherzo.yaml`; runtime `WORKFLOW.md` paths are no longer valid.
- `src/scherzo/doctor.gleam` defines the stable doctor check names, result/status/report/summary types, check parsing, de-duplication, event names, and log fields.
- `src/scherzo/runtime_bundle.gleam` is the current load boundary for startup-like operations. It selects a `.yaml`/`.yml` config path, parses the orchestrator config, loads routed workflow DAG YAML files, and resolves prompt-template files.
- `src/scherzo/orchestrator/service.gleam` has `ContractCheckDependencies` for fake contract-reader tests and `DoctorDependencies` for fake doctor tests.
- `src/scherzo/orchestrator/service.gleam` implements `start_pi_probe` with `workspace_run.prepare_step`, `probe.probe`, and `workspace_run.cleanup_run`; doctor uses the same lower-level functions through injection.
- `test/doctor_test.gleam`, `test/orchestrator_service_doctor_test.gleam`, and `test/main_test.gleam` cover the new doctor behavior.
- `test/orchestrator_service_test.gleam` still contains service tests for Linear contract success, mismatch, fetch error, YAML config loading, and no-prompt pi probe.
- `config.validate_dispatch` only checks legacy top-level `hooks.after_create` or `hooks.before_run`; it is not called by current startup paths and is not the doctor readiness gate for YAML DAG configs.
- Current workspace hook execution for YAML DAG runs lives under `workspace.hooks.create`, `workspace.hooks.before_step`, `workspace.hooks.after_step`, and `workspace.hooks.remove` in `domain.DagHooksConfig` and `workspace_run.gleam`.
- `probe.probe` validates pi RPC without sending a task prompt.
- `README.md` now recommends `doctor` as the primary readiness command and leaves the separate `--linear-smoke`, `--linear-contract-check`, and `--pi-probe` modes as focused troubleshooting alternatives.
- `docs/TODO.md` does not contain a doctor TODO, and `docs/plans/real-board-readiness.md` marks the doctor progress item complete.

If these facts differ during future maintenance, update this plan first so it remains self-contained.

## Scope Boundaries

In scope: a `doctor` CLI subcommand; stable named check selection; per-check pass/fail/skip/warn results; default readiness check set; YAML orchestrator config, routed workflow DAG, and prompt-template validation; instance-lock availability check; read-only Linear smoke; read-only Linear contract check; scratch workflow-run workspace hook validation through `workspace_run`; no-prompt pi probe using the scratch step workspace; structured stderr logs; deterministic tests; README updates; removal of any stale doctor item from `docs/TODO.md` if present; marking the real-board-readiness doctor item complete after implementation.

Out of scope: dispatching Linear issues; daemon startup; local control API health checks; EventHub health checks; web dashboard checks; Linear mutations; Linear command comment processing; handoff comments; durable ledger checks; network retries beyond current readers; deprecating the existing `--linear-smoke`, `--linear-contract-check`, or `--pi-probe` flags; JSON output mode for doctor. A future plan may add `doctor --json` and daemon health checks once the text/log output has stabilized.

## Milestones

Milestone 1 adds the doctor model and pure behavior. At the end, tests can select default checks, parse repeated `--check` names through a helper, format pass/fail/skip/warn results, compute summary counts, and compute the correct exit decision without touching Linear, filesystems, locks, or pi.

Milestone 2 adds service execution with injected dependencies. At the end, service tests can run doctor against temporary YAML orchestrator configs and routed workflow DAG files, fake Linear smoke readers, fake contract clients, fake lock operations, fake `workspace_run.prepare_step`/`cleanup_run` functions, and fake pi probes. Tests prove that a Linear smoke failure does not hide later independent local checks, that a lock failure skips workspace and pi checks, that a valid current YAML config is not rejected by the legacy `config.validate_dispatch` hook gate, and that pi probe never sends a prompt.

Milestone 3 wires the CLI. At the end, `src/scherzo/main.gleam` accepts `doctor`, `doctor --check <name>`, and `doctor --list-checks`; usage text documents the command; and parser tests cover valid and invalid doctor arguments.

Milestone 4 updates documentation and validates the full tree. At the end, the README quick start uses doctor as the primary validation command while preserving one-off commands for focused debugging, stale TODO documentation is removed or confirmed absent, the real-board-readiness progress item is marked complete, and the deterministic suite passes.

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

    pub type Summary {
      Summary(passed: Int, warned: Int, failed: Int, skipped: Int)
    }

Add helpers `summary(report)`, `has_failures(report)`, `result_event(result)`, and `result_log_fields(result)`. Stable event names should be `doctor_check_pass`, `doctor_check_warn`, `doctor_check_fail`, `doctor_check_skip`, and `doctor_summary`. The summary fields should include `passed`, `warned`, `failed`, and `skipped`.

Add `Options` or `DoctorOptions` in `src/scherzo/doctor.gleam` unless a separate small CLI helper module is clearly cleaner. It should contain `path: Option(String)`, `checks: List(String)`, and `list_checks: Bool`. Keep raw check names in the CLI layer and let the service map unknown names to a usage error or startup error with a clear message. The main parser should support:

    gleam run -- doctor
    gleam run -- doctor examples/scherzo.yaml
    gleam run -- doctor --check linear-smoke --check pi-probe examples/scherzo.yaml
    gleam run -- doctor --list-checks

Reject unknown doctor options, missing check names after `--check`, duplicate positional YAML config paths, and unknown top-level modes. Duplicated `--check` names should be de-duplicated in first-seen order by the doctor layer so a repeated flag does not run a check twice.

Add `DoctorDependencies` to `src/scherzo/orchestrator/service.gleam`, or put it in `src/scherzo/doctor.gleam` and re-export only what service needs. It should provide injectable functions for Linear smoke reader creation, Linear contract client creation, instance-lock acquire/release, scratch step workspace preparation/cleanup, pi probe, and logging. Production dependencies use `smoke.real_linear_reader`, `linear.real_contract_client`, `instance_lock.acquire`, `instance_lock.release`, `workspace_run.prepare_step`, `workspace_run.cleanup_run`, `probe.probe`, and `log_stderr`.

Implement `service.start_doctor(options)`, `service.start_doctor_with_dependencies(options, dependencies)`, and a test-friendly `service.build_doctor_report_with_dependencies(options, dependencies)` or equivalent helper that returns a `doctor.Report` before exit-code mapping. The function resolves selected checks first. If `--list-checks` is set, it prints or logs the check list and returns `Ok(Nil)` without loading a runtime bundle. Otherwise it loads the optional YAML orchestrator config path with `runtime_bundle.load` and runs the selected checks in the canonical order, not in user-provided order. This keeps output stable and ensures prerequisites are handled consistently. Public `start_*` entry points should return `Error(StartupError("doctor_failed", ...))` after logging the report when any selected check failed; report-building tests may inspect the returned report directly.

The `workflow-config` check should call `runtime_bundle.load`. Success means the YAML orchestrator config parsed, required environment-backed settings such as `LINEAR_API_KEY` resolved, routed workflow DAG files loaded, workflow IDs matched routing keys, and referenced prompt-template files were inlined safely. Do not call `config.validate_dispatch`; that helper checks legacy top-level `hooks.after_create`/`hooks.before_run` and would reject current valid configs that use `workspace.hooks.create` and `workspace.hooks.before_step`. If the runtime bundle cannot be loaded, report `workflow-config` as failed and skip all selected checks that require the effective config or orchestrator. Linear read-only checks should be skipped when bundle loading fails because they need tracker config.

The `linear-contract` check should use the same behavior as `start_linear_contract_check_with_dependencies`: fetch the remote board through a `linear.ContractClient`, call `linear_contract.check`, pass when diagnostics are empty, and fail with one result per contract mismatch or one aggregate failed result plus diagnostic fields. To keep output compact, use one `doctor_check_fail check=linear-contract code=linear_contract_mismatch diagnostic_count=N` plus additional fields such as the first diagnostic code/message. The existing standalone `--linear-contract-check` continues to log every diagnostic.

The `linear-smoke` check should call `smoke.linear_read_smoke` and report `candidate_count`, `terminal_count`, and `refreshed_count` on success. On failure, map the tracker error through existing service error-code helpers so the check result has a stable code such as `linear_api_status`.

The `instance-lock` check should acquire the lock for the lock-required part of the doctor run when `instance-lock`, `workspace-hooks`, or `pi-probe` is selected. If acquiring the lock fails, report `instance-lock` as failed and skip `workspace-hooks` and `pi-probe`. If only `workspace-hooks` or `pi-probe` is selected and `instance-lock` was not explicitly selected, still acquire the lock and report an implicit `instance-lock` result so operators understand why a local probe did or did not run. Release the lock exactly once at the end of all lock-required checks.

The `workspace-hooks` check should prepare a scratch step workspace through the same path as `--pi-probe`: construct a synthetic issue with id and identifier `SCHERZO-DOCTOR`, call `workspace_run.prepare_step(issue, "doctor", "doctor", "doctor", workflow_dag.WorkspaceRef(name: "main", from: None), bundle.orchestrator, dict.new())`, and keep the returned `PreparedStepWorkspace` available for `pi-probe` if that check is also selected. On success, report `workspace_path`, `run_root`, and the hook names that were configured, such as `create`, `before_step`, and `remove`. If `workspace-hooks` is not selected but `pi-probe` is selected, still prepare the scratch workspace as a prerequisite and report a skipped or implicit workspace result only if the preparation fails; do not make pi probe appear to have failed because of pi when the real failure is workspace preparation.

The `pi-probe` check should run `probe.probe(bundle.effective.pi.command, prepared.path, bundle.effective.pi.read_timeout_ms)` against the prepared doctor step workspace. On success, report the workspace path. On failure, report the pi RPC error code. The fake-pi test must assert the transcript includes `get_state` and `get_session_stats` and does not include `prompt`.

After workspace and pi checks, cleanup the prepared run root with `workspace_run.cleanup_run(prepared.run_root, bundle.orchestrator)`. If cleanup fails, append a warning result with check name `workspace-hooks` and code `workspace_cleanup_failed`. Keep the summary warning count accurate.

Update `README.md`. In the quick start and a new `Doctor readiness checks` subsection near the existing validation and safety material, document `doctor`, list default checks, give read-only subset examples, and say that `--linear-smoke`, `--linear-contract-check`, and `--pi-probe` remain available for focused debugging. Make doctor the first recommended validation command after deterministic tests, then leave the individual commands as troubleshooting alternatives.

Update documentation cleanup. Search `docs/TODO.md` for `doctor`, `readiness`, and `smoke/probe` aggregation language. If a doctor TODO exists at implementation time, remove it because this plan and implementation supersede it. In the current tree there is no such `docs/TODO.md` entry, so record that no TODO removal was needed if the file is unchanged. In `docs/plans/real-board-readiness.md`, mark the doctor progress item complete after doctor is implemented and validated, and reference `docs/plans/doctor-command.md` in the progress entry.

## Concrete Steps

1. From the repository root, run `direnv exec . gleam test` and record the current pass count in this plan's Progress section. The plan-authoring baseline was `215 passed, no failures`; the 2026-04-30 relevance-review baseline was `410 passed, no failures` with plain `gleam test` because `direnv exec .` was blocked by an unapproved `.envrc`.

2. Create `test/doctor_test.gleam`. Add `default_checks_are_stable_test` asserting `doctor.default_checks()` returns `workflow-config`, `linear-contract`, `linear-smoke`, `instance-lock`, `workspace-hooks`, and `pi-probe` in that order.

3. In `test/doctor_test.gleam`, add `parse_check_name_accepts_known_names_test` and `parse_check_name_rejects_unknown_names_test` for all stable check-name strings.

4. In `test/doctor_test.gleam`, add `summary_counts_result_statuses_test` with one pass, one warn, one fail, and one skip result. Assert the summary counts and `doctor.has_failures(report) == True`.

5. Implement `src/scherzo/doctor.gleam` with the check-name, result, report, and summary helpers until the pure doctor tests pass.

6. Extend `src/scherzo/orchestrator/service.gleam` with a `DoctorDependencies` type and production dependency constructor. Keep existing `Dependencies`, `DaemonLifecycleDependencies`, and `ContractCheckDependencies` unchanged. Include injectable functions for `workspace_run.prepare_step` and `workspace_run.cleanup_run`, not `workspace.prepare`.

7. Add `test/orchestrator_service_doctor_test.gleam`. Start with `doctor_workflow_config_success_test`: write a temporary `scherzo.yaml`, a routed workflow DAG file under `workflows/`, and any referenced prompt template; configure a fake workspace root with `workspace.hooks.create` and `workspace.hooks.before_step`; run the report-building doctor helper using fake dependencies; assert a `doctor_check_pass` result for `workflow-config` and a final `doctor_summary` when the public start helper logs the report. In the same file, add `doctor_workflow_config_accepts_current_yaml_workspace_hooks_test` with no top-level `hooks.after_create` or `hooks.before_run`, so the test fails if doctor accidentally calls `config.validate_dispatch`.

8. Add `doctor_unknown_check_name_fails_before_loading_workflow_test`: pass a doctor option containing `--check no-such-check` and a nonexistent config path; assert `StartupError("unknown_doctor_check", _)` or the chosen usage error rather than `missing_config_file`. This proves check-name validation happens before `runtime_bundle.load`.

9. Add `doctor_linear_smoke_success_reports_counts_test`: fake `smoke.LinearSmokeReader` returns one candidate, zero terminal issues, and one refreshed issue; assert `doctor_check_pass check=linear-smoke candidate_count=1 terminal_count=0 refreshed_count=1`.

10. Add `doctor_linear_smoke_failure_does_not_skip_workspace_probe_test`: fake Linear smoke returns `error.LinearApiStatus(500)` while fake workspace and pi probe succeed. Assert the report-building helper returns a report containing a failed `linear-smoke` result and passed local probe results, and assert the public start helper logs those results before returning `doctor_failed`.

11. Add `doctor_contract_mismatch_reports_failure_test`: fake `linear.ContractClient` returns a board missing the configured active state; assert a failed `linear-contract` result with code `linear_contract_mismatch` and a nonzero summary failure count.

12. Add `doctor_lock_failure_skips_workspace_and_pi_test`: fake lock acquire fails; assert `instance-lock` fails and `workspace-hooks` plus `pi-probe` are skipped without calling fake `workspace_run.prepare_step` or fake pi probe.

13. Add `doctor_workspace_and_pi_share_one_prepared_workspace_test`: fake `workspace_run.prepare_step` returns a `PreparedStepWorkspace` with run root `test/tmp/doctor/workspaces/doctor/SCHERZO-DOCTOR/doctor` and path `test/tmp/doctor/workspaces/doctor/SCHERZO-DOCTOR/doctor/main`; fake pi probe records that path; assert prepare is called once, pi receives the same path, and cleanup is called once with the run root after the probe.

14. Add `doctor_pi_probe_does_not_prompt_test`: use `test/fixtures/fake_pi_rpc.sh`, a real temporary transcript path, a temporary YAML orchestrator config, and a minimal routed workflow DAG; run only `--check pi-probe` with Linear checks omitted; assert the transcript contains `get_state` and `get_session_stats` but not `prompt`.

15. Implement the doctor report-building helper, `service.start_doctor_with_dependencies`, and any private helper functions until the service doctor tests pass. Use existing error-code mapping helpers where possible rather than inventing new strings for existing error classes.

16. Extend `src/scherzo/main.gleam`. Add a `Doctor(doctor.Options)` branch to `CliResult`, or an equivalent shape that avoids a `main`/`service` import cycle. Add parser support for `doctor`, repeated `--check`, `--list-checks`, and one optional YAML config path.

17. Update `test/main_test.gleam`. Assert `main.parse_args(["doctor"])`, `main.parse_args(["doctor", "scherzo.yaml"])`, `main.parse_args(["doctor", "--check", "linear-smoke", "--check", "pi-probe", "scherzo.yaml"])`, and `main.parse_args(["doctor", "--list-checks"])` return the expected doctor result.

18. Add main parser rejection tests for `doctor --unknown`, `doctor --check`, and `doctor one.yaml two.yaml`.

19. Update `main.usage()` and its tests so help text mentions `doctor`, `doctor --check`, the default check list, and the existing individual smoke/probe flags.

20. Wire `main.main` to call `service.start_doctor` for the new doctor branch. For `--list-checks`, either have `main` print the check list directly or have `service.start_doctor` log/print it; test the pure parser and at least one service path.

21. Update `README.md`. Add a `Doctor readiness checks` subsection near the quick start/control/safety documentation and revise the quick start so doctor is the primary validation command. Include examples for default full doctor and a read-only subset doctor.

22. Search `docs/TODO.md` for `doctor`, `readiness`, `linear-smoke`, and `pi-probe`. If a doctor-command TODO exists, remove that bullet. If no such bullet exists, leave `docs/TODO.md` unchanged and record the fact in this plan's Outcomes.

23. Update `docs/plans/real-board-readiness.md`: mark the existing doctor progress item complete with the implementation date and mention that the work is now covered by `docs/plans/doctor-command.md`.

24. Update this plan's Progress, Surprises & Discoveries, Decision Log if any behavior changed, and Outcomes & Retrospective with final validation results.

25. Run `direnv exec . gleam format`. If `direnv` remains blocked and has not been deliberately approved, run `gleam format` instead and record the deviation.

26. Run `direnv exec . gleam format --check src test` and `direnv exec . gleam test`. If `direnv` remains blocked and has not been deliberately approved, run `gleam format --check src test` and `gleam test` instead, and record both the deviation and the final pass count in this plan's Progress and Outcomes.

27. Run `direnv exec . gleam run -- doctor --list-checks` and record the expected output shape in this plan's Artifacts and Notes. If `direnv` is blocked and the implementation used plain Gleam validation, run `gleam run -- doctor --list-checks` and record the deviation.

28. Optional credential-gated validation: with `LINEAR_API_KEY` and any required project-slug environment, run `direnv exec . gleam run -- doctor examples/scherzo.yaml` or a private test `.yaml` config. Confirm it prints per-check results, does not dispatch an issue, and fake or real pi transcript contains no prompt.

29. Commit the phase with a message such as `Add doctor readiness command`.

## Testing and Falsifiability

This plan is falsified if doctor can dispatch a worker, post or mutate Linear data, send a pi `prompt`, hide per-check output behind one aggregate error, reject a valid current YAML orchestrator config because legacy top-level hooks are absent, fail to release the instance lock, fail to clean the scratch workflow-run workspace on normal completion, or break the existing one-off readiness flags.

Pure tests in `test/doctor_test.gleam` must cover stable check names, parser helpers, result events, summary counts, failure detection, and first-seen de-duplication for repeated check names.

Service tests in `test/orchestrator_service_doctor_test.gleam` must cover successful YAML orchestrator config, routed workflow DAG, and prompt-template validation; proof that current `workspace.hooks.create`/`before_step` configs pass without legacy top-level hooks; Linear smoke success and failure; Linear contract success and mismatch; lock failure skipping local probes; `workspace_run.prepare_step` failure; shared workspace use for workspace and pi checks; `workspace_run.cleanup_run` warning behavior; and fake-pi transcript proof that no prompt is sent.

CLI tests in `test/main_test.gleam` must cover doctor argument parsing, invalid doctor options, usage text, and preservation of existing modes.

Run from the repository root:

    direnv exec . gleam format --check src test
    direnv exec . gleam test

If `direnv` is blocked and not approved in this workspace, record that fact and run:

    gleam format --check src test
    gleam test

Before implementation, tests referencing `scherzo/doctor` and `main.parse_args(["doctor", ...])` should fail. After implementation, the full deterministic suite should pass and current `--linear-smoke`, `--linear-contract-check`, `--pi-probe`, and `ctl` parser tests should still pass.

## Validation and Acceptance

Accept the implementation when all of the following are true:

- `direnv exec . gleam run -- doctor --list-checks` prints or logs the stable check names.
- `direnv exec . gleam run -- doctor path/to/scherzo.yaml` runs the default checks in stable order and emits one result per check plus `doctor_summary`.
- `direnv exec . gleam run -- doctor --check linear-smoke --check linear-contract path/to/scherzo.yaml` runs only the selected read-only checks and does not acquire the instance lock.
- A current YAML config that uses `workspace.hooks.create` and `workspace.hooks.before_step` but no top-level `hooks.after_create` or `hooks.before_run` passes `workflow-config`.
- `pi-probe` doctor validation uses a scratch workflow-run workspace and sends no `prompt` command to pi.
- A Linear smoke failure does not suppress independent local probe checks when their prerequisites are satisfied.
- A lock failure skips workspace and pi checks and releases any partially acquired resources.
- Existing standalone readiness flags still behave as before.
- README documents doctor as the primary readiness command.
- Any stale doctor TODO in `docs/TODO.md` is removed, or the retrospective records that none existed.
- The real-board-readiness doctor progress item is marked complete.
- The full deterministic test suite passes.

## Rollout, Recovery, and Idempotence

The doctor command is additive. If it behaves poorly, operators can continue using the existing `--linear-smoke`, `--linear-contract-check`, and `--pi-probe` modes while the issue is fixed. Doctor does not change daemon behavior, scheduler state, Linear issues, or handoff configuration.

Doctor runs are safe to repeat. Repeated read-only checks only query Linear. Repeated local checks may create and remove the same scratch workflow-run path under the configured workspace root, using synthetic issue identifier `SCHERZO-DOCTOR`, workflow id `doctor`, run id `doctor`, and workspace name `main`. The same `workspace_run.prepare_step` and `workspace_run.cleanup_run` path used by `--pi-probe` should keep the workspace under the configured root and remove the scratch run root at the end of each successful local-probe run. If cleanup fails, doctor reports a warning with the path and run root so the operator can inspect and remove it manually.

Doctor must release the instance lock on every path after a successful acquire. If the process is killed with `kill -9` during doctor, the same stale-lock recovery rule as daemon mode applies: verify no Scherzo process remains active for that workspace root before manually removing `workspace.root/.scherzo-state/instance.lock`.

## Artifacts and Notes

Expected default command shape:

    direnv exec . gleam run -- doctor examples/scherzo.yaml

Expected structured log shape on all-pass fake dependencies:

    level=info service=scherzo event=doctor_check_pass check=workflow-config code=ok message="YAML orchestrator config and workflow DAGs are valid"
    level=info service=scherzo event=doctor_check_pass check=linear-contract code=ok project_slug=TEST team_count=1
    level=info service=scherzo event=doctor_check_pass check=linear-smoke code=ok candidate_count=1 terminal_count=0 refreshed_count=1
    level=info service=scherzo event=doctor_check_pass check=instance-lock code=ok workspace_root=.scherzo/workspaces
    level=info service=scherzo event=doctor_check_pass check=workspace-hooks code=ok run_root=.../doctor/SCHERZO-DOCTOR/doctor workspace_path=.../doctor/SCHERZO-DOCTOR/doctor/main
    level=info service=scherzo event=doctor_check_pass check=pi-probe code=ok workspace_path=.../doctor/SCHERZO-DOCTOR/doctor/main
    level=info service=scherzo event=doctor_summary passed=6 warned=0 failed=0 skipped=0

Actual check list from final direnv-backed validation:

    direnv exec . gleam run -- doctor --list-checks
    workflow-config
    linear-contract
    linear-smoke
    instance-lock
    workspace-hooks
    pi-probe

Credential-gated manual validation sourced `~/Code/scherzo/.env.local` without printing secrets. The read-only subset passed with `doctor_summary passed=3 warned=0 failed=0 skipped=0`, reporting two candidate issues, three terminal sample issues, and one refreshed issue. The full default doctor run passed with `doctor_summary passed=6 warned=0 failed=0 skipped=0`. The focused legacy modes also passed: `--linear-smoke`, `--linear-contract-check`, and `--pi-probe`.

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

    pub type Summary {
      Summary(passed: Int, warned: Int, failed: Int, skipped: Int)
    }

    pub fn default_checks() -> List(CheckName)
    pub fn list_check_names() -> List(String)
    pub fn parse_check_name(String) -> Result(CheckName, String)
    pub fn check_name_to_string(CheckName) -> String
    pub fn summary(Report) -> Summary
    pub fn has_failures(Report) -> Bool
    pub fn result_event(CheckResult) -> String
    pub fn result_log_fields(CheckResult) -> List(log.Field)

In `src/scherzo/doctor.gleam`, add the options type used by both `main` and `service`:

    pub type Options {
      Options(
        path: Option(String),
        checks: List(String),
        list_checks: Bool,
      )
    }

In `src/scherzo/main.gleam`, add a doctor CLI result shape equivalent to:

    pub type CliResult {
      Run(RunMode, Option(String))
      Control(List(String))
      Doctor(doctor.Options)
      Help
    }

In `src/scherzo/orchestrator/service.gleam`, add production and test entry points equivalent to:

    pub fn start_doctor(options: doctor.Options) -> Result(Nil, StartupError)

    pub fn start_doctor_with_dependencies(
      options: doctor.Options,
      dependencies: DoctorDependencies,
    ) -> Result(Nil, StartupError)

    pub fn build_doctor_report_with_dependencies(
      options: doctor.Options,
      dependencies: DoctorDependencies,
    ) -> Result(doctor.Report, StartupError)

Keep the options type in `src/scherzo/doctor.gleam` rather than `src/scherzo/main.gleam` if importing `main` from `service` would create a cycle. `src/scherzo/main.gleam` can still expose or pattern-match a `Doctor(doctor.Options)` CLI branch.

Production doctor dependencies should use existing modules:

- `runtime_bundle.load` for YAML orchestrator config, routed workflow DAG, and prompt-template validation.
- No call to `config.validate_dispatch`; it checks legacy top-level hooks and is not a current readiness gate.
- `smoke.real_linear_reader` and `smoke.linear_read_smoke` for read-only Linear smoke.
- `linear.real_contract_client` and `linear_contract.check` for read-only board contract validation.
- `instance_lock.acquire` and `instance_lock.release` for local lock status.
- `workspace_run.prepare_step` and `workspace_run.cleanup_run` for scratch YAML DAG workspace hook validation.
- `probe.probe` for no-prompt pi RPC probing.
- `log_stderr` for structured logs with `bundle.secrets` redaction.
