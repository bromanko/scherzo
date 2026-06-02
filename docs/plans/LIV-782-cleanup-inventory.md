# LIV-782 cleanup inventory

This inventory records the current status of every surface named by the LIV-782 cleanup handoff. It is the evidence-backed gate for what can be removed now, what is already retired, and what must stay until a later migration or operator policy exists.

## Evidence snapshot

Facade import grep (`rg "import scherzo/(agent/pi_rpc|agent/runner|linear_triage|state/workflow_checkpoint|config/ui_server|tracker/conformance/linear_driver)" src test`) is now empty after migrating tests to the lasting modules (`scherzo/pi/client`, `scherzo/pi/protocol`, `scherzo/agent/run_attempt`, `scherzo/log`) or dropping dead duplicate tests alongside their removed dead modules.

Tracker bridge grep (`rg "adapter_legacy|workflow_compat_client|lookup_runtime_issue|refresh_runtime_issues_by_ids" src test`) now shows task-native runtime helper definitions in `src/scherzo/tracker/adapter.gleam`, direct daemon/effect-runner calls to those helpers, and `src/scherzo/tracker/adapter_legacy.gleam` as the remaining compatibility wrapper. The old daemon/effect-runner runtime issue fetch helpers moved onto task-native helpers, leaving `workflow_compat_client` as the last production bridge for the worker runner's legacy `tracker.Client` dependency.

Alias grep (`rg "linear-smoke|linear-contract|--linear-smoke|--linear-contract-check" README.md .scherzo/README.md docs src test`) shows the generic operator docs already point to tracker names, while code and tests retain explicit retirement diagnostics in `src/scherzo/main.gleam`, `src/scherzo/orchestrator/service.gleam`, `test/main_test.gleam`, `test/doctor_test.gleam`, and `test/orchestrator_service_doctor_test.gleam`.

Review-helper grep over `.scherzo/workflows`, `workflows/dogfood`, and `test` shows no production workflow YAML still invoking `run-lane` or `native-preflight`; the remaining matches are in the helper script itself, helper support modules, and tests that assert native workflows no longer route through those legacy paths.

Workstream/playbook grep confirms `src/scherzo/workstream/*.gleam`, `.scherzo/playbooks/standard-implementation.yaml`, and `.scherzo/playbooks/extended-implementation.yaml` are exercised by `test/workstream_playbook_test.gleam`, `test/workstream_start_test.gleam`, and related workstream tests.

## Candidate facade modules

All six named facade or dead duplicate candidates have now been removed.

- `src/scherzo/agent/pi_rpc.gleam`
  - Status: removed.
  - Evidence: tests now import `scherzo/pi/client` and `scherzo/pi/protocol` directly; facade import grep is empty.
- `src/scherzo/agent/runner.gleam`
  - Status: removed.
  - Evidence: worker runner tests now import `scherzo/agent/run_attempt` directly; facade import grep is empty.
- `src/scherzo/linear_triage.gleam`
  - Status: removed.
  - Evidence: there were no `src/` imports, and the only dedicated test covered the dead duplicate surface rather than the live `effect_runner` invalid-workflow path.
- `src/scherzo/state/workflow_checkpoint.gleam`
  - Status: removed.
  - Evidence: there were no `src/` imports, and the only remaining coverage targeted the dead compatibility wrapper rather than the live `src/scherzo/workflow_checkpoint.gleam` writer.
- `src/scherzo/config/ui_server.gleam`
  - Status: removed.
  - Evidence: `test/config_test.gleam` now exercises the redaction behavior through `scherzo/log` and resolved config data directly.
- `src/scherzo/tracker/conformance/linear_driver.gleam`
  - Status: removed.
  - Evidence: no `src/`, `test/`, docs, or helper references remained; the module only carried a stale implementation note.

## Tracker/task bridge

- `src/scherzo/tracker/adapter_legacy.gleam`
- `src/scherzo/tracker.gleam`
- `src/scherzo/tracker/issue.gleam`
- production call sites in `src/scherzo/orchestrator/daemon.gleam` and `src/scherzo/orchestrator/effect_runner.gleam`

Status: partially retired; one production bridge remains.

Reason: daemon candidate lookup and refresh now use task-native helpers in `src/scherzo/tracker/adapter.gleam`, and `src/scherzo/orchestrator/effect_runner.gleam` no longer imports `adapter_legacy`. The only remaining production bridge is `adapter_legacy.workflow_compat_client(...)` in `src/scherzo/orchestrator/daemon.gleam`, which still feeds the worker runner's legacy `tracker.Client` interface.

Named follow-up: **Runner tracker-client bridge retirement** — convert the worker runner path from `tracker.Client` to task-native adapter capabilities so `workflow_compat_client` can be deleted without mixing this cleanup slice with a broader runner/handoff refactor.

## Linear compatibility aliases

### Already retired from the preferred operator path

- `linear-smoke`
- `linear-contract`
- `--linear-smoke`
- `--linear-contract-check`

Status: generic docs and runbooks already prefer tracker names.

Evidence:

- `docs/runbooks/tracker-adapters.md` says these Linear names are no longer accepted operator paths.
- `src/scherzo/main.gleam` keeps explicit retirement hints such as `--linear-smoke was retired; use --tracker-smoke.`
- `src/scherzo/orchestrator/service.gleam` keeps explicit retired doctor-check diagnostics.
- `test/main_test.gleam`, `test/doctor_test.gleam`, and `test/orchestrator_service_doctor_test.gleam` cover the retirement behavior.

Decision: treat these as retired aliases with compatibility diagnostics still intentionally present. They do not block facade cleanup and do not need to be reintroduced.

## Review-helper legacy backends

- `.scherzo/workflows/scripts/scherzo-review` / `workflows/dogfood/scripts/scherzo-review`
  - `native-preflight`
    - Status: already retired.
    - Evidence: the helper prints `native-preflight is retired...`, and `test/review_artifacts_test.gleam` asserts that behavior.
  - `run-lane`
    - Status: retained only as a manual or historical validation path.
    - Evidence: matches exist in the helper implementation and helper tests, but not in production workflow YAML; tests such as `test/review_artifacts_test.gleam` and portability checks assert the native workflows do not embed `run-lane --lane`.
  - `heuristic`, `fixture`, and `external` agent backends
    - Status: retained only for fixture reproduction, historical contract coverage, and rollback/manual validation.
    - Evidence: matches are confined to the helper script, helper support modules, schemas, and tests.

Decision: production native review remains the required path. Any further deletion of manual/historical helper paths should be a dedicated green slice with helper-contract validation, not part of Milestone 1.

## Durable-state compatibility readers

The following compatibility surfaces are policy-gated and must stay readable until operators approve a migration, archive policy, or reinitialize story:

- `legacy_runs`
- `linear_command_*`
- `issue_id`
- `issue_identifier`
- task-ref compatibility fields used by workflow repair and retained-run recovery

Evidence:

- `src/scherzo/workflow_repair.gleam` still reconstructs and validates `issue_id`, `issue_identifier`, and `task_ref` evidence.
- retained-state and projection code still depend on these fields for recovery and provenance repair.

Policy decision: LIV-782 does not delete durable readers. The only acceptable outcomes in this issue are to document the retained policy, add migration follow-up work, or prove dual-read safety in a separate task.

## Workstream, playbook, and artifact-version surfaces

### Active

- `src/scherzo/workstream/*.gleam`
- `src/scherzo/ctl/workstream.gleam`
- `src/scherzo/ctl/workstream_start.gleam`
- `.scherzo/playbooks/standard-implementation.yaml`
- `.scherzo/playbooks/extended-implementation.yaml`

Reason: these files are part of the current workstream start/list/show flow and are covered by `test/workstream_playbook_test.gleam`, `test/workstream_start_test.gleam`, `test/workstream_handoff_emitter_test.gleam`, and related workstream tests.

### Active artifact-version references

- `scherzo.exec_plan_bundle.v1`
- `scherzo.code_change_bundle.v1`

Reason: both playbooks still declare these artifact types and current tests parse those playbooks successfully. They are not stale by themselves in LIV-782.

## Follow-up gates

The removal gate for the named facade candidates is now satisfied: the facade import grep is empty, the live production boundaries remain in place, and the remaining tracker bridge scope is explicitly narrowed to the runner tracker-client follow-up above.

Before deleting any additional candidate surface beyond this slice, require all of the following:

1. rerun the relevant `rg` import/usage grep and record the empty result or narrowed scope;
2. keep `src/scherzo/workflow_checkpoint.gleam`, task-native tracker APIs, native review lanes, and durable-state readers working;
3. run the validation suite appropriate for the touched slice, up to and including `direnv exec . gleam test`, and helper-contract validation if review helpers change.
