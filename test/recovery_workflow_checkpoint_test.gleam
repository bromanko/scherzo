import gleam/dict
import gleam/option.{type Option, None, Some}
import gleam/string
import legacy_ledger_fixtures
import scherzo/config
import scherzo/config/types as config_types
import scherzo/orchestrator/core
import scherzo/state/artifact_store
import scherzo/state/projection
import scherzo/state/record
import scherzo/state/recovery
import scherzo/step_artifact
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state
import scherzo/workflow_dag
import scherzo/workflow_outcome
import simplifile
import support/test_helpers

type RecoveryScenario {
  RecoveryScenario(
    root: String,
    store: artifact_store.Store,
    issue_fingerprint: String,
    run_id: String,
    run_root: String,
  )
}

type FinishedAndRunningCase {
  FinishedAndRunningCase(
    scenario: RecoveryScenario,
    finalized: recovery.WorkflowFinalization,
    artifact: step_artifact.StepArtifact,
  )
}

fn limits() -> config_types.ArtifactLimits {
  config_types.ArtifactLimits(
    command_stream_max_chars: 1000,
    template_field_max_chars: 1000,
    workflow_summary_max_chars: 4000,
  )
}

fn recovery_config() -> config_types.EffectiveConfig {
  config_types.EffectiveConfig(
    tracker: config.default_tracker_config(),
    polling: config.default_polling_config(),
    workspace: config_types.WorkspaceConfig(root: "test/tmp/workspaces"),
    hooks: config.default_hooks_config(),
    agent: config.default_agent_config(),
    pi: config.default_pi_config(),
    handoff: config.default_handoff_config(),
    linear_contract: config.default_linear_contract_config(),
    linear_commands: config.default_linear_command_config(),
    ui_server: config.default_ui_server_config(),
  )
}

fn issue() -> tracker_issue.Issue {
  issue_in_state("Todo")
}

fn issue_in_state(state: String) -> tracker_issue.Issue {
  tracker_issue.Issue(
    id: "issue-1",
    identifier: "LIV-59",
    title: "Durable checkpoints",
    description: None,
    priority: None,
    state: issue_state.from_string_unchecked(state),
    branch_name: None,
    url: None,
    labels: ["workflow:workflow-alpha"],
    blocked_by: [],
    blocked_by_complete: True,
    created_at: None,
    updated_at: None,
  )
}

fn parse_dag(content: String) -> workflow_dag.WorkflowDag {
  let assert Ok(dag) = workflow_dag.parse(content)
  dag
}

fn agent_dag() -> workflow_dag.WorkflowDag {
  parse_dag(
    "version: 1\nid: workflow-alpha\nsteps:\n  - id: a\n    kind: agent\n    prompt: a\n    workspace: main\n  - id: b\n    kind: agent\n    depends_on: [a]\n    prompt: b\n    workspace: main\n",
  )
}

fn fatal_dag() -> workflow_dag.WorkflowDag {
  parse_dag(
    "version: 1\nid: workflow-alpha\nsteps:\n  - id: fatal\n    kind: command\n    run: fatal\n    workspace: main\n",
  )
}

fn interrupted_command_dag() -> workflow_dag.WorkflowDag {
  parse_dag(
    "version: 1\nid: workflow-alpha\nsteps:\n  - id: command\n    kind: command\n    run: make changes\n    workspace: main\n",
  )
}

fn source_dag() -> workflow_dag.WorkflowDag {
  parse_dag(
    "version: 1\nid: workflow-alpha\nsteps:\n  - id: seed\n    kind: agent\n    prompt: seed\n    workspace: seed\n  - id: use_seed\n    kind: agent\n    depends_on: [seed]\n    prompt: use\n    workspace:\n      name: derived\n      from: seed\n",
  )
}

fn recovery_scenario(root: String, run_id: String) -> RecoveryScenario {
  recovery_scenario_with_fingerprint(
    root,
    run_id,
    core.issue_fingerprint(issue()),
  )
}

fn recovery_scenario_with_fingerprint(
  root: String,
  run_id: String,
  issue_fingerprint: String,
) -> RecoveryScenario {
  test_helpers.reset_dir(root)
  RecoveryScenario(
    root: root,
    store: artifact_store.new(root),
    issue_fingerprint: issue_fingerprint,
    run_id: run_id,
    run_root: root <> "/workflow-alpha/LIV-59/" <> run_id,
  )
}

fn legacy_stateful_todo_issue_fingerprint() -> String {
  "7:issue-1|6:LIV-59|19:Durable checkpoints|none|none|4:Todo|none|4:true|"
}

fn workspace_path(
  scenario: RecoveryScenario,
  workspace_name: String,
) -> String {
  scenario.run_root <> "/workspaces/" <> workspace_name
}

fn ensure_workspace(scenario: RecoveryScenario, workspace_name: String) -> Nil {
  let assert Ok(Nil) =
    simplifile.create_directory_all(workspace_path(scenario, workspace_name))
  Nil
}

fn command_artifact(
  step_id: String,
  exit_code: Int,
  stdout: String,
  stderr: String,
) -> step_artifact.StepArtifact {
  step_artifact.from_command_result(
    step_id,
    exit_code,
    stdout,
    stderr,
    False,
    [],
    limits(),
  )
}

fn write_artifact(
  scenario: RecoveryScenario,
  step_id: String,
  artifact: step_artifact.StepArtifact,
) -> artifact_store.ArtifactRef {
  write_artifact_attempt(scenario, step_id, 1, artifact)
}

fn write_artifact_attempt(
  scenario: RecoveryScenario,
  step_id: String,
  attempt_index: Int,
  artifact: step_artifact.StepArtifact,
) -> artifact_store.ArtifactRef {
  let assert Ok(stored) =
    artifact_store.write_step_artifact(
      scenario.store,
      scenario.run_id,
      "workflow-alpha",
      step_id,
      attempt_index,
      artifact,
    )
  stored
}

fn given_workflow_started(
  scenario: RecoveryScenario,
  sequence: Int,
) -> record.LedgerRecord {
  record.new(
    sequence,
    sequence,
    record.WorkflowRunStarted(
      scenario.run_id,
      "workflow-alpha",
      "wf-sha",
      "issue-1",
      "LIV-59",
      scenario.issue_fingerprint,
      0,
      scenario.run_root,
    ),
  )
}

fn given_step_prepared(
  scenario: RecoveryScenario,
  sequence: Int,
  step_id: String,
  workspace_name: String,
  source_workspace_name: Option(String),
  source_workspace_path: Option(String),
) -> record.LedgerRecord {
  record.new(
    sequence,
    sequence,
    record.StepAttemptPrepared(
      scenario.run_id,
      "workflow-alpha",
      step_id,
      1,
      workspace_name,
      workspace_path(scenario, workspace_name),
      scenario.run_root,
      source_workspace_name,
      source_workspace_path,
    ),
  )
}

fn given_step_started(
  scenario: RecoveryScenario,
  sequence: Int,
  step_id: String,
) -> record.LedgerRecord {
  record.new(
    sequence,
    sequence,
    record.StepAttemptStarted(
      scenario.run_id,
      "workflow-alpha",
      step_id,
      1,
      "workflow-step-" <> scenario.run_id <> "-" <> step_id <> "-a1",
      None,
      False,
    ),
  )
}

fn given_step_finished(
  scenario: RecoveryScenario,
  sequence: Int,
  step_id: String,
  status: String,
  stored: artifact_store.ArtifactRef,
  workspace_name: String,
) -> record.LedgerRecord {
  given_step_finished_attempt(
    scenario,
    sequence,
    step_id,
    1,
    status,
    stored,
    workspace_name,
  )
}

fn given_step_finished_attempt(
  scenario: RecoveryScenario,
  sequence: Int,
  step_id: String,
  attempt_index: Int,
  status: String,
  stored: artifact_store.ArtifactRef,
  workspace_name: String,
) -> record.LedgerRecord {
  record.new(
    sequence,
    sequence,
    record.StepAttemptFinished(
      scenario.run_id,
      "workflow-alpha",
      step_id,
      attempt_index,
      status,
      stored.ref,
      stored.sha256,
      workspace_name,
      workspace_path(scenario, workspace_name),
      0,
      0,
    ),
  )
}

fn given_step_superseded(
  scenario: RecoveryScenario,
  sequence: Int,
  step_id: String,
  attempt_index: Int,
  superseded_by_attempt_index: Int,
) -> record.LedgerRecord {
  record.new(
    sequence,
    sequence,
    record.StepAttemptSuperseded(
      scenario.run_id,
      "workflow-alpha",
      step_id,
      attempt_index,
      superseded_by_attempt_index,
      "retry_accepted",
    ),
  )
}

fn given_missing_step_finished(
  scenario: RecoveryScenario,
  sequence: Int,
  step_id: String,
  status: String,
  artifact_ref: String,
  artifact_sha: String,
  workspace_name: String,
) -> record.LedgerRecord {
  record.new(
    sequence,
    sequence,
    record.StepAttemptFinished(
      scenario.run_id,
      "workflow-alpha",
      step_id,
      1,
      status,
      artifact_ref,
      artifact_sha,
      workspace_name,
      workspace_path(scenario, workspace_name),
      0,
      0,
    ),
  )
}

fn given_step_recovery_started(
  scenario: RecoveryScenario,
  sequence: Int,
  step_id: String,
) -> record.LedgerRecord {
  record.new(
    sequence,
    sequence,
    record.WorkflowStepRecoveryStarted(
      scenario.run_id,
      "workflow-alpha",
      step_id,
      1,
      1,
      "recovery-session-1",
      Some("test-model"),
      "artifacts://prompt.md",
    ),
  )
}

fn given_step_recovery_finished(
  scenario: RecoveryScenario,
  sequence: Int,
  step_id: String,
  result: String,
  retry_attempt_index: Option(Int),
) -> record.LedgerRecord {
  record.new(
    sequence,
    sequence,
    record.WorkflowStepRecoveryFinished(
      scenario.run_id,
      "workflow-alpha",
      step_id,
      1,
      1,
      "recovery-session-1",
      result,
      "summary",
      "reason",
      retry_attempt_index,
    ),
  )
}

fn finalize_resume(
  scenario: RecoveryScenario,
  folded: projection.Projection,
  dag: workflow_dag.WorkflowDag,
) -> recovery.WorkflowFinalization {
  let assert [candidate] = recovery.workflow_candidates(folded)
  let assert Ok(finalized) =
    recovery.finalize_workflow_candidates(
      folded,
      [candidate],
      current_observations(scenario, dag),
      scenario.store,
      99,
    )
  finalized
}

fn finalize_with_mode(
  scenario: RecoveryScenario,
  folded: projection.Projection,
  dag: workflow_dag.WorkflowDag,
  mode: recovery.WorkflowRecoveryMode,
) -> recovery.WorkflowFinalization {
  let assert [candidate] = recovery.workflow_candidates(folded)
  let assert Ok(finalized) =
    recovery.finalize_workflow_candidates_with_mode(
      folded,
      [candidate],
      current_observations(scenario, dag),
      scenario.store,
      99,
      mode,
    )
  finalized
}

fn current_observations(
  scenario: RecoveryScenario,
  dag: workflow_dag.WorkflowDag,
) {
  current_observations_with_issue(scenario, dag, issue())
}

fn current_observations_with_issue(
  scenario: RecoveryScenario,
  dag: workflow_dag.WorkflowDag,
  current_issue: tracker_issue.Issue,
) {
  dict.from_list([
    #(
      scenario.run_id,
      recovery.CurrentWorkflow(
        current_issue,
        "workflow-alpha",
        "wf-sha",
        core.issue_fingerprint(current_issue),
        dag,
        scenario.root,
      ),
    ),
  ])
}

fn finished_a_running_b(root: String) -> FinishedAndRunningCase {
  let scenario = recovery_scenario(root, "run-1")
  ensure_workspace(scenario, "main")
  let artifact = command_artifact("a", 0, "done", "")
  let stored = write_artifact(scenario, "a", artifact)
  let folded =
    projection.fold([
      given_workflow_started(scenario, 1),
      given_step_prepared(
        scenario,
        2,
        "a",
        "main",
        Some("seed"),
        Some("root/seed"),
      ),
      given_step_started(scenario, 3, "a"),
      given_step_finished(scenario, 4, "a", "completed", stored, "main"),
      given_step_prepared(scenario, 5, "b", "main", None, None),
      given_step_started(scenario, 6, "b"),
    ])

  FinishedAndRunningCase(
    scenario: scenario,
    finalized: finalize_resume(scenario, folded, agent_dag()),
    artifact: artifact,
  )
}

pub fn old_workflow_checkpoint_records_recover_active_candidate_test() {
  let workflow_started =
    decode_checkpoint_record(legacy_ledger_fixtures.workflow_run_started_v2(
      "old-1",
      1,
    ))
  let pi_session_recorded =
    decode_checkpoint_record(
      legacy_ledger_fixtures.step_attempt_pi_session_recorded_v2("old-step", 2),
    )
  let folded = projection.fold([workflow_started, pi_session_recorded])

  let assert [
    recovery.WorkflowRecoveryCandidate(
      run_id: "run-1",
      workflow_id: "execplan",
      workflow_fingerprint: "wf-old",
      issue_id: "issue-1",
      issue_identifier: "LIV-266",
      task_ref: record.TaskRefFields(
        task_backend_kind: "linear",
        task_remote_id: "issue-1",
        task_key: Some("LIV-266"),
        task_url: None,
      ),
      issue_fingerprint: "fp-old",
      observed_updated_at_ms: 10,
      run_root: "test/tmp/run-root",
      recovery_evidence: workflow_outcome.NoStepRecovery,
      contract_input_manifest: None,
      contract_output_manifest: None,
      attempts: [
        projection.StepAttemptRunning(
          run_id: "run-1",
          workflow_id: "execplan",
          step_id: "step-1",
          attempt_index: 1,
          workspace_name: "main",
          workspace_path: "test/tmp/run-root/workspaces/main",
          pi_session_id: Some("pi-session-1"),
          pi_session_file: Some("state/sessions/run-1/step-1.json"),
          pi_session_fact_count: 1,
          ..,
        ),
      ],
    ),
  ] = recovery.workflow_candidates(folded)
}

fn decode_checkpoint_record(line: String) -> record.LedgerRecord {
  let assert Ok(decoded) = record.decode_string(line)
  decoded
}

pub fn workflow_recovery_candidate_marks_started_same_run_step_recovery_test() {
  let scenario =
    recovery_scenario(
      "test/tmp/recovery-workflow-step-recovery-started",
      "run-started",
    )
  let folded =
    projection.fold([
      given_workflow_started(scenario, 1),
      given_step_recovery_started(scenario, 2, "a"),
    ])

  let assert [candidate] = recovery.workflow_candidates(folded)
  assert candidate.recovery_evidence == workflow_outcome.StepRecoveryRan
}

pub fn workflow_recovery_candidate_marks_retry_requested_same_run_step_recovery_test() {
  let scenario =
    recovery_scenario(
      "test/tmp/recovery-workflow-step-recovery-finished",
      "run-finished",
    )
  let folded =
    projection.fold([
      given_workflow_started(scenario, 1),
      given_step_recovery_started(scenario, 2, "a"),
      given_step_recovery_finished(scenario, 3, "a", "retry_requested", Some(2)),
    ])

  let assert [candidate] = recovery.workflow_candidates(folded)
  assert candidate.recovery_evidence
    == workflow_outcome.StepRecoveryRetryRequested
}

pub fn workflow_recovery_ignores_other_run_step_recovery_records_test() {
  let scenario =
    recovery_scenario(
      "test/tmp/recovery-workflow-step-recovery-other-run",
      "run-clean",
    )
  ensure_workspace(scenario, "main")
  let artifact = command_artifact("a", 0, "done", "")
  let stored = write_artifact(scenario, "a", artifact)
  let folded =
    projection.fold([
      given_workflow_started(scenario, 1),
      given_step_prepared(scenario, 2, "a", "main", None, None),
      given_step_finished(scenario, 3, "a", "completed", stored, "main"),
      record.new(
        4,
        4,
        record.WorkflowStepRecoveryStarted(
          "other-run",
          "workflow-alpha",
          "a",
          1,
          1,
          "recovery-session-other",
          Some("test-model"),
          "artifacts://other-prompt.md",
        ),
      ),
    ])

  let assert [candidate] = recovery.workflow_candidates(folded)
  assert candidate.recovery_evidence == workflow_outcome.NoStepRecovery

  let finalized = finalize_resume(scenario, folded, agent_dag())
  let resumption = expect_single_resumption(finalized)
  assert resumption.recovery_evidence == workflow_outcome.NoStepRecovery
}

pub fn workflow_checkpoint_candidates_carry_contract_manifest_refs_test() {
  let folded =
    projection.fold([
      record.with_id(
        "workflow-started",
        1,
        record.WorkflowRunStarted(
          run_id: "run-1",
          workflow_id: "implementation",
          workflow_fingerprint: "wf-1",
          issue_id: "issue-1",
          issue_identifier: "LIV-1",
          issue_fingerprint: "issue-fp",
          observed_updated_at_ms: 1,
          run_root: "test/tmp/run-root",
        ),
      ),
      record.with_id(
        "inputs",
        2,
        record.WorkflowRunInputsRecorded(
          run_id: "run-1",
          workflow_id: "implementation",
          workflow_fingerprint: "wf-1",
          artifact_ref: "runs/run-1/inputs.v1.json",
          artifact_sha256: "sha-in",
          artifact_bytes: 10,
        ),
      ),
      record.with_id(
        "outputs",
        3,
        record.WorkflowRunOutputsRecorded(
          run_id: "run-1",
          workflow_id: "implementation",
          workflow_fingerprint: "wf-1",
          artifact_ref: "runs/run-1/outputs.v1.json",
          artifact_sha256: "sha-out",
          artifact_bytes: 20,
        ),
      ),
    ])

  let assert [candidate] = recovery.workflow_candidates(folded)
  let assert Some(input_manifest) = candidate.contract_input_manifest
  assert input_manifest.ref == "runs/run-1/inputs.v1.json"
  let assert Some(output_manifest) = candidate.contract_output_manifest
  assert output_manifest.sha256 == "sha-out"
}

pub fn workflow_recovery_restores_finished_artifacts_test() {
  let setup = finished_a_running_b("test/tmp/recovery-workflow-artifact")
  let resumption = expect_single_resumption(setup.finalized)

  expect_recovered_artifact(resumption, "a", setup.artifact)
  assert setup.finalized.warnings == []
}

pub fn workflow_recovery_uses_final_attempt_artifact_and_ignores_superseded_test() {
  let scenario =
    recovery_scenario("test/tmp/recovery-workflow-final-attempt", "run-1")
  ensure_workspace(scenario, "main")
  let superseded_artifact = command_artifact("a", 0, "superseded", "")
  let final_artifact = command_artifact("a", 0, "accepted", "")
  let superseded_stored =
    write_artifact_attempt(scenario, "a", 1, superseded_artifact)
  let final_stored = write_artifact_attempt(scenario, "a", 2, final_artifact)
  let folded =
    projection.fold([
      given_workflow_started(scenario, 1),
      given_step_finished_attempt(
        scenario,
        2,
        "a",
        1,
        "completed",
        superseded_stored,
        "main",
      ),
      given_step_superseded(scenario, 3, "a", 1, 2),
      given_step_finished_attempt(
        scenario,
        4,
        "a",
        2,
        "completed",
        final_stored,
        "main",
      ),
    ])

  let finalized = finalize_resume(scenario, folded, agent_dag())
  let resumption = expect_single_resumption(finalized)

  expect_recovered_artifact(resumption, "a", final_artifact)
  expect_next_attempt(resumption, "a", 3)
}

pub fn workflow_recovery_restores_finished_workspace_metadata_test() {
  let setup = finished_a_running_b("test/tmp/recovery-workflow-workspace")
  let resumption = expect_single_resumption(setup.finalized)

  expect_recovered_workspace(
    resumption,
    "main",
    workspace_path(setup.scenario, "main"),
    setup.scenario.run_root,
    Some("seed"),
    Some("root/seed"),
  )
}

pub fn workflow_recovery_interrupts_running_attempts_and_advances_indexes_test() {
  let setup = finished_a_running_b("test/tmp/recovery-workflow-interrupted")
  let resumption = expect_single_resumption(setup.finalized)

  expect_next_attempt(resumption, "a", 2)
  expect_next_attempt(resumption, "b", 2)
  expect_interrupted_record(
    setup.finalized,
    setup.scenario.run_id,
    "b",
    1,
    "daemon_restart",
  )
}

pub fn workflow_recovery_missing_artifact_parks_without_resumption_test() {
  let scenario =
    recovery_scenario(
      "test/tmp/recovery-workflow-missing-artifact",
      "run-missing",
    )
  let folded =
    projection.fold([
      given_workflow_started(scenario, 1),
      given_step_prepared(scenario, 2, "a", "main", None, None),
      given_missing_step_finished(
        scenario,
        3,
        "a",
        "completed",
        "runs/run-missing/a/attempt-1.json",
        "missing-sha",
        "main",
      ),
    ])

  let finalized = finalize_resume(scenario, folded, agent_dag())

  expect_no_resumption(finalized)
  expect_park_reason(finalized, "artifact_recovery_failed")
  expect_workflow_interrupted(finalized, scenario.run_id)
}

pub fn workflow_recovery_disabled_mode_parks_resumable_run_test() {
  let scenario =
    recovery_scenario("test/tmp/recovery-workflow-disabled", "run-disabled")
  let artifact = command_artifact("a", 0, "done", "")
  let stored = write_artifact(scenario, "a", artifact)
  let folded =
    projection.fold([
      given_workflow_started(scenario, 1),
      given_step_prepared(scenario, 2, "a", "main", None, None),
      given_step_finished(scenario, 3, "a", "completed", stored, "main"),
    ])

  let finalized =
    finalize_with_mode(
      scenario,
      folded,
      agent_dag(),
      recovery.ParkRecoveredWorkflows,
    )

  expect_no_resumption(finalized)
  expect_park_reason(finalized, "workflow_recovery_disabled")
}

pub fn workflow_recovery_ignores_handoff_state_only_transition_test() {
  let scenario =
    recovery_scenario_with_fingerprint(
      "test/tmp/recovery-workflow-state-transition",
      "run-state",
      legacy_stateful_todo_issue_fingerprint(),
    )
  let folded = projection.fold([given_workflow_started(scenario, 1)])
  let assert [candidate] = recovery.workflow_candidates(folded)
  let assert Ok(finalized) =
    recovery.finalize_workflow_candidates_with_config(
      folded,
      [candidate],
      current_observations_with_issue(
        scenario,
        agent_dag(),
        issue_in_state("In Progress"),
      ),
      scenario.store,
      99,
      recovery_config(),
    )

  let resumption = expect_single_resumption(finalized)
  assert resumption.issue.state
    == issue_state.from_string_unchecked("In Progress")
  expect_no_appended_records(finalized)
  assert finalized.warnings == []
}

pub fn workflow_recovery_parks_terminal_state_only_transition_test() {
  let scenario =
    recovery_scenario_with_fingerprint(
      "test/tmp/recovery-workflow-terminal-state-transition",
      "run-terminal-state",
      legacy_stateful_todo_issue_fingerprint(),
    )
  let folded = projection.fold([given_workflow_started(scenario, 1)])
  let assert [candidate] = recovery.workflow_candidates(folded)
  let assert Ok(finalized) =
    recovery.finalize_workflow_candidates_with_config(
      folded,
      [candidate],
      current_observations_with_issue(
        scenario,
        agent_dag(),
        issue_in_state("Done"),
      ),
      scenario.store,
      99,
      recovery_config(),
    )

  expect_no_resumption(finalized)
  expect_park_reason(finalized, "issue_state_drift:terminal_state")
  assert !has_park_reason(finalized.records_to_append, "workflow_drift")
  assert !has_park_reason(finalized.records_to_append, "issue_content_drift")
  assert finalized.warnings
    == [
      "workflow_recovery_parked_issue_state_drift:run-terminal-state:terminal_state:Done",
    ]
}

pub fn workflow_recovery_parks_non_active_state_only_transition_test() {
  let scenario =
    recovery_scenario_with_fingerprint(
      "test/tmp/recovery-workflow-non-active-state-transition",
      "run-non-active-state",
      legacy_stateful_todo_issue_fingerprint(),
    )
  let folded = projection.fold([given_workflow_started(scenario, 1)])
  let assert [candidate] = recovery.workflow_candidates(folded)
  let assert Ok(finalized) =
    recovery.finalize_workflow_candidates_with_config(
      folded,
      [candidate],
      current_observations_with_issue(
        scenario,
        agent_dag(),
        issue_in_state("Triage"),
      ),
      scenario.store,
      99,
      recovery_config(),
    )

  expect_no_resumption(finalized)
  expect_park_reason(finalized, "issue_state_drift:non_active_state")
  assert !has_park_reason(finalized.records_to_append, "workflow_drift")
  assert !has_park_reason(finalized.records_to_append, "issue_content_drift")
  assert finalized.warnings
    == [
      "workflow_recovery_parked_issue_state_drift:run-non-active-state:non_active_state:Triage",
    ]
}

pub fn workflow_recovery_parks_issue_content_drift_with_issue_reason_test() {
  let scenario =
    recovery_scenario("test/tmp/recovery-workflow-issue-drift", "run-issue")
  let folded = projection.fold([given_workflow_started(scenario, 1)])
  let assert [candidate] = recovery.workflow_candidates(folded)
  let changed_issue = tracker_issue.Issue(..issue(), title: "Changed title")
  let assert Ok(finalized) =
    recovery.finalize_workflow_candidates(
      folded,
      [candidate],
      current_observations_with_issue(scenario, agent_dag(), changed_issue),
      scenario.store,
      99,
    )

  expect_no_resumption(finalized)
  expect_park_reason(finalized, "issue_content_drift:issue_fingerprint_changed")
  assert !has_park_reason(finalized.records_to_append, "workflow_drift")
  assert finalized.warnings
    == [
      "workflow_recovery_parked_issue_content_drift:run-issue:issue_fingerprint_changed",
    ]
}

pub fn workflow_recovery_parks_workflow_drift_with_definition_reason_test() {
  let scenario =
    recovery_scenario("test/tmp/recovery-workflow-definition-drift", "run-wf")
  let folded = projection.fold([given_workflow_started(scenario, 1)])
  let assert [candidate] = recovery.workflow_candidates(folded)
  let observations =
    dict.from_list([
      #(
        scenario.run_id,
        recovery.CurrentWorkflow(
          issue(),
          "workflow-alpha",
          "wf-changed",
          core.issue_fingerprint(issue()),
          agent_dag(),
          scenario.root,
        ),
      ),
    ])
  let assert Ok(finalized) =
    recovery.finalize_workflow_candidates(
      folded,
      [candidate],
      observations,
      scenario.store,
      99,
    )

  expect_no_resumption(finalized)
  expect_park_reason(
    finalized,
    "workflow_definition_drift:workflow_fingerprint_changed",
  )
  assert !has_park_reason(finalized.records_to_append, "issue_content_drift")
  assert finalized.warnings
    == [
      "workflow_recovery_parked_workflow_definition_drift:run-wf:workflow_fingerprint_changed",
    ]
}

pub fn workflow_recovery_parks_workflow_id_drift_with_definition_reason_test() {
  let scenario =
    recovery_scenario("test/tmp/recovery-workflow-id-drift", "run-wf-id")
  let folded = projection.fold([given_workflow_started(scenario, 1)])
  let assert [candidate] = recovery.workflow_candidates(folded)
  let observations =
    dict.from_list([
      #(
        scenario.run_id,
        recovery.CurrentWorkflow(
          issue(),
          "workflow-beta",
          "wf-sha",
          core.issue_fingerprint(issue()),
          agent_dag(),
          scenario.root,
        ),
      ),
    ])
  let assert Ok(finalized) =
    recovery.finalize_workflow_candidates(
      folded,
      [candidate],
      observations,
      scenario.store,
      99,
    )

  expect_no_resumption(finalized)
  expect_park_reason(finalized, "workflow_definition_drift:workflow_id_changed")
  assert !has_park_reason(finalized.records_to_append, "issue_content_drift")
  assert finalized.warnings
    == [
      "workflow_recovery_parked_workflow_definition_drift:run-wf-id:workflow_id_changed",
    ]
}

pub fn workflow_recovery_parks_interrupted_command_attempts_test() {
  let scenario =
    recovery_scenario(
      "test/tmp/recovery-workflow-interrupted-command",
      "run-command",
    )
  let folded =
    projection.fold([
      given_workflow_started(scenario, 1),
      given_step_prepared(scenario, 2, "command", "main", None, None),
      given_step_started(scenario, 3, "command"),
    ])

  let finalized = finalize_resume(scenario, folded, interrupted_command_dag())

  expect_no_resumption(finalized)
  expect_park_reason(finalized, "unsafe_interrupted_command_step")
  expect_workflow_interrupted(finalized, scenario.run_id)
}

pub fn workflow_recovery_parks_missing_recovered_source_workspace_test() {
  let scenario =
    recovery_scenario(
      "test/tmp/recovery-workflow-missing-source-workspace",
      "run-source",
    )
  let artifact = command_artifact("seed", 0, "done", "")
  let stored = write_artifact(scenario, "seed", artifact)
  let folded =
    projection.fold([
      given_workflow_started(scenario, 1),
      given_step_prepared(scenario, 2, "seed", "seed", None, None),
      given_step_finished(scenario, 3, "seed", "completed", stored, "seed"),
    ])

  let finalized = finalize_resume(scenario, folded, source_dag())

  expect_no_resumption(finalized)
  expect_park_reason(finalized, "workspace_recovery_failed")
  expect_workflow_interrupted(finalized, scenario.run_id)
}

pub fn workflow_recovery_validates_failed_fatal_artifact_without_promoting_workspace_test() {
  let scenario =
    recovery_scenario("test/tmp/recovery-workflow-failed-fatal", "run-fatal")
  let artifact = command_artifact("fatal", 1, "", "boom")
  let stored = write_artifact(scenario, "fatal", artifact)
  let folded =
    projection.fold([
      given_workflow_started(scenario, 1),
      given_step_prepared(scenario, 2, "fatal", "main", None, None),
      given_step_finished(scenario, 3, "fatal", "failed_fatal", stored, "main"),
    ])

  let finalized = finalize_resume(scenario, folded, fatal_dag())
  let resumption = expect_single_resumption(finalized)

  expect_recovered_artifact(resumption, "fatal", artifact)
  expect_no_recovered_workspace(resumption, "main")
  expect_next_attempt(resumption, "fatal", 2)
  expect_no_appended_records(finalized)
}

fn expect_single_resumption(
  finalized: recovery.WorkflowFinalization,
) -> recovery.RecoveredWorkflowRun {
  let assert [resumption] = finalized.resumptions
  resumption
}

fn expect_no_resumption(finalized: recovery.WorkflowFinalization) -> Nil {
  assert finalized.resumptions == []
}

fn expect_recovered_artifact(
  resumption: recovery.RecoveredWorkflowRun,
  step_id: String,
  expected: step_artifact.StepArtifact,
) -> Nil {
  let assert Ok(recovered_artifact) =
    dict.get(resumption.completed_artifacts, step_id)
  assert recovered_artifact == expected
}

fn expect_recovered_workspace(
  resumption: recovery.RecoveredWorkflowRun,
  workspace_name: String,
  expected_path: String,
  expected_run_root: String,
  expected_source_name: Option(String),
  expected_source_path: Option(String),
) -> Nil {
  let assert Ok(recovered_workspace) =
    dict.get(resumption.completed_workspaces, workspace_name)
  assert recovered_workspace.path == expected_path
  assert recovered_workspace.run_root == expected_run_root
  assert recovered_workspace.source_workspace_name == expected_source_name
  assert recovered_workspace.source_workspace_path == expected_source_path
}

fn expect_no_recovered_workspace(
  resumption: recovery.RecoveredWorkflowRun,
  workspace_name: String,
) -> Nil {
  assert dict.get(resumption.completed_workspaces, workspace_name) == Error(Nil)
}

fn expect_next_attempt(
  resumption: recovery.RecoveredWorkflowRun,
  step_id: String,
  expected: Int,
) -> Nil {
  assert dict.get(resumption.next_attempt_indexes, step_id) == Ok(expected)
}

fn expect_interrupted_record(
  finalized: recovery.WorkflowFinalization,
  run_id: String,
  step_id: String,
  attempt_index: Int,
  reason: String,
) -> Nil {
  let assert [appended] = finalized.records_to_append
  let assert record.StepAttemptInterrupted(
    run_id: status_run_id,
    step_id: status_step_id,
    attempt_index: status_attempt_index,
    reason: status_reason,
    ..,
  ) = appended.body
  assert status_run_id == run_id
  assert status_step_id == step_id
  assert status_attempt_index == attempt_index
  assert status_reason == reason
}

fn expect_park_reason(
  finalized: recovery.WorkflowFinalization,
  reason: String,
) -> Nil {
  assert has_park_reason(finalized.records_to_append, reason)
}

fn expect_workflow_interrupted(
  finalized: recovery.WorkflowFinalization,
  run_id: String,
) -> Nil {
  assert has_workflow_interrupted(finalized.records_to_append, run_id)
}

fn expect_no_appended_records(finalized: recovery.WorkflowFinalization) -> Nil {
  assert finalized.records_to_append == []
}

fn has_park_reason(records: List(record.LedgerRecord), reason: String) -> Bool {
  case records {
    [] -> False
    [record, ..rest] ->
      case record.body {
        record.IssueParkedV2(reason: parked_reason, ..) ->
          parked_reason == reason
          || string.starts_with(parked_reason, reason <> ":")
          || has_park_reason(rest, reason)
        _ -> has_park_reason(rest, reason)
      }
  }
}

fn has_workflow_interrupted(
  records: List(record.LedgerRecord),
  run_id: String,
) -> Bool {
  case records {
    [] -> False
    [record, ..rest] ->
      case record.body {
        record.WorkflowRunInterrupted(run_id: status_run_id, ..) ->
          status_run_id == run_id || has_workflow_interrupted(rest, run_id)
        _ -> has_workflow_interrupted(rest, run_id)
      }
  }
}
