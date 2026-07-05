import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order.{Gt, Lt}
import gleam/string
import scherzo/config
import scherzo/config/types as config_types
import scherzo/control/command
import scherzo/control/protocol
import scherzo/control/query/types as query_types
import scherzo/model_config
import scherzo/session/event
import scherzo/session/tokens as session_tokens
import scherzo/state/projection
import scherzo/state/record
import scherzo/tracker/state as issue_state
import scherzo/workflow_completion_policy
import scherzo/workflow_dag
import simplifile
import yay

const fixture_root = "test/fixtures/schema"

type LedgerExample {
  LedgerExample(
    constructor: String,
    kind: String,
    ledger_record: record.LedgerRecord,
  )
}

type RequestExample {
  RequestExample(constructor: String, request: protocol.Request)
}

pub fn ledger_record_body_manifest_roundtrips_every_constructor_test() {
  let examples = ledger_examples()

  assert constructor_names_from_source(
      "src/scherzo/state/record.gleam",
      "RecordBody",
    )
    == list.map(examples, fn(example) { example.constructor })

  list.each(examples, fn(example) {
    assert record.kind(example.ledger_record.body) == example.kind
    let assert Ok(decoded) =
      example.ledger_record
      |> record.to_string
      |> record.decode_string
    assert decoded == example.ledger_record
  })
}

pub fn ledger_record_jsonl_fixture_matches_roundtrip_examples_test() {
  let expected =
    ledger_examples()
    |> list.map(fn(example) { record.to_string(example.ledger_record) })

  assert jsonl_fixture_lines("ledger_records_v2.jsonl") == expected
}

pub fn projection_snapshot_golden_fixture_decodes_and_reencodes_test() {
  let fixture = fixture_text("projection_snapshot_v2.json") |> string.trim
  let assert Ok(decoded) = projection.decode_string(fixture)

  assert decoded == projection_fixture_projection()
  assert projection.to_string(decoded) == fixture
}

pub fn control_protocol_request_manifest_and_golden_fixture_test() {
  let examples = request_examples()

  assert constructor_names_from_source(
      "src/scherzo/control/protocol.gleam",
      "Request",
    )
    == list.map(examples, fn(example) { example.constructor })

  let encoded =
    examples
    |> list.map(fn(example) { protocol.request_to_string(example.request) })

  assert jsonl_fixture_lines("control_requests_v1.jsonl") == encoded

  list.each(examples, fn(example) {
    let assert Ok(decoded) =
      example.request
      |> protocol.request_to_string
      |> protocol.decode_request
    assert decoded == example.request
  })
}

pub fn control_protocol_response_golden_fixture_test() {
  let lines = jsonl_fixture_lines("control_responses_v1.jsonl")
  assert lines == control_response_fixture_lines()

  let assert [ping_line, sessions_line, events_line, command_line, error_line] =
    lines

  let assert Ok(Nil) = protocol.decode_ping_response(ping_line)

  let assert Ok(snapshot) =
    protocol.decode_list_sessions_snapshot_response(sessions_line)
  assert snapshot == event.SessionList([sample_session()], 2500)

  let assert Ok(page) = protocol.decode_get_events_response(events_line)
  assert page == sample_event_page()

  let assert Ok(result) = protocol.decode_command_result_response(command_line)
  assert result == sample_command_result()
  assert command.status_to_string(result.status) == "rejected"

  let assert Ok(error_response) = protocol.decode_response(error_line)
  assert error_response
    == protocol.Response(
      id: "res-error",
      ok: False,
      data: None,
      error: Some(protocol.ErrorBody(
        code: "unknown_command",
        message: "unknown command: delete_everything",
      )),
    )
}

pub fn workflow_dag_yaml_fixture_parses_documented_schema_shape_test() {
  let assert Ok(source) =
    simplifile.read(fixture_root <> "/workflow_dag_complete.yaml")
  let assert Ok(dag) = workflow_dag.parse(source)

  assert workflow_dag.id(dag) == "implementation"
  assert workflow_dag.description(dag)
    == Some("Two-step implementation workflow fixture")
  assert workflow_dag.max_parallel_steps(dag) == 2

  let assert [implement_step, test_step] = workflow_dag.steps(dag)
  assert implement_step.id == "implement"
  assert implement_step.workspace == workflow_dag.WorkspaceRef("main", None)
  assert implement_step.model_settings.model
    == Some("github-copilot/gpt-5.1-codex")
  assert implement_step.model_settings.thinking
    == Some(model_config.ThinkingHigh)
  let assert workflow_dag.AgentStep(
    workflow_dag.PromptFile("prompts/implement.md"),
    None,
  ) = implement_step.kind

  assert test_step.id == "test"
  assert test_step.depends_on == ["implement"]
  assert test_step.workspace
    == workflow_dag.WorkspaceRef("test-workspace", Some("main"))
  assert test_step.on_failure == workflow_dag.ContinueWorkflow
  let assert workflow_dag.CommandStep(
    run: "direnv exec . gleam test",
    timeout_ms: Some(120_000),
  ) = test_step.kind
}

pub fn orchestrator_config_yaml_fixture_parses_schema_shape_test() {
  let assert Ok(source) =
    simplifile.read(fixture_root <> "/orchestrator_config_complete.yaml")
  let assert Ok([document]) = yay.parse_string(source)
  let assert Ok(orchestrator) =
    config.resolve_orchestrator_root(
      yay.document_root(document),
      "test/tmp/schema_guardrail/scherzo.yaml",
      schema_env,
    )

  let effective = orchestrator.effective
  assert effective.tracker.api_key == Some("linearkey")
  assert effective.tracker.project_slug == Some("SCHEMA")
  assert issue_state.to_strings(effective.tracker.active_states)
    == ["Todo", "In Progress"]
  assert issue_state.to_strings(effective.tracker.terminal_states)
    == ["Done", "Canceled"]
  assert effective.polling.interval_ms == 45_000
  assert effective.control.command_timeout_ms == 75_000
  assert string.ends_with(
    effective.workspace.root,
    "/test/tmp/schema_guardrail/workspaces",
  )

  assert effective.hooks == config.default_hooks_config()

  assert effective.agent.max_concurrent_agents == 3
  assert effective.agent.max_turns == 12
  assert effective.agent.max_sessions_per_issue == 2
  let assert Ok(todo_limit) =
    dict.get(
      effective.agent.max_concurrent_agents_by_state,
      issue_state.key_from_string("todo"),
    )
  assert todo_limit == 1

  assert effective.pi.command == "pi --mode rpc --rpc-message-updates off"
  assert effective.pi.turn_timeout_ms == 600_000
  assert effective.pi.read_timeout_ms == 7000
  assert effective.pi.stall_timeout_ms == 120_000
  assert effective.pi.auto_retry == False
  assert effective.pi.ui_request_policy == config_types.Operator
  assert effective.pi.ui_request_timeout_ms == 10_000
  assert effective.pi.compatibility_probe == False
  assert effective.pi.argv_command
    == Some(
      config_types.PiArgvCommand(
        executable: "pi",
        args: ["--mode", "rpc", "--rpc-message-updates", "off"],
        env: [#("PI_LOG", "debug")],
      ),
    )
  assert effective.pi.session_persistence.enabled == True
  assert string.contains(
    effective.pi.session_persistence.recovery_prompt,
    "being resumed by Scherzo",
  )

  assert effective.handoff.enabled == True
  assert effective.handoff.comment_on_claim == True
  assert effective.handoff.comment_on_success == True
  assert effective.handoff.comment_on_failure == False
  assert effective.handoff.comment_on_park == True
  assert effective.handoff.claim_state_id
    == Some(workflow_completion_policy.StateByName("In Progress"))
  assert effective.handoff.success_state_id
    == Some(workflow_completion_policy.StateByName("In Review"))
  assert effective.handoff.failure_state_id
    == Some(workflow_completion_policy.StateByName("Needs Attention"))
  assert effective.handoff.include_result_on_success == False
  assert effective.handoff.attach_result_on_success == True
  assert effective.handoff.attachment_fallback_to_markdown_link == True
  assert effective.handoff.result_max_chars == 4000
  let assert Some(completion_policy) = effective.handoff.completion_states
  assert completion_policy.default_completion_state
    == Some(workflow_completion_policy.StateByName("In Review"))
  assert completion_policy.no_review_completion_state
    == Some(workflow_completion_policy.StateByName("Done"))
  assert completion_policy.failure_state
    == Some(workflow_completion_policy.StateByName("Needs Attention"))
  assert completion_policy.partial_success_state
    == Some(workflow_completion_policy.StateByName("Triage"))
  assert dict.to_list(completion_policy.workflows) == []

  assert effective.linear_contract.enabled == True
  assert effective.linear_contract.workflow_label_prefix == "workflow:"
  assert effective.linear_contract.workflow_labels == ["implementation"]
  assert effective.linear_contract.support_labels == ["needs-workflow"]
  assert dict.to_list(effective.linear_contract.required_states) == []
  assert dict.to_list(effective.linear_contract.handoff_state_bindings) == []
  assert effective.linear_contract.enforce_issue_workflow_labels == True
  assert effective.linear_contract.invalid_workflow_state_id == Some("Triage")
  assert effective.linear_contract.invalid_workflow_state_target
    == Some(config_types.InvalidWorkflowStateName("Triage"))
  assert effective.linear_contract.comment_on_invalid_workflow == True

  assert effective.linear_commands == config.default_linear_command_config()

  assert orchestrator.routing.workflow_label_prefix == "workflow:"
  assert orchestrator.routing.require_exactly_one_workflow_label == True
  assert orchestrator.routing.default_workflow == Some("implementation")
  let assert Ok(workflow_path) =
    dict.get(orchestrator.routing.workflows, "implementation")
  assert string.ends_with(
    workflow_path,
    "/test/tmp/schema_guardrail/workflows/implementation.yaml",
  )

  assert orchestrator.dag_hooks == config_types.empty_dag_hooks()
  assert orchestrator.workspace_profiles.default_profile == "fixture"
  let assert Ok(fixture_profile) =
    dict.get(orchestrator.workspace_profiles.profiles, "fixture")
  assert fixture_profile.source == config_types.ConfiguredWorkspaceDriver
  let assert Some(fixture_driver) = fixture_profile.driver
  assert fixture_driver.command == "scripts/scherzo-workspace-noop"
  assert fixture_driver.lifecycle
    == [
      config_types.LifecycleCreate,
      config_types.LifecycleBeforeStep,
      config_types.LifecycleAfterStep,
      config_types.LifecycleRemove,
    ]
  assert fixture_driver.timeout_ms == 1234

  assert orchestrator.artifact_limits.command_stream_max_chars == 111
  assert orchestrator.artifact_limits.template_field_max_chars == 222
  assert orchestrator.artifact_limits.workflow_summary_max_chars == 333
  assert orchestrator.model_settings.model
    == Some("github-copilot/gpt-5.1-codex")
  assert orchestrator.model_settings.thinking == Some(model_config.ThinkingHigh)

  let assert [job] = orchestrator.scheduled_jobs
  assert job.id == "nightly-repair"
  assert job.workflow == "nightly-repair"
  assert job.enabled == True
  assert job.every_ms == 900_000
  assert job.overlap == config_types.SkipOverlap
  assert job.catch_up == False
  let config_types.ScheduledFailureConfig(task: task_failure) = job.on_failure
  assert task_failure.enabled == True
  assert task_failure.state == Some("Triage")
  assert task_failure.labels == ["job:nightly-repair"]
  assert task_failure.dedupe == config_types.OpenTaskPerSchedule
}

pub fn legacy_top_level_hooks_parse_guardrail_test() {
  let source =
    "version: 1\ntracker:\n  linear:\n    api_key_env: LINEAR_API_KEY\n    project: \"$LINEAR_PROJECT_SLUG\"\n  states:\n    ready: [Todo]\nworkflows:\n  implementation: workflows/implementation.yaml\nhooks:\n  before_run: test -d .git\n  after_run: echo done\n  timeout: 90s\n"
  let assert Ok([document]) = yay.parse_string(source)
  let assert Ok(orchestrator) =
    config.resolve_orchestrator_root(
      yay.document_root(document),
      "test/tmp/schema_guardrail/scherzo.yaml",
      schema_env,
    )

  let hooks = orchestrator.effective.hooks
  assert hooks.before_run == Some("test -d .git")
  assert hooks.after_run == Some("echo done")
  assert hooks.timeout_ms == 90_000
}

fn ledger_examples() -> List(LedgerExample) {
  [
    LedgerExample(
      "RunStarted",
      "run_started",
      record.with_id(
        "record-run-started",
        1000,
        record.RunStarted(
          run_id: "run-1",
          issue_id: "issue-1",
          issue_identifier: "LIV-1",
          workspace_path: ".scherzo/workspaces/LIV-1",
        ),
      ),
    ),
    LedgerExample(
      "RunFinished",
      "run_finished",
      record.with_id(
        "record-run-finished",
        1001,
        record.RunFinished(
          run_id: "run-1",
          issue_id: "issue-1",
          classification: "success",
          token_total: 42,
          turns: 3,
        ),
      ),
    ),
    LedgerExample(
      "RunInterrupted",
      "run_interrupted",
      record.with_id(
        "record-run-interrupted",
        1002,
        record.RunInterrupted(
          run_id: "run-2",
          issue_id: "issue-2",
          reason: "daemon_exit",
        ),
      ),
    ),
    LedgerExample(
      "WorkflowRunStarted",
      "workflow_run_started",
      record.with_id(
        "record-workflow-run-started",
        1003,
        record.WorkflowRunStarted(
          run_id: "workflow-run-1",
          workflow_id: "implementation",
          workflow_fingerprint: "wf-fingerprint",
          issue_id: "issue-1",
          issue_identifier: "LIV-1",
          issue_fingerprint: "issue-fingerprint",
          observed_updated_at_ms: 999,
          run_root: ".scherzo/workflows/implementation/run-1",
        ),
      ),
    ),
    LedgerExample(
      "WorkflowRunStartedWithTask",
      "workflow_run_started",
      record.with_id(
        "record-workflow-run-started-with-task",
        1003,
        record.WorkflowRunStartedWithTask(
          run_id: "workflow-run-1-task",
          workflow_id: "implementation",
          workflow_fingerprint: "wf-fingerprint",
          issue_id: "issue-1",
          issue_identifier: "LIV-1",
          task_ref: record.TaskRefFields(
            task_backend_kind: "linear",
            task_remote_id: "issue-1",
            task_key: Some("LIV-1"),
            task_url: Some("https://linear.app/living-systems/issue/LIV-1"),
          ),
          issue_fingerprint: "issue-fingerprint",
          observed_updated_at_ms: 999,
          run_root: ".scherzo/workflows/implementation/run-1",
        ),
      ),
    ),
    LedgerExample(
      "WorkflowRunProvenanceRepaired",
      "workflow_run_provenance_repaired",
      record.with_id(
        "record-workflow-run-provenance-repaired",
        1003,
        record.WorkflowRunProvenanceRepaired(
          run_id: "workflow-run-1-task",
          workflow_id: "implementation",
          workflow_fingerprint: "wf-fingerprint",
          issue_id: "issue-1",
          issue_identifier: "LIV-1",
          task_ref: record.TaskRefFields(
            task_backend_kind: "linear",
            task_remote_id: "issue-1",
            task_key: Some("LIV-1"),
            task_url: Some("https://linear.app/living-systems/issue/LIV-1"),
          ),
          issue_fingerprint: "issue-fingerprint",
          observed_updated_at_ms: 999,
          run_root: ".scherzo/workflows/implementation/run-1",
          repair_mode: "state_repair_explicit",
          source_evidence: [
            "workflow_run_interrupted:workflow-run-1-task",
            "workflow_run_inputs_recorded:workflow-run-1-task",
          ],
        ),
      ),
    ),
    LedgerExample(
      "WorkflowRunFinished",
      "workflow_run_finished",
      record.with_id(
        "record-workflow-run-finished",
        1004,
        record.WorkflowRunFinished(
          run_id: "workflow-run-1",
          workflow_id: "implementation",
          issue_id: "issue-1",
          outcome: "success",
          token_total: 100,
          turns: 5,
        ),
      ),
    ),
    LedgerExample(
      "WorkflowRunFinishedWithTask",
      "workflow_run_finished",
      record.with_id(
        "record-workflow-run-finished-with-task",
        1004,
        record.WorkflowRunFinishedWithTask(
          run_id: "workflow-run-1-task",
          workflow_id: "implementation",
          issue_id: "issue-1",
          task_ref: record.TaskRefFields(
            task_backend_kind: "linear",
            task_remote_id: "issue-1",
            task_key: Some("LIV-1"),
            task_url: Some("https://linear.app/living-systems/issue/LIV-1"),
          ),
          outcome: "success",
          token_total: 100,
          turns: 5,
        ),
      ),
    ),
    LedgerExample(
      "WorkflowRunInputsRecorded",
      "workflow_run_inputs_recorded",
      record.with_id(
        "record-workflow-run-inputs-recorded",
        1004,
        record.WorkflowRunInputsRecorded(
          run_id: "workflow-run-1",
          workflow_id: "research",
          workflow_fingerprint: "fp",
          artifact_ref: "runs/workflow-run-1/inputs.v1.json",
          artifact_sha256: "abc",
          artifact_bytes: 123,
        ),
      ),
    ),
    LedgerExample(
      "WorkflowInterfaceSnapshotRecorded",
      "workflow_interface_snapshot_recorded",
      record.with_id(
        "record-workflow-interface-snapshot-recorded",
        1004,
        record.WorkflowInterfaceSnapshotRecorded(
          run_id: "workflow-run-1",
          workflow_id: "research",
          workflow_fingerprint: "fp",
          artifact_ref: "runs/workflow-run-1/workflow-interface.v1.json",
          artifact_sha256: "bcd",
          artifact_bytes: 234,
        ),
      ),
    ),
    LedgerExample(
      "WorkflowRunOutputsRecorded",
      "workflow_run_outputs_recorded",
      record.with_id(
        "record-workflow-run-outputs-recorded",
        1004,
        record.WorkflowRunOutputsRecorded(
          run_id: "workflow-run-1",
          workflow_id: "research",
          workflow_fingerprint: "fp",
          artifact_ref: "runs/workflow-run-1/outputs.v1.json",
          artifact_sha256: "def",
          artifact_bytes: 456,
        ),
      ),
    ),
    LedgerExample(
      "PublicationAttemptRecorded",
      "publication_attempt_recorded",
      record.with_id(
        "record-publication-attempt-recorded",
        1004,
        record.PublicationAttemptRecorded(
          run_id: "workflow-run-1",
          workflow_id: "research",
          publication_id: "review_doc",
          series_id: "issue-1:research:review_doc",
          attempt_id: "version-1",
          status: "planned",
          required: False,
          retryable: False,
          retry_execution_available: False,
          version_id: Some("version-1"),
          manifest_ref: Some(
            "runs/workflow-run-1/publications/review_doc/version-1.json",
          ),
          manifest_sha256: Some("pub-sha"),
          manifest_bytes: Some(789),
          error_code: None,
          error_message: None,
        ),
      ),
    ),
    LedgerExample(
      "WorkflowRunDiagnostic",
      "workflow_run_diagnostic",
      record.with_id(
        "record-workflow-run-diagnostic",
        1005,
        record.WorkflowRunDiagnostic(
          run_id: "workflow-run-1",
          workflow_id: "implementation",
          issue_id: "issue-1",
          reason: "artifact_recovery_failed: step_id=seed artifact_ref=runs/run-1/seed/attempt-1.json reason=missing",
        ),
      ),
    ),
    LedgerExample(
      "WorkflowRunInterrupted",
      "workflow_run_interrupted",
      record.with_id(
        "record-workflow-run-interrupted",
        1005,
        record.WorkflowRunInterrupted(
          run_id: "workflow-run-2",
          workflow_id: "implementation",
          issue_id: "issue-2",
          reason: "operator_pause",
        ),
      ),
    ),
    LedgerExample(
      "WorkflowRunSuperseded",
      "workflow_run_superseded",
      record.with_id(
        "record-workflow-run-superseded",
        1006,
        record.WorkflowRunSuperseded(
          run_id: "workflow-run-3",
          workflow_id: "implementation",
          issue_id: "issue-3",
          superseded_by_run_id: "workflow-run-4",
          reason: "issue_updated",
        ),
      ),
    ),
    LedgerExample(
      "WorkflowRepairRequested",
      "workflow_repair_requested",
      record.with_id(
        "record-workflow-repair-requested",
        1006,
        record.WorkflowRepairRequested(
          run_id: "workflow-run-3",
          workflow_id: "implementation",
          issue_id: "issue-3",
          issue_identifier: "LIV-3",
          requested_target: "workflow-run-3",
          requested_step_id: Some("implement"),
          selected_step_id: "implement",
          failed_attempt_index: 1,
          next_attempt_index: 2,
          reason: "retry-step",
        ),
      ),
    ),
    LedgerExample(
      "StepAttemptPrepared",
      "step_attempt_prepared",
      record.with_id(
        "record-step-attempt-prepared",
        1007,
        record.StepAttemptPrepared(
          run_id: "workflow-run-1",
          workflow_id: "implementation",
          step_id: "implement",
          attempt_index: 1,
          workspace_name: "implementation",
          workspace_path: ".scherzo/workspaces/implementation",
          run_root: ".scherzo/workflows/implementation/run-1",
          source_workspace_name: Some("main"),
          source_workspace_path: Some("/repo"),
        ),
      ),
    ),
    LedgerExample(
      "StepAttemptStarted",
      "step_attempt_started",
      record.with_id(
        "record-step-attempt-started",
        1008,
        record.StepAttemptStarted(
          run_id: "workflow-run-1",
          workflow_id: "implementation",
          step_id: "implement",
          attempt_index: 1,
          operator_session_id: "operator-session-1",
          external_session_ref: Some("pi-session-1"),
          continuation_capable: True,
        ),
      ),
    ),
    LedgerExample(
      "StepAttemptContinuationStarted",
      "step_attempt_continuation_started",
      record.with_id(
        "record-step-attempt-continuation-started",
        1009,
        record.StepAttemptContinuationStarted(
          run_id: "workflow-run-1",
          workflow_id: "implementation",
          step_id: "implement",
          attempt_index: 1,
          session_id: "pi-session-1",
        ),
      ),
    ),
    LedgerExample(
      "StepAttemptPiSessionRecorded",
      "step_attempt_pi_session_recorded",
      record.with_id(
        "record-step-attempt-pi-session-recorded",
        1010,
        record.StepAttemptPiSessionRecorded(
          run_id: "workflow-run-1",
          issue_id: "issue-1",
          issue_identifier: "LIV-1",
          workflow_id: "implementation",
          workflow_fingerprint: "wf-fingerprint",
          step_id: "implement",
          workspace_name: "implementation",
          attempt_index: 1,
          workspace_path: ".scherzo/workspaces/implementation",
          session_id: "pi-session-1",
          session_file: ".scherzo/sessions/pi-session-1.json",
        ),
      ),
    ),
    LedgerExample(
      "StepAttemptPiSessionRecordedWithTask",
      "step_attempt_pi_session_recorded",
      record.with_id(
        "record-step-attempt-pi-session-recorded-with-task",
        1010,
        record.StepAttemptPiSessionRecordedWithTask(
          run_id: "workflow-run-1-task",
          issue_id: "issue-1",
          issue_identifier: "LIV-1",
          task_ref: record.TaskRefFields(
            task_backend_kind: "linear",
            task_remote_id: "issue-1",
            task_key: Some("LIV-1"),
            task_url: Some("https://linear.app/living-systems/issue/LIV-1"),
          ),
          workflow_id: "implementation",
          workflow_fingerprint: "wf-fingerprint",
          step_id: "implement",
          workspace_name: "implementation",
          attempt_index: 1,
          workspace_path: ".scherzo/workspaces/implementation",
          session_id: "pi-session-1",
          session_file: ".scherzo/sessions/pi-session-1.json",
        ),
      ),
    ),
    LedgerExample(
      "StepAttemptFinished",
      "step_attempt_finished",
      record.with_id(
        "record-step-attempt-finished",
        1011,
        record.StepAttemptFinished(
          run_id: "workflow-run-1",
          workflow_id: "implementation",
          step_id: "implement",
          attempt_index: 1,
          outcome: "success",
          artifact_ref: "artifact-1",
          artifact_sha256: "sha256",
          workspace_name: "implementation",
          workspace_path: ".scherzo/workspaces/implementation",
          token_total: 100,
          turns: 5,
        ),
      ),
    ),
    LedgerExample(
      "WorkflowStepRecoveryStarted",
      "workflow_step_recovery_started",
      record.with_id(
        "record-workflow-step-recovery-started",
        1012,
        record.WorkflowStepRecoveryStarted(
          run_id: "workflow-run-1",
          workflow_id: "implementation",
          step_id: "implement",
          failed_attempt_index: 1,
          recovery_attempt_number: 1,
          recovery_session_id: "workflow-run-1-implement-recovery-1",
          model: Some("gpt-5"),
          prompt_ref: ".scherzo/workflows/prompts/recover_failed_step.md",
        ),
      ),
    ),
    LedgerExample(
      "WorkflowStepRecoveryFinished",
      "workflow_step_recovery_finished",
      record.with_id(
        "record-workflow-step-recovery-finished",
        1013,
        record.WorkflowStepRecoveryFinished(
          run_id: "workflow-run-1",
          workflow_id: "implementation",
          step_id: "implement",
          failed_attempt_index: 1,
          recovery_attempt_number: 1,
          recovery_session_id: "workflow-run-1-implement-recovery-1",
          result: "recheck",
          summary: "Fixed tests",
          reason: "The workspace is ready for a recheck.",
          retry_attempt_index: Some(2),
        ),
      ),
    ),
    LedgerExample(
      "StepAttemptInterrupted",
      "step_attempt_interrupted",
      record.with_id(
        "record-step-attempt-interrupted",
        1012,
        record.StepAttemptInterrupted(
          run_id: "workflow-run-1",
          workflow_id: "implementation",
          step_id: "review",
          attempt_index: 1,
          reason: "operator_abort",
        ),
      ),
    ),
    LedgerExample(
      "StepAttemptSuperseded",
      "step_attempt_superseded",
      record.with_id(
        "record-step-attempt-superseded",
        1013,
        record.StepAttemptSuperseded(
          run_id: "workflow-run-1",
          workflow_id: "implementation",
          step_id: "review",
          attempt_index: 1,
          superseded_by_attempt_index: 2,
          reason: "retry",
        ),
      ),
    ),
    LedgerExample(
      "RetryScheduled",
      "retry_scheduled",
      record.with_id(
        "record-retry-scheduled",
        1014,
        record.RetryScheduled(
          issue_id: "issue-1",
          issue_identifier: "LIV-1",
          delay_ms: 10_000,
          generation: 2,
          reason: "backoff",
        ),
      ),
    ),
    LedgerExample(
      "RetryCancelled",
      "retry_cancelled",
      record.with_id(
        "record-retry-cancelled",
        1015,
        record.RetryCancelled(
          issue_id: "issue-1",
          generation: 2,
          reason: "manual_retry",
        ),
      ),
    ),
    LedgerExample(
      "IssueCounterUpdated",
      "issue_counter_updated",
      record.with_id(
        "record-issue-counter-updated",
        1016,
        record.IssueCounterUpdated(
          issue_id: "issue-1",
          issue_identifier: "LIV-1",
          failure_attempts: 1,
          worker_sessions: 2,
          observed_updated_at_ms: 1000,
          source_run_id: Some("run-1"),
        ),
      ),
    ),
    LedgerExample(
      "KnownWorkspace",
      "known_workspace",
      record.with_id(
        "record-known-workspace",
        1017,
        record.KnownWorkspace(
          issue_id: "issue-1",
          issue_identifier: "LIV-1",
          workspace_path: ".scherzo/workspaces/LIV-1",
        ),
      ),
    ),
    LedgerExample(
      "IssueParked",
      "issue_parked",
      record.with_id(
        "record-issue-parked",
        1018,
        record.IssueParked(
          issue_id: "issue-2",
          issue_identifier: "LIV-2",
          reason: "blocked",
          observed_updated_at_ms: 1017,
        ),
      ),
    ),
    LedgerExample(
      "IssueParkedV2",
      "issue_parked_v2",
      record.with_id(
        "record-issue-parked-v2",
        1019,
        record.IssueParkedV2(
          issue_id: "issue-3",
          issue_identifier: "LIV-3",
          reason: "max_retry_attempts",
          release_policy: "auto_unpark_on_issue_change",
          issue_fingerprint: "fingerprint",
          observed_updated_at_ms: 1018,
        ),
      ),
    ),
    LedgerExample(
      "IssueUnparked",
      "issue_unparked",
      record.with_id(
        "record-issue-unparked",
        1020,
        record.IssueUnparked(
          issue_id: "issue-3",
          issue_identifier: "LIV-3",
          reason: "operator",
        ),
      ),
    ),
    LedgerExample(
      "DispatchPauseChanged",
      "dispatch_pause_changed",
      record.with_id(
        "record-dispatch-pause-changed",
        1021,
        record.DispatchPauseChanged(paused: True),
      ),
    ),
    LedgerExample(
      "LinearCommandSeen",
      "linear_command_seen",
      record.with_id(
        "record-linear-command-seen",
        1021,
        record.LinearCommandSeen(
          comment_id: "comment-1",
          issue_id: "issue-1",
          author_id: "user-1",
          command_name: "retry",
          excerpt: "/scherzo retry",
        ),
      ),
    ),
    LedgerExample(
      "LinearCommandStarted",
      "linear_command_started",
      record.with_id(
        "record-linear-command-started",
        1022,
        record.LinearCommandStarted(
          comment_id: "comment-1",
          issue_id: "issue-1",
          command_name: "retry",
        ),
      ),
    ),
    LedgerExample(
      "LinearCommandCompleted",
      "linear_command_completed",
      record.with_id(
        "record-linear-command-completed",
        1023,
        record.LinearCommandCompleted(
          comment_id: "comment-1",
          issue_id: "issue-1",
          status: "accepted",
          message_excerpt: "queued retry",
        ),
      ),
    ),
    LedgerExample(
      "LinearCommandAcked",
      "linear_command_acked",
      record.with_id(
        "record-linear-command-acked",
        1024,
        record.LinearCommandAcked(comment_id: "comment-1", issue_id: "issue-1"),
      ),
    ),
    LedgerExample(
      "RemoteCommandSeen",
      "remote_command_seen",
      record.with_id(
        "record-remote-command-seen",
        1021,
        record.RemoteCommandSeen(
          backend_kind: "linear",
          event_id: "comment-1",
          task_remote_id: "issue-1",
          task_key: Some("LIV-1"),
          author_id: "user-1",
          command_name: "retry",
          excerpt: "/scherzo retry",
        ),
      ),
    ),
    LedgerExample(
      "RemoteCommandStarted",
      "remote_command_started",
      record.with_id(
        "record-remote-command-started",
        1022,
        record.RemoteCommandStarted(
          backend_kind: "linear",
          event_id: "comment-1",
          task_remote_id: "issue-1",
          command_name: "retry",
        ),
      ),
    ),
    LedgerExample(
      "RemoteCommandCompleted",
      "remote_command_completed",
      record.with_id(
        "record-remote-command-completed",
        1023,
        record.RemoteCommandCompleted(
          backend_kind: "linear",
          event_id: "comment-1",
          task_remote_id: "issue-1",
          status: "accepted",
          message_excerpt: "queued retry",
        ),
      ),
    ),
    LedgerExample(
      "RemoteCommandAcked",
      "remote_command_acked",
      record.with_id(
        "record-remote-command-acked",
        1024,
        record.RemoteCommandAcked(
          backend_kind: "linear",
          event_id: "comment-1",
          task_remote_id: "issue-1",
        ),
      ),
    ),
    LedgerExample(
      "ControlOperationQueued",
      "control_operation_queued",
      record.with_id(
        "record-control-operation-queued",
        1024,
        record.ControlOperationQueued(
          operation_id: "op-1",
          operation_kind: "retry_step",
          command_name: "retry_step",
          target: "run:run-1",
          run_id: Some("run-1"),
          issue_id: Some("issue-1"),
          issue_identifier: Some("LIV-1"),
          requested_step_id: Some("apply_feedback"),
          publication_id: None,
        ),
      ),
    ),
    LedgerExample(
      "ControlOperationStarted",
      "control_operation_started",
      record.with_id(
        "record-control-operation-started",
        1025,
        record.ControlOperationStarted(operation_id: "op-1"),
      ),
    ),
    LedgerExample(
      "ControlOperationCompleted",
      "control_operation_completed",
      record.with_id(
        "record-control-operation-completed",
        1026,
        record.ControlOperationCompleted(
          operation_id: "op-1",
          message: Some("retry-step completed"),
        ),
      ),
    ),
    LedgerExample(
      "ControlOperationFailed",
      "control_operation_failed",
      record.with_id(
        "record-control-operation-failed",
        1027,
        record.ControlOperationFailed(
          operation_id: "op-2",
          reason: "artifact_recovery_failed",
          message: Some("retry-step failed"),
        ),
      ),
    ),
    LedgerExample(
      "ScheduledJobDue",
      "scheduled_job_due",
      record.with_id(
        "record-scheduled-job-due",
        1025,
        record.ScheduledJobDue(
          job_id: "nightly-repair",
          workflow_id: "implementation",
          due_at_ms: 900_000,
          run_id: "scheduled-run-1",
          trigger: "automatic",
        ),
      ),
    ),
    LedgerExample(
      "ScheduledJobSkipped",
      "scheduled_job_skipped",
      record.with_id(
        "record-scheduled-job-skipped",
        1026,
        record.ScheduledJobSkipped(
          job_id: "nightly-repair",
          workflow_id: "implementation",
          due_at_ms: 1_800_000,
          run_id: "scheduled-run-2",
          reason: "overlap_running",
          skipped_count: 1,
        ),
      ),
    ),
    LedgerExample(
      "ScheduledRunPending",
      "scheduled_run_pending",
      record.with_id(
        "record-scheduled-run-pending",
        1027,
        record.ScheduledRunPending(
          job_id: "nightly-repair",
          workflow_id: "implementation",
          due_at_ms: 900_000,
          run_id: "scheduled-run-1",
          trigger: "manual",
          requested_at_ms: 1027,
        ),
      ),
    ),
    LedgerExample(
      "ScheduledRunPendingBlocked",
      "scheduled_run_pending_blocked",
      record.with_id(
        "record-scheduled-run-pending-blocked",
        1028,
        record.ScheduledRunPendingBlocked(
          job_id: "nightly-repair",
          workflow_id: "implementation",
          due_at_ms: 900_000,
          run_id: "scheduled-run-1",
          reason: "waiting_for_global_slot",
          observed_at_ms: 1028,
        ),
      ),
    ),
    LedgerExample(
      "ScheduledRunPendingCancelled",
      "scheduled_run_pending_cancelled",
      record.with_id(
        "record-scheduled-run-pending-cancelled",
        1029,
        record.ScheduledRunPendingCancelled(
          job_id: "nightly-repair",
          workflow_id: "implementation",
          due_at_ms: 900_000,
          run_id: "scheduled-run-1",
          reason: "job_disabled",
          cancelled_at_ms: 1029,
        ),
      ),
    ),
    LedgerExample(
      "ScheduledRunStarted",
      "scheduled_run_started",
      record.with_id(
        "record-scheduled-run-started",
        1030,
        record.ScheduledRunStarted(
          job_id: "nightly-repair",
          workflow_id: "implementation",
          due_at_ms: 900_000,
          started_at_ms: 1030,
          run_id: "scheduled-run-1",
          attempt: 1,
          session_id: "session-1",
          run_root: ".scherzo/scheduled/nightly-repair/run-1",
        ),
      ),
    ),
    LedgerExample(
      "ScheduledRunSucceeded",
      "scheduled_run_succeeded",
      record.with_id(
        "record-scheduled-run-succeeded",
        1031,
        record.ScheduledRunSucceeded(
          job_id: "nightly-repair",
          workflow_id: "implementation",
          due_at_ms: 900_000,
          run_id: "scheduled-run-1",
          attempt: 1,
          finished_at_ms: 1031,
          token_total: 20,
          turns: 2,
        ),
      ),
    ),
    LedgerExample(
      "ScheduledRunFailed",
      "scheduled_run_failed",
      record.with_id(
        "record-scheduled-run-failed",
        1032,
        record.ScheduledRunFailed(
          job_id: "nightly-repair",
          workflow_id: "implementation",
          due_at_ms: 900_000,
          run_id: "scheduled-run-1",
          attempt: 1,
          finished_at_ms: 1032,
          reason: "workflow_step_failed",
          retry_exhausted: False,
          run_root: Some(".scherzo/scheduled/nightly-repair/run-1"),
        ),
      ),
    ),
    LedgerExample(
      "ScheduledRunRetryScheduled",
      "scheduled_run_retry_scheduled",
      record.with_id(
        "record-scheduled-run-retry-scheduled",
        1033,
        record.ScheduledRunRetryScheduled(
          job_id: "nightly-repair",
          workflow_id: "implementation",
          due_at_ms: 900_000,
          run_id: "scheduled-run-1",
          next_attempt: 2,
          delay_ms: 10_000,
          generation: 1,
          reason: "workflow_step_failed",
        ),
      ),
    ),
    LedgerExample(
      "ScheduledRunRetryCancelled",
      "scheduled_run_retry_cancelled",
      record.with_id(
        "record-scheduled-run-retry-cancelled",
        1034,
        record.ScheduledRunRetryCancelled(
          job_id: "nightly-repair",
          run_id: "scheduled-run-1",
          generation: 1,
          reason: "superseded",
        ),
      ),
    ),
    LedgerExample(
      "ScheduledFailureReported",
      "scheduled_failure_reported",
      record.with_id(
        "record-scheduled-failure-reported",
        1035,
        record.ScheduledFailureReported(
          job_id: "nightly-repair",
          workflow_id: "implementation",
          due_at_ms: 900_000,
          run_id: "scheduled-run-1",
          attempt: 2,
          dedupe_key: "scheduled-job:nightly-repair",
          linear_issue_id: "issue-linear",
          action: "created",
        ),
      ),
    ),
    LedgerExample(
      "ScheduledFailureReportFailed",
      "scheduled_failure_report_failed",
      record.with_id(
        "record-scheduled-failure-report-failed",
        1036,
        record.ScheduledFailureReportFailed(
          job_id: "nightly-repair",
          workflow_id: "implementation",
          due_at_ms: 900_000,
          run_id: "scheduled-run-1",
          attempt: 2,
          dedupe_key: "scheduled-job:nightly-repair",
          error_code: "linear_api_request",
          error_message: "network",
          next_retry_at_ms: 20_000,
          generation: 1,
        ),
      ),
    ),
    LedgerExample(
      "ScheduledJobQuarantineReleased",
      "scheduled_job_quarantine_released",
      record.with_id(
        "record-scheduled-job-quarantine-released",
        1036,
        record.ScheduledJobQuarantineReleased(
          job_id: "nightly-repair",
          reason: "operator",
          released_at_ms: 20_100,
        ),
      ),
    ),
    LedgerExample(
      "OutboxPending",
      "outbox_pending",
      record.with_id(
        "record-outbox-pending",
        1037,
        record.OutboxPending(
          outbox_id: "outbox-1",
          issue_id: "issue-1",
          outbox_kind: "linear_comment",
          dedupe_key: "comment-1:ack",
        ),
      ),
    ),
    LedgerExample(
      "OutboxPendingV2",
      "outbox_pending_v2",
      record.with_id(
        "record-outbox-pending-v2",
        1038,
        record.OutboxPendingV2(
          outbox_id: "outbox-2",
          issue_id: "issue-1",
          outbox_kind: "linear_comment",
          dedupe_key: "run-1:success",
          payload_json: "{\"body\":\"ok\"}",
        ),
      ),
    ),
    LedgerExample(
      "OutboxPendingV2WithTask",
      "outbox_pending_v2",
      record.with_id(
        "record-outbox-pending-v2-with-task",
        1038,
        record.OutboxPendingV2WithTask(
          outbox_id: "outbox-2-task",
          task_ref: record.linear_task_ref_fields(
            "issue-2",
            Some("LIV-2"),
            None,
          ),
          outbox_kind: "linear_comment",
          dedupe_key: "run-2:success",
          payload_json: "{\"body\":\"ok\"}",
        ),
      ),
    ),
    LedgerExample(
      "OutboxAttempted",
      "outbox_attempted",
      record.with_id(
        "record-outbox-attempted",
        1039,
        record.OutboxAttempted(
          outbox_id: "outbox-3",
          issue_id: "issue-3",
          outbox_kind: "linear_comment",
          dedupe_key: "run-3:success",
          payload_json: "{\"body\":\"attempted\"}",
          attempt_count: 1,
        ),
      ),
    ),
    LedgerExample(
      "OutboxAttemptedWithTask",
      "outbox_attempted",
      record.with_id(
        "record-outbox-attempted-with-task",
        1039,
        record.OutboxAttemptedWithTask(
          outbox_id: "outbox-3-task",
          task_ref: record.linear_task_ref_fields(
            "issue-3-task",
            Some("LIV-3"),
            None,
          ),
          outbox_kind: "linear_comment",
          dedupe_key: "run-3-task:success",
          payload_json: "{\"body\":\"attempted\"}",
          attempt_count: 2,
        ),
      ),
    ),
    LedgerExample(
      "OutboxRetryScheduled",
      "outbox_retry_scheduled",
      record.with_id(
        "record-outbox-retry-scheduled",
        1040,
        record.OutboxRetryScheduled(
          outbox_id: "outbox-4",
          issue_id: "issue-4",
          outbox_kind: "linear_comment",
          dedupe_key: "run-4:success",
          payload_json: "{\"body\":\"retry\"}",
          error_code: "http_429",
          attempt_count: 2,
          next_attempt_at_ms: 2040,
        ),
      ),
    ),
    LedgerExample(
      "OutboxRetryScheduledWithTask",
      "outbox_retry_scheduled",
      record.with_id(
        "record-outbox-retry-scheduled-with-task",
        1040,
        record.OutboxRetryScheduledWithTask(
          outbox_id: "outbox-4-task",
          task_ref: record.linear_task_ref_fields(
            "issue-4-task",
            Some("LIV-4"),
            None,
          ),
          outbox_kind: "linear_comment",
          dedupe_key: "run-4-task:success",
          payload_json: "{\"body\":\"retry\"}",
          error_code: "http_429",
          attempt_count: 3,
          next_attempt_at_ms: 2041,
        ),
      ),
    ),
    LedgerExample(
      "OutboxCompleted",
      "outbox_completed",
      record.with_id(
        "record-outbox-completed",
        1041,
        record.OutboxCompleted(
          outbox_id: "outbox-1",
          issue_id: "issue-1",
          outbox_kind: "linear_comment",
        ),
      ),
    ),
    LedgerExample(
      "OutboxCompletedWithTask",
      "outbox_completed",
      record.with_id(
        "record-outbox-completed-with-task",
        1041,
        record.OutboxCompletedWithTask(
          outbox_id: "outbox-2-task",
          task_ref: record.linear_task_ref_fields(
            "issue-2",
            Some("LIV-2"),
            None,
          ),
          outbox_kind: "linear_comment",
        ),
      ),
    ),
    LedgerExample(
      "OutboxFailed",
      "outbox_failed",
      record.with_id(
        "record-outbox-failed",
        1042,
        record.OutboxFailed(
          outbox_id: "outbox-5",
          issue_id: "issue-2",
          outbox_kind: "linear_comment",
          error_code: "http_500",
        ),
      ),
    ),
    LedgerExample(
      "OutboxFailedWithTask",
      "outbox_failed",
      record.with_id(
        "record-outbox-failed-with-task",
        1042,
        record.OutboxFailedWithTask(
          outbox_id: "outbox-5-task",
          task_ref: record.linear_task_ref_fields(
            "issue-5",
            Some("LIV-5"),
            None,
          ),
          outbox_kind: "linear_comment",
          error_code: "http_500",
        ),
      ),
    ),
    LedgerExample(
      "OutboxPermanentlyFailed",
      "outbox_permanently_failed",
      record.with_id(
        "record-outbox-permanently-failed",
        1043,
        record.OutboxPermanentlyFailed(
          outbox_id: "outbox-6",
          issue_id: "issue-6",
          outbox_kind: "linear_comment",
          error_code: "unauthorized",
          attempt_count: 4,
        ),
      ),
    ),
    LedgerExample(
      "OutboxPermanentlyFailedWithTask",
      "outbox_permanently_failed",
      record.with_id(
        "record-outbox-permanently-failed-with-task",
        1043,
        record.OutboxPermanentlyFailedWithTask(
          outbox_id: "outbox-6-task",
          task_ref: record.linear_task_ref_fields(
            "issue-6-task",
            Some("LIV-6"),
            None,
          ),
          outbox_kind: "linear_comment",
          error_code: "unauthorized",
          attempt_count: 5,
        ),
      ),
    ),
    LedgerExample(
      "WorkstreamCreated",
      "workstream_created",
      record.with_id(
        "record-workstream-created",
        1041,
        record.WorkstreamCreated(
          workstream_id: "linear:LIV-393",
          task_ref: record.linear_task_ref_fields(
            "issue-393",
            Some("LIV-393"),
            Some("https://linear.app/living-systems/issue/LIV-393"),
          ),
          idempotency_key: "ws-create-1",
        ),
      ),
    ),
    LedgerExample(
      "WorkstreamAssigned",
      "workstream_assigned",
      record.with_id(
        "record-workstream-assigned",
        1042,
        record.WorkstreamAssigned(
          workstream_id: "linear:LIV-393",
          assignment_id: "assignment-1",
          workflow_id: "execplan-implementation",
          playbook_id: Some("playbook-1"),
          reason: "manual_claim",
          idempotency_key: "ws-assign-1",
        ),
      ),
    ),
    LedgerExample(
      "WorkstreamArtifactRecorded",
      "workstream_artifact_recorded",
      record.with_id(
        "record-workstream-artifact-recorded",
        1043,
        record.WorkstreamArtifactRecorded(
          workstream_id: "linear:LIV-393",
          artifact_id: "artifact-1",
          artifact_type: "scherzo.workstream.v1",
          snapshot_ref: "workstream-artifacts/sha256/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa.json",
          snapshot_sha256: string.repeat("a", times: 64),
          snapshot_bytes: 123,
          original_path: "docs/plan.md",
          contract_type: "handoff",
          media_type: "application/json",
          producer_workflow_id: "execplan",
          producer_run_id: "run-1",
          producer_step_id: "step-1",
          idempotency_key: "ws-artifact-1",
        ),
      ),
    ),
    LedgerExample(
      "WorkstreamHandoffRecorded",
      "workstream_handoff_recorded",
      record.with_id(
        "record-workstream-handoff-recorded",
        1044,
        record.WorkstreamHandoffRecorded(
          workstream_id: "linear:LIV-393",
          handoff_id: "handoff-1",
          handoff_ref: "workstream-artifacts/sha256/bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb.json",
          handoff_sha256: string.repeat("b", times: 64),
          handoff_bytes: 456,
          source_workflow_id: "execplan",
          source_run_id: "run-1",
          idempotency_key: "ws-handoff-1",
        ),
      ),
    ),
    LedgerExample(
      "WorkstreamPhaseRunQueued",
      "workstream_phase_run_queued",
      record.with_id(
        "record-workstream-phase-run-queued",
        1045,
        record.WorkstreamPhaseRunQueued(
          workstream_id: "linear:LIV-393",
          phase_run_id: "phase-run-1",
          action_id: "action-1",
          workflow_id: "execplan-implementation",
          input_bundle_ref: "workstream-artifacts/sha256/cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc.json",
          input_bundle_sha256: string.repeat("c", times: 64),
          input_bundle_bytes: 789,
          idempotency_key: "ws-phase-1",
        ),
      ),
    ),
  ]
}

fn request_examples() -> List(RequestExample) {
  [
    RequestExample("Ping", protocol.Ping("req-ping", "secret")),
    RequestExample(
      "ListSessions",
      protocol.ListSessions("req-list-sessions", "secret"),
    ),
    RequestExample(
      "GetSession",
      protocol.GetSession("req-get-session", "secret", "session-1"),
    ),
    RequestExample(
      "GetEvents",
      protocol.GetEvents(
        "req-get-events",
        "secret",
        "session-1",
        after: 7,
        limit: 50,
      ),
    ),
    RequestExample(
      "StreamEvents",
      protocol.StreamEvents(
        "req-stream-events",
        "secret",
        "session-1",
        after: 7,
      ),
    ),
    RequestExample(
      "Query",
      protocol.Query("req-query", "secret", query_types.Status),
    ),
    RequestExample("Pause", protocol.Pause("req-pause", "secret")),
    RequestExample("Resume", protocol.Resume("req-resume", "secret")),
    RequestExample(
      "ReloadWorkflow",
      protocol.ReloadWorkflow("req-reload", "secret"),
    ),
    RequestExample(
      "RetryIssue",
      protocol.RetryIssue(
        "req-retry",
        "secret",
        command.IssueIdentifier("LIV-94"),
      ),
    ),
    RequestExample(
      "RetryIssueStartFresh",
      protocol.RetryIssueStartFresh(
        "req-retry-start-fresh",
        "secret",
        command.IssueIdentifier("LIV-94"),
        "workflow drift",
      ),
    ),
    RequestExample(
      "RetryWorkflowStep",
      protocol.RetryWorkflowStep(
        "req-retry-step",
        "secret",
        command.RetryWorkflowStepRunId("run-1"),
        Some("build"),
      ),
    ),
    RequestExample(
      "RetryWorkflowStepExact",
      protocol.RetryWorkflowStepExact(
        "req-retry-step-exact",
        "secret",
        command.RetryWorkflowStepRunId("run-1"),
        Some("build"),
      ),
    ),
    RequestExample(
      "RecollectWorkflowOutputs",
      protocol.RecollectWorkflowOutputs(
        "req-recollect-outputs",
        "secret",
        "run-1",
      ),
    ),
    RequestExample(
      "RunFinalize",
      protocol.RunFinalize(
        "req-run-finalize",
        "secret",
        "run-1",
        True,
        command.RunFinalizeOutputsAuto,
        True,
        True,
        True,
        "operator salvage",
        False,
      ),
    ),
    RequestExample(
      "RetryArtifactPublication",
      protocol.RetryArtifactPublication(
        "req-retry-publication",
        "secret",
        "run-1",
        Some("review_doc"),
      ),
    ),
    RequestExample(
      "ParkIssue",
      protocol.ParkIssue(
        "req-park",
        "secret",
        command.IssueId("issue-1"),
        "blocked on review",
      ),
    ),
    RequestExample(
      "UnparkIssue",
      protocol.UnparkIssue(
        "req-unpark",
        "secret",
        command.IssueIdentifier("LIV-94"),
      ),
    ),
    RequestExample(
      "AbortSession",
      protocol.AbortSession("req-abort", "secret", "session-1"),
    ),
    RequestExample(
      "StopAfterCurrentTurn",
      protocol.StopAfterCurrentTurn("req-stop", "secret", "session-1"),
    ),
    RequestExample(
      "CleanupOrphanSteps",
      protocol.CleanupOrphanSteps(
        "req-cleanup-orphans",
        "secret",
        "run-1",
        True,
      ),
    ),
    RequestExample(
      "PromptSession",
      protocol.PromptSession(
        "req-prompt",
        "secret",
        "session-1",
        "continue please",
      ),
    ),
    RequestExample(
      "RespondUi",
      protocol.RespondUi(
        "req-respond-ui",
        "secret",
        "session-1",
        "ui-1",
        command.UiValue("approved"),
      ),
    ),
    RequestExample(
      "RunScheduleNow",
      protocol.RunScheduleNow("req-schedule-now", "secret", "nightly-repair"),
    ),
    RequestExample(
      "ReenableSchedule",
      protocol.ReenableSchedule(
        "req-schedule-reenable",
        "secret",
        "nightly-repair",
      ),
    ),
    RequestExample(
      "WorkItemAction",
      protocol.WorkItemAction(
        "req-work-item-action",
        "secret",
        command.WorkItemActionRequest(
          action_id: "work_subtask.cancel",
          action_instance_id: "wia_1",
          target_kind: "workflow_subtask",
          target_provider: Some("linear"),
          target_id: "issue-1",
          observed_fingerprint: "fp-1",
          idempotency_key: "idem-1",
          params: [#("confirm", "true")],
        ),
      ),
    ),
  ]
}

fn projection_fixture_projection() -> projection.Projection {
  projection.Projection(
    runs: dict.from_list([
      #(
        "run-1",
        projection.RunRunning(
          issue_id: "issue-1",
          issue_identifier: "LIV-1",
          workspace_path: ".scherzo/workspaces/LIV-1",
          started_at_ms: 1000,
        ),
      ),
    ]),
    dispatch_paused: False,
    workflow_runs: dict.from_list([
      #(
        "workflow-run-1",
        projection.WorkflowRunActive(
          workflow_id: "implementation",
          workflow_fingerprint: "wf-fingerprint",
          issue_id: "issue-1",
          issue_identifier: "LIV-1",
          issue_fingerprint: "issue-fingerprint",
          observed_updated_at_ms: 999,
          run_root: ".scherzo/workflows/implementation/run-1",
          started_at_ms: 1003,
        ),
      ),
    ]),
    workflow_run_provenances: dict.from_list([
      #(
        "workflow-run-1",
        projection.WorkflowRunProvenance(
          workflow_id: "implementation",
          workflow_fingerprint: "wf-fingerprint",
          issue_id: "issue-1",
          issue_identifier: "LIV-1",
          issue_fingerprint: "issue-fingerprint",
          observed_updated_at_ms: 999,
          run_root: ".scherzo/workflows/implementation/run-1",
          task_ref: record.TaskRefFields(
            task_backend_kind: "linear",
            task_remote_id: "issue-1",
            task_key: Some("LIV-1"),
            task_url: None,
          ),
        ),
      ),
    ]),
    workflow_task_refs: dict.from_list([
      #(
        "workflow-run-1",
        record.TaskRefFields(
          task_backend_kind: "linear",
          task_remote_id: "issue-1",
          task_key: Some("LIV-1"),
          task_url: None,
        ),
      ),
    ]),
    workflow_input_manifests: dict.new(),
    workflow_interface_snapshots: dict.new(),
    workflow_output_manifests: dict.new(),
    publication_attempts: dict.new(),
    publication_latest_by_series: dict.new(),
    workflow_repairs: dict.new(),
    step_attempts: dict.from_list([
      #(
        "workflow-run-1:implement:1",
        projection.StepAttemptRunning(
          run_id: "workflow-run-1",
          workflow_id: "implementation",
          step_id: "implement",
          attempt_index: 1,
          workspace_name: "implementation",
          workspace_path: ".scherzo/workspaces/implementation",
          run_root: ".scherzo/workflows/implementation/run-1",
          source_workspace_name: Some("main"),
          source_workspace_path: Some("/repo"),
          operator_session_id: "operator-session-1",
          external_session_ref: Some("pi-session-1"),
          continuation_capable: True,
          pi_session_id: Some("pi-session-1"),
          pi_session_file: Some(".scherzo/sessions/pi-session-1.json"),
          pi_session_fact_count: 1,
          started_at_ms: 1008,
        ),
      ),
    ]),
    retries: dict.from_list([
      #(
        "issue-1",
        projection.RetryScheduled(
          issue_identifier: "LIV-1",
          delay_ms: 10_000,
          generation: 2,
          reason: "backoff",
          scheduled_at_ms: 1014,
        ),
      ),
    ]),
    parked_issues: dict.from_list([
      #(
        "issue-3",
        projection.ParkedIssue(
          issue_identifier: "LIV-3",
          reason: "max_retry_attempts",
          observed_updated_at_ms: 1018,
          parked_at_ms: 1019,
          release_policy: "auto_unpark_on_issue_change",
          issue_fingerprint: "fingerprint",
        ),
      ),
    ]),
    commands: dict.from_list([
      #(
        "comment-1",
        projection.CommandCompleted(
          issue_id: "issue-1",
          status: "accepted",
          message_excerpt: "queued retry",
          completed_at_ms: 1023,
        ),
      ),
    ]),
    command_receipts: dict.from_list([
      #(
        "comment-1",
        projection.CommandReceiptCompleted(
          issue_id: "issue-1",
          author_id: "user-1",
          command_name: "retry",
          excerpt: "/scherzo retry",
          result_status: "accepted",
          message_excerpt: "queued retry",
          seen_at_ms: 1021,
          started_at_ms: 1022,
          completed_at_ms: 1023,
          acked_at_ms: Some(1024),
        ),
      ),
    ]),
    control_operations: dict.new(),
    outbox: dict.from_list([
      #(
        "outbox-2",
        projection.OutboxPendingV2(
          issue_id: "issue-1",
          outbox_kind: "linear_comment",
          dedupe_key: "run-1:success",
          payload_json: "{\"body\":\"ok\"}",
          pending_at_ms: 1038,
        ),
      ),
    ]),
    issue_counters: dict.from_list([
      #(
        "issue-1",
        projection.IssueCounterStatus(
          issue_identifier: "LIV-1",
          failure_attempts: 1,
          worker_sessions: 2,
          observed_updated_at_ms: 1000,
          source_run_ids: ["run-1"],
          updated_at_ms: 1016,
        ),
      ),
    ]),
    known_workspaces: dict.from_list([
      #(
        "issue-1",
        projection.KnownWorkspace(
          issue_identifier: "LIV-1",
          workspace_path: ".scherzo/workspaces/LIV-1",
          recorded_at_ms: 1017,
        ),
      ),
    ]),
    workstreams: dict.from_list([
      #(
        "linear:LIV-393",
        projection.WorkstreamStatus(
          workstream_id: "linear:LIV-393",
          task_ref: Some(record.linear_task_ref_fields(
            "issue-393",
            Some("LIV-393"),
            None,
          )),
          created_at_ms: Some(1041),
          latest_assignment: Some(projection.WorkstreamAssignment(
            assignment_id: "assignment-1",
            workflow_id: "execplan-implementation",
            playbook_id: Some("playbook-1"),
            reason: "manual_claim",
            idempotency_key: "ws-assign-1",
            assigned_at_ms: 1042,
          )),
          artifacts: dict.from_list([
            #(
              "workstream-artifacts/sha256/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa.json",
              projection.WorkstreamArtifactSnapshot(
                artifact_id: "artifact-1",
                artifact_type: "scherzo.workstream.v1",
                snapshot_ref: "workstream-artifacts/sha256/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa.json",
                snapshot_sha256: string.repeat("a", times: 64),
                snapshot_bytes: 123,
                original_path: "docs/plan.md",
                contract_type: "handoff",
                media_type: "application/json",
                producer_workflow_id: "execplan",
                producer_run_id: "run-1",
                producer_step_id: "step-1",
                idempotency_key: "ws-artifact-1",
                recorded_at_ms: 1043,
              ),
            ),
          ]),
          handoffs: dict.from_list([
            #(
              "workstream-artifacts/sha256/bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb.json",
              projection.WorkstreamHandoffSnapshot(
                handoff_id: "handoff-1",
                handoff_ref: "workstream-artifacts/sha256/bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb.json",
                handoff_sha256: string.repeat("b", times: 64),
                handoff_bytes: 456,
                source_workflow_id: "execplan",
                source_run_id: "run-1",
                idempotency_key: "ws-handoff-1",
                recorded_at_ms: 1044,
              ),
            ),
          ]),
          queued_phase_runs: dict.from_list([
            #(
              "phase-run-1",
              projection.WorkstreamPhaseRun(
                phase_run_id: "phase-run-1",
                action_id: "action-1",
                workflow_id: "execplan-implementation",
                input_bundle_ref: "workstream-artifacts/sha256/cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc.json",
                input_bundle_sha256: string.repeat("c", times: 64),
                input_bundle_bytes: 789,
                idempotency_key: "ws-phase-1",
                queued_at_ms: 1045,
              ),
            ),
          ]),
        ),
      ),
    ]),
    step_recoveries: dict.new(),
    scheduled_jobs: dict.from_list([
      #(
        "nightly-repair",
        projection.ScheduledJobStatus(
          job_id: "nightly-repair",
          workflow_id: "implementation",
          state: projection.ScheduledReportRetryWaiting,
          current_run: Some(projection.ScheduledRunSummary(
            run_id: "scheduled-run-1",
            due_at_ms: 900_000,
            trigger: "automatic",
            attempt: 2,
            status: "report_retry_waiting",
            reason: Some("workflow_step_failed"),
            session_id: Some("session-1"),
            run_root: Some(".scherzo/scheduled/nightly-repair/run-1"),
          )),
          last_due_at_ms: Some(900_000),
          last_success_at_ms: Some(800_000),
          last_success_run_id: Some("scheduled-run-success"),
          last_failure_at_ms: Some(1032),
          last_failure_run_id: Some("scheduled-run-1"),
          last_failure_reason: Some("workflow_step_failed"),
          retry_count: 1,
          skipped_overlap_count: 1,
          skipped_catch_up_count: 0,
          skipped_paused_count: 0,
          skipped_capacity_count: 1,
          failure_issue_id: Some("issue-linear"),
          failure_dedupe_key: Some("scheduled-job:nightly-repair"),
          report_retry: Some(projection.ScheduledReportRetry(
            run_id: "scheduled-run-1",
            attempt: 2,
            dedupe_key: "scheduled-job:nightly-repair",
            error_code: "linear_api_request",
            error_message: "network",
            next_retry_at_ms: 20_000,
            generation: 1,
          )),
          recent_run_ids: ["scheduled-run-1", "scheduled-run-success"],
          consecutive_failure_count: 1,
          quarantine_reason: None,
          quarantined_at_ms: None,
        ),
      ),
    ]),
  )
}

fn control_response_fixture_lines() -> List(String) {
  [
    protocol.success_response("res-ping", protocol.ping_data())
      |> protocol.response_to_string,
    protocol.success_response(
      "res-sessions",
      protocol.list_sessions_data(event.SessionList([sample_session()], 2500)),
    )
      |> protocol.response_to_string,
    protocol.success_response(
      "res-events",
      protocol.event_page_data(sample_event_page()),
    )
      |> protocol.response_to_string,
    protocol.success_response(
      "res-command",
      protocol.command_result_data(sample_command_result()),
    )
      |> protocol.response_to_string,
    protocol.error_response(
      "res-error",
      "unknown_command",
      "unknown command: delete_everything",
    )
      |> protocol.response_to_string,
  ]
}

fn sample_command_result() -> command.CommandResult {
  command.CommandResult(
    command: "abort",
    status: command.Rejected("busy"),
    target: Some("session-1"),
    message: Some("session is busy"),
    operation_id: None,
  )
}

fn sample_session() -> event.SessionSummary {
  event.SessionSummary(
    session_id: "session-1",
    display_name: "LIV-1-implementation",
    issue_id: "issue-1",
    issue_identifier: "LIV-1",
    issue_title: "Implement schema guardrails",
    workspace_path: ".scherzo/workspaces/LIV-1",
    pi_session_id: Some("pi-session-1"),
    status: event.Running,
    recovery: Some(sample_recovery()),
    current_turn: 2,
    current_turn_status: None,
    current_turn_started_at_ms: None,
    last_turn_finished_at_ms: Some(2400),
    last_turn_duration_ms: Some(1000),
    last_turn_token_delta: session_tokens.TokenTotals(
      input: 10,
      output: 5,
      cache_read: 0,
      cache_write: 0,
      total: 15,
    ),
    last_turn_reason: None,
    started_at_ms: 1000,
    last_event_at_ms: 2400,
    token_totals: session_tokens.TokenTotals(
      input: 30,
      output: 10,
      cache_read: 0,
      cache_write: 0,
      total: 40,
    ),
  )
}

fn sample_event_page() -> event.EventPage {
  event.EventPage(
    events: [
      event.SessionEvent(
        cursor: 7,
        at_ms: 2400,
        session_id: "session-1",
        issue_id: "issue-1",
        payload: event.lifecycle_payload(
          event.RecoveryInterrupted,
          Some("daemon_restart"),
          Some(sample_recovery()),
        ),
      ),
    ],
    next_cursor: 7,
    truncated: False,
  )
}

fn sample_recovery() -> event.RecoveryInfo {
  event.RecoveryInfo(
    status: event.Interrupted,
    source: "projection.step_attempt_interrupted",
    message: Some("daemon_restart"),
    safe_actions: [event.Inspect, event.ViewEvents, event.Retry, event.Park],
    workflow_run_id: Some("workflow-run-1"),
    workflow_step_id: Some("implement"),
    workflow_attempt_index: None,
    parent_session_id: None,
    orphan_status: None,
    issue_state: None,
    recommended_action: None,
    current_pi_session_id: Some("pi-session-1"),
    previous_pi_session_id: None,
    park_reason: None,
    park_release_policy: None,
    parked_at_ms: None,
    drift_kind: None,
    retention_until_ms: None,
    cleanup_eligible_at_ms: None,
    cleanup_phase: None,
  )
}

fn constructor_names_from_source(
  path: String,
  type_name: String,
) -> List(String) {
  let assert Ok(contents) = simplifile.read(path)
  collect_constructors(
    contents |> string.split(on: "\n"),
    "pub type " <> type_name <> " {",
    False,
    [],
  )
}

fn collect_constructors(
  lines: List(String),
  header: String,
  inside_type: Bool,
  acc: List(String),
) -> List(String) {
  case lines {
    [] -> list.reverse(acc)
    [line, ..rest] -> {
      let trimmed = string.trim(line)
      case inside_type {
        False -> collect_constructors(rest, header, trimmed == header, acc)
        True ->
          case trimmed == "}" {
            True -> list.reverse(acc)
            False -> {
              let acc = case constructor_name(trimmed) {
                Some(name) -> [name, ..acc]
                None -> acc
              }
              collect_constructors(rest, header, True, acc)
            }
          }
      }
    }
  }
}

fn constructor_name(line: String) -> Option(String) {
  case string.to_graphemes(line) {
    [first, ..] ->
      case is_upper_letter(first) {
        True -> {
          let name = case string.split_once(line, on: "(") {
            Ok(#(name, _)) -> name
            Error(Nil) -> line
          }
          Some(name)
        }
        False -> None
      }
    [] -> None
  }
}

fn is_upper_letter(grapheme: String) -> Bool {
  string.compare(grapheme, "A") != Lt && string.compare(grapheme, "Z") != Gt
}

fn jsonl_fixture_lines(name: String) -> List(String) {
  fixture_text(name)
  |> string.split(on: "\n")
  |> list.map(string.trim)
  |> list.filter(fn(line) { line != "" })
}

fn fixture_text(name: String) -> String {
  let assert Ok(contents) = simplifile.read(fixture_root <> "/" <> name)
  contents
}

fn schema_env(name: String) -> Option(String) {
  case name {
    "LINEAR_API_KEY" -> Some("linearkey")
    "LINEAR_PROJECT_SLUG" -> Some("SCHEMA")
    _ -> None
  }
}
