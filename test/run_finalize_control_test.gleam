import gleam/list
import gleam/option.{None, Some}
import scherzo/control/command
import scherzo/orchestrator/run_finalize_control
import scherzo/state/projection
import scherzo/state/record

pub fn run_finalize_dry_run_rejects_missing_run_test() {
  let projected = projection.fold([])

  assert run_finalize_control.dry_run(projected, "missing-run")
    == Error(#("run_not_found", "run not found"))
}

pub fn run_finalize_dry_run_rejects_active_run_test() {
  let projected = projection.fold(active_run_records("run-1"))

  assert run_finalize_control.dry_run(projected, "run-1")
    == Error(#("run_active", "run finalize requires a non-active run"))
}

pub fn run_finalize_dry_run_reports_recollect_outputs_and_publication_test() {
  let projected = projection.fold(interrupted_run_records("run-1", False))
  let assert Ok(plan) = run_finalize_control.dry_run(projected, "run-1")

  assert plan.output_action == "recollect_outputs"
  assert plan.publication_ids == ["execplan_review_doc"]
  assert !plan.already_finalized
  assert run_finalize_control.dry_run_message(plan)
    == "dry run: run is not finalized yet; would validate retained evidence, recollect materialized outputs, verify publication status for 1 target(s): execplan_review_doc=failed(required), update tracker, and append workflow_run_finished without starting a worker"
}

pub fn run_finalize_queue_decision_rejects_unpublished_required_publication_test() {
  let operator_command = operator_command()
  let projected = projection.fold(interrupted_run_records("run-1", True))

  assert run_finalize_control.queue_decision(
      projected,
      operator_command,
      "run-1",
      42,
      allow_unpublished: False,
    )
    == Error(#(
      "publication_pending",
      "run finalize blocked: required publication route(s) are not published for run run-1: execplan_review_doc=failed(required). Next: scherzoctl publication retry run-1 --publication execplan_review_doc --json, then rerun run finalize. Override only with --allow-unpublished --reason <text>.",
    ))
}

pub fn run_finalize_queue_decision_queues_after_required_publication_published_test() {
  let operator_command = operator_command()
  let projected =
    projection.fold(interrupted_run_records_with_publication_status(
      "run-1",
      True,
      "published",
    ))

  let assert Ok(run_finalize_control.NewOperation(operation_id, queued_body)) =
    run_finalize_control.queue_decision(
      projected,
      operator_command,
      "run-1",
      42,
      allow_unpublished: False,
    )
  assert operation_id == "run-finalize:run-1:42"
  let assert record.ControlOperationQueued(
    operation_id: "run-finalize:run-1:42",
    operation_kind: "run_finalize",
    run_id: Some("run-1"),
    issue_id: Some("issue-1"),
    issue_identifier: Some("LIV-1336"),
    ..,
  ) = queued_body
}

pub fn run_finalize_queue_decision_returns_already_finalized_for_finished_run_test() {
  let projected = projection.fold(finished_run_records("run-1"))
  let operator_command =
    command.RunFinalize(
      run_id: "run-1",
      validate: True,
      outputs: command.RunFinalizeOutputsAuto,
      publish: True,
      update_tracker: True,
      dry_run: False,
      reason: "operator salvage",
      allow_unpublished: False,
    )

  let assert Ok(run_finalize_control.AlreadyFinalized(message)) =
    run_finalize_control.queue_decision(
      projected,
      operator_command,
      "run-1",
      42,
      allow_unpublished: False,
    )
  assert message == "run run-1 is already finalized"
}

pub fn run_finalize_queue_decision_reuses_incomplete_operation_test() {
  let operator_command = operator_command()
  let projected =
    projection.fold(
      list.append(interrupted_run_records("run-1", True), [
        record.with_id(
          "run-finalize-queued",
          20,
          record.ControlOperationQueued(
            operation_id: "run-finalize:run-1:20",
            operation_kind: "run_finalize",
            command_name: command.command_name(operator_command),
            target: "run:run-1",
            run_id: Some("run-1"),
            issue_id: Some("issue-1"),
            issue_identifier: Some("LIV-1336"),
            requested_step_id: None,
            publication_id: None,
          ),
        ),
      ]),
    )

  let assert Ok(run_finalize_control.ExistingOperation(operation_id)) =
    run_finalize_control.queue_decision(
      projected,
      operator_command,
      "run-1",
      42,
      allow_unpublished: False,
    )
  assert operation_id == "run-finalize:run-1:20"
}

pub fn run_finalize_dry_run_rejects_superseded_run_test() {
  let projected = projection.fold(superseded_run_records("run-1"))

  assert run_finalize_control.dry_run(projected, "run-1")
    == Error(#(
      "run_superseded",
      "run finalize cannot finalize a superseded run",
    ))
}

pub fn run_finalize_queue_decision_rejects_overlapping_publication_retry_test() {
  let operator_command = operator_command()
  let projected =
    projection.fold(
      list.append(interrupted_run_records("run-1", True), [
        record.with_id(
          "publication-retry-queued",
          20,
          record.ControlOperationQueued(
            operation_id: "artifact-publication-retry:run-1:all:20",
            operation_kind: "artifact_publication_retry",
            command_name: "artifact_publication_retry",
            target: "run:run-1",
            run_id: Some("run-1"),
            issue_id: Some("issue-1"),
            issue_identifier: Some("LIV-1336"),
            requested_step_id: None,
            publication_id: None,
          ),
        ),
      ]),
    )

  let assert Ok(run_finalize_control.ConflictingOperation(operation_id, kind, _)) =
    run_finalize_control.queue_decision(
      projected,
      operator_command,
      "run-1",
      42,
      allow_unpublished: False,
    )
  assert operation_id == "artifact-publication-retry:run-1:all:20"
  assert kind == "artifact_publication_retry"
}

fn operator_command() -> command.OperatorCommand {
  command.RunFinalize(
    run_id: "run-1",
    validate: True,
    outputs: command.RunFinalizeOutputsAuto,
    publish: True,
    update_tracker: True,
    dry_run: False,
    reason: "operator salvage",
    allow_unpublished: False,
  )
}

fn active_run_records(run_id: String) -> List(record.LedgerRecord) {
  started_records(run_id)
}

fn interrupted_run_records(
  run_id: String,
  include_output_manifest: Bool,
) -> List(record.LedgerRecord) {
  interrupted_run_records_with_publication_status(
    run_id,
    include_output_manifest,
    "failed",
  )
}

fn interrupted_run_records_with_publication_status(
  run_id: String,
  include_output_manifest: Bool,
  publication_status: String,
) -> List(record.LedgerRecord) {
  let output_records = case include_output_manifest {
    True -> [output_manifest_record(run_id)]
    False -> []
  }
  list.append(
    started_records(run_id),
    list.append(output_records, [
      record.with_id(
        "workflow-run-interrupted-" <> run_id,
        20,
        record.WorkflowRunInterrupted(
          run_id: run_id,
          workflow_id: "execplan",
          issue_id: "issue-1",
          reason: "operator_stop",
        ),
      ),
      publication_record_with_status(run_id, publication_status),
    ]),
  )
}

fn finished_run_records(run_id: String) -> List(record.LedgerRecord) {
  list.append(started_records(run_id), [
    output_manifest_record(run_id),
    record.with_id(
      "workflow-run-finished-" <> run_id,
      20,
      record.WorkflowRunFinishedWithTask(
        run_id: run_id,
        workflow_id: "execplan",
        issue_id: "issue-1",
        task_ref: task_ref(),
        outcome: "success",
        token_total: 0,
        turns: 0,
      ),
    ),
    publication_record(run_id),
  ])
}

fn superseded_run_records(run_id: String) -> List(record.LedgerRecord) {
  list.append(started_records(run_id), [
    output_manifest_record(run_id),
    record.with_id(
      "workflow-run-superseded-" <> run_id,
      20,
      record.WorkflowRunSuperseded(
        run_id: run_id,
        workflow_id: "execplan",
        issue_id: "issue-1",
        superseded_by_run_id: "run-2",
        reason: "newer_run_started",
      ),
    ),
    publication_record(run_id),
  ])
}

fn started_records(run_id: String) -> List(record.LedgerRecord) {
  [
    record.with_id(
      "workflow-run-started-" <> run_id,
      10,
      record.WorkflowRunStartedWithTask(
        run_id: run_id,
        workflow_id: "execplan",
        workflow_fingerprint: "workflow-fingerprint",
        issue_id: "issue-1",
        issue_identifier: "LIV-1336",
        task_ref: task_ref(),
        issue_fingerprint: "issue-fingerprint",
        observed_updated_at_ms: 9,
        run_root: "/tmp/" <> run_id,
      ),
    ),
  ]
}

fn output_manifest_record(run_id: String) -> record.LedgerRecord {
  record.with_id(
    "workflow-run-outputs-" <> run_id,
    15,
    record.WorkflowRunOutputsRecorded(
      run_id: run_id,
      workflow_id: "execplan",
      workflow_fingerprint: "workflow-fingerprint",
      artifact_ref: "runs/" <> run_id <> "/outputs.v1.json",
      artifact_sha256: "sha256",
      artifact_bytes: 10,
    ),
  )
}

fn publication_record(run_id: String) -> record.LedgerRecord {
  publication_record_with_status(run_id, "failed")
}

fn publication_record_with_status(
  run_id: String,
  status: String,
) -> record.LedgerRecord {
  let error_code = case status {
    "published" -> None
    _ -> Some("git_push_failed")
  }
  let error_message = case status {
    "published" -> None
    _ -> Some("push failed")
  }
  record.with_id(
    "publication-" <> run_id,
    25,
    record.PublicationAttemptRecorded(
      run_id: run_id,
      workflow_id: "execplan",
      publication_id: "execplan_review_doc",
      series_id: "series-1",
      attempt_id: "attempt-1",
      status: status,
      required: True,
      retryable: status != "published",
      retry_execution_available: status != "published",
      version_id: None,
      manifest_ref: None,
      manifest_sha256: None,
      manifest_bytes: None,
      error_code: error_code,
      error_message: error_message,
    ),
  )
}

fn task_ref() -> record.TaskRefFields {
  record.linear_task_ref_fields(
    "issue-1",
    Some("LIV-1336"),
    Some("https://linear.app/living-systems/issue/LIV-1336"),
  )
}
