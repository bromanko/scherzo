import birl
import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/orchestrator/core
import scherzo/orchestrator/scheduled_runtime
import scherzo/orchestrator/startup_recovery
import scherzo/runtime_bundle
import scherzo/state/ledger
import scherzo/state/outbox
import scherzo/state/projection
import scherzo/state/record
import scherzo/state/recovery
import scherzo/tracker
import scherzo/tracker/adapter
import scherzo/tracker/adapter_legacy
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state
import scherzo/workflow_fingerprint
import simplifile
import support/test_helpers

fn issue(id: String, identifier: String) -> tracker_issue.Issue {
  tracker_issue.Issue(
    id: id,
    identifier: identifier,
    title: "Title " <> identifier,
    description: None,
    priority: Some(1),
    state: issue_state.from_string_unchecked("Todo"),
    branch_name: None,
    url: None,
    labels: ["workflow:implementation"],
    blocked_by: [],
    blocked_by_complete: True,
    created_at: Some(birl.from_unix(0)),
    updated_at: Some(birl.from_unix(0)),
  )
}

fn workflow_text(root: String) -> String {
  "version: 1
tracker:
  kind: linear
  api_key: test-key
  project_slug: TEST
  states:
    ready: [Todo]
    active: [Todo]
    terminal: [Done]
  polling:
    every: 1s
workspace:
  root: " <> root <> "
agents:
  concurrency: 1
  sessions_per_task: 2
  retries:
    attempts: 3
  runtime:
    type: pi
    pi:
      executable: fake
task_routing:
  labels:
    require_exactly_one: false
    default_workflow: implementation
workflows:
  implementation: workflows/implementation.yaml
"
}

fn write_bundle(
  dir: String,
  prompt_ref: String,
) -> runtime_bundle.RuntimeBundle {
  write_bundle_with_extra_config(dir, prompt_ref, "")
}

fn write_bundle_with_extra_config(
  dir: String,
  prompt_ref: String,
  extra_config: String,
) -> runtime_bundle.RuntimeBundle {
  test_helpers.reset_dir(dir)
  let config_path = dir <> "/scherzo.yaml"
  let workflow_dir = dir <> "/workflows"
  let prompt_dir = workflow_dir <> "/prompts"
  let assert Ok(Nil) = simplifile.create_directory_all(prompt_dir)
  let assert Ok(Nil) =
    simplifile.write(
      config_path,
      workflow_text(dir <> "/workspaces") <> extra_config,
    )
  let assert Ok(Nil) =
    simplifile.write(workflow_dir <> "/implementation.yaml", "version: 1
id: implementation
steps:
  - id: implement
    kind: agent
    prompt: " <> prompt_ref <> "
    run_in: main
")
  let assert Ok(Nil) = simplifile.write(prompt_dir <> "/task.md", "Prompt")
  let assert Ok(bundle) = runtime_bundle.load(Some(config_path))
  bundle
}

fn scheduled_bundle(
  dir: String,
  entries: List(String),
) -> runtime_bundle.RuntimeBundle {
  write_bundle_with_extra_config(
    dir,
    "prompts/task.md",
    "schedules:\n" <> string.join(entries, ""),
  )
}

fn scheduled_entry(job_id: String, enabled: Bool) -> String {
  "  - id: "
  <> job_id
  <> "\n"
  <> "    workflow: implementation\n"
  <> "    enabled: "
  <> bool_config(enabled)
  <> "\n"
  <> "    every: 1s\n"
  <> "    overlap: skip\n"
  <> "    catch_up: false\n"
}

fn bool_config(value: Bool) -> String {
  case value {
    True -> "true"
    False -> "false"
  }
}

fn tracker_adapter(
  issues: List(tracker_issue.Issue),
) -> adapter.TrackerAdapter {
  adapter_legacy.adapter_from_legacy_client(
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(ids) {
        Ok(list.filter(issues, fn(found) { list.contains(ids, found.id) }))
      },
    ),
    "linear",
  )
}

fn append_test_ledger_bodies(
  root: String,
  bodies: List(record.RecordBody),
) -> Nil {
  let assert Ok(ledger_path) = ledger.path_for_workspace_root(root)
  let records = test_records_for_bodies(bodies, 100, 1)
  let assert Ok(Nil) = ledger.append_many(ledger_path, records, True)
  Nil
}

fn test_records_for_bodies(
  bodies: List(record.RecordBody),
  at_ms: Int,
  sequence: Int,
) -> List(record.LedgerRecord) {
  case bodies {
    [] -> []
    [body, ..rest] -> [
      record.new(at_ms, sequence, body),
      ..test_records_for_bodies(rest, at_ms + 100, sequence + 1)
    ]
  }
}

fn load_records(root: String) -> List(record.LedgerRecord) {
  let assert Ok(ledger_path) = ledger.path_for_workspace_root(root)
  let assert Ok(read) = ledger.read_records(ledger_path)
  read.records
}

fn startup_dependencies() -> startup_recovery.Dependencies {
  startup_recovery.Dependencies(
    logger: fn(_, _, _, _) { Ok(Nil) },
    now_ms: fn() { 7000 },
  )
}

fn scheduled_run(
  run_id: String,
  attempt: Int,
) -> projection.ScheduledRunSummary {
  projection.ScheduledRunSummary(
    run_id: run_id,
    due_at_ms: 5000,
    trigger: "automatic",
    attempt: attempt,
    status: "running",
    reason: None,
    session_id: Some("session-" <> run_id),
    run_root: Some("/tmp/" <> run_id),
  )
}

fn scheduled_status(
  job_id: String,
  state: projection.ScheduledRunState,
  run: projection.ScheduledRunSummary,
  report_retry: Option(projection.ScheduledReportRetry),
) -> projection.ScheduledJobStatus {
  projection.ScheduledJobStatus(
    job_id: job_id,
    workflow_id: "implementation",
    state: state,
    current_run: Some(run),
    last_due_at_ms: Some(run.due_at_ms),
    last_success_at_ms: None,
    last_success_run_id: None,
    last_failure_at_ms: None,
    last_failure_run_id: None,
    last_failure_reason: None,
    retry_count: 0,
    skipped_overlap_count: 0,
    skipped_catch_up_count: 0,
    skipped_paused_count: 0,
    skipped_capacity_count: 0,
    failure_issue_id: None,
    failure_dedupe_key: None,
    report_retry: report_retry,
    recent_run_ids: [run.run_id],
  )
}

pub fn current_workflow_observation_returns_current_workflow_test() {
  let bundle =
    write_bundle(
      "test/tmp/startup-recovery-current-workflow",
      "prompts/task.md",
    )
  let candidate = issue("issue-1", "ABC-1")

  let assert recovery.CurrentWorkflow(
    issue: observed_issue,
    workflow_id: workflow_id,
    workspace_root: workspace_root,
    ..,
  ) = startup_recovery.current_workflow_observation(bundle, candidate)
  assert observed_issue.id == candidate.id
  assert workflow_id == "implementation"
  assert workspace_root == bundle.effective.workspace.root
}

pub fn current_workflow_observation_uses_default_workflow_for_unlabelled_issue_test() {
  let bundle =
    write_bundle(
      "test/tmp/startup-recovery-default-workflow",
      "prompts/task.md",
    )
  let candidate = tracker_issue.Issue(..issue("issue-2", "ABC-2"), labels: [])

  let assert recovery.CurrentWorkflow(workflow_id: workflow_id, ..) =
    startup_recovery.current_workflow_observation(bundle, candidate)
  assert workflow_id == "implementation"
}

pub fn load_recovers_interrupted_run_retry_metadata_test() {
  let bundle =
    write_bundle("test/tmp/startup-recovery-interrupted-run", "prompts/task.md")
  let candidate = issue("issue-1", "ABC-1")
  let workspace_root = bundle.effective.workspace.root

  append_test_ledger_bodies(workspace_root, [
    record.RunStarted(
      run_id: "run-1",
      issue_id: candidate.id,
      issue_identifier: candidate.identifier,
      workspace_path: workspace_root <> "/ABC-1",
    ),
  ])

  let assert Ok(loaded) =
    startup_recovery.load(
      bundle,
      tracker_adapter([candidate]),
      startup_dependencies(),
      [],
    )

  let assert [
    recovery.RecoveredRetry(
      issue_id: "issue-1",
      issue_identifier: "ABC-1",
      generation: 1,
      reason: "failure",
      ..,
    ),
  ] = loaded.retry_timers
  assert dict.has_key(loaded.recovery_by_issue, "issue-1")
  assert list.any(load_records(workspace_root), fn(entry) {
    case entry.body {
      record.RunInterrupted(run_id: run_id, issue_id: issue_id, ..) ->
        run_id == "run-1" && issue_id == "issue-1"
      _ -> False
    }
  })
}

pub fn load_recovers_cleanup_request_for_terminal_interrupted_run_test() {
  let bundle =
    write_bundle("test/tmp/startup-recovery-cleanup-request", "prompts/task.md")
  let candidate =
    tracker_issue.Issue(
      ..issue("issue-1", "ABC-1"),
      state: issue_state.from_string_unchecked("Done"),
    )
  let workspace_root = bundle.effective.workspace.root
  let workspace_path = workspace_root <> "/ABC-1"

  append_test_ledger_bodies(workspace_root, [
    record.RunStarted(
      run_id: "run-1",
      issue_id: candidate.id,
      issue_identifier: candidate.identifier,
      workspace_path: workspace_path,
    ),
  ])

  let assert Ok(loaded) =
    startup_recovery.load(
      bundle,
      tracker_adapter([candidate]),
      startup_dependencies(),
      [],
    )

  let assert [
    recovery.CleanupRequest(
      issue_id: "issue-1",
      issue_identifier: "ABC-1",
      workspace_path: recovered_workspace,
    ),
  ] = loaded.cleanup_workspaces
  assert recovered_workspace == workspace_path
  assert loaded.retry_timers == []
}

pub fn load_marks_pending_command_outbox_failed_test() {
  let bundle =
    write_bundle("test/tmp/startup-recovery-outbox", "prompts/task.md")
  let workspace_root = bundle.effective.workspace.root

  append_test_ledger_bodies(workspace_root, [
    record.OutboxPendingV2(
      outbox_id: "comment-1",
      issue_id: "issue-1",
      outbox_kind: "linear_command_ack",
      dedupe_key: "linear_command_ack:comment-1",
      payload_json: outbox.linear_command_ack_payload("comment-1", "ack", []),
    ),
  ])

  let assert Ok(loaded) =
    startup_recovery.load(
      bundle,
      tracker_adapter([]),
      startup_dependencies(),
      [],
    )

  assert loaded.outbox_to_replay == []
  assert list.contains(
    loaded.warnings,
    "outbox_replay_failed:comment-1:unsupported_outbox_kind:linear_command_ack",
  )
  assert list.any(load_records(workspace_root), fn(entry) {
    case entry.body {
      record.OutboxFailedWithTask(
        outbox_id: outbox_id,
        error_code: error_code,
        ..,
      ) ->
        outbox_id == "comment-1"
        && error_code == "unsupported_outbox_kind:linear_command_ack"
      _ -> False
    }
  })
}

pub fn load_emits_park_report_for_workflow_identity_mismatch_test() {
  let bundle =
    write_bundle("test/tmp/startup-recovery-park-report", "prompts/task.md")
  let original_issue = issue("issue-1", "ABC-1")
  let changed_issue =
    tracker_issue.Issue(..original_issue, title: "Changed title ABC-1")
  let workspace_root = bundle.effective.workspace.root
  let run_root = workspace_root <> "/implementation/ABC-1/run-1"
  let assert Ok(#(_, dag)) =
    runtime_bundle.select_workflow(bundle, original_issue)
  let assert Ok(fingerprint) =
    workflow_fingerprint.fingerprint_for_execution(dag, bundle.orchestrator)

  append_test_ledger_bodies(workspace_root, [
    record.WorkflowRunStarted(
      "run-1",
      "implementation",
      fingerprint,
      original_issue.id,
      original_issue.identifier,
      core.issue_fingerprint(original_issue),
      0,
      run_root,
    ),
  ])

  let assert Ok(loaded) =
    startup_recovery.load(
      bundle,
      tracker_adapter([changed_issue]),
      startup_dependencies(),
      [],
    )

  let assert [
    adapter.ParkReport(
      issue_identifier: issue_identifier,
      reason: reason,
      run_id: Some(run_id),
      ..,
    ),
  ] = loaded.park_reports
  assert issue_identifier == "ABC-1"
  assert run_id == "run-1"
  assert string.starts_with(reason, "issue_content_drift:")
  assert loaded.workflow_resumptions == []
}

pub fn recover_scheduled_runtime_restores_report_retry_timer_test() {
  let bundle =
    scheduled_bundle("test/tmp/startup-recovery-scheduled-report-retry", [
      scheduled_entry("scheduled-job", True),
    ])
  let report_retry =
    projection.ScheduledReportRetry(
      run_id: "run-report",
      attempt: 3,
      dedupe_key: "dedupe-run-report",
      error_code: "linear_failed",
      error_message: "temporary failure",
      next_retry_at_ms: 7500,
      generation: 4,
    )

  let recovery =
    startup_recovery.recover_scheduled_runtime(bundle, 7000, [
      scheduled_status(
        "scheduled-job",
        projection.ScheduledReportRetryWaiting,
        scheduled_run("run-report", 3),
        Some(report_retry),
      ),
    ])

  assert scheduled_runtime.report_retry_tick_matches(
    recovery.runtime,
    "run-report",
    4,
  )
  assert recovery.effects
    == [
      startup_recovery.ScheduleReportRetryTimer(
        run_id: "run-report",
        generation: 4,
        delay_ms: 500,
      ),
    ]
}

pub fn recover_scheduled_runtime_records_disabled_jobs_without_dispatch_test() {
  let bundle =
    scheduled_bundle("test/tmp/startup-recovery-disabled-scheduled-jobs", [
      scheduled_entry("disabled-pending", False),
      scheduled_entry("disabled-active", False),
      scheduled_entry("disabled-retry", False),
    ])

  let recovery =
    startup_recovery.recover_scheduled_runtime(bundle, 7000, [
      scheduled_status(
        "disabled-pending",
        projection.ScheduledDuePending,
        scheduled_run("run-pending", 1),
        None,
      ),
      scheduled_status(
        "disabled-active",
        projection.ScheduledActive,
        scheduled_run("run-active", 2),
        None,
      ),
      scheduled_status(
        "disabled-retry",
        projection.ScheduledRetryWaiting,
        scheduled_run("run-retry", 3),
        None,
      ),
    ])

  assert scheduled_runtime.pending_starts(recovery.runtime) == []
  assert scheduled_runtime.retry_run_ids(recovery.runtime) == []
  assert recovery.effects
    == [
      startup_recovery.AppendLedger(
        record_bodies: [
          record.ScheduledRunPendingCancelled(
            "disabled-pending",
            "implementation",
            5000,
            "run-pending",
            "job_disabled",
            7000,
          ),
        ],
        failure_event: "scheduled_recovery_append_failed",
      ),
      startup_recovery.AppendLedger(
        record_bodies: [
          record.ScheduledRunFailed(
            "disabled-active",
            "implementation",
            5000,
            "run-active",
            2,
            7000,
            "daemon_restart",
            True,
            Some("/tmp/run-active"),
          ),
        ],
        failure_event: "scheduled_recovery_append_failed",
      ),
      startup_recovery.AppendLedger(
        record_bodies: [
          record.ScheduledRunRetryCancelled(
            "disabled-retry",
            "run-retry",
            0,
            "job_disabled",
          ),
        ],
        failure_event: "scheduled_recovery_append_failed",
      ),
    ]
}
