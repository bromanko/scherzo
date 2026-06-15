import birl
import gleam/dict
import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/orchestrator/core
import scherzo/orchestrator/scheduled_runtime
import scherzo/orchestrator/startup_recovery
import scherzo/runtime/reason
import scherzo/runtime/state as orchestrator_state
import scherzo/runtime_bundle
import scherzo/state/ledger
import scherzo/state/outbox
import scherzo/state/projection
import scherzo/state/record
import scherzo/state/recovery
import scherzo/task
import scherzo/tracker
import scherzo/tracker/adapter
import scherzo/tracker/adapter_legacy
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state
import scherzo/workflow_fingerprint
import simplifile
import support/test_helpers
import test_async

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

fn count_record_kind(records: List(record.LedgerRecord), kind: String) -> Int {
  records
  |> list.filter(fn(entry) { record.kind(entry.body) == kind })
  |> list.length
}

fn many_retry_scheduled_records(
  remaining: Int,
  index: Int,
  acc: List(record.RecordBody),
) -> List(record.RecordBody) {
  case remaining <= 0 {
    True -> list.reverse(acc)
    False ->
      many_retry_scheduled_records(remaining - 1, index + 1, [
        record.RetryScheduled(
          issue_id: "issue-" <> int.to_string(index),
          issue_identifier: "ABC-" <> int.to_string(index),
          delay_ms: 2500,
          generation: 1,
          reason: "continuation",
        ),
        ..acc
      ])
  }
}

fn many_issues(
  remaining: Int,
  acc: List(tracker_issue.Issue),
) -> List(tracker_issue.Issue) {
  case remaining <= 0 {
    True -> list.reverse(acc)
    False ->
      many_issues(remaining - 1, [
        issue(
          "issue-" <> int.to_string(remaining),
          "ABC-" <> int.to_string(remaining),
        ),
        ..acc
      ])
  }
}

fn release_claim_record_count(
  records: List(record.LedgerRecord),
  release_key: String,
) -> Int {
  records
  |> list.filter(fn(entry) {
    case entry.body {
      record.OutboxPendingV2WithTask(
        outbox_id: outbox_id,
        outbox_kind: "release_claim",
        ..,
      ) -> outbox_id == release_key
      _ -> False
    }
  })
  |> list.length
}

fn abandoned_park_record_count(
  records: List(record.LedgerRecord),
  issue_id: String,
  reason_text: String,
) -> Int {
  records
  |> list.filter(fn(entry) {
    case entry.body {
      record.IssueParkedV2(issue_id: parked_issue_id, reason: parked_reason, ..) ->
        parked_issue_id == issue_id && parked_reason == reason_text
      _ -> False
    }
  })
  |> list.length
}

fn startup_dependencies() -> startup_recovery.Dependencies {
  startup_dependencies_with_sleep(process.new_subject())
}

fn startup_dependencies_with_sleep(
  sleeps: process.Subject(Int),
) -> startup_recovery.Dependencies {
  startup_recovery.Dependencies(
    logger: fn(_, _, _, _) { Ok(Nil) },
    now_ms: fn() { 7000 },
    sleep_ms: fn(delay_ms) {
      process.send(sleeps, delay_ms)
      Nil
    },
  )
}

type FakeRefreshAdapter {
  FakeRefreshAdapter(
    tracker_adapter: adapter.TrackerAdapter,
    refresh_calls: process.Subject(List(String)),
  )
}

fn fake_refresh_adapter(
  refresh_results: process.Subject(
    Result(List(tracker_issue.Issue), adapter.TrackerError),
  ),
) -> FakeRefreshAdapter {
  let refresh_calls = process.new_subject()
  let tracker_adapter =
    adapter.TrackerAdapter(
      kind: "linear",
      display_name: "Linear",
      task_source: adapter.TaskSourceCapability(
        fetch_candidates: fn(_) { Ok([]) },
        refresh_by_refs: fn(refs) {
          process.send(refresh_calls, list.map(refs, fn(ref) { ref.remote_id }))
          let result = test_async.expect_message(refresh_results)
          case result {
            Ok(issues) -> Ok(list.map(issues, task.from_legacy_issue))
            Error(err) -> Error(err)
          }
        },
        lookup_by_operator_ref: fn(_) { Ok(None) },
        list_tasks: fn(_) { Ok(adapter.TaskPage([], False)) },
        lookup_task_detail: fn(_) { Ok(None) },
      ),
      comments: None,
      remote_commands: None,
      state_transitions: None,
      routing_metadata: None,
      links: None,
      handoff: None,
      scheduled_failures: None,
      readiness: None,
      smoke: None,
      attachments: None,
    )
  FakeRefreshAdapter(tracker_adapter, refresh_calls)
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

fn scheduled_failure_task_ref(job_id: String) -> record.TaskRefFields {
  record.TaskRefFields(
    task_backend_kind: "scheduled_failure",
    task_remote_id: job_id,
    task_key: Some(job_id),
    task_url: None,
  )
}

fn scheduled_failure_payload(
  job_id: String,
  run_id: String,
  report_attempt_index: Int,
) -> String {
  let dedupe_key = "scheduled-job:" <> job_id
  outbox.scheduled_failure_payload(
    outbox.ScheduledFailurePayload(
      kind: outbox.scheduled_failure_publication_kind,
      job_id: job_id,
      workflow_id: "implementation",
      due_at_ms: 5000,
      run_id: run_id,
      attempt: 1,
      max_attempts: 1,
      reason: "daemon_restart",
      run_root: Some("/tmp/" <> run_id),
      session_id: Some("session-" <> run_id),
      dedupe_key: dedupe_key,
      title: "Scheduled workflow failure: " <> job_id,
      body: "daemon_restart",
      labels: ["job:" <> job_id],
      target_state_name: Some("Triage"),
      previous_task_remote_id: None,
      report_attempt_index: report_attempt_index,
    ),
    [],
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

pub fn load_recovers_interrupted_run_as_parked_issue_test() {
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

  assert loaded.retry_timers == []
  let identity = orchestrator_state.linear_issue_id_identity("issue-1")
  let assert Ok(parked) = dict.get(loaded.runtime.parked, identity)
  assert parked.reason == reason.ParkWorkerFailure
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

pub fn load_replays_claim_start_append_failure_retry_test() {
  let bundle =
    write_bundle(
      "test/tmp/startup-recovery-claim-start-retry",
      "prompts/task.md",
    )
  let candidate = issue("issue-1", "ABC-1")
  let workspace_root = bundle.effective.workspace.root

  append_test_ledger_bodies(workspace_root, [
    record.RetryScheduled(
      issue_id: candidate.id,
      issue_identifier: candidate.identifier,
      delay_ms: 2500,
      generation: 3,
      reason: "claim_start_ledger_append_failed",
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
      delay_ms: 0,
      generation: 3,
      reason: "claim_start_ledger_append_failed",
    ),
  ] = loaded.retry_timers
  let task_identity = orchestrator_state.linear_issue_id_identity("issue-1")
  assert dict.has_key(loaded.runtime.retry_attempts, task_identity)
  assert dict.get(loaded.runtime.claimed, task_identity) == Ok("ABC-1")
  assert dict.has_key(loaded.recovery_by_issue, "issue-1")
}

pub fn load_replays_pending_command_outbox_test() {
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

  assert loaded.outbox_to_replay
    == [
      recovery.OutboxReplay(
        "comment-1",
        record.linear_task_ref_fields("issue-1", None, None),
        "linear_command_ack",
        "linear_command_ack:comment-1",
        outbox.linear_command_ack_payload("comment-1", "ack", []),
      ),
    ]
  assert loaded.warnings == []
  assert !list.any(load_records(workspace_root), fn(entry) {
    case entry.body {
      record.OutboxFailedWithTask(outbox_id: "comment-1", ..) -> True
      _ -> False
    }
  })
}

pub fn load_replays_pending_scheduled_failure_outbox_test() {
  let bundle =
    write_bundle(
      "test/tmp/startup-recovery-scheduled-failure-outbox",
      "prompts/task.md",
    )
  let workspace_root = bundle.effective.workspace.root
  let dedupe_key = "scheduled-job:scheduled-job"
  let payload_json = scheduled_failure_payload("scheduled-job", "run-1", 1)

  append_test_ledger_bodies(workspace_root, [
    record.OutboxPendingV2WithTask(
      outbox_id: dedupe_key,
      task_ref: scheduled_failure_task_ref("scheduled-job"),
      outbox_kind: outbox.scheduled_failure_publication_kind,
      dedupe_key: dedupe_key,
      payload_json: payload_json,
    ),
  ])

  let assert Ok(loaded) =
    startup_recovery.load(
      bundle,
      tracker_adapter([]),
      startup_dependencies(),
      [],
    )

  assert loaded.outbox_to_replay
    == [
      recovery.OutboxReplay(
        dedupe_key,
        scheduled_failure_task_ref("scheduled-job"),
        outbox.scheduled_failure_publication_kind,
        dedupe_key,
        payload_json,
      ),
    ]
}

pub fn load_replays_due_scheduled_failure_retry_outbox_only_test() {
  let bundle =
    write_bundle(
      "test/tmp/startup-recovery-scheduled-failure-retry-outbox",
      "prompts/task.md",
    )
  let workspace_root = bundle.effective.workspace.root
  let due_key = "scheduled-job:due-job"
  let future_key = "scheduled-job:future-job"
  let due_payload = scheduled_failure_payload("due-job", "run-due", 2)
  let future_payload = scheduled_failure_payload("future-job", "run-future", 2)

  append_test_ledger_bodies(workspace_root, [
    record.OutboxRetryScheduledWithTask(
      outbox_id: due_key,
      task_ref: scheduled_failure_task_ref("due-job"),
      outbox_kind: outbox.scheduled_failure_publication_kind,
      dedupe_key: due_key,
      payload_json: due_payload,
      error_code: "tracker_transient",
      attempt_count: 1,
      next_attempt_at_ms: 6500,
    ),
    record.OutboxRetryScheduledWithTask(
      outbox_id: future_key,
      task_ref: scheduled_failure_task_ref("future-job"),
      outbox_kind: outbox.scheduled_failure_publication_kind,
      dedupe_key: future_key,
      payload_json: future_payload,
      error_code: "tracker_transient",
      attempt_count: 1,
      next_attempt_at_ms: 9000,
    ),
  ])

  let assert Ok(loaded) =
    startup_recovery.load(
      bundle,
      tracker_adapter([]),
      startup_dependencies(),
      [],
    )

  assert loaded.outbox_to_replay
    == [
      recovery.OutboxReplay(
        due_key,
        scheduled_failure_task_ref("due-job"),
        outbox.scheduled_failure_publication_kind,
        due_key,
        due_payload,
      ),
    ]
}

pub fn load_marks_malformed_scheduled_failure_outbox_failed_test() {
  let bundle =
    write_bundle(
      "test/tmp/startup-recovery-malformed-scheduled-failure-outbox",
      "prompts/task.md",
    )
  let workspace_root = bundle.effective.workspace.root
  let dedupe_key = "scheduled-job:scheduled-job"
  let payload_json =
    outbox.bounded_payload_json(
      outbox.scheduled_failure_publication_kind,
      "",
      [],
    )

  append_test_ledger_bodies(workspace_root, [
    record.OutboxPendingV2WithTask(
      outbox_id: dedupe_key,
      task_ref: scheduled_failure_task_ref("scheduled-job"),
      outbox_kind: outbox.scheduled_failure_publication_kind,
      dedupe_key: dedupe_key,
      payload_json: payload_json,
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
  assert loaded.warnings
    == [
      "outbox_replay_failed:scheduled-job:scheduled-job:invalid_outbox_payload",
    ]
  assert list.any(load_records(workspace_root), fn(entry) {
    case entry.body {
      record.OutboxFailedWithTask(
        outbox_id: "scheduled-job:scheduled-job",
        outbox_kind: kind,
        error_code: "invalid_outbox_payload",
        ..,
      ) -> kind == outbox.scheduled_failure_publication_kind
      _ -> False
    }
  })
}

pub fn load_suppresses_completed_and_permanent_scheduled_failure_outbox_test() {
  let bundle =
    write_bundle(
      "test/tmp/startup-recovery-scheduled-failure-terminal-outbox",
      "prompts/task.md",
    )
  let workspace_root = bundle.effective.workspace.root
  let completed_key = "scheduled-job:completed-job"
  let permanent_key = "scheduled-job:permanent-job"

  append_test_ledger_bodies(workspace_root, [
    record.OutboxCompletedWithTask(
      outbox_id: completed_key,
      task_ref: scheduled_failure_task_ref("completed-job"),
      outbox_kind: outbox.scheduled_failure_publication_kind,
    ),
    record.OutboxPermanentlyFailedWithTask(
      outbox_id: permanent_key,
      task_ref: scheduled_failure_task_ref("permanent-job"),
      outbox_kind: outbox.scheduled_failure_publication_kind,
      error_code: "tracker_permanent",
      attempt_count: 1,
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
}

pub fn load_compensates_recovered_claim_outbox_with_release_claim_test() {
  let bundle =
    write_bundle("test/tmp/startup-recovery-release-claim", "prompts/task.md")
  let candidate = issue("issue-1", "ABC-1")
  let workspace_root = bundle.effective.workspace.root
  let claim_key = "claim:linear:issue-1:run-1"

  append_test_ledger_bodies(workspace_root, [
    record.OutboxPendingV2WithTask(
      outbox_id: claim_key,
      task_ref: record.linear_task_ref_fields("issue-1", Some("ABC-1"), None),
      outbox_kind: "claim",
      dedupe_key: claim_key,
      payload_json: outbox.tracker_update_payload(
        "claim",
        claim_key,
        "claimed",
        None,
        None,
        [],
      ),
    ),
  ])

  let assert Ok(loaded) =
    startup_recovery.load(
      bundle,
      tracker_adapter([candidate]),
      startup_dependencies(),
      [],
    )

  let release_key = "release_claim:linear:issue-1:run-1"
  let assert [release_replay] = loaded.outbox_to_replay
  let recovery.OutboxReplay(
    outbox_id: release_outbox_id,
    task_ref: release_task_ref,
    outbox_kind: release_kind,
    dedupe_key: release_dedupe_key,
    ..,
  ) = release_replay
  assert release_outbox_id == release_key
  assert release_task_ref.task_remote_id == "issue-1"
  assert release_kind == "release_claim"
  assert release_dedupe_key == release_key
  let identity = orchestrator_state.linear_issue_id_identity("issue-1")
  let assert Ok(parked) = dict.get(loaded.runtime.parked, identity)
  assert parked.release_policy == orchestrator_state.ExplicitUnparkOnly
  assert parked.reason
    == reason.ParkOperator("abandoned_claim:startup_recovered_claim")
  assert loaded.park_reports == []
  let records = load_records(workspace_root)
  assert list.any(records, fn(entry) {
    case entry.body {
      record.OutboxPermanentlyFailedWithTask(
        outbox_id: outbox_id,
        outbox_kind: "claim",
        error_code: "abandoned_claim_recovered",
        ..,
      ) -> outbox_id == claim_key
      _ -> False
    }
  })
  assert list.any(records, fn(entry) {
    case entry.body {
      record.IssueParkedV2(
        issue_id: "issue-1",
        reason: "abandoned_claim:startup_recovered_claim",
        release_policy: "explicit_unpark_only",
        ..,
      ) -> True
      _ -> False
    }
  })
}

pub fn load_compensates_permanent_claim_outbox_failure_test() {
  let bundle =
    write_bundle(
      "test/tmp/startup-recovery-release-claim-permanent",
      "prompts/task.md",
    )
  let candidate = issue("issue-1", "ABC-1")
  let workspace_root = bundle.effective.workspace.root
  let claim_key = "claim:linear:issue-1:run-1"

  append_test_ledger_bodies(workspace_root, [
    record.OutboxPermanentlyFailedWithTask(
      outbox_id: claim_key,
      task_ref: record.linear_task_ref_fields("issue-1", Some("ABC-1"), None),
      outbox_kind: "claim",
      error_code: "unauthorized",
      attempt_count: 1,
    ),
  ])

  let assert Ok(loaded) =
    startup_recovery.load(
      bundle,
      tracker_adapter([candidate]),
      startup_dependencies(),
      [],
    )

  let assert [release_replay] = loaded.outbox_to_replay
  let recovery.OutboxReplay(
    outbox_id: release_outbox_id,
    outbox_kind: release_kind,
    ..,
  ) = release_replay
  assert release_outbox_id == "release_claim:linear:issue-1:run-1"
  assert release_kind == "release_claim"
  let identity = orchestrator_state.linear_issue_id_identity("issue-1")
  let assert Ok(parked) = dict.get(loaded.runtime.parked, identity)
  assert parked.reason
    == reason.ParkOperator("abandoned_claim:permanent_failure:unauthorized")
}

pub fn load_compensates_completed_claim_without_workflow_run_test() {
  let bundle =
    write_bundle(
      "test/tmp/startup-recovery-release-claim-completed",
      "prompts/task.md",
    )
  let candidate = issue("issue-1", "ABC-1")
  let workspace_root = bundle.effective.workspace.root
  let claim_key = "claim:linear:issue-1:run-1"

  append_test_ledger_bodies(workspace_root, [
    record.OutboxCompletedWithTask(
      outbox_id: claim_key,
      task_ref: record.linear_task_ref_fields("issue-1", Some("ABC-1"), None),
      outbox_kind: "claim",
    ),
  ])

  let assert Ok(loaded) =
    startup_recovery.load(
      bundle,
      tracker_adapter([candidate]),
      startup_dependencies(),
      [],
    )

  let release_key = "release_claim:linear:issue-1:run-1"
  let assert [release_replay] = loaded.outbox_to_replay
  let recovery.OutboxReplay(
    outbox_id: release_outbox_id,
    outbox_kind: release_kind,
    dedupe_key: release_dedupe_key,
    ..,
  ) = release_replay
  assert release_outbox_id == release_key
  assert release_kind == "release_claim"
  assert release_dedupe_key == release_key
  let identity = orchestrator_state.linear_issue_id_identity("issue-1")
  let assert Ok(parked) = dict.get(loaded.runtime.parked, identity)
  assert parked.reason
    == reason.ParkOperator("abandoned_claim:stale_claim_success")
  assert loaded.park_reports == []
  assert list.contains(
    loaded.warnings,
    "abandoned_claim_compensation:issue-1:stale_claim_success",
  )
  let records = load_records(workspace_root)
  assert list.any(records, fn(entry) {
    case entry.body {
      record.OutboxPendingV2WithTask(
        outbox_id: outbox_id,
        outbox_kind: "release_claim",
        dedupe_key: dedupe_key,
        ..,
      ) -> outbox_id == release_key && dedupe_key == release_key
      _ -> False
    }
  })
  assert list.any(records, fn(entry) {
    case entry.body {
      record.IssueParkedV2(
        issue_id: "issue-1",
        reason: "abandoned_claim:stale_claim_success",
        release_policy: "explicit_unpark_only",
        ..,
      ) -> True
      _ -> False
    }
  })
}

pub fn load_does_not_compensate_completed_claim_with_legacy_run_test() {
  let bundle =
    write_bundle(
      "test/tmp/startup-recovery-release-claim-legacy-run",
      "prompts/task.md",
    )
  let candidate = issue("issue-1", "ABC-1")
  let workspace_root = bundle.effective.workspace.root
  let workspace_path = workspace_root <> "/ABC-1"
  let claim_key = "claim:linear:issue-1:run-1"

  append_test_ledger_bodies(workspace_root, [
    record.RunStarted(
      run_id: "run-1",
      issue_id: candidate.id,
      issue_identifier: candidate.identifier,
      workspace_path: workspace_path,
    ),
    record.OutboxCompletedWithTask(
      outbox_id: claim_key,
      task_ref: record.linear_task_ref_fields("issue-1", Some("ABC-1"), None),
      outbox_kind: "claim",
    ),
  ])

  let assert Ok(loaded) =
    startup_recovery.load(
      bundle,
      tracker_adapter([candidate]),
      startup_dependencies(),
      [],
    )

  assert loaded.outbox_to_replay == []
  let identity = orchestrator_state.linear_issue_id_identity("issue-1")
  let assert Ok(parked) = dict.get(loaded.runtime.parked, identity)
  assert parked.reason == reason.ParkWorkerFailure
  assert !list.contains(
    loaded.warnings,
    "abandoned_claim_compensation:issue-1:stale_claim_success",
  )
  let records = load_records(workspace_root)
  assert release_claim_record_count(
      records,
      "release_claim:linear:issue-1:run-1",
    )
    == 0
  assert abandoned_park_record_count(
      records,
      "issue-1",
      "abandoned_claim:stale_claim_success",
    )
    == 0
}

pub fn load_reuses_existing_release_claim_and_park_records_test() {
  let bundle =
    write_bundle(
      "test/tmp/startup-recovery-release-claim-idempotent",
      "prompts/task.md",
    )
  let candidate = issue("issue-1", "ABC-1")
  let workspace_root = bundle.effective.workspace.root
  let task_ref = record.linear_task_ref_fields("issue-1", Some("ABC-1"), None)
  let claim_key = "claim:linear:issue-1:run-1"
  let release_key = "release_claim:linear:issue-1:run-1"

  append_test_ledger_bodies(workspace_root, [
    record.OutboxCompletedWithTask(
      outbox_id: claim_key,
      task_ref: task_ref,
      outbox_kind: "claim",
    ),
    record.OutboxPendingV2WithTask(
      outbox_id: release_key,
      task_ref: task_ref,
      outbox_kind: "release_claim",
      dedupe_key: release_key,
      payload_json: outbox.tracker_update_payload(
        "release_claim",
        release_key,
        "already parked",
        None,
        None,
        [],
      ),
    ),
    record.IssueParkedV2(
      "issue-1",
      "ABC-1",
      "abandoned_claim:stale_claim_success",
      "explicit_unpark_only",
      "",
      6000,
    ),
  ])

  let assert Ok(loaded) =
    startup_recovery.load(
      bundle,
      tracker_adapter([candidate]),
      startup_dependencies(),
      [],
    )

  let assert [release_replay] = loaded.outbox_to_replay
  let recovery.OutboxReplay(
    outbox_id: release_outbox_id,
    outbox_kind: release_kind,
    dedupe_key: release_dedupe_key,
    ..,
  ) = release_replay
  assert release_outbox_id == release_key
  assert release_kind == "release_claim"
  assert release_dedupe_key == release_key
  let records = load_records(workspace_root)
  assert release_claim_record_count(records, release_key) == 1
  assert abandoned_park_record_count(
      records,
      "issue-1",
      "abandoned_claim:stale_claim_success",
    )
    == 1
}

pub fn load_compensates_failed_claim_outbox_test() {
  let bundle =
    write_bundle(
      "test/tmp/startup-recovery-release-claim-failed",
      "prompts/task.md",
    )
  let candidate = issue("issue-1", "ABC-1")
  let workspace_root = bundle.effective.workspace.root
  let claim_key = "claim:linear:issue-1:run-1"

  append_test_ledger_bodies(workspace_root, [
    record.OutboxFailedWithTask(
      outbox_id: claim_key,
      task_ref: record.linear_task_ref_fields("issue-1", Some("ABC-1"), None),
      outbox_kind: "claim",
      error_code: "temporary_claim_error",
    ),
  ])

  let assert Ok(loaded) =
    startup_recovery.load(
      bundle,
      tracker_adapter([candidate]),
      startup_dependencies(),
      [],
    )

  let release_key = "release_claim:linear:issue-1:run-1"
  let assert [release_replay] = loaded.outbox_to_replay
  let recovery.OutboxReplay(
    outbox_id: release_outbox_id,
    outbox_kind: release_kind,
    dedupe_key: release_dedupe_key,
    ..,
  ) = release_replay
  assert release_outbox_id == release_key
  assert release_kind == "release_claim"
  assert release_dedupe_key == release_key
  let identity = orchestrator_state.linear_issue_id_identity("issue-1")
  let assert Ok(parked) = dict.get(loaded.runtime.parked, identity)
  assert parked.reason
    == reason.ParkOperator(
      "abandoned_claim:operator_action_required:temporary_claim_error",
    )
  assert loaded.park_reports == []
  assert list.contains(
    loaded.warnings,
    "abandoned_claim_compensation:issue-1:operator_action_required:temporary_claim_error",
  )
  let records = load_records(workspace_root)
  assert list.any(records, fn(entry) {
    case entry.body {
      record.OutboxPendingV2WithTask(
        outbox_id: outbox_id,
        outbox_kind: "release_claim",
        dedupe_key: dedupe_key,
        ..,
      ) -> outbox_id == release_key && dedupe_key == release_key
      _ -> False
    }
  })
  assert list.any(records, fn(entry) {
    case entry.body {
      record.IssueParkedV2(
        issue_id: "issue-1",
        reason: "abandoned_claim:operator_action_required:temporary_claim_error",
        release_policy: "explicit_unpark_only",
        ..,
      ) -> True
      _ -> False
    }
  })
}

pub fn load_compensates_recovered_claim_outbox_cancels_existing_retry_test() {
  let bundle =
    write_bundle(
      "test/tmp/startup-recovery-release-claim-retry",
      "prompts/task.md",
    )
  let candidate = issue("issue-1", "ABC-1")
  let workspace_root = bundle.effective.workspace.root
  let claim_key = "claim:linear:issue-1:run-1"

  append_test_ledger_bodies(workspace_root, [
    record.OutboxPendingV2WithTask(
      outbox_id: claim_key,
      task_ref: record.linear_task_ref_fields("issue-1", Some("ABC-1"), None),
      outbox_kind: "claim",
      dedupe_key: claim_key,
      payload_json: outbox.tracker_update_payload(
        "claim",
        claim_key,
        "claimed",
        None,
        None,
        [],
      ),
    ),
    record.RetryScheduled(
      issue_id: candidate.id,
      issue_identifier: candidate.identifier,
      delay_ms: 2500,
      generation: 3,
      reason: "claim_start_ledger_append_failed",
    ),
  ])

  let assert Ok(loaded) =
    startup_recovery.load(
      bundle,
      tracker_adapter([candidate]),
      startup_dependencies(),
      [],
    )

  assert loaded.retry_timers == []
  let task_identity = orchestrator_state.linear_issue_id_identity("issue-1")
  assert !dict.has_key(loaded.runtime.retry_attempts, task_identity)
  assert !dict.has_key(loaded.runtime.claimed, task_identity)
  let assert Ok(parked) = dict.get(loaded.runtime.parked, task_identity)
  assert parked.reason
    == reason.ParkOperator("abandoned_claim:startup_recovered_claim")
  let records = load_records(workspace_root)
  assert list.any(records, fn(entry) {
    case entry.body {
      record.RetryCancelled(
        issue_id: "issue-1",
        generation: 3,
        reason: "recovery_parked",
      ) -> True
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

pub fn load_degrades_when_workflow_refresh_stays_unavailable_test() {
  let bundle =
    write_bundle(
      "test/tmp/startup-recovery-refresh-unavailable",
      "prompts/task.md",
    )
  let original_issue = issue("issue-1", "ABC-1")
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

  let refresh_results = process.new_subject()
  process.send(refresh_results, Error(adapter.Transient("tracker down")))
  process.send(refresh_results, Error(adapter.Transient("tracker down")))
  process.send(refresh_results, Error(adapter.Transient("tracker down")))
  let FakeRefreshAdapter(tracker_adapter, refresh_calls) =
    fake_refresh_adapter(refresh_results)
  let sleeps = process.new_subject()

  let assert Ok(loaded) =
    startup_recovery.load(
      bundle,
      tracker_adapter,
      startup_dependencies_with_sleep(sleeps),
      [],
    )

  assert loaded.workflow_resumptions == []
  assert test_async.drain_subject(refresh_calls)
    == [["issue-1"], ["issue-1"], ["issue-1"]]
  assert test_async.drain_subject(sleeps) == [50, 200]
  let assert [adapter.ParkReport(reason: reason_text, run_id: Some(run_id), ..)] =
    loaded.park_reports
  assert reason_text == "tracker_refresh_unavailable"
  assert run_id == "run-1"
  assert list.contains(
    loaded.warnings,
    "tracker_refresh_unavailable:linear:tracker down",
  )
  assert list.contains(
    loaded.warnings,
    "workflow_recovery_parked_tracker_refresh_unavailable:run-1",
  )
}

pub fn load_preserves_successful_refresh_chunk_when_later_chunk_fails_test() {
  let bundle =
    write_bundle("test/tmp/startup-recovery-partial-refresh", "prompts/task.md")
  let workspace_root = bundle.effective.workspace.root
  append_test_ledger_bodies(
    workspace_root,
    many_retry_scheduled_records(51, 1, []),
  )

  let refresh_results = process.new_subject()
  process.send(refresh_results, Ok(many_issues(51, [])))
  process.send(refresh_results, Error(adapter.Transient("tracker down")))
  process.send(refresh_results, Error(adapter.Transient("tracker down")))
  process.send(refresh_results, Error(adapter.Transient("tracker down")))
  let FakeRefreshAdapter(tracker_adapter, refresh_calls) =
    fake_refresh_adapter(refresh_results)
  let sleeps = process.new_subject()

  let assert Ok(loaded) =
    startup_recovery.load(
      bundle,
      tracker_adapter,
      startup_dependencies_with_sleep(sleeps),
      [],
    )

  assert list.length(loaded.retry_timers) == 50
  assert loaded.workflow_resumptions == []
  let assert [first_chunk, second_try, third_try, fourth_try] =
    test_async.drain_subject(refresh_calls)
  assert list.length(first_chunk) == 50
  assert list.length(second_try) == 1
  assert second_try == third_try
  assert third_try == fourth_try
  assert test_async.drain_subject(sleeps) == [50, 200]
  assert list.contains(
    loaded.warnings,
    "tracker_refresh_unavailable:linear:tracker down",
  )
}

pub fn load_global_refresh_auth_failure_skips_retries_and_remaining_chunks_test() {
  let bundle =
    write_bundle("test/tmp/startup-recovery-global-refresh", "prompts/task.md")
  let workspace_root = bundle.effective.workspace.root
  append_test_ledger_bodies(
    workspace_root,
    many_retry_scheduled_records(51, 1, []),
  )

  let refresh_results = process.new_subject()
  process.send(refresh_results, Error(adapter.Unauthorized("auth failed")))
  process.send(refresh_results, Error(adapter.Unauthorized("auth failed")))
  process.send(refresh_results, Error(adapter.Unauthorized("auth failed")))
  process.send(refresh_results, Error(adapter.Unauthorized("auth failed")))
  process.send(refresh_results, Error(adapter.Unauthorized("auth failed")))
  process.send(refresh_results, Error(adapter.Unauthorized("auth failed")))
  let FakeRefreshAdapter(tracker_adapter, refresh_calls) =
    fake_refresh_adapter(refresh_results)
  let sleeps = process.new_subject()

  let assert Ok(loaded) =
    startup_recovery.load(
      bundle,
      tracker_adapter,
      startup_dependencies_with_sleep(sleeps),
      [],
    )

  assert loaded.retry_timers == []
  let assert [first_chunk] = test_async.drain_subject(refresh_calls)
  assert list.length(first_chunk) == 50
  assert test_async.drain_subject(sleeps) == []
  assert list.contains(
    loaded.warnings,
    "tracker_refresh_unavailable:linear:auth failed",
  )
}

pub fn load_degraded_boot_is_idempotent_for_unavailable_refresh_test() {
  let bundle =
    write_bundle(
      "test/tmp/startup-recovery-degraded-idempotent",
      "prompts/task.md",
    )
  let original_issue = issue("issue-1", "ABC-1")
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

  let refresh_results = process.new_subject()
  process.send(refresh_results, Error(adapter.Transient("tracker down")))
  process.send(refresh_results, Error(adapter.Transient("tracker down")))
  process.send(refresh_results, Error(adapter.Transient("tracker down")))
  process.send(refresh_results, Error(adapter.Transient("tracker down")))
  process.send(refresh_results, Error(adapter.Transient("tracker down")))
  process.send(refresh_results, Error(adapter.Transient("tracker down")))
  let FakeRefreshAdapter(tracker_adapter, _) =
    fake_refresh_adapter(refresh_results)

  let assert Ok(_) =
    startup_recovery.load(bundle, tracker_adapter, startup_dependencies(), [])
  let assert Ok(_) =
    startup_recovery.load(bundle, tracker_adapter, startup_dependencies(), [])

  assert count_record_kind(load_records(workspace_root), "issue_parked_v2") == 1
  assert count_record_kind(
      load_records(workspace_root),
      "workflow_run_interrupted",
    )
    == 1
}

pub fn load_recovers_active_scheduled_run_from_replayed_projection_test() {
  let bundle =
    scheduled_bundle("test/tmp/startup-recovery-active-scheduled-run", [
      scheduled_entry("scheduled-job", True),
    ])
  let workspace_root = bundle.effective.workspace.root
  append_test_ledger_bodies(workspace_root, [
    record.ScheduledJobDue(
      "scheduled-job",
      "implementation",
      5000,
      "run-active",
      "timer",
    ),
    record.ScheduledRunPending(
      "scheduled-job",
      "implementation",
      5000,
      "run-active",
      "timer",
      5000,
    ),
    record.ScheduledRunStarted(
      "scheduled-job",
      "implementation",
      5000,
      5100,
      "run-active",
      2,
      "session-run-active",
      "/tmp/run-active",
    ),
  ])

  let assert Ok(loaded) =
    startup_recovery.load(
      bundle,
      tracker_adapter([]),
      startup_dependencies(),
      [],
    )

  assert scheduled_runtime.pending_starts(loaded.scheduled.runtime) == []
  assert loaded.scheduled.effects
    == [
      startup_recovery.AppendLedger(
        record_bodies: [
          record.ScheduledRunFailed(
            "scheduled-job",
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
      startup_recovery.BeginFailureReport(
        scheduled_runtime.FailureReportRequest(
          job_id: "scheduled-job",
          workflow_id: "implementation",
          due_at_ms: 5000,
          run_id: "run-active",
          attempt: 2,
          reason: "daemon_restart",
          run_root: Some("/tmp/run-active"),
          session_id: Some("session-run-active"),
        ),
      ),
    ]
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

pub fn recover_scheduled_runtime_suppresses_report_retry_timer_when_outbox_replays_test() {
  let bundle =
    scheduled_bundle("test/tmp/startup-recovery-scheduled-report-outbox-owner", [
      scheduled_entry("scheduled-job", True),
    ])
  let report_retry =
    projection.ScheduledReportRetry(
      run_id: "run-report",
      attempt: 3,
      dedupe_key: "scheduled-job:scheduled-job",
      error_code: "linear_failed",
      error_message: "temporary failure",
      next_retry_at_ms: 6500,
      generation: 4,
    )
  let payload_json = scheduled_failure_payload("scheduled-job", "run-report", 4)

  let recovery =
    startup_recovery.recover_scheduled_runtime_with_outbox_replay(
      bundle,
      7000,
      [
        scheduled_status(
          "scheduled-job",
          projection.ScheduledReportRetryWaiting,
          scheduled_run("run-report", 3),
          Some(report_retry),
        ),
      ],
      [
        recovery.OutboxReplay(
          "scheduled-job:scheduled-job",
          scheduled_failure_task_ref("scheduled-job"),
          outbox.scheduled_failure_publication_kind,
          "scheduled-job:scheduled-job",
          payload_json,
        ),
      ],
    )

  assert scheduled_runtime.report_retry_tick_matches(
    recovery.runtime,
    "run-report",
    4,
  )
  assert recovery.effects == []
}

pub fn recover_scheduled_runtime_reports_enabled_retry_waiting_as_failed_test() {
  let bundle =
    scheduled_bundle("test/tmp/startup-recovery-enabled-scheduled-retry", [
      scheduled_entry("scheduled-job", True),
    ])
  let run = scheduled_run("run-retry", 0)

  let recovery =
    startup_recovery.recover_scheduled_runtime(bundle, 7000, [
      scheduled_status(
        "scheduled-job",
        projection.ScheduledRetryWaiting,
        run,
        None,
      ),
    ])

  assert scheduled_runtime.pending_starts(recovery.runtime) == []
  assert scheduled_runtime.retry_run_ids(recovery.runtime) == []
  assert recovery.effects
    == [
      startup_recovery.AppendLedger(
        record_bodies: [
          record.ScheduledRunFailed(
            "scheduled-job",
            "implementation",
            5000,
            "run-retry",
            1,
            7000,
            "whole_run_retry_removed",
            True,
            Some("/tmp/run-retry"),
          ),
        ],
        failure_event: "scheduled_recovery_append_failed",
      ),
      startup_recovery.BeginFailureReport(
        scheduled_runtime.FailureReportRequest(
          job_id: "scheduled-job",
          workflow_id: "implementation",
          due_at_ms: 5000,
          run_id: "run-retry",
          attempt: 1,
          reason: "whole_run_retry_removed",
          run_root: Some("/tmp/run-retry"),
          session_id: Some("session-run-retry"),
        ),
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
