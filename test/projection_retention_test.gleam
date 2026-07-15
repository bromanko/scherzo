import gleam/bit_array
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import scherzo/config/types as config_types
import scherzo/state/projection
import scherzo/state/projection/retention
import scherzo/state/record

const now_ms = 1_000_000_000

pub fn issue_run_is_retained_at_grace_and_selected_one_millisecond_later_test() {
  let policy = policy(1000, 10_000, 25)
  let at_boundary = issue_projection("at-boundary", now_ms - 1000)
  let after_boundary = issue_projection("after-boundary", now_ms - 1001)

  let boundary = retention.preview(at_boundary, policy, now_ms, no_marker)
  let eligible = retention.preview(after_boundary, policy, now_ms, no_marker)

  assert boundary.candidate_run_ids == []
  assert boundary.blockers.within_grace == 1
  assert eligible.candidate_run_ids == ["after-boundary"]
}

pub fn active_runs_are_held_and_every_terminal_status_is_eligible_test() {
  let active_source = issue_projection("active", now_ms - 2001)
  let active =
    projection.Projection(
      ..active_source,
      workflow_runs: dict.insert(
        active_source.workflow_runs,
        "active",
        projection.WorkflowRunActive(
          workflow_id: "workflow",
          workflow_fingerprint: "fingerprint",
          issue_id: "issue-active",
          issue_identifier: "LIV-active",
          issue_fingerprint: "issue-fingerprint",
          observed_updated_at_ms: now_ms - 3000,
          run_root: "test/tmp/run-active",
          started_at_ms: now_ms - 2500,
        ),
      ),
    )
  let interrupted_source = issue_projection("interrupted", now_ms - 2001)
  let interrupted =
    projection.Projection(
      ..interrupted_source,
      workflow_runs: dict.insert(
        interrupted_source.workflow_runs,
        "interrupted",
        projection.WorkflowRunInterrupted(
          "workflow",
          "issue-interrupted",
          "daemon_exit",
          now_ms - 2001,
          "test/tmp/run-interrupted",
        ),
      ),
    )
  let superseded_source = issue_projection("superseded", now_ms - 2001)
  let superseded =
    projection.Projection(
      ..superseded_source,
      workflow_runs: dict.insert(
        superseded_source.workflow_runs,
        "superseded",
        projection.WorkflowRunSuperseded(
          "workflow",
          "issue-superseded",
          "replacement",
          "newer observation",
          now_ms - 2001,
          "test/tmp/run-superseded",
        ),
      ),
    )

  assert preview(active).candidate_run_ids == []
  assert preview(active).blockers.active == 1
  assert preview(issue_projection("finished", now_ms - 2001)).candidate_run_ids
    == ["finished"]
  assert preview(interrupted).candidate_run_ids == ["interrupted"]
  assert preview(superseded).candidate_run_ids == ["superseded"]
}

pub fn scheduled_selection_uses_exact_age_and_newest_count_boundaries_test() {
  let source =
    integers(1, 27)
    |> list.fold(projection.new(), fn(state, rank) {
      scheduled_projection(
        state,
        "run-" <> int.to_string(rank),
        now_ms - 1000 - rank,
      )
    })
  let report =
    retention.preview(source, policy(999, 100_000, 25), now_ms, no_marker)

  assert list.contains(report.candidate_run_ids, "run-26")
  assert list.contains(report.candidate_run_ids, "run-27")
  assert !list.contains(report.candidate_run_ids, "run-25")

  let at_max_age =
    scheduled_projection(projection.new(), "at-max-age", now_ms - 10_000)
  let older = scheduled_projection(projection.new(), "older", now_ms - 10_001)
  let age_policy = policy(999, 10_000, 25)
  assert retention.preview(at_max_age, age_policy, now_ms, no_marker).candidate_run_ids
    == []
  assert retention.preview(older, age_policy, now_ms, no_marker).candidate_run_ids
    == ["older"]
}

pub fn active_parked_and_workspace_marker_states_fail_closed_test() {
  let terminal = issue_projection("held", now_ms - 2000)
  let parked =
    projection.Projection(
      ..terminal,
      parked_issues: dict.insert(
        terminal.parked_issues,
        "issue-held",
        projection.ParkedIssue(
          "LIV-held",
          "operator",
          1,
          1,
          "explicit",
          "fingerprint",
        ),
      ),
    )
  let parked_report = preview(parked)
  let present_report =
    retention.preview(terminal, test_policy(), now_ms, fn(_) {
      retention.MarkerPresent
    })
  let unreadable_report =
    retention.preview(terminal, test_policy(), now_ms, fn(_) {
      retention.MarkerUnreadable
    })

  assert parked_report.candidate_run_ids == []
  assert parked_report.blockers.parked == 1
  assert present_report.candidate_run_ids == []
  assert present_report.blockers.retained_workspace == 1
  assert unreadable_report.candidate_run_ids == []
  assert unreadable_report.blockers.marker_unavailable == 1
  assert preview(terminal).candidate_run_ids == ["held"]
}

pub fn started_recovery_is_an_isolated_hold_and_finished_recovery_is_not_test() {
  let source = issue_projection("held", now_ms - 2000)
  let started =
    projection.StepRecoveryStartedStatus(
      run_id: "held",
      workflow_id: "workflow",
      step_id: "implement",
      failed_attempt_index: 1,
      recovery_attempt_number: 1,
      recovery_session_id: "recovery-1",
      model: None,
      prompt_ref: "prompt.md",
      started_at_ms: 1,
    )
  let finished =
    projection.StepRecoveryFinishedStatus(
      run_id: "held",
      workflow_id: "workflow",
      step_id: "implement",
      failed_attempt_index: 1,
      recovery_attempt_number: 1,
      recovery_session_id: "recovery-1",
      model: None,
      prompt_ref: "prompt.md",
      result: "recheck",
      summary: "repaired",
      reason: "done",
      retry_attempt_index: Some(2),
      started_at_ms: 1,
      finished_at_ms: 2,
    )
  let held = with_recovery(source, started)
  let released = with_recovery(source, finished)

  assert preview(held).candidate_run_ids == []
  assert preview(held).blockers.recovery_started == 1
  assert preview(released).candidate_run_ids == ["held"]
  assert preview(source).candidate_run_ids == ["held"]
}

pub fn queued_and_running_controls_are_isolated_holds_test() {
  let source = issue_projection("held", now_ms - 2000)
  assert_control_hold(source, "queued")
  assert_control_hold(source, "running")

  let settled = with_control(source, "finished")
  assert preview(settled).candidate_run_ids == ["held"]
  assert preview(source).candidate_run_ids == ["held"]
}

pub fn latest_planned_and_failed_publications_are_isolated_holds_test() {
  let source = issue_projection("held", now_ms - 2000)
  assert_publication_hold(source, "planned")
  assert_publication_hold(source, "failed")

  let succeeded = with_latest_publication(source, "succeeded")
  assert preview(succeeded).candidate_run_ids == ["held"]
  assert preview(source).candidate_run_ids == ["held"]
}

pub fn every_unsettled_outbox_variant_holds_the_associated_run_test() {
  let issue_id = "issue-held"
  let task = task_ref(issue_id, "LIV-held")
  let payload = "{\"run_id\":\"held\"}"
  let statuses = [
    projection.OutboxPending(issue_id, "kind", "dedupe", 1),
    projection.OutboxPendingV2(issue_id, "kind", "dedupe", payload, 1),
    projection.OutboxPendingV2WithTask(task, "kind", "dedupe", payload, 1),
    projection.OutboxAttempted(issue_id, "kind", "dedupe", payload, 1, 2),
    projection.OutboxAttemptedWithTask(task, "kind", "dedupe", payload, 1, 2),
    projection.OutboxRetryScheduled(
      issue_id,
      "kind",
      "dedupe",
      payload,
      "retry",
      1,
      3,
      2,
    ),
    projection.OutboxRetryScheduledWithTask(
      task,
      "kind",
      "dedupe",
      payload,
      "retry",
      1,
      3,
      2,
    ),
  ]

  statuses
  |> list.index_map(fn(status, index) { #(index, status) })
  |> list.each(fn(entry) {
    let #(index, status) = entry
    let source = issue_projection("held", now_ms - 2000)
    let held = with_outbox(source, "outbox-" <> int.to_string(index), status)
    let report = preview(held)
    assert report.candidate_run_ids == []
    assert report.blockers.outbox_unsettled == 1
    assert preview(source).candidate_run_ids == ["held"]
  })
}

pub fn scheduled_outbox_requires_payload_or_exact_run_key_association_test() {
  let source =
    scheduled_projection(projection.new(), "scheduled-target", now_ms - 20_000)
  let status = projection.OutboxPending("", "scheduled_report", "dedupe", 1)
  let unrelated = with_outbox(source, "scheduled-target-suffix", status)
  let exact = with_outbox(source, "scheduled-report:scheduled-target", status)
  let payload =
    with_outbox(
      source,
      "scheduled-report",
      projection.OutboxPendingV2(
        "",
        "scheduled_report",
        "dedupe",
        "{\"run_id\":\"scheduled-target\"}",
        1,
      ),
    )

  assert preview(unrelated).candidate_run_ids == ["scheduled-target"]
  assert preview(exact).candidate_run_ids == []
  assert preview(exact).blockers.outbox_unsettled == 1
  assert preview(payload).candidate_run_ids == []
  assert preview(payload).blockers.outbox_unsettled == 1
}

pub fn malformed_associated_outbox_payload_fails_closed_test() {
  let source = issue_projection("held", now_ms - 2000)
  let malformed =
    projection.OutboxPendingV2WithTask(
      task_ref("issue-held", "LIV-held"),
      "kind",
      "dedupe",
      "not-json",
      1,
    )
  let report = preview(with_outbox(source, "outbox", malformed))

  assert report.candidate_run_ids == []
  assert report.blockers.outbox_unsettled == 1
  assert preview(source).candidate_run_ids == ["held"]
}

pub fn settled_outbox_variants_do_not_hold_a_run_test() {
  let issue_id = "issue-held"
  let task = task_ref(issue_id, "LIV-held")
  let statuses = [
    projection.OutboxCompleted(issue_id, "kind", 1),
    projection.OutboxCompletedWithTask(task, "kind", 1),
    projection.OutboxFailed(issue_id, "kind", "failed", 1),
    projection.OutboxFailedWithTask(task, "kind", "failed", 1),
    projection.OutboxPermanentlyFailed(issue_id, "kind", "failed", 1, 1),
    projection.OutboxPermanentlyFailedWithTask(task, "kind", "failed", 1, 1),
  ]

  list.each(statuses, fn(status) {
    let source = issue_projection("held", now_ms - 2000)
    assert preview(with_outbox(source, "settled:held", status)).candidate_run_ids
      == ["held"]
  })
}

pub fn missing_or_inconsistent_run_provenance_fails_closed_test() {
  let source = issue_projection("held", now_ms - 2000)
  let missing =
    projection.Projection(
      ..source,
      workflow_run_provenances: dict.delete(
        source.workflow_run_provenances,
        "held",
      ),
    )
  let inconsistent =
    projection.Projection(
      ..source,
      workflow_run_provenances: dict.insert(
        source.workflow_run_provenances,
        "held",
        projection.WorkflowRunProvenance(
          "workflow",
          "fingerprint",
          "another-issue",
          "LIV-held",
          "issue-fingerprint",
          1,
          "test/tmp/run-held",
          task_ref("another-issue", "LIV-held"),
        ),
      ),
    )

  assert preview(missing).candidate_run_ids == []
  assert preview(missing).blockers.malformed_association == 1
  assert preview(inconsistent).candidate_run_ids == []
  assert preview(inconsistent).blockers.malformed_association == 1
}

pub fn two_job_fifteen_minute_plateau_has_exact_counts_and_byte_bound_test() {
  let day_ms = 86_400_000
  let first_now = 20 * day_ms
  let initial =
    integers(0, 9)
    |> list.fold(projection.new(), fn(state, day) {
      add_plateau_day(state, day, first_now - 10 * day_ms + day * day_ms)
    })
    |> add_plateau_exceptions(first_now)
  let policy = policy(day_ms, 7 * day_ms, 25)
  let first = retention.prune(initial, policy, first_now, plateau_marker)
  assert_plateau_projection(first.projection, first_now, 121, 3)
  let first_bytes = projection_byte_size(first.projection)

  let with_eleventh_day = add_plateau_day(first.projection, 10, first_now)
  let second =
    retention.prune(
      with_eleventh_day,
      policy,
      first_now + day_ms,
      plateau_marker,
    )
  assert_plateau_projection(second.projection, first_now + day_ms, 121, 3)
  let second_bytes = projection_byte_size(second.projection)
  let allowed_growth = int.max(4096, { first_bytes + 99 } / 100)
  assert second_bytes <= first_bytes + allowed_growth
}

fn add_plateau_day(
  source: projection.Projection,
  day: Int,
  day_start_ms: Int,
) -> projection.Projection {
  ["job-a", "job-b"]
  |> list.fold(source, fn(state, job_id) {
    integers(0, 95)
    |> list.fold(state, fn(state, slot) {
      let run_id =
        job_id
        <> "-day-"
        <> int.to_string(day)
        <> "-slot-"
        <> int.to_string(slot)
      let terminal_at_ms = day_start_ms + slot * 900_000
      state
      |> scheduled_projection_for_job(run_id, job_id, terminal_at_ms)
      |> add_plateau_run_families(run_id, "", job_id, terminal_at_ms, 3)
    })
  })
}

fn add_plateau_exceptions(
  source: projection.Projection,
  first_now: Int,
) -> projection.Projection {
  let day_ms = 86_400_000
  let active_source =
    source
    |> issue_projection_in("plateau-active", first_now - 9 * day_ms)
    |> add_plateau_run_families(
      "plateau-active",
      "issue-plateau-active",
      "LIV-plateau-active",
      first_now - 9 * day_ms,
      3,
    )
  let active =
    projection.Projection(
      ..active_source,
      workflow_runs: dict.insert(
        active_source.workflow_runs,
        "plateau-active",
        projection.WorkflowRunActive(
          "workflow",
          "fingerprint",
          "issue-plateau-active",
          "LIV-plateau-active",
          "issue-fingerprint",
          first_now - 9 * day_ms,
          "test/tmp/run-plateau-active",
          first_now - 9 * day_ms,
        ),
      ),
    )

  ["job-a", "job-b"]
  |> list.fold(active, fn(state, job_id) {
    let run_id = job_id <> "-held"
    let terminal_at_ms = first_now - 8 * day_ms
    state
    |> scheduled_projection_for_job(run_id, job_id, terminal_at_ms)
    |> add_plateau_run_families(run_id, "", job_id, terminal_at_ms, 3)
  })
}

fn add_plateau_run_families(
  source: projection.Projection,
  run_id: String,
  issue_id: String,
  issue_identifier: String,
  recorded_at_ms: Int,
  attempts_per_run: Int,
) -> projection.Projection {
  let with_attempts =
    integers(1, attempts_per_run)
    |> list.fold(source, fn(state, attempt) {
      projection.apply(
        state,
        record.with_id(
          run_id <> "-attempt-" <> int.to_string(attempt),
          recorded_at_ms,
          record.StepAttemptFinished(
            run_id,
            "workflow",
            "step",
            attempt,
            "success",
            "artifact",
            "sha",
            "main",
            "test/tmp",
            1,
            1,
          ),
        ),
      )
    })
  let manifest =
    projection.WorkflowContractManifestRef(
      "workflow",
      "fingerprint",
      "artifact/" <> run_id,
      "sha",
      1,
      recorded_at_ms,
    )
  let interface =
    projection.WorkflowInterfaceSnapshotRef(
      "workflow",
      "fingerprint",
      "interface/" <> run_id,
      "sha",
      1,
      recorded_at_ms,
    )
  let publication =
    projection.PublicationAttempt(
      run_id,
      "workflow",
      "review",
      "series-" <> run_id,
      "attempt-" <> run_id,
      "succeeded",
      True,
      False,
      False,
      None,
      None,
      None,
      None,
      None,
      None,
      recorded_at_ms,
    )
  let recovery =
    projection.StepRecoveryFinishedStatus(
      run_id,
      "workflow",
      "step",
      1,
      1,
      "recovery-" <> run_id,
      None,
      "prompt.md",
      "recheck",
      "repaired",
      "complete",
      Some(2),
      recorded_at_ms,
      recorded_at_ms + 1,
    )
  projection.Projection(
    ..with_attempts,
    workflow_task_refs: dict.insert(
      with_attempts.workflow_task_refs,
      run_id,
      task_ref(issue_id, issue_identifier),
    ),
    workflow_input_manifests: dict.insert(
      with_attempts.workflow_input_manifests,
      run_id,
      manifest,
    ),
    workflow_interface_snapshots: dict.insert(
      with_attempts.workflow_interface_snapshots,
      run_id,
      interface,
    ),
    workflow_output_manifests: dict.insert(
      with_attempts.workflow_output_manifests,
      run_id,
      manifest,
    ),
    workflow_repairs: dict.insert(
      with_attempts.workflow_repairs,
      run_id,
      projection.WorkflowRepairStatus(
        "workflow",
        issue_id,
        issue_identifier,
        run_id,
        Some("step"),
        "step",
        1,
        2,
        "retry-step",
        recorded_at_ms,
        1,
      ),
    ),
    step_recoveries: dict.insert(
      with_attempts.step_recoveries,
      "recovery-" <> run_id,
      recovery,
    ),
    publication_attempts: dict.insert(
      with_attempts.publication_attempts,
      run_id <> ":review",
      [publication],
    ),
    publication_latest_by_series: dict.insert(
      with_attempts.publication_latest_by_series,
      "series-" <> run_id,
      publication,
    ),
    control_operations: dict.insert(
      with_attempts.control_operations,
      "control-" <> run_id,
      projection.ControlOperationStatus(
        "control-" <> run_id,
        "retry_step",
        "retry-step",
        "run:" <> run_id,
        Some(run_id),
        Some(issue_id),
        Some(issue_identifier),
        Some("step"),
        None,
        "finished",
        None,
        None,
        recorded_at_ms,
        Some(recorded_at_ms),
        Some(recorded_at_ms + 1),
      ),
    ),
    outbox: dict.insert(
      with_attempts.outbox,
      "settled:" <> run_id,
      projection.OutboxCompleted(issue_id, "report", recorded_at_ms),
    ),
  )
}

fn assert_plateau_projection(
  source: projection.Projection,
  now: Int,
  expected_per_job: Int,
  attempts_per_run: Int,
) -> Nil {
  ["job-a", "job-b"]
  |> list.each(fn(job_id) {
    let ordinary_run_ids =
      source.workflow_run_provenances
      |> dict.to_list
      |> list.filter_map(fn(entry) {
        case
          entry.1.issue_id == ""
          && entry.1.issue_identifier == job_id
          && !string.ends_with(entry.0, "-held")
        {
          True -> Ok(entry.0)
          False -> Error(Nil)
        }
      })
    assert list.length(ordinary_run_ids) == expected_per_job
    ordinary_run_ids
    |> list.each(fn(run_id) {
      let assert Ok(projection.WorkflowRunFinished(finished_at_ms: at, ..)) =
        dict.get(source.workflow_runs, run_id)
      assert now - at <= 7 * 86_400_000
    })
    let assert Ok(projection.WorkflowRunFinished(finished_at_ms: held_at, ..)) =
      dict.get(source.workflow_runs, job_id <> "-held")
    assert now - held_at > 7 * 86_400_000
  })
  let assert Ok(projection.WorkflowRunActive(..)) =
    dict.get(source.workflow_runs, "plateau-active")

  let retained_runs = expected_per_job * 2 + 3
  assert dict.size(source.workflow_runs) == retained_runs
  assert dict.size(source.workflow_run_provenances) == retained_runs
  assert dict.size(source.workflow_task_refs) == retained_runs
  assert dict.size(source.workflow_input_manifests) == retained_runs
  assert dict.size(source.workflow_interface_snapshots) == retained_runs
  assert dict.size(source.workflow_output_manifests) == retained_runs
  assert dict.size(source.workflow_repairs) == retained_runs
  assert dict.size(source.step_attempts) == retained_runs * attempts_per_run
  assert dict.size(source.step_recoveries) == retained_runs
  assert dict.size(source.publication_attempts) == retained_runs
  assert dict.size(source.publication_latest_by_series) == retained_runs
  assert dict.size(source.control_operations) == retained_runs
  assert dict.size(source.outbox) == retained_runs
  source.scheduled_jobs
  |> dict.values
  |> list.each(fn(status) {
    assert list.length(status.recent_run_ids) <= 25
  })
}

fn plateau_marker(run_root: String) -> retention.MarkerState {
  case string.contains(run_root, "-held") {
    True -> retention.MarkerPresent
    False -> retention.MarkerAbsent
  }
}

fn projection_byte_size(source: projection.Projection) -> Int {
  source
  |> projection.to_string
  |> bit_array.from_string
  |> bit_array.byte_size
}

pub fn prune_is_idempotent_and_reports_removed_bytes_test() {
  let source = issue_projection("old", now_ms - 2000)
  let first = retention.prune(source, test_policy(), now_ms, no_marker)
  let second =
    retention.prune(first.projection, test_policy(), now_ms, no_marker)

  assert first.report.pruned_run_ids == ["old"]
  assert first.report.families_removed.workflow_runs == 1
  assert first.report.after_bytes < first.report.before_bytes
  assert second.report.pruned_run_ids == []
  assert second.report.before_bytes == second.report.after_bytes
}

fn assert_control_hold(source: projection.Projection, status: String) {
  let report = preview(with_control(source, status))
  assert report.candidate_run_ids == []
  assert report.blockers.control_in_flight == 1
}

fn assert_publication_hold(source: projection.Projection, status: String) {
  let report = preview(with_latest_publication(source, status))
  assert report.candidate_run_ids == []
  assert report.blockers.publication_unsettled == 1
}

fn preview(source: projection.Projection) -> retention.PruneReport {
  retention.preview(source, test_policy(), now_ms, no_marker)
}

fn with_recovery(
  source: projection.Projection,
  status: projection.StepRecoveryStatus,
) -> projection.Projection {
  projection.Projection(
    ..source,
    step_recoveries: dict.insert(source.step_recoveries, "recovery", status),
  )
}

fn with_control(
  source: projection.Projection,
  status: String,
) -> projection.Projection {
  projection.Projection(
    ..source,
    control_operations: dict.insert(
      source.control_operations,
      "operation",
      projection.ControlOperationStatus(
        operation_id: "operation",
        operation_kind: "retry_step",
        command_name: "retry-step",
        target: "run:held",
        run_id: Some("held"),
        issue_id: Some("issue-held"),
        issue_identifier: Some("LIV-held"),
        requested_step_id: Some("implement"),
        publication_id: None,
        status: status,
        reason: None,
        message: None,
        queued_at_ms: 1,
        started_at_ms: None,
        finished_at_ms: None,
      ),
    ),
  )
}

fn with_latest_publication(
  source: projection.Projection,
  status: String,
) -> projection.Projection {
  let attempt =
    projection.PublicationAttempt(
      run_id: "held",
      workflow_id: "workflow",
      publication_id: "review",
      series_id: "series",
      attempt_id: "attempt",
      status: status,
      required: True,
      retryable: True,
      retry_execution_available: True,
      version_id: None,
      manifest_ref: None,
      manifest_sha256: None,
      manifest_bytes: None,
      error_code: None,
      error_message: None,
      recorded_at_ms: 1,
    )
  projection.Projection(
    ..source,
    publication_attempts: dict.insert(
      source.publication_attempts,
      "held:review",
      [attempt],
    ),
    publication_latest_by_series: dict.insert(
      source.publication_latest_by_series,
      "series",
      attempt,
    ),
  )
}

fn with_outbox(
  source: projection.Projection,
  outbox_id: String,
  status: projection.OutboxStatus,
) -> projection.Projection {
  projection.Projection(
    ..source,
    outbox: dict.insert(source.outbox, outbox_id, status),
  )
}

fn issue_projection(
  run_id: String,
  terminal_at_ms: Int,
) -> projection.Projection {
  issue_projection_in(projection.new(), run_id, terminal_at_ms)
}

fn issue_projection_in(
  base: projection.Projection,
  run_id: String,
  terminal_at_ms: Int,
) -> projection.Projection {
  projection.Projection(
    ..base,
    workflow_runs: dict.insert(
      base.workflow_runs,
      run_id,
      projection.WorkflowRunFinished(
        "workflow",
        "issue-" <> run_id,
        "success",
        1,
        1,
        terminal_at_ms,
        "test/tmp/run-" <> run_id,
      ),
    ),
    workflow_run_provenances: dict.insert(
      base.workflow_run_provenances,
      run_id,
      projection.WorkflowRunProvenance(
        "workflow",
        "fingerprint",
        "issue-" <> run_id,
        "LIV-" <> run_id,
        "issue-fingerprint",
        terminal_at_ms - 1,
        "test/tmp/run-" <> run_id,
        task_ref("issue-" <> run_id, "LIV-" <> run_id),
      ),
    ),
  )
}

fn scheduled_projection(
  base: projection.Projection,
  run_id: String,
  terminal_at_ms: Int,
) -> projection.Projection {
  scheduled_projection_for_job(base, run_id, "job-1", terminal_at_ms)
}

fn scheduled_projection_for_job(
  base: projection.Projection,
  run_id: String,
  job_id: String,
  terminal_at_ms: Int,
) -> projection.Projection {
  let scheduled =
    projection.apply(
      base,
      record.with_id(
        "scheduled-due-" <> run_id,
        terminal_at_ms,
        record.ScheduledJobDue(
          job_id,
          "workflow",
          terminal_at_ms,
          run_id,
          "automatic",
        ),
      ),
    )
  projection.Projection(
    ..scheduled,
    workflow_runs: dict.insert(
      scheduled.workflow_runs,
      run_id,
      projection.WorkflowRunFinished(
        "workflow",
        "",
        "success",
        1,
        1,
        terminal_at_ms,
        "test/tmp/" <> run_id,
      ),
    ),
    workflow_run_provenances: dict.insert(
      scheduled.workflow_run_provenances,
      run_id,
      projection.WorkflowRunProvenance(
        "workflow",
        "fingerprint",
        "",
        job_id,
        "scheduled",
        terminal_at_ms - 1,
        "test/tmp/" <> run_id,
        task_ref("", job_id),
      ),
    ),
  )
}

fn task_ref(issue_id: String, identifier: String) -> record.TaskRefFields {
  record.TaskRefFields(
    task_backend_kind: "linear",
    task_remote_id: issue_id,
    task_key: Some(identifier),
    task_url: None,
  )
}

fn integers(from: Int, through: Int) -> List(Int) {
  case from > through {
    True -> []
    False -> [from, ..integers(from + 1, through)]
  }
}

fn test_policy() -> config_types.ProjectionRetentionConfig {
  policy(1000, 10_000, 25)
}

fn policy(
  grace: Int,
  max_age: Int,
  count: Int,
) -> config_types.ProjectionRetentionConfig {
  config_types.ProjectionRetentionConfig(True, grace, max_age, count)
}

fn no_marker(_run_root: String) -> retention.MarkerState {
  retention.MarkerAbsent
}
