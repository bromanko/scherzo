import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/order.{type Order, Eq}
import gleam/result
import gleam/string
import scherzo/config/types as config_types
import scherzo/orchestrator/core
import scherzo/orchestrator/reason
import scherzo/orchestrator/state as orchestrator_state
import scherzo/state/outbox
import scherzo/state/projection
import scherzo/state/record
import scherzo/tracker/issue as tracker_issue

pub type RecoveredRetry {
  RecoveredRetry(
    issue_id: String,
    issue_identifier: String,
    delay_ms: Int,
    generation: Int,
    reason: String,
  )
}

pub type CleanupRequest {
  CleanupRequest(
    issue_id: String,
    issue_identifier: String,
    workspace_path: String,
  )
}

pub type OutboxReplay {
  OutboxReplay(
    outbox_id: String,
    issue_id: String,
    outbox_kind: String,
    dedupe_key: String,
    payload_json: String,
  )
}

pub type RecoveryPlan {
  RecoveryPlan(
    runtime: orchestrator_state.RuntimeState,
    retry_timers: List(RecoveredRetry),
    records_to_append: List(record.LedgerRecord),
    cleanup_workspaces: List(CleanupRequest),
    outbox_to_replay: List(OutboxReplay),
    warnings: List(String),
  )
}

pub type RecoveryError {
  MissingOutboxPayload(outbox_id: String)
  InvalidRecordSemantics(reason: String)
}

type Build {
  Build(
    runtime: orchestrator_state.RuntimeState,
    retry_timers: List(RecoveredRetry),
    record_bodies: List(record.RecordBody),
    cleanup_workspaces: List(CleanupRequest),
    warnings: List(String),
    auto_unparked_issue_ids: List(String),
  )
}

type OutboxRecovery {
  OutboxRecovery(
    outbox_to_replay: List(OutboxReplay),
    record_bodies: List(record.RecordBody),
    warnings: List(String),
  )
}

pub fn known_issue_ids(projection: projection.Projection) -> List(String) {
  projection.known_issue_ids(projection)
}

pub fn plan(
  projection: projection.Projection,
  config: config_types.EffectiveConfig,
  refreshed_issues: List(tracker_issue.Issue),
  now_ms: Int,
) -> Result(RecoveryPlan, RecoveryError) {
  let outbox_recovery = replayable_outbox(projection)
  let issue_by_id = issues_by_id(refreshed_issues)
  let base = core.new_state(config)
  let build =
    Build(
      runtime: restore_counters(base, projection),
      retry_timers: [],
      record_bodies: list.reverse(outbox_recovery.record_bodies),
      cleanup_workspaces: [],
      warnings: list.reverse(outbox_recovery.warnings),
      auto_unparked_issue_ids: [],
    )
  let build = restore_parked(build, projection, issue_by_id)
  let build = restore_retries(build, projection, config, issue_by_id, now_ms)
  let build =
    recover_interrupted_runs(build, projection, config, issue_by_id, now_ms)
  Ok(RecoveryPlan(
    runtime: build.runtime,
    retry_timers: list.reverse(build.retry_timers),
    records_to_append: ledger_records(now_ms, list.reverse(build.record_bodies)),
    cleanup_workspaces: list.reverse(build.cleanup_workspaces),
    outbox_to_replay: outbox_recovery.outbox_to_replay,
    warnings: list.reverse(build.warnings),
  ))
}

pub fn describe_error(error: RecoveryError) -> String {
  case error {
    MissingOutboxPayload(outbox_id) -> "outbox_payload_missing:" <> outbox_id
    InvalidRecordSemantics(reason) -> reason
  }
}

fn replayable_outbox(projection: projection.Projection) -> OutboxRecovery {
  let recovered =
    projection.outbox
    |> dict.to_list
    |> list.sort(by: compare_outbox_entries_by_time)
    |> list.fold(
      OutboxRecovery(outbox_to_replay: [], record_bodies: [], warnings: []),
      recover_outbox_entry,
    )

  OutboxRecovery(
    outbox_to_replay: list.reverse(recovered.outbox_to_replay),
    record_bodies: list.reverse(recovered.record_bodies),
    warnings: list.reverse(recovered.warnings),
  )
}

fn recover_outbox_entry(
  recovery: OutboxRecovery,
  entry: #(String, projection.OutboxStatus),
) -> OutboxRecovery {
  let #(outbox_id, status) = entry
  case status {
    projection.OutboxPending(issue_id, outbox_kind, _, _) ->
      fail_outbox_recovery(
        recovery,
        outbox_id,
        issue_id,
        outbox_kind,
        "outbox_payload_missing",
      )
    projection.OutboxPendingV2(
      issue_id,
      outbox_kind,
      dedupe_key,
      payload_json,
      _,
    ) ->
      case outbox.decode_payload(payload_json) {
        Error(error_code) ->
          fail_outbox_recovery(
            recovery,
            outbox_id,
            issue_id,
            outbox_kind,
            error_code,
          )
        Ok(payload) ->
          case outbox.recovery_replay_error(outbox_kind, payload.kind) {
            Error(error_code) ->
              fail_outbox_recovery(
                recovery,
                outbox_id,
                issue_id,
                outbox_kind,
                error_code,
              )
            Ok(Nil) ->
              OutboxRecovery(..recovery, outbox_to_replay: [
                OutboxReplay(
                  outbox_id,
                  issue_id,
                  outbox_kind,
                  dedupe_key,
                  payload_json,
                ),
                ..recovery.outbox_to_replay
              ])
          }
      }
    projection.OutboxCompleted(_, _, _) | projection.OutboxFailed(_, _, _, _) ->
      recovery
  }
}

fn fail_outbox_recovery(
  recovery: OutboxRecovery,
  outbox_id: String,
  issue_id: String,
  outbox_kind: String,
  error_code: String,
) -> OutboxRecovery {
  OutboxRecovery(
    ..recovery,
    record_bodies: [
      record.OutboxFailed(outbox_id, issue_id, outbox_kind, error_code),
      ..recovery.record_bodies
    ],
    warnings: [
      outbox_recovery_warning(outbox_id, error_code),
      ..recovery.warnings
    ],
  )
}

fn outbox_recovery_warning(outbox_id: String, error_code: String) -> String {
  "outbox_replay_failed:" <> outbox_id <> ":" <> error_code
}

fn compare_outbox_entries_by_time(
  a: #(String, projection.OutboxStatus),
  b: #(String, projection.OutboxStatus),
) -> Order {
  let #(a_id, a_status) = a
  let #(b_id, b_status) = b
  case int.compare(outbox_status_time(a_status), outbox_status_time(b_status)) {
    Eq -> string.compare(a_id, b_id)
    order -> order
  }
}

fn outbox_status_time(status: projection.OutboxStatus) -> Int {
  case status {
    projection.OutboxPending(_, _, _, pending_at_ms) -> pending_at_ms
    projection.OutboxPendingV2(_, _, _, _, pending_at_ms) -> pending_at_ms
    projection.OutboxCompleted(_, _, completed_at_ms) -> completed_at_ms
    projection.OutboxFailed(_, _, _, failed_at_ms) -> failed_at_ms
  }
}

fn restore_counters(
  runtime: orchestrator_state.RuntimeState,
  projection: projection.Projection,
) -> orchestrator_state.RuntimeState {
  let counters =
    projection.issue_counters
    |> dict.to_list
    |> list.map(fn(entry) {
      let #(issue_id, counter) = entry
      #(
        issue_id,
        orchestrator_state.IssueCounter(
          counter.failure_attempts,
          counter.worker_sessions,
        ),
      )
    })
    |> dict.from_list
  orchestrator_state.RuntimeState(..runtime, issue_counters: counters)
}

fn restore_parked(
  build: Build,
  projection: projection.Projection,
  issue_by_id: Dict(String, tracker_issue.Issue),
) -> Build {
  projection.parked_issues
  |> dict.to_list
  |> list.fold(build, fn(build, entry) {
    let #(issue_id, parked) = entry
    case parked_should_survive(parked, issue_id, issue_by_id) {
      True -> {
        let parked_entry =
          orchestrator_state.ParkedEntry(
            issue_id: issue_id,
            identifier: parked.issue_identifier,
            reason: park_reason_from_string(parked.reason),
            release_policy: release_policy_from_projection(parked),
            parked_at_ms: parked.parked_at_ms,
          )
        Build(
          ..build,
          runtime: orchestrator_state.RuntimeState(
            ..build.runtime,
            parked: dict.insert(build.runtime.parked, issue_id, parked_entry),
            claimed: dict.delete(build.runtime.claimed, issue_id),
          ),
        )
      }
      False ->
        Build(
          ..build,
          runtime: orchestrator_state.RuntimeState(
            ..build.runtime,
            parked: dict.delete(build.runtime.parked, issue_id),
            retry_attempts: dict.delete(build.runtime.retry_attempts, issue_id),
            issue_counters: dict.delete(build.runtime.issue_counters, issue_id),
            claimed: dict.delete(build.runtime.claimed, issue_id),
          ),
          record_bodies: [
            record.IssueUnparked(
              issue_id,
              parked.issue_identifier,
              "issue_changed",
            ),
            record.IssueCounterUpdated(
              issue_id,
              parked.issue_identifier,
              0,
              0,
              parked.observed_updated_at_ms,
              None,
            ),
            ..build.record_bodies
          ],
          auto_unparked_issue_ids: [issue_id, ..build.auto_unparked_issue_ids],
        )
    }
  })
}

fn restore_retries(
  build: Build,
  projection: projection.Projection,
  config: config_types.EffectiveConfig,
  issue_by_id: Dict(String, tracker_issue.Issue),
  now_ms: Int,
) -> Build {
  projection.retries
  |> dict.to_list
  |> list.fold(build, fn(build, entry) {
    let #(issue_id, status) = entry
    case status {
      projection.RetryScheduled(issue_identifier, _, generation, reason_text, _) ->
        restore_scheduled_retry(
          build,
          config,
          issue_by_id,
          issue_id,
          issue_identifier,
          generation,
          reason_text,
          status,
          now_ms,
        )
      projection.RetryCancelled(_, _, _) -> build
    }
  })
}

fn restore_scheduled_retry(
  build: Build,
  config: config_types.EffectiveConfig,
  issue_by_id: Dict(String, tracker_issue.Issue),
  issue_id: String,
  issue_identifier: String,
  generation: Int,
  reason_text: String,
  status: projection.RetryStatus,
  now_ms: Int,
) -> Build {
  case list.contains(build.auto_unparked_issue_ids, issue_id) {
    True -> build
    False ->
      case dict.has_key(build.runtime.parked, issue_id) {
        True -> build
        False ->
          case dict.get(issue_by_id, issue_id) {
            Error(_) -> warn(build, "missing_issue_for_retry:" <> issue_id)
            Ok(issue) ->
              case core.is_terminal(config, issue.state) {
                True -> build
                False ->
                  case core.is_active(config, issue.state) {
                    False ->
                      warn(build, "non_active_issue_for_retry:" <> issue_id)
                    True -> {
                      let remaining = remaining_retry_delay(status, now_ms)
                      let retry =
                        orchestrator_state.RetryEntry(
                          issue_id,
                          remaining,
                          generation,
                        )
                      Build(
                        ..build,
                        runtime: orchestrator_state.RuntimeState(
                          ..build.runtime,
                          retry_attempts: dict.insert(
                            build.runtime.retry_attempts,
                            issue_id,
                            retry,
                          ),
                          claimed: dict.insert(
                            build.runtime.claimed,
                            issue_id,
                            issue_identifier,
                          ),
                        ),
                        retry_timers: [
                          RecoveredRetry(
                            issue_id,
                            issue_identifier,
                            remaining,
                            generation,
                            reason_text,
                          ),
                          ..build.retry_timers
                        ],
                      )
                    }
                  }
              }
          }
      }
  }
}

fn recover_interrupted_runs(
  build: Build,
  projection: projection.Projection,
  config: config_types.EffectiveConfig,
  issue_by_id: Dict(String, tracker_issue.Issue),
  now_ms: Int,
) -> Build {
  projection.runs
  |> dict.to_list
  |> list.fold(build, fn(build, entry) {
    let #(run_id, status) = entry
    case status {
      projection.RunRunning(issue_id, issue_identifier, workspace_path, _) ->
        recover_one_interrupted_run(
          build,
          projection,
          config,
          issue_by_id,
          run_id,
          issue_id,
          issue_identifier,
          workspace_path,
          True,
          now_ms,
        )
      projection.RunInterrupted(issue_id, _, _) ->
        recover_one_interrupted_run(
          build,
          projection,
          config,
          issue_by_id,
          run_id,
          issue_id,
          identifier_for_issue(projection, issue_by_id, issue_id),
          workspace_for_issue(projection, issue_id),
          False,
          now_ms,
        )
      projection.RunFinished(_, _, _, _, _) -> build
    }
  })
}

fn recover_one_interrupted_run(
  build: Build,
  projection: projection.Projection,
  config: config_types.EffectiveConfig,
  issue_by_id: Dict(String, tracker_issue.Issue),
  run_id: String,
  issue_id: String,
  issue_identifier: String,
  workspace_path: String,
  append_interrupted: Bool,
  now_ms: Int,
) -> Build {
  let build = case append_interrupted {
    True ->
      Build(..build, record_bodies: [
        record.RunInterrupted(run_id, issue_id, "daemon_restart"),
        ..build.record_bodies
      ])
    False -> build
  }
  case dict.get(issue_by_id, issue_id) {
    Error(_) -> warn(build, "missing_issue_for_interrupted_run:" <> issue_id)
    Ok(issue) ->
      case core.is_terminal(config, issue.state) {
        True ->
          recover_terminal_interrupted(
            build,
            projection,
            issue,
            issue_id,
            issue_identifier,
            workspace_path,
          )
        False ->
          case core.is_active(config, issue.state) {
            True ->
              recover_active_interrupted(
                build,
                projection,
                config,
                issue,
                run_id,
                issue_identifier,
                now_ms,
              )
            False -> warn(build, "non_active_interrupted_run:" <> issue_id)
          }
      }
  }
}

fn recover_terminal_interrupted(
  build: Build,
  projection: projection.Projection,
  issue: tracker_issue.Issue,
  issue_id: String,
  issue_identifier: String,
  workspace_path: String,
) -> Build {
  let workspace_path = case string.trim(workspace_path) == "" {
    True -> workspace_for_issue(projection, issue_id)
    False -> workspace_path
  }
  let cleanup_workspaces = case string.trim(workspace_path) == "" {
    True -> build.cleanup_workspaces
    False -> [
      CleanupRequest(issue_id, issue_identifier, workspace_path),
      ..build.cleanup_workspaces
    ]
  }
  Build(
    ..build,
    runtime: orchestrator_state.RuntimeState(
      ..build.runtime,
      completed: dict.insert(build.runtime.completed, issue_id, issue),
      claimed: dict.delete(build.runtime.claimed, issue_id),
      retry_attempts: dict.delete(build.runtime.retry_attempts, issue_id),
    ),
    cleanup_workspaces: cleanup_workspaces,
  )
}

fn recover_active_interrupted(
  build: Build,
  projection: projection.Projection,
  config: config_types.EffectiveConfig,
  issue: tracker_issue.Issue,
  run_id: String,
  issue_identifier: String,
  now_ms: Int,
) -> Build {
  let issue_id = issue.id
  case projection.counter_has_source_run(projection, issue_id, run_id) {
    True ->
      ensure_retry_or_park_for_counter(
        build,
        config,
        issue,
        issue_identifier,
        now_ms,
      )
    False -> {
      let counter = counter_for_runtime(build.runtime, issue_id)
      let failures = counter.failure_attempts + 1
      let counter =
        orchestrator_state.IssueCounter(..counter, failure_attempts: failures)
      let build =
        Build(
          ..build,
          runtime: orchestrator_state.RuntimeState(
            ..build.runtime,
            issue_counters: dict.insert(
              build.runtime.issue_counters,
              issue_id,
              counter,
            ),
          ),
          record_bodies: [
            record.IssueCounterUpdated(
              issue_id,
              issue_identifier,
              counter.failure_attempts,
              counter.worker_sessions,
              now_ms,
              Some(run_id),
            ),
            ..build.record_bodies
          ],
        )
      ensure_retry_or_park_for_counter(
        build,
        config,
        issue,
        issue_identifier,
        now_ms,
      )
    }
  }
}

fn ensure_retry_or_park_for_counter(
  build: Build,
  config: config_types.EffectiveConfig,
  issue: tracker_issue.Issue,
  issue_identifier: String,
  now_ms: Int,
) -> Build {
  let issue_id = issue.id
  let counter = counter_for_runtime(build.runtime, issue_id)
  case counter.failure_attempts >= config.agent.max_retry_attempts {
    True ->
      case dict.has_key(build.runtime.parked, issue_id) {
        True -> build
        False -> {
          let fingerprint = core.issue_fingerprint(issue)
          let parked =
            orchestrator_state.ParkedEntry(
              issue_id: issue_id,
              identifier: issue_identifier,
              reason: reason.ParkMaxRetryAttempts,
              release_policy: orchestrator_state.AutoUnparkOnIssueChange(
                fingerprint,
              ),
              parked_at_ms: now_ms,
            )
          Build(
            ..build,
            runtime: orchestrator_state.RuntimeState(
              ..build.runtime,
              parked: dict.insert(build.runtime.parked, issue_id, parked),
              retry_attempts: dict.delete(
                build.runtime.retry_attempts,
                issue_id,
              ),
              claimed: dict.delete(build.runtime.claimed, issue_id),
            ),
            record_bodies: [
              record.IssueParkedV2(
                issue_id,
                issue_identifier,
                reason.park_to_string(reason.ParkMaxRetryAttempts),
                "auto_unpark_on_issue_change",
                fingerprint,
                now_ms,
              ),
              ..build.record_bodies
            ],
          )
        }
      }
    False ->
      case dict.has_key(build.runtime.retry_attempts, issue_id) {
        True -> build
        False -> {
          let delay_ms =
            core.backoff_delay(
              counter.failure_attempts,
              config.agent.max_retry_backoff_ms,
            )
          let generation = 1
          let retry =
            orchestrator_state.RetryEntry(issue_id, delay_ms, generation)
          Build(
            ..build,
            runtime: orchestrator_state.RuntimeState(
              ..build.runtime,
              retry_attempts: dict.insert(
                build.runtime.retry_attempts,
                issue_id,
                retry,
              ),
              claimed: dict.insert(
                build.runtime.claimed,
                issue_id,
                issue_identifier,
              ),
            ),
            retry_timers: [
              RecoveredRetry(
                issue_id,
                issue_identifier,
                delay_ms,
                generation,
                reason.retry_to_string(reason.RetryAfterFailure),
              ),
              ..build.retry_timers
            ],
            record_bodies: [
              record.RetryScheduled(
                issue_id,
                issue_identifier,
                delay_ms,
                generation,
                reason.retry_to_string(reason.RetryAfterFailure),
              ),
              ..build.record_bodies
            ],
          )
        }
      }
  }
}

fn parked_should_survive(
  parked: projection.ParkedIssue,
  issue_id: String,
  issue_by_id: Dict(String, tracker_issue.Issue),
) -> Bool {
  case parked.release_policy {
    "auto_unpark_on_issue_change" ->
      case dict.get(issue_by_id, issue_id) {
        Ok(issue) -> core.issue_fingerprint(issue) == parked.issue_fingerprint
        Error(_) -> True
      }
    _ -> True
  }
}

fn release_policy_from_projection(
  parked: projection.ParkedIssue,
) -> orchestrator_state.ParkReleasePolicy {
  case parked.release_policy {
    "auto_unpark_on_issue_change" ->
      orchestrator_state.AutoUnparkOnIssueChange(parked.issue_fingerprint)
    _ -> orchestrator_state.ExplicitUnparkOnly
  }
}

fn remaining_retry_delay(status: projection.RetryStatus, now_ms: Int) -> Int {
  case projection.retry_due_at_ms(status) {
    Ok(due_at_ms) -> max_int(0, due_at_ms - now_ms)
    Error(_) -> 0
  }
}

fn max_int(a: Int, b: Int) -> Int {
  case a > b {
    True -> a
    False -> b
  }
}

fn issues_by_id(
  issues: List(tracker_issue.Issue),
) -> Dict(String, tracker_issue.Issue) {
  issues
  |> list.map(fn(issue) { #(issue.id, issue) })
  |> dict.from_list
}

fn ledger_records(
  now_ms: Int,
  bodies: List(record.RecordBody),
) -> List(record.LedgerRecord) {
  ledger_records_loop(bodies, now_ms, 1, [])
}

fn ledger_records_loop(
  bodies: List(record.RecordBody),
  now_ms: Int,
  sequence: Int,
  acc: List(record.LedgerRecord),
) -> List(record.LedgerRecord) {
  case bodies {
    [] -> list.reverse(acc)
    [body, ..rest] ->
      ledger_records_loop(rest, now_ms, sequence + 1, [
        record.new(now_ms, sequence, body),
        ..acc
      ])
  }
}

fn counter_for_runtime(
  runtime: orchestrator_state.RuntimeState,
  issue_id: String,
) -> orchestrator_state.IssueCounter {
  dict.get(runtime.issue_counters, issue_id)
  |> result.unwrap(orchestrator_state.new_issue_counter())
}

fn park_reason_from_string(text: String) -> reason.ParkReason {
  case text {
    "max_retry_attempts" -> reason.ParkMaxRetryAttempts
    "max_sessions_per_issue" -> reason.ParkMaxSessionsPerIssue
    other -> reason.ParkOperator(other)
  }
}

fn identifier_for_issue(
  projection: projection.Projection,
  issue_by_id: Dict(String, tracker_issue.Issue),
  issue_id: String,
) -> String {
  case dict.get(issue_by_id, issue_id) {
    Ok(issue) -> issue.identifier
    Error(_) ->
      case dict.get(projection.known_workspaces, issue_id) {
        Ok(workspace) -> workspace.issue_identifier
        Error(_) ->
          case dict.get(projection.issue_counters, issue_id) {
            Ok(counter) -> counter.issue_identifier
            Error(_) -> issue_id
          }
      }
  }
}

fn workspace_for_issue(
  projection: projection.Projection,
  issue_id: String,
) -> String {
  projection.known_workspace_for_issue(projection, issue_id)
  |> result.unwrap("")
}

fn warn(build: Build, warning: String) -> Build {
  Build(..build, warnings: [warning, ..build.warnings])
}
