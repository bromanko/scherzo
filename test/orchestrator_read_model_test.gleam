import gleam/json
import gleam/list
import gleam/string
import scherzo/control/query/dto
import scherzo/control/query/types as query_types
import scherzo/orchestrator/read_model
import scherzo/session/tokens as session_tokens

pub fn fresh_snapshot_uses_safe_zero_values_test() {
  let snapshot =
    read_model.new(
      daemon_id: "daemon-1",
      boot_id: "boot-1",
      ui_server_enabled: False,
    )
    |> read_model.snapshot(sampled_at_ms: 123)

  let status = snapshot |> read_model.status_source |> dto.status_from_source
  let metrics =
    snapshot |> read_model.metrics_source |> dto.operational_metrics_from_source

  assert status
    == query_types.StatusDto(
      daemon_id: "daemon-1",
      boot_id: "boot-1",
      dispatch_paused: False,
      ui_server_enabled: False,
      supported_queries: query_types.supported_queries(),
    )
  assert metrics.remote_client_status == "disabled"
  assert metrics.sampled_at_ms == 123
  assert metrics.workflow_count == 0
  assert metrics.token_totals
    == query_types.TokenTotalsDto(
      input: 0,
      output: 0,
      cache_read: 0,
      cache_write: 0,
      total: 0,
    )
}

pub fn snapshot_preserves_cache_token_totals_exactly_test() {
  let metrics =
    read_model.new(
      daemon_id: "daemon-2",
      boot_id: "boot-2",
      ui_server_enabled: True,
    )
    |> read_model.update_counts(
      read_model.RuntimeCounts(
        ..read_model.empty_runtime_counts(),
        workflow_count: 2,
        active_sessions: 3,
        running_workers: 4,
      ),
    )
    |> read_model.update_dispatch_paused(dispatch_paused: True)
    |> read_model.update_remote_client_status(read_model.Connected)
    |> read_model.update_token_totals(session_tokens.TokenTotals(
      input: 10,
      output: 20,
      cache_read: 3,
      cache_write: 4,
      total: 37,
    ))
    |> read_model.snapshot(sampled_at_ms: 456)
    |> read_model.metrics_source
    |> dto.operational_metrics_from_source

  assert metrics.remote_client_status == "connected"
  assert metrics.dispatch_paused
  assert metrics.workflow_count == 2
  assert metrics.active_sessions == 3
  assert metrics.running_workers == 4
  assert metrics.token_totals
    == query_types.TokenTotalsDto(
      input: 10,
      output: 20,
      cache_read: 3,
      cache_write: 4,
      total: 37,
    )
}

pub fn snapshot_preserves_scheduler_counts_test() {
  let metrics =
    read_model.new(
      daemon_id: "daemon-4",
      boot_id: "boot-4",
      ui_server_enabled: True,
    )
    |> read_model.update_counts(
      read_model.RuntimeCounts(
        ..read_model.empty_runtime_counts(),
        scheduled_due_count: 5,
        scheduled_pending_count: 6,
        scheduled_retry_count: 7,
        scheduled_report_retry_count: 8,
        scheduled_retry_timer_count: 9,
        scheduled_report_retry_timer_count: 10,
      ),
    )
    |> read_model.snapshot(sampled_at_ms: 321)
    |> read_model.metrics_source
    |> dto.operational_metrics_from_source

  assert metrics.scheduled_due_count == 5
  assert metrics.scheduled_pending_count == 6
  assert metrics.scheduled_retry_count == 7
  assert metrics.scheduled_report_retry_count == 8
  assert metrics.scheduled_retry_timer_count == 9
  assert metrics.scheduled_report_retry_timer_count == 10
}

pub fn snapshot_exposes_full_remote_client_status_vocabulary_test() {
  assert remote_client_status_label(read_model.new(
      daemon_id: "daemon-5",
      boot_id: "boot-5",
      ui_server_enabled: False,
    ))
    == "disabled"
  assert remote_client_status_label(read_model.new(
      daemon_id: "daemon-6",
      boot_id: "boot-6",
      ui_server_enabled: True,
    ))
    == "starting"
  assert remote_client_status_label(
      read_model.new(
        daemon_id: "daemon-7",
        boot_id: "boot-7",
        ui_server_enabled: True,
      )
      |> read_model.update_remote_client_status(read_model.Connected),
    )
    == "connected"
  assert remote_client_status_label(
      read_model.new(
        daemon_id: "daemon-8",
        boot_id: "boot-8",
        ui_server_enabled: True,
      )
      |> read_model.update_remote_client_status(read_model.Retrying(
        "dial_failed",
      )),
    )
    == "retrying"
  assert remote_client_status_label(
      read_model.new(
        daemon_id: "daemon-9",
        boot_id: "boot-9",
        ui_server_enabled: True,
      )
      |> read_model.update_remote_client_status(read_model.Stopped),
    )
    == "stopped"
}

pub fn retrying_status_snapshot_redacts_secret_markers_test() {
  let secret_markers = [
    "local-secret-token",
    "enrollment-secret-token",
    "api-key-123",
    "raw prompt body",
    "provider:linear",
    "raw failure payload",
  ]
  let encoded_status =
    read_model.new(
      daemon_id: "daemon-3",
      boot_id: "boot-3",
      ui_server_enabled: True,
    )
    |> read_model.update_remote_client_status(read_model.Retrying("dial_failed"))
    |> read_model.snapshot(sampled_at_ms: 789)
    |> read_model.status_source
    |> dto.status_from_source
    |> dto.status_to_json
    |> json.to_string
  let encoded_metrics =
    read_model.new(
      daemon_id: "daemon-3",
      boot_id: "boot-3",
      ui_server_enabled: True,
    )
    |> read_model.update_remote_client_status(read_model.Retrying("dial_failed"))
    |> read_model.snapshot(sampled_at_ms: 789)
    |> read_model.metrics_source
    |> dto.operational_metrics_from_source
    |> dto.operational_metrics_to_json
    |> json.to_string

  assert string.contains(
    encoded_metrics,
    "\"remote_client_status\":\"retrying\"",
  )
  list.each(secret_markers, fn(marker) {
    assert !string.contains(encoded_status, marker)
    assert !string.contains(encoded_metrics, marker)
  })
}

fn remote_client_status_label(
  read_model_value: read_model.ReadModel,
) -> String {
  read_model_value
  |> read_model.snapshot(sampled_at_ms: 999)
  |> read_model.metrics_source
  |> dto.operational_metrics_from_source
  |> fn(metrics) { metrics.remote_client_status }
}
