import scherzo/control/query/metrics
import scherzo/control/query/types
import scherzo/orchestrator/read_model

pub fn execute_status_times_out_when_snapshot_unavailable_test() {
  let result = metrics.execute_status(get_snapshot: fn(_) { Error(Nil) })

  let assert Error(types.QueryError(code: code, message: message)) = result
  assert code == types.QueryTimeout
  assert message == "daemon status query timed out"
}

pub fn execute_status_uses_read_model_snapshot_test() {
  let snapshot =
    read_model.new(
      daemon_id: "daemon-1",
      boot_id: "boot-1",
      ui_server_enabled: True,
    )
    |> read_model.update_dispatch_paused(dispatch_paused: True)
    |> read_model.snapshot(sampled_at_ms: 123)

  let assert Ok(types.StatusResponse(status_response)) =
    metrics.execute_status(get_snapshot: fn(_) { Ok(snapshot) })

  assert status_response.daemon_id == "daemon-1"
  assert status_response.boot_id == "boot-1"
  assert status_response.dispatch_paused
  assert status_response.ui_server_enabled
  assert status_response.supported_queries == types.supported_queries()
}

pub fn execute_metrics_times_out_when_snapshot_unavailable_test() {
  let result = metrics.execute_metrics(get_snapshot: fn(_) { Error(Nil) })

  let assert Error(types.QueryError(code: code, message: message)) = result
  assert code == types.QueryTimeout
  assert message == "daemon metrics query timed out"
}

pub fn execute_metrics_uses_read_model_snapshot_test() {
  let snapshot =
    read_model.new(
      daemon_id: "daemon-1",
      boot_id: "boot-1",
      ui_server_enabled: True,
    )
    |> read_model.update_dispatch_paused(dispatch_paused: True)
    |> read_model.update_remote_client_status(read_model.Connected)
    |> read_model.update_counts(
      read_model.RuntimeCounts(
        ..read_model.empty_runtime_counts(),
        workflow_count: 2,
        active_sessions: 3,
        lifecycle_projection_failed: True,
      ),
    )
    |> read_model.snapshot(sampled_at_ms: 123)

  let assert Ok(types.MetricsResponse(metrics_response)) =
    metrics.execute_metrics(get_snapshot: fn(_) { Ok(snapshot) })

  assert metrics_response.daemon_id == "daemon-1"
  assert metrics_response.boot_id == "boot-1"
  assert metrics_response.dispatch_paused
  assert metrics_response.remote_client_status == "connected"
  assert metrics_response.workflow_count == 2
  assert metrics_response.active_sessions == 3
  assert metrics_response.lifecycle_projection_failed
}
