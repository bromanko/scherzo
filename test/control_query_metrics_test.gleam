import scherzo/control/query/metrics
import scherzo/control/query/types
import scherzo/daemon_identity

pub fn execute_metrics_times_out_when_dispatch_state_unavailable_test() {
  let result =
    metrics.execute_metrics(
      ui_server_enabled: False,
      identity: test_identity(),
      sampled_at_ms: 123,
      get_dispatch_paused: fn(_) { Error(Nil) },
      get_runtime_metrics: fn(_) { Ok(metrics.empty_runtime_metrics()) },
    )

  let assert Error(types.QueryError(code: code, message: message)) = result
  assert code == types.QueryTimeout
  assert message == "daemon metrics query timed out"
}

pub fn execute_metrics_times_out_when_runtime_metrics_unavailable_test() {
  let result =
    metrics.execute_metrics(
      ui_server_enabled: False,
      identity: test_identity(),
      sampled_at_ms: 123,
      get_dispatch_paused: fn(_) { Ok(False) },
      get_runtime_metrics: fn(_) { Error(Nil) },
    )

  let assert Error(types.QueryError(code: code, message: message)) = result
  assert code == types.QueryTimeout
  assert message == "daemon metrics query timed out"
}

fn test_identity() -> daemon_identity.DaemonIdentity {
  daemon_identity.DaemonIdentity(
    daemon_id: "daemon-1",
    boot_id: "boot-1",
    path: "test/tmp/control-query-metrics/daemon_identity.json",
  )
}
