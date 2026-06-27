import gleam/erlang/process
import gleam/option.{None}
import scherzo/control/query/service
import scherzo/control/query/types
import simplifile
import test_async

pub fn query_service_fast_success_test() {
  let assert Ok(handle) =
    service.start(
      service.Settings(max_concurrent: 1, max_queued: 1, timeout_ms: 50),
      service.Backend(run: fn(_) { Ok(status_response()) }),
    )

  assert service.query(handle, types.Status) == Ok(status_response())
  assert service.stop(handle, 1000) == Ok(Nil)
}

pub fn query_service_metrics_success_test() {
  let assert Ok(handle) =
    service.start(
      service.Settings(max_concurrent: 1, max_queued: 1, timeout_ms: 50),
      service.Backend(run: fn(_) { Ok(metrics_response()) }),
    )

  assert service.query(handle, types.Metrics) == Ok(metrics_response())
  assert service.stop(handle, 1000) == Ok(Nil)
}

pub fn query_service_outbox_success_test() {
  let request = types.OutboxList(types.default_outbox_list_query())
  let response = outbox_list_response()
  let assert Ok(handle) =
    service.start(
      service.Settings(max_concurrent: 1, max_queued: 1, timeout_ms: 50),
      service.Backend(run: fn(query) {
        case query == request {
          True -> Ok(response)
          False -> Ok(status_response())
        }
      }),
    )

  assert service.query(handle, request) == Ok(response)
  assert service.stop(handle, 1000) == Ok(Nil)
}

pub fn query_service_backend_failure_test() {
  let backend_error =
    types.QueryError(types.QueryBackendFailed, "backend failed")
  let assert Ok(handle) =
    service.start(
      service.Settings(max_concurrent: 1, max_queued: 1, timeout_ms: 50),
      service.Backend(run: fn(_) { Error(backend_error) }),
    )

  assert service.query(handle, types.Status) == Error(backend_error)
  assert service.stop(handle, 1000) == Ok(Nil)
}

pub fn query_service_stop_reports_timeout_when_caller_does_not_wait_test() {
  let assert Ok(handle) =
    service.start(
      service.Settings(max_concurrent: 1, max_queued: 1, timeout_ms: 50),
      service.Backend(run: fn(_) { Ok(status_response()) }),
    )

  assert service.stop(handle, 0) == Error(Nil)
  assert service.query(handle, types.Status)
    == Error(types.QueryError(types.QueryShutdown, "query service unavailable"))
}

pub fn query_service_timeout_test() {
  let started = process.new_subject()
  let barrier = test_async.new_barrier()
  let result = process.new_subject()
  let assert Ok(handle) =
    service.start(
      service.Settings(max_concurrent: 1, max_queued: 1, timeout_ms: 50),
      service.Backend(run: fn(_) {
        process.send(started, Nil)
        test_async.block_until_released(barrier)
        Ok(status_response())
      }),
    )

  let _ =
    process.spawn(fn() {
      process.send(result, service.query(handle, types.Status))
      Nil
    })
  let _ = test_async.expect_message(started)
  assert test_async.expect_message(result)
    == Error(types.QueryError(types.QueryTimeout, "query timed out"))
  test_async.release_barrier_if_waiting(barrier)
  assert service.stop(handle, 1000) == Ok(Nil)
}

pub fn query_service_overload_test() {
  let started = process.new_subject()
  let barrier = test_async.new_barrier()
  let results = process.new_subject()
  let assert Ok(handle) =
    service.start(
      service.Settings(max_concurrent: 1, max_queued: 0, timeout_ms: 100),
      service.Backend(run: fn(_) {
        process.send(started, "started")
        test_async.block_until_released(barrier)
        Ok(status_response())
      }),
    )

  let _ =
    process.spawn(fn() {
      process.send(results, service.query(handle, types.Status))
      Nil
    })
  let _ = test_async.expect_message(started)

  assert service.query(handle, types.Status)
    == Error(types.QueryError(types.QueryOverloaded, "query service overloaded"))

  test_async.release_barrier(barrier)
  assert test_async.expect_message(results) == Ok(status_response())
  assert service.stop(handle, 1000) == Ok(Nil)
}

pub fn query_service_stale_completion_is_ignored_after_timeout_test() {
  let path = "test/tmp/query-service-first-call"
  let _ = simplifile.delete(path)
  let started = process.new_subject()
  let barrier = test_async.new_barrier()
  let first_result = process.new_subject()
  let assert Ok(handle) =
    service.start(
      service.Settings(max_concurrent: 1, max_queued: 1, timeout_ms: 50),
      service.Backend(run: fn(_) {
        case simplifile.read(path) {
          Ok(_) -> Ok(status_response())
          Error(_) -> {
            let assert Ok(Nil) = simplifile.write(path, "first")
            process.send(started, Nil)
            test_async.block_until_released(barrier)
            Ok(status_response())
          }
        }
      }),
    )

  let _ =
    process.spawn(fn() {
      process.send(first_result, service.query(handle, types.Status))
      Nil
    })
  let _ = test_async.expect_message(started)
  assert test_async.expect_message(first_result)
    == Error(types.QueryError(types.QueryTimeout, "query timed out"))
  test_async.release_barrier_if_waiting(barrier)
  assert service.query(handle, types.Status) == Ok(status_response())

  assert service.stop(handle, 1000) == Ok(Nil)
}

pub fn query_service_shutdown_cleans_up_running_and_queued_queries_test() {
  let started = process.new_subject()
  let barrier = test_async.new_barrier()
  let running_result = process.new_subject()
  let queued_result = process.new_subject()
  let assert Ok(handle) =
    service.start(
      service.Settings(max_concurrent: 1, max_queued: 1, timeout_ms: 100),
      service.Backend(run: fn(_) {
        process.send(started, "started")
        test_async.block_until_released(barrier)
        Ok(status_response())
      }),
    )

  let _ =
    process.spawn(fn() {
      process.send(running_result, service.query(handle, types.Status))
      Nil
    })
  let _ = test_async.expect_message(started)
  let _ =
    process.spawn(fn() {
      process.send(queued_result, service.query(handle, types.Status))
      Nil
    })

  assert service.stop(handle, 1000) == Ok(Nil)
  assert shutdown_error_code(test_async.expect_message(running_result))
  assert shutdown_error_code(test_async.expect_message(queued_result))
  test_async.release_barrier_if_waiting(barrier)
}

fn shutdown_error_code(
  result: Result(types.QueryResponse, types.QueryError),
) -> Bool {
  case result {
    Error(types.QueryError(types.QueryShutdown, _)) -> True
    _ -> False
  }
}

fn status_response() -> types.QueryResponse {
  types.StatusResponse(
    types.StatusDto(
      daemon_id: "daemon-1",
      boot_id: "boot-1",
      dispatch_paused: False,
      ui_server_enabled: False,
      supported_queries: ["status"],
    ),
  )
}

fn outbox_list_response() -> types.QueryResponse {
  types.OutboxListResponse(types.OutboxListDto(
    items: [],
    page: types.PageDto(next_cursor: None, has_more: False),
  ))
}

fn metrics_response() -> types.QueryResponse {
  types.MetricsResponse(types.OperationalMetricsDto(
    schema_version: types.operational_metrics_schema_version,
    daemon_id: "daemon-1",
    boot_id: "boot-1",
    sampled_at_ms: 123,
    dispatch_paused: False,
    ui_server_enabled: False,
    remote_client_status: "disabled",
    workflow_count: 1,
    scheduled_job_count: 0,
    active_sessions: 0,
    running_workers: 0,
    running_scheduled_workers: 0,
    queued_claims: 0,
    pending_dispatch_validations: 0,
    pending_review_lane_preflights: 0,
    claimed_tasks: 0,
    retry_tasks: 0,
    parked_tasks: 0,
    completed_tasks: 0,
    pending_outbox_count: 0,
    in_flight_outbox_count: 0,
    retryable_outbox_count: 0,
    permanent_outbox_count: 0,
    poll_generation: 1,
    poll_in_flight: False,
    poll_timer_active: True,
    retry_timer_count: 0,
    retry_refresh_in_flight_count: 0,
    lifecycle_projection_failed: False,
    scheduled_due_count: 0,
    scheduled_next_due_count: 0,
    scheduled_pending_count: 0,
    scheduled_retry_count: 0,
    scheduled_report_retry_count: 0,
    scheduled_retry_timer_count: 0,
    scheduled_report_retry_timer_count: 0,
    token_totals: types.TokenTotalsDto(
      input: 0,
      output: 0,
      cache_read: 0,
      cache_write: 0,
      total: 0,
    ),
  ))
}
