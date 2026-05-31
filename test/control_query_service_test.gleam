import gleam/erlang/process
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

pub fn query_service_timeout_test() {
  let assert Ok(handle) =
    service.start(
      service.Settings(max_concurrent: 1, max_queued: 1, timeout_ms: 10),
      service.Backend(run: fn(_) {
        process.sleep(50)
        Ok(status_response())
      }),
    )

  assert service.query(handle, types.Status)
    == Error(types.QueryError(types.QueryTimeout, "query timed out"))
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
  let assert Ok(handle) =
    service.start(
      service.Settings(max_concurrent: 1, max_queued: 1, timeout_ms: 10),
      service.Backend(run: fn(_) {
        case simplifile.read(path) {
          Ok(_) -> Ok(status_response())
          Error(_) -> {
            let assert Ok(Nil) = simplifile.write(path, "first")
            process.sleep(50)
            Ok(status_response())
          }
        }
      }),
    )

  assert service.query(handle, types.Status)
    == Error(types.QueryError(types.QueryTimeout, "query timed out"))
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
