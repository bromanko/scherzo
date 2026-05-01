import gleam/erlang/process
import gleam/option.{None}
import scherzo/domain
import scherzo/orchestrator/effect_runner

fn hooks() -> domain.HooksConfig {
  domain.HooksConfig(
    after_create: None,
    before_run: None,
    after_run: None,
    before_remove: None,
    timeout_ms: 1000,
  )
}

fn start_runner(
  completions: process.Subject(effect_runner.Completion),
) -> effect_runner.Handle {
  let assert Ok(handle) =
    effect_runner.start(
      effect_runner.Dependencies(max_concurrent: 1, notify: fn(completion) {
        process.send(completions, completion)
      }),
    )
  handle
}

pub fn effect_runner_runs_successful_effect_once_test() {
  let completions = process.new_subject()
  let started = process.new_subject()
  let runner = start_runner(completions)

  effect_runner.enqueue(
    runner,
    effect_runner.CleanupWorkspace(
      root: "root",
      workspace_path: "workspace",
      hooks: hooks(),
      cleanup: fn(_, _, _) {
        process.send(started, "cleanup_started")
        Ok(Nil)
      },
    ),
  )

  assert process.receive(started, within: 1000) == Ok("cleanup_started")
  let assert Ok(effect_runner.Finished(
    _,
    effect_runner.CleanupFinished("workspace", Ok(Nil)),
  )) = process.receive(completions, within: 1000)
  case process.receive(completions, within: 50) {
    Error(_) -> Nil
    Ok(_) -> panic as "duplicate completion"
  }
  assert effect_runner.shutdown(runner, 1000) == Ok(Nil)
}

pub fn effect_runner_reports_crash_and_drains_queue_test() {
  let completions = process.new_subject()
  let started = process.new_subject()
  let runner = start_runner(completions)

  effect_runner.enqueue(
    runner,
    effect_runner.CleanupWorkspace(
      root: "root",
      workspace_path: "first",
      hooks: hooks(),
      cleanup: fn(_, _, _) {
        process.send(started, "first_started")
        panic as "boom"
      },
    ),
  )
  effect_runner.enqueue(
    runner,
    effect_runner.CleanupWorkspace(
      root: "root",
      workspace_path: "second",
      hooks: hooks(),
      cleanup: fn(_, _, _) {
        process.send(started, "second_started")
        Ok(Nil)
      },
    ),
  )

  assert process.receive(started, within: 1000) == Ok("first_started")
  let assert Ok(effect_runner.Crashed(_, crashed_effect, reason)) =
    process.receive(completions, within: 1000)
  assert reason == "side_effect_crashed"
  case crashed_effect {
    effect_runner.CleanupWorkspace(_, "first", _, _) -> Nil
    _ -> panic as "unexpected crashed effect"
  }
  assert process.receive(started, within: 1000) == Ok("second_started")
  let assert Ok(effect_runner.Finished(
    _,
    effect_runner.CleanupFinished("second", Ok(Nil)),
  )) = process.receive(completions, within: 1000)
  assert effect_runner.shutdown(runner, 1000) == Ok(Nil)
}
