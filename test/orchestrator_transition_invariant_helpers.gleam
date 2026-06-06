import gleam/list
import scherzo/orchestrator/effects/interpreter
import scherzo/orchestrator/transition
import scherzo/orchestrator/transition_invariants
import scherzo/orchestrator/transition_runner
import scherzo/orchestrator/transition_types
import scherzo/runtime/state as orchestrator_state

pub fn assert_valid_state(state: transition_types.State) -> Nil {
  case transition_invariants.check(state) {
    Ok(Nil) -> Nil
    Error(errors) -> {
      let report = transition_invariants.format_errors(errors)
      assert report == ""
      Nil
    }
  }
}

pub fn assert_runtime_error(
  runtime: orchestrator_state.RuntimeState,
  code: String,
) -> Nil {
  case transition_invariants.check_runtime(runtime) {
    Ok(Nil) -> {
      assert code == "expected invariant error"
      Nil
    }
    Error(errors) -> assert_error_code(errors, code)
  }
}

pub fn assert_state_error(state: transition_types.State, code: String) -> Nil {
  case transition_invariants.check(state) {
    Ok(Nil) -> {
      assert code == "expected invariant error"
      Nil
    }
    Error(errors) -> assert_error_code(errors, code)
  }
}

pub fn handle_and_assert(
  message: transition_types.Message,
  state: transition_types.State,
) -> transition_types.Outcome {
  let outcome = transition.handle(message, state)
  assert_valid_state(outcome.state)
  outcome
}

pub fn run_and_assert(
  state state: transition_types.State,
  shell shell: interpreter.ShellState(shell),
  messages messages: List(transition_types.Message),
  max_messages max_messages: Int,
) -> transition_runner.RunResult(shell) {
  let result =
    transition_runner.run(
      state: state,
      shell: shell,
      messages: messages,
      max_messages: max_messages,
    )
  assert_valid_state(result.state)
  result
}

fn assert_error_code(
  errors: List(transition_invariants.InvariantError),
  code: String,
) -> Nil {
  let matched =
    list.any(errors, fn(error) {
      transition_invariants.error_code(error) == code
    })
  case matched {
    True -> Nil
    False -> {
      let report = transition_invariants.format_errors(errors)
      assert report == code
      Nil
    }
  }
}
