import gleam/list
import scherzo/orchestrator/effects/interpreter
import scherzo/orchestrator/transition
import scherzo/orchestrator/transition_types

pub type RunResult(shell) {
  RunResult(
    state: transition_types.State,
    shell: interpreter.ShellState(shell),
    exhausted: Bool,
  )
}

pub fn run(
  state state: transition_types.State,
  shell shell: interpreter.ShellState(shell),
  messages messages: List(transition_types.Message),
  max_messages max_messages: Int,
) -> RunResult(shell) {
  run_loop(state, shell, messages, max_messages)
}

fn run_loop(
  state: transition_types.State,
  shell: interpreter.ShellState(shell),
  messages: List(transition_types.Message),
  remaining: Int,
) -> RunResult(shell) {
  case messages {
    [] -> RunResult(state: state, shell: shell, exhausted: False)
    [message, ..rest] ->
      case remaining <= 0 {
        True -> RunResult(state: state, shell: shell, exhausted: True)
        False -> {
          let transition_types.Outcome(state: next_state, effects: effects) =
            transition.handle(message, state)
          let interpreter.ApplyResult(
            shell: shell,
            follow_up_messages: follow_up_messages,
          ) = interpreter.apply(shell, effects)
          run_loop(
            next_state,
            shell,
            list.append(rest, follow_up_messages),
            remaining - 1,
          )
        }
      }
  }
}
