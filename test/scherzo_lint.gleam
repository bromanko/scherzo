import gleam/io
import glinter
import scherzo_lint/high_signal/agent_pi_guard
import scherzo_lint/rules/public_function_labels

pub fn main() {
  case agent_pi_guard.run() {
    Ok(Nil) ->
      glinter.run(extra_rules: [
        public_function_labels.rule(),
      ])
    Error(error) -> {
      io.println_error(agent_pi_guard.error_message(error))
      halt(1)
    }
  }
}

@external(erlang, "erlang", "halt")
fn halt(code: Int) -> Nil
