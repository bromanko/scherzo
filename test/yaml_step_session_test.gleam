import gleam/string
import scherzo/orchestrator/yaml_step_session

pub fn repeated_step_attempts_have_unique_session_ids_test() {
  let first = yaml_step_session.id("run-1", "build", 1)
  let second = yaml_step_session.id("run-1", "build", 2)

  assert first != second
  assert string.contains(first, "run-1")
  assert string.contains(first, "build")
  assert string.contains(first, "a1")
  assert string.contains(second, "run-1")
  assert string.contains(second, "build")
  assert string.contains(second, "a2")
}
