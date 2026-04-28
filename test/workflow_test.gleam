import gleam/option.{None, Some}
import scherzo/workflow
import simplifile
import yay

fn reset_dir(path: String) -> Nil {
  let _ = simplifile.delete(path)
  let assert Ok(Nil) = simplifile.create_directory_all(path)
  Nil
}

pub fn missing_workflow_file_test() {
  let assert Error(_) = workflow.load(Some("test/tmp/missing-workflow.md"))
}

pub fn no_front_matter_uses_empty_map_test() {
  let assert Ok(definition) = workflow.parse("  Hello issue  \n")
  let assert yay.NodeMap([]) = definition.config
  assert definition.prompt_template == "Hello issue"
}

pub fn valid_map_front_matter_test() {
  let content = "---\ntracker:\n  kind: linear\n---\nPrompt body\n"
  let assert Ok(definition) = workflow.parse(content)
  let assert yay.NodeMap(_) = definition.config
  assert definition.prompt_template == "Prompt body"
}

pub fn invalid_yaml_returns_parse_error_test() {
  let assert Error(_) = workflow.parse("---\n: bad\n---\nPrompt")
}

pub fn non_map_yaml_returns_error_test() {
  let assert Error(_) = workflow.parse("---\n- one\n---\nPrompt")
}

pub fn missing_closing_delimiter_returns_error_test() {
  let assert Error(_) = workflow.parse("---\ntracker:\n  kind: linear\nPrompt")
}

pub fn explicit_path_selection_test() {
  let dir = "test/tmp/workflow"
  reset_dir(dir)
  let path = dir <> "/WORKFLOW.md"
  let assert Ok(Nil) = simplifile.write(path, "Prompt")
  let assert Ok(definition) = workflow.load(Some(path))
  assert definition.prompt_template == "Prompt"
  assert workflow.choose_path(None) == "WORKFLOW.md"
}
