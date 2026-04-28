import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/domain
import scherzo/error
import simplifile
import yay

pub fn choose_path(explicit: Option(String)) -> String {
  case explicit {
    Some(path) -> path
    None -> "WORKFLOW.md"
  }
}

pub fn load(
  explicit: Option(String),
) -> Result(domain.WorkflowDefinition, error.WorkflowError) {
  let path = choose_path(explicit)
  case simplifile.read(path) {
    Ok(content) -> parse(content)
    Error(_) -> Error(error.MissingWorkflowFile(path))
  }
}

pub fn parse(
  content: String,
) -> Result(domain.WorkflowDefinition, error.WorkflowError) {
  case string.starts_with(content, "---\n") {
    False ->
      Ok(domain.WorkflowDefinition(
        config: yay.NodeMap([]),
        prompt_template: string.trim(content),
      ))
    True -> parse_with_front_matter(string.drop_start(content, 4))
  }
}

fn parse_with_front_matter(
  content: String,
) -> Result(domain.WorkflowDefinition, error.WorkflowError) {
  case string.split_once(content, on: "\n---\n") {
    Error(_) -> Error(error.WorkflowParseError("missing closing ---"))
    Ok(#(front_matter, body)) -> {
      case yay.parse_string(front_matter) {
        Error(_) -> Error(error.WorkflowParseError("YAML parse error"))
        Ok([document]) ->
          case yay.document_root(document) {
            yay.NodeMap(_) as map ->
              Ok(domain.WorkflowDefinition(
                config: map,
                prompt_template: string.trim(body),
              ))
            yay.NodeNil ->
              Ok(domain.WorkflowDefinition(
                config: yay.NodeMap([]),
                prompt_template: string.trim(body),
              ))
            _ -> Error(error.WorkflowFrontMatterNotMap)
          }
        Ok(_) -> Error(error.WorkflowParseError("expected one YAML document"))
      }
    }
  }
}
