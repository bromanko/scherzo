import gleam/int
import gleam/option.{None, Some}
import scherzo/agent/runner
import scherzo/domain
import scherzo/log

pub fn success_comment(
  issue: domain.Issue,
  success: runner.WorkerSuccess,
  run_id: String,
  include_result: Bool,
  secrets: List(String),
) -> String {
  let header =
    "Scherzo completed run " <> run_id <> " for " <> issue.identifier <> "."
  let body = case include_result {
    True ->
      header <> "\n\n" <> result_section(success) <> "\n\n" <> metadata(success)
    False -> header <> "\n\n" <> metadata(success)
  }
  log.redact("comment_body", body, secrets)
}

fn result_section(success: runner.WorkerSuccess) -> String {
  let result_text = case success.result.final_response {
    Some(text) -> text
    None -> "_No assistant result text was captured._"
  }
  let truncation_note = case success.result.truncated {
    True -> "\n\n_Result truncated by Scherzo._"
    False -> ""
  }
  "Result:\n" <> result_text <> truncation_note
}

fn metadata(success: runner.WorkerSuccess) -> String {
  "Metadata:\n"
  <> "- classification: "
  <> classification_to_string(success.final_classification)
  <> "\n- turns: "
  <> int.to_string(success.turns)
  <> "\n- tokens: input="
  <> int.to_string(success.tokens.input)
  <> " output="
  <> int.to_string(success.tokens.output)
  <> " cache_read="
  <> int.to_string(success.tokens.cache_read)
  <> " cache_write="
  <> int.to_string(success.tokens.cache_write)
  <> " total="
  <> int.to_string(success.tokens.total)
}

fn classification_to_string(
  classification: runner.FinalClassification,
) -> String {
  case classification {
    runner.FinalActive -> "active"
    runner.FinalTerminal -> "terminal"
    runner.FinalNonActive -> "non_active"
  }
}
