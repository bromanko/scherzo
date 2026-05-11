import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/log
import scherzo/port

pub const domain_invalid_code = "review_lane_draft_domain_invalid"

const validator_timeout_ms = 30_000

pub type ValidatedReviewLaneDraft {
  ValidatedReviewLaneDraft(stdout: String)
}

pub type ReviewLaneDraftValidationError {
  ReviewLaneDraftValidationRejected(message: String)
  ReviewLaneDraftValidationCrashed(message: String)
}

pub fn validate_captured_json(
  payload_json: String,
  lane_id: String,
  secrets: List(String),
) -> Result(ValidatedReviewLaneDraft, ReviewLaneDraftValidationError) {
  let command =
    "python3 scripts/scherzo-review validate-lane-draft --lane "
    <> shell_quote(lane_id)
    <> " --draft-json -"
  case port.start(command, ".") {
    Error(error) ->
      Error(ReviewLaneDraftValidationCrashed(port.port_error_to_string(error)))
    Ok(process) -> {
      case port.send_line(process, payload_json) {
        Error(error) -> {
          let _cleanup = port.terminate(process)
          Error(
            ReviewLaneDraftValidationCrashed(port.port_error_to_string(error)),
          )
        }
        Ok(Nil) -> read_validator(process, "", secrets)
      }
    }
  }
}

pub fn error_message(error: ReviewLaneDraftValidationError) -> String {
  case error {
    ReviewLaneDraftValidationRejected(message) -> message
    ReviewLaneDraftValidationCrashed(message) -> message
  }
}

fn read_validator(
  process: port.Process,
  stdout: String,
  secrets: List(String),
) -> Result(ValidatedReviewLaneDraft, ReviewLaneDraftValidationError) {
  case port.read_stdout_line(process, validator_timeout_ms) {
    Ok(line) -> read_validator(process, stdout <> line <> "\n", secrets)
    Error(port.ProcessExited(status)) ->
      finish_validator(process, status, stdout, secrets)
    Error(port.ReadTimeout) -> {
      let _cleanup = port.terminate(process)
      Error(ReviewLaneDraftValidationCrashed("validator timed out"))
    }
    Error(error) -> {
      let stderr = read_diagnostics(process)
      let _cleanup = port.terminate(process)
      Error(
        ReviewLaneDraftValidationCrashed(redact(
          stderr <> port.port_error_to_string(error),
          secrets,
        )),
      )
    }
  }
}

fn finish_validator(
  process: port.Process,
  status: Int,
  stdout: String,
  secrets: List(String),
) -> Result(ValidatedReviewLaneDraft, ReviewLaneDraftValidationError) {
  let stderr = read_diagnostics(process)
  let _cleanup = port.terminate(process)
  case status == 0 {
    True -> Ok(ValidatedReviewLaneDraft(stdout: redact(stdout, secrets)))
    False ->
      Error(
        ReviewLaneDraftValidationRejected(redact(
          validation_failure_message(stdout, stderr),
          secrets,
        )),
      )
  }
}

fn validation_failure_message(stdout: String, stderr: String) -> String {
  case first_non_empty([stderr, stdout]) {
    Some(message) -> message
    None -> "deterministic review lane draft validation failed"
  }
}

fn read_diagnostics(process: port.Process) -> String {
  case port.read_diagnostics(process) {
    Ok(stderr) -> stderr
    Error(error) -> port.port_error_to_string(error)
  }
}

fn redact(value: String, secrets: List(String)) -> String {
  log.redact("review_lane_draft_validator", value, secrets)
  |> log.truncate(1000)
}

fn shell_quote(value: String) -> String {
  "'" <> string.replace(value, "'", "'\\''") <> "'"
}

fn first_non_empty(values: List(String)) -> Option(String) {
  case values {
    [] -> None
    [value, ..rest] ->
      case string.trim(value) == "" {
        True -> first_non_empty(rest)
        False -> Some(value)
      }
  }
}
