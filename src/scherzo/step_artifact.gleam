import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/agent/types as agent_types
import scherzo/config/types as config_types
import scherzo/log
import scherzo/result_artifact
import scherzo/template
import scherzo/workflow_dag

pub type StepStatus {
  StepSucceeded
  StepFailed
}

pub type StepArtifact {
  StepArtifact(
    step_id: String,
    status: StepStatus,
    final_response: Option(String),
    exit_code: Option(Int),
    command: Option(String),
    duration_ms: Option(Int),
    diagnostic_path: Option(String),
    stdout: String,
    stderr: String,
    timed_out: Bool,
    final_response_truncated: Bool,
    stdout_truncated: Bool,
    stderr_truncated: Bool,
    summary_text: String,
  )
}

pub fn status_to_string(status: StepStatus) -> String {
  case status {
    StepSucceeded -> "success"
    StepFailed -> "failure"
  }
}

pub fn status_from_exit(exit_code: Int, timed_out: Bool) -> StepStatus {
  case exit_code == 0 && !timed_out {
    True -> StepSucceeded
    False -> StepFailed
  }
}

pub fn succeeded(status: StepStatus) -> Bool {
  case status {
    StepSucceeded -> True
    StepFailed -> False
  }
}

pub fn from_agent_success(
  step_id: String,
  success: agent_types.WorkerSuccess,
  secrets: List(String),
  limits: config_types.ArtifactLimits,
) -> StepArtifact {
  let #(final_response, final_truncated) =
    cap_optional(
      success.result.final_response,
      secrets,
      limits.template_field_max_chars,
    )
  let summary = step_id <> " success agent"
  StepArtifact(
    step_id: step_id,
    status: StepSucceeded,
    final_response: final_response,
    exit_code: None,
    command: None,
    duration_ms: None,
    diagnostic_path: None,
    stdout: "",
    stderr: "",
    timed_out: False,
    final_response_truncated: final_truncated || success.result.truncated,
    stdout_truncated: False,
    stderr_truncated: False,
    summary_text: summary,
  )
}

pub fn from_command_result(
  step_id: String,
  exit_code: Int,
  stdout: String,
  stderr: String,
  timed_out: Bool,
  secrets: List(String),
  limits: config_types.ArtifactLimits,
) -> StepArtifact {
  from_command_result_with_metadata(
    step_id,
    None,
    exit_code,
    None,
    None,
    stdout,
    stderr,
    timed_out,
    secrets,
    limits,
    False,
    False,
  )
}

pub fn from_command_result_with_truncation(
  step_id: String,
  exit_code: Int,
  stdout: String,
  stderr: String,
  timed_out: Bool,
  secrets: List(String),
  limits: config_types.ArtifactLimits,
  stdout_already_truncated: Bool,
  stderr_already_truncated: Bool,
) -> StepArtifact {
  from_command_result_with_metadata(
    step_id,
    None,
    exit_code,
    None,
    None,
    stdout,
    stderr,
    timed_out,
    secrets,
    limits,
    stdout_already_truncated,
    stderr_already_truncated,
  )
}

pub fn from_command_result_with_metadata(
  step_id: String,
  command: Option(String),
  exit_code: Int,
  duration_ms: Option(Int),
  diagnostic_path: Option(String),
  stdout: String,
  stderr: String,
  timed_out: Bool,
  secrets: List(String),
  limits: config_types.ArtifactLimits,
  stdout_already_truncated: Bool,
  stderr_already_truncated: Bool,
) -> StepArtifact {
  let #(stdout, stdout_truncated) =
    cap_with_truncation(
      stdout,
      secrets,
      limits.command_stream_max_chars,
      stdout_already_truncated,
    )
  let #(stderr, stderr_truncated) =
    cap_with_truncation(
      stderr,
      secrets,
      limits.command_stream_max_chars,
      stderr_already_truncated,
    )
  let #(command, _) =
    cap_optional(command, secrets, limits.template_field_max_chars)
  let status = status_from_exit(exit_code, timed_out)
  let status_text = status_to_string(status)
  let summary =
    step_id
    <> " "
    <> status_text
    <> " command exit_code="
    <> int_to_string(exit_code)
    <> case timed_out {
      True -> " timed_out=true"
      False -> ""
    }
  StepArtifact(
    step_id: step_id,
    status: status,
    final_response: None,
    exit_code: Some(exit_code),
    command: command,
    duration_ms: duration_ms,
    diagnostic_path: diagnostic_path,
    stdout: stdout,
    stderr: stderr,
    timed_out: timed_out,
    final_response_truncated: False,
    stdout_truncated: stdout_truncated,
    stderr_truncated: stderr_truncated,
    summary_text: summary,
  )
}

pub fn command_failure_summary(artifact: StepArtifact) -> Option(String) {
  case artifact.status {
    StepSucceeded -> None
    StepFailed -> {
      let head =
        "command step failed: step="
        <> artifact.step_id
        <> command_inline(artifact.command)
        <> exit_inline(artifact.exit_code)
        <> duration_inline(artifact.duration_ms)
        <> timeout_inline(artifact.timed_out)
      Some(
        head
        <> stream_inline("stdout", artifact.stdout, artifact.stdout_truncated)
        <> stream_inline("stderr", artifact.stderr, artifact.stderr_truncated)
        <> artifact_inline(artifact.diagnostic_path),
      )
    }
  }
}

pub fn command_failure_details(artifact: StepArtifact) -> String {
  let metadata =
    "step: "
    <> artifact.step_id
    <> "\ncommand: "
    <> option_string(artifact.command, "<not recorded>")
    <> "\n"
    <> exit_detail(artifact.exit_code)
    <> "\n"
    <> duration_detail(artifact.duration_ms)
    <> timeout_detail(artifact.timed_out)
    <> truncation_detail(artifact)
  let stdout = stream_detail("stdout", artifact.stdout)
  let stderr = stream_detail("stderr", artifact.stderr)
  metadata <> stdout <> stderr
}

fn command_inline(command: Option(String)) -> String {
  case command {
    Some(command) -> " command=\"" <> inline(command, 80) <> "\""
    None -> " command=<not recorded>"
  }
}

fn exit_inline(exit_code: Option(Int)) -> String {
  case exit_code {
    Some(exit_code) -> " exit_code=" <> int.to_string(exit_code)
    None -> " exit_status=<not recorded>"
  }
}

fn duration_inline(duration_ms: Option(Int)) -> String {
  case duration_ms {
    Some(duration_ms) -> " duration_ms=" <> int.to_string(duration_ms)
    None -> ""
  }
}

fn timeout_inline(timed_out: Bool) -> String {
  case timed_out {
    True -> " timed_out=true"
    False -> ""
  }
}

fn stream_inline(label: String, value: String, truncated: Bool) -> String {
  case value == "" {
    True ->
      case truncated {
        True -> " " <> label <> "=<truncated empty excerpt>"
        False -> ""
      }
    False -> {
      let suffix = case truncated {
        True -> " [truncated]"
        False -> ""
      }
      " " <> label <> "=\"" <> inline(value, 60) <> suffix <> "\""
    }
  }
}

fn artifact_inline(path: Option(String)) -> String {
  case path {
    Some(path) -> " artifact=" <> path
    None -> ""
  }
}

fn inline(value: String, max_chars: Int) -> String {
  let compact =
    value
    |> string.replace(each: "\r\n", with: "\n")
    |> string.replace(each: "\n", with: " ⏎ ")
  case string.length(compact) > max_chars {
    True -> string.slice(compact, 0, max_chars) <> "…"
    False -> compact
  }
}

fn exit_detail(exit_code: Option(Int)) -> String {
  "exit_code: "
  <> case exit_code {
    Some(exit_code) -> int.to_string(exit_code)
    None -> "<not recorded>"
  }
}

fn duration_detail(duration_ms: Option(Int)) -> String {
  "duration_ms: "
  <> case duration_ms {
    Some(duration_ms) -> int.to_string(duration_ms)
    None -> "<not recorded>"
  }
}

fn timeout_detail(timed_out: Bool) -> String {
  case timed_out {
    True -> "\ntimed_out: true"
    False -> ""
  }
}

fn truncation_detail(artifact: StepArtifact) -> String {
  let stdout =
    stream_truncation_detail(
      "stdout",
      artifact.stdout_truncated,
      artifact.diagnostic_path,
    )
  let stderr =
    stream_truncation_detail(
      "stderr",
      artifact.stderr_truncated,
      artifact.diagnostic_path,
    )
  stdout <> stderr
}

fn stream_truncation_detail(
  label: String,
  truncated: Bool,
  diagnostic_path: Option(String),
) -> String {
  case truncated {
    False -> ""
    True ->
      "\n"
      <> label
      <> "_truncated: true"
      <> case diagnostic_path {
        Some(path) -> " (full retained artifact: " <> path <> ")"
        None -> " (full retained artifact unavailable)"
      }
  }
}

fn stream_detail(label: String, value: String) -> String {
  case value == "" {
    True -> "\n" <> label <> ": <empty>"
    False -> "\n" <> label <> ":\n" <> value
  }
}

pub fn to_template_locals(
  artifacts: Dict(String, StepArtifact),
) -> List(#(String, template.Value)) {
  dict.to_list(artifacts)
  |> list.flat_map(fn(entry) {
    let #(step_id, artifact) = entry
    artifact_locals(step_id, artifact)
  })
}

pub fn workflow_result_artifact(
  dag: workflow_dag.WorkflowDag,
  artifacts: Dict(String, StepArtifact),
  limits: config_types.ArtifactLimits,
) -> result_artifact.ResultArtifact {
  let primary = primary_text(dag, artifacts)
  let summary = summary_for_dag(dag.steps, artifacts, [])
  let text = case primary, summary {
    "", "" -> ""
    _, "" -> primary
    "", _ -> "Workflow step summary:\n" <> summary
    _, _ -> primary <> "\n\nWorkflow step summary:\n" <> summary
  }
  let truncated =
    string.length(text) > limits.workflow_summary_max_chars
    || any_truncated(dict.values(artifacts))
  case text == "" {
    True ->
      result_artifact.ResultArtifact(
        final_response: None,
        truncated: truncated,
        source: "workflow_dag",
      )
    False ->
      result_artifact.ResultArtifact(
        final_response: Some(log.truncate(
          text,
          limits.workflow_summary_max_chars,
        )),
        truncated: truncated,
        source: "workflow_dag",
      )
  }
}

fn artifact_locals(
  step_id: String,
  artifact: StepArtifact,
) -> List(#(String, template.Value)) {
  let prefix = "steps." <> step_id <> "."
  [
    #(prefix <> "status", template.VString(status_to_string(artifact.status))),
    #(prefix <> "final_response", option_string_value(artifact.final_response)),
    #(prefix <> "exit_code", option_int_value(artifact.exit_code)),
    #(prefix <> "command", option_string_value(artifact.command)),
    #(prefix <> "duration_ms", option_int_value(artifact.duration_ms)),
    #(
      prefix <> "diagnostic_path",
      option_string_value(artifact.diagnostic_path),
    ),
    #(prefix <> "stdout", template.VString(artifact.stdout)),
    #(prefix <> "stderr", template.VString(artifact.stderr)),
    #(prefix <> "timed_out", template.VBool(artifact.timed_out)),
    #(
      prefix <> "final_response_truncated",
      template.VBool(artifact.final_response_truncated),
    ),
    #(prefix <> "stdout_truncated", template.VBool(artifact.stdout_truncated)),
    #(prefix <> "stderr_truncated", template.VBool(artifact.stderr_truncated)),
    #(prefix <> "summary", template.VString(artifact.summary_text)),
  ]
}

fn primary_text(
  dag: workflow_dag.WorkflowDag,
  artifacts: Dict(String, StepArtifact),
) -> String {
  case workflow_dag.terminal_step(dag) {
    None -> ""
    Some(step) ->
      case dict.get(artifacts, step.id) {
        Error(_) -> ""
        Ok(artifact) -> artifact_primary_text(artifact)
      }
  }
}

fn artifact_primary_text(artifact: StepArtifact) -> String {
  case artifact.final_response {
    Some(text) -> text
    None ->
      case artifact.stdout != "" {
        True -> artifact.stdout
        False -> artifact.stderr
      }
  }
}

fn summary_for_dag(
  steps: List(workflow_dag.WorkflowStep),
  artifacts: Dict(String, StepArtifact),
  acc: List(String),
) -> String {
  case steps {
    [] -> string.join(list.reverse(acc), with: "\n")
    [step, ..rest] -> {
      let acc = case dict.get(artifacts, step.id) {
        Ok(artifact) -> [artifact.summary_text, ..acc]
        Error(_) -> acc
      }
      summary_for_dag(rest, artifacts, acc)
    }
  }
}

fn any_truncated(artifacts: List(StepArtifact)) -> Bool {
  case artifacts {
    [] -> False
    [artifact, ..rest] ->
      artifact.final_response_truncated
      || artifact.stdout_truncated
      || artifact.stderr_truncated
      || any_truncated(rest)
  }
}

fn cap_optional(
  value: Option(String),
  secrets: List(String),
  max_chars: Int,
) -> #(Option(String), Bool) {
  case value {
    None -> #(None, False)
    Some(value) -> {
      let #(value, truncated) = cap(value, secrets, max_chars)
      #(Some(value), truncated)
    }
  }
}

fn cap(
  value: String,
  secrets: List(String),
  max_chars: Int,
) -> #(String, Bool) {
  cap_with_truncation(value, secrets, max_chars, False)
}

fn cap_with_truncation(
  value: String,
  secrets: List(String),
  max_chars: Int,
  already_truncated: Bool,
) -> #(String, Bool) {
  let redacted = log.redact("step_artifact", value, secrets)
  let truncated = already_truncated || string.length(redacted) > max_chars
  case truncated {
    True -> #(string.slice(redacted, 0, max_chars) <> "...", True)
    False -> #(redacted, False)
  }
}

fn option_string(value: Option(String), default: String) -> String {
  case value {
    Some(value) -> value
    None -> default
  }
}

fn option_string_value(value: Option(String)) -> template.Value {
  case value {
    Some(value) -> template.VString(value)
    None -> template.VNil
  }
}

fn option_int_value(value: Option(Int)) -> template.Value {
  case value {
    Some(value) -> template.VInt(value)
    None -> template.VNil
  }
}

@external(erlang, "erlang", "integer_to_binary")
fn int_to_string(value: Int) -> String
