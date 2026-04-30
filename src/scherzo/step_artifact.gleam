import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/agent/runner
import scherzo/domain
import scherzo/log
import scherzo/template
import scherzo/workflow_dag

pub type StepArtifact {
  StepArtifact(
    step_id: String,
    status: String,
    final_response: Option(String),
    exit_code: Option(Int),
    stdout: String,
    stderr: String,
    timed_out: Bool,
    final_response_truncated: Bool,
    stdout_truncated: Bool,
    stderr_truncated: Bool,
    summary_text: String,
  )
}

pub fn from_agent_success(
  step_id: String,
  success: runner.WorkerSuccess,
  secrets: List(String),
  limits: domain.ArtifactLimits,
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
    status: "success",
    final_response: final_response,
    exit_code: None,
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
  limits: domain.ArtifactLimits,
) -> StepArtifact {
  from_command_result_with_truncation(
    step_id,
    exit_code,
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
  limits: domain.ArtifactLimits,
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
  let status = case exit_code == 0 && !timed_out {
    True -> "success"
    False -> "failure"
  }
  let summary =
    step_id
    <> " "
    <> status
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
    stdout: stdout,
    stderr: stderr,
    timed_out: timed_out,
    final_response_truncated: False,
    stdout_truncated: stdout_truncated,
    stderr_truncated: stderr_truncated,
    summary_text: summary,
  )
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
  limits: domain.ArtifactLimits,
) -> domain.ResultArtifact {
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
      domain.ResultArtifact(
        final_response: None,
        truncated: truncated,
        source: "workflow_dag",
      )
    False ->
      domain.ResultArtifact(
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
    #(prefix <> "status", template.VString(artifact.status)),
    #(prefix <> "final_response", option_string_value(artifact.final_response)),
    #(prefix <> "exit_code", option_int_value(artifact.exit_code)),
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

fn cap(value: String, secrets: List(String), max_chars: Int) -> #(String, Bool) {
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
