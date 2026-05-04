import gleam/json
import gleam/option.{type Option, None, Some}
import scherzo/hash
import scherzo/model_config
import scherzo/workflow_dag

pub type FingerprintError {
  PromptFileReadFailed(path: String)
  UnsupportedWorkflowShape(reason: String)
}

pub fn fingerprint(
  dag: workflow_dag.WorkflowDag,
) -> Result(String, FingerprintError) {
  Ok(hash.sha256_hex(canonical_input(dag)))
}

pub fn canonical_input(dag: workflow_dag.WorkflowDag) -> String {
  dag_to_json(dag) |> json.to_string
}

fn dag_to_json(dag: workflow_dag.WorkflowDag) -> json.Json {
  json.object([
    #("id", json.string(dag.id)),
    #("description", option_string_to_json(dag.description)),
    #("max_parallel_steps", json.int(dag.max_parallel_steps)),
    #("steps", json.array(dag.steps, of: step_to_json)),
  ])
}

fn step_to_json(step: workflow_dag.WorkflowStep) -> json.Json {
  json.object([
    #("id", json.string(step.id)),
    #("depends_on", json.array(step.depends_on, of: json.string)),
    #("kind", kind_to_json(step.kind)),
    #("workspace", workspace_to_json(step.workspace)),
    #("on_failure", json.string(failure_policy_to_string(step.on_failure))),
    #("model_settings", model_settings_to_json(step.model_settings)),
  ])
}

fn kind_to_json(kind: workflow_dag.StepKind) -> json.Json {
  case kind {
    workflow_dag.CommandStep(run, timeout_ms) ->
      json.object([
        #("type", json.string("command")),
        #("run", json.string(run)),
        #("timeout_ms", option_int_to_json(timeout_ms)),
      ])
    workflow_dag.AgentStep(prompt_ref) ->
      json.object([
        #("type", json.string("agent")),
        #("prompt", prompt_ref_to_json(prompt_ref)),
      ])
  }
}

fn prompt_ref_to_json(prompt_ref: workflow_dag.PromptRef) -> json.Json {
  case prompt_ref {
    workflow_dag.PromptInline(prompt) ->
      json.object([
        #("type", json.string("inline")),
        #("text", json.string(prompt)),
      ])
    workflow_dag.PromptFile(path) ->
      json.object([#("type", json.string("file")), #("path", json.string(path))])
  }
}

fn workspace_to_json(workspace: workflow_dag.WorkspaceRef) -> json.Json {
  json.object([
    #("name", json.string(workspace.name)),
    #("from", option_string_to_json(workspace.from)),
  ])
}

fn failure_policy_to_string(policy: workflow_dag.FailurePolicy) -> String {
  case policy {
    workflow_dag.FailWorkflow -> "fail"
    workflow_dag.ContinueWorkflow -> "continue"
  }
}

fn model_settings_to_json(settings: model_config.Settings) -> json.Json {
  json.object([
    #("model", option_string_to_json(settings.model)),
    #("thinking", option_thinking_to_json(settings.thinking)),
  ])
}

fn option_thinking_to_json(
  value: Option(model_config.ThinkingLevel),
) -> json.Json {
  case value {
    Some(level) -> json.string(model_config.thinking_to_string(level))
    None -> json.null()
  }
}

fn option_string_to_json(value: Option(String)) -> json.Json {
  case value {
    Some(value) -> json.string(value)
    None -> json.null()
  }
}

fn option_int_to_json(value: Option(Int)) -> json.Json {
  case value {
    Some(value) -> json.int(value)
    None -> json.null()
  }
}
