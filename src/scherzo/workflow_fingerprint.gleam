import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/config/types as config_types
import scherzo/hash
import scherzo/model_config
import scherzo/structured_output_source
import scherzo/workflow_dag
import scherzo/workspace_profile

pub type FingerprintError {
  PromptFileReadFailed(path: String)
  UnsupportedWorkflowShape(reason: String)
  WorkspaceProfileUnavailable(profile_name: String)
}

pub fn fingerprint(
  dag: workflow_dag.WorkflowDag,
) -> Result(String, FingerprintError) {
  Ok(for_dag(dag.id, dag))
}

pub fn fingerprint_for_execution(
  dag: workflow_dag.WorkflowDag,
  orchestrator: config_types.OrchestratorConfig,
) -> Result(String, FingerprintError) {
  for_execution(dag.id, dag, orchestrator)
}

pub fn for_dag(workflow_id: String, dag: workflow_dag.WorkflowDag) -> String {
  hash.sha256_hex(canonical_input_for(workflow_id, dag))
}

pub fn for_execution(
  workflow_id: String,
  dag: workflow_dag.WorkflowDag,
  orchestrator: config_types.OrchestratorConfig,
) -> Result(String, FingerprintError) {
  use profile <- result.try(
    workspace_profile.resolve(dag, orchestrator)
    |> result.map_error(workspace_profile_error_to_fingerprint_error),
  )
  Ok(for_execution_profile_options(
    workflow_id,
    dag,
    profile,
    orchestrator.artifact_limits,
    orchestrator.model_settings,
  ))
}

pub fn for_execution_options(
  workflow_id: String,
  dag: workflow_dag.WorkflowDag,
  dag_hooks: config_types.DagHooksConfig,
  artifact_limits: config_types.ArtifactLimits,
  model_settings: model_config.Settings,
) -> String {
  let profile =
    config_types.WorkspaceHookProfile(
      name: "default",
      hooks: Some(dag_hooks),
      driver: None,
      source: config_types.LegacyWorkspaceHooks,
    )
  for_execution_profile_options(
    workflow_id,
    dag,
    profile,
    artifact_limits,
    model_settings,
  )
}

pub fn for_execution_profile_options(
  workflow_id: String,
  dag: workflow_dag.WorkflowDag,
  profile: config_types.WorkspaceHookProfile,
  artifact_limits: config_types.ArtifactLimits,
  model_settings: model_config.Settings,
) -> String {
  hash.sha256_hex(canonical_execution_input_for_profile(
    workflow_id,
    dag,
    profile,
    artifact_limits,
    model_settings,
  ))
}

pub fn canonical_input(dag: workflow_dag.WorkflowDag) -> String {
  canonical_input_for(dag.id, dag)
}

pub fn canonical_input_for(
  workflow_id: String,
  dag: workflow_dag.WorkflowDag,
) -> String {
  dag_to_json(workflow_id, dag) |> json.to_string
}

pub fn canonical_execution_input_for(
  workflow_id: String,
  dag: workflow_dag.WorkflowDag,
  dag_hooks: config_types.DagHooksConfig,
  artifact_limits: config_types.ArtifactLimits,
  model_settings: model_config.Settings,
) -> String {
  let profile =
    config_types.WorkspaceHookProfile(
      name: "default",
      hooks: Some(dag_hooks),
      driver: None,
      source: config_types.LegacyWorkspaceHooks,
    )
  canonical_execution_input_for_profile(
    workflow_id,
    dag,
    profile,
    artifact_limits,
    model_settings,
  )
}

pub fn canonical_execution_input_for_profile(
  workflow_id: String,
  dag: workflow_dag.WorkflowDag,
  profile: config_types.WorkspaceHookProfile,
  artifact_limits: config_types.ArtifactLimits,
  model_settings: model_config.Settings,
) -> String {
  execution_to_json(workflow_id, dag, profile, artifact_limits, model_settings)
  |> json.to_string
}

fn execution_to_json(
  workflow_id: String,
  dag: workflow_dag.WorkflowDag,
  profile: config_types.WorkspaceHookProfile,
  artifact_limits: config_types.ArtifactLimits,
  model_settings: model_config.Settings,
) -> json.Json {
  let fields = [#("dag", dag_to_json(workflow_id, dag))]
  let fields = case profile.source {
    config_types.LegacyWorkspaceHooks
    | config_types.SyntheticDefaultWorkspace -> fields
    config_types.ConfiguredWorkspaceHooks
    | config_types.ConfiguredWorkspaceDriver ->
      list.append(fields, [
        #("workspace_profile", workspace_profile_to_json(profile)),
      ])
  }
  let fields = case profile.hooks {
    Some(hooks) ->
      list.append(fields, [#("dag_hooks", dag_hooks_to_json(hooks))])
    None -> fields
  }
  let fields = case profile.driver {
    Some(driver) ->
      list.append(fields, [
        #("workspace_driver", workspace_driver_to_json(driver)),
      ])
    None -> fields
  }
  json.object(
    list.append(fields, [
      #("artifact_limits", artifact_limits_to_json(artifact_limits)),
      #("global_model_settings", model_settings_to_json(model_settings)),
    ]),
  )
}

fn dag_to_json(
  workflow_id: String,
  dag: workflow_dag.WorkflowDag,
) -> json.Json {
  let prefix = [
    #("id", json.string(workflow_id)),
    #("description", option_string_to_json(dag.description)),
  ]
  let prefix = case dag.workspace_profile {
    None -> prefix
    Some(profile) ->
      list.append(prefix, [#("workspace_profile", json.string(profile))])
  }
  let prefix = case dag.workspace_capabilities {
    [] -> prefix
    capabilities ->
      list.append(prefix, [
        #(
          "workspace_capabilities",
          workspace_capabilities_to_json(capabilities),
        ),
      ])
  }
  json.object(
    list.append(prefix, [
      #("max_parallel_steps", json.int(dag.max_parallel_steps)),
      #("steps", json.array(sorted_steps(dag.steps), of: step_to_json)),
    ]),
  )
}

fn sorted_steps(
  steps: List(workflow_dag.WorkflowStep),
) -> List(workflow_dag.WorkflowStep) {
  list.sort(steps, by: fn(left, right) { string.compare(left.id, right.id) })
}

fn sorted_strings(values: List(String)) -> List(String) {
  list.sort(values, by: string.compare)
}

fn step_to_json(step: workflow_dag.WorkflowStep) -> json.Json {
  json.object([
    #("id", json.string(step.id)),
    #(
      "depends_on",
      json.array(sorted_strings(step.depends_on), of: json.string),
    ),
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
    workflow_dag.AgentStep(prompt_ref, structured_output) ->
      json.object([
        #("type", json.string("agent")),
        #("prompt", prompt_ref_to_json(prompt_ref)),
        #("structured_output", structured_output_to_json(structured_output)),
      ])
  }
}

fn structured_output_to_json(
  structured_output: Option(workflow_dag.StructuredOutputSpec),
) -> json.Json {
  case structured_output {
    None -> json.null()
    Some(spec) ->
      json.object([
        #(
          "format",
          json.string(workflow_dag.structured_output_format_to_string(
            spec.format,
          )),
        ),
        #("artifact_name", json.string(spec.artifact_name)),
        #("required", json.bool(spec.required)),
        #("source", structured_output_source_to_json(spec.source)),
        #("schema", structured_output_schema_to_json(spec.schema)),
        #("validator", structured_output_validator_to_json(spec.validator)),
        #("validation_retries", json.int(spec.validation_retries)),
      ])
  }
}

fn structured_output_source_to_json(
  source: structured_output_source.StructuredOutputSource,
) -> json.Json {
  case source {
    structured_output_source.FinalResponseSource ->
      json.object([#("type", json.string("final_response"))])
    structured_output_source.PiToolCallSource(
      tool_name,
      require_single,
      reject_sibling_tool_calls,
    ) ->
      json.object([
        #("type", json.string("pi_tool_call")),
        #("tool_name", json.string(tool_name)),
        #("require_single", json.bool(require_single)),
        #("reject_sibling_tool_calls", json.bool(reject_sibling_tool_calls)),
      ])
  }
}

fn structured_output_validator_to_json(
  validator: Option(workflow_dag.StructuredOutputValidator),
) -> json.Json {
  case validator {
    None -> json.null()
    Some(validator) ->
      json.string(workflow_dag.structured_output_validator_to_string(validator))
  }
}

fn structured_output_schema_to_json(
  schema: workflow_dag.StructuredOutputSchema,
) -> json.Json {
  case schema {
    workflow_dag.StructuredObjectSchema(required_keys) ->
      json.object([
        #("type", json.string("object")),
        #(
          "required",
          json.array(sorted_strings(required_keys), of: json.string),
        ),
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

fn workspace_capabilities_to_json(
  capabilities: List(config_types.WorkspaceCapability),
) -> json.Json {
  capabilities
  |> config_types.canonical_workspace_capabilities
  |> json.array(of: fn(capability) {
    json.string(config_types.workspace_capability_to_string(capability))
  })
}

fn workspace_lifecycle_to_json(
  operations: List(config_types.WorkspaceLifecycleOperation),
) -> json.Json {
  operations
  |> config_types.canonical_lifecycle_operations
  |> json.array(of: fn(operation) {
    json.string(config_types.workspace_lifecycle_operation_to_string(operation))
  })
}

fn workspace_driver_to_json(
  driver: config_types.WorkspaceDriverConfig,
) -> json.Json {
  json.object([
    #("command", json.string(driver.command)),
    #("lifecycle", workspace_lifecycle_to_json(driver.lifecycle)),
    #("capabilities", workspace_capabilities_to_json(driver.capabilities)),
    #("timeout_ms", json.int(driver.timeout_ms)),
  ])
}

fn workspace_profile_to_json(
  profile: config_types.WorkspaceHookProfile,
) -> json.Json {
  json.object([
    #("name", json.string(profile.name)),
    #("source", json.string(workspace_profile_source_to_string(profile.source))),
  ])
}

fn workspace_profile_source_to_string(
  source: config_types.WorkspaceProfileSource,
) -> String {
  case source {
    config_types.LegacyWorkspaceHooks -> "legacy-hooks"
    config_types.ConfiguredWorkspaceHooks -> "configured-hooks"
    config_types.ConfiguredWorkspaceDriver -> "configured-driver"
    config_types.SyntheticDefaultWorkspace -> "synthetic-default"
  }
}

fn dag_hooks_to_json(hooks: config_types.DagHooksConfig) -> json.Json {
  json.object([
    #("create", option_string_to_json(hooks.create)),
    #("before_step", option_string_to_json(hooks.before_step)),
    #("after_step", option_string_to_json(hooks.after_step)),
    #("remove", option_string_to_json(hooks.remove)),
    #("timeout_ms", json.int(hooks.timeout_ms)),
  ])
}

fn artifact_limits_to_json(limits: config_types.ArtifactLimits) -> json.Json {
  json.object([
    #("command_stream_max_chars", json.int(limits.command_stream_max_chars)),
    #("template_field_max_chars", json.int(limits.template_field_max_chars)),
    #("workflow_summary_max_chars", json.int(limits.workflow_summary_max_chars)),
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

fn workspace_profile_error_to_fingerprint_error(
  err: workspace_profile.ProfileResolutionError,
) -> FingerprintError {
  case err {
    workspace_profile.UnknownWorkspaceProfile(profile_name: profile_name, ..) ->
      WorkspaceProfileUnavailable(profile_name)
    workspace_profile.WorkspaceCapabilitiesUnavailable(
      profile_name: profile_name,
      ..,
    ) -> WorkspaceProfileUnavailable(profile_name)
    workspace_profile.WorkspaceDriverInvocationUnavailable(
      profile_name: profile_name,
      ..,
    ) -> WorkspaceProfileUnavailable(profile_name)
  }
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
