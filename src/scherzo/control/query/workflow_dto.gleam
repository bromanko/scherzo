import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/json
import gleam/option.{None}
import scherzo/control/query/types

pub fn workflow_list_to_json(workflows: types.WorkflowListDto) -> json.Json {
  json.object([
    #("schema_version", json.int(workflows.schema_version)),
    #("freshness", freshness_to_json(workflows.freshness)),
    #("diagnostics", json.array(workflows.diagnostics, of: diagnostic_to_json)),
    #("workflows", json.array(workflows.workflows, of: summary_to_json)),
  ])
}

pub fn workflow_detail_to_json(workflow: types.WorkflowDetailDto) -> json.Json {
  json.object([
    #("schema_version", json.int(workflow.schema_version)),
    #("summary", summary_to_json(workflow.summary)),
    #(
      "yaml_sources",
      json.array(workflow.yaml_sources, of: yaml_source_to_json),
    ),
    #("diagnostics", json.array(workflow.diagnostics, of: diagnostic_to_json)),
    #("freshness", freshness_to_json(workflow.freshness)),
    #("trigger", trigger_to_json(workflow.trigger)),
    #("workspace", workspace_to_json(workflow.workspace)),
    #("execution", execution_to_json(workflow.execution)),
    #("contract", json.nullable(workflow.contract, of: contract_to_json)),
    #("steps", json.array(workflow.steps, of: step_to_json)),
    #(
      "publications",
      json.array(workflow.publications, of: publication_to_json),
    ),
    #(
      "next_actions",
      json.array(workflow.next_actions, of: next_action_to_json),
    ),
    #("graph", graph_to_json(workflow.graph)),
  ])
}

pub fn decode_workflow_list_dynamic(
  value: Dynamic,
) -> Result(types.WorkflowListDto, types.QueryError) {
  case decode.run(value, workflow_list_decoder()) {
    Ok(workflows) -> validate_workflow_list_schema(workflows)
    Error(_) ->
      Error(types.QueryError(
        types.QueryBackendFailed,
        "invalid workflow list query payload",
      ))
  }
}

pub fn decode_workflow_detail_dynamic(
  value: Dynamic,
) -> Result(types.WorkflowDetailDto, types.QueryError) {
  case decode.run(value, workflow_detail_decoder()) {
    Ok(workflow) -> validate_workflow_detail_schema(workflow)
    Error(_) ->
      Error(types.QueryError(
        types.QueryBackendFailed,
        "invalid workflow detail query payload",
      ))
  }
}

fn summary_to_json(summary: types.WorkflowSummaryDto) -> json.Json {
  json.object([
    #("id", json.string(summary.id)),
    #("name", json.string(summary.name)),
    #("route", json.nullable(summary.route, of: json.string)),
    #("label", json.nullable(summary.label, of: json.string)),
    #("yaml_paths", json.array(summary.yaml_paths, of: json.string)),
    #("step_count", json.int(summary.step_count)),
    #("status", json.string(summary.status)),
  ])
}

fn yaml_source_to_json(source: types.WorkflowYamlSourceDto) -> json.Json {
  json.object([
    #("path", json.string(source.path)),
    #("contents", json.string(source.contents)),
    #("contents_sha256", json.string(source.contents_sha256)),
    #("contents_truncated", json.bool(source.contents_truncated)),
  ])
}

fn diagnostic_to_json(diagnostic: types.WorkflowDiagnosticDto) -> json.Json {
  json.object([
    #("severity", json.string(diagnostic.severity)),
    #("code", json.string(diagnostic.code)),
    #("message", json.string(diagnostic.message)),
    #("path", json.nullable(diagnostic.path, of: json.string)),
  ])
}

fn freshness_to_json(freshness: types.WorkflowFreshnessDto) -> json.Json {
  json.object([
    #("source_hash", json.string(freshness.source_hash)),
    #("reload_status", json.string(freshness.reload_status)),
  ])
}

fn trigger_to_json(trigger: types.WorkflowTriggerDto) -> json.Json {
  case trigger {
    types.WorkflowRoutedTriggerDto(route, label) ->
      json.object([
        #("kind", json.string("routed")),
        #("route", json.string(route)),
        #("label", json.nullable(label, of: json.string)),
      ])
    types.WorkflowScheduledTriggerDto(
      schedule_id,
      every_ms,
      overlap,
      catch_up,
      on_failure,
    ) ->
      json.object([
        #("kind", json.string("scheduled")),
        #("schedule_id", json.string(schedule_id)),
        #("every_ms", json.int(every_ms)),
        #("overlap", json.string(overlap)),
        #("catch_up", json.bool(catch_up)),
        #("on_failure", scheduled_failure_to_json(on_failure)),
      ])
  }
}

fn scheduled_failure_to_json(
  failure: types.WorkflowScheduledFailureDto,
) -> json.Json {
  json.object([
    #("task_enabled", json.bool(failure.task_enabled)),
    #("task_state", json.nullable(failure.task_state, of: json.string)),
    #("task_labels", json.array(failure.task_labels, of: json.string)),
    #("task_dedupe", json.string(failure.task_dedupe)),
  ])
}

fn workspace_to_json(workspace: types.WorkflowWorkspaceDto) -> json.Json {
  json.object([
    #("driver", json.string(workspace.driver)),
    #(
      "required_capabilities",
      json.array(workspace.required_capabilities, of: json.string),
    ),
  ])
}

fn model_settings_to_json(
  settings: types.WorkflowModelSettingsDto,
) -> json.Json {
  json.object([
    #("model", json.nullable(settings.model, of: json.string)),
    #("thinking", json.nullable(settings.thinking, of: json.string)),
  ])
}

fn prompt_ref_to_json(prompt: types.WorkflowPromptRefDto) -> json.Json {
  json.object([
    #("kind", json.string(prompt.kind)),
    #("ref", json.nullable(prompt.ref, of: json.string)),
  ])
}

fn recovery_to_json(recovery: types.WorkflowRecoveryDto) -> json.Json {
  json.object([
    #("attempts", json.int(recovery.attempts)),
    #("model", json.nullable(recovery.model, of: json.string)),
    #("prompt", prompt_ref_to_json(recovery.prompt)),
  ])
}

fn execution_to_json(
  execution: types.WorkflowExecutionDefaultsDto,
) -> json.Json {
  json.object([
    #("model", model_settings_to_json(execution.model)),
    #("max_parallel_steps", json.int(execution.max_parallel_steps)),
    #("recovery", json.nullable(execution.recovery, of: recovery_to_json)),
  ])
}

fn contract_to_json(contract: types.WorkflowContractDto) -> json.Json {
  json.object([
    #("version", json.int(contract.version)),
    #("inputs", json.array(contract.inputs, of: contract_spec_to_json)),
    #("context", json.array(contract.context, of: contract_spec_to_json)),
    #("outputs", json.array(contract.outputs, of: contract_spec_to_json)),
  ])
}

fn contract_spec_to_json(spec: types.WorkflowContractSpecDto) -> json.Json {
  json.object([
    #("name", json.string(spec.name)),
    #("type", json.string(spec.type_)),
    #("description", json.nullable(spec.description, of: json.string)),
    #("source", contract_source_to_json(spec.source)),
    #("descriptor_present", json.bool(spec.descriptor_present)),
  ])
}

fn contract_source_to_json(
  source: types.WorkflowContractSourceDto,
) -> json.Json {
  json.object([
    #("required", json.bool(source.required)),
    #("kind", json.nullable(source.kind, of: json.string)),
  ])
}

fn step_to_json(step: types.WorkflowStepDto) -> json.Json {
  json.object([
    #("id", json.string(step.id)),
    #("kind", json.string(step.kind)),
    #("depends_on", json.array(step.depends_on, of: json.string)),
    #("on_failure", json.string(step.on_failure)),
    #("model", json.nullable(step.model, of: model_settings_to_json)),
    #("recovery", json.nullable(step.recovery, of: recovery_to_json)),
    #("command", json.nullable(step.command, of: command_step_to_json)),
    #("agent", json.nullable(step.agent, of: agent_step_to_json)),
  ])
}

fn command_step_to_json(command: types.WorkflowCommandStepDto) -> json.Json {
  json.object([
    #("run", json.string(command.run)),
    #("timeout_ms", json.nullable(command.timeout_ms, of: json.int)),
  ])
}

fn agent_step_to_json(agent: types.WorkflowAgentStepDto) -> json.Json {
  json.object([
    #("prompt", prompt_ref_to_json(agent.prompt)),
    #(
      "structured_output",
      json.nullable(agent.structured_output, of: structured_output_to_json),
    ),
  ])
}

fn structured_output_to_json(
  structured_output: types.WorkflowStructuredOutputDto,
) -> json.Json {
  json.object([
    #("artifact_name", json.string(structured_output.artifact_name)),
    #("required", json.bool(structured_output.required)),
    #(
      "validators",
      json.array(
        structured_output.validators,
        of: structured_output_validator_to_json,
      ),
    ),
    #("validation_retries", json.int(structured_output.validation_retries)),
  ])
}

fn structured_output_validator_to_json(
  validator: types.WorkflowStructuredOutputValidatorDto,
) -> json.Json {
  json.object([
    #("name", json.string(validator.name)),
    #("kind", json.string(validator.kind)),
  ])
}

fn publication_to_json(publication: types.WorkflowPublicationDto) -> json.Json {
  json.object([
    #("id", json.string(publication.id)),
    #("repository", json.string(publication.repository)),
    #("required", json.bool(publication.required)),
    #("mode", json.string(publication.mode)),
  ])
}

fn next_action_to_json(action: types.WorkflowNextActionDto) -> json.Json {
  json.object([
    #("action_id", json.string(action.action_id)),
    #("workflow_id", json.string(action.workflow_id)),
    #("requires_gate", json.nullable(action.requires_gate, of: json.string)),
    #("auto_enqueue", json.bool(action.auto_enqueue)),
  ])
}

fn graph_to_json(graph: types.WorkflowGraphDto) -> json.Json {
  json.object([
    #("nodes", json.array(graph.nodes, of: graph_node_to_json)),
    #("edges", json.array(graph.edges, of: graph_edge_to_json)),
  ])
}

fn graph_node_to_json(node: types.WorkflowGraphNodeDto) -> json.Json {
  json.object([
    #("id", json.string(node.id)),
    #("label", json.string(node.label)),
    #("kind", json.string(node.kind)),
  ])
}

fn graph_edge_to_json(edge: types.WorkflowGraphEdgeDto) -> json.Json {
  json.object([
    #("from", json.string(edge.from)),
    #("to", json.string(edge.to)),
  ])
}

fn workflow_list_decoder() -> decode.Decoder(types.WorkflowListDto) {
  use schema_version <- decode.field("schema_version", decode.int)
  use freshness <- decode.field("freshness", freshness_decoder())
  use diagnostics <- decode.field(
    "diagnostics",
    decode.list(diagnostic_decoder()),
  )
  use workflows <- decode.field("workflows", decode.list(summary_decoder()))
  decode.success(types.WorkflowListDto(
    schema_version: schema_version,
    freshness: freshness,
    diagnostics: diagnostics,
    workflows: workflows,
  ))
}

fn workflow_detail_decoder() -> decode.Decoder(types.WorkflowDetailDto) {
  use schema_version <- decode.field("schema_version", decode.int)
  use summary <- decode.field("summary", summary_decoder())
  use yaml_sources <- decode.field(
    "yaml_sources",
    decode.list(yaml_source_decoder()),
  )
  use diagnostics <- decode.field(
    "diagnostics",
    decode.list(diagnostic_decoder()),
  )
  use freshness <- decode.field("freshness", freshness_decoder())
  use trigger <- decode.field("trigger", trigger_decoder())
  use workspace <- decode.field("workspace", workspace_decoder())
  use execution <- decode.field("execution", execution_decoder())
  use contract <- decode.field("contract", decode.optional(contract_decoder()))
  use steps <- decode.field("steps", decode.list(step_decoder()))
  use publications <- decode.field(
    "publications",
    decode.list(publication_decoder()),
  )
  use next_actions <- decode.field(
    "next_actions",
    decode.list(next_action_decoder()),
  )
  use graph <- decode.field("graph", graph_decoder())
  decode.success(types.WorkflowDetailDto(
    schema_version: schema_version,
    summary: summary,
    yaml_sources: yaml_sources,
    diagnostics: diagnostics,
    freshness: freshness,
    trigger: trigger,
    workspace: workspace,
    execution: execution,
    contract: contract,
    steps: steps,
    publications: publications,
    next_actions: next_actions,
    graph: graph,
  ))
}

fn summary_decoder() -> decode.Decoder(types.WorkflowSummaryDto) {
  use id <- decode.field("id", decode.string)
  use name <- decode.field("name", decode.string)
  use route <- decode.field("route", decode.optional(decode.string))
  use label <- decode.field("label", decode.optional(decode.string))
  use yaml_paths <- decode.field("yaml_paths", decode.list(decode.string))
  use step_count <- decode.field("step_count", decode.int)
  use status <- decode.field("status", decode.string)
  decode.success(types.WorkflowSummaryDto(
    id: id,
    name: name,
    route: route,
    label: label,
    yaml_paths: yaml_paths,
    step_count: step_count,
    status: status,
  ))
}

fn yaml_source_decoder() -> decode.Decoder(types.WorkflowYamlSourceDto) {
  use path <- decode.field("path", decode.string)
  use contents <- decode.field("contents", decode.string)
  use contents_sha256 <- decode.field("contents_sha256", decode.string)
  use contents_truncated <- decode.field("contents_truncated", decode.bool)
  decode.success(types.WorkflowYamlSourceDto(
    path: path,
    contents: contents,
    contents_sha256: contents_sha256,
    contents_truncated: contents_truncated,
  ))
}

fn diagnostic_decoder() -> decode.Decoder(types.WorkflowDiagnosticDto) {
  use severity <- decode.field("severity", decode.string)
  use code <- decode.field("code", decode.string)
  use message <- decode.field("message", decode.string)
  use path <- decode.field("path", decode.optional(decode.string))
  decode.success(types.WorkflowDiagnosticDto(
    severity: severity,
    code: code,
    message: message,
    path: path,
  ))
}

fn freshness_decoder() -> decode.Decoder(types.WorkflowFreshnessDto) {
  use source_hash <- decode.field("source_hash", decode.string)
  use reload_status <- decode.field("reload_status", decode.string)
  decode.success(types.WorkflowFreshnessDto(
    source_hash: source_hash,
    reload_status: reload_status,
  ))
}

fn trigger_decoder() -> decode.Decoder(types.WorkflowTriggerDto) {
  use kind <- decode.field("kind", decode.string)
  case kind {
    "routed" -> routed_trigger_decoder()
    "scheduled" -> scheduled_trigger_decoder()
    _ ->
      decode.failure(
        types.WorkflowRoutedTriggerDto(route: "", label: None),
        expected: "workflow trigger kind",
      )
  }
}

fn routed_trigger_decoder() -> decode.Decoder(types.WorkflowTriggerDto) {
  use route <- decode.field("route", decode.string)
  use label <- decode.field("label", decode.optional(decode.string))
  decode.success(types.WorkflowRoutedTriggerDto(route: route, label: label))
}

fn scheduled_trigger_decoder() -> decode.Decoder(types.WorkflowTriggerDto) {
  use schedule_id <- decode.field("schedule_id", decode.string)
  use every_ms <- decode.field("every_ms", decode.int)
  use overlap <- decode.field("overlap", decode.string)
  use catch_up <- decode.field("catch_up", decode.bool)
  use on_failure <- decode.field("on_failure", scheduled_failure_decoder())
  decode.success(types.WorkflowScheduledTriggerDto(
    schedule_id: schedule_id,
    every_ms: every_ms,
    overlap: overlap,
    catch_up: catch_up,
    on_failure: on_failure,
  ))
}

fn scheduled_failure_decoder() -> decode.Decoder(
  types.WorkflowScheduledFailureDto,
) {
  use task_enabled <- decode.field("task_enabled", decode.bool)
  use task_state <- decode.field("task_state", decode.optional(decode.string))
  use task_labels <- decode.field("task_labels", decode.list(decode.string))
  use task_dedupe <- decode.field("task_dedupe", decode.string)
  decode.success(types.WorkflowScheduledFailureDto(
    task_enabled: task_enabled,
    task_state: task_state,
    task_labels: task_labels,
    task_dedupe: task_dedupe,
  ))
}

fn workspace_decoder() -> decode.Decoder(types.WorkflowWorkspaceDto) {
  use driver <- decode.field("driver", decode.string)
  use required_capabilities <- decode.field(
    "required_capabilities",
    decode.list(decode.string),
  )
  decode.success(types.WorkflowWorkspaceDto(
    driver: driver,
    required_capabilities: required_capabilities,
  ))
}

fn model_settings_decoder() -> decode.Decoder(types.WorkflowModelSettingsDto) {
  use model <- decode.field("model", decode.optional(decode.string))
  use thinking <- decode.field("thinking", decode.optional(decode.string))
  decode.success(types.WorkflowModelSettingsDto(
    model: model,
    thinking: thinking,
  ))
}

fn prompt_ref_decoder() -> decode.Decoder(types.WorkflowPromptRefDto) {
  use kind <- decode.field("kind", decode.string)
  use ref <- decode.field("ref", decode.optional(decode.string))
  decode.success(types.WorkflowPromptRefDto(kind: kind, ref: ref))
}

fn recovery_decoder() -> decode.Decoder(types.WorkflowRecoveryDto) {
  use attempts <- decode.field("attempts", decode.int)
  use model <- decode.field("model", decode.optional(decode.string))
  use prompt <- decode.field("prompt", prompt_ref_decoder())
  decode.success(types.WorkflowRecoveryDto(
    attempts: attempts,
    model: model,
    prompt: prompt,
  ))
}

fn execution_decoder() -> decode.Decoder(types.WorkflowExecutionDefaultsDto) {
  use model <- decode.field("model", model_settings_decoder())
  use max_parallel_steps <- decode.field("max_parallel_steps", decode.int)
  use recovery <- decode.field("recovery", decode.optional(recovery_decoder()))
  decode.success(types.WorkflowExecutionDefaultsDto(
    model: model,
    max_parallel_steps: max_parallel_steps,
    recovery: recovery,
  ))
}

fn contract_decoder() -> decode.Decoder(types.WorkflowContractDto) {
  use version <- decode.field("version", decode.int)
  use inputs <- decode.field("inputs", decode.list(contract_spec_decoder()))
  use context <- decode.field("context", decode.list(contract_spec_decoder()))
  use outputs <- decode.field("outputs", decode.list(contract_spec_decoder()))
  decode.success(types.WorkflowContractDto(
    version: version,
    inputs: inputs,
    context: context,
    outputs: outputs,
  ))
}

fn contract_spec_decoder() -> decode.Decoder(types.WorkflowContractSpecDto) {
  use name <- decode.field("name", decode.string)
  use type_ <- decode.field("type", decode.string)
  use description <- decode.field("description", decode.optional(decode.string))
  use source <- decode.field("source", contract_source_decoder())
  use descriptor_present <- decode.field("descriptor_present", decode.bool)
  decode.success(types.WorkflowContractSpecDto(
    name: name,
    type_: type_,
    description: description,
    source: source,
    descriptor_present: descriptor_present,
  ))
}

fn contract_source_decoder() -> decode.Decoder(types.WorkflowContractSourceDto) {
  use required <- decode.field("required", decode.bool)
  use kind <- decode.field("kind", decode.optional(decode.string))
  decode.success(types.WorkflowContractSourceDto(required: required, kind: kind))
}

fn step_decoder() -> decode.Decoder(types.WorkflowStepDto) {
  use id <- decode.field("id", decode.string)
  use kind <- decode.field("kind", decode.string)
  use depends_on <- decode.field("depends_on", decode.list(decode.string))
  use on_failure <- decode.field("on_failure", decode.string)
  use model <- decode.field("model", decode.optional(model_settings_decoder()))
  use recovery <- decode.field("recovery", decode.optional(recovery_decoder()))
  use command <- decode.field(
    "command",
    decode.optional(command_step_decoder()),
  )
  use agent <- decode.field("agent", decode.optional(agent_step_decoder()))
  decode.success(types.WorkflowStepDto(
    id: id,
    kind: kind,
    depends_on: depends_on,
    on_failure: on_failure,
    model: model,
    recovery: recovery,
    command: command,
    agent: agent,
  ))
}

fn command_step_decoder() -> decode.Decoder(types.WorkflowCommandStepDto) {
  use run <- decode.field("run", decode.string)
  use timeout_ms <- decode.field("timeout_ms", decode.optional(decode.int))
  decode.success(types.WorkflowCommandStepDto(run: run, timeout_ms: timeout_ms))
}

fn agent_step_decoder() -> decode.Decoder(types.WorkflowAgentStepDto) {
  use prompt <- decode.field("prompt", prompt_ref_decoder())
  use structured_output <- decode.field(
    "structured_output",
    decode.optional(structured_output_decoder()),
  )
  decode.success(types.WorkflowAgentStepDto(
    prompt: prompt,
    structured_output: structured_output,
  ))
}

fn structured_output_decoder() -> decode.Decoder(
  types.WorkflowStructuredOutputDto,
) {
  use artifact_name <- decode.field("artifact_name", decode.string)
  use required <- decode.field("required", decode.bool)
  use validators <- decode.field(
    "validators",
    decode.list(structured_output_validator_decoder()),
  )
  use validation_retries <- decode.field("validation_retries", decode.int)
  decode.success(types.WorkflowStructuredOutputDto(
    artifact_name: artifact_name,
    required: required,
    validators: validators,
    validation_retries: validation_retries,
  ))
}

fn structured_output_validator_decoder() -> decode.Decoder(
  types.WorkflowStructuredOutputValidatorDto,
) {
  use name <- decode.field("name", decode.string)
  use kind <- decode.field("kind", decode.string)
  decode.success(types.WorkflowStructuredOutputValidatorDto(
    name: name,
    kind: kind,
  ))
}

fn publication_decoder() -> decode.Decoder(types.WorkflowPublicationDto) {
  use id <- decode.field("id", decode.string)
  use repository <- decode.field("repository", decode.string)
  use required <- decode.field("required", decode.bool)
  use mode <- decode.field("mode", decode.string)
  decode.success(types.WorkflowPublicationDto(
    id: id,
    repository: repository,
    required: required,
    mode: mode,
  ))
}

fn next_action_decoder() -> decode.Decoder(types.WorkflowNextActionDto) {
  use action_id <- decode.field("action_id", decode.string)
  use workflow_id <- decode.field("workflow_id", decode.string)
  use requires_gate <- decode.field(
    "requires_gate",
    decode.optional(decode.string),
  )
  use auto_enqueue <- decode.field("auto_enqueue", decode.bool)
  decode.success(types.WorkflowNextActionDto(
    action_id: action_id,
    workflow_id: workflow_id,
    requires_gate: requires_gate,
    auto_enqueue: auto_enqueue,
  ))
}

fn graph_decoder() -> decode.Decoder(types.WorkflowGraphDto) {
  use nodes <- decode.field("nodes", decode.list(graph_node_decoder()))
  use edges <- decode.field("edges", decode.list(graph_edge_decoder()))
  decode.success(types.WorkflowGraphDto(nodes: nodes, edges: edges))
}

fn graph_node_decoder() -> decode.Decoder(types.WorkflowGraphNodeDto) {
  use id <- decode.field("id", decode.string)
  use label <- decode.field("label", decode.string)
  use kind <- decode.field("kind", decode.string)
  decode.success(types.WorkflowGraphNodeDto(id: id, label: label, kind: kind))
}

fn graph_edge_decoder() -> decode.Decoder(types.WorkflowGraphEdgeDto) {
  use from <- decode.field("from", decode.string)
  use to <- decode.field("to", decode.string)
  decode.success(types.WorkflowGraphEdgeDto(from: from, to: to))
}

fn validate_workflow_list_schema(
  workflows: types.WorkflowListDto,
) -> Result(types.WorkflowListDto, types.QueryError) {
  case workflows.schema_version == types.workflow_query_schema_version {
    True -> Ok(workflows)
    False -> unsupported_workflow_schema_version()
  }
}

fn validate_workflow_detail_schema(
  workflow: types.WorkflowDetailDto,
) -> Result(types.WorkflowDetailDto, types.QueryError) {
  case workflow.schema_version == types.workflow_query_schema_version {
    True -> Ok(workflow)
    False -> unsupported_workflow_schema_version()
  }
}

fn unsupported_workflow_schema_version() -> Result(a, types.QueryError) {
  Error(types.QueryError(
    types.QueryBackendFailed,
    "unsupported workflow query schema version",
  ))
}
