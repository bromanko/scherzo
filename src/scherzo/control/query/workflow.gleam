import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order.{type Order, Gt, Lt}
import gleam/string
import scherzo/artifact_publication_config
import scherzo/config
import scherzo/config/types as config_types
import scherzo/control/query/types
import scherzo/hash
import scherzo/model_config
import scherzo/orchestrator/workflow_reloader
import scherzo/path
import scherzo/runtime_bundle
import scherzo/workflow_contract
import scherzo/workflow_dag
import scherzo/workspace_profile
import scherzo/workstream/phase_metadata

pub const max_yaml_source_contents_chars = 65_536

pub const max_structured_field_chars = 65_536

pub fn execute_list(
  state: workflow_reloader.State,
) -> Result(types.QueryResponse, types.QueryError) {
  Ok(types.WorkflowListResponse(workflow_list_from_state(state)))
}

pub fn execute_detail(
  state: workflow_reloader.State,
  query: types.WorkflowDetailQuery,
) -> Result(types.QueryResponse, types.QueryError) {
  let workflow_id = normalize_workflow_id(query.workflow_id)
  case dict.get(state.bundle.workflows, workflow_id) {
    Ok(dag) ->
      Ok(
        types.WorkflowDetailResponse(workflow_detail_from_state(
          state,
          workflow_id,
          dag,
        )),
      )
    Error(Nil) ->
      Error(types.QueryError(
        types.QueryNotFound,
        "workflow not found: " <> query.workflow_id,
      ))
  }
}

pub fn workflow_list_from_state(
  state: workflow_reloader.State,
) -> types.WorkflowListDto {
  let roots = display_roots(state)
  let freshness = freshness_from_state(state)
  let diagnostics = diagnostics_from_state(state, roots)
  let workflows =
    state.bundle.workflows
    |> dict.to_list
    |> list.map(fn(entry) {
      let #(workflow_id, dag) = entry
      summary_from_workflow(state, workflow_id, dag, roots)
    })
    |> list.sort(by: compare_workflow_summary)

  types.WorkflowListDto(
    schema_version: types.workflow_query_schema_version,
    freshness: freshness,
    diagnostics: diagnostics,
    workflows: workflows,
  )
}

pub fn workflow_detail_from_state(
  state: workflow_reloader.State,
  workflow_id: String,
  dag: workflow_dag.WorkflowDag,
) -> types.WorkflowDetailDto {
  let roots = display_roots(state)
  types.WorkflowDetailDto(
    schema_version: types.workflow_query_schema_version,
    summary: summary_from_workflow(state, workflow_id, dag, roots),
    yaml_sources: yaml_sources_for_workflow(state, workflow_id, roots),
    diagnostics: diagnostics_from_state(state, roots),
    freshness: freshness_from_state(state),
    trigger: trigger_from_state(state, workflow_id),
    workspace: workspace_from_dag(state, dag),
    execution: execution_defaults_from_dag(state, dag),
    contract: contract_from_dag(dag),
    steps: steps_from_dag(state.bundle.orchestrator.model_settings, dag),
    publications: publications_from_dag(dag),
    next_actions: next_actions_from_dag(dag),
    graph: graph_from_dag(dag),
  )
}

pub fn safe_relative_path(raw_path: String, roots: List(String)) -> String {
  let target = path.absolute_or_original(raw_path)
  case safe_relative_for_roots(target, roots) {
    Some(relative) -> relative
    None -> external_relative_path(raw_path)
  }
}

fn summary_from_workflow(
  state: workflow_reloader.State,
  workflow_id: String,
  dag: workflow_dag.WorkflowDag,
  roots: List(String),
) -> types.WorkflowSummaryDto {
  types.WorkflowSummaryDto(
    id: workflow_id,
    name: workflow_dag.id(dag),
    route: Some(workflow_id),
    label: label_for_workflow(
      state.bundle.orchestrator.routing.workflow_label_prefix,
      workflow_id,
    ),
    yaml_paths: yaml_paths_for_workflow(state, workflow_id, roots),
    step_count: list.length(workflow_dag.steps(dag)),
    status: reload_status_to_string(state.reload_state.current_status),
  )
}

fn yaml_paths_for_workflow(
  state: workflow_reloader.State,
  workflow_id: String,
  roots: List(String),
) -> List(String) {
  yaml_sources_for_workflow(state, workflow_id, roots)
  |> list.map(fn(source) { source.path })
}

fn yaml_sources_for_workflow(
  state: workflow_reloader.State,
  workflow_id: String,
  roots: List(String),
) -> List(types.WorkflowYamlSourceDto) {
  workflow_yaml_paths(state, workflow_id)
  |> list.filter_map(fn(source_path) {
    case find_dependency(state.bundle.dependencies, source_path) {
      Some(dependency) ->
        Ok(workflow_yaml_source_from_dependency(dependency, roots))
      None -> Error(Nil)
    }
  })
}

fn workflow_yaml_paths(
  state: workflow_reloader.State,
  workflow_id: String,
) -> List(String) {
  let workflow_path = case
    dict.get(state.bundle.orchestrator.routing.workflows, workflow_id)
  {
    Ok(path) -> [path]
    Error(Nil) -> []
  }
  [state.bundle.config_path, ..workflow_path]
  |> list.filter(is_yaml_path)
  |> dedupe_strings
}

fn find_dependency(
  dependencies: List(runtime_bundle.BundleDependency),
  dependency_path: String,
) -> Option(runtime_bundle.BundleDependency) {
  case dependencies {
    [] -> None
    [dependency, ..rest] ->
      case dependency.path == dependency_path {
        True -> Some(dependency)
        False -> find_dependency(rest, dependency_path)
      }
  }
}

fn workflow_yaml_source_from_dependency(
  dependency: runtime_bundle.BundleDependency,
  roots: List(String),
) -> types.WorkflowYamlSourceDto {
  let redacted_contents = redact_yaml_source_contents(dependency.contents)
  let contents_truncated =
    string.length(redacted_contents) > max_yaml_source_contents_chars
  let contents = case contents_truncated {
    True -> string.slice(redacted_contents, 0, max_yaml_source_contents_chars)
    False -> redacted_contents
  }

  types.WorkflowYamlSourceDto(
    path: safe_relative_path(dependency.path, roots),
    contents: contents,
    contents_sha256: hash.sha256_hex(dependency.contents),
    contents_truncated: contents_truncated,
  )
}

fn redact_yaml_source_contents(contents: String) -> String {
  contents
  |> string.split(on: "\n")
  |> list.map(redact_yaml_source_line)
  |> string.join(with: "\n")
}

fn redact_yaml_source_line(line: String) -> String {
  case string.split_once(line, on: ":") {
    Ok(#(key, _)) ->
      case sensitive_yaml_key(key) {
        True -> key <> ": [REDACTED]"
        False -> line
      }
    Error(Nil) -> line
  }
}

fn sensitive_yaml_key(raw_key: String) -> Bool {
  let key = normalize_yaml_key(raw_key)
  key == "api_key"
  || key == "authorization"
  || key == "password"
  || key == "secret"
  || key == "token"
  || string.ends_with(key, "_api_key")
  || string.ends_with(key, "_password")
  || string.ends_with(key, "_secret")
  || string.ends_with(key, "_token")
}

fn cap_structured_text(value: String) -> String {
  case string.length(value) > max_structured_field_chars {
    True -> string.slice(value, 0, max_structured_field_chars)
    False -> value
  }
}

fn normalize_yaml_key(raw_key: String) -> String {
  let key = raw_key |> string.trim |> string.lowercase
  case string.starts_with(key, "- ") {
    True -> key |> string.drop_start(2) |> string.trim
    False -> key
  }
}

fn freshness_from_state(
  state: workflow_reloader.State,
) -> types.WorkflowFreshnessDto {
  types.WorkflowFreshnessDto(
    source_hash: source_hash(state.bundle.dependencies),
    reload_status: reload_status_to_string(state.reload_state.current_status),
  )
}

fn source_hash(dependencies: List(runtime_bundle.BundleDependency)) -> String {
  dependencies
  |> list.sort(by: compare_dependency_path)
  |> list.map(fn(dependency) {
    dependency.path <> "\n" <> hash.sha256_hex(dependency.contents)
  })
  |> string.join(with: "\n")
  |> hash.sha256_hex
}

fn diagnostics_from_state(
  state: workflow_reloader.State,
  roots: List(String),
) -> List(types.WorkflowDiagnosticDto) {
  case state.reload_state.current_status {
    config.CurrentValid -> []
    config.CurrentInvalid(reason) -> {
      let dependency_diagnostics =
        invalid_dependency_diagnostics(
          state.last_invalid_dependency_snapshot,
          roots,
        )
      [
        types.WorkflowDiagnosticDto(
          severity: "error",
          code: reason,
          message: "workflow reload failed; serving last known good workflows",
          path: None,
        ),
        ..dependency_diagnostics
      ]
    }
  }
}

fn invalid_dependency_diagnostics(
  snapshot: Option(List(workflow_reloader.DependencyRead)),
  roots: List(String),
) -> List(types.WorkflowDiagnosticDto) {
  case snapshot {
    None -> []
    Some(reads) ->
      reads
      |> list.filter_map(fn(read) {
        case read.status {
          workflow_reloader.ReadFailed ->
            Ok(types.WorkflowDiagnosticDto(
              severity: "error",
              code: "dependency_read_failed",
              message: "could not read workflow dependency",
              path: Some(safe_relative_path(read.path, roots)),
            ))
          workflow_reloader.ReadContents(_) -> Error(Nil)
        }
      })
  }
}

fn trigger_from_state(
  state: workflow_reloader.State,
  workflow_id: String,
) -> types.WorkflowTriggerDto {
  case
    schedule_for_workflow(state.bundle.orchestrator.scheduled_jobs, workflow_id)
  {
    Some(job) ->
      types.WorkflowScheduledTriggerDto(
        schedule_id: job.id,
        every_ms: job.every_ms,
        overlap: scheduled_overlap_to_string(job.overlap),
        catch_up: job.catch_up,
        on_failure: scheduled_failure_to_dto(job.on_failure),
      )
    None ->
      types.WorkflowRoutedTriggerDto(
        route: workflow_id,
        label: label_for_workflow(
          state.bundle.orchestrator.routing.workflow_label_prefix,
          workflow_id,
        ),
      )
  }
}

fn schedule_for_workflow(
  jobs: List(config_types.ScheduledJobConfig),
  workflow_id: String,
) -> Option(config_types.ScheduledJobConfig) {
  case list.find(jobs, fn(job) { job.enabled && job.workflow == workflow_id }) {
    Ok(job) -> Some(job)
    Error(Nil) -> None
  }
}

fn scheduled_overlap_to_string(
  overlap: config_types.ScheduledOverlap,
) -> String {
  case overlap {
    config_types.SkipOverlap -> "skip"
  }
}

fn scheduled_failure_to_dto(
  failure: config_types.ScheduledFailureConfig,
) -> types.WorkflowScheduledFailureDto {
  types.WorkflowScheduledFailureDto(
    task_enabled: failure.task.enabled,
    task_state: failure.task.state,
    task_labels: failure.task.labels,
    task_dedupe: scheduled_failure_dedupe_to_string(failure.task.dedupe),
  )
}

fn scheduled_failure_dedupe_to_string(
  dedupe: config_types.ScheduledFailureDedupe,
) -> String {
  case dedupe {
    config_types.OpenTaskPerSchedule -> "open_task_per_schedule"
  }
}

fn workspace_from_dag(
  state: workflow_reloader.State,
  dag: workflow_dag.WorkflowDag,
) -> types.WorkflowWorkspaceDto {
  types.WorkflowWorkspaceDto(
    driver: workspace_profile.selected_name(dag, state.bundle.orchestrator),
    required_capabilities: workflow_dag.workspace_capabilities(dag)
      |> config_types.workspace_capability_names,
  )
}

fn execution_defaults_from_dag(
  state: workflow_reloader.State,
  dag: workflow_dag.WorkflowDag,
) -> types.WorkflowExecutionDefaultsDto {
  types.WorkflowExecutionDefaultsDto(
    model: model_config.resolve(
      state.bundle.orchestrator.model_settings,
      workflow_dag.model_settings(dag),
    )
      |> model_settings_to_dto,
    max_parallel_steps: workflow_dag.max_parallel_steps(dag),
    recovery: workflow_recovery_from_patch(workflow_dag.recovery_config(dag)),
  )
}

fn model_settings_to_dto(
  settings: model_config.Settings,
) -> types.WorkflowModelSettingsDto {
  types.WorkflowModelSettingsDto(
    model: settings.model,
    thinking: option.map(settings.thinking, model_config.thinking_to_string),
  )
}

fn workflow_recovery_from_patch(
  recover: Option(workflow_dag.RecoveryConfigPatch),
) -> Option(types.WorkflowRecoveryDto) {
  case recover {
    None -> None
    Some(workflow_dag.RecoveryConfigPatch(enabled, attempts, model, prompt)) ->
      case bool_option_default(enabled, True), prompt {
        False, _ -> None
        True, Some(prompt) ->
          Some(types.WorkflowRecoveryDto(
            attempts: int_option_default(attempts, 1),
            model: model,
            prompt: prompt_ref_to_dto(prompt),
          ))
        True, None -> None
      }
  }
}

fn contract_from_dag(
  dag: workflow_dag.WorkflowDag,
) -> Option(types.WorkflowContractDto) {
  workflow_dag.contract(dag)
  |> option.map(contract_to_dto)
}

fn contract_to_dto(
  contract: workflow_contract.Contract,
) -> types.WorkflowContractDto {
  types.WorkflowContractDto(
    version: contract.version,
    inputs: list.map(contract.inputs, input_spec_to_dto),
    context: list.map(contract.context, context_spec_to_dto),
    outputs: list.map(contract.outputs, output_spec_to_dto),
  )
}

fn input_spec_to_dto(
  input: workflow_contract.InputSpec,
) -> types.WorkflowContractSpecDto {
  types.WorkflowContractSpecDto(
    name: input.name,
    type_: workflow_contract.type_to_string(input.type_),
    description: option.map(input.description, cap_structured_text),
    source: contract_source_requirement_to_dto(input.source, input_source_kind),
    descriptor_present: has_contract_descriptor(input.descriptor),
  )
}

fn context_spec_to_dto(
  context: workflow_contract.ContextSpec,
) -> types.WorkflowContractSpecDto {
  types.WorkflowContractSpecDto(
    name: context.name,
    type_: workflow_contract.type_to_string(context.type_),
    description: option.map(context.description, cap_structured_text),
    source: contract_source_requirement_to_dto(
      context.source,
      context_source_kind,
    ),
    descriptor_present: has_contract_descriptor(context.descriptor),
  )
}

fn output_spec_to_dto(
  output: workflow_contract.OutputSpec,
) -> types.WorkflowContractSpecDto {
  types.WorkflowContractSpecDto(
    name: output.name,
    type_: workflow_contract.type_to_string(output.type_),
    description: option.map(output.description, cap_structured_text),
    source: contract_source_requirement_to_dto(
      output.source,
      output_source_kind,
    ),
    descriptor_present: has_contract_descriptor(output.descriptor),
  )
}

fn contract_source_requirement_to_dto(
  requirement: workflow_contract.SourceRequirement(source),
  source_kind: fn(source) -> String,
) -> types.WorkflowContractSourceDto {
  types.WorkflowContractSourceDto(
    required: workflow_contract.requirement_required(requirement),
    kind: workflow_contract.requirement_source(requirement)
      |> option.map(source_kind),
  )
}

fn has_contract_descriptor(
  descriptor: Option(workflow_contract.ContractDescriptorSpec),
) -> Bool {
  case descriptor {
    Some(_) -> True
    None -> False
  }
}

fn input_source_kind(source: workflow_contract.InputSource) -> String {
  case source {
    workflow_contract.IssueContext -> "issue_context"
    workflow_contract.ScheduledContext -> "scheduled_context"
    workflow_contract.LiteralInput(_) -> "literal"
    workflow_contract.MappedOutputSource -> "mapped_output"
  }
}

fn context_source_kind(source: workflow_contract.ContextSource) -> String {
  case source {
    workflow_contract.WorkspaceDriverBase -> "workspace_driver_base"
    workflow_contract.LiteralContext(_) -> "literal"
    workflow_contract.MappedOutputContext -> "mapped_output"
  }
}

fn output_source_kind(source: workflow_contract.OutputSource) -> String {
  case source {
    workflow_contract.StepField(_, _) -> "field"
    workflow_contract.StepFile(_, _) -> "file"
    workflow_contract.StructuredOutput(_, _) -> "structured_output"
    workflow_contract.StaticUrl(_) -> "url"
    workflow_contract.StaticGitRef(_) -> "git_ref"
    workflow_contract.InlineJson(_, _) -> "inline_json"
  }
}

fn steps_from_dag(
  defaults: model_config.Settings,
  dag: workflow_dag.WorkflowDag,
) -> List(types.WorkflowStepDto) {
  workflow_dag.steps(dag)
  |> list.map(fn(step) { step_to_dto(defaults, dag, step) })
}

fn step_to_dto(
  defaults: model_config.Settings,
  dag: workflow_dag.WorkflowDag,
  step: workflow_dag.WorkflowStep,
) -> types.WorkflowStepDto {
  let #(model, command, agent) = step_payload_to_dto(defaults, step)
  types.WorkflowStepDto(
    id: step.id,
    kind: step_kind_to_string(step.kind),
    depends_on: step.depends_on,
    on_failure: failure_policy_to_string(step.on_failure),
    model: model,
    recovery: step_recovery_to_dto(dag, step),
    command: command,
    agent: agent,
  )
}

fn step_payload_to_dto(
  defaults: model_config.Settings,
  step: workflow_dag.WorkflowStep,
) -> #(
  Option(types.WorkflowModelSettingsDto),
  Option(types.WorkflowCommandStepDto),
  Option(types.WorkflowAgentStepDto),
) {
  case step.kind {
    workflow_dag.CommandStep(run, timeout_ms) -> #(
      None,
      Some(types.WorkflowCommandStepDto(
        run: cap_structured_text(run),
        timeout_ms: timeout_ms,
      )),
      None,
    )
    workflow_dag.AgentStep(prompt, structured_output) -> #(
      Some(
        model_config.resolve(defaults, step.model_settings)
        |> model_settings_to_dto,
      ),
      None,
      Some(types.WorkflowAgentStepDto(
        prompt: prompt_ref_to_dto(prompt),
        structured_output: option.map(
          structured_output,
          structured_output_to_dto,
        ),
      )),
    )
  }
}

fn prompt_ref_to_dto(
  prompt: workflow_dag.PromptRef,
) -> types.WorkflowPromptRefDto {
  case prompt {
    workflow_dag.PromptFile(path) ->
      types.WorkflowPromptRefDto(kind: "file", ref: Some(path))
    workflow_dag.PromptResolvedFile(path, _) ->
      types.WorkflowPromptRefDto(kind: "file", ref: Some(path))
    workflow_dag.PromptInline(_) ->
      types.WorkflowPromptRefDto(kind: "inline", ref: None)
  }
}

fn structured_output_to_dto(
  spec: workflow_dag.StructuredOutputSpec,
) -> types.WorkflowStructuredOutputDto {
  types.WorkflowStructuredOutputDto(
    artifact_name: spec.artifact_name,
    required: spec.required,
    validators: list.map(spec.validators, structured_validator_to_dto),
    validation_retries: spec.validation_retries,
  )
}

fn structured_validator_to_dto(
  validator: workflow_dag.StructuredOutputValidator,
) -> types.WorkflowStructuredOutputValidatorDto {
  types.WorkflowStructuredOutputValidatorDto(
    name: workflow_dag.structured_output_validator_name(validator),
    kind: workflow_dag.structured_output_validator_type_to_string(validator),
  )
}

fn step_recovery_to_dto(
  dag: workflow_dag.WorkflowDag,
  step: workflow_dag.WorkflowStep,
) -> Option(types.WorkflowRecoveryDto) {
  case workflow_dag.effective_recovery_config(dag, step) {
    Ok(Some(workflow_dag.EffectiveRecoveryConfig(attempts, model, prompt))) ->
      Some(types.WorkflowRecoveryDto(
        attempts: attempts,
        model: model,
        prompt: prompt_ref_to_dto(prompt),
      ))
    Ok(None) -> None
    Error(error) -> recovery_error_to_none(error)
  }
}

fn recovery_error_to_none(
  error: workflow_dag.DagError,
) -> Option(types.WorkflowRecoveryDto) {
  let workflow_dag.DagError(code: _code, message: _message) = error
  None
}

fn failure_policy_to_string(policy: workflow_dag.FailurePolicy) -> String {
  case policy {
    workflow_dag.FailWorkflow -> "fail"
    workflow_dag.ContinueWorkflow -> "continue"
  }
}

fn publications_from_dag(
  dag: workflow_dag.WorkflowDag,
) -> List(types.WorkflowPublicationDto) {
  workflow_dag.publication_routes(dag)
  |> list.map(publication_to_dto)
}

fn publication_to_dto(
  route: artifact_publication_config.PublicationRoute,
) -> types.WorkflowPublicationDto {
  types.WorkflowPublicationDto(
    id: route.id,
    repository: route.repository,
    required: route.required,
    mode: publication_mode_to_string(route.publication),
  )
}

fn publication_mode_to_string(
  publication: artifact_publication_config.PublicationRoutePublication,
) -> String {
  case publication {
    artifact_publication_config.FilePublicationRoute(_) -> "file"
    artifact_publication_config.CommitStackPublicationRoute(_) -> "commit_stack"
  }
}

fn next_actions_from_dag(
  dag: workflow_dag.WorkflowDag,
) -> List(types.WorkflowNextActionDto) {
  case workflow_dag.workstream_phase(dag) {
    None -> []
    Some(metadata) -> list.map(metadata.next_actions, next_action_to_dto)
  }
}

fn next_action_to_dto(
  action: phase_metadata.PhaseNextAction,
) -> types.WorkflowNextActionDto {
  types.WorkflowNextActionDto(
    action_id: action.action_id,
    workflow_id: action.workflow_id,
    requires_gate: action.requires_gate,
    auto_enqueue: action.auto_enqueue,
  )
}

fn bool_option_default(value: Option(Bool), default: Bool) -> Bool {
  case value {
    Some(value) -> value
    None -> default
  }
}

fn int_option_default(value: Option(Int), default: Int) -> Int {
  case value {
    Some(value) -> value
    None -> default
  }
}

fn graph_from_dag(dag: workflow_dag.WorkflowDag) -> types.WorkflowGraphDto {
  types.WorkflowGraphDto(
    nodes: workflow_dag.steps(dag) |> list.map(graph_node_from_step),
    edges: workflow_dag.steps(dag) |> list.flat_map(graph_edges_from_step),
  )
}

fn graph_node_from_step(
  step: workflow_dag.WorkflowStep,
) -> types.WorkflowGraphNodeDto {
  types.WorkflowGraphNodeDto(
    id: step.id,
    label: step.id,
    kind: step_kind_to_string(step.kind),
  )
}

fn graph_edges_from_step(
  step: workflow_dag.WorkflowStep,
) -> List(types.WorkflowGraphEdgeDto) {
  step.depends_on
  |> list.map(fn(dependency) {
    types.WorkflowGraphEdgeDto(from: dependency, to: step.id)
  })
}

fn step_kind_to_string(kind: workflow_dag.StepKind) -> String {
  case kind {
    workflow_dag.AgentStep(_, _) -> "agent"
    workflow_dag.CommandStep(_, _) -> "command"
  }
}

fn display_roots(state: workflow_reloader.State) -> List(String) {
  [
    state.bundle.effective.workspace.root,
    state.bundle.orchestrator.config_dir,
  ]
}

fn safe_relative_for_roots(
  target: String,
  roots: List(String),
) -> Option(String) {
  case roots {
    [] -> None
    [root, ..rest] -> {
      let root_abs = root |> path.absolute_or_original |> trim_trailing_slash
      case path.contains(root_abs, target) {
        True -> {
          let relative = case target == root_abs {
            True -> "."
            False -> string.drop_start(target, string.length(root_abs) + 1)
          }
          case safe_relative_output(relative) {
            True -> Some(relative)
            False -> safe_relative_for_roots(target, rest)
          }
        }
        False -> safe_relative_for_roots(target, rest)
      }
    }
  }
}

fn safe_relative_output(relative: String) -> Bool {
  relative != ""
  && !string.starts_with(relative, "/")
  && !path.has_parent_segment(relative)
  && !path.contains_control_character(relative)
}

fn external_relative_path(raw_path: String) -> String {
  "external/"
  <> hash.short_sha256_hex(raw_path, 12)
  <> "-"
  <> safe_leaf(raw_path)
}

fn safe_leaf(raw_path: String) -> String {
  let leaf = path_leaf(raw_path)
  let sanitized =
    leaf
    |> string.to_graphemes
    |> list.map(fn(ch) {
      case safe_leaf_char(ch) {
        True -> ch
        False -> "-"
      }
    })
    |> string.concat
  case sanitized == "" {
    True -> "file"
    False -> sanitized
  }
}

fn path_leaf(raw_path: String) -> String {
  raw_path
  |> string.replace(each: "\\", with: "/")
  |> string.split(on: "/")
  |> list.filter(fn(part) { part != "" })
  |> last_string("file")
}

fn last_string(values: List(String), fallback: String) -> String {
  case values {
    [] -> fallback
    [value] -> value
    [_, ..rest] -> last_string(rest, fallback)
  }
}

fn safe_leaf_char(ch: String) -> Bool {
  is_between(ch, "a", "z")
  || is_between(ch, "A", "Z")
  || is_between(ch, "0", "9")
  || ch == "."
  || ch == "_"
  || ch == "-"
}

fn is_between(value: String, low: String, high: String) -> Bool {
  string.compare(value, low) != Lt && string.compare(value, high) != Gt
}

fn label_for_workflow(prefix: String, workflow_id: String) -> Option(String) {
  case prefix == "" {
    True -> None
    False -> Some(prefix <> workflow_id)
  }
}

fn reload_status_to_string(status: config.ReloadStatus) -> String {
  case status {
    config.CurrentValid -> "valid"
    config.CurrentInvalid(_) -> "reload_error"
  }
}

fn normalize_workflow_id(value: String) -> String {
  string.trim(value)
}

fn is_yaml_path(value: String) -> Bool {
  let lower = value |> string.lowercase
  string.ends_with(lower, ".yaml") || string.ends_with(lower, ".yml")
}

fn dedupe_strings(values: List(String)) -> List(String) {
  dedupe_strings_loop(values, []) |> list.reverse
}

fn dedupe_strings_loop(
  values: List(String),
  seen: List(String),
) -> List(String) {
  case values {
    [] -> seen
    [value, ..rest] ->
      case list.contains(seen, value) {
        True -> dedupe_strings_loop(rest, seen)
        False -> dedupe_strings_loop(rest, [value, ..seen])
      }
  }
}

fn compare_workflow_summary(
  left: types.WorkflowSummaryDto,
  right: types.WorkflowSummaryDto,
) -> Order {
  string.compare(left.id, right.id)
}

fn compare_dependency_path(
  left: runtime_bundle.BundleDependency,
  right: runtime_bundle.BundleDependency,
) -> Order {
  string.compare(left.path, right.path)
}

fn trim_trailing_slash(value: String) -> String {
  case value != "/" && string.ends_with(value, "/") {
    True -> string.drop_end(value, 1)
    False -> value
  }
}
