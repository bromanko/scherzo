import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order.{Gt, Lt}
import gleam/result
import gleam/string
import scherzo/model_config
import yay

pub type WorkflowDag {
  WorkflowDag(
    id: String,
    description: Option(String),
    max_parallel_steps: Int,
    steps: List(WorkflowStep),
  )
}

pub type WorkflowStep {
  WorkflowStep(
    id: String,
    kind: StepKind,
    depends_on: List(String),
    workspace: WorkspaceRef,
    on_failure: FailurePolicy,
    model_settings: model_config.Settings,
  )
}

pub type StepKind {
  AgentStep(prompt: PromptRef)
  CommandStep(run: String, timeout_ms: Option(Int))
}

pub type PromptRef {
  PromptFile(String)
  PromptInline(String)
}

pub type WorkspaceRef {
  WorkspaceRef(name: String, from: Option(String))
}

pub type FailurePolicy {
  FailWorkflow
  ContinueWorkflow
}

pub type DagError {
  DagError(code: String, message: String)
}

pub fn parse(content: String) -> Result(WorkflowDag, DagError) {
  case yay.parse_string(content) {
    Error(_) -> Error(DagError("yaml_parse_error", "YAML parse error"))
    Ok([document]) -> parse_root(yay.document_root(document))
    Ok(_) -> Error(DagError("multiple_documents", "expected one YAML document"))
  }
}

pub fn parse_root(root: yay.Node) -> Result(WorkflowDag, DagError) {
  case root {
    yay.NodeMap(_) -> {
      use _ <- result.try(require_version(root))
      use id <- result.try(required_string(root, "id", "missing_workflow_id"))
      use _ <- result.try(validate_workflow_id(id))
      use max_parallel_steps <- result.try(read_max_parallel_steps(root))
      use steps <- result.try(read_steps(root))
      let dag =
        WorkflowDag(
          id: id,
          description: optional_string(root, "description"),
          max_parallel_steps: max_parallel_steps,
          steps: steps,
        )
      validate(dag)
    }
    _ -> Error(DagError("root_not_map", "workflow DAG must be a map"))
  }
}

pub fn step_by_id(dag: WorkflowDag, id: String) -> Result(WorkflowStep, Nil) {
  case dag.steps {
    [] -> Error(Nil)
    steps ->
      steps
      |> list.find(fn(step) { step.id == id })
  }
}

pub fn terminal_step(dag: WorkflowDag) -> Option(WorkflowStep) {
  case terminal_steps(dag.steps) {
    [step] -> Some(step)
    _ -> None
  }
}

pub fn prompt_file_path(step: WorkflowStep) -> Option(String) {
  case step.kind {
    AgentStep(PromptFile(path)) -> Some(path)
    _ -> None
  }
}

pub fn with_prompt(step: WorkflowStep, prompt: PromptRef) -> WorkflowStep {
  case step.kind {
    AgentStep(_) -> WorkflowStep(..step, kind: AgentStep(prompt))
    _ -> step
  }
}

fn validate(dag: WorkflowDag) -> Result(WorkflowDag, DagError) {
  use _ <- result.try(validate_unique_step_ids(dag.steps))
  use _ <- result.try(validate_dependencies_exist(dag.steps))
  use _ <- result.try(validate_acyclic(dag.steps))
  use _ <- result.try(validate_workspace_sources(dag.steps))
  use _ <- result.try(validate_single_terminal_sink(dag.steps))
  Ok(dag)
}

fn require_version(root: yay.Node) -> Result(Nil, DagError) {
  case get_node(root, "version") {
    Some(yay.NodeInt(1)) -> Ok(Nil)
    Some(_) -> Error(DagError("invalid_version", "version must be 1"))
    None -> Error(DagError("missing_version", "version is required"))
  }
}

fn read_max_parallel_steps(root: yay.Node) -> Result(Int, DagError) {
  let value = optional_int(root, "max_parallel_steps") |> option.unwrap(1)
  case value >= 1 {
    True -> Ok(value)
    False ->
      Error(DagError(
        "invalid_max_parallel_steps",
        "max_parallel_steps must be at least 1",
      ))
  }
}

fn read_steps(root: yay.Node) -> Result(List(WorkflowStep), DagError) {
  case get_node(root, "steps") {
    Some(yay.NodeSeq(values)) -> read_step_list(values, [])
    Some(_) -> Error(DagError("steps_not_list", "steps must be a list"))
    None -> Error(DagError("missing_steps", "steps is required"))
  }
}

fn read_step_list(
  values: List(yay.Node),
  acc: List(WorkflowStep),
) -> Result(List(WorkflowStep), DagError) {
  case values {
    [] -> Ok(list.reverse(acc))
    [value, ..rest] -> {
      use step <- result.try(read_step(value))
      read_step_list(rest, [step, ..acc])
    }
  }
}

fn read_step(node: yay.Node) -> Result(WorkflowStep, DagError) {
  case node {
    yay.NodeMap(_) -> {
      use id <- result.try(required_string(node, "id", "missing_step_id"))
      use _ <- result.try(validate_step_id(id))
      use kind <- result.try(read_step_kind(node))
      use depends_on <- result.try(read_depends_on(node))
      use workspace <- result.try(read_workspace(node))
      use on_failure <- result.try(read_failure_policy(node))
      use model_settings <- result.try(read_model_settings(kind, node))
      Ok(WorkflowStep(
        id: id,
        kind: kind,
        depends_on: depends_on,
        workspace: workspace,
        on_failure: on_failure,
        model_settings: model_settings,
      ))
    }
    _ -> Error(DagError("step_not_map", "each step must be a map"))
  }
}

fn read_step_kind(node: yay.Node) -> Result(StepKind, DagError) {
  let kind = optional_string(node, "kind")
  case kind {
    Some(raw) -> {
      case string.trim(raw) |> string.lowercase {
        "agent" -> {
          use prompt <- result.try(required_string(
            node,
            "prompt",
            "missing_prompt",
          ))
          Ok(AgentStep(PromptFile(prompt)))
        }
        "command" -> {
          use run <- result.try(required_string(node, "run", "missing_run"))
          Ok(CommandStep(run: run, timeout_ms: optional_int(node, "timeout_ms")))
        }
        other ->
          Error(DagError("unknown_step_kind", "unknown step kind: " <> other))
      }
    }
    None -> infer_step_kind(node)
  }
}

fn infer_step_kind(node: yay.Node) -> Result(StepKind, DagError) {
  case optional_string(node, "prompt"), optional_string(node, "run") {
    Some(prompt), None -> Ok(AgentStep(PromptFile(prompt)))
    None, Some(run) ->
      Ok(CommandStep(run: run, timeout_ms: optional_int(node, "timeout_ms")))
    Some(_), Some(_) ->
      Error(DagError(
        "ambiguous_step_kind",
        "step cannot have both prompt and run without kind",
      ))
    None, None -> Error(DagError("missing_step_kind", "step kind is required"))
  }
}

fn read_depends_on(node: yay.Node) -> Result(List(String), DagError) {
  case get_node(node, "depends_on") {
    None -> Ok([])
    Some(yay.NodeSeq(values)) -> read_string_seq(values, "depends_on", [])
    Some(_) ->
      Error(DagError("depends_on_not_list", "depends_on must be a list"))
  }
}

fn read_workspace(node: yay.Node) -> Result(WorkspaceRef, DagError) {
  case get_node(node, "workspace") {
    None -> Ok(WorkspaceRef(name: "main", from: None))
    Some(yay.NodeStr(name)) -> {
      use _ <- result.try(validate_workspace_name(name))
      Ok(WorkspaceRef(name: name, from: None))
    }
    Some(workspace_node) ->
      case workspace_node {
        yay.NodeMap(_) -> {
          use name <- result.try(required_string(
            workspace_node,
            "name",
            "missing_workspace_name",
          ))
          use _ <- result.try(validate_workspace_name(name))
          use source <- result.try(read_workspace_source(workspace_node))
          Ok(WorkspaceRef(name: name, from: source))
        }
        _ ->
          Error(DagError(
            "workspace_invalid",
            "workspace must be a string or map",
          ))
      }
  }
}

fn read_workspace_source(node: yay.Node) -> Result(Option(String), DagError) {
  case optional_string(node, "from") {
    None -> Ok(None)
    Some(source) -> {
      use _ <- result.try(validate_workspace_name(source))
      Ok(Some(source))
    }
  }
}

fn read_failure_policy(node: yay.Node) -> Result(FailurePolicy, DagError) {
  case optional_string(node, "on_failure") {
    None -> Ok(FailWorkflow)
    Some(value) ->
      case string.trim(value) |> string.lowercase {
        "fail" -> Ok(FailWorkflow)
        "continue" -> Ok(ContinueWorkflow)
        other ->
          Error(DagError(
            "invalid_on_failure",
            "unknown failure policy: " <> other,
          ))
      }
  }
}

fn read_model_settings(
  kind: StepKind,
  node: yay.Node,
) -> Result(model_config.Settings, DagError) {
  case kind {
    AgentStep(_) -> read_agent_model_settings(node)
    CommandStep(_, _) -> reject_command_model_settings(node)
  }
}

fn read_agent_model_settings(
  node: yay.Node,
) -> Result(model_config.Settings, DagError) {
  model_config.read_settings(
    node,
    model_config.SettingsPaths(
      provider_path: "step.provider",
      provider_model_path: "model",
      model_path: "step.model",
      thinking_path: "step.thinking",
    ),
    fn(code, message) { DagError(code, message) },
  )
}

fn reject_command_model_settings(
  node: yay.Node,
) -> Result(model_config.Settings, DagError) {
  case first_model_settings_field(node) {
    None -> Ok(model_config.default_settings())
    Some(field) ->
      Error(DagError(
        "model_settings_on_command_step",
        "command steps do not run pi, so "
          <> field
          <> " is not supported; move model/thinking settings to an agent step",
      ))
  }
}

fn first_model_settings_field(node: yay.Node) -> Option(String) {
  case get_node(node, "provider") {
    Some(_) -> Some("provider")
    None ->
      case get_node(node, "model") {
        Some(_) -> Some("model")
        None ->
          case get_node(node, "thinking") {
            Some(_) -> Some("thinking")
            None -> None
          }
      }
  }
}

fn validate_unique_step_ids(
  steps: List(WorkflowStep),
) -> Result(Nil, DagError) {
  validate_unique_step_ids_loop(steps, [])
}

fn validate_unique_step_ids_loop(
  steps: List(WorkflowStep),
  seen: List(String),
) -> Result(Nil, DagError) {
  case steps {
    [] -> Ok(Nil)
    [step, ..rest] ->
      case list.contains(seen, step.id) {
        True ->
          Error(DagError("duplicate_step_id", "duplicate step id: " <> step.id))
        False -> validate_unique_step_ids_loop(rest, [step.id, ..seen])
      }
  }
}

fn validate_dependencies_exist(
  steps: List(WorkflowStep),
) -> Result(Nil, DagError) {
  let ids = list.map(steps, fn(step) { step.id })
  validate_dependencies_exist_loop(steps, ids)
}

fn validate_dependencies_exist_loop(
  steps: List(WorkflowStep),
  ids: List(String),
) -> Result(Nil, DagError) {
  case steps {
    [] -> Ok(Nil)
    [step, ..rest] -> {
      use _ <- result.try(validate_dependency_ids(step.depends_on, ids, step.id))
      validate_dependencies_exist_loop(rest, ids)
    }
  }
}

fn validate_dependency_ids(
  deps: List(String),
  ids: List(String),
  step_id: String,
) -> Result(Nil, DagError) {
  case deps {
    [] -> Ok(Nil)
    [dep, ..rest] ->
      case list.contains(ids, dep) {
        True -> validate_dependency_ids(rest, ids, step_id)
        False ->
          Error(DagError(
            "missing_dependency",
            step_id <> " depends on unknown step " <> dep,
          ))
      }
  }
}

fn validate_acyclic(steps: List(WorkflowStep)) -> Result(Nil, DagError) {
  let by_id = steps_dict(steps)
  validate_acyclic_loop(steps, by_id)
}

fn validate_acyclic_loop(
  steps: List(WorkflowStep),
  by_id: Dict(String, WorkflowStep),
) -> Result(Nil, DagError) {
  case steps {
    [] -> Ok(Nil)
    [step, ..rest] -> {
      use _ <- result.try(detect_cycle(step.id, by_id, []))
      validate_acyclic_loop(rest, by_id)
    }
  }
}

fn detect_cycle(
  step_id: String,
  by_id: Dict(String, WorkflowStep),
  path: List(String),
) -> Result(Nil, DagError) {
  case list.contains(path, step_id) {
    True -> Error(DagError("cycle", "cycle includes step " <> step_id))
    False -> {
      case dict.get(by_id, step_id) {
        Error(_) -> Ok(Nil)
        Ok(step) -> detect_cycle_deps(step.depends_on, by_id, [step_id, ..path])
      }
    }
  }
}

fn detect_cycle_deps(
  deps: List(String),
  by_id: Dict(String, WorkflowStep),
  path: List(String),
) -> Result(Nil, DagError) {
  case deps {
    [] -> Ok(Nil)
    [dep, ..rest] -> {
      use _ <- result.try(detect_cycle(dep, by_id, path))
      detect_cycle_deps(rest, by_id, path)
    }
  }
}

fn validate_workspace_sources(
  steps: List(WorkflowStep),
) -> Result(Nil, DagError) {
  let by_id = steps_dict(steps)
  validate_workspace_sources_loop(steps, by_id)
}

fn validate_workspace_sources_loop(
  steps: List(WorkflowStep),
  by_id: Dict(String, WorkflowStep),
) -> Result(Nil, DagError) {
  case steps {
    [] -> Ok(Nil)
    [step, ..rest] -> {
      case step.workspace.from {
        None -> validate_workspace_sources_loop(rest, by_id)
        Some(source) -> {
          let dep_ids = transitive_dependency_ids(step.depends_on, by_id, [])
          let workspaces = workspace_names_for(dep_ids, by_id, [])
          case list.contains(workspaces, source) {
            True -> validate_workspace_sources_loop(rest, by_id)
            False ->
              Error(DagError(
                "invalid_workspace_from",
                step.id
                  <> " derives from workspace not produced by transitive dependency: "
                  <> source,
              ))
          }
        }
      }
    }
  }
}

fn transitive_dependency_ids(
  deps: List(String),
  by_id: Dict(String, WorkflowStep),
  seen: List(String),
) -> List(String) {
  case deps {
    [] -> seen
    [dep, ..rest] -> {
      case list.contains(seen, dep) {
        True -> transitive_dependency_ids(rest, by_id, seen)
        False -> {
          let seen = [dep, ..seen]
          let seen = case dict.get(by_id, dep) {
            Ok(step) -> transitive_dependency_ids(step.depends_on, by_id, seen)
            Error(_) -> seen
          }
          transitive_dependency_ids(rest, by_id, seen)
        }
      }
    }
  }
}

fn workspace_names_for(
  ids: List(String),
  by_id: Dict(String, WorkflowStep),
  acc: List(String),
) -> List(String) {
  case ids {
    [] -> acc
    [id, ..rest] -> {
      let acc = case dict.get(by_id, id) {
        Ok(step) -> [step.workspace.name, ..acc]
        Error(_) -> acc
      }
      workspace_names_for(rest, by_id, acc)
    }
  }
}

fn validate_single_terminal_sink(
  steps: List(WorkflowStep),
) -> Result(Nil, DagError) {
  case terminal_steps(steps) {
    [] -> Ok(Nil)
    [_] -> Ok(Nil)
    sinks -> {
      let ids =
        sinks |> list.map(fn(step) { step.id }) |> string.join(with: ", ")
      Error(DagError(
        "multiple_terminal_steps",
        "workflow DAG must have exactly one terminal step; terminal steps: "
          <> ids,
      ))
    }
  }
}

fn terminal_steps(steps: List(WorkflowStep)) -> List(WorkflowStep) {
  steps
  |> list.filter(fn(step) { !has_dependent(steps, step.id) })
}

fn has_dependent(steps: List(WorkflowStep), step_id: String) -> Bool {
  case steps {
    [] -> False
    [step, ..rest] ->
      list.contains(step.depends_on, step_id) || has_dependent(rest, step_id)
  }
}

fn steps_dict(steps: List(WorkflowStep)) -> Dict(String, WorkflowStep) {
  steps
  |> list.map(fn(step) { #(step.id, step) })
  |> dict.from_list
}

fn validate_workflow_id(id: String) -> Result(Nil, DagError) {
  case valid_workflow_or_workspace_id(id) {
    True -> Ok(Nil)
    False ->
      Error(DagError("invalid_workflow_id", "invalid workflow id: " <> id))
  }
}

fn validate_step_id(id: String) -> Result(Nil, DagError) {
  case valid_step_id(id) {
    True -> Ok(Nil)
    False -> Error(DagError("invalid_step_id", "invalid step id: " <> id))
  }
}

fn validate_workspace_name(name: String) -> Result(Nil, DagError) {
  case valid_workflow_or_workspace_id(name) {
    True -> Ok(Nil)
    False ->
      Error(DagError(
        "invalid_workspace_name",
        "invalid workspace name: " <> name,
      ))
  }
}

fn valid_workflow_or_workspace_id(value: String) -> Bool {
  case string.to_graphemes(value) {
    [] -> False
    [first, ..rest] ->
      is_lower_or_digit(first) && all(rest, is_workflow_workspace_char)
  }
}

fn valid_step_id(value: String) -> Bool {
  case string.to_graphemes(value) {
    [] -> False
    [first, ..rest] -> is_lower(first) && all(rest, is_step_char)
  }
}

fn is_workflow_workspace_char(ch: String) -> Bool {
  is_lower_or_digit(ch) || ch == "_" || ch == "-"
}

fn is_step_char(ch: String) -> Bool {
  is_lower_or_digit(ch) || ch == "_"
}

fn is_lower_or_digit(ch: String) -> Bool {
  is_lower(ch) || is_digit(ch)
}

fn is_lower(ch: String) -> Bool {
  string.compare(ch, "a") != Lt && string.compare(ch, "z") != Gt
}

fn is_digit(ch: String) -> Bool {
  string.compare(ch, "0") != Lt && string.compare(ch, "9") != Gt
}

fn all(values: List(a), predicate: fn(a) -> Bool) -> Bool {
  case values {
    [] -> True
    [value, ..rest] -> predicate(value) && all(rest, predicate)
  }
}

fn read_string_seq(
  values: List(yay.Node),
  field: String,
  acc: List(String),
) -> Result(List(String), DagError) {
  case values {
    [] -> Ok(list.reverse(acc))
    [yay.NodeStr(value), ..rest] -> read_string_seq(rest, field, [value, ..acc])
    [_, ..] ->
      Error(DagError(
        field <> "_entry_not_string",
        field <> " entries must be strings",
      ))
  }
}

fn required_string(
  node: yay.Node,
  key: String,
  code: String,
) -> Result(String, DagError) {
  case optional_string(node, key) {
    Some(value) -> Ok(value)
    None -> Error(DagError(code, key <> " is required"))
  }
}

fn optional_string(node: yay.Node, key: String) -> Option(String) {
  case get_node(node, key) {
    Some(yay.NodeStr(value)) -> Some(value)
    _ -> None
  }
}

fn optional_int(node: yay.Node, key: String) -> Option(Int) {
  case get_node(node, key) {
    Some(yay.NodeInt(value)) -> Some(value)
    _ -> None
  }
}

fn get_node(node: yay.Node, key: String) -> Option(yay.Node) {
  case node {
    yay.NodeMap(pairs) ->
      case list.key_find(pairs, yay.NodeStr(key)) {
        Ok(value) -> Some(value)
        Error(_) -> None
      }
    _ -> None
  }
}
