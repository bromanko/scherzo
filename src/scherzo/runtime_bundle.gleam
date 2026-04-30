import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/config
import scherzo/domain
import scherzo/error
import scherzo/path
import scherzo/workflow
import scherzo/workflow_dag
import simplifile
import yay

pub type BundleMode {
  LegacyMarkdown
  OrchestratorYaml
}

pub type RuntimeBundle {
  RuntimeBundle(
    mode: BundleMode,
    config_path: String,
    config_contents: String,
    effective: domain.EffectiveConfig,
    orchestrator: Option(domain.OrchestratorConfig),
    workflows: Dict(String, workflow_dag.WorkflowDag),
    legacy_workflow: Option(domain.WorkflowDefinition),
    secrets: List(String),
  )
}

pub type BundleError {
  BundleError(code: String, message: String)
}

pub fn select_workflow(
  bundle: RuntimeBundle,
  issue: domain.Issue,
) -> Result(#(String, workflow_dag.WorkflowDag), BundleError) {
  case bundle.mode, bundle.orchestrator {
    LegacyMarkdown, _ ->
      case dict.get(bundle.workflows, "legacy") {
        Ok(dag) -> Ok(#("legacy", dag))
        Error(_) ->
          Error(BundleError("missing_workflow", "legacy workflow is missing"))
      }
    OrchestratorYaml, Some(orchestrator) ->
      select_routed_workflow(bundle.workflows, orchestrator.routing, issue)
    OrchestratorYaml, None ->
      Error(BundleError(
        "missing_orchestrator_config",
        "orchestrator config is missing",
      ))
  }
}

pub fn load(explicit: Option(String)) -> Result(RuntimeBundle, BundleError) {
  load_with_env(explicit, real_env)
}

pub fn load_with_env(
  explicit: Option(String),
  env: config.Env,
) -> Result(RuntimeBundle, BundleError) {
  let selected = select_config_path(explicit)
  case path_kind(selected) {
    Some(LegacyMarkdown) -> load_legacy(selected, env)
    Some(OrchestratorYaml) -> load_orchestrator(selected, env)
    None ->
      Error(BundleError(
        "unsupported_config_path",
        "workflow path must end in .md, .yaml, or .yml",
      ))
  }
}

fn select_routed_workflow(
  workflows: Dict(String, workflow_dag.WorkflowDag),
  routing: domain.RoutingConfig,
  issue: domain.Issue,
) -> Result(#(String, workflow_dag.WorkflowDag), BundleError) {
  let labels = workflow_labels(issue.labels, routing.workflow_label_prefix, [])
  case labels {
    [] ->
      case routing.require_exactly_one_workflow_label {
        True ->
          Error(BundleError(
            "missing_workflow_label",
            "issue has no workflow label",
          ))
        False ->
          case routing.default_workflow {
            Some(id) -> lookup_workflow(workflows, id)
            None ->
              Error(BundleError(
                "missing_workflow_label",
                "issue has no workflow label",
              ))
          }
      }
    [id] -> lookup_workflow(workflows, id)
    _ ->
      Error(BundleError(
        "multiple_workflow_labels",
        "issue has multiple workflow labels",
      ))
  }
}

fn lookup_workflow(
  workflows: Dict(String, workflow_dag.WorkflowDag),
  id: String,
) -> Result(#(String, workflow_dag.WorkflowDag), BundleError) {
  case dict.get(workflows, id) {
    Ok(dag) -> Ok(#(id, dag))
    Error(_) ->
      Error(BundleError(
        "unknown_workflow_label",
        "unknown workflow label: " <> id,
      ))
  }
}

fn workflow_labels(
  labels: List(String),
  prefix: String,
  acc: List(String),
) -> List(String) {
  case labels {
    [] -> list.reverse(acc)
    [label, ..rest] -> {
      let label = label |> string.trim |> string.lowercase
      case prefix != "" && string.starts_with(label, prefix) {
        True ->
          workflow_labels(rest, prefix, [
            string.drop_start(label, string.length(prefix)),
            ..acc
          ])
        False -> workflow_labels(rest, prefix, acc)
      }
    }
  }
}

fn load_legacy(
  selected: String,
  env: config.Env,
) -> Result(RuntimeBundle, BundleError) {
  use content <- result_try(read_file(selected, "missing_workflow_file"))
  use definition <- result_try(workflow.parse(content) |> map_workflow_error)
  use effective <- result_try(
    config.resolve_with_env(definition, selected, env)
    |> map_config_error,
  )
  let dag = workflow_dag.legacy_inline("legacy", definition.prompt_template)
  Ok(RuntimeBundle(
    mode: LegacyMarkdown,
    config_path: selected,
    config_contents: content,
    effective: effective,
    orchestrator: None,
    workflows: dict.from_list([#("legacy", dag)]),
    legacy_workflow: Some(definition),
    secrets: config.resolved_secrets(effective),
  ))
}

fn load_orchestrator(
  selected: String,
  env: config.Env,
) -> Result(RuntimeBundle, BundleError) {
  use content <- result_try(read_file(selected, "missing_config_file"))
  use root <- result_try(parse_yaml_root(content))
  use orchestrator <- result_try(
    config.resolve_orchestrator_root(root, selected, env)
    |> map_config_error,
  )
  use workflows <- result_try(load_workflow_map(
    dict.to_list(orchestrator.routing.workflows),
    dict.new(),
  ))
  Ok(RuntimeBundle(
    mode: OrchestratorYaml,
    config_path: selected,
    config_contents: content,
    effective: orchestrator.effective,
    orchestrator: Some(orchestrator),
    workflows: workflows,
    legacy_workflow: None,
    secrets: config.resolved_secrets(orchestrator.effective),
  ))
}

fn load_workflow_map(
  entries: List(#(String, String)),
  acc: Dict(String, workflow_dag.WorkflowDag),
) -> Result(Dict(String, workflow_dag.WorkflowDag), BundleError) {
  case entries {
    [] -> Ok(acc)
    [#(id, workflow_path), ..rest] -> {
      use dag <- result_try(load_workflow_dag(workflow_path))
      case dag.id == id {
        True -> load_workflow_map(rest, dict.insert(acc, id, dag))
        False ->
          Error(BundleError(
            "workflow_id_mismatch",
            "routing key " <> id <> " points to workflow id " <> dag.id,
          ))
      }
    }
  }
}

fn load_workflow_dag(
  workflow_path: String,
) -> Result(workflow_dag.WorkflowDag, BundleError) {
  use content <- result_try(read_file(workflow_path, "missing_workflow_file"))
  use dag <- result_try(workflow_dag.parse(content) |> map_dag_error)
  resolve_prompt_files(dag, workflow_path)
}

fn resolve_prompt_files(
  dag: workflow_dag.WorkflowDag,
  workflow_path: String,
) -> Result(workflow_dag.WorkflowDag, BundleError) {
  use steps <- result_try(resolve_step_prompts(dag.steps, workflow_path, []))
  Ok(workflow_dag.WorkflowDag(..dag, steps: steps))
}

fn resolve_step_prompts(
  steps: List(workflow_dag.WorkflowStep),
  workflow_path: String,
  acc: List(workflow_dag.WorkflowStep),
) -> Result(List(workflow_dag.WorkflowStep), BundleError) {
  case steps {
    [] -> Ok(list.reverse(acc))
    [step, ..rest] -> {
      case step.kind {
        workflow_dag.AgentStep(workflow_dag.PromptFile(prompt_path)) -> {
          use prompt <- result_try(read_relative_prompt(
            prompt_path,
            workflow_path,
          ))
          let step =
            workflow_dag.WorkflowStep(
              ..step,
              kind: workflow_dag.AgentStep(workflow_dag.PromptInline(prompt)),
            )
          resolve_step_prompts(rest, workflow_path, [step, ..acc])
        }
        _ -> resolve_step_prompts(rest, workflow_path, [step, ..acc])
      }
    }
  }
}

fn read_relative_prompt(
  prompt_path: String,
  workflow_path: String,
) -> Result(String, BundleError) {
  case validate_relative_path(prompt_path, "invalid_prompt_path") {
    Error(err) -> Error(err)
    Ok(Nil) -> {
      let workflow_dir = path.dirname(workflow_path) |> result_unwrap(".")
      let full_path =
        path.join(workflow_dir, string.trim(prompt_path))
        |> path.absolute
        |> result_unwrap(path.join(workflow_dir, string.trim(prompt_path)))
      let workflow_dir_abs =
        path.absolute(workflow_dir) |> result_unwrap(workflow_dir)
      case path.contains(workflow_dir_abs, full_path) {
        False ->
          Error(BundleError(
            "invalid_prompt_path",
            "prompt path escapes workflow directory: " <> prompt_path,
          ))
        True -> read_file(full_path, "missing_prompt_file")
      }
    }
  }
}

fn validate_relative_path(
  value: String,
  code: String,
) -> Result(Nil, BundleError) {
  let trimmed = string.trim(value)
  case trimmed == "" {
    True -> Error(BundleError(code, "path must be non-empty"))
    False ->
      case string.starts_with(trimmed, "/") || has_parent_segment(trimmed) {
        True ->
          Error(BundleError(
            code,
            "path must be relative and must not contain ..",
          ))
        False -> Ok(Nil)
      }
  }
}

fn has_parent_segment(value: String) -> Bool {
  value == ".."
  || string.starts_with(value, "../")
  || string.ends_with(value, "/..")
  || string.contains(value, "/../")
}

fn parse_yaml_root(content: String) -> Result(yay.Node, BundleError) {
  case yay.parse_string(content) {
    Error(_) -> Error(BundleError("yaml_parse_error", "YAML parse error"))
    Ok([document]) -> Ok(yay.document_root(document))
    Ok(_) ->
      Error(BundleError("multiple_documents", "expected one YAML document"))
  }
}

fn read_file(path: String, code: String) -> Result(String, BundleError) {
  case simplifile.read(path) {
    Ok(content) -> Ok(content)
    Error(_) -> Error(BundleError(code, "could not read " <> path))
  }
}

fn select_config_path(explicit: Option(String)) -> String {
  case explicit {
    Some(path) -> path
    None -> default_config_path()
  }
}

fn default_config_path() -> String {
  case file_exists("WORKFLOW.md") {
    True -> "WORKFLOW.md"
    False ->
      case file_exists(".scherzo/scherzo.yaml") {
        True -> ".scherzo/scherzo.yaml"
        False ->
          case file_exists(".scherzo/scherzo.yml") {
            True -> ".scherzo/scherzo.yml"
            False ->
              case file_exists("scherzo.yaml") {
                True -> "scherzo.yaml"
                False ->
                  case file_exists("scherzo.yml") {
                    True -> "scherzo.yml"
                    False -> workflow.choose_path(None)
                  }
              }
          }
      }
  }
}

fn file_exists(path: String) -> Bool {
  case simplifile.is_file(path) {
    Ok(True) -> True
    _ -> False
  }
}

fn path_kind(path: String) -> Option(BundleMode) {
  let lower = string.lowercase(path)
  case string.ends_with(lower, ".md") {
    True -> Some(LegacyMarkdown)
    False ->
      case string.ends_with(lower, ".yaml") || string.ends_with(lower, ".yml") {
        True -> Some(OrchestratorYaml)
        False -> None
      }
  }
}

fn map_workflow_error(
  result: Result(a, error.WorkflowError),
) -> Result(a, BundleError) {
  case result {
    Ok(value) -> Ok(value)
    Error(err) -> Error(BundleError(error.workflow_code(err), "workflow error"))
  }
}

fn map_config_error(
  result: Result(a, error.ConfigError),
) -> Result(a, BundleError) {
  case result {
    Ok(value) -> Ok(value)
    Error(err) -> Error(BundleError(error.config_code(err), "config error"))
  }
}

fn map_dag_error(
  result: Result(a, workflow_dag.DagError),
) -> Result(a, BundleError) {
  case result {
    Ok(value) -> Ok(value)
    Error(workflow_dag.DagError(code, message)) ->
      Error(BundleError(code, message))
  }
}

fn real_env(name: String) -> Option(String) {
  path.env(name)
}

fn result_unwrap(result: Result(a, b), default: a) -> a {
  case result {
    Ok(value) -> value
    Error(_) -> default
  }
}

fn result_try(result: Result(a, e), next: fn(a) -> Result(b, e)) -> Result(b, e) {
  case result {
    Ok(value) -> next(value)
    Error(err) -> Error(err)
  }
}
