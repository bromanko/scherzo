import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/config
import scherzo/config/types as config_types
import scherzo/error
import scherzo/model_config
import scherzo/path
import scherzo/template
import scherzo/tracker/issue as tracker_issue
import scherzo/workflow_dag
import scherzo/workspace_profile
import simplifile
import yay

pub type RuntimeBundle {
  RuntimeBundle(
    config_path: String,
    config_contents: String,
    effective: config_types.EffectiveConfig,
    orchestrator: config_types.OrchestratorConfig,
    workflows: Dict(String, workflow_dag.WorkflowDag),
    secrets: List(String),
  )
}

pub type BundleError {
  BundleError(code: String, message: String)
}

pub fn select_workflow(
  bundle: RuntimeBundle,
  issue: tracker_issue.Issue,
) -> Result(#(String, workflow_dag.WorkflowDag), BundleError) {
  select_routed_workflow(bundle.workflows, bundle.orchestrator.routing, issue)
}

pub fn load(explicit: Option(String)) -> Result(RuntimeBundle, BundleError) {
  load_with_env(explicit, real_env)
}

pub fn load_with_env(
  explicit: Option(String),
  env: config.Env,
) -> Result(RuntimeBundle, BundleError) {
  let selected = select_config_path(explicit)
  case is_yaml_config_path(selected) {
    True -> load_orchestrator(selected, env)
    False ->
      Error(BundleError(
        "unsupported_config_path",
        "runtime config path must end in .yaml or .yml",
      ))
  }
}

fn select_routed_workflow(
  workflows: Dict(String, workflow_dag.WorkflowDag),
  routing: config_types.RoutingConfig,
  issue: tracker_issue.Issue,
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

fn load_orchestrator(
  selected: String,
  env: config.Env,
) -> Result(RuntimeBundle, BundleError) {
  use content <- result.try(read_file(selected, "missing_config_file"))
  use root <- result.try(parse_yaml_root(content))
  use orchestrator <- result.try(
    config.resolve_orchestrator_root(root, selected, env)
    |> map_config_error,
  )
  use workflows <- result.try(load_workflow_map(
    dict.to_list(orchestrator.routing.workflows),
    dict.new(),
  ))
  use _ <- result.try(validate_workspace_profiles(
    orchestrator,
    dict.to_list(workflows),
  ))
  use _ <- result.try(validate_workflow_model_settings(
    orchestrator.model_settings,
    dict.to_list(workflows),
  ))
  use _ <- result.try(validate_scheduled_workflows(
    orchestrator.scheduled_jobs,
    workflows,
  ))
  Ok(RuntimeBundle(
    config_path: selected,
    config_contents: content,
    effective: orchestrator.effective,
    orchestrator: orchestrator,
    workflows: workflows,
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
      use dag <- result.try(load_workflow_dag(workflow_path))
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
  use content <- result.try(read_file(workflow_path, "missing_workflow_file"))
  use dag <- result.try(workflow_dag.parse(content) |> map_dag_error)
  resolve_prompt_files(dag, workflow_path)
}

fn resolve_prompt_files(
  dag: workflow_dag.WorkflowDag,
  workflow_path: String,
) -> Result(workflow_dag.WorkflowDag, BundleError) {
  use steps <- result.try(resolve_step_prompts(dag.steps, workflow_path, []))
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
        workflow_dag.AgentStep(
          workflow_dag.PromptFile(prompt_path),
          structured_output,
        ) -> {
          use prompt <- result.try(read_relative_prompt(
            prompt_path,
            workflow_path,
          ))
          let step =
            workflow_dag.WorkflowStep(
              ..step,
              kind: workflow_dag.AgentStep(
                workflow_dag.PromptInline(prompt),
                structured_output,
              ),
            )
          resolve_step_prompts(rest, workflow_path, [step, ..acc])
        }
        _ -> resolve_step_prompts(rest, workflow_path, [step, ..acc])
      }
    }
  }
}

fn validate_workspace_profiles(
  orchestrator: config_types.OrchestratorConfig,
  workflows: List(#(String, workflow_dag.WorkflowDag)),
) -> Result(Nil, BundleError) {
  case workflows {
    [] -> Ok(Nil)
    [#(_, dag), ..rest] -> {
      use profile <- result.try(
        workspace_profile.resolve(dag, orchestrator)
        |> result.map_error(workspace_profile_error_to_bundle_error),
      )
      use _ <- result.try(
        workspace_profile.validate_capabilities(dag, profile)
        |> result.map_error(workspace_profile_error_to_bundle_error),
      )
      use _ <- result.try(
        workspace_profile.validate_dispatchable_profile(dag, profile)
        |> result.map_error(workspace_profile_error_to_bundle_error),
      )
      validate_workspace_profiles(orchestrator, rest)
    }
  }
}

fn workspace_profile_error_to_bundle_error(
  err: workspace_profile.ProfileResolutionError,
) -> BundleError {
  BundleError(
    workspace_profile.error_code(err),
    workspace_profile.error_message(err),
  )
}

fn validate_scheduled_workflows(
  jobs: List(config_types.ScheduledJobConfig),
  workflows: Dict(String, workflow_dag.WorkflowDag),
) -> Result(Nil, BundleError) {
  case jobs {
    [] -> Ok(Nil)
    [job, ..rest] ->
      case job.enabled {
        False -> validate_scheduled_workflows(rest, workflows)
        True ->
          case dict.get(workflows, job.workflow) {
            Error(_) ->
              Error(BundleError(
                "scheduled_workflow_missing",
                "scheduled job "
                  <> job.id
                  <> " references missing workflow "
                  <> job.workflow,
              ))
            Ok(dag) -> {
              use _ <- result.try(validate_scheduled_workflow(job, dag))
              validate_scheduled_workflows(rest, workflows)
            }
          }
      }
  }
}

fn validate_scheduled_workflow(
  job: config_types.ScheduledJobConfig,
  dag: workflow_dag.WorkflowDag,
) -> Result(Nil, BundleError) {
  validate_scheduled_steps(job, dag.id, dag.steps)
}

fn validate_scheduled_steps(
  job: config_types.ScheduledJobConfig,
  workflow_id: String,
  steps: List(workflow_dag.WorkflowStep),
) -> Result(Nil, BundleError) {
  case steps {
    [] -> Ok(Nil)
    [step, ..rest] -> {
      use _ <- result.try(validate_scheduled_step(job, workflow_id, step))
      validate_scheduled_steps(job, workflow_id, rest)
    }
  }
}

fn validate_scheduled_step(
  job: config_types.ScheduledJobConfig,
  workflow_id: String,
  step: workflow_dag.WorkflowStep,
) -> Result(Nil, BundleError) {
  let source = case step.kind {
    workflow_dag.AgentStep(workflow_dag.PromptInline(prompt), _) -> prompt
    workflow_dag.AgentStep(workflow_dag.PromptFile(path), _) -> path
    workflow_dag.CommandStep(run, _) -> run
  }
  case first_issue_reference(template.referenced_variables(source)) {
    None -> Ok(Nil)
    Some(variable) ->
      Error(BundleError(
        "scheduled_workflow_requires_issue_context",
        "scheduled job "
          <> job.id
          <> " workflow "
          <> workflow_id
          <> " step "
          <> step.id
          <> " references issue variable "
          <> variable
          <> "; scheduled workflows must use scheduled_job.*, schedule.*, or run.* variables",
      ))
  }
}

fn first_issue_reference(variables: List(String)) -> Option(String) {
  case variables {
    [] -> None
    [variable, ..rest] ->
      case variable == "issue" || string.starts_with(variable, "issue.") {
        True -> Some(variable)
        False -> first_issue_reference(rest)
      }
  }
}

fn validate_workflow_model_settings(
  defaults: model_config.Settings,
  workflows: List(#(String, workflow_dag.WorkflowDag)),
) -> Result(Nil, BundleError) {
  use _ <- result.try(
    model_config.validate_resolved(defaults, "pi") |> map_model_error,
  )
  validate_workflow_model_entries(workflows, defaults)
}

fn validate_workflow_model_entries(
  workflows: List(#(String, workflow_dag.WorkflowDag)),
  defaults: model_config.Settings,
) -> Result(Nil, BundleError) {
  case workflows {
    [] -> Ok(Nil)
    [#(id, dag), ..rest] -> {
      use _ <- result.try(validate_step_model_settings(id, dag.steps, defaults))
      validate_workflow_model_entries(rest, defaults)
    }
  }
}

fn validate_step_model_settings(
  workflow_id: String,
  steps: List(workflow_dag.WorkflowStep),
  defaults: model_config.Settings,
) -> Result(Nil, BundleError) {
  case steps {
    [] -> Ok(Nil)
    [step, ..rest] -> {
      case step.kind {
        workflow_dag.AgentStep(_, _) -> {
          let resolved = model_config.resolve(defaults, step.model_settings)
          use _ <- result.try(
            model_config.validate_resolved(
              resolved,
              "workflow " <> workflow_id <> " step " <> step.id,
            )
            |> map_model_error,
          )
          validate_step_model_settings(workflow_id, rest, defaults)
        }
        workflow_dag.CommandStep(_, _) ->
          validate_step_model_settings(workflow_id, rest, defaults)
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
      let prompt_path = string.trim(prompt_path)
      use workflow_dir <- result.try(
        path.dirname(workflow_path)
        |> result.replace_error(BundleError(
          "invalid_prompt_path",
          "could not resolve workflow directory for " <> workflow_path,
        )),
      )
      let joined_path = path.join(workflow_dir, prompt_path)
      use full_path <- result.try(
        path.absolute(joined_path)
        |> result.replace_error(BundleError(
          "invalid_prompt_path",
          "could not resolve prompt path: " <> prompt_path,
        )),
      )
      use workflow_dir_abs <- result.try(
        path.absolute(workflow_dir)
        |> result.replace_error(BundleError(
          "invalid_prompt_path",
          "could not resolve workflow directory for " <> workflow_path,
        )),
      )
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
                False -> ".scherzo/scherzo.yaml"
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

fn is_yaml_config_path(path: String) -> Bool {
  let lower = string.lowercase(path)
  string.ends_with(lower, ".yaml") || string.ends_with(lower, ".yml")
}

fn map_config_error(
  result: Result(a, error.ConfigError),
) -> Result(a, BundleError) {
  case result {
    Ok(value) -> Ok(value)
    Error(err) ->
      Error(BundleError(error.config_code(err), error.config_message(err)))
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

fn map_model_error(
  result: Result(a, model_config.ModelError),
) -> Result(a, BundleError) {
  case result {
    Ok(value) -> Ok(value)
    Error(err) ->
      Error(BundleError(
        model_config.error_code(err),
        model_config.error_message(err),
      ))
  }
}

fn real_env(name: String) -> Option(String) {
  path.env(name)
}
