import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/artifact_publication_config
import scherzo/commit_stack_publication_preflight
import scherzo/config
import scherzo/config/tracker_config
import scherzo/config/types as config_types
import scherzo/error
import scherzo/model_config
import scherzo/path
import scherzo/template
import scherzo/tracker/issue as tracker_issue
import scherzo/workflow_bundle
import scherzo/workflow_completion_policy
import scherzo/workflow_dag
import scherzo/workflow_policy
import scherzo/workspace_driver_discovery
import scherzo/workspace_profile
import simplifile
import yay

pub type BundleDependency {
  BundleDependency(path: String, contents: String)
}

pub type RuntimeBundle {
  RuntimeBundle(
    config_path: String,
    config_contents: String,
    dependencies: List(BundleDependency),
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
  select_routed_workflow(
    bundle.workflows,
    bundle.orchestrator.routing,
    bundle.effective.linear_contract,
    issue,
  )
}

pub fn workflow_by_id(
  bundle: RuntimeBundle,
  id: String,
) -> Result(#(String, workflow_dag.WorkflowDag), BundleError) {
  lookup_workflow(bundle.workflows, id)
}

pub fn workflow_bundle_dir(
  bundle: RuntimeBundle,
  workflow_id: String,
) -> String {
  workflow_bundle.dir(bundle.orchestrator, workflow_id)
}

pub fn normalized_workflows(
  bundle: RuntimeBundle,
) -> Dict(String, workflow_dag.WorkflowDag) {
  bundle.workflows
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
    False -> unsupported_config_path_error()
  }
}

pub fn load_workflow_by_id(
  explicit: Option(String),
  workflow_id: String,
) -> Result(#(String, workflow_dag.WorkflowDag), BundleError) {
  let selected = select_config_path(explicit)
  case is_yaml_config_path(selected) {
    True -> load_routed_workflow(selected, workflow_id)
    False -> unsupported_config_path_error()
  }
}

fn unsupported_config_path_error() -> Result(a, BundleError) {
  Error(BundleError(
    "unsupported_config_path",
    "runtime config path must end in .yaml or .yml",
  ))
}

fn select_routed_workflow(
  workflows: Dict(String, workflow_dag.WorkflowDag),
  routing: config_types.RoutingConfig,
  linear_contract: config_types.LinearContractConfig,
  issue: tracker_issue.Issue,
) -> Result(#(String, workflow_dag.WorkflowDag), BundleError) {
  case workflow_policy.classify_issue(linear_contract, issue) {
    workflow_policy.WorkflowInvalid(violation) ->
      Error(workflow_violation_to_bundle_error(violation))
    workflow_policy.WorkflowSelected(id, _) -> lookup_workflow(workflows, id)
    workflow_policy.WorkflowPolicyDisabled ->
      select_unenforced_routed_workflow(workflows, routing, issue)
  }
}

fn select_unenforced_routed_workflow(
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

fn workflow_violation_to_bundle_error(
  violation: workflow_policy.IssueWorkflowViolation,
) -> BundleError {
  case violation {
    workflow_policy.MissingWorkflowLabel ->
      BundleError("missing_workflow_label", "issue has no workflow label")
    workflow_policy.MultipleWorkflowLabels(_) ->
      BundleError(
        "multiple_workflow_labels",
        "issue has multiple workflow labels",
      )
    workflow_policy.UnknownWorkflowLabel(label) ->
      BundleError("unknown_workflow_label", "unknown workflow label: " <> label)
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

fn load_routed_workflow(
  selected: String,
  workflow_id: String,
) -> Result(#(String, workflow_dag.WorkflowDag), BundleError) {
  use content <- result.try(read_file(selected, "missing_config_file"))
  use root <- result.try(parse_yaml_root(content, selected))
  use routing <- result.try(
    tracker_config.resolve_root_routing(root, selected)
    |> map_config_error,
  )
  use #(workflows, _) <- result.try(
    load_workflow_map(dict.to_list(routing.workflows), dict.new(), []),
  )
  lookup_workflow(workflows, workflow_id)
}

fn load_orchestrator(
  selected: String,
  env: config.Env,
) -> Result(RuntimeBundle, BundleError) {
  use content <- result.try(read_file(selected, "missing_config_file"))
  let config_dependency = BundleDependency(selected, content)
  use root <- result.try(parse_yaml_root(content, selected))
  use orchestrator <- result.try(
    config.resolve_orchestrator_root(root, selected, env)
    |> map_config_error,
  )
  use #(workflows, workflow_dependencies) <- result.try(
    load_workflow_map(
      dict.to_list(orchestrator.routing.workflows),
      dict.new(),
      [],
    ),
  )
  use _ <- result.try(validate_publication_repositories(orchestrator, workflows))
  use orchestrator <- result.try(enrich_completion_state_policy(
    orchestrator,
    workflows,
  ))
  use orchestrator <- result.try(
    workspace_driver_discovery.enrich_orchestrator(orchestrator)
    |> result.map_error(workspace_driver_discovery_error_to_bundle_error),
  )
  use _ <- result.try(
    commit_stack_publication_preflight.validate_required(
      orchestrator,
      dict.to_list(workflows),
    )
    |> result.map_error(fn(err) {
      BundleError(
        commit_stack_publication_preflight.error_code(err),
        commit_stack_publication_preflight.error_message(err),
      )
    }),
  )
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
    dependencies: normalize_dependencies([
      config_dependency,
      ..workflow_dependencies
    ]),
    effective: orchestrator.effective,
    orchestrator: orchestrator,
    workflows: workflows,
    secrets: config.resolved_secrets(orchestrator.effective),
  ))
}

fn enrich_completion_state_policy(
  orchestrator: config_types.OrchestratorConfig,
  workflows: Dict(String, workflow_dag.WorkflowDag),
) -> Result(config_types.OrchestratorConfig, BundleError) {
  let effective = orchestrator.effective
  let handoff = effective.handoff
  case handoff.completion_states {
    None -> Ok(orchestrator)
    Some(policy) -> {
      use _ <- result.try(reject_unknown_workflow_overrides(policy, workflows))
      let completion_states =
        workflow_completion_policy.CompletionStatePolicy(
          ..policy,
          workflows: workflow_completion_overrides(policy.workflows, workflows),
        )
      let handoff =
        config_types.HandoffConfig(
          ..handoff,
          completion_states: Some(completion_states),
        )
      let effective = config_types.EffectiveConfig(..effective, handoff:)
      Ok(config_types.OrchestratorConfig(..orchestrator, effective:))
    }
  }
}

fn reject_unknown_workflow_overrides(
  policy: workflow_completion_policy.CompletionStatePolicy,
  workflows: Dict(String, workflow_dag.WorkflowDag),
) -> Result(Nil, BundleError) {
  case
    policy.workflows
    |> dict.keys
    |> list.find(fn(workflow_id) { !dict.has_key(workflows, workflow_id) })
  {
    Ok(workflow_id) -> {
      let message =
        "task_updates.workflows."
        <> workflow_id
        <> " does not match a configured workflow"
      Error(BundleError("unknown_task_update_workflow", message))
    }
    Error(Nil) -> Ok(Nil)
  }
}

fn workflow_completion_overrides(
  existing: Dict(String, workflow_completion_policy.WorkflowCompletionOverride),
  workflows: Dict(String, workflow_dag.WorkflowDag),
) -> Dict(String, workflow_completion_policy.WorkflowCompletionOverride) {
  workflows
  |> dict.to_list
  |> list.fold(existing, fn(acc, entry) {
    let #(id, dag) = entry
    let inferred = workflow_completion_override(dag)
    case dict.get(acc, id) {
      Ok(configured) ->
        dict.insert(
          acc,
          id,
          workflow_completion_policy.merge_overrides(inferred, configured),
        )
      Error(Nil) -> dict.insert(acc, id, inferred)
    }
  })
}

fn workflow_completion_override(
  dag: workflow_dag.WorkflowDag,
) -> workflow_completion_policy.WorkflowCompletionOverride {
  let requires_review = workflow_requires_review(dag)
  workflow_completion_policy.WorkflowCompletionOverride(
    ..workflow_completion_policy.default_override(),
    produces_reviewable_artifacts: Some(requires_review),
    requires_review: Some(requires_review),
  )
}

fn workflow_requires_review(dag: workflow_dag.WorkflowDag) -> Bool {
  let capabilities = workflow_dag.workspace_capabilities(dag)
  list.contains(capabilities, config_types.WorkspacePublishChange)
  || list.contains(capabilities, config_types.WorkspacePublishCommitStack)
  || workflow_declares_outputs(dag)
}

fn workflow_declares_outputs(dag: workflow_dag.WorkflowDag) -> Bool {
  case workflow_dag.contract(dag) {
    Some(contract) -> !list.is_empty(contract.outputs)
    None -> False
  }
}

fn load_workflow_map(
  entries: List(#(String, String)),
  acc: Dict(String, workflow_dag.WorkflowDag),
  acc_dependencies: List(BundleDependency),
) -> Result(
  #(Dict(String, workflow_dag.WorkflowDag), List(BundleDependency)),
  BundleError,
) {
  case entries {
    [] -> Ok(#(acc, acc_dependencies))
    [#(id, workflow_path), ..rest] -> {
      use #(dag, dependencies) <- result.try(load_workflow_dag(workflow_path))
      let actual_id = workflow_dag.id(dag)
      case actual_id == id {
        True ->
          load_workflow_map(
            rest,
            dict.insert(acc, id, dag),
            list.append(acc_dependencies, dependencies),
          )
        False ->
          Error(BundleError(
            "workflow_id_mismatch",
            "routing key " <> id <> " points to workflow id " <> actual_id,
          ))
      }
    }
  }
}

pub fn load_workflow_file(
  workflow_path: String,
) -> Result(workflow_dag.WorkflowDag, BundleError) {
  use #(dag, _) <- result.try(load_workflow_dag(workflow_path))
  Ok(dag)
}

fn load_workflow_dag(
  workflow_path: String,
) -> Result(#(workflow_dag.WorkflowDag, List(BundleDependency)), BundleError) {
  use content <- result.try(read_file(workflow_path, "missing_workflow_file"))
  let workflow_dependency = BundleDependency(workflow_path, content)
  use dag <- result.try(
    workflow_dag.parse(content) |> map_dag_error(workflow_path),
  )
  use #(dag, prompt_dependencies) <- result.try(resolve_prompt_files(
    dag,
    workflow_path,
  ))
  Ok(#(dag, [workflow_dependency, ..prompt_dependencies]))
}

fn resolve_prompt_files(
  dag: workflow_dag.WorkflowDag,
  workflow_path: String,
) -> Result(#(workflow_dag.WorkflowDag, List(BundleDependency)), BundleError) {
  use #(recover, recover_dependencies) <- result.try(resolve_recover_prompt(
    workflow_dag.recovery_config(dag),
    workflow_path,
  ))
  use #(steps, step_dependencies) <- result.try(
    resolve_step_prompts(workflow_dag.steps(dag), workflow_path, [], []),
  )
  use dag <- result.try(
    workflow_dag.with_recovery_and_steps(dag, recover: recover, steps: steps)
    |> map_dag_error(workflow_path),
  )
  Ok(#(dag, list.append(recover_dependencies, step_dependencies)))
}

fn resolve_step_prompts(
  steps: List(workflow_dag.WorkflowStep),
  workflow_path: String,
  acc: List(workflow_dag.WorkflowStep),
  acc_dependencies: List(BundleDependency),
) -> Result(
  #(List(workflow_dag.WorkflowStep), List(BundleDependency)),
  BundleError,
) {
  case steps {
    [] -> Ok(#(list.reverse(acc), acc_dependencies))
    [step, ..rest] -> {
      use #(step, step_dependencies) <- result.try(resolve_step_prompt_refs(
        step,
        workflow_path,
      ))
      resolve_step_prompts(
        rest,
        workflow_path,
        [step, ..acc],
        list.append(acc_dependencies, step_dependencies),
      )
    }
  }
}

fn resolve_step_prompt_refs(
  step: workflow_dag.WorkflowStep,
  workflow_path: String,
) -> Result(#(workflow_dag.WorkflowStep, List(BundleDependency)), BundleError) {
  use #(recover, recover_dependencies) <- result.try(resolve_recover_prompt(
    step.recover,
    workflow_path,
  ))
  let step = workflow_dag.WorkflowStep(..step, recover: recover)
  case step.kind {
    workflow_dag.AgentStep(
      workflow_dag.PromptFile(prompt_path),
      structured_output,
    ) -> {
      use #(prompt, dependency) <- result.try(read_relative_prompt(
        prompt_path,
        workflow_path,
      ))
      Ok(#(
        workflow_dag.WorkflowStep(
          ..step,
          kind: workflow_dag.AgentStep(
            workflow_dag.PromptInline(prompt),
            structured_output,
          ),
        ),
        list.append(recover_dependencies, [dependency]),
      ))
    }
    _ -> Ok(#(step, recover_dependencies))
  }
}

fn resolve_recover_prompt(
  recover: Option(workflow_dag.RecoveryConfigPatch),
  workflow_path: String,
) -> Result(
  #(Option(workflow_dag.RecoveryConfigPatch), List(BundleDependency)),
  BundleError,
) {
  case recover {
    None -> Ok(#(None, []))
    Some(workflow_dag.RecoveryConfigPatch(enabled, attempts, model, prompt)) ->
      case prompt {
        Some(workflow_dag.PromptFile(prompt_path)) -> {
          use #(contents, dependency) <- result.try(read_relative_prompt(
            prompt_path,
            workflow_path,
          ))
          Ok(
            #(
              Some(workflow_dag.RecoveryConfigPatch(
                enabled: enabled,
                attempts: attempts,
                model: model,
                prompt: Some(workflow_dag.PromptInline(contents)),
              )),
              [dependency],
            ),
          )
        }
        _ -> Ok(#(recover, []))
      }
  }
}

fn validate_publication_repositories(
  orchestrator: config_types.OrchestratorConfig,
  workflows: Dict(String, workflow_dag.WorkflowDag),
) -> Result(Nil, BundleError) {
  validate_workflow_publication_repositories(
    dict.to_list(workflows),
    orchestrator.artifact_repositories,
  )
}

fn validate_workflow_publication_repositories(
  workflows: List(#(String, workflow_dag.WorkflowDag)),
  repositories: artifact_publication_config.ArtifactRepositories,
) -> Result(Nil, BundleError) {
  case workflows {
    [] -> Ok(Nil)
    [#(workflow_id, dag), ..rest] -> {
      use _ <- result.try(validate_publication_repository_routes(
        workflow_id,
        workflow_dag.publication_routes(dag),
        repositories,
      ))
      validate_workflow_publication_repositories(rest, repositories)
    }
  }
}

fn validate_publication_repository_routes(
  workflow_id: String,
  routes: List(artifact_publication_config.PublicationRoute),
  repositories: artifact_publication_config.ArtifactRepositories,
) -> Result(Nil, BundleError) {
  case routes {
    [] -> Ok(Nil)
    [route, ..rest] -> {
      use _ <- result.try(validate_publication_repository_route(
        workflow_id,
        route,
        repositories,
      ))
      validate_publication_repository_routes(workflow_id, rest, repositories)
    }
  }
}

fn validate_publication_repository_route(
  workflow_id: String,
  route: artifact_publication_config.PublicationRoute,
  repositories: artifact_publication_config.ArtifactRepositories,
) -> Result(Nil, BundleError) {
  use #(backend, name) <- result.try(
    artifact_publication_config.repository_ref_parts(
      route.repository,
      "artifacts.publications[].repository",
    )
    |> result.map_error(fn(parse_error) {
      BundleError(
        artifact_publication_config.error_code(parse_error),
        artifact_publication_config.error_message(parse_error),
      )
    }),
  )
  case backend {
    "github" ->
      case dict.get(repositories.github, name) {
        Ok(_) -> Ok(Nil)
        Error(_) ->
          Error(BundleError(
            "publication_repository_missing",
            "workflow "
              <> workflow_id
              <> " publication "
              <> route.id
              <> " references unknown repository "
              <> route.repository,
          ))
      }
    _ ->
      Error(BundleError(
        "unsupported_publication_repository_backend",
        "workflow "
          <> workflow_id
          <> " publication "
          <> route.id
          <> " references unsupported repository backend "
          <> backend,
      ))
  }
}

fn validate_workspace_profiles(
  orchestrator: config_types.OrchestratorConfig,
  workflows: List(#(String, workflow_dag.WorkflowDag)),
) -> Result(Nil, BundleError) {
  case workflows {
    [] -> Ok(Nil)
    [#(_, dag), ..rest] -> {
      use _ <- result.try(
        workspace_profile.resolve(dag, orchestrator)
        |> result.map_error(workspace_profile_error_to_bundle_error),
      )
      validate_workspace_profiles(orchestrator, rest)
    }
  }
}

fn workspace_driver_discovery_error_to_bundle_error(
  err: workspace_driver_discovery.DiscoveryError,
) -> BundleError {
  BundleError(
    workspace_driver_discovery.error_code(err),
    workspace_driver_discovery.error_message(err),
  )
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
  validate_scheduled_steps(job, workflow_dag.id(dag), workflow_dag.steps(dag))
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
      let steps = workflow_dag.steps(dag)
      use _ <- result.try(validate_step_model_settings(id, steps, defaults))
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
) -> Result(#(String, BundleDependency), BundleError) {
  case validate_relative_path(prompt_path, "invalid_prompt_path") {
    Error(BundleError(code, message)) ->
      Error(BundleError(code, message <> " in workflow " <> workflow_path))
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
          "could not resolve prompt path "
            <> prompt_path
            <> " in workflow "
            <> workflow_path,
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
            "prompt path escapes workflow directory: "
              <> prompt_path
              <> " in workflow "
              <> workflow_path,
          ))
        True -> {
          use contents <- result.try(read_file(full_path, "missing_prompt_file"))
          Ok(#(contents, BundleDependency(full_path, contents)))
        }
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

fn normalize_dependencies(
  dependencies: List(BundleDependency),
) -> List(BundleDependency) {
  dependencies
  |> list.fold(dict.new(), fn(acc, dependency) {
    dict.insert(acc, dependency.path, dependency.contents)
  })
  |> dict.to_list
  |> list.map(fn(entry) {
    let #(path, contents) = entry
    BundleDependency(path, contents)
  })
  |> list.sort(by: fn(left, right) { string.compare(left.path, right.path) })
}

fn parse_yaml_root(
  content: String,
  config_path: String,
) -> Result(yay.Node, BundleError) {
  case yay.parse_string(content) {
    Error(_) ->
      Error(BundleError(
        "yaml_parse_error",
        "config " <> config_path <> ": YAML parse error",
      ))
    Ok([document]) -> Ok(yay.document_root(document))
    Ok(_) ->
      Error(BundleError(
        "multiple_documents",
        "config " <> config_path <> ": expected one YAML document",
      ))
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
  workflow_path: String,
) -> Result(a, BundleError) {
  case result {
    Ok(value) -> Ok(value)
    Error(workflow_dag.DagError(code, message)) ->
      Error(BundleError(code, "workflow " <> workflow_path <> ": " <> message))
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
