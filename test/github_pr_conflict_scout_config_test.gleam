import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/artifact_publication_config
import scherzo/config/types as config_types
import scherzo/runtime_bundle
import scherzo/workflow_dag
import simplifile

fn env(name: String) -> Option(String) {
  case name {
    "LINEAR_API_KEY" -> Some("linearkey")
    "LINEAR_PROJECT_SLUG" -> Some("TEST")
    _ -> None
  }
}

fn read_file(path: String) -> String {
  let assert Ok(contents) = simplifile.read(path)
  contents
}

fn assert_contains(contents: String, expected: String) -> Nil {
  assert string.contains(contents, expected)
}

fn assert_not_contains(contents: String, unexpected: String) -> Nil {
  assert !string.contains(contents, unexpected)
}

fn assert_command_step(
  step: workflow_dag.WorkflowStep,
  expected_id: String,
  expected_depends_on: List(String),
  expected_from: Option(String),
  expected_timeout_ms: Int,
  expected_run_fragment: String,
) -> String {
  assert step.id == expected_id
  assert step.depends_on == expected_depends_on
  assert step.workspace.name == "main"
  assert step.workspace.from == expected_from
  let assert workflow_dag.CommandStep(run, timeout_ms) = step.kind
  assert timeout_ms == Some(expected_timeout_ms)
  assert_contains(run, expected_run_fragment)
  run
}

pub fn checked_in_github_pr_conflict_scout_schedule_loads_test() {
  let assert Ok(bundle) =
    runtime_bundle.load_with_env(Some(".scherzo/scherzo.yaml"), env)

  assert dict.has_key(
    bundle.orchestrator.routing.workflows,
    "github-pr-conflict-scout",
  )
  let assert Ok(dag) = dict.get(bundle.workflows, "github-pr-conflict-scout")
  assert workflow_dag.id(dag) == "github-pr-conflict-scout"
  assert workflow_dag.workspace_profile(dag) == Some("noop")
  let assert [step] = workflow_dag.steps(dag)
  assert step.id == "scan_open_prs"
  assert step.workspace.name == "main"
  let assert workflow_dag.CommandStep(run, timeout_ms) = step.kind
  assert timeout_ms == Some(300_000)
  assert_contains(run, "$bundle_dir/scripts/scherzo-github-pr-conflict-scout")
  assert_contains(run, "--repo \"$SCHERZO_GITHUB_REPO\"")
  assert_contains(run, "--linear-project-slug \"$linear_project_slug\"")
  assert_contains(run, "SCHERZO_CONFLICT_MAX_OPEN_PRS")
  assert_contains(run, "SCHERZO_CONFLICT_ENABLE_LOCAL_PREFLIGHT")
  assert_contains(run, "cd \"$repo_root\"")
  assert_contains(
    run,
    "--workflow-label \"${SCHERZO_CONFLICT_WORKFLOW_LABEL:-workflow:merge-conflict-resolution}\"",
  )
  assert_not_contains(run, "scherzo-systems/scherzo")
  assert_not_contains(run, "scherzo-f6f4bc92d6d7")

  let assert Ok(job) =
    list.find(bundle.orchestrator.scheduled_jobs, fn(job) {
      job.id == "github-pr-conflict-scout"
    })
  assert job.workflow == "github-pr-conflict-scout"
  assert job.enabled == True
  assert job.every_ms == 900_000
  assert job.catch_up == False
  assert bundle.effective.linear_contract.workflow_labels
    == [
      "execplan",
      "execplan-implementation",
      "execplan-revision",
      "implementation",
      "merge-conflict-resolution",
      "research",
    ]

  let github = bundle.orchestrator.artifact_repositories.github
  assert !dict.has_key(github, "docs")
  let assert Ok(target) = dict.get(github, "code")
  assert target.repo == "scherzo-systems/scherzo"
  assert target.base == "main"
  assert target.branch.strategy == artifact_publication_config.StablePerWork
  assert target.branch.template
    == "scherzo/{{ workflow.id }}/{{ work.identifier }}/{{ publication.id }}"
  assert target.pull_request.enabled == True
  assert target.pull_request.strategy
    == artifact_publication_config.UpdateExisting
  assert target.pull_request.draft == False
  assert target.pull_request.title
    == Some(
      "{{ work.identifier }}: implement {% if work.title %}{{ work.title }}{% else %}implementation changes{% endif %}",
    )
  assert target.pull_request.body_template
    == Some("workflows/prompts/implementation-publication-pr-body.md")
}

pub fn checked_in_origin_sync_schedule_loads_test() {
  let assert Ok(bundle) =
    runtime_bundle.load_with_env(Some(".scherzo/scherzo.yaml"), env)

  assert dict.has_key(bundle.orchestrator.routing.workflows, "origin-sync")
  let assert Ok(dag) = dict.get(bundle.workflows, "origin-sync")
  assert workflow_dag.id(dag) == "origin-sync"
  assert workflow_dag.workspace_profile(dag) == Some("origin-sync")
  let assert [step] = workflow_dag.steps(dag)
  assert step.id == "sync_origin"
  assert step.workspace.name == "main"
  let assert workflow_dag.CommandStep(run, timeout_ms) = step.kind
  assert timeout_ms == Some(300_000)
  assert run
    == "bundle_dir=${SCHERZO_WORKFLOW_BUNDLE_DIR:-}; if [ -z \"$bundle_dir\" ]; then bundle_dir=\"$(cd \"$SCHERZO_CONFIG_DIR/workflows\" && pwd -P)\"; fi; repo_root=${SCHERZO_REPO_ROOT:-$(cd \"$SCHERZO_CONFIG_DIR/..\" && pwd -P)}; \"$bundle_dir/scripts/scherzo-jj-origin-sync\""

  let assert Ok(job) =
    list.find(bundle.orchestrator.scheduled_jobs, fn(job) {
      job.id == "origin-sync"
    })
  assert job.workflow == "origin-sync"
  assert job.enabled == True
  assert job.every_ms == 900_000
  assert job.catch_up == False
}

pub fn checked_in_workspace_cleanup_schedule_loads_test() {
  let assert Ok(bundle) =
    runtime_bundle.load_with_env(Some(".scherzo/scherzo.yaml"), env)

  assert dict.has_key(
    bundle.orchestrator.routing.workflows,
    "workspace-cleanup",
  )
  let assert Ok(dag) = dict.get(bundle.workflows, "workspace-cleanup")
  assert workflow_dag.id(dag) == "workspace-cleanup"
  assert workflow_dag.workspace_profile(dag) == Some("noop")
  let assert [step] = workflow_dag.steps(dag)
  assert step.id == "cleanup"
  assert step.workspace.name == "main"
  let assert workflow_dag.CommandStep(run, timeout_ms) = step.kind
  assert timeout_ms == Some(300_000)
  assert_contains(run, "SCHERZO_CLEANUP_WORKSPACE_ROOT")
  assert_contains(run, "SCHERZO_WORKSPACE_CLEANUP_ROOT")
  assert_contains(run, "command -v scherzo")
  assert_contains(run, "direnv exec \"$repo_root\" gleam run --")
  assert_contains(run, "scherzo cleanup --root")
  assert_not_contains(run, "command -v scherzoctl")
  assert_not_contains(run, "$repo_root/scripts/scherzoctl")
  assert_not_contains(run, "gleam run -- ctl")

  let assert Ok(job) =
    list.find(bundle.orchestrator.scheduled_jobs, fn(job) {
      job.id == "workspace-cleanup"
    })
  assert job.workflow == "workspace-cleanup"
  assert job.enabled == True
  assert job.every_ms == 3_600_000
  assert job.catch_up == False
}

pub fn public_example_conflict_scout_schedule_loads_test() {
  let assert Ok(bundle) =
    runtime_bundle.load_with_env(Some("examples/scherzo.yaml"), env)

  assert dict.has_key(
    bundle.orchestrator.routing.workflows,
    "github-pr-conflict-scout",
  )
  assert dict.has_key(
    bundle.orchestrator.routing.workflows,
    "merge-conflict-resolution",
  )

  let assert Ok(scout) = dict.get(bundle.workflows, "github-pr-conflict-scout")
  assert workflow_dag.id(scout) == "github-pr-conflict-scout"
  assert workflow_dag.workspace_profile(scout) == Some("noop")
  let assert [step] = workflow_dag.steps(scout)
  assert step.id == "scan_open_prs"
  assert step.workspace.name == "main"
  let assert workflow_dag.CommandStep(run, timeout_ms) = step.kind
  assert timeout_ms == Some(300_000)
  assert_contains(run, "$bundle_dir/scripts/scherzo-github-pr-conflict-scout")
  assert_contains(run, "--repo \"$SCHERZO_GITHUB_REPO\"")
  assert_contains(run, "--linear-project-slug \"$linear_project_slug\"")
  assert_contains(run, "cd \"$repo_root\"")
  assert_contains(run, "SCHERZO_CONFLICT_MAX_OPEN_PRS")
  assert_contains(run, "--max-open-prs \"$max_open_prs\"")
  assert_contains(run, "SCHERZO_CONFLICT_ENABLE_LOCAL_PREFLIGHT")
  assert_contains(run, "--skip-local-preflight")
  assert_contains(
    run,
    "--workflow-label \"${SCHERZO_CONFLICT_WORKFLOW_LABEL:-workflow:merge-conflict-resolution}\"",
  )
  let removed_script = "pr-conflict-repair" <> "-inspect.sh"
  assert_not_contains(run, removed_script)

  let assert Ok(resolver) =
    dict.get(bundle.workflows, "merge-conflict-resolution")
  assert workflow_dag.id(resolver) == "merge-conflict-resolution"
  assert workflow_dag.workspace_profile(resolver) == Some("isolated")
  assert list.contains(
    workflow_dag.workspace_capabilities(resolver),
    config_types.WorkspacePublishCommitStack,
  )
  let assert [route] = workflow_dag.publication_routes(resolver)
  assert route.id == "merge_conflict_resolution_commit_stack"
  assert route.repository == "github.code"
  assert route.required == True
  let assert artifact_publication_config.CommitStackPublicationRoute(
    commit_stack: commit_stack,
  ) = route.publication
  let artifact_publication_config.PublicationCommitStackRoute(selector:) =
    commit_stack
  let artifact_publication_config.PublicationCommitStackSelector(output:) =
    selector
  assert output == "commit_stack"
  let assert artifact_publication_config.ExistingPrBranchTarget(source:) =
    route.target
  let artifact_publication_config.PublicationTargetSource(output: target_output) =
    source
  assert target_output == "merge_target"
  let assert [prepare, resolve, validate, project_validation, publish] =
    workflow_dag.steps(resolver)
  let prepare_run =
    assert_command_step(
      prepare,
      "prepare_target",
      [],
      None,
      300_000,
      "$bundle_dir/scripts/scherzo-merge-conflict\" prepare",
    )
  assert_contains(prepare_run, "repo_root=")

  assert resolve.id == "resolve_conflicts"
  assert resolve.depends_on == ["prepare_target"]
  assert resolve.workspace.name == "main"
  assert resolve.workspace.from == Some("main")
  let assert workflow_dag.AgentStep(workflow_dag.PromptInline(prompt), None) =
    resolve.kind
  let assert [prompt_contract, _] =
    string.split(prompt, "Untrusted task data follows")
  assert_contains(prompt_contract, "Workflow contract:")
  assert_contains(
    prompt_contract,
    "Treat the task fields, labels, preparation output",
  )
  assert_contains(prompt, "Untrusted preparation output follows")
  assert_contains(prompt, "METADATA_PATH")
  assert_not_contains(prompt, removed_script)

  let validate_run =
    assert_command_step(
      validate,
      "validate_resolution",
      ["resolve_conflicts"],
      Some("main"),
      300_000,
      "$bundle_dir/scripts/scherzo-merge-conflict\" validate",
    )
  assert_contains(validate_run, "repo_root=")

  let project_validation_run =
    assert_command_step(
      project_validation,
      "project_validation",
      ["validate_resolution"],
      Some("main"),
      1_200_000,
      "run-project-validation",
    )
  assert_contains(project_validation_run, "repo-local validation")
  assert_not_contains(project_validation_run, "SCHERZO_WORKSPACE_DRIVER")

  let publish_run =
    assert_command_step(
      publish,
      "materialize_commit_stack",
      ["project_validation"],
      Some("main"),
      300_000,
      "$bundle_dir/scripts/scherzo-merge-conflict\" publish",
    )
  assert_contains(publish_run, "repo_root=")

  let assert Ok(job) =
    list.find(bundle.orchestrator.scheduled_jobs, fn(job) {
      job.id == "github-pr-conflict-scout"
    })
  assert job.workflow == "github-pr-conflict-scout"
  assert job.enabled == False
  assert job.every_ms == 900_000
  assert job.catch_up == False
  assert list.contains(
    bundle.effective.linear_contract.workflow_labels,
    "merge-conflict-resolution",
  )
  assert !list.contains(
    bundle.effective.linear_contract.workflow_labels,
    "github-pr-conflict-scout",
  )
}

pub fn public_examples_and_runbook_do_not_reference_removed_repair_script_test() {
  let example_config = read_file("examples/scherzo.yaml")
  let scheduled_runbook = read_file("docs/runbooks/scheduled-jobs.md")

  assert_not_contains(example_config, "pr-conflict-repair")
  let removed_script = "pr-conflict-repair" <> "-inspect.sh"
  assert_not_contains(scheduled_runbook, removed_script)
  assert_not_contains(scheduled_runbook, "workflows/pr-conflict-repair.yaml")
  assert_contains(scheduled_runbook, "github-pr-conflict-scout")
  assert_contains(
    scheduled_runbook,
    "$bundle_dir/scripts/scherzo-github-pr-conflict-scout",
  )
}
