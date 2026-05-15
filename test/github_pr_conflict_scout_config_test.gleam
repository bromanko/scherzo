import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
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
  assert dag.id == "github-pr-conflict-scout"
  assert dag.workspace_profile == Some("dogfood-jj")
  let assert [step] = dag.steps
  assert step.id == "scan_open_prs"
  assert step.workspace.name == "main"
  let assert workflow_dag.CommandStep(run, timeout_ms) = step.kind
  assert timeout_ms == Some(300_000)
  let expected_run =
    "repo_root=${SCHERZO_REPO_ROOT:-$(cd \"$SCHERZO_CONFIG_DIR/..\" && pwd -P)}; \"$repo_root/scripts/scherzo-github-pr-conflict-scout\" scan "
    <> "--repo bromanko/scherzo --linear-project-slug scherzo-f6f4bc92d6d7 "
    <> "--create-state Todo --workflow-label workflow:merge-conflict-resolution"
  assert run == expected_run

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
      "execplan-implementation-v2",
      "execplan-revision",
      "execplan-revision-v2",
      "execplan-v2",
      "implementation",
      "merge-conflict-resolution",
      "research",
    ]
}

pub fn checked_in_origin_sync_schedule_loads_test() {
  let assert Ok(bundle) =
    runtime_bundle.load_with_env(Some(".scherzo/scherzo.yaml"), env)

  assert dict.has_key(bundle.orchestrator.routing.workflows, "origin-sync")
  let assert Ok(dag) = dict.get(bundle.workflows, "origin-sync")
  assert dag.id == "origin-sync"
  assert dag.workspace_profile == Some("noop")
  let assert [step] = dag.steps
  assert step.id == "sync_origin"
  assert step.workspace.name == "main"
  let assert workflow_dag.CommandStep(run, timeout_ms) = step.kind
  assert timeout_ms == Some(300_000)
  assert run
    == "repo_root=${SCHERZO_REPO_ROOT:-$(cd \"$SCHERZO_CONFIG_DIR/..\" && pwd -P)}; \"$repo_root/scripts/scherzo-jj-origin-sync\""

  let assert Ok(job) =
    list.find(bundle.orchestrator.scheduled_jobs, fn(job) {
      job.id == "origin-sync"
    })
  assert job.workflow == "origin-sync"
  assert job.enabled == True
  assert job.every_ms == 900_000
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
  assert scout.id == "github-pr-conflict-scout"
  assert scout.workspace_profile == Some("noop")
  let assert [step] = scout.steps
  assert step.id == "scan_open_prs"
  assert step.workspace.name == "main"
  let assert workflow_dag.CommandStep(run, timeout_ms) = step.kind
  assert timeout_ms == Some(300_000)
  assert_contains(run, "scripts/scherzo-github-pr-conflict-scout")
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
  assert resolver.id == "merge-conflict-resolution"
  assert resolver.workspace_profile == Some("isolated")
  assert list.contains(
    resolver.workspace_capabilities,
    config_types.WorkspacePublishChange,
  )
  let assert [prepare, resolve, validate, publish] = resolver.steps
  let prepare_run =
    assert_command_step(
      prepare,
      "prepare_target",
      [],
      None,
      300_000,
      "scripts/scherzo-merge-conflict\" prepare",
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
      1_200_000,
      "scripts/scherzo-merge-conflict\" validate",
    )
  assert_contains(validate_run, "repo_root=")

  let publish_run =
    assert_command_step(
      publish,
      "publish_resolution",
      ["validate_resolution"],
      Some("main"),
      300_000,
      "scripts/scherzo-merge-conflict\" publish",
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
  assert_contains(scheduled_runbook, "scripts/scherzo-github-pr-conflict-scout")
}
