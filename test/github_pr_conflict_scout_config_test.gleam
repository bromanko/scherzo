import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import scherzo/runtime_bundle
import scherzo/workflow_dag

fn env(name: String) -> Option(String) {
  case name {
    "LINEAR_API_KEY" -> Some("linearkey")
    "LINEAR_PROJECT_SLUG" -> Some("TEST")
    _ -> None
  }
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
      "execplan-revision",
      "implementation",
      "merge-conflict-resolution",
      "research",
    ]
}
