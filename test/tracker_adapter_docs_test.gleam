import gleam/string
import simplifile

fn read_file(path: String) -> String {
  case simplifile.read(path) {
    Ok(contents) -> contents
    Error(_) -> {
      let message = path <> " could not be read"
      panic as message
    }
  }
}

fn assert_contains(path: String, contents: String, expected: String) -> Nil {
  case string.contains(contents, expected) {
    True -> Nil
    False -> {
      let message = path <> " is missing expected text: " <> expected
      panic as message
    }
  }
}

pub fn tracker_adapter_runbook_documents_capability_matrix_test() {
  let path = "docs/runbooks/tracker-adapters.md"
  let runbook = read_file(path)

  assert_contains(path, runbook, "task")
  assert_contains(path, runbook, "tracker adapter")
  assert_contains(path, runbook, "Linear issue")
  assert_contains(path, runbook, "tracker-smoke")
  assert_contains(path, runbook, "linear-smoke")
  assert_contains(path, runbook, "scripts/scherzo-execplan-v2")
  assert_contains(path, runbook, "scheduled_failures")
  assert_contains(path, runbook, "remote_commands")
  assert_contains(path, runbook, "| Linear | Production |")
  assert_contains(path, runbook, "| Jira follow-up | Future |")
  assert_contains(path, runbook, "| Trello follow-up | Future |")
  assert_contains(path, runbook, "| test-memory | Test fixture |")
}

pub fn tracker_adapter_runbook_is_linked_from_operator_docs_test() {
  let readme_path = "README.md"
  let getting_started_path = "docs/GETTING_STARTED.md"
  let architecture_path = "docs/ARCHITECTURE.md"
  let scheduled_path = "docs/runbooks/scheduled-jobs.md"

  assert_contains(
    readme_path,
    read_file(readme_path),
    "docs/runbooks/tracker-adapters.md",
  )
  assert_contains(
    getting_started_path,
    read_file(getting_started_path),
    "runbooks/tracker-adapters.md",
  )
  assert_contains(
    architecture_path,
    read_file(architecture_path),
    "runbooks/tracker-adapters.md",
  )
  assert_contains(scheduled_path, read_file(scheduled_path), "tracker adapter")
}

pub fn getting_started_prefers_tracker_doctor_aliases_test() {
  let path = "docs/GETTING_STARTED.md"
  let guide = read_file(path)

  assert_contains(path, guide, "--check tracker-contract")
  assert_contains(path, guide, "--check tracker-smoke")
  assert_contains(
    path,
    guide,
    "linear-contract` and `linear-smoke` remain compatibility aliases",
  )
}

pub fn execplan_v2_workflows_use_task_operator_language_test() {
  let dogfood_readme_path = ".scherzo/README.md"
  let dogfood_readme = read_file(dogfood_readme_path)
  assert_contains(dogfood_readme_path, dogfood_readme, "workflow:execplan-v2")
  assert_contains(
    dogfood_readme_path,
    dogfood_readme,
    "workflow:execplan-revision-v2",
  )
  assert_contains(
    dogfood_readme_path,
    dogfood_readme,
    "workflow:execplan-implementation-v2",
  )
  assert_contains(
    dogfood_readme_path,
    dogfood_readme,
    "Linear-backed implementation task",
  )

  let draft_prompt_path = ".scherzo/workflows/prompts/execplan-v2-draft.md"
  let draft_prompt = read_file(draft_prompt_path)
  assert_contains(draft_prompt_path, draft_prompt, "for this task")
  assert_contains(draft_prompt_path, draft_prompt, "Task:")
  assert_contains(
    draft_prompt_path,
    draft_prompt,
    "source_issue` compatibility",
  )

  let implementation_workflow_path =
    ".scherzo/workflows/execplan-implementation-v2.yaml"
  assert_contains(
    implementation_workflow_path,
    read_file(implementation_workflow_path),
    "execplan-implementation-v2-verify-completion.md",
  )

  let pi_wrapper_path = "scripts/scherzo-pi"
  let pi_wrapper = read_file(pi_wrapper_path)
  assert_contains(pi_wrapper_path, pi_wrapper, "execplan-v2")
  assert_contains(pi_wrapper_path, pi_wrapper, "execplan-revision-v2")
  assert_contains(pi_wrapper_path, pi_wrapper, "execplan-implementation-v2")
}
