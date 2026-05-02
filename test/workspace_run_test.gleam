import gleam/dict
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/config
import scherzo/domain
import scherzo/error
import scherzo/tracker/state as issue_state
import scherzo/workflow_dag
import scherzo/workspace_run
import simplifile
import yay

fn env(_name: String) -> Option(String) {
  None
}

fn issue() -> domain.Issue {
  domain.Issue(
    id: "issue-id",
    identifier: "ABC-123",
    title: "Implement DAGs",
    description: None,
    priority: None,
    state: issue_state.from_string_unchecked("Todo"),
    branch_name: None,
    url: None,
    labels: [],
    blocked_by: [],
    created_at: None,
    updated_at: None,
  )
}

fn root(source: String) -> yay.Node {
  let assert Ok([document]) = yay.parse_string(source)
  yay.document_root(document)
}

fn reset_dir(path: String) -> Nil {
  let _ = simplifile.delete(path)
  let assert Ok(Nil) = simplifile.create_directory_all(path)
  Nil
}

fn orchestrator(
  dir: String,
  create_hook: String,
  before_hook: String,
) -> domain.OrchestratorConfig {
  let source =
    "version: 1\ntracker:\n  kind: linear\n  api_key: linearkey\n  project_slug: TEST\nworkspace:\n  root: workspaces\n  hooks:\n    create: |\n"
    <> indent(create_hook)
    <> "    before_step: |\n"
    <> indent(before_hook)
    <> "    timeout_ms: 5000\nrouting:\n  workflows:\n    implementation: workflows/implementation.yaml\n"
  let assert Ok(orchestrator) =
    config.resolve_orchestrator_root(root(source), dir <> "/scherzo.yaml", env)
  orchestrator
}

fn indent(script: String) -> String {
  script
  |> string.split(on: "\n")
  |> list_map(fn(line) { "      " <> line <> "\n" })
  |> string.join(with: "")
}

pub fn prepares_logical_workspace_paths_under_run_root_test() {
  let dir = "test/tmp/workspace-run-layout"
  reset_dir(dir)
  let orchestrator =
    orchestrator(
      dir,
      "mkdir -p \"$SCHERZO_WORKSPACE_PATH\"",
      "test -d \"$SCHERZO_WORKSPACE_PATH\"",
    )
  let assert Ok(main) =
    workspace_run.prepare_step(
      issue(),
      "implementation",
      "run-1",
      "implement",
      workflow_dag.WorkspaceRef(name: "main", from: None),
      orchestrator,
      dict.new(),
    )
  let assert Ok(code_review_path) =
    workspace_run.workspace_path_for(
      issue(),
      "implementation",
      "run-1",
      "code-review",
      orchestrator,
    )
  let assert Ok(other_run) =
    workspace_run.prepare_step(
      issue(),
      "implementation",
      "run-2",
      "implement",
      workflow_dag.WorkspaceRef(name: "main", from: None),
      orchestrator,
      dict.new(),
    )
  assert string.ends_with(
    main.path,
    "/workspaces/implementation/ABC-123/run-1/main",
  )
  assert string.ends_with(
    code_review_path,
    "/workspaces/implementation/ABC-123/run-1/code-review",
  )
  assert string.ends_with(
    other_run.path,
    "/workspaces/implementation/ABC-123/run-2/main",
  )
  assert main.path != code_review_path
  assert main.path != other_run.path
}

pub fn hook_receives_step_environment_and_config_cwd_test() {
  let dir = "test/tmp/workspace-run-hooks"
  reset_dir(dir)
  let create_hook =
    "mkdir -p \"$SCHERZO_WORKSPACE_PATH\"\nprintf '%s|%s|%s|%s|%s|%s|%s\\n' \"$PWD\" \"$SCHERZO_CONFIG_DIR\" \"$SCHERZO_STEP_ID\" \"$SCHERZO_WORKSPACE_NAME\" \"$SCHERZO_WORKSPACE_PATH\" \"$SCHERZO_SOURCE_WORKSPACE_NAME\" \"$SCHERZO_SOURCE_WORKSPACE_PATH\" >> hook.log\nif [ -n \"$SCHERZO_SOURCE_WORKSPACE_PATH\" ]; then cp \"$SCHERZO_SOURCE_WORKSPACE_PATH/marker\" \"$SCHERZO_WORKSPACE_PATH/from-source\"; fi"
  let before_hook = "pwd > \"$SCHERZO_WORKSPACE_PATH/before-cwd\""
  let orchestrator = orchestrator(dir, create_hook, before_hook)
  let assert Ok(main) =
    workspace_run.prepare_step(
      issue(),
      "implementation",
      "run-1",
      "implement",
      workflow_dag.WorkspaceRef(name: "main", from: None),
      orchestrator,
      dict.new(),
    )
  let assert Ok(Nil) = simplifile.write(main.path <> "/marker", "copied")
  let known = dict.from_list([#("main", main)])
  let assert Ok(review) =
    workspace_run.prepare_step(
      issue(),
      "implementation",
      "run-1",
      "code_review",
      workflow_dag.WorkspaceRef(name: "code-review", from: Some("main")),
      orchestrator,
      known,
    )
  let assert Ok(hook_log) =
    simplifile.read(orchestrator.config_dir <> "/hook.log")
  assert string.contains(hook_log, "|code_review|code-review|")
  assert string.contains(hook_log, "|main|")
  assert string.contains(hook_log, main.path)
  let assert Ok(copied) = simplifile.read(review.path <> "/from-source")
  assert copied == "copied"
  let assert Ok(before_cwd) = simplifile.read(review.path <> "/before-cwd")
  assert string.trim(before_cwd) == orchestrator.config_dir
}

pub fn cleanup_rejects_paths_outside_workspace_root_test() {
  let dir = "test/tmp/workspace-run-cleanup"
  reset_dir(dir)
  let orchestrator =
    orchestrator(dir, "mkdir -p \"$SCHERZO_WORKSPACE_PATH\"", "")
  let assert Error(error.WorkspaceOutsideRoot(_)) =
    workspace_run.cleanup_run("/tmp/not-under-scherzo-root", orchestrator)
}

pub fn cleanup_retention_marker_skips_delete_until_removed_test() {
  let dir = "test/tmp/workspace-run-retained-cleanup"
  reset_dir(dir)
  let orchestrator =
    orchestrator(dir, "mkdir -p \"$SCHERZO_WORKSPACE_PATH\"", "")
  let assert Ok(main) =
    workspace_run.prepare_step(
      issue(),
      "execplan-implementation",
      "run-retained",
      "prepare_plan",
      workflow_dag.WorkspaceRef(name: "main", from: None),
      orchestrator,
      dict.new(),
    )
  let marker = workspace_run.cleanup_retention_marker(main.run_root)
  let assert Ok(Nil) = simplifile.write(marker, "keep until PR publish\n")
  let assert Ok(Nil) = simplifile.write(main.path <> "/work", "saved")

  let assert Ok(Nil) = workspace_run.cleanup_run(main.run_root, orchestrator)
  let assert Ok(True) = simplifile.is_directory(main.run_root)
  let assert Ok(True) = simplifile.is_file(main.path <> "/work")

  let assert Ok(Nil) = simplifile.delete(marker)
  let assert Ok(Nil) = workspace_run.cleanup_run(main.run_root, orchestrator)
  let assert Ok(False) = simplifile.is_directory(main.run_root)
}

fn list_map(values: List(a), mapper: fn(a) -> b) -> List(b) {
  case values {
    [] -> []
    [value, ..rest] -> [mapper(value), ..list_map(rest, mapper)]
  }
}
