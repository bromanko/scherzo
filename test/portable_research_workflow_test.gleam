import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/command_step
import scherzo/config/types as config_types
import scherzo/path
import scherzo/runtime_bundle
import scherzo/step_artifact
import scherzo/workflow_dag
import simplifile
import support/test_helpers

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

fn example_research_dag() -> workflow_dag.WorkflowDag {
  let assert Ok(dag) =
    read_file("examples/workflows/research.yaml")
    |> workflow_dag.parse
  dag
}

fn collect_findings_step() -> workflow_dag.WorkflowStep {
  let assert Ok(step) =
    workflow_dag.step_by_id(example_research_dag(), "collect_findings")
  step
}

fn collect_findings_run() -> String {
  let step = collect_findings_step()
  let assert workflow_dag.CommandStep(run, _) = step.kind
  run
}

fn assert_contains(content: String, needle: String) -> Nil {
  assert string.contains(content, needle)
}

fn assert_not_contains(content: String, needle: String) -> Nil {
  assert !string.contains(content, needle)
}

fn absolute(value: String) -> String {
  case path.absolute(value) {
    Ok(value) -> value
    Error(_) -> value
  }
}

fn write_describe_driver(path: String, capabilities_json: String) -> Nil {
  let assert Ok(Nil) =
    simplifile.write(
      path,
      "#!/bin/sh\n"
        <> "if [ \"$1\" = describe ] && [ \"$2\" = --json ]; then\n"
        <> "  printf '%s\\n' '{\"version\":1,\"capabilities\":"
        <> capabilities_json
        <> "}'\n"
        <> "  exit 0\n"
        <> "fi\n"
        <> "exit 2\n",
    )
  test_helpers.chmod_executable(path)
}

fn write_fake_driver(path: String) -> Nil {
  let assert Ok(Nil) =
    simplifile.write(
      path,
      "#!/bin/sh\n"
        <> "set -eu\n"
        <> "printf '%s\\n' \"$@\" > driver-argv.log\n"
        <> "if [ \"$#\" -ne 3 ] || [ \"$1\" != assert-only ] || [ \"$2\" != --path ] || [ \"$3\" != research-findings.md ]; then\n"
        <> "  echo 'unexpected driver arguments' >&2\n"
        <> "  exit 64\n"
        <> "fi\n"
        <> "if [ -e unexpected-artifact.txt ]; then\n"
        <> "  echo 'unexpected artifact: unexpected-artifact.txt' >&2\n"
        <> "  exit 65\n"
        <> "fi\n",
    )
  test_helpers.chmod_executable(path)
}

fn setup_driver_workspace(dir: String) -> String {
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  let driver = dir <> "/bin/fake-driver"
  write_fake_driver(driver)
  absolute(driver)
}

fn driver_env(driver: String) -> List(#(String, String)) {
  [
    #("SCHERZO_WORKSPACE_DRIVER", driver),
    #("SCHERZO_CONFIG_DIR", absolute(".")),
  ]
}

pub fn example_research_workflow_is_driver_portable_test() {
  let dag = example_research_dag()
  assert dag.id == "research"
  assert dag.workspace_profile == Some("noop")
  assert dag.workspace_capabilities == [config_types.WorkspaceAssertOnly]

  let step_ids = dag.steps |> list.map(fn(step) { step.id })
  assert step_ids == ["research", "collect_findings"]

  let assert Ok(research) = workflow_dag.step_by_id(dag, "research")
  assert research.depends_on == []
  assert research.workspace == workflow_dag.WorkspaceRef("main", None)
  let assert workflow_dag.AgentStep(
    workflow_dag.PromptFile("prompts/research.md"),
    None,
  ) = research.kind
  assert research.recover == None

  let collect = collect_findings_step()
  assert collect.depends_on == ["research"]
  assert collect.workspace == workflow_dag.WorkspaceRef("main", None)
  let assert Some(collect_recover) = collect.recover
  assert collect_recover
    == workflow_dag.RecoveryConfigPatch(
      enabled: None,
      attempts: Some(1),
      model: None,
      prompt: Some(workflow_dag.PromptFile(
        "prompts/research-recover-failed-step.md",
      )),
    )
  let assert Ok(Some(effective_recover)) =
    workflow_dag.effective_recovery_config(dag, collect)
  assert effective_recover
    == workflow_dag.EffectiveRecoveryConfig(
      attempts: 1,
      model: None,
      prompt: workflow_dag.PromptFile("prompts/research-recover-failed-step.md"),
    )
  let assert workflow_dag.CommandStep(run, _) = collect.kind
  assert_contains(run, "SCHERZO_WORKSPACE_DRIVER")
  assert_contains(run, "assert-only --path")
  assert_contains(run, "research-findings.md")
  assert_contains(run, "cat")
  assert_contains(run, "driver_command=${SCHERZO_WORKSPACE_DRIVER")
  assert_contains(run, "SCHERZO_CONFIG_DIR")
  assert_contains(run, "\"$driver\" assert-only")
  assert_not_contains(run, "\"$SCHERZO_WORKSPACE_DRIVER\" assert-only")
  assert_not_contains(run, "jj")
  assert_not_contains(run, "git diff")
  assert_not_contains(run, "Linear")
}

pub fn example_research_prompt_is_tracker_and_vcs_neutral_test() {
  let prompt = read_file("examples/workflows/prompts/research.md")
  assert_contains(prompt, "task {{ issue.identifier }}")
  assert_contains(prompt, "dedicated workflow workspace")
  assert_contains(prompt, "research-findings.md")
  assert_contains(prompt, "## Brief summary")
  assert_contains(prompt, "## Findings")
  assert_contains(prompt, "## Evidence")
  assert_contains(prompt, "## Issues encountered")
  assert_contains(prompt, "## Recommendation")
  assert_contains(prompt, "avoid")
  assert_contains(prompt, "clean up")
  assert_not_contains(prompt, "Linear")
  assert_not_contains(prompt, "jj")
  assert_not_contains(prompt, "jj status")
  assert_not_contains(prompt, "git status")
  assert_not_contains(prompt, "git diff")
  assert_not_contains(prompt, "pull request")
}

pub fn example_research_recovery_prompt_is_bounded_to_collection_test() {
  let prompt =
    read_file("examples/workflows/prompts/research-recover-failed-step.md")
  assert_contains(prompt, "collect_findings")
  assert_contains(prompt, "research-findings.md")
  assert_contains(prompt, "one-artifact contract")
  assert_contains(prompt, "submit_workflow_step_recovery_result")
  assert_contains(prompt, "retry_requested")
  assert_contains(prompt, "gave_up")
  assert_contains(prompt, "missing workspace driver")
  assert_contains(prompt, "unsafe unexpected source change")
  assert_not_contains(prompt, "Linear")
  assert_not_contains(prompt, "jj")
  assert_not_contains(prompt, "git status")
  assert_not_contains(prompt, "git diff")
  assert_not_contains(prompt, "pull request")
}

pub fn example_research_package_profile_supports_assert_only_test() {
  let assert Ok(bundle) =
    runtime_bundle.load_with_env(Some("examples/scherzo.yaml"), env)
  let assert Ok(dag) = dict.get(bundle.workflows, "research")
  assert dag.workspace_capabilities == [config_types.WorkspaceAssertOnly]

  let assert Some(profile_name) = dag.workspace_profile
  let assert Ok(profile) =
    dict.get(bundle.orchestrator.workspace_profiles.profiles, profile_name)
  let assert Some(driver) = profile.driver
  assert driver.command == "../scripts/scherzo-workspace-noop"
  assert list.contains(driver.capabilities, config_types.WorkspaceAssertOnly)
}

pub fn example_research_package_rejects_profile_without_assert_only_test() {
  let dir = "test/tmp/portable-research-workflow/missing-capability-package"
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/workflows")
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/scripts")
  write_describe_driver(
    dir <> "/scripts/scherzo-workspace-noop",
    "[\"status\"]",
  )
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/workflows/research.yaml",
      "version: 1\nid: research\nworkspace_profile: noop\nworkspace_capabilities: [assert-only]\nsteps:\n  - id: collect\n    kind: command\n    run: echo ok\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/scherzo.yaml",
      "version: 1\ntracker:\n  kind: linear\n  api_key: linearkey\n  project_slug: TEST\n  states:\n    ready: [Todo]\nworkspace:\n  root: workspaces\n  default_profile: noop\n  profiles:\n    noop:\n      driver:\n        command: scripts/scherzo-workspace-noop\nworkflows:\n    research: workflows/research.yaml\n",
    )

  let assert Error(runtime_bundle.BundleError(code, message)) =
    runtime_bundle.load_with_env(Some(dir <> "/scherzo.yaml"), env)
  assert code == "workspace_capabilities_unavailable"
  assert_contains(message, "workflow research")
  assert_contains(message, "missing: assert-only")
}

pub fn collect_findings_command_executes_driver_and_streams_findings_test() {
  let dir = "test/tmp/portable-research-workflow/happy"
  let driver = setup_driver_workspace(dir)
  let report = "# Research findings\n\n## Brief summary\n\nPortable.\n"
  let assert Ok(Nil) = simplifile.write(dir <> "/research-findings.md", report)

  let artifact =
    command_step.run_with_env(
      "collect_findings",
      collect_findings_run(),
      dir,
      5000,
      driver_env(driver),
      [],
      test_helpers.default_artifact_limits(),
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert_contains(artifact.stdout, report)
  let assert Ok(argv_log) = simplifile.read(dir <> "/driver-argv.log")
  assert argv_log == "assert-only\n--path\nresearch-findings.md\n"
}

pub fn collect_findings_command_fails_when_driver_rejects_extra_artifact_test() {
  let dir = "test/tmp/portable-research-workflow/extra-artifact"
  let driver = setup_driver_workspace(dir)
  let report = "# Research findings\n\nBody that must not stream.\n"
  let assert Ok(Nil) = simplifile.write(dir <> "/research-findings.md", report)
  let assert Ok(Nil) =
    simplifile.write(dir <> "/unexpected-artifact.txt", "extra\n")

  let artifact =
    command_step.run_with_env(
      "collect_findings",
      collect_findings_run(),
      dir,
      5000,
      driver_env(driver),
      [],
      test_helpers.default_artifact_limits(),
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(65)
  assert_contains(artifact.stderr, "unexpected artifact")
  assert_not_contains(artifact.stdout, "Body that must not stream")
}

pub fn collect_findings_command_requires_driver_and_findings_file_test() {
  let missing_driver_dir = "test/tmp/portable-research-workflow/missing-driver"
  test_helpers.reset_dir(missing_driver_dir)
  let assert Ok(Nil) =
    simplifile.write(missing_driver_dir <> "/research-findings.md", "report\n")
  let missing_driver_artifact =
    command_step.run_with_env(
      "collect_findings",
      collect_findings_run(),
      missing_driver_dir,
      5000,
      [#("SCHERZO_WORKSPACE_DRIVER", "")],
      [],
      test_helpers.default_artifact_limits(),
    )
  assert missing_driver_artifact.status == step_artifact.StepFailed
  assert_contains(
    missing_driver_artifact.stderr,
    "SCHERZO_WORKSPACE_DRIVER is required for the research workflow",
  )

  let missing_findings_dir =
    "test/tmp/portable-research-workflow/missing-findings"
  let driver = setup_driver_workspace(missing_findings_dir)
  let missing_findings_artifact =
    command_step.run_with_env(
      "collect_findings",
      collect_findings_run(),
      missing_findings_dir,
      5000,
      driver_env(driver),
      [],
      test_helpers.default_artifact_limits(),
    )
  assert missing_findings_artifact.status == step_artifact.StepFailed
  assert missing_findings_artifact.exit_code == Some(1)
  let assert Ok(False) =
    simplifile.is_file(missing_findings_dir <> "/driver-argv.log")
}
