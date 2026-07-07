import gleam/option.{Some}
import gleam/string
import scherzo/command_step
import scherzo/step_artifact
import simplifile
import support/test_helpers

fn run_command(command: String) -> step_artifact.StepArtifact {
  command_step.run(
    "workflow-lint",
    command,
    ".",
    120_000,
    [],
    test_helpers.artifact_limits(16_000),
  )
}

fn lint_command(bundle_root: String) -> String {
  ".scherzo/workflows/scripts/scherzo-workflow-lint check --repo-root . --bundle-root "
  <> test_helpers.shell_quote(bundle_root)
  <> " --config .scherzo/scherzo.yaml"
}

fn assert_contains(contents: String, expected: String) -> Nil {
  case string.contains(contents, expected) {
    True -> Nil
    False -> {
      let message = "expected text not found: " <> expected
      panic as message
    }
  }
}

fn assert_success(artifact: step_artifact.StepArtifact) -> Nil {
  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
}

fn assert_lint_failure(
  artifact: step_artifact.StepArtifact,
  expected: String,
) -> Nil {
  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(1)
  assert_contains(artifact.stderr, expected)
}

fn copy_bundle(path: String) -> Nil {
  test_helpers.reset_dir(path)
  let artifact =
    run_command("cp -R workflows/dogfood/. " <> test_helpers.shell_quote(path))
  assert_success(artifact)
}

fn replace_in_file(path: String, old: String, new: String) -> Nil {
  let assert Ok(contents) = simplifile.read(path)
  let updated = string.replace(contents, each: old, with: new)
  assert updated != contents
  let assert Ok(Nil) = simplifile.write(path, updated)
  Nil
}

pub fn workflow_lint_passes_checked_in_bundle_test() {
  let artifact = run_command(lint_command(".scherzo/workflows"))

  assert_success(artifact)
  assert_contains(artifact.stdout, "WORKFLOW_LINT=ok")
  assert_contains(artifact.stdout, "PROMPT_ORPHANS=0")
  assert_contains(
    artifact.stdout,
    "SHARED_STEP_BLOCK=implementation-review-pipeline",
  )
  assert_contains(
    artifact.stdout,
    "SHARED_PROMPT_FRAGMENT=execplan-identity-model",
  )
}

pub fn workflow_lint_catches_orphan_prompt_test() {
  let bundle = "test/tmp/workflow-lint/orphan/bundle"
  copy_bundle(bundle)
  let assert Ok(Nil) =
    simplifile.write(bundle <> "/prompts/unused.md", "unused\n")

  let artifact = run_command(lint_command(bundle))

  assert_lint_failure(artifact, "orphan prompt")
}

pub fn workflow_lint_catches_seeded_shared_step_drift_test() {
  let bundle = "test/tmp/workflow-lint/step-drift/bundle"
  copy_bundle(bundle)
  replace_in_file(
    bundle <> "/execplan-implementation.yaml",
    "    timeout: 5m\n    run_in: main\n\n\n  - id: lane_correctness",
    "    run_in: main\n\n\n  - id: lane_correctness",
  )

  let artifact = run_command(lint_command(bundle))

  assert_lint_failure(artifact, "shared step block drift")
}

pub fn workflow_lint_catches_seeded_prompt_fragment_drift_test() {
  let bundle = "test/tmp/workflow-lint/prompt-drift/bundle"
  copy_bundle(bundle)
  replace_in_file(
    bundle <> "/prompts/execplan-implementation-apply-feedback.md",
    "implementation_handoff.issue_identifier",
    "implementation_handoff.issue_key",
  )

  let artifact = run_command(lint_command(bundle))

  assert_lint_failure(artifact, "shared prompt fragment drift")
}

pub fn workflow_lint_catches_dead_repo_root_assignment_test() {
  let bundle = "test/tmp/workflow-lint/dead-repo-root/bundle"
  copy_bundle(bundle)
  replace_in_file(
    bundle <> "/origin-sync.yaml",
    "run: ': \"${SCHERZO_WORKFLOW_BUNDLE_DIR:?Scherzo command-step contract missing SCHERZO_WORKFLOW_BUNDLE_DIR}\"; \"$SCHERZO_WORKFLOW_BUNDLE_DIR/scripts/scherzo-jj-origin-sync\"'",
    "run: 'repo_root=${SCHERZO_REPO_ROOT:-.}; echo ok'",
  )

  let artifact = run_command(lint_command(bundle))

  assert_lint_failure(artifact, "dead repo_root assignment")
}
