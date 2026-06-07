import gleam/bit_array
import gleam/dict
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/artifact_publication_config
import scherzo/artifact_publication_driver
import scherzo/artifact_publication_executor
import scherzo/artifact_publication_manifest
import scherzo/artifact_repository/command_runner
import scherzo/commit_stack_artifact
import scherzo/config/types as config_types
import scherzo/hash
import scherzo/path
import scherzo/state/ledger
import scherzo/state/projection
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state
import scherzo/workflow_checkpoint
import scherzo/workflow_contract
import scherzo/workflow_contract_manifest
import simplifile
import support/test_helpers

pub fn execute_routes_prepares_artifact_bytes_and_records_published_attempt_test() {
  let root = "test/tmp/artifact-publication-executor/published"
  test_helpers.reset_dir(root)
  write_template(root)
  write_artifact(root, plan_ref(), plan_contents())
  let checkpoint = workflow_checkpoint.ledger_writer(root, fn() { 123 })

  let assert Ok(result) =
    artifact_publication_executor.execute_routes_with_runner(
      [route(True)],
      repositories(),
      root,
      output_manifest(),
      issue(),
      "run-1",
      checkpoint,
      fake_runner(),
    )

  assert result.required_failures == []
  assert list.length(result.attempts) == 1
  let assert [attempt] = result.attempts
  assert attempt.status == "published"
  let projected = load_projection(root)
  let assert Ok(latest) =
    projection.latest_publication_for_run(
      projected,
      "run-1",
      "execplan_review_doc",
    )
  assert latest.status == "published"
  assert latest.retry_execution_available == True
}

pub fn execute_routes_publishes_commit_stack_with_workspace_driver_test() {
  let root = "test/tmp/artifact-publication-executor/commit-stack-driver"
  let state_root = root <> "/state"
  let workspace = root <> "/workspace"
  let log = root <> "/driver.log"
  test_helpers.reset_dir(root)
  let assert Ok(Nil) = simplifile.create_directory_all(workspace)
  write_commit_stack_artifacts(state_root)
  let checkpoint = workflow_checkpoint.ledger_writer(state_root, fn() { 123 })

  let assert Ok(result) =
    artifact_publication_executor.execute_routes_with_runner_and_state_root_and_publication_driver(
      [commit_stack_route(True)],
      repositories(),
      root,
      root,
      state_root,
      commit_stack_output_manifest(),
      issue(),
      "run-1",
      checkpoint,
      driver_runner(log, workspace, DriverPublishes),
      Some(
        publication_driver(workspace, [
          config_types.WorkspacePublishChange,
        ]),
      ),
    )

  assert result.required_failures == []
  let assert [attempt] = result.attempts
  assert attempt.status == "published"
  let manifest = read_publication_manifest(state_root, attempt.manifest_ref)
  assert manifest.pr_url == Some(driver_pr_url())
  assert manifest.branch == Some(existing_branch())
  assert manifest.commit_sha == Some(driver_head_revision())
  assert manifest.base_ref == Some("main")
  assert manifest.base_revision == Some(driver_base_revision())
  assert manifest.head_revision == Some(driver_head_revision())
  assert manifest.change_id == Some(driver_change_id())
  assert simplifile.is_directory(
      state_root <> "/.scherzo-state/artifact-repositories/github",
    )
    == Ok(False)
  let transcript = read_file(log)
  assert string.contains(
    transcript,
    "fake-driver publish-change --kind implementation",
  )
  assert string.contains(transcript, "--base main")
  assert string.contains(transcript, "--target-branch " <> existing_branch())
  assert string.contains(transcript, "--target-pr 42")
  assert string.contains(transcript, "--allow-no-changes true --json")
  assert string.contains(transcript, "CWD=" <> workspace)
}

pub fn execute_routes_commit_stack_passes_configured_driver_timeout_test() {
  let root = "test/tmp/artifact-publication-executor/commit-stack-timeout"
  let state_root = root <> "/state"
  let workspace = root <> "/workspace"
  let log = root <> "/driver.log"
  test_helpers.reset_dir(root)
  let assert Ok(Nil) = simplifile.create_directory_all(workspace)
  write_commit_stack_artifacts(state_root)

  let assert Ok(result) =
    artifact_publication_executor.execute_routes_with_runner_and_state_root_and_publication_driver(
      [commit_stack_route(True)],
      repositories(),
      root,
      root,
      state_root,
      commit_stack_output_manifest(),
      issue(),
      "run-1",
      workflow_checkpoint.ledger_writer(state_root, fn() { 123 }),
      driver_runner(log, workspace, DriverPublishes),
      Some(publication_driver_with_timeout(
        workspace,
        [config_types.WorkspacePublishChange],
        1234,
      )),
    )

  assert result.required_failures == []
  assert string.contains(read_file(log), "TIMEOUT=1234")
}

pub fn execute_routes_commit_stack_rerun_rechecks_stale_target_with_driver_test() {
  let root = "test/tmp/artifact-publication-executor/commit-stack-stale-rerun"
  let state_root = root <> "/state"
  let workspace = root <> "/workspace"
  let log = root <> "/driver.log"
  test_helpers.reset_dir(root)
  let assert Ok(Nil) = simplifile.create_directory_all(workspace)
  write_commit_stack_artifacts(state_root)

  let assert Ok(first) =
    artifact_publication_executor.execute_routes_with_runner_and_state_root_and_publication_driver(
      [commit_stack_route(True)],
      repositories(),
      root,
      root,
      state_root,
      commit_stack_output_manifest(),
      issue(),
      "run-1",
      workflow_checkpoint.ledger_writer(state_root, fn() { 123 }),
      driver_runner(log, workspace, DriverPublishes),
      Some(
        publication_driver(workspace, [
          config_types.WorkspacePublishChange,
        ]),
      ),
    )
  let assert [first_attempt] = first.attempts
  assert first_attempt.status == "published"

  let assert Ok(second) =
    artifact_publication_executor.execute_routes_with_runner_and_state_root_and_publication_driver(
      [commit_stack_route(True)],
      repositories(),
      root,
      root,
      state_root,
      commit_stack_output_manifest(),
      issue(),
      "run-1",
      workflow_checkpoint.ledger_writer(state_root, fn() { 456 }),
      driver_runner(log, workspace, DriverStaleExistingBranch),
      Some(
        publication_driver(workspace, [
          config_types.WorkspacePublishChange,
        ]),
      ),
    )

  let assert [failure] = second.required_failures
  assert failure.code == "stale_existing_branch"
  let assert [second_attempt] = second.attempts
  assert second_attempt.status == "failed"
  assert second_attempt.error_code == Some("stale_existing_branch")
  assert string.contains(read_file(log), "fake-driver publish-change")
}

pub fn execute_routes_commit_stack_accepts_successor_driver_operation_test() {
  let root = "test/tmp/artifact-publication-executor/commit-stack-successor"
  let state_root = root <> "/state"
  let workspace = root <> "/workspace"
  let log = root <> "/driver.log"
  test_helpers.reset_dir(root)
  let assert Ok(Nil) = simplifile.create_directory_all(workspace)
  write_commit_stack_artifacts(state_root)

  let assert Ok(result) =
    artifact_publication_executor.execute_routes_with_runner_and_state_root_and_publication_driver(
      [commit_stack_route(True)],
      repositories(),
      root,
      root,
      state_root,
      commit_stack_output_manifest(),
      issue(),
      "run-1",
      workflow_checkpoint.ledger_writer(state_root, fn() { 123 }),
      driver_runner(log, workspace, DriverPublishes),
      Some(
        publication_driver(workspace, [
          config_types.WorkspacePublishCommitStack,
        ]),
      ),
    )

  assert result.required_failures == []
  assert string.contains(read_file(log), "fake-driver publish-commit-stack")
}

pub fn execute_routes_commit_stack_unchanged_records_existing_pr_url_test() {
  let root = "test/tmp/artifact-publication-executor/commit-stack-unchanged"
  let state_root = root <> "/state"
  let workspace = root <> "/workspace"
  test_helpers.reset_dir(root)
  let assert Ok(Nil) = simplifile.create_directory_all(workspace)
  write_commit_stack_artifacts(state_root)

  let assert Ok(result) =
    artifact_publication_executor.execute_routes_with_runner_and_state_root_and_publication_driver(
      [commit_stack_route(True)],
      repositories(),
      root,
      root,
      state_root,
      commit_stack_output_manifest(),
      issue(),
      "run-1",
      workflow_checkpoint.ledger_writer(state_root, fn() { 123 }),
      driver_runner(root <> "/driver.log", workspace, DriverUnchangedNoUrl),
      Some(
        publication_driver(workspace, [
          config_types.WorkspacePublishChange,
        ]),
      ),
    )

  assert result.required_failures == []
  let assert [attempt] = result.attempts
  assert attempt.status == "unchanged"
  let manifest = read_publication_manifest(state_root, attempt.manifest_ref)
  assert manifest.pr_url == Some(existing_pr_url())
  assert manifest.branch == Some(existing_branch())
  assert manifest.base_ref == Some("main")
  assert manifest.base_revision == Some(driver_base_revision())
  assert manifest.head_revision == Some(driver_head_revision())
}

pub fn execute_routes_fails_commit_stack_when_driver_unavailable_test() {
  let root = "test/tmp/artifact-publication-executor/commit-stack-no-driver"
  test_helpers.reset_dir(root)
  write_commit_stack_artifacts(root)

  let assert Ok(result) =
    artifact_publication_executor.execute_routes_with_runner(
      [commit_stack_route(True)],
      repositories(),
      root,
      commit_stack_output_manifest(),
      issue(),
      "run-1",
      workflow_checkpoint.ledger_writer(root, fn() { 123 }),
      fail_if_called_runner(),
    )

  let assert [failure] = result.required_failures
  assert failure.code == "commit_stack_publication_driver_unavailable"
  let assert [attempt] = result.attempts
  assert attempt.status == "failed"
  assert attempt.error_code
    == Some("commit_stack_publication_driver_unavailable")
}

pub fn execute_routes_fails_commit_stack_on_malformed_driver_output_test() {
  let root = "test/tmp/artifact-publication-executor/commit-stack-malformed"
  let state_root = root <> "/state"
  let workspace = root <> "/workspace"
  test_helpers.reset_dir(root)
  let assert Ok(Nil) = simplifile.create_directory_all(workspace)
  write_commit_stack_artifacts(state_root)

  let assert Ok(result) =
    artifact_publication_executor.execute_routes_with_runner_and_state_root_and_publication_driver(
      [commit_stack_route(True)],
      repositories(),
      root,
      root,
      state_root,
      commit_stack_output_manifest(),
      issue(),
      "run-1",
      workflow_checkpoint.ledger_writer(state_root, fn() { 123 }),
      driver_runner(root <> "/driver.log", workspace, DriverMalformed),
      Some(
        publication_driver(workspace, [
          config_types.WorkspacePublishChange,
        ]),
      ),
    )

  let assert [failure] = result.required_failures
  assert failure.code == "workspace_driver_publish_malformed"
  let assert [attempt] = result.attempts
  assert attempt.status == "failed"
  assert attempt.error_code == Some("workspace_driver_publish_malformed")
  assert simplifile.is_directory(
      state_root <> "/.scherzo-state/artifact-repositories/github",
    )
    == Ok(False)
}

pub fn execute_routes_fails_commit_stack_on_driver_head_mismatch_test() {
  let root = "test/tmp/artifact-publication-executor/commit-stack-head-mismatch"
  let state_root = root <> "/state"
  let workspace = root <> "/workspace"
  test_helpers.reset_dir(root)
  let assert Ok(Nil) = simplifile.create_directory_all(workspace)
  write_commit_stack_artifacts(state_root)

  let assert Ok(result) =
    artifact_publication_executor.execute_routes_with_runner_and_state_root_and_publication_driver(
      [commit_stack_route(True)],
      repositories(),
      root,
      root,
      state_root,
      commit_stack_output_manifest(),
      issue(),
      "run-1",
      workflow_checkpoint.ledger_writer(state_root, fn() { 123 }),
      driver_runner(root <> "/driver.log", workspace, DriverHeadMismatch),
      Some(
        publication_driver(workspace, [
          config_types.WorkspacePublishChange,
        ]),
      ),
    )

  let assert [failure] = result.required_failures
  assert failure.code == "workspace_driver_publish_head_mismatch"
  let assert [attempt] = result.attempts
  assert attempt.status == "failed"
  assert attempt.error_code == Some("workspace_driver_publish_head_mismatch")
}

pub fn execute_routes_redacts_sensitive_commit_stack_driver_failure_test() {
  let root = "test/tmp/artifact-publication-executor/commit-stack-redaction"
  let state_root = root <> "/state"
  let workspace = root <> "/workspace"
  let secret = "supersecret-token"
  test_helpers.reset_dir(root)
  let assert Ok(Nil) = simplifile.create_directory_all(workspace)
  write_commit_stack_artifacts(state_root)

  let assert Ok(result) =
    artifact_publication_executor.execute_routes_with_runner_and_state_root_and_publication_driver(
      [commit_stack_route(True)],
      repositories(),
      root,
      root,
      state_root,
      commit_stack_output_manifest(),
      issue(),
      "run-1",
      workflow_checkpoint.ledger_writer(state_root, fn() { 123 }),
      driver_runner(
        root <> "/driver.log",
        workspace,
        DriverLeaksSecretFailure(secret),
      ),
      Some(
        publication_driver_with_env(
          workspace,
          [config_types.WorkspacePublishChange],
          [#("GITHUB_TOKEN", secret)],
          [secret],
        ),
      ),
    )

  let assert [failure] = result.required_failures
  assert failure.code == "workspace_driver_publish_failed"
  assert !string.contains(failure.message, secret)
  assert string.contains(failure.message, "[REDACTED]")
  let assert [attempt] = result.attempts
  assert attempt.status == "failed"
  assert attempt.error_message == Some(failure.message)
}

pub fn execute_routes_fails_commit_stack_on_unsuccessful_driver_status_test() {
  let root = "test/tmp/artifact-publication-executor/commit-stack-unsuccessful"
  let state_root = root <> "/state"
  let workspace = root <> "/workspace"
  test_helpers.reset_dir(root)
  let assert Ok(Nil) = simplifile.create_directory_all(workspace)
  write_commit_stack_artifacts(state_root)

  let assert Ok(result) =
    artifact_publication_executor.execute_routes_with_runner_and_state_root_and_publication_driver(
      [commit_stack_route(False)],
      repositories(),
      root,
      root,
      state_root,
      commit_stack_output_manifest(),
      issue(),
      "run-1",
      workflow_checkpoint.ledger_writer(state_root, fn() { 123 }),
      driver_runner(root <> "/driver.log", workspace, DriverUnsuccessful),
      Some(
        publication_driver(workspace, [
          config_types.WorkspacePublishChange,
        ]),
      ),
    )

  assert result.required_failures == []
  let assert [failure] = result.optional_failures
  assert failure.code == "remote_rejected"
  let assert [attempt] = result.attempts
  assert attempt.status == "failed"
  assert attempt.error_code == Some("remote_rejected")
}

pub fn execute_routes_fails_when_planned_artifact_bytes_disappear_test() {
  let root = "test/tmp/artifact-publication-executor/missing-bytes"
  test_helpers.reset_dir(root)
  write_template(root)
  write_artifact(root, plan_ref(), plan_contents())
  let checkpoint = disappearing_checkpoint(root)

  let assert Ok(result) =
    artifact_publication_executor.execute_routes_with_runner(
      [route(True)],
      repositories(),
      root,
      output_manifest(),
      issue(),
      "run-1",
      checkpoint,
      fake_runner(),
    )

  let assert [failure] = result.required_failures
  assert failure.code == "missing_artifact_bytes"
  let assert [attempt] = result.attempts
  assert attempt.status == "failed"
  assert attempt.error_code == Some("missing_artifact_bytes")
}

pub fn execute_routes_classifies_required_and_optional_failures_test() {
  let required_root = "test/tmp/artifact-publication-executor/required-failure"
  test_helpers.reset_dir(required_root)
  write_template(required_root)
  write_artifact(required_root, plan_ref(), plan_contents())

  let assert Ok(required_result) =
    artifact_publication_executor.execute_routes_with_runner(
      [route(True)],
      repositories(),
      required_root,
      output_manifest(),
      issue(),
      "run-1",
      workflow_checkpoint.ledger_writer(required_root, fn() { 123 }),
      commit_failure_runner(),
    )

  let assert [required_failure] = required_result.required_failures
  assert required_failure.code == "git_commit_failed"
  assert required_result.optional_failures == []
  let assert [required_attempt] = required_result.attempts
  assert required_attempt.status == "failed"

  let optional_root = "test/tmp/artifact-publication-executor/optional-failure"
  test_helpers.reset_dir(optional_root)
  write_template(optional_root)
  write_artifact(optional_root, plan_ref(), plan_contents())

  let assert Ok(optional_result) =
    artifact_publication_executor.execute_routes_with_runner(
      [route(False)],
      repositories(),
      optional_root,
      output_manifest(),
      issue(),
      "run-1",
      workflow_checkpoint.ledger_writer(optional_root, fn() { 123 }),
      commit_failure_runner(),
    )

  assert optional_result.required_failures == []
  let assert [optional_failure] = optional_result.optional_failures
  assert optional_failure.code == "git_commit_failed"
  let assert [optional_attempt] = optional_result.attempts
  assert optional_attempt.status == "failed"
}

pub fn execute_routes_dedupes_repeated_finalization_after_success_test() {
  let root = "test/tmp/artifact-publication-executor/finalization-dedupe"
  test_helpers.reset_dir(root)
  write_template(root)
  write_artifact(root, plan_ref(), plan_contents())

  let assert Ok(first) =
    artifact_publication_executor.execute_routes_with_runner(
      [route(True)],
      repositories(),
      root,
      output_manifest(),
      issue(),
      "run-1",
      workflow_checkpoint.ledger_writer(root, fn() { 123 }),
      fake_runner(),
    )
  let assert [first_attempt] = first.attempts
  assert first_attempt.status == "published"

  let assert Ok(second) =
    artifact_publication_executor.execute_routes_with_runner(
      [route(True)],
      repositories(),
      root,
      output_manifest(),
      issue(),
      "run-1",
      workflow_checkpoint.ledger_writer(root, fn() { 456 }),
      fail_if_called_runner(),
    )
  let assert [second_attempt] = second.attempts
  assert second_attempt.status == "published"

  let projected = load_projection(root)
  let attempts =
    projection.publication_attempts_for_run(
      projected,
      "run-1",
      "execplan_review_doc",
    )
  assert list.length(attempts) == 1
}

pub fn execute_routes_with_state_root_uses_state_root_for_managed_checkout_test() {
  let root = "test/tmp/artifact-publication-executor/separate-state-root"
  let config_dir = root <> "/config"
  let state_root = root <> "/state"
  test_helpers.reset_dir(root)
  write_template(config_dir)
  write_artifact(state_root, plan_ref(), plan_contents())

  let assert Ok(result) =
    artifact_publication_executor.execute_routes_with_runner_and_state_root(
      [route(True)],
      repositories(),
      config_dir,
      config_dir,
      state_root,
      output_manifest(),
      issue(),
      "run-1",
      workflow_checkpoint.ledger_writer(state_root, fn() { 123 }),
      state_root_runner(state_root),
    )

  assert result.required_failures == []
  let assert [attempt] = result.attempts
  assert attempt.status == "published"
}

pub fn execute_recovered_routes_with_state_root_uses_state_root_for_managed_checkout_test() {
  let root =
    "test/tmp/artifact-publication-executor/separate-state-root-recovered"
  let config_dir = root <> "/config"
  let state_root = root <> "/state"
  test_helpers.reset_dir(root)
  write_template(config_dir)
  write_artifact(state_root, plan_ref(), plan_contents())

  let assert Ok(result) =
    artifact_publication_executor.execute_recovered_routes_with_runner_and_state_root(
      [route(True)],
      repositories(),
      config_dir,
      config_dir,
      state_root,
      output_manifest(),
      issue(),
      "run-1",
      workflow_checkpoint.ledger_writer(state_root, fn() { 123 }),
      state_root_runner(state_root),
    )

  assert result.required_failures == []
  let assert [attempt] = result.attempts
  assert attempt.status == "published"
}

pub fn recovered_routes_preserve_failed_publication_attempt_test() {
  let root = "test/tmp/artifact-publication-executor/recovered-failed"
  test_helpers.reset_dir(root)
  write_template(root)
  write_artifact(root, plan_ref(), plan_contents())

  let assert Ok(first) =
    artifact_publication_executor.execute_routes_with_runner(
      [route(True)],
      repositories(),
      root,
      output_manifest(),
      issue(),
      "run-1",
      workflow_checkpoint.ledger_writer(root, fn() { 123 }),
      commit_failure_runner(),
    )
  let assert [first_failure] = first.required_failures
  assert first_failure.code == "git_commit_failed"

  let assert Ok(second) =
    artifact_publication_executor.execute_recovered_routes_with_runner(
      [route(True)],
      repositories(),
      root,
      output_manifest(),
      issue(),
      "run-1",
      workflow_checkpoint.ledger_writer(root, fn() { 456 }),
      fake_runner(),
    )

  let assert [second_failure] = second.required_failures
  assert second_failure.code == "git_commit_failed"
  let assert [attempt] = second.attempts
  assert attempt.status == "failed"
  let attempts =
    projection.publication_attempts_for_run(
      load_projection(root),
      "run-1",
      "execplan_review_doc",
    )
  assert list.length(attempts) == 1
}

pub fn execute_routes_resolves_route_template_from_workflow_bundle_dir_test() {
  let root = "test/tmp/artifact-publication-executor/workflow-template-root"
  let config_dir = root <> "/config"
  let workflow_bundle_dir = root <> "/workflows/execplan"
  let state_root = root <> "/state"
  test_helpers.reset_dir(root)
  write_template(workflow_bundle_dir)
  write_artifact(state_root, plan_ref(), plan_contents())

  let assert Ok(result) =
    artifact_publication_executor.execute_routes_with_runner_and_state_root(
      [route(True)],
      repositories(),
      config_dir,
      workflow_bundle_dir,
      state_root,
      output_manifest(),
      issue(),
      "run-1",
      workflow_checkpoint.ledger_writer(state_root, fn() { 123 }),
      fake_runner(),
    )

  assert result.required_failures == []
  let assert [attempt] = result.attempts
  assert attempt.status == "published"
}

fn fake_runner() -> command_runner.Runner {
  command_runner.Runner(run: fake_command)
}

fn state_root_runner(expected_root: String) -> command_runner.Runner {
  command_runner.Runner(run: fn(spec) {
    let command_runner.CommandSpec(cwd: cwd, ..) = spec
    case
      string.starts_with(
        path.absolute_or_original(cwd),
        path.absolute_or_original(expected_root),
      )
    {
      True -> fake_command(spec)
      False -> Error(command_runner.command_error("wrong_workspace_root"))
    }
  })
}

fn fake_command(
  spec: command_runner.CommandSpec,
) -> Result(command_runner.CommandOutput, command_runner.CommandError) {
  let command_runner.CommandSpec(
    executable: executable,
    args: args,
    cwd: cwd,
    ..,
  ) = spec
  let _ = simplifile.create_directory_all(cwd)
  case executable, args {
    "git", ["clone", _, target] -> {
      let _ = simplifile.create_directory_all(target)
      Ok(command_runner.CommandOutput(0, "", ""))
    }
    "git", ["fetch", ..] -> Ok(command_runner.CommandOutput(0, "", ""))
    "git", ["remote", "get-url", "origin"] ->
      Ok(command_runner.CommandOutput(
        0,
        "https://github.com/scherzo-systems/scherzo.git",
        "",
      ))
    "git", ["ls-remote", ..] -> Ok(command_runner.CommandOutput(2, "", ""))
    "git", ["rev-parse", "--verify", ..] ->
      Ok(command_runner.CommandOutput(1, "", ""))
    "git", ["checkout", ..] -> Ok(command_runner.CommandOutput(0, "", ""))
    "git", ["status", ..] -> Ok(command_runner.CommandOutput(0, "", ""))
    "git", ["add", ..] -> Ok(command_runner.CommandOutput(0, "", ""))
    "git", ["diff", ..] -> Ok(command_runner.CommandOutput(1, "", ""))
    "git", ["commit", ..] -> Ok(command_runner.CommandOutput(0, "", ""))
    "git", ["rev-parse", "HEAD"] ->
      Ok(command_runner.CommandOutput(0, "deadbeef", ""))
    "git", ["push", ..] -> Ok(command_runner.CommandOutput(0, "", ""))
    "gh", ["pr", "list", ..] -> Ok(command_runner.CommandOutput(0, "[]", ""))
    "gh", ["pr", "create", ..] ->
      Ok(command_runner.CommandOutput(0, "https://example.test/pr/1", ""))
    _, _ -> Error(command_runner.command_error("unexpected_command"))
  }
}

type DriverBehavior {
  DriverPublishes
  DriverUnchangedNoUrl
  DriverStaleExistingBranch
  DriverHeadMismatch
  DriverMalformed
  DriverUnsuccessful
  DriverLeaksSecretFailure(String)
}

fn publication_driver(
  workspace: String,
  capabilities: List(config_types.WorkspaceCapability),
) -> artifact_publication_driver.WorkspacePublicationDriver {
  publication_driver_with_env(
    workspace,
    capabilities,
    [#("DRIVER_ENV", "present")],
    [],
  )
}

fn publication_driver_with_timeout(
  workspace: String,
  capabilities: List(config_types.WorkspaceCapability),
  timeout_ms: Int,
) -> artifact_publication_driver.WorkspacePublicationDriver {
  publication_driver_with_env_and_timeout(
    workspace,
    capabilities,
    [#("DRIVER_ENV", "present")],
    [],
    timeout_ms,
  )
}

fn publication_driver_with_env(
  workspace: String,
  capabilities: List(config_types.WorkspaceCapability),
  env: List(#(String, String)),
  redaction_values: List(String),
) -> artifact_publication_driver.WorkspacePublicationDriver {
  publication_driver_with_env_and_timeout(
    workspace,
    capabilities,
    env,
    redaction_values,
    60_000,
  )
}

fn publication_driver_with_env_and_timeout(
  workspace: String,
  capabilities: List(config_types.WorkspaceCapability),
  env: List(#(String, String)),
  redaction_values: List(String),
  timeout_ms: Int,
) -> artifact_publication_driver.WorkspacePublicationDriver {
  artifact_publication_driver.WorkspacePublicationDriver(
    workspace_path: workspace,
    command: "fake-driver",
    capabilities: capabilities,
    env: env,
    redaction_values: redaction_values,
    timeout_ms: timeout_ms,
  )
}

fn driver_runner(
  log: String,
  expected_workspace: String,
  behavior: DriverBehavior,
) -> command_runner.Runner {
  command_runner.Runner(run: fn(spec) {
    let command_runner.CommandSpec(
      executable: executable,
      args: args,
      cwd: cwd,
      env: env,
      timeout_ms: timeout_ms,
      ..,
    ) = spec
    let _ = write_driver_log(log, executable, args, cwd, env, timeout_ms)
    case executable == "fake-driver", cwd == expected_workspace {
      True, True -> driver_output(behavior, args)
      _, _ -> Error(command_runner.command_error("unexpected_driver_command"))
    }
  })
}

fn write_driver_log(
  log: String,
  executable: String,
  args: List(String),
  cwd: String,
  env: List(#(String, String)),
  timeout_ms: Option(Int),
) -> Nil {
  let _ = case path.dirname(log) {
    Ok(dir) -> simplifile.create_directory_all(dir)
    Error(Nil) -> Ok(Nil)
  }
  let _ =
    simplifile.write(
      log,
      executable
        <> " "
        <> string.join(args, with: " ")
        <> "\nCWD="
        <> cwd
        <> "\nENV="
        <> render_env(env)
        <> "\nTIMEOUT="
        <> render_timeout_ms(timeout_ms),
    )
  Nil
}

fn render_env(env: List(#(String, String))) -> String {
  env
  |> list.map(fn(entry) {
    let #(key, value) = entry
    key <> "=" <> value
  })
  |> string.join(with: " ")
}

fn render_timeout_ms(timeout_ms: Option(Int)) -> String {
  case timeout_ms {
    Some(timeout_ms) -> int.to_string(timeout_ms)
    None -> "none"
  }
}

fn driver_output(
  behavior: DriverBehavior,
  args: List(String),
) -> Result(command_runner.CommandOutput, command_runner.CommandError) {
  case behavior {
    DriverPublishes ->
      case driver_args_have_required_shape(args) {
        True -> Ok(command_runner.CommandOutput(0, driver_success_json(), ""))
        False -> Error(command_runner.command_error("bad_driver_args"))
      }
    DriverUnchangedNoUrl ->
      case driver_args_have_required_shape(args) {
        True -> Ok(command_runner.CommandOutput(0, driver_unchanged_json(), ""))
        False -> Error(command_runner.command_error("bad_driver_args"))
      }
    DriverStaleExistingBranch ->
      case driver_args_have_required_shape(args) {
        True ->
          Ok(command_runner.CommandOutput(
            0,
            driver_stale_existing_branch_json(),
            "",
          ))
        False -> Error(command_runner.command_error("bad_driver_args"))
      }
    DriverHeadMismatch ->
      case driver_args_have_required_shape(args) {
        True ->
          Ok(command_runner.CommandOutput(
            0,
            driver_success_json_with_head(driver_mismatched_head_revision()),
            "",
          ))
        False -> Error(command_runner.command_error("bad_driver_args"))
      }
    DriverMalformed ->
      Ok(command_runner.CommandOutput(
        0,
        "{\"version\":1,\"status\":\"published\"}",
        "",
      ))
    DriverUnsuccessful ->
      Ok(command_runner.CommandOutput(
        0,
        "{\"version\":1,\"status\":\"rejected\",\"failure_code\":\"remote_rejected\",\"message\":\"branch lease rejected\"}",
        "",
      ))
    DriverLeaksSecretFailure(secret) ->
      Ok(command_runner.CommandOutput(
        1,
        "{\"failure_code\":\"workspace_driver_publish_failed\",\"message\":\"driver leaked "
          <> secret
          <> "\"}",
        "diagnostics leaked " <> secret,
      ))
  }
}

fn driver_args_have_required_shape(args: List(String)) -> Bool {
  string.contains(
    string.join(args, with: " "),
    "--title-file tmp/scherzo-publication/",
  )
  && string.contains(
    string.join(args, with: " "),
    "--body-file tmp/scherzo-publication/",
  )
  && string.contains(
    string.join(args, with: " "),
    "--branch-prefix " <> existing_branch(),
  )
  && string.contains(string.join(args, with: " "), "--base main")
  && string.contains(
    string.join(args, with: " "),
    "--target-branch " <> existing_branch(),
  )
  && string.contains(string.join(args, with: " "), "--target-pr 42")
  && string.contains(string.join(args, with: " "), "--allow-no-changes true")
  && string.ends_with(string.join(args, with: " "), "--json")
}

fn driver_success_json() -> String {
  driver_success_json_with_head(driver_head_revision())
}

fn driver_success_json_with_head(head_revision: String) -> String {
  json.object([
    #("version", json.int(1)),
    #("status", json.string("updated")),
    #("url", json.string(driver_pr_url())),
    #("branch", json.string(existing_branch())),
    #("base_ref", json.string("main")),
    #("base_revision", json.string(driver_base_revision())),
    #("head_revision", json.string(head_revision)),
    #("change_id", json.string(driver_change_id())),
    #("created", json.bool(False)),
    #("updated", json.bool(True)),
  ])
  |> json.to_string
}

fn driver_unchanged_json() -> String {
  json.object([
    #("version", json.int(1)),
    #("status", json.string("unchanged")),
    #("branch", json.string(existing_branch())),
    #("base_ref", json.string("main")),
    #("base_revision", json.string(driver_base_revision())),
    #("head_revision", json.string(driver_head_revision())),
    #("change_id", json.string(driver_change_id())),
    #("created", json.bool(False)),
    #("updated", json.bool(False)),
  ])
  |> json.to_string
}

fn driver_stale_existing_branch_json() -> String {
  json.object([
    #("version", json.int(1)),
    #("status", json.string("rejected")),
    #("failure_code", json.string("stale_existing_branch")),
    #("message", json.string("target branch moved before publication")),
  ])
  |> json.to_string
}

fn commit_failure_runner() -> command_runner.Runner {
  command_runner.Runner(run: fn(spec) {
    let command_runner.CommandSpec(
      executable: executable,
      args: args,
      cwd: cwd,
      ..,
    ) = spec
    let _ = simplifile.create_directory_all(cwd)
    case executable, args {
      "git", ["clone", _, target] -> {
        let _ = simplifile.create_directory_all(target)
        Ok(command_runner.CommandOutput(0, "", ""))
      }
      "git", ["fetch", ..] -> Ok(command_runner.CommandOutput(0, "", ""))
      "git", ["ls-remote", ..] -> Ok(command_runner.CommandOutput(2, "", ""))
      "git", ["rev-parse", "--verify", ..] ->
        Ok(command_runner.CommandOutput(1, "", ""))
      "git", ["checkout", ..] -> Ok(command_runner.CommandOutput(0, "", ""))
      "git", ["status", ..] -> Ok(command_runner.CommandOutput(0, "", ""))
      "git", ["add", ..] -> Ok(command_runner.CommandOutput(0, "", ""))
      "git", ["diff", ..] -> Ok(command_runner.CommandOutput(1, "", ""))
      "git", ["commit", ..] ->
        Ok(command_runner.CommandOutput(2, "", "commit failed"))
      _, _ -> Error(command_runner.command_error("unexpected_command"))
    }
  })
}

fn fail_if_called_runner() -> command_runner.Runner {
  command_runner.Runner(run: fn(_) {
    Error(command_runner.command_error("runner should not be called"))
  })
}

fn disappearing_checkpoint(root: String) -> workflow_checkpoint.Writer {
  let base = workflow_checkpoint.ledger_writer(root, fn() { 123 })
  workflow_checkpoint.Writer(
    ..base,
    read_artifact: fn(ref) {
      case ref == plan_ref() {
        True -> Error(workflow_checkpoint.CheckpointArtifactFailed("missing"))
        False -> base.read_artifact(ref)
      }
    },
    artifact_location: fn(ref) {
      case ref == plan_ref() {
        True -> Error(workflow_checkpoint.CheckpointArtifactFailed("missing"))
        False -> base.artifact_location(ref)
      }
    },
  )
}

fn load_projection(root: String) -> projection.Projection {
  let assert Ok(ledger_path) = ledger.path_for_workspace_root(root)
  let assert Ok(projected) = ledger.load_projection(ledger_path)
  projected
}

fn read_publication_manifest(
  root: String,
  ref: String,
) -> artifact_publication_manifest.PublicationManifest {
  let assert Ok(contents) =
    simplifile.read(root <> "/.scherzo-state/artifacts/" <> ref)
  let assert Ok(manifest) =
    artifact_publication_manifest.decode_manifest_json(contents)
  manifest
}

fn read_file(path: String) -> String {
  let assert Ok(contents) = simplifile.read(path)
  contents
}

fn write_template(root: String) -> Nil {
  let template = root <> "/templates/publication.md"
  let assert Ok(Nil) = simplifile.create_directory_all(root <> "/templates")
  let assert Ok(Nil) =
    simplifile.write(
      template,
      "Version {{ publication.version_id }}\n{{ publication.files_markdown }}",
    )
  Nil
}

fn write_artifact(root: String, ref: String, contents: String) -> Nil {
  let absolute = root <> "/.scherzo-state/artifacts/" <> ref
  let assert Ok(dir) = path.dirname(absolute)
  let assert Ok(Nil) = simplifile.create_directory_all(dir)
  let assert Ok(Nil) = simplifile.write(absolute, contents)
  Nil
}

fn issue() -> tracker_issue.Issue {
  tracker_issue.Issue(
    id: "task-1",
    identifier: "LIV-761",
    title: "Publication executor",
    description: None,
    priority: None,
    state: issue_state.from_string_unchecked("Todo"),
    branch_name: None,
    url: None,
    labels: [],
    blocked_by: [],
    blocked_by_complete: True,
    created_at: None,
    updated_at: None,
  )
}

fn repositories() -> artifact_publication_config.ArtifactRepositories {
  artifact_publication_config.ArtifactRepositories(
    github: dict.from_list([
      #(
        "docs",
        artifact_publication_config.GithubRepositoryTarget(
          name: "docs",
          repo: "scherzo-systems/scherzo",
          base: "main",
          checkout: artifact_publication_config.GithubCheckoutConfig(
            strategy: artifact_publication_config.ManagedGit,
          ),
          branch: artifact_publication_config.GithubBranchConfig(
            strategy: artifact_publication_config.StablePerWork,
            template: "scherzo/{{ workflow.id }}/{{ work.identifier }}/{{ publication.id }}",
          ),
          pull_request: artifact_publication_config.GithubPullRequestConfig(
            enabled: True,
            strategy: artifact_publication_config.UpdateExisting,
            draft: True,
            title: Some("{{ work.identifier }} publication"),
            body_template: Some("templates/publication.md"),
          ),
        ),
      ),
    ]),
  )
}

fn route(required: Bool) -> artifact_publication_config.PublicationRoute {
  artifact_publication_config.PublicationRoute(
    id: "execplan_review_doc",
    repository: "github.docs",
    required: required,
    pull_request: Some(
      artifact_publication_config.PublicationPullRequestOverride(
        title: Some("{{ work.identifier }} publication"),
        body_template: Some("templates/publication.md"),
      ),
    ),
    mode: artifact_publication_config.FilePublication,
    commit_stack: None,
    target: artifact_publication_config.StableBranchTarget,
    files: [
      artifact_publication_config.PublicationFileRoute(
        selector: artifact_publication_config.PublicationFileSelector(
          output: "review_doc",
          entry: None,
        ),
        path: "docs/plans/{{ work.identifier }}{{ artifact.default_extension }}",
      ),
    ],
  )
}

fn commit_stack_route(
  required: Bool,
) -> artifact_publication_config.PublicationRoute {
  artifact_publication_config.PublicationRoute(
    id: "conflict_resolution",
    repository: "github.docs",
    required: required,
    pull_request: None,
    mode: artifact_publication_config.CommitStackPublication,
    files: [],
    commit_stack: Some(
      artifact_publication_config.PublicationCommitStackRoute(
        selector: artifact_publication_config.PublicationCommitStackSelector(
          output: "commit_stack",
        ),
      ),
    ),
    target: artifact_publication_config.ExistingPrBranchTarget(
      artifact_publication_config.PublicationTargetSource(
        output: "merge_conflict_target",
      ),
    ),
  )
}

fn output_manifest() -> workflow_contract_manifest.ContractOutputManifest {
  workflow_contract_manifest.ContractOutputManifest(
    run_id: "run-1",
    workflow_id: "workflow.execplan",
    workflow_fingerprint: "wf-1",
    outputs: [
      workflow_contract_manifest.NamedManifestValue(
        name: "review_doc",
        value: workflow_contract_manifest.present_run_artifact(
          workflow_contract.DocumentMarkdown,
          workflow_contract_manifest.ArtifactWritten(
            ref: plan_ref(),
            sha256: plan_sha(),
            bytes: plan_bytes(),
          ),
          "text/markdown",
          None,
        ),
      ),
    ],
    diagnostics: [],
  )
}

fn commit_stack_output_manifest() -> workflow_contract_manifest.ContractOutputManifest {
  let stack_contents = commit_stack_manifest_contents()
  let target_contents = existing_target_contents()
  workflow_contract_manifest.ContractOutputManifest(
    run_id: "run-1",
    workflow_id: "workflow.implementation",
    workflow_fingerprint: "wf-1",
    outputs: [
      workflow_contract_manifest.NamedManifestValue(
        name: "commit_stack",
        value: workflow_contract_manifest.present_run_artifact(
          workflow_contract.CommitStack,
          workflow_contract_manifest.ArtifactWritten(
            ref: commit_stack_ref(),
            sha256: hash.sha256_hex(stack_contents),
            bytes: bytes_of(stack_contents),
          ),
          commit_stack_artifact.commit_stack_media_type,
          None,
        ),
      ),
      workflow_contract_manifest.NamedManifestValue(
        name: "merge_conflict_target",
        value: workflow_contract_manifest.present_run_artifact(
          workflow_contract.CodeChange,
          workflow_contract_manifest.ArtifactWritten(
            ref: existing_target_ref(),
            sha256: hash.sha256_hex(target_contents),
            bytes: bytes_of(target_contents),
          ),
          "application/json",
          None,
        ),
      ),
    ],
    diagnostics: [],
  )
}

fn plan_ref() -> String {
  "runs/run-1/outputs/review_doc.md"
}

fn plan_contents() -> String {
  "# Review\n"
}

fn plan_sha() -> String {
  hash.sha256_hex(plan_contents())
}

fn plan_bytes() -> Int {
  bytes_of(plan_contents())
}

fn write_commit_stack_artifacts(root: String) -> Nil {
  write_artifact(root, commit_stack_ref(), commit_stack_manifest_contents())
  write_artifact(root, existing_target_ref(), existing_target_contents())
  write_artifact(root, commit_stack_carrier_ref(), commit_stack_carrier())
}

fn commit_stack_manifest_contents() -> String {
  json.object([
    #("schema_version", json.int(1)),
    #(
      "artifact_type",
      json.string(commit_stack_artifact.commit_stack_artifact_type),
    ),
    #(
      "repository",
      json.object([#("repo", json.string("scherzo-systems/scherzo"))]),
    ),
    #(
      "base",
      json.object([
        #("ref", json.string(existing_branch())),
        #("sha", json.string(expected_existing_head_sha())),
      ]),
    ),
    #(
      "head",
      json.object([
        #("sha", json.string(commit_stack_head_sha())),
        #("tree", json.string(commit_stack_head_tree())),
      ]),
    ),
    #(
      "carrier",
      json.object([
        #("ref", json.string(commit_stack_carrier_ref())),
        #("sha256", json.string(hash.sha256_hex(commit_stack_carrier()))),
        #("bytes", json.int(bytes_of(commit_stack_carrier()))),
        #("media_type", json.string(commit_stack_artifact.bundle_media_type)),
      ]),
    ),
  ])
  |> json.to_string
}

fn existing_target_contents() -> String {
  json.object([
    #("schema_version", json.int(1)),
    #(
      "artifact_type",
      json.string("scherzo.github_existing_pr_branch_target.v1"),
    ),
    #(
      "repository",
      json.object([#("repo", json.string("scherzo-systems/scherzo"))]),
    ),
    #(
      "head",
      json.object([
        #("repo", json.string("scherzo-systems/scherzo")),
        #("branch", json.string(existing_branch())),
        #("sha", json.string(expected_existing_head_sha())),
      ]),
    ),
    #(
      "base",
      json.object([
        #("branch", json.string("main")),
        #("sha", json.string(expected_base_sha())),
      ]),
    ),
    #(
      "pull_request",
      json.object([
        #("number", json.int(42)),
        #("url", json.string(existing_pr_url())),
      ]),
    ),
  ])
  |> json.to_string
}

fn bytes_of(contents: String) -> Int {
  bit_array.byte_size(bit_array.from_string(contents))
}

fn commit_stack_ref() -> String {
  "runs/run-1/outputs/commit_stack.json"
}

fn existing_target_ref() -> String {
  "runs/run-1/outputs/merge_conflict_target.json"
}

fn commit_stack_carrier_ref() -> String {
  "runs/run-1/outputs/commit_stack.bundle"
}

fn existing_branch() -> String {
  "feature/conflict-resolution"
}

fn existing_pr_url() -> String {
  "https://example.test/pr/42"
}

fn driver_pr_url() -> String {
  "https://example.test/pr/driver"
}

fn driver_base_revision() -> String {
  "main@origin"
}

fn driver_head_revision() -> String {
  "3333333333333333333333333333333333333333"
}

fn driver_mismatched_head_revision() -> String {
  "4444444444444444444444444444444444444444"
}

fn driver_change_id() -> String {
  "change-123"
}

fn expected_existing_head_sha() -> String {
  "1111111111111111111111111111111111111111"
}

fn expected_base_sha() -> String {
  "2222222222222222222222222222222222222222"
}

fn commit_stack_head_sha() -> String {
  "3333333333333333333333333333333333333333"
}

fn commit_stack_head_tree() -> String {
  "4444444444444444444444444444444444444444"
}

fn commit_stack_carrier() -> String {
  "bundle bytes"
}
