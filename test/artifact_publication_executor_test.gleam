import gleam/bit_array
import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import scherzo/artifact_publication_config
import scherzo/artifact_publication_executor
import scherzo/artifact_repository/command_runner
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
    projection.latest_publication_for_run(projected, "run-1", "review_doc")
  assert latest.status == "published"
  assert latest.retry_execution_available == True
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
    projection.publication_attempts_for_run(projected, "run-1", "review_doc")
  assert list.length(attempts) == 1
}

fn fake_runner() -> command_runner.Runner {
  command_runner.Runner(run: fn(spec) {
    let command_runner.CommandSpec(executable, args, cwd, _, _) = spec
    let _ = simplifile.create_directory_all(cwd)
    case executable, args {
      "git", ["clone", _, target] -> {
        let _ = simplifile.create_directory_all(target)
        Ok(command_runner.CommandOutput(0, "", ""))
      }
      "git", ["fetch", ..] -> Ok(command_runner.CommandOutput(0, "", ""))
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
  })
}

fn commit_failure_runner() -> command_runner.Runner {
  command_runner.Runner(run: fn(spec) {
    let command_runner.CommandSpec(executable, args, cwd, _, _) = spec
    let _ = simplifile.create_directory_all(cwd)
    case executable, args {
      "git", ["clone", _, target] -> {
        let _ = simplifile.create_directory_all(target)
        Ok(command_runner.CommandOutput(0, "", ""))
      }
      "git", ["fetch", ..] -> Ok(command_runner.CommandOutput(0, "", ""))
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
  workflow_checkpoint.Writer(..base, read_artifact: fn(ref) {
    case ref == plan_ref() {
      True -> Error(workflow_checkpoint.CheckpointArtifactFailed("missing"))
      False -> base.read_artifact(ref)
    }
  })
}

fn load_projection(root: String) -> projection.Projection {
  let assert Ok(ledger_path) = ledger.path_for_workspace_root(root)
  let assert Ok(projected) = ledger.load_projection(ledger_path)
  projected
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
    id: "review_doc",
    repository: "github.docs",
    required: required,
    pull_request: Some(
      artifact_publication_config.PublicationPullRequestOverride(
        title: Some("{{ work.identifier }} publication"),
        body_template: Some("templates/publication.md"),
      ),
    ),
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
  bit_array.byte_size(bit_array.from_string(plan_contents()))
}
