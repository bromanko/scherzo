import gleam/bit_array
import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import scherzo/artifact_publication_config
import scherzo/artifact_publication_recording
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

pub fn record_routes_writes_manifest_and_is_idempotent_test() {
  let root = "test/tmp/artifact-publication-recording/idempotent"
  test_helpers.reset_dir(root)
  write_template(root)
  write_artifact(root, plan_ref(), plan_contents())
  let checkpoint = workflow_checkpoint.ledger_writer(root, fn() { 123 })

  let assert Ok(first) =
    artifact_publication_recording.record_routes(
      [route(True)],
      repositories(),
      root,
      root,
      output_manifest(),
      issue(),
      "run-1",
      checkpoint,
    )
  let assert Ok(second) =
    artifact_publication_recording.record_routes(
      [route(True)],
      repositories(),
      root,
      root,
      output_manifest(),
      issue(),
      "run-1",
      checkpoint,
    )

  assert list.length(first.attempts) == 1
  assert list.length(second.attempts) == 1
  let assert [attempt] = first.attempts
  assert attempt.status == "planned"
  assert attempt.retryable == False
  assert attempt.retry_execution_available == False
  assert attempt.manifest_ref
    == "runs/run-1/publications/execplan_review_doc/"
    <> attempt.attempt_id
    <> ".json"
  let assert Ok(manifest) =
    simplifile.read(
      root <> "/.scherzo-state/artifacts/" <> attempt.manifest_ref,
    )
  assert string.contains(
    manifest,
    "\"artifact_type\":\"scherzo.artifact_publication_manifest.v1\"",
  )
  assert publication_attempt_records(root) == 1
}

pub fn record_routes_records_optional_failures_in_ledger_projection_test() {
  let root = "test/tmp/artifact-publication-recording/optional-failure"
  test_helpers.reset_dir(root)
  write_template(root)
  write_artifact(root, plan_ref(), plan_contents())
  let checkpoint = workflow_checkpoint.ledger_writer(root, fn() { 123 })

  let assert Ok(result) =
    artifact_publication_recording.record_routes(
      [missing_output_route(False)],
      repositories(),
      root,
      root,
      output_manifest(),
      issue(),
      "run-1",
      checkpoint,
    )

  assert result.required_failures == []
  assert list.length(result.optional_failures) == 1
  let assert [failure] = result.optional_failures
  assert failure.publication_id == "missing_review_doc"
  assert failure.required == False
  let assert [attempt] = result.attempts
  assert attempt.status == "failed"
  assert attempt.retryable == True
  assert attempt.retry_execution_available == False

  let projected = load_projection(root)
  let attempts =
    projection.publication_attempts_for_run(
      projected,
      "run-1",
      "missing_review_doc",
    )
  assert list.length(attempts) == 1
  let assert Ok(latest) =
    projection.latest_publication_for_run(
      projected,
      "run-1",
      "missing_review_doc",
    )
  assert latest.status == "failed"
  assert latest.error_code == Some("unknown_output")
  assert latest.retry_execution_available == False
}

pub fn record_routes_records_missing_body_template_as_failed_attempt_test() {
  let root = "test/tmp/artifact-publication-recording/missing-template"
  test_helpers.reset_dir(root)
  write_artifact(root, plan_ref(), plan_contents())
  let checkpoint = workflow_checkpoint.ledger_writer(root, fn() { 123 })

  let assert Ok(result) =
    artifact_publication_recording.record_routes(
      [missing_template_route(False)],
      repositories(),
      root,
      root,
      output_manifest(),
      issue(),
      "run-1",
      checkpoint,
    )

  assert result.required_failures == []
  let assert [failure] = result.optional_failures
  assert failure.code == "publication_body_template_read_failed"
  assert failure.required == False
  let assert [attempt] = result.attempts
  assert attempt.status == "failed"
  assert attempt.error_code == Some("publication_body_template_read_failed")
  assert publication_attempt_records(root) == 1
}

pub fn record_routes_records_hash_mismatch_as_failed_attempt_test() {
  let root = "test/tmp/artifact-publication-recording/hash-mismatch"
  test_helpers.reset_dir(root)
  write_template(root)
  write_artifact(root, plan_ref(), "# Different\n")
  let checkpoint = workflow_checkpoint.ledger_writer(root, fn() { 123 })

  let assert Ok(result) =
    artifact_publication_recording.record_routes(
      [route(True)],
      repositories(),
      root,
      root,
      output_manifest(),
      issue(),
      "run-1",
      checkpoint,
    )

  let assert [failure] = result.required_failures
  assert failure.code == "hash_mismatch"
  let assert [attempt] = result.attempts
  assert attempt.status == "failed"
  assert attempt.error_code == Some("hash_mismatch")
  assert publication_attempt_records(root) == 1
}

pub fn record_routes_keeps_config_dir_and_checkpoint_state_root_separate_test() {
  let root = "test/tmp/artifact-publication-recording/separate-roots"
  let config_dir = root <> "/config"
  let state_root = root <> "/state"
  test_helpers.reset_dir(root)
  write_template(config_dir)
  write_artifact(state_root, plan_ref(), plan_contents())
  let checkpoint = workflow_checkpoint.ledger_writer(state_root, fn() { 123 })

  let assert Ok(result) =
    artifact_publication_recording.record_routes(
      [route(True)],
      repositories(),
      config_dir,
      config_dir,
      output_manifest(),
      issue(),
      "run-1",
      checkpoint,
    )

  assert result.required_failures == []
  let assert [attempt] = result.attempts
  assert attempt.status == "planned"
  assert publication_attempt_records(state_root) == 1
  let assert Ok(_) =
    simplifile.read(
      state_root <> "/.scherzo-state/artifacts/" <> attempt.manifest_ref,
    )
}

pub fn load_body_templates_uses_workflow_dir_for_route_overrides_test() {
  let root = "test/tmp/artifact-publication-recording/workflow-template-root"
  let config_dir = root <> "/config"
  let workflow_bundle_dir = root <> "/bundles/execplan"
  test_helpers.reset_dir(root)
  write_template_contents(config_dir, "Config template")
  write_template_contents(workflow_bundle_dir, "Workflow template")

  let assert Ok(templates) =
    artifact_publication_recording.load_body_templates(
      [route(True)],
      repositories(),
      config_dir,
      workflow_bundle_dir,
    )
  let assert Ok(contents) = dict.get(templates, "templates/publication.md")
  assert contents == "Workflow template"
}

pub fn load_body_templates_keeps_repository_defaults_config_relative_test() {
  let root = "test/tmp/artifact-publication-recording/config-template-root"
  let config_dir = root <> "/config"
  let workflow_bundle_dir = root <> "/bundles/execplan"
  test_helpers.reset_dir(root)
  write_template_contents(config_dir, "Config template")
  write_template_contents(workflow_bundle_dir, "Workflow template")

  let assert Ok(templates) =
    artifact_publication_recording.load_body_templates(
      [repository_default_route(True)],
      repositories(),
      config_dir,
      workflow_bundle_dir,
    )
  let assert Ok(contents) = dict.get(templates, "templates/publication.md")
  assert contents == "Config template"
}

pub fn load_body_templates_rejects_conflicting_mixed_roots_test() {
  let root = "test/tmp/artifact-publication-recording/mixed-template-load"
  let config_dir = root <> "/config"
  let workflow_bundle_dir = root <> "/bundles/execplan"
  test_helpers.reset_dir(root)
  write_template_contents(config_dir, "Config template")
  write_template_contents(workflow_bundle_dir, "Workflow template")

  let assert Error(message) =
    artifact_publication_recording.load_body_templates(
      [route(True), repository_default_route(True)],
      repositories(),
      config_dir,
      workflow_bundle_dir,
    )
  assert message
    == "conflicting_publication_body_template_roots:templates/publication.md"
}

pub fn record_routes_distinguishes_mixed_template_roots_test() {
  let root = "test/tmp/artifact-publication-recording/mixed-template-record"
  let config_dir = root <> "/config"
  let workflow_bundle_dir = root <> "/bundles/execplan"
  test_helpers.reset_dir(root)
  write_template_contents(config_dir, "Config body")
  write_template_contents(workflow_bundle_dir, "Workflow body")
  write_artifact(root, plan_ref(), plan_contents())
  let checkpoint = workflow_checkpoint.ledger_writer(root, fn() { 123 })

  let assert Ok(result) =
    artifact_publication_recording.record_routes(
      [
        route_with_id("workflow_doc", True),
        repository_default_route_with_id("config_doc", True),
      ],
      repositories(),
      config_dir,
      workflow_bundle_dir,
      output_manifest(),
      issue(),
      "run-1",
      checkpoint,
    )

  assert result.required_failures == []
  assert result.optional_failures == []
  let assert [workflow_attempt, config_attempt] = result.attempts
  assert_manifest_contains(root, workflow_attempt.manifest_ref, "Workflow body")
  assert_manifest_contains(root, config_attempt.manifest_ref, "Config body")
}

fn publication_attempt_records(root: String) -> Int {
  let assert Ok(contents) =
    simplifile.read(root <> "/.scherzo-state/ledger/current.jsonl")
  contents
  |> string.split(on: "\n")
  |> list.filter(fn(line) {
    string.contains(line, "\"kind\":\"publication_attempt_recorded\"")
  })
  |> list.length
}

fn load_projection(root: String) -> projection.Projection {
  let assert Ok(ledger_path) = ledger.path_for_workspace_root(root)
  let assert Ok(projected) = ledger.load_projection(ledger_path)
  projected
}

fn assert_manifest_contains(
  root: String,
  manifest_ref: String,
  expected: String,
) -> Nil {
  let assert Ok(manifest) =
    simplifile.read(root <> "/.scherzo-state/artifacts/" <> manifest_ref)
  assert string.contains(manifest, expected)
}

fn write_template(root: String) -> Nil {
  write_template_contents(
    root,
    "Version {{ publication.version_id }}\n{{ publication.files_markdown }}",
  )
}

fn write_template_contents(root: String, contents: String) -> Nil {
  let template = root <> "/templates/publication.md"
  let assert Ok(Nil) = simplifile.create_directory_all(root <> "/templates")
  let assert Ok(Nil) = simplifile.write(template, contents)
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
    title: "Publication recording",
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
  route_with_id("execplan_review_doc", required)
}

fn route_with_id(
  id: String,
  required: Bool,
) -> artifact_publication_config.PublicationRoute {
  artifact_publication_config.PublicationRoute(
    id: id,
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

fn repository_default_route(
  required: Bool,
) -> artifact_publication_config.PublicationRoute {
  repository_default_route_with_id("execplan_review_doc", required)
}

fn repository_default_route_with_id(
  id: String,
  required: Bool,
) -> artifact_publication_config.PublicationRoute {
  artifact_publication_config.PublicationRoute(
    ..route_with_id(id, required),
    pull_request: None,
  )
}

fn missing_output_route(
  required: Bool,
) -> artifact_publication_config.PublicationRoute {
  artifact_publication_config.PublicationRoute(
    ..route(required),
    id: "missing_review_doc",
    mode: artifact_publication_config.FilePublication,
    commit_stack: None,
    target: artifact_publication_config.StableBranchTarget,
    files: [
      artifact_publication_config.PublicationFileRoute(
        selector: artifact_publication_config.PublicationFileSelector(
          output: "missing",
          entry: None,
        ),
        path: "docs/plans/{{ work.identifier }}{{ artifact.default_extension }}",
      ),
    ],
  )
}

fn missing_template_route(
  required: Bool,
) -> artifact_publication_config.PublicationRoute {
  artifact_publication_config.PublicationRoute(
    ..route(required),
    pull_request: Some(
      artifact_publication_config.PublicationPullRequestOverride(
        title: Some("{{ work.identifier }} publication"),
        body_template: Some("templates/missing.md"),
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
