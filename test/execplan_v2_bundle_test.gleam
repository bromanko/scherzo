import gleam/bit_array
import gleam/int
import gleam/option.{Some}
import gleam/string
import scherzo/command_step
import scherzo/config/types as config_types
import scherzo/hash
import scherzo/path as scherzo_path
import scherzo/step_artifact
import simplifile
import workflow_context_test_support

fn limits() -> config_types.ArtifactLimits {
  config_types.ArtifactLimits(
    command_stream_max_chars: 12_000,
    template_field_max_chars: 12_000,
    workflow_summary_max_chars: 12_000,
  )
}

fn run_helper(command: String) -> step_artifact.StepArtifact {
  command_step.run(
    "execplan_v2_helper",
    workflow_context_test_support.without_workflow_context(
      ".scherzo/workflows/scripts/scherzo-execplan " <> command,
    ),
    ".",
    30_000,
    [],
    limits(),
  )
}

fn run_shell(command: String) -> step_artifact.StepArtifact {
  command_step.run(
    "execplan_v2_helper",
    workflow_context_test_support.without_workflow_context(command),
    ".",
    30_000,
    [],
    limits(),
  )
}

fn run_shell_in(cwd: String, command: String) -> step_artifact.StepArtifact {
  command_step.run(
    "execplan_v2_helper",
    workflow_context_test_support.without_workflow_context(command),
    cwd,
    30_000,
    [],
    limits(),
  )
}

fn reset_dir(path: String) -> Nil {
  let _ = simplifile.delete(path)
  let assert Ok(Nil) = simplifile.create_directory_all(path)
  Nil
}

fn assert_completion_preflight_failed(
  artifact: step_artifact.StepArtifact,
  diagnostic: String,
) -> Nil {
  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(2)
  assert string.contains(
    artifact.stderr,
    "SCHERZO_FAILURE_CODE=execplan_completion_preflight_failed",
  )
  assert string.contains(artifact.stderr, diagnostic)
  Nil
}

fn shell_quote(value: String) -> String {
  "'" <> string.replace(value, each: "'", with: "'\\''") <> "'"
}

fn tmp_repo_path(path: String) -> String {
  "../../../" <> path
}

fn write_valid_review_doc(path: String) -> Nil {
  let assert Ok(valid) =
    simplifile.read("test/fixtures/execplan_v2/review-doc.valid.md")
  let assert Ok(Nil) = simplifile.write(path, valid)
  Nil
}

fn mutated_bundle(dir: String, each old: String, with new: String) -> String {
  reset_dir(dir)
  let assert Ok(source) =
    simplifile.read("test/fixtures/execplan_v2/exec-plan-bundle.valid.json")
  let path = dir <> "/bundle.json"
  let assert Ok(Nil) =
    simplifile.write(path, string.replace(source, each: old, with: new))
  path
}

fn mutated_pack(dir: String, each old: String, with new: String) -> String {
  reset_dir(dir)
  let assert Ok(source) =
    simplifile.read("test/fixtures/execplan_v2/implementation-pack.valid.json")
  let path = dir <> "/pack.json"
  let assert Ok(Nil) =
    simplifile.write(path, string.replace(source, each: old, with: new))
  path
}

fn pack_submission(title: String) -> String {
  "{\n"
  <> "  \"artifact_name\": \"implementation_pack_submission\",\n"
  <> "  \"payload\": {\n"
  <> "    \"schema_version\": 2,\n"
  <> "    \"artifact_type\": \"implementation_pack_submission\",\n"
  <> "    \"source_issue\": {\n"
  <> "      \"identifier\": \"LIV-314\",\n"
  <> "      \"title\": \""
  <> title
  <> "\",\n"
  <> "      \"url\": \"https://linear.app/living-systems/issue/LIV-314/fixture-v2-execplan-bundle\"\n"
  <> "    },\n"
  <> "    \"sections\": {\n"
  <> "      \"repo_context\": \"Fixture repo context.\",\n"
  <> "      \"verified_facts\": [{\"fact\": \"Fact\", \"evidence\": \"Evidence\"}],\n"
  <> "      \"concrete_steps\": [{\"title\": \"Validate the fixture review document\", \"instructions\": \"Validate the review document, validate the pack, and prove the bundle links by hash.\", \"files\": [\"test/fixtures/execplan_v2/review-doc.valid.md\"], \"commands\": [\"scripts/scherzo-execplan validate-review-doc --path test/fixtures/execplan_v2/review-doc.valid.md\"], \"expected_result\": \"The command exits zero and the bundle/pack hash linkage remains valid.\"}],\n"
  <> "      \"testing_and_falsifiability\": \"Run validate-review-doc and validate-bundle; any review document, pack, bundle, or hash mismatch falsifies the fixture.\",\n"
  <> "      \"interfaces_and_dependencies\": \"Use .scherzo/workflows/scripts/scherzo-execplan.\",\n"
  <> "      \"artifacts_and_notes\": \"No extra artifacts.\"\n"
  <> "    },\n"
  <> "    \"conflict_policy\": \"Stop on review doc and pack conflicts.\"\n"
  <> "  }\n"
  <> "}\n"
}

fn review_doc_with_validation(validation: String) -> String {
  let assert Ok(valid) =
    simplifile.read("test/fixtures/execplan_v2/review-doc.valid.md")
  string.replace(
    valid,
    each: "## Validation and Acceptance\n\nRun `scripts/scherzo-execplan validate-review-doc --path test/fixtures/execplan_v2/review-doc.valid.md` and expect a zero exit code.\n\n## Rollout, Recovery, and Idempotence",
    with: "## Validation and Acceptance\n\n"
      <> validation
      <> "\n\n## Rollout, Recovery, and Idempotence",
  )
}

fn pack_submission_with_commands_and_testing(
  title: String,
  commands_json: String,
  testing: String,
) -> String {
  let with_commands =
    string.replace(
      pack_submission(title),
      each: "\"commands\": [\"scripts/scherzo-execplan validate-review-doc --path test/fixtures/execplan_v2/review-doc.valid.md\"]",
      with: "\"commands\": " <> commands_json,
    )
  string.replace(
    with_commands,
    each: "\"testing_and_falsifiability\": \"Run validate-review-doc and validate-bundle; any review document, pack, bundle, or hash mismatch falsifies the fixture.\"",
    with: "\"testing_and_falsifiability\": \"" <> testing <> "\"",
  )
}

fn write_revision_bundle_with_surface(
  dir: String,
  review_path: String,
  branch: String,
  head_revision: String,
) -> #(String, String) {
  let bundle_ref = "runs/run-prepare/outputs/exec_plan_bundle.json"
  let bundle_dir =
    dir <> "/repo/.scherzo-state/artifacts/runs/run-prepare/outputs"
  let assert Ok(Nil) = simplifile.create_directory_all(bundle_dir)
  let assert Ok(source) =
    simplifile.read("test/fixtures/execplan_v2/exec-plan-bundle.valid.json")
  let with_path =
    string.replace(
      source,
      each: "test/fixtures/execplan_v2/review-doc.valid.md",
      with: review_path,
    )
  let with_branch =
    string.replace(
      with_path,
      each: "    \"branch\": \"execplan/liv-314\",\n",
      with: "    \"branch\": \""
        <> branch
        <> "\",\n    \"head_revision\": \""
        <> head_revision
        <> "\",\n",
    )
  let bundle_path = bundle_dir <> "/exec_plan_bundle.json"
  let assert Ok(Nil) = simplifile.write(bundle_path, with_branch)
  #(bundle_ref, hash.sha256_hex(with_branch))
}

fn write_revision_bundle(
  dir: String,
  review_path: String,
) -> #(String, String) {
  write_revision_bundle_with_surface(
    dir,
    review_path,
    "execplan/liv-314-unmerged",
    "ca667773c9a6d31bb64676c103b3f1f14c3bcced",
  )
}

fn text_bytes(contents: String) -> String {
  int.to_string(bit_array.byte_size(bit_array.from_string(contents)))
}

fn write_source_handoff_split_bundle(dir: String) -> #(String, String) {
  let run_id = "run-source-handoff-split"
  let output_dir =
    dir <> "/.scherzo-state/artifacts/runs/" <> run_id <> "/outputs"
  let review_dir = dir <> "/test/fixtures/execplan_v2"
  let assert Ok(Nil) = simplifile.create_directory_all(output_dir)
  let assert Ok(Nil) = simplifile.create_directory_all(review_dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/.scherzo")
  let assert Ok(workflows_target) = scherzo_path.absolute("workflows/dogfood")
  let assert Ok(Nil) =
    scherzo_path.symlink(workflows_target, dir <> "/.scherzo/workflows")
  let assert Ok(review_doc) =
    simplifile.read("test/fixtures/execplan_v2/review-doc.valid.md")
  let assert Ok(Nil) =
    simplifile.write(review_dir <> "/review-doc.valid.md", review_doc)

  let assert Ok(pack_source) =
    simplifile.read("test/fixtures/execplan_v2/implementation-pack.valid.json")
  let pack_with_source_key =
    string.replace(pack_source, each: "LIV-314", with: "LIV-418")
  let pack_with_run =
    string.replace(pack_with_source_key, each: "run-1", with: run_id)
  let pack_text =
    string.replace(
      pack_with_run,
      each: "Fixture v2 ExecPlan bundle",
      with: "Fixture source LIV-418",
    )
  let pack_path = output_dir <> "/implementation_pack.json"
  let assert Ok(Nil) = simplifile.write(pack_path, pack_text)
  let pack_sha = hash.sha256_hex(pack_text)
  let pack_bytes = text_bytes(pack_text)

  let assert Ok(bundle_source) =
    simplifile.read("test/fixtures/execplan_v2/exec-plan-bundle.valid.json")
  let bundle_with_source_key =
    string.replace(bundle_source, each: "LIV-314", with: "LIV-418")
  let bundle_with_source_slug =
    string.replace(bundle_with_source_key, each: "liv-314", with: "liv-418")
  let bundle_with_handoff_key =
    string.replace(bundle_with_source_slug, each: "LIV-315", with: "LIV-423")
  let bundle_with_run =
    string.replace(bundle_with_handoff_key, each: "run-1", with: run_id)
  let bundle_with_title =
    string.replace(
      bundle_with_run,
      each: "Fixture v2 ExecPlan bundle",
      with: "Fixture source LIV-418",
    )
  let bundle_with_pack_sha =
    string.replace(
      bundle_with_title,
      each: "dbcb84d078e47839e8da760b1208e6b5606bce45ab3228a24a92e0b5afd21545",
      with: pack_sha,
    )
  let bundle_text =
    string.replace(
      bundle_with_pack_sha,
      each: "\"bytes\": 2155,",
      with: "\"bytes\": " <> pack_bytes <> ",",
    )
  let bundle_path = output_dir <> "/exec_plan_bundle.json"
  let assert Ok(Nil) = simplifile.write(bundle_path, bundle_text)
  let bundle_ref = "runs/" <> run_id <> "/outputs/exec_plan_bundle.json"
  #(bundle_ref, hash.sha256_hex(bundle_text))
}

fn write_artifact_backed_plan_bundle(
  dir: String,
  plan_ref: String,
  plan_sha_override: String,
  include_plan_artifact: Bool,
) -> #(String, String, String) {
  let run_id = "run-artifact-plan"
  let output_dir =
    dir <> "/.scherzo-state/artifacts/runs/" <> run_id <> "/outputs"
  let assert Ok(Nil) = simplifile.create_directory_all(output_dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/.scherzo")
  let assert Ok(workflows_target) = scherzo_path.absolute("workflows/dogfood")
  let assert Ok(Nil) =
    scherzo_path.symlink(workflows_target, dir <> "/.scherzo/workflows")
  let assert Ok(plan_text) =
    simplifile.read("test/fixtures/execplan_v2/review-doc.valid.md")
  case include_plan_artifact {
    True -> {
      let assert Ok(Nil) = simplifile.write(output_dir <> "/plan.md", plan_text)
      Nil
    }
    False -> Nil
  }
  let plan_sha = case plan_sha_override == "" {
    True -> hash.sha256_hex(plan_text)
    False -> plan_sha_override
  }
  let plan_bytes = text_bytes(plan_text)

  let assert Ok(pack_source) =
    simplifile.read("test/fixtures/execplan_v2/implementation-pack.valid.json")
  let pack_text = string.replace(pack_source, each: "run-1", with: run_id)
  let pack_path = output_dir <> "/implementation_pack.json"
  let assert Ok(Nil) = simplifile.write(pack_path, pack_text)
  let pack_sha = hash.sha256_hex(pack_text)
  let pack_bytes = text_bytes(pack_text)

  let bundle_ref = "runs/" <> run_id <> "/outputs/exec_plan_bundle.json"
  let bundle_text =
    "{\n"
    <> "  \"artifact_type\": \"exec_plan_bundle\",\n"
    <> "  \"bundle_id\": \"fixture-bundle-artifact-plan\",\n"
    <> "  \"implementation_handoff\": {\n"
    <> "    \"bundle_ref\": \""
    <> bundle_ref
    <> "\",\n"
    <> "    \"issue_identifier\": \"LIV-315\",\n"
    <> "    \"issue_url\": \"https://linear.app/living-systems/issue/LIV-315/implement-fixture-v2-execplan-bundle\",\n"
    <> "    \"workflow_label\": \"workflow:execplan-implementation\"\n"
    <> "  },\n"
    <> "  \"implementation_pack\": {\n"
    <> "    \"bytes\": "
    <> pack_bytes
    <> ",\n"
    <> "    \"derived_from_review_doc_sha256\": \""
    <> plan_sha
    <> "\",\n"
    <> "    \"ref\": \"runs/"
    <> run_id
    <> "/outputs/implementation_pack.json\",\n"
    <> "    \"schema\": \".scherzo/workflows/schemas/implementation-pack.v2.schema.json\",\n"
    <> "    \"sha256\": \""
    <> pack_sha
    <> "\"\n"
    <> "  },\n"
    <> "  \"plan\": {\n"
    <> "    \"bytes\": "
    <> plan_bytes
    <> ",\n"
    <> "    \"media_type\": \"text/markdown\",\n"
    <> "    \"ref\": \""
    <> plan_ref
    <> "\",\n"
    <> "    \"sha256\": \""
    <> plan_sha
    <> "\"\n"
    <> "  },\n"
    <> "  \"review_surface\": {\n"
    <> "    \"branch\": \"execplan/liv-314\",\n"
    <> "    \"pr_url\": \"https://github.com/living-systems/scherzo/pull/314\",\n"
    <> "    \"source_bundle_ref\": null,\n"
    <> "    \"status\": \"published\"\n"
    <> "  },\n"
    <> "  \"revision\": {\"number\": 1, \"status\": \"created\", \"supersedes\": null},\n"
    <> "  \"schema_version\": 2,\n"
    <> "  \"source_issue\": {\n"
    <> "    \"identifier\": \"LIV-314\",\n"
    <> "    \"title\": \"Fixture v2 ExecPlan bundle\",\n"
    <> "    \"url\": \"https://linear.app/living-systems/issue/LIV-314/fixture-v2-execplan-bundle\"\n"
    <> "  },\n"
    <> "  \"validation\": [{\"name\": \"fixture\", \"status\": \"passed\"}],\n"
    <> "  \"workflow\": {\n"
    <> "    \"run_id\": \""
    <> run_id
    <> "\",\n"
    <> "    \"workflow_fingerprint\": \"fixture-fingerprint\",\n"
    <> "    \"workflow_id\": \"execplan\"\n"
    <> "  }\n"
    <> "}\n"
  let assert Ok(Nil) =
    simplifile.write(output_dir <> "/exec_plan_bundle.json", bundle_text)
  #(bundle_ref, hash.sha256_hex(bundle_text), plan_text)
}

pub fn validate_bundle_accepts_valid_fixture_test() {
  let artifact =
    run_helper(
      "validate-bundle --bundle test/fixtures/execplan_v2/exec-plan-bundle.valid.json --repo-root .",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(artifact.stdout, "BUNDLE_VALID=ok")
}

pub fn validate_bundle_accepts_artifact_backed_plan_without_repo_path_test() {
  let dir = "test/tmp/execplan-artifact-backed-plan-validate"
  reset_dir(dir)
  let plan_ref = "runs/run-artifact-plan/outputs/plan.md"
  let #(bundle_ref, _bundle_sha, _plan_text) =
    write_artifact_backed_plan_bundle(dir, plan_ref, "", True)
  let helper = "../../../.scherzo/workflows/scripts/scherzo-execplan"

  let artifact =
    run_shell_in(
      dir,
      helper
        <> " validate-bundle --bundle .scherzo-state/artifacts/"
        <> bundle_ref
        <> " --artifact-root .scherzo-state/artifacts --repo-root .",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(artifact.stdout, "BUNDLE_VALID=ok")
}

pub fn implementation_prepare_uses_plan_artifact_without_repo_path_test() {
  let dir = "test/tmp/execplan-artifact-backed-plan-prepare"
  reset_dir(dir)
  let plan_ref = "runs/run-artifact-plan/outputs/plan.md"
  let #(bundle_ref, bundle_sha, plan_text) =
    write_artifact_backed_plan_bundle(dir, plan_ref, "", True)
  let issue_context =
    "Bundle ref: " <> bundle_ref <> "\nBundle sha256: " <> bundle_sha <> "\n"
  let helper = "../../../.scherzo/workflows/scripts/scherzo-execplan"

  let artifact =
    run_shell_in(
      dir,
      "env SCHERZO_REPO_ROOT=$PWD SCHERZO_ISSUE_CONTEXT="
        <> shell_quote(issue_context)
        <> " "
        <> helper
        <> " implementation-prepare --from-issue-context",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(artifact.stdout, "PLAN=tmp/execplan-review-doc.md")
  let assert Ok(prepared_plan) =
    simplifile.read(dir <> "/tmp/execplan-review-doc.md")
  assert prepared_plan == plan_text
  let assert Ok(metadata) =
    simplifile.read(dir <> "/tmp/scherzo-implementation.json")
  assert string.contains(
    metadata,
    "\"plan_path\": \"tmp/execplan-review-doc.md\"",
  )
  assert string.contains(
    metadata,
    "\"plan_artifact_ref\": \"" <> plan_ref <> "\"",
  )
  assert string.contains(metadata, "\"legacy_review_doc_path\": \"\"")
}

pub fn implementation_prepare_rejects_plan_hash_mismatch_test() {
  let dir = "test/tmp/execplan-artifact-plan-hash-mismatch"
  reset_dir(dir)
  let plan_ref = "runs/run-artifact-plan/outputs/plan.md"
  let #(bundle_ref, bundle_sha, _plan_text) =
    write_artifact_backed_plan_bundle(
      dir,
      plan_ref,
      "0000000000000000000000000000000000000000000000000000000000000000",
      True,
    )
  let issue_context =
    "Bundle ref: " <> bundle_ref <> "\nBundle sha256: " <> bundle_sha <> "\n"
  let helper = "../../../.scherzo/workflows/scripts/scherzo-execplan"

  let artifact =
    run_shell_in(
      dir,
      "env SCHERZO_REPO_ROOT=$PWD SCHERZO_ISSUE_CONTEXT="
        <> shell_quote(issue_context)
        <> " "
        <> helper
        <> " implementation-prepare --from-issue-context",
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(2)
  assert string.contains(
    artifact.stderr,
    "SCHERZO_FAILURE_CODE=execplan_v2_plan_hash_mismatch",
  )
}

pub fn implementation_prepare_rejects_missing_plan_artifact_test() {
  let dir = "test/tmp/execplan-artifact-plan-missing"
  reset_dir(dir)
  let #(bundle_ref, bundle_sha, _plan_text) =
    write_artifact_backed_plan_bundle(
      dir,
      "runs/run-artifact-plan/outputs/missing-plan.md",
      "",
      False,
    )
  let issue_context =
    "Bundle ref: " <> bundle_ref <> "\nBundle sha256: " <> bundle_sha <> "\n"
  let helper = "../../../.scherzo/workflows/scripts/scherzo-execplan"

  let artifact =
    run_shell_in(
      dir,
      "env SCHERZO_REPO_ROOT=$PWD SCHERZO_ISSUE_CONTEXT="
        <> shell_quote(issue_context)
        <> " "
        <> helper
        <> " implementation-prepare --from-issue-context",
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(2)
  assert string.contains(
    artifact.stderr,
    "SCHERZO_FAILURE_CODE=execplan_v2_plan_missing",
  )
}

pub fn implementation_prepare_failure_writes_retention_marker_test() {
  let dir = "test/tmp/execplan-implementation-prepare-retention"
  reset_dir(dir)
  let run_root = dir <> "/run-root"
  let assert Ok(Nil) = simplifile.create_directory_all(run_root)

  let artifact =
    run_shell(
      "SCHERZO_RUN_ROOT="
      <> shell_quote(run_root)
      <> " SCHERZO_ISSUE_IDENTIFIER=LIV-385 SCHERZO_ISSUE_CONTEXT="
      <> shell_quote("no bundle here")
      <> " .scherzo/workflows/scripts/scherzo-execplan implementation-prepare --from-issue-context",
    )

  assert artifact.status == step_artifact.StepFailed
  assert string.contains(artifact.stderr, "Bundle ref")
  let assert Ok(marker) =
    simplifile.read(run_root <> "/.scherzo-keep-workspace")
  assert string.contains(marker, "Source kind: execplan")
  assert string.contains(marker, "Source: LIV-385")
}

pub fn implementation_prepare_accepts_source_handoff_issue_split_test() {
  let dir = "test/tmp/execplan-implementation-source-handoff-split"
  reset_dir(dir)
  let #(bundle_ref, bundle_sha) = write_source_handoff_split_bundle(dir)
  let issue_context =
    "Bundle ref: " <> bundle_ref <> "\nBundle sha256: " <> bundle_sha <> "\n"
  let helper = "../../../.scherzo/workflows/scripts/scherzo-execplan"

  let artifact =
    run_shell_in(
      dir,
      "env SCHERZO_REPO_ROOT=$PWD"
        <> " SCHERZO_ISSUE_IDENTIFIER=LIV-423"
        <> " SCHERZO_ISSUE_TITLE="
        <> shell_quote("Implement: Fixture source LIV-418")
        <> " SCHERZO_ISSUE_URL="
        <> shell_quote(
        "https://linear.app/living-systems/issue/LIV-423/implement-fixture",
      )
        <> " SCHERZO_ISSUE_CONTEXT="
        <> shell_quote(issue_context)
        <> " "
        <> helper
        <> " implementation-prepare --from-issue-context && "
        <> helper
        <> " gate-no-conflict",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(
    artifact.stdout,
    "IMPLEMENTATION_ISSUE_IDENTIFIER=LIV-423",
  )
  assert string.contains(artifact.stdout, "SOURCE_ISSUE_IDENTIFIER=LIV-418")
  assert string.contains(
    artifact.stdout,
    "HANDOFF_SOURCE_IDENTITY_SPLIT=expected",
  )
  assert string.contains(artifact.stdout, "EXECPLAN_V2_CONFLICT=none")
  let assert Ok(metadata) =
    simplifile.read(dir <> "/tmp/scherzo-implementation.json")
  assert string.contains(metadata, "\"issue_identifier\": \"LIV-423\"")
  assert string.contains(
    metadata,
    "\"implementation_issue_identifier\": \"LIV-423\"",
  )
  assert string.contains(metadata, "\"source_issue_identifier\": \"LIV-418\"")
  assert string.contains(
    metadata,
    "\"source_issue_url\": \"https://linear.app/living-systems/issue/LIV-418/fixture-v2-execplan-bundle\"",
  )
  let assert Ok(False) = simplifile.is_file(dir <> "/tmp/execplan-conflict.md")
}

pub fn implementation_prepare_rejects_current_handoff_issue_mismatch_test() {
  let dir = "test/tmp/execplan-implementation-current-handoff-mismatch"
  reset_dir(dir)
  let #(bundle_ref, bundle_sha) = write_source_handoff_split_bundle(dir)
  let issue_context =
    "Bundle ref: " <> bundle_ref <> "\nBundle sha256: " <> bundle_sha <> "\n"
  let helper = "../../../.scherzo/workflows/scripts/scherzo-execplan"

  let artifact =
    run_shell_in(
      dir,
      "env SCHERZO_REPO_ROOT=$PWD"
        <> " SCHERZO_ISSUE_IDENTIFIER=LIV-999"
        <> " SCHERZO_ISSUE_TITLE="
        <> shell_quote("Unexpected implementation handoff")
        <> " SCHERZO_ISSUE_CONTEXT="
        <> shell_quote(issue_context)
        <> " "
        <> helper
        <> " implementation-prepare --from-issue-context",
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(2)
  assert artifact.failure_code
    == Some("execplan_implementation_handoff_mismatch")
  assert string.contains(artifact.stderr, "SCHERZO_ISSUE_IDENTIFIER=LIV-999")
  assert string.contains(
    artifact.stderr,
    "implementation_handoff.issue_identifier=LIV-423",
  )
  let assert Ok(False) =
    simplifile.is_file(dir <> "/tmp/scherzo-implementation.json")
}

pub fn validate_bundle_rejects_stale_pack_fixture_test() {
  let artifact =
    run_helper(
      "validate-bundle --bundle test/fixtures/execplan_v2/exec-plan-bundle.stale-pack.json --repo-root .",
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(2)
  assert string.contains(
    artifact.stderr,
    "SCHERZO_FAILURE_CODE=execplan_v2_stale_implementation_pack",
  )
}

pub fn validate_bundle_rejects_missing_review_doc_test() {
  let path =
    mutated_bundle(
      "test/tmp/execplan-missing-review-doc",
      each: "test/fixtures/execplan_v2/review-doc.valid.md",
      with: "test/tmp/execplan-missing-review-doc/missing.md",
    )

  let artifact =
    run_helper("validate-bundle --bundle " <> path <> " --repo-root .")

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(2)
  assert string.contains(
    artifact.stderr,
    "SCHERZO_FAILURE_CODE=execplan_v2_review_doc_missing",
  )
}

pub fn prepare_revision_resolves_review_doc_from_recorded_branch_test() {
  let dir = "test/tmp/execplan-prepare-revision-branch"
  reset_dir(dir)
  let review_path = dir <> "/docs/plans/unmerged.md"
  let #(bundle_ref, bundle_sha) = write_revision_bundle(dir, review_path)
  let driver = dir <> "/workspace-driver"
  let log = dir <> "/workspace-driver.log"
  let assert Ok(Nil) =
    simplifile.write(
      driver,
      "#!/bin/sh\n"
        <> "printf '%s\\n' \"$*\" >> "
        <> shell_quote(log)
        <> "\n"
        <> "if [ \"$1\" = refresh-base ] && [ \"$5\" = ca667773c9a6d31bb64676c103b3f1f14c3bcced ]; then\n"
        <> "  printf '%s\\n' '{\"version\":1,\"status\":\"base_not_found\",\"failure_code\":\"base_not_found\",\"message\":\"head not local\"}'\n"
        <> "  exit 1\n"
        <> "fi\n"
        <> "if [ \"$1\" = refresh-base ] && [ \"$5\" = execplan/liv-314-unmerged@origin ]; then\n"
        <> "  mkdir -p "
        <> shell_quote(dir <> "/docs/plans")
        <> "\n"
        <> "  cp test/fixtures/execplan_v2/review-doc.valid.md "
        <> shell_quote(review_path)
        <> "\n"
        <> "  printf '%s\\n' '{\"version\":1,\"status\":\"rebased_clean\",\"stage\":\"prepare_revision\",\"base_ref\":\"execplan/liv-314-unmerged@origin\",\"base_revision\":\"execplan/liv-314-unmerged@origin\",\"before_revision\":\"main\",\"after_revision\":\"branch\",\"conflict_files\":[]}'\n"
        <> "  exit 0\n"
        <> "fi\n"
        <> "printf '%s\\n' '{\"version\":1,\"status\":\"base_not_found\",\"failure_code\":\"base_not_found\",\"message\":\"missing revision base\"}'\n"
        <> "exit 1\n",
    )
  let chmod = run_shell("chmod +x " <> shell_quote(driver))
  assert chmod.status == step_artifact.StepSucceeded
  let issue_context =
    "Bundle ref: " <> bundle_ref <> "\nBundle sha256: " <> bundle_sha <> "\n"

  let artifact =
    run_shell(
      "env SCHERZO_REPO_ROOT="
      <> shell_quote(dir <> "/repo")
      <> " SCHERZO_WORKSPACE_DRIVER="
      <> shell_quote(driver)
      <> " SCHERZO_JJ_WORKSPACE_REMOTE=origin SCHERZO_ISSUE_CONTEXT="
      <> shell_quote(issue_context)
      <> " .scherzo/workflows/scripts/scherzo-execplan prepare-revision --from-issue-context --write-bundle "
      <> shell_quote(dir <> "/previous-bundle.json")
      <> " --write-review-doc-path "
      <> shell_quote(dir <> "/review.path")
      <> " --write-pack "
      <> shell_quote(dir <> "/previous-pack.json"),
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(artifact.stdout, "PREPARE_REVISION_STATUS=ok")
  let assert Ok(path_contents) = simplifile.read(dir <> "/review.path")
  assert path_contents == review_path <> "\n"
  let assert Ok(review_doc) = simplifile.read(review_path)
  assert string.contains(review_doc, "Purpose / Big Picture")
  let assert Ok(driver_log) = simplifile.read(log)
  assert string.contains(
    driver_log,
    "refresh-base --stage prepare_revision --target ca667773c9a6d31bb64676c103b3f1f14c3bcced --json",
  )
  assert string.contains(
    driver_log,
    "refresh-base --stage prepare_revision --target execplan/liv-314-unmerged@origin --json",
  )
}

pub fn prepare_revision_reports_revision_base_missing_when_branch_unresolved_test() {
  let dir = "test/tmp/execplan-prepare-revision-base-missing"
  reset_dir(dir)
  let review_path = dir <> "/docs/plans/unmerged.md"
  let #(bundle_ref, bundle_sha) = write_revision_bundle(dir, review_path)
  let driver = dir <> "/workspace-driver"
  let assert Ok(Nil) =
    simplifile.write(
      driver,
      "#!/bin/sh\n"
        <> "printf '%s\\n' '{\"version\":1,\"status\":\"base_not_found\",\"failure_code\":\"base_not_found\",\"message\":\"missing revision base\"}'\n"
        <> "exit 1\n",
    )
  let chmod = run_shell("chmod +x " <> shell_quote(driver))
  assert chmod.status == step_artifact.StepSucceeded
  let issue_context =
    "Bundle ref: " <> bundle_ref <> "\nBundle sha256: " <> bundle_sha <> "\n"

  let artifact =
    run_shell(
      "env SCHERZO_REPO_ROOT="
      <> shell_quote(dir <> "/repo")
      <> " SCHERZO_WORKSPACE_DRIVER="
      <> shell_quote(driver)
      <> " SCHERZO_JJ_WORKSPACE_REMOTE=origin SCHERZO_ISSUE_CONTEXT="
      <> shell_quote(issue_context)
      <> " .scherzo/workflows/scripts/scherzo-execplan prepare-revision --from-issue-context --write-bundle "
      <> shell_quote(dir <> "/previous-bundle.json")
      <> " --write-review-doc-path "
      <> shell_quote(dir <> "/review.path")
      <> " --write-pack "
      <> shell_quote(dir <> "/previous-pack.json"),
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(2)
  assert string.contains(
    artifact.stderr,
    "SCHERZO_FAILURE_CODE=execplan_revision_base_missing",
  )
  assert !string.contains(
    artifact.stderr,
    "SCHERZO_FAILURE_CODE=execplan_v2_review_doc_missing",
  )
}

pub fn prepare_revision_refresh_base_timeout_test() {
  let dir = "test/tmp/execplan-prepare-revision-refresh-timeout"
  reset_dir(dir)
  let review_path = dir <> "/docs/plans/unmerged.md"
  let #(bundle_ref, bundle_sha) = write_revision_bundle(dir, review_path)
  let driver = dir <> "/workspace-driver"
  let assert Ok(Nil) =
    simplifile.write(
      driver,
      "#!/usr/bin/env python3\nimport time\ntime.sleep(5)\n",
    )
  let chmod = run_shell("chmod +x " <> shell_quote(driver))
  assert chmod.status == step_artifact.StepSucceeded
  let issue_context =
    "Bundle ref: " <> bundle_ref <> "\nBundle sha256: " <> bundle_sha <> "\n"

  let artifact =
    run_shell(
      "env SCHERZO_REPO_ROOT="
      <> shell_quote(dir <> "/repo")
      <> " SCHERZO_WORKSPACE_DRIVER="
      <> shell_quote(driver)
      <> " SCHERZO_EXECPLAN_REVISION_REFRESH_TIMEOUT_SECONDS=0.1 SCHERZO_ISSUE_CONTEXT="
      <> shell_quote(issue_context)
      <> " .scherzo/workflows/scripts/scherzo-execplan prepare-revision --from-issue-context --write-bundle "
      <> shell_quote(dir <> "/previous-bundle.json")
      <> " --write-review-doc-path "
      <> shell_quote(dir <> "/review.path")
      <> " --write-pack "
      <> shell_quote(dir <> "/previous-pack.json"),
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(2)
  assert string.contains(
    artifact.stderr,
    "SCHERZO_FAILURE_CODE=execplan_revision_base_missing",
  )
  assert string.contains(artifact.stderr, "timed out after 0.1s")
}

pub fn prepare_revision_rejects_unsafe_review_surface_targets_before_refresh_test() {
  let dir = "test/tmp/execplan-prepare-revision-unsafe-target"
  reset_dir(dir)
  let review_path = dir <> "/docs/plans/unmerged.md"
  let #(bundle_ref, bundle_sha) =
    write_revision_bundle_with_surface(
      dir,
      review_path,
      "execplan/liv-314@unexpected-remote",
      "--not-a-commit",
    )
  let driver = dir <> "/workspace-driver"
  let log = dir <> "/workspace-driver.log"
  let assert Ok(Nil) =
    simplifile.write(
      driver,
      "#!/bin/sh\n"
        <> "printf '%s\\n' \"$*\" >> "
        <> shell_quote(log)
        <> "\n"
        <> "printf '%s\\n' '{\"version\":1,\"status\":\"rebased_clean\"}'\n",
    )
  let chmod = run_shell("chmod +x " <> shell_quote(driver))
  assert chmod.status == step_artifact.StepSucceeded
  let issue_context =
    "Bundle ref: " <> bundle_ref <> "\nBundle sha256: " <> bundle_sha <> "\n"

  let artifact =
    run_shell(
      "env SCHERZO_REPO_ROOT="
      <> shell_quote(dir <> "/repo")
      <> " SCHERZO_WORKSPACE_DRIVER="
      <> shell_quote(driver)
      <> " SCHERZO_ISSUE_CONTEXT="
      <> shell_quote(issue_context)
      <> " .scherzo/workflows/scripts/scherzo-execplan prepare-revision --from-issue-context --write-bundle "
      <> shell_quote(dir <> "/previous-bundle.json")
      <> " --write-review-doc-path "
      <> shell_quote(dir <> "/review.path")
      <> " --write-pack "
      <> shell_quote(dir <> "/previous-pack.json"),
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(2)
  assert string.contains(
    artifact.stderr,
    "SCHERZO_FAILURE_CODE=execplan_revision_base_missing",
  )
  assert string.contains(artifact.stderr, "no safe review_surface branch/head")
  let assert Error(_) = simplifile.read(log)
}

pub fn validate_bundle_rejects_review_doc_hash_mismatch_test() {
  let path =
    mutated_bundle(
      "test/tmp/execplan-review-hash-mismatch",
      each: "64288f367d31d10a48decbb7f5b19ec4975e1a3a2991be2a4bc1007d8a61dcf4",
      with: "0000000000000000000000000000000000000000000000000000000000000000",
    )

  let artifact =
    run_helper("validate-bundle --bundle " <> path <> " --repo-root .")

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(2)
  assert string.contains(
    artifact.stderr,
    "SCHERZO_FAILURE_CODE=execplan_v2_review_doc_hash_mismatch",
  )
}

pub fn validate_bundle_rejects_missing_implementation_pack_test() {
  let path =
    mutated_bundle(
      "test/tmp/execplan-missing-pack",
      each: "runs/run-1/outputs/implementation_pack.json",
      with: "runs/run-missing/outputs/implementation_pack.json",
    )

  let artifact =
    run_helper("validate-bundle --bundle " <> path <> " --repo-root .")

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(2)
  assert string.contains(
    artifact.stderr,
    "SCHERZO_FAILURE_CODE=execplan_v2_implementation_pack_missing",
  )
}

pub fn validate_bundle_rejects_implementation_pack_hash_mismatch_test() {
  let path =
    mutated_bundle(
      "test/tmp/execplan-pack-hash-mismatch",
      each: "dbcb84d078e47839e8da760b1208e6b5606bce45ab3228a24a92e0b5afd21545",
      with: "0000000000000000000000000000000000000000000000000000000000000000",
    )

  let artifact =
    run_helper("validate-bundle --bundle " <> path <> " --repo-root .")

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(2)
  assert string.contains(
    artifact.stderr,
    "SCHERZO_FAILURE_CODE=execplan_v2_implementation_pack_hash_mismatch",
  )
}

pub fn validate_bundle_rejects_absolute_review_doc_path_test() {
  let artifact =
    run_helper(
      "validate-bundle --bundle test/fixtures/execplan_v2/exec-plan-bundle.absolute-path.json --repo-root .",
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(2)
  assert string.contains(artifact.stderr, "schema validation failed")
}

pub fn validate_bundle_rejects_bundle_self_hash_test() {
  let dir = "test/tmp/execplan-self-hash"
  reset_dir(dir)
  let assert Ok(source) =
    simplifile.read("test/fixtures/execplan_v2/exec-plan-bundle.valid.json")
  let mutated =
    string.replace(
      source,
      each: "  \"artifact_type\": \"exec_plan_bundle\",\n",
      with: "  \"artifact_type\": \"exec_plan_bundle\",\n  \"sha256\": \"0000000000000000000000000000000000000000000000000000000000000000\",\n",
    )
  let path = dir <> "/bundle.json"
  let assert Ok(Nil) = simplifile.write(path, mutated)

  let artifact =
    run_helper("validate-bundle --bundle " <> path <> " --repo-root .")

  assert artifact.status == step_artifact.StepFailed
  assert string.contains(artifact.stderr, "must not store its own SHA-256")
}

pub fn validate_review_doc_rejects_mechanical_sections_test() {
  let dir = "test/tmp/execplan-review-doc"
  reset_dir(dir)
  let path = dir <> "/review.md"
  let assert Ok(valid) =
    simplifile.read("test/fixtures/execplan_v2/review-doc.valid.md")
  let assert Ok(Nil) =
    simplifile.write(
      path,
      valid <> "\n## Concrete Steps\n\nThese belong in the pack.\n",
    )

  let artifact = run_helper("validate-review-doc --path " <> path)

  assert artifact.status == step_artifact.StepFailed
  assert string.contains(artifact.stderr, "mechanical implementation sections")
}

pub fn validate_review_doc_rejects_unchecked_required_progress_test() {
  let dir = "test/tmp/execplan-progress-preflight"
  reset_dir(dir)
  let path = dir <> "/review.md"
  let assert Ok(valid) =
    simplifile.read("test/fixtures/execplan_v2/review-doc.valid.md")
  let review =
    string.replace(
      valid,
      each: "## Progress\n\n- [x] 2026-05-15: Created the fixture review document.\n\n## Decision Log",
      with: "## Progress\n\n- [ ] Run full validation before rollout.\n\n## Decision Log",
    )
  let assert Ok(Nil) = simplifile.write(path, review)

  let artifact = run_helper("validate-review-doc --path " <> path)

  assert_completion_preflight_failed(
    artifact,
    "unchecked required implementation",
  )
}

pub fn validate_review_doc_rejects_ambiguous_milestones_test() {
  let dir = "test/tmp/execplan-ambiguous-milestone"
  reset_dir(dir)
  let path = dir <> "/review.md"
  let assert Ok(valid) =
    simplifile.read("test/fixtures/execplan_v2/review-doc.valid.md")
  let review =
    string.replace(
      valid,
      each: "## Milestones\n\nThe single milestone is to validate the review document, validate the pack, and prove the bundle links the two by hash.\n\n## Progress",
      with: "## Milestones\n\nFinish the remaining implementation work as needed.\n\n## Progress",
    )
  let assert Ok(Nil) = simplifile.write(path, review)

  let artifact = run_helper("validate-review-doc --path " <> path)

  assert_completion_preflight_failed(artifact, "ambiguous milestone")
}

pub fn validate_review_doc_rejects_unverifiable_acceptance_test() {
  let dir = "test/tmp/execplan-unverifiable-acceptance"
  reset_dir(dir)
  let path = dir <> "/review.md"
  let review =
    review_doc_with_validation(
      "The feature is done when the outcome looks good as needed.",
    )
  let assert Ok(Nil) = simplifile.write(path, review)

  let artifact = run_helper("validate-review-doc --path " <> path)

  assert_completion_preflight_failed(
    artifact,
    "Validation and Acceptance is not verifiable",
  )
  assert string.contains(artifact.stderr, "broad completion wording")
}

pub fn validate_review_doc_rejects_negated_acceptance_evidence_test() {
  let dir = "test/tmp/execplan-negated-acceptance-evidence"
  reset_dir(dir)
  let path = dir <> "/review.md"
  let review =
    review_doc_with_validation(
      "No manual evidence is required. No commands are required. No tests are required. No artifact output is required. The feature is complete when appropriate.",
    )
  let assert Ok(Nil) = simplifile.write(path, review)

  let artifact = run_helper("validate-review-doc --path " <> path)

  assert_completion_preflight_failed(
    artifact,
    "Validation and Acceptance is not verifiable",
  )
}

pub fn materialize_pack_rejects_missing_negative_test_evidence_test() {
  let dir = "test/tmp/execplan-pack-negative-evidence"
  reset_dir(dir)
  let review_path = dir <> "/review.md"
  let submission_path = dir <> "/submission.json"
  let output_path = dir <> "/pack.json"
  let review =
    review_doc_with_validation(
      "Acceptance requires negative, invalid-payload, absent-output, and idempotent duplicate-conflict test evidence before implementation is complete.",
    )
  let assert Ok(Nil) = simplifile.write(review_path, review)
  let assert Ok(Nil) =
    simplifile.write(
      submission_path,
      pack_submission("Missing negative evidence"),
    )

  let artifact =
    run_helper(
      "materialize-pack --review-doc "
      <> review_path
      <> " --submission "
      <> submission_path
      <> " --output "
      <> output_path,
    )

  assert_completion_preflight_failed(
    artifact,
    "negative/error-path test coverage",
  )
}

pub fn materialize_pack_rejects_negated_negative_test_evidence_test() {
  let dir = "test/tmp/execplan-pack-negated-negative-evidence"
  reset_dir(dir)
  let review_path = dir <> "/review.md"
  let submission_path = dir <> "/submission.json"
  let output_path = dir <> "/pack.json"
  let review =
    review_doc_with_validation(
      "Acceptance requires negative error-path test evidence before implementation is complete.",
    )
  let assert Ok(Nil) = simplifile.write(review_path, review)
  let assert Ok(Nil) =
    simplifile.write(
      submission_path,
      pack_submission_with_commands_and_testing(
        "Negated negative evidence",
        "[\"scripts/scherzo-execplan validate-review-doc --path test/fixtures/execplan_v2/review-doc.valid.md\"]",
        "No negative error-path tests are included. Run validate-review-doc only.",
      ),
    )

  let artifact =
    run_helper(
      "materialize-pack --review-doc "
      <> review_path
      <> " --submission "
      <> submission_path
      <> " --output "
      <> output_path,
    )

  assert_completion_preflight_failed(
    artifact,
    "negative/error-path test coverage",
  )
}

pub fn materialize_pack_rejects_missing_validation_evidence_step_test() {
  let dir = "test/tmp/execplan-pack-no-validation-evidence"
  reset_dir(dir)
  let review_path = dir <> "/review.md"
  let submission_path = dir <> "/submission.json"
  let output_path = dir <> "/pack.json"
  let review =
    review_doc_with_validation(
      "Run `scripts/scherzo-execplan validate-review-doc --path test/fixtures/execplan_v2/review-doc.valid.md` and expect a zero exit code.",
    )
  let assert Ok(Nil) = simplifile.write(review_path, review)
  let assert Ok(Nil) =
    simplifile.write(
      submission_path,
      pack_submission_with_commands_and_testing(
        "Missing validation evidence",
        "[]",
        "The pack links to the review document and bundle hash.",
      ),
    )

  let artifact =
    run_helper(
      "materialize-pack --review-doc "
      <> review_path
      <> " --submission "
      <> submission_path
      <> " --output "
      <> output_path,
    )

  assert_completion_preflight_failed(
    artifact,
    "no validation commands or explicit manual-evidence step",
  )
}

pub fn materialize_pack_rejects_unrecognized_validation_commands_test() {
  let dir = "test/tmp/execplan-pack-unrecognized-validation-command"
  reset_dir(dir)
  let review_path = dir <> "/review.md"
  let submission_path = dir <> "/submission.json"
  let output_path = dir <> "/pack.json"
  let review =
    review_doc_with_validation(
      "Run `scripts/scherzo-execplan validate-review-doc --path test/fixtures/execplan_v2/review-doc.valid.md` and expect a zero exit code.",
    )
  let assert Ok(Nil) = simplifile.write(review_path, review)
  let assert Ok(Nil) =
    simplifile.write(
      submission_path,
      pack_submission_with_commands_and_testing(
        "Unrecognized validation command",
        "[\"echo fixture ready\"]",
        "The pack links to the review document and bundle hash.",
      ),
    )

  let artifact =
    run_helper(
      "materialize-pack --review-doc "
      <> review_path
      <> " --submission "
      <> submission_path
      <> " --output "
      <> output_path,
    )

  assert_completion_preflight_failed(
    artifact,
    "commands do not include a recognizable test",
  )
}

pub fn materialize_pack_accepts_manual_screenshot_evidence_without_commands_test() {
  let dir = "test/tmp/execplan-pack-manual-evidence"
  reset_dir(dir)
  let review_path = dir <> "/review.md"
  let submission_path = dir <> "/submission.json"
  let output_path = dir <> "/pack.json"
  let review =
    review_doc_with_validation(
      "Acceptance requires manual browser evidence: collect screenshot evidence and inspect the rendered output.",
    )
  let assert Ok(Nil) = simplifile.write(review_path, review)
  let assert Ok(Nil) =
    simplifile.write(
      submission_path,
      pack_submission_with_commands_and_testing(
        "Manual screenshot evidence",
        "[]",
        "Collect screenshot evidence and inspect the rendered browser output manually.",
      ),
    )

  let artifact =
    run_helper(
      "materialize-pack --review-doc "
      <> review_path
      <> " --submission "
      <> submission_path
      <> " --output "
      <> output_path,
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  let assert Ok(_) = simplifile.read(output_path)
}

pub fn materialize_pack_rejects_required_behavior_missing_from_steps_test() {
  let dir = "test/tmp/execplan-pack-missing-behavior"
  reset_dir(dir)
  let review_path = dir <> "/review.md"
  let submission_path = dir <> "/submission.json"
  let output_path = dir <> "/pack.json"
  let review =
    review_doc_with_validation(
      "Acceptance requires migrating every local test and scheduled-job runbook reference from removed root helper paths to bundle-local helper paths.",
    )
  let assert Ok(Nil) = simplifile.write(review_path, review)
  let assert Ok(Nil) =
    simplifile.write(
      submission_path,
      pack_submission("Missing helper migration"),
    )

  let artifact =
    run_helper(
      "materialize-pack --review-doc "
      <> review_path
      <> " --submission "
      <> submission_path
      <> " --output "
      <> output_path,
    )

  assert_completion_preflight_failed(
    artifact,
    "not represented in implementation pack",
  )
}

pub fn prepare_review_doc_target_creates_custom_directory_test() {
  let dir = "test/tmp/execplan-target-prepare-custom"
  reset_dir(dir)

  let artifact =
    run_shell_in(
      dir,
      "env SCHERZO_ISSUE_CONTEXT="
        <> shell_quote("Create an execplan at doobar/docs/plans")
        <> " "
        <> tmp_repo_path(".scherzo/workflows/scripts/scherzo-execplan")
        <> " prepare-review-doc-target --from-issue-context --write-target tmp/target.json",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert string.contains(artifact.stdout, "REVIEW_DOC_TARGET_KIND=directory")
  assert string.contains(
    artifact.stdout,
    "REVIEW_DOC_TARGET_PATH=doobar/docs/plans",
  )
  let assert Ok(True) = simplifile.is_directory(dir <> "/doobar/docs/plans")
}

pub fn prepare_review_doc_target_creates_custom_file_parent_test() {
  let dir = "test/tmp/execplan-target-prepare-file"
  reset_dir(dir)

  let artifact =
    run_shell_in(
      dir,
      "env SCHERZO_ISSUE_CONTEXT="
        <> shell_quote("Create an execplan at doobar/docs/plans/exact.md")
        <> " "
        <> tmp_repo_path(".scherzo/workflows/scripts/scherzo-execplan")
        <> " prepare-review-doc-target --from-issue-context --write-target tmp/target.json",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert string.contains(artifact.stdout, "REVIEW_DOC_TARGET_KIND=file")
  assert string.contains(
    artifact.stdout,
    "REVIEW_DOC_TARGET_PATH=doobar/docs/plans/exact.md",
  )
  let assert Ok(True) = simplifile.is_directory(dir <> "/doobar/docs/plans")
}

pub fn prepare_review_doc_target_ignores_plain_infinitive_to_test() {
  let dir = "test/tmp/execplan-target-infinitive-to"
  reset_dir(dir)

  let artifact =
    run_shell_in(
      dir,
      "env SCHERZO_ISSUE_CONTEXT="
        <> shell_quote("Create an execplan to add custom target support")
        <> " "
        <> tmp_repo_path(".scherzo/workflows/scripts/scherzo-execplan")
        <> " prepare-review-doc-target --from-issue-context --write-target tmp/target.json",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert string.contains(artifact.stdout, "REVIEW_DOC_TARGET_KIND=directory")
  assert string.contains(artifact.stdout, "REVIEW_DOC_TARGET_PATH=docs/plans")
  assert string.contains(artifact.stdout, "REVIEW_DOC_TARGET_DEFAULT=1")
  let assert Ok(True) = simplifile.is_directory(dir <> "/docs/plans")
  let assert Ok(False) = simplifile.is_directory(dir <> "/add")
}

pub fn prepare_review_doc_target_ignores_generic_destination_field_test() {
  let dir = "test/tmp/execplan-target-generic-destination"
  reset_dir(dir)

  let artifact =
    run_shell_in(
      dir,
      "env SCHERZO_ISSUE_CONTEXT="
        <> shell_quote("Create an execplan\n\nDestination: production")
        <> " "
        <> tmp_repo_path(".scherzo/workflows/scripts/scherzo-execplan")
        <> " prepare-review-doc-target --from-issue-context --write-target tmp/target.json",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert string.contains(artifact.stdout, "REVIEW_DOC_TARGET_PATH=docs/plans")
  assert string.contains(artifact.stdout, "REVIEW_DOC_TARGET_DEFAULT=1")
  let assert Ok(False) = simplifile.is_directory(dir <> "/production")
}

pub fn discover_changed_review_doc_accepts_default_docs_plans_test() {
  let dir = "test/tmp/execplan-discovery-default"
  reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/docs/plans")
  write_valid_review_doc(dir <> "/docs/plans/default.md")

  let artifact =
    run_shell_in(
      dir,
      "env SCHERZO_WORKSPACE_DRIVER="
        <> shell_quote(tmp_repo_path("scripts/scherzo-workspace-noop"))
        <> " SCHERZO_WORKSPACE_PATH=. "
        <> tmp_repo_path(".scherzo/workflows/scripts/scherzo-execplan")
        <> " validate-review-doc --discover-changed-review-doc --write-path tmp/review.path",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert string.contains(artifact.stdout, "REVIEW_DOC_TARGET_PATH=docs/plans")
  assert string.contains(
    artifact.stdout,
    "REVIEW_DOC_PATH=docs/plans/default.md",
  )
  let assert Ok(review_path) = simplifile.read(dir <> "/tmp/review.path")
  assert review_path == "docs/plans/default.md\n"
}

pub fn discover_changed_review_doc_accepts_custom_requested_directory_test() {
  let dir = "test/tmp/execplan-discovery-custom"
  reset_dir(dir)
  let helper = tmp_repo_path(".scherzo/workflows/scripts/scherzo-execplan")
  let driver = tmp_repo_path("scripts/scherzo-workspace-noop")

  let prepare =
    run_shell_in(
      dir,
      "env SCHERZO_ISSUE_CONTEXT="
        <> shell_quote("Create an execplan at doobar/docs/plans")
        <> " "
        <> helper
        <> " prepare-review-doc-target --from-issue-context --write-target tmp/target.json",
    )
  assert prepare.status == step_artifact.StepSucceeded
  write_valid_review_doc(dir <> "/doobar/docs/plans/custom.md")

  let artifact =
    run_shell_in(
      dir,
      "env SCHERZO_WORKSPACE_DRIVER="
        <> shell_quote(driver)
        <> " SCHERZO_WORKSPACE_PATH=. "
        <> helper
        <> " validate-review-doc --discover-changed-review-doc --target-file tmp/target.json --write-path tmp/review.path",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert string.contains(
    artifact.stdout,
    "REVIEW_DOC_TARGET_PATH=doobar/docs/plans",
  )
  assert string.contains(
    artifact.stdout,
    "REVIEW_DOC_PATH=doobar/docs/plans/custom.md",
  )
  let assert Ok(review_path) = simplifile.read(dir <> "/tmp/review.path")
  assert review_path == "doobar/docs/plans/custom.md\n"
}

pub fn discover_changed_review_doc_accepts_custom_requested_file_test() {
  let dir = "test/tmp/execplan-discovery-custom-file"
  reset_dir(dir)
  let helper = tmp_repo_path(".scherzo/workflows/scripts/scherzo-execplan")
  let driver = tmp_repo_path("scripts/scherzo-workspace-noop")

  let prepare =
    run_shell_in(
      dir,
      "env SCHERZO_ISSUE_CONTEXT="
        <> shell_quote("Create an execplan at doobar/docs/plans/exact.md")
        <> " "
        <> helper
        <> " prepare-review-doc-target --from-issue-context --write-target tmp/target.json",
    )
  assert prepare.status == step_artifact.StepSucceeded
  write_valid_review_doc(dir <> "/doobar/docs/plans/exact.md")
  write_valid_review_doc(dir <> "/doobar/docs/plans/sibling.md")

  let artifact =
    run_shell_in(
      dir,
      "env SCHERZO_WORKSPACE_DRIVER="
        <> shell_quote(driver)
        <> " SCHERZO_WORKSPACE_PATH=. "
        <> helper
        <> " validate-review-doc --discover-changed-review-doc --target-file tmp/target.json --write-path tmp/review.path",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert string.contains(artifact.stdout, "REVIEW_DOC_TARGET_KIND=file")
  assert string.contains(
    artifact.stdout,
    "REVIEW_DOC_TARGET_PATH=doobar/docs/plans/exact.md",
  )
  assert string.contains(
    artifact.stdout,
    "REVIEW_DOC_PATH=doobar/docs/plans/exact.md",
  )
  let assert Ok(review_path) = simplifile.read(dir <> "/tmp/review.path")
  assert review_path == "doobar/docs/plans/exact.md\n"
}

pub fn discover_changed_review_doc_rejects_sibling_for_custom_file_test() {
  let dir = "test/tmp/execplan-discovery-custom-file-sibling"
  reset_dir(dir)
  let helper = tmp_repo_path(".scherzo/workflows/scripts/scherzo-execplan")
  let driver = tmp_repo_path("scripts/scherzo-workspace-noop")

  let prepare =
    run_shell_in(
      dir,
      "env SCHERZO_ISSUE_CONTEXT="
        <> shell_quote("Create an execplan at doobar/docs/plans/exact.md")
        <> " "
        <> helper
        <> " prepare-review-doc-target --from-issue-context --write-target tmp/target.json",
    )
  assert prepare.status == step_artifact.StepSucceeded
  write_valid_review_doc(dir <> "/doobar/docs/plans/sibling.md")

  let artifact =
    run_shell_in(
      dir,
      "env SCHERZO_WORKSPACE_DRIVER="
        <> shell_quote(driver)
        <> " SCHERZO_WORKSPACE_PATH=. "
        <> helper
        <> " validate-review-doc --discover-changed-review-doc --target-file tmp/target.json --write-path tmp/review.path",
    )

  assert artifact.status == step_artifact.StepFailed
  assert string.contains(artifact.stderr, "expected exactly one")
  assert string.contains(artifact.stderr, "found 0")
}

pub fn prepare_review_doc_target_rejects_unsafe_path_test() {
  let dir = "test/tmp/execplan-target-unsafe"
  reset_dir(dir)

  let artifact =
    run_shell_in(
      dir,
      "env SCHERZO_ISSUE_CONTEXT="
        <> shell_quote("Create an execplan at ../outside")
        <> " "
        <> tmp_repo_path(".scherzo/workflows/scripts/scherzo-execplan")
        <> " prepare-review-doc-target --from-issue-context --write-target tmp/target.json",
    )

  assert artifact.status == step_artifact.StepFailed
  assert string.contains(
    artifact.stderr,
    "must not contain parent-directory traversal",
  )
}

pub fn discover_changed_review_doc_rejects_zero_candidates_test() {
  let dir = "test/tmp/execplan-discovery-zero"
  reset_dir(dir)

  let artifact =
    run_shell(
      "env SCHERZO_WORKSPACE_DRIVER=scripts/scherzo-workspace-noop SCHERZO_WORKSPACE_PATH="
      <> shell_quote(dir)
      <> " .scherzo/workflows/scripts/scherzo-execplan validate-review-doc --discover-changed-review-doc --write-path "
      <> shell_quote(dir <> "/review.path"),
    )

  assert artifact.status == step_artifact.StepFailed
  assert string.contains(artifact.stderr, "expected exactly one")
}

pub fn discover_changed_review_doc_rejects_multiple_candidates_test() {
  let dir = "test/tmp/execplan-discovery-multiple"
  reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/docs/plans")
  let assert Ok(Nil) = simplifile.write(dir <> "/docs/plans/a.md", "# A\n")
  let assert Ok(Nil) = simplifile.write(dir <> "/docs/plans/b.md", "# B\n")

  let artifact =
    run_shell(
      "env SCHERZO_WORKSPACE_DRIVER=scripts/scherzo-workspace-noop SCHERZO_WORKSPACE_PATH="
      <> shell_quote(dir)
      <> " .scherzo/workflows/scripts/scherzo-execplan validate-review-doc --discover-changed-review-doc --write-path "
      <> shell_quote(dir <> "/review.path"),
    )

  assert artifact.status == step_artifact.StepFailed
  assert string.contains(artifact.stderr, "expected exactly one")
  assert string.contains(artifact.stderr, "found 2")
}

pub fn materialize_pack_discovers_latest_structured_submission_test() {
  let dir = "test/tmp/execplan-structured-latest"
  reset_dir(dir)
  let run_dir = dir <> "/artifacts/runs/run-structured"
  let attempt_1 =
    run_dir
    <> "/incorporate_review/attempt-1/structured/implementation_pack_submission.json"
  let attempt_2 =
    run_dir
    <> "/incorporate_review/attempt-2/structured/implementation_pack_submission.json"
  let assert Ok(Nil) =
    simplifile.create_directory_all(
      run_dir <> "/incorporate_review/attempt-1/structured",
    )
  let assert Ok(Nil) =
    simplifile.create_directory_all(
      run_dir <> "/incorporate_review/attempt-2/structured",
    )
  let assert Ok(Nil) = simplifile.write(attempt_1, pack_submission("Old Pack"))
  let assert Ok(Nil) =
    simplifile.write(attempt_2, pack_submission("Latest Pack"))
  let output = dir <> "/implementation-pack.json"

  let artifact =
    run_shell(
      "env SCHERZO_RUN_ID=run-structured SCHERZO_RUN_ARTIFACT_DIR="
      <> shell_quote(run_dir)
      <> " .scherzo/workflows/scripts/scherzo-execplan materialize-pack --review-doc test/fixtures/execplan_v2/review-doc.valid.md --submission-step incorporate_review --submission-artifact implementation_pack_submission --output "
      <> shell_quote(output),
    )

  assert artifact.status == step_artifact.StepSucceeded
  let assert Ok(pack) = simplifile.read(output)
  assert string.contains(pack, "Latest Pack")
  assert !string.contains(pack, "Old Pack")
}

pub fn publish_review_doc_writes_offline_context_test() {
  let dir = "test/tmp/execplan-publish-context"
  reset_dir(dir)
  let path_file = dir <> "/review.path"
  let context_path = dir <> "/publish-context.json"
  let assert Ok(Nil) =
    simplifile.write(
      path_file,
      "test/fixtures/execplan_v2/review-doc.valid.md\n",
    )

  let artifact =
    run_shell(
      "env SCHERZO_WORKSPACE_DRIVER= SCHERZO_EXECPLAN_OFFLINE_PUBLISH=1 SCHERZO_EXECPLAN_FIXED_TIME=2026-05-15T00:00:00Z SCHERZO_ISSUE_IDENTIFIER=LIV-900 SCHERZO_ISSUE_TITLE="
      <> shell_quote("Offline publish fixture")
      <> " SCHERZO_ISSUE_URL=https://linear.app/living-systems/issue/LIV-900/offline-publish-fixture .scherzo/workflows/scripts/scherzo-execplan publish-review-doc --review-doc-path-file "
      <> shell_quote(path_file)
      <> " --publish-context "
      <> shell_quote(context_path),
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert string.contains(artifact.stdout, "PUBLISH_REVIEW_DOC_STATUS=published")
  let assert Ok(context) = simplifile.read(context_path)
  assert string.contains(
    context,
    "\"artifact_type\": \"execplan_v2_publish_context\"",
  )
  assert string.contains(context, "\"identifier\": \"LIV-900\"")
  assert string.contains(
    context,
    "\"path\": \"test/fixtures/execplan_v2/review-doc.valid.md\"",
  )
  assert string.contains(context, "\"status\": \"published\"")
}

pub fn publish_review_doc_revision_targets_existing_pr_test() {
  let dir = "test/tmp/execplan-v2-revision-publish-target"
  reset_dir(dir)
  let path_file = dir <> "/review.path"
  let context_path = dir <> "/publish-context.json"
  let previous_bundle =
    mutated_bundle(
      dir <> "/previous",
      each: "  \"review_doc\": {\n    \"bytes\": 1767,\n    \"path\": \"test/fixtures/execplan_v2/review-doc.valid.md\",\n    \"sha256\": \"64288f367d31d10a48decbb7f5b19ec4975e1a3a2991be2a4bc1007d8a61dcf4\"\n  },",
      with: "  \"review_doc\": {\n    \"bytes\": 1767,\n    \"path\": \"test/fixtures/execplan_v2/review-doc.valid.md\",\n    \"sha256\": \"0000000000000000000000000000000000000000000000000000000000000000\"\n  },",
    )
  let driver = dir <> "/workspace-driver"
  let log = dir <> "/workspace-driver.log"
  let assert Ok(Nil) =
    simplifile.write(
      path_file,
      "test/fixtures/execplan_v2/review-doc.valid.md\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      driver,
      "#!/bin/sh\n"
        <> "printf '%s\\n' \"$*\" >> "
        <> shell_quote(log)
        <> "\n"
        <> "if [ \"$1\" = changed-files ]; then printf '%s\\n' '{\"version\":1,\"files\":[{\"path\":\"test/fixtures/execplan_v2/review-doc.valid.md\",\"status\":\"modified\"}]}'; exit 0; fi\n"
        <> "if [ \"$1\" = publish-change ]; then printf '%s\\n' '{\"version\":1,\"status\":\"updated\",\"url\":\"https://github.com/living-systems/scherzo/pull/314\",\"branch\":\"execplan/liv-314\",\"base_ref\":\"main\",\"base_revision\":\"main\",\"head_revision\":\"abcdef123456\",\"change_id\":\"chg\",\"created\":false,\"updated\":true}'; exit 0; fi\n"
        <> "echo unexpected driver command >&2\n"
        <> "exit 1\n",
    )
  let chmod = run_shell("chmod +x " <> shell_quote(driver))
  assert chmod.status == step_artifact.StepSucceeded

  let artifact =
    run_shell(
      "env SCHERZO_WORKSPACE_DRIVER="
      <> shell_quote(driver)
      <> " SCHERZO_EXECPLAN_FIXED_TIME=2026-05-15T00:00:00Z .scherzo/workflows/scripts/scherzo-execplan publish-review-doc --review-doc-path-file "
      <> shell_quote(path_file)
      <> " --publish-context "
      <> shell_quote(context_path)
      <> " --previous-bundle "
      <> shell_quote(previous_bundle)
      <> " --skip-if-unchanged",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert string.contains(artifact.stdout, "PUBLISH_REVIEW_DOC_STATUS=published")
  let assert Ok(driver_log) = simplifile.read(log)
  assert string.contains(driver_log, "changed-files --json")
  assert string.contains(
    driver_log,
    "publish-change --kind execplan-revision --title-file tmp/execplan-pr-title.txt --body-file tmp/execplan-pr-body.md --branch-prefix execplan/liv-314-fixture-v2-execplan-bundle --base main --target-branch execplan/liv-314 --target-pr 314 --allow-no-changes true --json",
  )
  let assert Ok(context) = simplifile.read(context_path)
  assert string.contains(
    context,
    "\"url\": \"https://github.com/living-systems/scherzo/pull/314\"",
  )
  assert string.contains(context, "\"branch\": \"execplan/liv-314\"")
  assert string.contains(context, "\"head_revision\": \"abcdef123456\"")
}

pub fn publish_review_doc_prefers_pack_source_issue_for_pr_title_test() {
  let dir = "test/tmp/execplan-v2-publish-pack-source"
  reset_dir(dir)
  let path_file = dir <> "/review.path"
  let context_path = dir <> "/publish-context.json"
  let output = dir <> "/bundle.json"
  let title_file = "tmp/execplan-pr-title.txt"
  let _ = simplifile.delete(title_file)
  let assert Ok(Nil) =
    simplifile.write(
      path_file,
      "test/fixtures/execplan_v2/review-doc.valid.md\n",
    )

  let artifact =
    run_shell(
      "env SCHERZO_WORKSPACE_DRIVER= SCHERZO_EXECPLAN_OFFLINE_PUBLISH=1 SCHERZO_EXECPLAN_FIXED_TIME=2026-05-15T00:00:00Z SCHERZO_ISSUE_IDENTIFIER=LIV-314 .scherzo/workflows/scripts/scherzo-execplan publish-review-doc --review-doc-path-file "
      <> shell_quote(path_file)
      <> " --publish-context "
      <> shell_quote(context_path)
      <> " --pack test/fixtures/execplan_v2/implementation-pack.valid.json",
    )

  assert artifact.status == step_artifact.StepSucceeded
  let assert Ok(title) = simplifile.read(title_file)
  assert string.contains(title, "ExecPlan: LIV-314 Fixture v2 ExecPlan bundle")
  assert !string.contains(title, "Untitled source task")
  let assert Ok(context) = simplifile.read(context_path)
  assert string.contains(context, "\"identifier\": \"LIV-314\"")
  assert string.contains(context, "\"title\": \"Fixture v2 ExecPlan bundle\"")
  assert string.contains(
    context,
    "\"url\": \"https://linear.app/living-systems/issue/LIV-314/fixture-v2-execplan-bundle\"",
  )

  let bundle_artifact =
    run_shell(
      "env SCHERZO_EXECPLAN_OFFLINE_LINEAR=1 SCHERZO_RUN_ID=run-publish-pack-source .scherzo/workflows/scripts/scherzo-execplan materialize-bundle --review-doc-path-file "
      <> shell_quote(path_file)
      <> " --pack test/fixtures/execplan_v2/implementation-pack.valid.json --publish-context "
      <> shell_quote(context_path)
      <> " --output "
      <> shell_quote(output),
    )
  assert bundle_artifact.status == step_artifact.StepSucceeded
  let assert Ok(bundle) = simplifile.read(output)
  assert string.contains(bundle, "\"head_revision\": \"offline-head\"")
}

pub fn publish_review_doc_rejects_pack_review_doc_hash_mismatch_test() {
  let dir = "test/tmp/execplan-v2-publish-stale-pack"
  reset_dir(dir)
  let path_file = dir <> "/review.path"
  let context_path = dir <> "/publish-context.json"
  let pack_path =
    mutated_pack(
      dir <> "/pack",
      each: "\"review_doc_sha256\": \"64288f367d31d10a48decbb7f5b19ec4975e1a3a2991be2a4bc1007d8a61dcf4\"",
      with: "\"review_doc_sha256\": \"0000000000000000000000000000000000000000000000000000000000000000\"",
    )
  let assert Ok(Nil) =
    simplifile.write(
      path_file,
      "test/fixtures/execplan_v2/review-doc.valid.md\n",
    )

  let artifact =
    run_shell(
      "env SCHERZO_WORKSPACE_DRIVER= SCHERZO_EXECPLAN_OFFLINE_PUBLISH=1 .scherzo/workflows/scripts/scherzo-execplan publish-review-doc --review-doc-path-file "
      <> shell_quote(path_file)
      <> " --publish-context "
      <> shell_quote(context_path)
      <> " --pack "
      <> shell_quote(pack_path),
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(2)
  assert string.contains(
    artifact.stderr,
    "SCHERZO_FAILURE_CODE=execplan_v2_stale_implementation_pack",
  )
  let assert Error(_) = simplifile.read(context_path)
}

pub fn publish_review_doc_rejects_pack_source_issue_identifier_mismatch_test() {
  let dir = "test/tmp/execplan-v2-publish-pack-source-mismatch"
  reset_dir(dir)
  let path_file = dir <> "/review.path"
  let context_path = dir <> "/publish-context.json"
  let assert Ok(Nil) =
    simplifile.write(
      path_file,
      "test/fixtures/execplan_v2/review-doc.valid.md\n",
    )

  let artifact =
    run_shell(
      "env SCHERZO_WORKSPACE_DRIVER= SCHERZO_EXECPLAN_OFFLINE_PUBLISH=1 SCHERZO_ISSUE_IDENTIFIER=LIV-999 .scherzo/workflows/scripts/scherzo-execplan publish-review-doc --review-doc-path-file "
      <> shell_quote(path_file)
      <> " --publish-context "
      <> shell_quote(context_path)
      <> " --pack test/fixtures/execplan_v2/implementation-pack.valid.json",
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(2)
  assert string.contains(
    artifact.stderr,
    "SCHERZO_FAILURE_CODE=execplan_v2_source_issue_mismatch",
  )
  let assert Error(_) = simplifile.read(context_path)
}

pub fn materialize_bundle_prefers_pack_source_issue_over_publish_context_test() {
  let dir = "test/tmp/execplan-v2-materialize-pack-source"
  reset_dir(dir)
  let path_file = dir <> "/review.path"
  let context_path = dir <> "/publish-context.json"
  let output = dir <> "/bundle.json"
  let assert Ok(Nil) =
    simplifile.write(
      path_file,
      "test/fixtures/execplan_v2/review-doc.valid.md\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      context_path,
      "{\n"
        <> "  \"artifact_type\": \"execplan_v2_publish_context\",\n"
        <> "  \"source_issue\": {\"identifier\": \"LIV-314\", \"title\": \"Untitled source task\", \"url\": \"https://linear.app/living-systems/issue/LIV-314\"},\n"
        <> "  \"pr\": {\"url\": \"https://github.com/living-systems/scherzo/pull/314\", \"branch\": \"execplan-v2/liv-314\"},\n"
        <> "  \"review_surface\": {\"status\": \"published\", \"source_bundle_ref\": null}\n"
        <> "}\n",
    )

  let artifact =
    run_shell(
      "env SCHERZO_EXECPLAN_OFFLINE_LINEAR=1 SCHERZO_RUN_ID=run-pack-source .scherzo/workflows/scripts/scherzo-execplan materialize-bundle --review-doc-path-file "
      <> shell_quote(path_file)
      <> " --pack test/fixtures/execplan_v2/implementation-pack.valid.json --publish-context "
      <> shell_quote(context_path)
      <> " --output "
      <> shell_quote(output),
    )

  assert artifact.status == step_artifact.StepSucceeded
  let assert Ok(bundle) = simplifile.read(output)
  assert string.contains(
    bundle,
    "\"bundle_id\": \"bundle-liv-314-run-pack-source\"",
  )
  assert string.contains(bundle, "\"identifier\": \"LIV-314\"")
  assert string.contains(bundle, "\"title\": \"Fixture v2 ExecPlan bundle\"")
  assert string.contains(
    bundle,
    "\"url\": \"https://linear.app/living-systems/issue/LIV-314/fixture-v2-execplan-bundle\"",
  )
  assert !string.contains(bundle, "Untitled source task")
}

pub fn materialize_bundle_rejects_publish_context_source_issue_identifier_mismatch_test() {
  let dir = "test/tmp/execplan-v2-materialize-pack-source-mismatch"
  reset_dir(dir)
  let path_file = dir <> "/review.path"
  let context_path = dir <> "/publish-context.json"
  let output = dir <> "/bundle.json"
  let assert Ok(Nil) =
    simplifile.write(
      path_file,
      "test/fixtures/execplan_v2/review-doc.valid.md\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      context_path,
      "{\n"
        <> "  \"artifact_type\": \"execplan_v2_publish_context\",\n"
        <> "  \"source_issue\": {\"identifier\": \"LIV-999\", \"title\": \"Other source task\", \"url\": \"https://linear.app/living-systems/issue/LIV-999\"},\n"
        <> "  \"pr\": {\"url\": \"https://github.com/living-systems/scherzo/pull/314\", \"branch\": \"execplan-v2/liv-314\"},\n"
        <> "  \"review_surface\": {\"status\": \"published\", \"source_bundle_ref\": null}\n"
        <> "}\n",
    )

  let artifact =
    run_shell(
      "env SCHERZO_EXECPLAN_OFFLINE_LINEAR=1 SCHERZO_RUN_ID=run-pack-source .scherzo/workflows/scripts/scherzo-execplan materialize-bundle --review-doc-path-file "
      <> shell_quote(path_file)
      <> " --pack test/fixtures/execplan_v2/implementation-pack.valid.json --publish-context "
      <> shell_quote(context_path)
      <> " --output "
      <> shell_quote(output),
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(2)
  assert string.contains(
    artifact.stderr,
    "SCHERZO_FAILURE_CODE=execplan_v2_source_issue_mismatch",
  )
  let assert Error(_) = simplifile.read(output)
}

pub fn materialize_bundle_creates_handoff_with_non_json_linear_create_test() {
  let dir = "test/tmp/execplan-online-linear-create"
  reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  let log = dir <> "/linear.log"
  let update_desc = dir <> "/updated-description.md"
  let linear = dir <> "/bin/linear"
  let assert Ok(Nil) =
    simplifile.write(
      linear,
      "#!/bin/sh\n"
        <> "printf '%s\\n' \"$*\" >> "
        <> shell_quote(log)
        <> "\n"
        <> "if [ \"$1 $2\" = 'issue query' ]; then printf '%s\\n' '{\"nodes\":[]}'; exit 0; fi\n"
        <> "if [ \"$1 $2 $3\" = 'issue view LIV-314' ]; then printf '%s\\n' '{\"identifier\":\"LIV-314\",\"url\":\"https://linear.app/living-systems/issue/LIV-314/fixture-v2-execplan-bundle\",\"project\":{\"name\":\"Scherzo Core\"}}'; exit 0; fi\n"
        <> "if [ \"$1 $2\" = 'issue create' ]; then\n"
        <> "  for arg in \"$@\"; do if [ \"$arg\" = --json ]; then echo 'unexpected --json' >&2; exit 2; fi; done\n"
        <> "  printf '%s\\n' 'Creating issue in LIV' '' 'https://linear.app/living-systems/issue/LIV-315/implement-fixture-v2-execplan-bundle'\n"
        <> "  exit 0\n"
        <> "fi\n"
        <> "if [ \"$1 $2 $3\" = 'issue view LIV-315' ]; then printf '%s\\n' '{\"identifier\":\"LIV-315\",\"url\":\"https://linear.app/living-systems/issue/LIV-315/implement-fixture-v2-execplan-bundle\"}'; exit 0; fi\n"
        <> "if [ \"$1 $2 $3\" = 'issue update LIV-315' ]; then\n"
        <> "  desc=''\n"
        <> "  prev=''\n"
        <> "  for arg in \"$@\"; do if [ \"$prev\" = --description-file ]; then desc=$arg; fi; prev=$arg; done\n"
        <> "  if [ -z \"$desc\" ]; then echo 'missing description file' >&2; exit 2; fi\n"
        <> "  cp \"$desc\" "
        <> shell_quote(update_desc)
        <> "\n"
        <> "  grep -Eq '^Bundle sha256: [a-f0-9]{64}$' \"$desc\" || { echo 'missing final bundle sha' >&2; exit 3; }\n"
        <> "  grep -q '^Bundle sha256: pending$' \"$desc\" && { echo 'pending bundle sha' >&2; exit 4; }\n"
        <> "  exit 0\n"
        <> "fi\n"
        <> "if [ \"$1 $2 $3\" = 'issue comment add' ]; then exit 0; fi\n"
        <> "exit 1\n",
    )
  let chmod = run_shell("chmod +x " <> shell_quote(linear))
  assert chmod.status == step_artifact.StepSucceeded

  let path_file = dir <> "/review.path"
  let context_path = dir <> "/publish-context.json"
  let output = dir <> "/bundle.json"
  let assert Ok(Nil) =
    simplifile.write(
      path_file,
      "test/fixtures/execplan_v2/review-doc.valid.md\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      context_path,
      "{\n"
        <> "  \"artifact_type\": \"execplan_v2_publish_context\",\n"
        <> "  \"source_issue\": {\"identifier\": \"LIV-314\", \"title\": \"Fixture v2 ExecPlan bundle\", \"url\": \"https://linear.app/living-systems/issue/LIV-314/fixture-v2-execplan-bundle\"},\n"
        <> "  \"pr\": {\"url\": \"https://github.com/living-systems/scherzo/pull/314\", \"branch\": \"execplan/liv-314\"},\n"
        <> "  \"review_surface\": {\"status\": \"published\", \"source_bundle_ref\": null}\n"
        <> "}\n",
    )

  let artifact =
    run_shell(
      "env PATH="
      <> shell_quote(dir <> "/bin")
      <> ":$PATH SCHERZO_RUN_ID=run-online .scherzo/workflows/scripts/scherzo-execplan materialize-bundle --review-doc-path-file "
      <> shell_quote(path_file)
      <> " --pack test/fixtures/execplan_v2/implementation-pack.valid.json --publish-context "
      <> shell_quote(context_path)
      <> " --output "
      <> shell_quote(output),
    )

  assert artifact.status == step_artifact.StepSucceeded
  let assert Ok(bundle) = simplifile.read(output)
  assert string.contains(bundle, "\"issue_identifier\": \"LIV-315\"")
  assert string.contains(
    bundle,
    "\"issue_url\": \"https://linear.app/living-systems/issue/LIV-315/implement-fixture-v2-execplan-bundle\"",
  )
  let assert Ok(linear_log) = simplifile.read(log)
  assert string.contains(linear_log, "issue create")
  assert string.contains(linear_log, "--project Scherzo Core")
  assert string.contains(linear_log, "issue update LIV-315")
  let assert Ok(updated_description) = simplifile.read(update_desc)
  assert string.contains(
    updated_description,
    "Bundle ref: runs/run-online/outputs/exec_plan_bundle.json",
  )
  assert !string.contains(updated_description, "Bundle sha256: pending")
}

pub fn materialize_revision_reuses_unchanged_review_surface_test() {
  let dir = "test/tmp/execplan-unchanged-revision"
  reset_dir(dir)
  let path_file = dir <> "/review.path"
  let context_path = dir <> "/publish-context.json"
  let output = dir <> "/bundle.json"
  let assert Ok(Nil) =
    simplifile.write(
      path_file,
      "test/fixtures/execplan_v2/review-doc.valid.md\n",
    )

  let publish =
    run_shell(
      "env SCHERZO_WORKSPACE_DRIVER= SCHERZO_EXECPLAN_OFFLINE_PUBLISH=1 SCHERZO_EXECPLAN_FIXED_TIME=2026-05-15T00:00:00Z .scherzo/workflows/scripts/scherzo-execplan publish-review-doc --review-doc-path-file "
      <> shell_quote(path_file)
      <> " --publish-context "
      <> shell_quote(context_path)
      <> " --previous-bundle test/fixtures/execplan_v2/exec-plan-bundle.valid.json --skip-if-unchanged",
    )
  assert publish.status == step_artifact.StepSucceeded
  assert string.contains(publish.stdout, "PUBLISH_REVIEW_DOC_STATUS=reused")

  let revision =
    run_shell(
      "env SCHERZO_EXECPLAN_OFFLINE_LINEAR=1 SCHERZO_RUN_ID=run-revision .scherzo/workflows/scripts/scherzo-execplan materialize-revision --previous-bundle test/fixtures/execplan_v2/exec-plan-bundle.valid.json --review-doc-path-file "
      <> shell_quote(path_file)
      <> " --pack test/fixtures/execplan_v2/implementation-pack.valid.json --publish-context "
      <> shell_quote(context_path)
      <> " --status auto --output "
      <> shell_quote(output),
    )

  assert revision.status == step_artifact.StepSucceeded
  let assert Ok(bundle) = simplifile.read(output)
  assert string.contains(bundle, "\"status\": \"unchanged\"")
  assert string.contains(
    bundle,
    "\"source_bundle_ref\": \"runs/run-1/outputs/exec_plan_bundle.json\"",
  )
  assert string.contains(
    bundle,
    "\"ref\": \"runs/run-1/outputs/exec_plan_bundle.json\"",
  )
  assert string.contains(
    bundle,
    "\"sha256\": \"e4117164704e943de716797a83f98cd4927833dc0f3b4a179c78c657b25334ec\"",
  )
  assert string.contains(bundle, "\"head_revision\": \"reused\"")
}

pub fn materialize_revision_prefers_pack_source_issue_title_and_url_test() {
  let dir = "test/tmp/execplan-v2-revision-pack-source"
  reset_dir(dir)
  let previous_bundle =
    mutated_bundle(
      dir <> "/previous",
      each: "  \"source_issue\": {\n    \"identifier\": \"LIV-314\",\n    \"title\": \"Fixture v2 ExecPlan bundle\",\n    \"url\": \"https://linear.app/living-systems/issue/LIV-314/fixture-v2-execplan-bundle\"\n  },",
      with: "  \"source_issue\": {\n    \"identifier\": \"LIV-314\",\n    \"title\": \"Untitled source task\",\n    \"url\": \"https://linear.app/living-systems/issue/LIV-314\"\n  },",
    )
  let path_file = dir <> "/review.path"
  let context_path = dir <> "/publish-context.json"
  let output = dir <> "/bundle.json"
  let assert Ok(Nil) =
    simplifile.write(
      path_file,
      "test/fixtures/execplan_v2/review-doc.valid.md\n",
    )

  let publish =
    run_shell(
      "env SCHERZO_WORKSPACE_DRIVER= SCHERZO_EXECPLAN_OFFLINE_PUBLISH=1 SCHERZO_EXECPLAN_FIXED_TIME=2026-05-15T00:00:00Z .scherzo/workflows/scripts/scherzo-execplan publish-review-doc --review-doc-path-file "
      <> shell_quote(path_file)
      <> " --publish-context "
      <> shell_quote(context_path)
      <> " --pack test/fixtures/execplan_v2/implementation-pack.valid.json --previous-bundle "
      <> shell_quote(previous_bundle)
      <> " --skip-if-unchanged",
    )
  assert publish.status == step_artifact.StepSucceeded
  assert string.contains(publish.stdout, "PUBLISH_REVIEW_DOC_STATUS=reused")
  let assert Ok(context) = simplifile.read(context_path)
  assert string.contains(context, "\"identifier\": \"LIV-314\"")
  assert string.contains(context, "\"title\": \"Fixture v2 ExecPlan bundle\"")
  assert string.contains(
    context,
    "\"url\": \"https://linear.app/living-systems/issue/LIV-314/fixture-v2-execplan-bundle\"",
  )
  assert !string.contains(context, "Untitled source task")

  let revision =
    run_shell(
      "env SCHERZO_EXECPLAN_OFFLINE_LINEAR=1 SCHERZO_RUN_ID=run-revision-pack-source .scherzo/workflows/scripts/scherzo-execplan materialize-revision --previous-bundle "
      <> shell_quote(previous_bundle)
      <> " --review-doc-path-file "
      <> shell_quote(path_file)
      <> " --pack test/fixtures/execplan_v2/implementation-pack.valid.json --publish-context "
      <> shell_quote(context_path)
      <> " --status auto --output "
      <> shell_quote(output),
    )

  assert revision.status == step_artifact.StepSucceeded
  let assert Ok(bundle) = simplifile.read(output)
  assert string.contains(bundle, "\"status\": \"unchanged\"")
  assert string.contains(
    bundle,
    "\"bundle_id\": \"bundle-liv-314-run-revision-pack-source\"",
  )
  assert string.contains(bundle, "\"identifier\": \"LIV-314\"")
  assert string.contains(bundle, "\"title\": \"Fixture v2 ExecPlan bundle\"")
  assert string.contains(
    bundle,
    "\"url\": \"https://linear.app/living-systems/issue/LIV-314/fixture-v2-execplan-bundle\"",
  )
  assert !string.contains(bundle, "Untitled source task")
}

pub fn materialize_revision_updates_existing_handoff_issue_test() {
  let dir = "test/tmp/execplan-v2-revision-handoff-update"
  reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  let log = dir <> "/linear.log"
  let update_desc = dir <> "/updated-description.md"
  let linear = dir <> "/bin/linear"
  let assert Ok(Nil) =
    simplifile.write(
      linear,
      "#!/bin/sh\n"
        <> "printf '%s\\n' \"$*\" >> "
        <> shell_quote(log)
        <> "\n"
        <> "if [ \"$1 $2 $3\" = 'issue update LIV-315' ]; then\n"
        <> "  desc=''\n"
        <> "  prev=''\n"
        <> "  for arg in \"$@\"; do if [ \"$prev\" = --description-file ]; then desc=$arg; fi; prev=$arg; done\n"
        <> "  if [ -z \"$desc\" ]; then echo 'missing description file' >&2; exit 2; fi\n"
        <> "  cp \"$desc\" "
        <> shell_quote(update_desc)
        <> "\n"
        <> "  grep -q '^Bundle ref: runs/run-revision-update/outputs/exec_plan_bundle.json$' \"$desc\" || { echo 'missing revised bundle ref' >&2; exit 3; }\n"
        <> "  grep -Eq '^Bundle sha256: [a-f0-9]{64}$' \"$desc\" || { echo 'missing revised bundle sha' >&2; exit 4; }\n"
        <> "  grep -q '^Bundle sha256: pending$' \"$desc\" && { echo 'pending bundle sha' >&2; exit 5; }\n"
        <> "  exit 0\n"
        <> "fi\n"
        <> "if [ \"$1 $2 $3 $4\" = 'issue comment add LIV-315' ]; then exit 0; fi\n"
        <> "exit 1\n",
    )
  let chmod = run_shell("chmod +x " <> shell_quote(linear))
  assert chmod.status == step_artifact.StepSucceeded

  let path_file = dir <> "/review.path"
  let context_path = dir <> "/publish-context.json"
  let output = dir <> "/bundle.json"
  let assert Ok(Nil) =
    simplifile.write(
      path_file,
      "test/fixtures/execplan_v2/review-doc.valid.md\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      context_path,
      "{\n"
        <> "  \"artifact_type\": \"execplan_v2_publish_context\",\n"
        <> "  \"source_issue\": {\"identifier\": \"LIV-314\", \"title\": \"Fixture v2 ExecPlan bundle\", \"url\": \"https://linear.app/living-systems/issue/LIV-314/fixture-v2-execplan-bundle\"},\n"
        <> "  \"pr\": {\"url\": \"https://github.com/living-systems/scherzo/pull/314\", \"branch\": \"execplan/liv-314\", \"head_revision\": \"reused\"},\n"
        <> "  \"review_surface\": {\"status\": \"reused\", \"source_bundle_ref\": \"runs/run-1/outputs/exec_plan_bundle.json\", \"head_revision\": \"reused\"}\n"
        <> "}\n",
    )

  let artifact =
    run_shell(
      "env PATH="
      <> shell_quote(dir <> "/bin")
      <> ":$PATH SCHERZO_RUN_ID=run-revision-update .scherzo/workflows/scripts/scherzo-execplan materialize-revision --previous-bundle test/fixtures/execplan_v2/exec-plan-bundle.valid.json --review-doc-path-file "
      <> shell_quote(path_file)
      <> " --pack test/fixtures/execplan_v2/implementation-pack.valid.json --publish-context "
      <> shell_quote(context_path)
      <> " --status auto --output "
      <> shell_quote(output),
    )

  assert artifact.status == step_artifact.StepSucceeded
  let assert Ok(linear_log) = simplifile.read(log)
  assert string.contains(linear_log, "issue update LIV-315")
  assert string.contains(linear_log, "issue comment add LIV-315")
  let assert Ok(updated_description) = simplifile.read(update_desc)
  assert string.contains(
    updated_description,
    "Bundle ref: runs/run-revision-update/outputs/exec_plan_bundle.json",
  )
  assert !string.contains(
    updated_description,
    "Bundle ref: runs/run-1/outputs/exec_plan_bundle.json",
  )
}

pub fn materialize_code_change_bundle_emits_retained_refs_test() {
  let dir = "test/tmp/execplan-code-change"
  reset_dir(dir)
  let artifact_root = dir <> "/artifacts/runs/run-2"
  let run_root = dir <> "/run-root"
  let assert Ok(Nil) = simplifile.create_directory_all(artifact_root)
  let assert Ok(Nil) =
    simplifile.create_directory_all(
      run_root <> "/artifacts/review/final_dispositions",
    )
  let assert Ok(Nil) = simplifile.create_directory_all("tmp")
  let assert Ok(Nil) =
    simplifile.write(
      "tmp/scherzo-implementation-publish.json",
      "{\n"
        <> "  \"branch\": \"impl/liv-315\",\n"
        <> "  \"changed_files\": [\"src/example.gleam\"],\n"
        <> "  \"driver_head_revision\": \"head-rev\",\n"
        <> "  \"pr_url\": \"https://github.com/living-systems/scherzo/pull/315\",\n"
        <> "  \"publish_base_revision\": \"base-rev\"\n"
        <> "}\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      "tmp/scherzo-implementation-validation.json",
      "{\"status\":\"passed\"}\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      "tmp/scherzo-plan-completion-verdict.json",
      "{\"verdict\":\"pass\"}\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      run_root <> "/artifacts/review/final_dispositions/final-review.v1.json",
      "{\"artifact_type\":\"final_review\"}\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      run_root <> "/artifacts/review/final_dispositions/final-review.md",
      "# Final review\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      run_root
        <> "/artifacts/review/final_dispositions/review-finding-dispositions.v1.json",
      "{\"artifact_type\":\"review_finding_dispositions\"}\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      run_root
        <> "/artifacts/review/final_dispositions/review-finding-dispositions.md",
      "## Finding dispositions\n",
    )
  let output = dir <> "/code-change-bundle.json"

  let artifact =
    run_shell(
      "env SCHERZO_RUN_ID=run-2 SCHERZO_RUN_ARTIFACT_DIR="
      <> shell_quote(artifact_root)
      <> " SCHERZO_RUN_ROOT="
      <> shell_quote(run_root)
      <> " SCHERZO_EXECPLAN_DIFF_PATH=test/fixtures/execplan_v2/artifacts/runs/run-2/execplan/code-change/diff.patch .scherzo/workflows/scripts/scherzo-execplan materialize-code-change-bundle --bundle test/fixtures/execplan_v2/exec-plan-bundle.valid.json --output "
      <> shell_quote(output),
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  let assert Ok(bundle) = simplifile.read(output)
  assert string.contains(bundle, "\"artifact_type\": \"code_change_bundle\"")
  assert string.contains(bundle, "runs/run-2/execplan/code-change/diff.patch")
  assert string.contains(bundle, "\"verdict\": \"complete\"")
  assert string.contains(bundle, "\"name\": \"final-review\"")
  assert string.contains(bundle, "\"name\": \"final-review-markdown\"")
  assert string.contains(bundle, "\"name\": \"review-finding-dispositions\"")
  assert string.contains(
    bundle,
    "\"name\": \"review-finding-dispositions-markdown\"",
  )
}
