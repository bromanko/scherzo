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
import support/test_helpers
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

fn assert_review_doc_section_failed(
  artifact: step_artifact.StepArtifact,
  diagnostic: String,
  section: String,
) -> Nil {
  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(2)
  assert string.contains(artifact.stderr, diagnostic)
  assert string.contains(artifact.stderr, section)
  assert string.contains(
    artifact.stderr,
    "Required review-doc sections are fail-closed",
  )
  assert string.contains(artifact.stderr, "No open questions.")
  Nil
}

fn assert_review_doc_missing_section_failed(
  artifact: step_artifact.StepArtifact,
  section: String,
) -> Nil {
  assert string.contains(
    artifact.stderr,
    "SCHERZO_FAILURE_CODE=execplan_review_doc_required_section_missing",
  )
  assert_review_doc_section_failed(
    artifact,
    "review doc missing required section",
    section,
  )
}

fn assert_review_doc_empty_section_failed(
  artifact: step_artifact.StepArtifact,
  section: String,
) -> Nil {
  assert string.contains(
    artifact.stderr,
    "SCHERZO_FAILURE_CODE=execplan_review_doc_required_section_empty",
  )
  assert_review_doc_section_failed(
    artifact,
    "review doc required section has no meaningful content",
    section,
  )
}

fn tmp_repo_path(path: String) -> String {
  "../../../" <> path
}

fn write_valid_review_doc(path: String) -> Nil {
  let assert Ok(valid) =
    simplifile.read("test/fixtures/execplan_v2/review-doc.valid.md")
  let assert Ok(parent) = scherzo_path.dirname(path)
  let assert Ok(Nil) = simplifile.create_directory_all(parent)
  let assert Ok(Nil) = simplifile.write(path, valid)
  Nil
}

fn write_fake_commit_stack_driver(path: String, review_path: String) -> Nil {
  let assert Ok(Nil) =
    simplifile.write(
      path,
      "#!/bin/sh\n"
        <> "if [ \"$1\" = changed-files ]; then\n"
        <> "  printf '%s\\n' '{\"version\":1,\"files\":[{\"path\":\""
        <> review_path
        <> "\",\"status\":\"modified\"}]}'\n"
        <> "  exit 0\n"
        <> "fi\n"
        <> "if [ \"$1\" = refresh-base ]; then\n"
        <> "  printf '%s\\n' \"$5\" > .fake-base-target\n"
        <> "  printf '%s\\n' '{\"version\":1,\"status\":\"fresh\"}'\n"
        <> "  exit 0\n"
        <> "fi\n"
        <> "printf 'unexpected driver command: %s\\n' \"$*\" >&2\n"
        <> "exit 2\n",
    )
  Nil
}

fn write_fake_commit_stack_driver_with_extra_path(
  path: String,
  review_path: String,
  extra_path: String,
) -> Nil {
  let assert Ok(Nil) =
    simplifile.write(
      path,
      "#!/bin/sh\n"
        <> "if [ \"$1\" = changed-files ]; then\n"
        <> "  printf '%s\\n' '{\"version\":1,\"files\":[{\"path\":\""
        <> review_path
        <> "\",\"status\":\"modified\"},{\"path\":\""
        <> extra_path
        <> "\",\"status\":\"modified\"}]}'\n"
        <> "  exit 0\n"
        <> "fi\n"
        <> "printf 'unexpected driver command: %s\\n' \"$*\" >&2\n"
        <> "exit 2\n",
    )
  Nil
}

fn write_fake_commit_stack_jj(path: String) -> Nil {
  let assert Ok(Nil) =
    simplifile.write(
      path,
      "#!/bin/sh\n"
        <> "printf '%s\\n' \"$*\" >> jj.log\n"
        <> "if [ \"$1\" = describe ]; then touch .fake-described; exit 0; fi\n"
        <> "if [ \"$1\" = log ]; then\n"
        <> "  rev=\n"
        <> "  template=\n"
        <> "  prev=\n"
        <> "  for arg in \"$@\"; do\n"
        <> "    if [ \"$prev\" = -r ]; then rev=$arg; fi\n"
        <> "    if [ \"$prev\" = -T ]; then template=$arg; fi\n"
        <> "    prev=$arg\n"
        <> "  done\n"
        <> "  case \"$rev\" in\n"
        <> "    @-) case \"$template\" in *commit_id*) if [ -f .fake-base-target ] && grep -Eq '^[0-9a-f]{40}$' .fake-base-target; then cat .fake-base-target; else echo 2222222222222222222222222222222222222222; fi;; *) echo parentdescription;; esac; exit 0;;\n"
        <> "    @) case \"$template\" in *description*) if [ \"${SCHERZO_FAKE_EMPTY_DESCRIPTION:-}\" = 1 ] && [ ! -f .fake-described ]; then printf '\\n'; else echo currentdescription; fi;; *commit_id*) if [ -f .fake-described ]; then echo 5555555555555555555555555555555555555555; else echo 3333333333333333333333333333333333333333; fi;; *) echo currentcommit;; esac; exit 0;;\n"
        <> "  esac\n"
        <> "fi\n"
        <> "if [ \"$1\" = debug ] && [ \"$2\" = object ] && [ \"$3\" = commit ]; then\n"
        <> "  printf '%s\\n' 'Commit {' '  root_tree: Resolved(' '    TreeId(' '      \"4444444444444444444444444444444444444444\",' '    ),' '  ),' '}'\n"
        <> "  exit 0\n"
        <> "fi\n"
        <> "exit 1\n",
    )
  Nil
}

fn write_fake_commit_stack_git(path: String) -> Nil {
  let assert Ok(Nil) =
    simplifile.write(
      path,
      "#!/bin/sh\n"
        <> "if [ \"$1\" = update-ref ]; then exit 0; fi\n"
        <> "if [ \"$1 $2\" = 'bundle create' ]; then mkdir -p \"$(dirname \"$3\")\"; printf 'fake bundle for %s\\n' \"$*\" > \"$3\"; exit 0; fi\n"
        <> "if [ \"$1 $2\" = 'bundle verify' ]; then test -s \"$3\"; exit $?; fi\n"
        <> "exit 1\n",
    )
  Nil
}

fn write_fake_commit_stack_git_with_oversized_bundle(path: String) -> Nil {
  let assert Ok(Nil) =
    simplifile.write(
      path,
      "#!/bin/sh\n"
        <> "if [ \"$1\" = update-ref ]; then exit 0; fi\n"
        <> "if [ \"$1 $2\" = 'bundle create' ]; then mkdir -p \"$(dirname \"$3\")\"; python3 -c 'import sys; open(sys.argv[1], \"wb\").truncate(104857601)' \"$3\"; exit 0; fi\n"
        <> "if [ \"$1 $2\" = 'bundle verify' ]; then test -e \"$3\"; exit $?; fi\n"
        <> "exit 1\n",
    )
  Nil
}

fn mutated_bundle(dir: String, each old: String, with new: String) -> String {
  test_helpers.reset_dir(dir)
  let assert Ok(source) =
    simplifile.read("test/fixtures/execplan_v2/exec-plan-bundle.valid.json")
  let path = dir <> "/bundle.json"
  let assert Ok(Nil) =
    simplifile.write(path, string.replace(source, each: old, with: new))
  path
}

fn mutated_legacy_bundle(
  dir: String,
  each old: String,
  with new: String,
) -> String {
  test_helpers.reset_dir(dir)
  let assert Ok(source) =
    simplifile.read(
      "test/fixtures/execplan_v2/legacy/exec-plan-bundle.legacy.json",
    )
  let path = dir <> "/bundle.json"
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

fn write_revision_bundle_from_fixture_with_surface(
  dir: String,
  review_path: String,
  branch: String,
  head_revision: String,
  fixture_path: String,
) -> #(String, String) {
  let bundle_ref = "runs/run-prepare/outputs/exec_plan_bundle.json"
  let bundle_dir =
    dir <> "/repo/.scherzo-state/artifacts/runs/run-prepare/outputs"
  let assert Ok(Nil) = simplifile.create_directory_all(bundle_dir)
  let assert Ok(source) = simplifile.read(fixture_path)
  let with_path =
    string.replace(
      source,
      each: "test/fixtures/execplan_v2/review-doc.valid.md",
      with: review_path,
    )
  let head_revision_json = case head_revision {
    "" -> ""
    value -> "    \"head_revision\": \"" <> value <> "\",\n"
  }
  let with_branch =
    string.replace(
      with_path,
      each: "    \"branch\": \"execplan/liv-314\",\n",
      with: "    \"branch\": \"" <> branch <> "\",\n" <> head_revision_json,
    )
  let bundle_path = bundle_dir <> "/exec_plan_bundle.json"
  let assert Ok(Nil) = simplifile.write(bundle_path, with_branch)
  #(bundle_ref, hash.sha256_hex(with_branch))
}

fn write_revision_bundle_with_surface(
  dir: String,
  review_path: String,
  branch: String,
  head_revision: String,
) -> #(String, String) {
  write_revision_bundle_from_fixture_with_surface(
    dir,
    review_path,
    branch,
    head_revision,
    "test/fixtures/execplan_v2/exec-plan-bundle.valid.json",
  )
}

fn write_revision_legacy_bundle_with_surface(
  dir: String,
  review_path: String,
  branch: String,
  head_revision: String,
) -> #(String, String) {
  write_revision_bundle_from_fixture_with_surface(
    dir,
    review_path,
    branch,
    head_revision,
    "test/fixtures/execplan_v2/legacy/exec-plan-bundle.legacy.json",
  )
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

fn write_revision_legacy_bundle(
  dir: String,
  review_path: String,
) -> #(String, String) {
  write_revision_legacy_bundle_with_surface(
    dir,
    review_path,
    "execplan/liv-314-unmerged",
    "ca667773c9a6d31bb64676c103b3f1f14c3bcced",
  )
}

fn bundle_without_review_surface_publication(source: String) -> String {
  source
  |> string.replace(
    each: "    \"branch\": \"execplan/liv-314\",\n",
    with: "    \"branch\": null,\n",
  )
  |> string.replace(
    each: "    \"pr_url\": \"https://github.com/living-systems/scherzo/pull/314\",\n",
    with: "    \"pr_url\": null,\n",
  )
  |> string.replace(
    each: "    \"status\": \"published\"\n",
    with: "    \"status\": \"not_applicable\"\n",
  )
}

fn write_materialize_previous_bundle_with_surface_head(
  dir: String,
  head_revision: String,
) -> String {
  let assert Ok(source) =
    simplifile.read("test/fixtures/execplan_v2/exec-plan-bundle.valid.json")
  let previous_bundle = dir <> "/previous-bundle.json"
  let with_head =
    string.replace(
      source,
      each: "    \"branch\": \"execplan/liv-314\",\n",
      with: "    \"branch\": \"execplan/liv-314\",\n    \"head_revision\": \""
        <> head_revision
        <> "\",\n",
    )
  let assert Ok(Nil) = simplifile.write(previous_bundle, with_head)
  "previous-bundle.json"
}

fn write_revision_legacy_bundle_without_surface(
  dir: String,
  review_path: String,
) -> #(String, String) {
  let bundle_ref = "runs/run-prepare/outputs/exec_plan_bundle.json"
  let bundle_dir =
    dir <> "/repo/.scherzo-state/artifacts/runs/run-prepare/outputs"
  let assert Ok(Nil) = simplifile.create_directory_all(bundle_dir)
  let assert Ok(source) =
    simplifile.read(
      "test/fixtures/execplan_v2/legacy/exec-plan-bundle.legacy.json",
    )
  let with_path =
    string.replace(
      source,
      each: "test/fixtures/execplan_v2/review-doc.valid.md",
      with: review_path,
    )
  let without_surface = bundle_without_review_surface_publication(with_path)
  let bundle_path = bundle_dir <> "/exec_plan_bundle.json"
  let assert Ok(Nil) = simplifile.write(bundle_path, without_surface)
  #(bundle_ref, hash.sha256_hex(without_surface))
}

fn write_execplan_publication_manifest(
  root: String,
  run_id: String,
  status: String,
  branch: String,
  pr_url: String,
  head_revision: String,
) -> Nil {
  let manifest_dir =
    root
    <> "/.scherzo-state/artifacts/runs/"
    <> run_id
    <> "/publications/execplan_review_doc"
  let assert Ok(Nil) = simplifile.create_directory_all(manifest_dir)
  let manifest =
    "{\n"
    <> "  \"schema_version\": 1,\n"
    <> "  \"artifact_type\": \"scherzo.artifact_publication_manifest.v1\",\n"
    <> "  \"run_id\": \""
    <> run_id
    <> "\",\n"
    <> "  \"workflow_id\": \"execplan\",\n"
    <> "  \"publication_id\": \"execplan_review_doc\",\n"
    <> "  \"series_id\": \"series-1\",\n"
    <> "  \"version_id\": \"version-1\",\n"
    <> "  \"attempt_id\": \"version-1\",\n"
    <> "  \"status\": \""
    <> status
    <> "\",\n"
    <> "  \"publication_mode\": \"commit_stack\",\n"
    <> "  \"required\": true,\n"
    <> "  \"retryable\": false,\n"
    <> "  \"retry_execution_available\": true,\n"
    <> "  \"generated_at_ms\": 123,\n"
    <> "  \"branch\": \""
    <> branch
    <> "\",\n"
    <> "  \"commit_sha\": \""
    <> head_revision
    <> "\",\n"
    <> "  \"pr_url\": \""
    <> pr_url
    <> "\",\n"
    <> "  \"pr_number\": 314,\n"
    <> "  \"base_ref\": \"main\",\n"
    <> "  \"base_revision\": null,\n"
    <> "  \"head_revision\": \""
    <> head_revision
    <> "\",\n"
    <> "  \"change_id\": null,\n"
    <> "  \"selected_paths\": [],\n"
    <> "  \"changed_paths\": [],\n"
    <> "  \"removed_paths\": [],\n"
    <> "  \"dry_run_manifest\": null,\n"
    <> "  \"error\": null,\n"
    <> "  \"cleanup_diagnostics\": null\n"
    <> "}\n"
  let assert Ok(Nil) =
    simplifile.write(manifest_dir <> "/version-1.json", manifest)
  Nil
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
  let assert Ok(Nil) = simplifile.write(output_dir <> "/plan.md", review_doc)

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
      each: "bb23c8a36dc5e9b3d46d8062fd073dc6acf3db208138d2e4b14530d092aa0f40",
      with: pack_sha,
    )
  let bundle_text =
    string.replace(
      bundle_with_pack_sha,
      each: "\"bytes\": 2220,",
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
    <> "  \"artifact_type\": \"scherzo.exec_plan_bundle.v2\",\n"
    <> "  \"entries\": [\n"
    <> "    {\"name\": \"plan\", \"kind\": \"file\", \"artifact_type\": \"scherzo.exec_plan.v1\", \"ref\": \""
    <> plan_ref
    <> "\", \"sha256\": \""
    <> plan_sha
    <> "\", \"bytes\": "
    <> plan_bytes
    <> ", \"media_type\": \"text/markdown\"},\n"
    <> "    {\"name\": \"implementation_pack\", \"kind\": \"file\", \"artifact_type\": \"scherzo.implementation_pack.v2\", \"ref\": \"runs/"
    <> run_id
    <> "/outputs/implementation_pack.json\", \"sha256\": \""
    <> pack_sha
    <> "\", \"bytes\": "
    <> pack_bytes
    <> ", \"media_type\": \"application/json\"}\n"
    <> "  ],\n"
    <> "  \"kind\": \"artifact_set\",\n"
    <> "  \"media_type\": \"application/json\",\n"
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

fn write_exec_plan_bundle_input_manifest(
  artifact_workspace: String,
  run_id: String,
  bundle_ref: String,
  bundle_sha: String,
) -> Nil {
  let manifest_dir =
    artifact_workspace <> "/.scherzo-state/artifacts/runs/" <> run_id
  let assert Ok(Nil) = simplifile.create_directory_all(manifest_dir)
  let manifest =
    "{\n"
    <> "  \"schema_version\": 1,\n"
    <> "  \"inputs\": [\n"
    <> "    {\n"
    <> "      \"name\": \"exec_plan_bundle\",\n"
    <> "      \"value\": {\n"
    <> "        \"status\": \"present\",\n"
    <> "        \"type\": \"exec_plan_bundle\",\n"
    <> "        \"ref\": \""
    <> bundle_ref
    <> "\",\n"
    <> "        \"sha256\": \""
    <> bundle_sha
    <> "\"\n"
    <> "      }\n"
    <> "    }\n"
    <> "  ]\n"
    <> "}\n"
  let assert Ok(Nil) =
    simplifile.write(manifest_dir <> "/inputs.v1.json", manifest)
  Nil
}

fn copy_bundle_to_workstream_artifact(
  dir: String,
  bundle_ref: String,
  bundle_sha: String,
) -> String {
  let assert Ok(bundle_text) =
    simplifile.read(dir <> "/.scherzo-state/artifacts/" <> bundle_ref)
  let workstream_ref = "workstream-artifacts/sha256/" <> bundle_sha <> ".json"
  let output_dir =
    dir <> "/.scherzo-state/artifacts/workstream-artifacts/sha256"
  let assert Ok(Nil) = simplifile.create_directory_all(output_dir)
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/.scherzo-state/artifacts/" <> workstream_ref,
      bundle_text,
    )
  workstream_ref
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

pub fn validate_bundle_accepts_legacy_retained_shape_fixture_test() {
  let artifact =
    run_helper(
      "validate-bundle --bundle test/fixtures/execplan_v2/legacy/exec-plan-bundle.legacy.json --repo-root .",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(artifact.stdout, "BUNDLE_VALID=ok")
}

pub fn schema_validator_accepts_legacy_pack_and_code_change_fixtures_test() {
  let pack =
    run_shell(
      "scripts/scherzo-json-schema-validate --schema .scherzo/workflows/schemas/implementation-pack.v2.schema.json < test/fixtures/execplan_v2/legacy/implementation-pack.legacy.json",
    )
  assert pack.status == step_artifact.StepSucceeded
  assert pack.exit_code == Some(0)

  let code_change =
    run_shell(
      "scripts/scherzo-json-schema-validate --schema .scherzo/workflows/schemas/code-change-bundle.v2.schema.json < test/fixtures/execplan_v2/legacy/code-change-bundle.legacy.json",
    )
  assert code_change.status == step_artifact.StepSucceeded
  assert code_change.exit_code == Some(0)
}

pub fn validate_bundle_accepts_artifact_backed_plan_without_repo_path_test() {
  let dir = "test/tmp/execplan-artifact-backed-plan-validate"
  test_helpers.reset_dir(dir)
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
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/run-root")
  let plan_ref = "runs/run-artifact-plan/outputs/plan.md"
  let #(bundle_ref, bundle_sha, plan_text) =
    write_artifact_backed_plan_bundle(dir, plan_ref, "", True)
  let issue_context =
    "Bundle ref: " <> bundle_ref <> "\nBundle sha256: " <> bundle_sha <> "\n"
  let helper = "../../../.scherzo/workflows/scripts/scherzo-execplan"

  let artifact =
    run_shell_in(
      dir,
      "env SCHERZO_REPO_ROOT=$PWD SCHERZO_RUN_ROOT=$PWD/run-root SCHERZO_ISSUE_CONTEXT="
        <> test_helpers.shell_quote(issue_context)
        <> " "
        <> helper
        <> " implementation-prepare --from-issue-context",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(
    artifact.stdout,
    "PLAN=state/implementation/execplan-review-doc.md",
  )
  let assert Ok(prepared_plan) =
    simplifile.read(dir <> "/tmp/execplan-review-doc.md")
  let assert Ok(canonical_plan) =
    simplifile.read(
      dir <> "/run-root/state/implementation/execplan-review-doc.md",
    )
  let assert Ok(canonical_pack) =
    simplifile.read(
      dir <> "/run-root/state/implementation/execplan-implementation-pack.json",
    )
  let assert Ok(canonical_bundle) =
    simplifile.read(
      dir <> "/run-root/state/implementation/execplan-bundle.json",
    )
  assert prepared_plan == plan_text
  assert canonical_plan == plan_text
  assert string.contains(canonical_pack, "schema_version")
  assert string.contains(canonical_bundle, "schema_version")
  let assert Ok(metadata) =
    simplifile.read(dir <> "/tmp/scherzo-implementation.json")
  let assert Ok(canonical_metadata) =
    simplifile.read(dir <> "/run-root/state/implementation/metadata.json")
  assert canonical_metadata == metadata
  assert string.contains(
    metadata,
    "\"plan_path\": \"state/implementation/execplan-review-doc.md\"",
  )
  assert string.contains(
    metadata,
    "\"plan_artifact_ref\": \"" <> plan_ref <> "\"",
  )
  assert string.contains(metadata, "\"canonical_plan_path\":")
  assert string.contains(metadata, "\"canonical_execplan_v2_bundle_path\":")
  assert string.contains(metadata, "\"legacy_review_doc_path\": \"\"")
}

pub fn implementation_prepare_uses_workstream_input_manifest_test() {
  let dir = "test/tmp/execplan-workstream-input-prepare"
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/run-root")
  let plan_ref = "runs/run-artifact-plan/outputs/plan.md"
  let #(bundle_ref, bundle_sha, plan_text) =
    write_artifact_backed_plan_bundle(dir, plan_ref, "", True)
  let workstream_ref =
    copy_bundle_to_workstream_artifact(dir, bundle_ref, bundle_sha)
  write_exec_plan_bundle_input_manifest(
    dir,
    "run-workstream-input",
    workstream_ref,
    bundle_sha,
  )
  let helper = "../../../.scherzo/workflows/scripts/scherzo-execplan"

  let artifact =
    run_shell_in(
      dir,
      "env SCHERZO_REPO_ROOT=$PWD SCHERZO_RUN_ROOT=$PWD/run-root SCHERZO_RUN_ID=run-workstream-input "
        <> helper
        <> " implementation-prepare --from-workstream-input",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(
    artifact.stdout,
    "PLAN=state/implementation/execplan-review-doc.md",
  )
  let assert Ok(prepared_plan) =
    simplifile.read(dir <> "/tmp/execplan-review-doc.md")
  assert prepared_plan == plan_text
}

pub fn implementation_prepare_workstream_input_rejects_issue_context_fallback_test() {
  let dir = "test/tmp/execplan-workstream-input-no-fallback"
  test_helpers.reset_dir(dir)
  let plan_ref = "runs/run-artifact-plan/outputs/plan.md"
  let #(bundle_ref, bundle_sha, _plan_text) =
    write_artifact_backed_plan_bundle(dir, plan_ref, "", True)
  let issue_context =
    "Bundle ref: " <> bundle_ref <> "\nBundle sha256: " <> bundle_sha <> "\n"
  let helper = "../../../.scherzo/workflows/scripts/scherzo-execplan"

  let artifact =
    run_shell_in(
      dir,
      "env SCHERZO_REPO_ROOT=$PWD SCHERZO_RUN_ID=run-without-input SCHERZO_ISSUE_CONTEXT="
        <> test_helpers.shell_quote(issue_context)
        <> " "
        <> helper
        <> " implementation-prepare --from-workstream-input",
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(2)
  assert artifact.failure_code == Some("execplan_v2_input_bundle_missing")
}

pub fn implementation_prepare_prefer_workstream_input_falls_back_to_issue_context_test() {
  let dir = "test/tmp/execplan-prefer-workstream-input-fallback"
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/run-root")
  let plan_ref = "runs/run-artifact-plan/outputs/plan.md"
  let #(bundle_ref, bundle_sha, _plan_text) =
    write_artifact_backed_plan_bundle(dir, plan_ref, "", True)
  let issue_context =
    "Bundle ref: " <> bundle_ref <> "\nBundle sha256: " <> bundle_sha <> "\n"
  let helper = "../../../.scherzo/workflows/scripts/scherzo-execplan"

  let artifact =
    run_shell_in(
      dir,
      "env SCHERZO_REPO_ROOT=$PWD SCHERZO_RUN_ROOT=$PWD/run-root SCHERZO_RUN_ID=run-without-input SCHERZO_ISSUE_CONTEXT="
        <> test_helpers.shell_quote(issue_context)
        <> " "
        <> helper
        <> " implementation-prepare --prefer-workstream-input",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
}

pub fn implementation_prepare_issue_context_ignores_workstream_input_manifest_test() {
  let dir = "test/tmp/execplan-issue-context-ignores-input"
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/run-root")
  let plan_ref = "runs/run-artifact-plan/outputs/plan.md"
  let #(bundle_ref, bundle_sha, _plan_text) =
    write_artifact_backed_plan_bundle(dir, plan_ref, "", True)
  let manifest_dir = dir <> "/.scherzo-state/artifacts/runs/run-bad-input"
  let assert Ok(Nil) = simplifile.create_directory_all(manifest_dir)
  let assert Ok(Nil) =
    simplifile.write(
      manifest_dir <> "/inputs.v1.json",
      "{\"inputs\":[{\"name\":\"exec_plan_bundle\",\"value\":{\"status\":\"present\",\"type\":\"exec_plan_bundle\"}}]}",
    )
  let issue_context =
    "Bundle ref: " <> bundle_ref <> "\nBundle sha256: " <> bundle_sha <> "\n"
  let helper = "../../../.scherzo/workflows/scripts/scherzo-execplan"

  let artifact =
    run_shell_in(
      dir,
      "env SCHERZO_REPO_ROOT=$PWD SCHERZO_RUN_ROOT=$PWD/run-root SCHERZO_RUN_ID=run-bad-input SCHERZO_ISSUE_CONTEXT="
        <> test_helpers.shell_quote(issue_context)
        <> " "
        <> helper
        <> " implementation-prepare --from-issue-context",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
}

pub fn prepare_revision_uses_workstream_input_manifest_test() {
  let dir = "test/tmp/execplan-prepare-revision-workstream-input"
  test_helpers.reset_dir(dir)
  let #(bundle_ref, bundle_sha) =
    write_revision_bundle(dir, "test/fixtures/execplan_v2/review-doc.valid.md")
  write_exec_plan_bundle_input_manifest(
    dir <> "/repo",
    "run-revision-input",
    bundle_ref,
    bundle_sha,
  )

  let artifact =
    run_shell(
      "env SCHERZO_REPO_ROOT="
      <> test_helpers.shell_quote(dir <> "/repo")
      <> " SCHERZO_RUN_ID=run-revision-input .scherzo/workflows/scripts/scherzo-execplan prepare-revision --from-workstream-input --write-bundle "
      <> test_helpers.shell_quote(dir <> "/previous-bundle.json")
      <> " --write-review-doc-path "
      <> test_helpers.shell_quote(dir <> "/review.path")
      <> " --write-pack "
      <> test_helpers.shell_quote(dir <> "/previous-pack.json"),
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(artifact.stdout, "PREPARE_REVISION_STATUS=ok")
  assert string.contains(
    artifact.stdout,
    "BUNDLE_DISCOVERY_STATUS=mapped_input",
  )
}

pub fn implementation_prepare_rejects_plan_hash_mismatch_test() {
  let dir = "test/tmp/execplan-artifact-plan-hash-mismatch"
  test_helpers.reset_dir(dir)
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
        <> test_helpers.shell_quote(issue_context)
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
  test_helpers.reset_dir(dir)
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
        <> test_helpers.shell_quote(issue_context)
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
  test_helpers.reset_dir(dir)
  let run_root = dir <> "/run-root"
  let assert Ok(Nil) = simplifile.create_directory_all(run_root)

  let artifact =
    run_shell(
      "SCHERZO_RUN_ROOT="
      <> test_helpers.shell_quote(run_root)
      <> " SCHERZO_ISSUE_IDENTIFIER=LIV-385 SCHERZO_ISSUE_CONTEXT="
      <> test_helpers.shell_quote("no bundle here")
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
  test_helpers.reset_dir(dir)
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
        <> test_helpers.shell_quote("Implement: Fixture source LIV-418")
        <> " SCHERZO_ISSUE_URL="
        <> test_helpers.shell_quote(
        "https://linear.app/living-systems/issue/LIV-423/implement-fixture",
      )
        <> " SCHERZO_ISSUE_CONTEXT="
        <> test_helpers.shell_quote(issue_context)
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
  test_helpers.reset_dir(dir)
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
        <> test_helpers.shell_quote("Unexpected implementation handoff")
        <> " SCHERZO_ISSUE_CONTEXT="
        <> test_helpers.shell_quote(issue_context)
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

pub fn validate_bundle_rejects_descriptor_plan_ref_mismatch_test() {
  let path =
    mutated_bundle(
      "test/tmp/execplan-descriptor-plan-ref-mismatch",
      each: "      \"name\": \"plan\",\n      \"ref\": \"runs/run-1/outputs/plan.md\",",
      with: "      \"name\": \"plan\",\n      \"ref\": \"runs/run-1/outputs/other-plan.md\",",
    )

  let artifact =
    run_helper("validate-bundle --bundle " <> path <> " --repo-root .")

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(2)
  assert string.contains(
    artifact.stderr,
    "SCHERZO_FAILURE_CODE=execplan_v2_descriptor_entry_mismatch",
  )
}

pub fn validate_bundle_rejects_descriptor_plan_hash_mismatch_test() {
  let path =
    mutated_bundle(
      "test/tmp/execplan-descriptor-plan-hash-mismatch",
      each: "      \"name\": \"plan\",\n      \"ref\": \"runs/run-1/outputs/plan.md\",\n      \"sha256\": \"6f1b07718f377d21629aca606b39beb2424d6b8503b419b7e65166c003389674\"",
      with: "      \"name\": \"plan\",\n      \"ref\": \"runs/run-1/outputs/plan.md\",\n      \"sha256\": \"0000000000000000000000000000000000000000000000000000000000000000\"",
    )

  let artifact =
    run_helper("validate-bundle --bundle " <> path <> " --repo-root .")

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(2)
  assert string.contains(
    artifact.stderr,
    "SCHERZO_FAILURE_CODE=execplan_v2_descriptor_entry_mismatch",
  )
}

pub fn validate_bundle_rejects_descriptor_pack_bytes_mismatch_test() {
  let path =
    mutated_bundle(
      "test/tmp/execplan-descriptor-pack-bytes-mismatch",
      each: "      \"bytes\": 2220,\n      \"kind\": \"file\",\n      \"media_type\": \"application/json\",\n      \"name\": \"implementation_pack\"",
      with: "      \"bytes\": 2221,\n      \"kind\": \"file\",\n      \"media_type\": \"application/json\",\n      \"name\": \"implementation_pack\"",
    )

  let artifact =
    run_helper("validate-bundle --bundle " <> path <> " --repo-root .")

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(2)
  assert string.contains(
    artifact.stderr,
    "SCHERZO_FAILURE_CODE=execplan_v2_descriptor_entry_mismatch",
  )
}

pub fn validate_bundle_rejects_missing_review_doc_test() {
  let path =
    mutated_legacy_bundle(
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

pub fn prepare_revision_seeds_stale_review_doc_from_retained_plan_artifact_test() {
  let dir = "test/tmp/execplan-prepare-revision-seeded-plan"
  test_helpers.reset_dir(dir)
  let review_path = dir <> "/repo/docs/plans/seeded.md"
  let #(bundle_ref, bundle_sha) = write_revision_bundle(dir, review_path)
  let plan_dir = dir <> "/repo/.scherzo-state/artifacts/runs/run-1/outputs"
  let assert Ok(Nil) = simplifile.create_directory_all(plan_dir)
  write_valid_review_doc(plan_dir <> "/plan.md")
  let assert Ok(Nil) =
    simplifile.create_directory_all(dir <> "/repo/docs/plans")
  let assert Ok(Nil) = simplifile.write(review_path, "# stale\n")
  let issue_context =
    "Bundle ref: " <> bundle_ref <> "\nBundle sha256: " <> bundle_sha <> "\n"

  let artifact =
    run_shell(
      "env SCHERZO_REPO_ROOT="
      <> test_helpers.shell_quote(dir <> "/repo")
      <> " SCHERZO_ISSUE_CONTEXT="
      <> test_helpers.shell_quote(issue_context)
      <> " .scherzo/workflows/scripts/scherzo-execplan prepare-revision --from-issue-context --write-bundle "
      <> test_helpers.shell_quote(dir <> "/previous-bundle.json")
      <> " --write-review-doc-path "
      <> test_helpers.shell_quote(dir <> "/review.path")
      <> " --write-pack "
      <> test_helpers.shell_quote(dir <> "/previous-pack.json"),
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  let assert Ok(review_doc) = simplifile.read(review_path)
  assert string.contains(review_doc, "Purpose / Big Picture")
  assert !string.contains(review_doc, "# stale")
}

pub fn prepare_revision_rejects_non_markdown_seed_destination_test() {
  let dir = "test/tmp/execplan-prepare-revision-seeded-non-md"
  test_helpers.reset_dir(dir)
  let review_path = dir <> "/repo/docs/plans/seeded.txt"
  let #(bundle_ref, bundle_sha) = write_revision_bundle(dir, review_path)
  let plan_dir = dir <> "/repo/.scherzo-state/artifacts/runs/run-1/outputs"
  let assert Ok(Nil) = simplifile.create_directory_all(plan_dir)
  write_valid_review_doc(plan_dir <> "/plan.md")
  let issue_context =
    "Bundle ref: " <> bundle_ref <> "\nBundle sha256: " <> bundle_sha <> "\n"

  let artifact =
    run_shell(
      "env SCHERZO_REPO_ROOT="
      <> test_helpers.shell_quote(dir <> "/repo")
      <> " SCHERZO_ISSUE_CONTEXT="
      <> test_helpers.shell_quote(issue_context)
      <> " .scherzo/workflows/scripts/scherzo-execplan prepare-revision --from-issue-context --write-bundle "
      <> test_helpers.shell_quote(dir <> "/previous-bundle.json")
      <> " --write-review-doc-path "
      <> test_helpers.shell_quote(dir <> "/review.path")
      <> " --write-pack "
      <> test_helpers.shell_quote(dir <> "/previous-pack.json"),
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(2)
  assert string.contains(
    artifact.stderr,
    "SCHERZO_FAILURE_CODE=execplan_revision_base_missing",
  )
  assert string.contains(artifact.stderr, "review doc must be Markdown source")
  let assert Error(_) = simplifile.read(review_path)
}

pub fn prepare_revision_resolves_review_doc_from_recorded_branch_test() {
  let dir = "test/tmp/execplan-prepare-revision-branch"
  test_helpers.reset_dir(dir)
  let review_path = dir <> "/docs/plans/unmerged.md"
  let #(bundle_ref, bundle_sha) = write_revision_legacy_bundle(dir, review_path)
  let driver = dir <> "/workspace-driver"
  let log = dir <> "/workspace-driver.log"
  let assert Ok(Nil) =
    simplifile.write(
      driver,
      "#!/bin/sh\n"
        <> "printf '%s\\n' \"$*\" >> "
        <> test_helpers.shell_quote(log)
        <> "\n"
        <> "if [ \"$1\" = refresh-base ] && [ \"$5\" = ca667773c9a6d31bb64676c103b3f1f14c3bcced ]; then\n"
        <> "  printf '%s\\n' '{\"version\":1,\"status\":\"base_not_found\",\"failure_code\":\"base_not_found\",\"message\":\"head not local\"}'\n"
        <> "  exit 1\n"
        <> "fi\n"
        <> "if [ \"$1\" = refresh-base ] && [ \"$5\" = execplan/liv-314-unmerged@fork ]; then\n"
        <> "  mkdir -p "
        <> test_helpers.shell_quote(dir <> "/docs/plans")
        <> "\n"
        <> "  cp test/fixtures/execplan_v2/review-doc.valid.md "
        <> test_helpers.shell_quote(review_path)
        <> "\n"
        <> "  printf '%s\\n' '{\"version\":1,\"status\":\"rebased_clean\",\"stage\":\"prepare_revision\",\"base_ref\":\"execplan/liv-314-unmerged@fork\",\"base_revision\":\"execplan/liv-314-unmerged@fork\",\"before_revision\":\"main\",\"after_revision\":\"branch\",\"conflict_files\":[]}'\n"
        <> "  exit 0\n"
        <> "fi\n"
        <> "printf '%s\\n' '{\"version\":1,\"status\":\"base_not_found\",\"failure_code\":\"base_not_found\",\"message\":\"missing revision base\"}'\n"
        <> "exit 1\n",
    )
  let chmod = run_shell("chmod +x " <> test_helpers.shell_quote(driver))
  assert chmod.status == step_artifact.StepSucceeded
  let issue_context =
    "Bundle ref: " <> bundle_ref <> "\nBundle sha256: " <> bundle_sha <> "\n"

  let artifact =
    run_shell(
      "env SCHERZO_REPO_ROOT="
      <> test_helpers.shell_quote(dir <> "/repo")
      <> " SCHERZO_WORKSPACE_DRIVER="
      <> test_helpers.shell_quote(driver)
      <> " SCHERZO_JJ_WORKSPACE_REMOTE=upstream SCHERZO_JJ_WORKSPACE_PUBLISH_REMOTE=fork SCHERZO_PR_REMOTE=legacy SCHERZO_ISSUE_CONTEXT="
      <> test_helpers.shell_quote(issue_context)
      <> " .scherzo/workflows/scripts/scherzo-execplan prepare-revision --from-issue-context --write-bundle "
      <> test_helpers.shell_quote(dir <> "/previous-bundle.json")
      <> " --write-review-doc-path "
      <> test_helpers.shell_quote(dir <> "/review.path")
      <> " --write-pack "
      <> test_helpers.shell_quote(dir <> "/previous-pack.json"),
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
    "refresh-base --stage prepare_revision --target execplan/liv-314-unmerged@fork --json",
  )
  assert !string.contains(
    driver_log,
    "refresh-base --stage prepare_revision --target execplan/liv-314-unmerged@upstream --json",
  )
}

pub fn prepare_revision_resolves_review_doc_from_retained_publication_manifest_test() {
  let dir = "test/tmp/execplan-prepare-revision-publication-manifest"
  test_helpers.reset_dir(dir)
  let review_path = dir <> "/docs/plans/unmerged.md"
  let #(bundle_ref, bundle_sha) =
    write_revision_legacy_bundle_without_surface(dir, review_path)
  write_execplan_publication_manifest(
    dir <> "/repo",
    "run-legacy",
    "published",
    "scherzo/execplan/LIV-314/execplan_review_doc",
    "https://github.com/example/repo/pull/314",
    "",
  )
  let driver = dir <> "/workspace-driver"
  let log = dir <> "/workspace-driver.log"
  let assert Ok(Nil) =
    simplifile.write(
      driver,
      "#!/bin/sh\n"
        <> "printf '%s\\n' \"$*\" >> "
        <> test_helpers.shell_quote(log)
        <> "\n"
        <> "if [ \"$1\" = refresh-base ] && [ \"$5\" = scherzo/execplan/LIV-314/execplan_review_doc@fork ]; then\n"
        <> "  mkdir -p "
        <> test_helpers.shell_quote(dir <> "/docs/plans")
        <> "\n"
        <> "  cp test/fixtures/execplan_v2/review-doc.valid.md "
        <> test_helpers.shell_quote(review_path)
        <> "\n"
        <> "  printf '%s\\n' '{\"version\":1,\"status\":\"rebased_clean\",\"stage\":\"prepare_revision\",\"base_ref\":\"scherzo/execplan/LIV-314/execplan_review_doc@fork\",\"base_revision\":\"scherzo/execplan/LIV-314/execplan_review_doc@fork\",\"before_revision\":\"main\",\"after_revision\":\"branch\",\"conflict_files\":[]}'\n"
        <> "  exit 0\n"
        <> "fi\n"
        <> "printf '%s\\n' '{\"version\":1,\"status\":\"base_not_found\",\"failure_code\":\"base_not_found\",\"message\":\"missing revision base\"}'\n"
        <> "exit 1\n",
    )
  let chmod = run_shell("chmod +x " <> test_helpers.shell_quote(driver))
  assert chmod.status == step_artifact.StepSucceeded
  let issue_context =
    "Bundle ref: " <> bundle_ref <> "\nBundle sha256: " <> bundle_sha <> "\n"

  let artifact =
    run_shell(
      "env SCHERZO_REPO_ROOT="
      <> test_helpers.shell_quote(dir <> "/repo")
      <> " SCHERZO_WORKSPACE_DRIVER="
      <> test_helpers.shell_quote(driver)
      <> " SCHERZO_JJ_WORKSPACE_PUBLISH_REMOTE=fork SCHERZO_ISSUE_CONTEXT="
      <> test_helpers.shell_quote(issue_context)
      <> " .scherzo/workflows/scripts/scherzo-execplan prepare-revision --from-issue-context --write-bundle "
      <> test_helpers.shell_quote(dir <> "/previous-bundle.json")
      <> " --write-review-doc-path "
      <> test_helpers.shell_quote(dir <> "/review.path")
      <> " --write-pack "
      <> test_helpers.shell_quote(dir <> "/previous-pack.json"),
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  let assert Ok(path_contents) = simplifile.read(dir <> "/review.path")
  assert path_contents == review_path <> "\n"
  let assert Ok(driver_log) = simplifile.read(log)
  assert string.contains(
    driver_log,
    "refresh-base --stage prepare_revision --target scherzo/execplan/LIV-314/execplan_review_doc@fork --json",
  )
}

pub fn prepare_revision_resolves_review_doc_from_retained_publication_manifest_head_test() {
  let dir = "test/tmp/execplan-prepare-revision-publication-manifest-head"
  test_helpers.reset_dir(dir)
  let review_path = dir <> "/docs/plans/unmerged.md"
  let #(bundle_ref, bundle_sha) =
    write_revision_legacy_bundle_without_surface(dir, review_path)
  write_execplan_publication_manifest(
    dir <> "/repo",
    "run-legacy",
    "published",
    "",
    "https://github.com/example/repo/pull/314",
    "abcdefabcdefabcdefabcdefabcdefabcdefabcd",
  )
  let driver = dir <> "/workspace-driver"
  let log = dir <> "/workspace-driver.log"
  let assert Ok(Nil) =
    simplifile.write(
      driver,
      "#!/bin/sh\n"
        <> "printf '%s\\n' \"$*\" >> "
        <> test_helpers.shell_quote(log)
        <> "\n"
        <> "if [ \"$1\" = refresh-base ] && [ \"$5\" = abcdefabcdefabcdefabcdefabcdefabcdefabcd ]; then\n"
        <> "  mkdir -p "
        <> test_helpers.shell_quote(dir <> "/docs/plans")
        <> "\n"
        <> "  cp test/fixtures/execplan_v2/review-doc.valid.md "
        <> test_helpers.shell_quote(review_path)
        <> "\n"
        <> "  printf '%s\\n' '{\"version\":1,\"status\":\"rebased_clean\",\"stage\":\"prepare_revision\",\"base_ref\":\"abcdefabcdefabcdefabcdefabcdefabcdefabcd\",\"base_revision\":\"abcdefabcdefabcdefabcdefabcdefabcdefabcd\",\"before_revision\":\"main\",\"after_revision\":\"head\",\"conflict_files\":[]}'\n"
        <> "  exit 0\n"
        <> "fi\n"
        <> "printf '%s\\n' '{\"version\":1,\"status\":\"base_not_found\",\"failure_code\":\"base_not_found\",\"message\":\"missing revision base\"}'\n"
        <> "exit 1\n",
    )
  let chmod = run_shell("chmod +x " <> test_helpers.shell_quote(driver))
  assert chmod.status == step_artifact.StepSucceeded
  let issue_context =
    "Bundle ref: " <> bundle_ref <> "\nBundle sha256: " <> bundle_sha <> "\n"

  let artifact =
    run_shell(
      "env SCHERZO_REPO_ROOT="
      <> test_helpers.shell_quote(dir <> "/repo")
      <> " SCHERZO_WORKSPACE_DRIVER="
      <> test_helpers.shell_quote(driver)
      <> " SCHERZO_JJ_WORKSPACE_PUBLISH_REMOTE=fork SCHERZO_ISSUE_CONTEXT="
      <> test_helpers.shell_quote(issue_context)
      <> " .scherzo/workflows/scripts/scherzo-execplan prepare-revision --from-issue-context --write-bundle "
      <> test_helpers.shell_quote(dir <> "/previous-bundle.json")
      <> " --write-review-doc-path "
      <> test_helpers.shell_quote(dir <> "/review.path")
      <> " --write-pack "
      <> test_helpers.shell_quote(dir <> "/previous-pack.json"),
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  let assert Ok(driver_log) = simplifile.read(log)
  assert string.contains(
    driver_log,
    "refresh-base --stage prepare_revision --target abcdefabcdefabcdefabcdefabcdefabcdefabcd --json",
  )
  assert !string.contains(driver_log, "@fork")
}

pub fn prepare_revision_prefers_review_surface_over_conflicting_retained_manifest_test() {
  let dir = "test/tmp/execplan-prepare-revision-prefers-surface"
  test_helpers.reset_dir(dir)
  let review_path = dir <> "/docs/plans/unmerged.md"
  let #(bundle_ref, bundle_sha) =
    write_revision_legacy_bundle_with_surface(
      dir,
      review_path,
      "execplan/liv-314-surface",
      "",
    )
  write_execplan_publication_manifest(
    dir <> "/repo",
    "run-legacy",
    "published",
    "scherzo/execplan/LIV-314/execplan_review_doc",
    "https://github.com/example/repo/pull/314",
    "",
  )
  let driver = dir <> "/workspace-driver"
  let log = dir <> "/workspace-driver.log"
  let assert Ok(Nil) =
    simplifile.write(
      driver,
      "#!/bin/sh\n"
        <> "printf '%s\\n' \"$*\" >> "
        <> test_helpers.shell_quote(log)
        <> "\n"
        <> "if [ \"$1\" = refresh-base ] && [ \"$5\" = execplan/liv-314-surface@fork ]; then\n"
        <> "  mkdir -p "
        <> test_helpers.shell_quote(dir <> "/docs/plans")
        <> "\n"
        <> "  cp test/fixtures/execplan_v2/review-doc.valid.md "
        <> test_helpers.shell_quote(review_path)
        <> "\n"
        <> "  printf '%s\\n' '{\"version\":1,\"status\":\"rebased_clean\",\"stage\":\"prepare_revision\",\"base_ref\":\"execplan/liv-314-surface@fork\",\"base_revision\":\"execplan/liv-314-surface@fork\",\"before_revision\":\"main\",\"after_revision\":\"branch\",\"conflict_files\":[]}'\n"
        <> "  exit 0\n"
        <> "fi\n"
        <> "printf '%s\\n' '{\"version\":1,\"status\":\"base_not_found\",\"failure_code\":\"base_not_found\",\"message\":\"missing revision base\"}'\n"
        <> "exit 1\n",
    )
  let chmod = run_shell("chmod +x " <> test_helpers.shell_quote(driver))
  assert chmod.status == step_artifact.StepSucceeded
  let issue_context =
    "Bundle ref: " <> bundle_ref <> "\nBundle sha256: " <> bundle_sha <> "\n"

  let artifact =
    run_shell(
      "env SCHERZO_REPO_ROOT="
      <> test_helpers.shell_quote(dir <> "/repo")
      <> " SCHERZO_WORKSPACE_DRIVER="
      <> test_helpers.shell_quote(driver)
      <> " SCHERZO_JJ_WORKSPACE_PUBLISH_REMOTE=fork SCHERZO_ISSUE_CONTEXT="
      <> test_helpers.shell_quote(issue_context)
      <> " .scherzo/workflows/scripts/scherzo-execplan prepare-revision --from-issue-context --write-bundle "
      <> test_helpers.shell_quote(dir <> "/previous-bundle.json")
      <> " --write-review-doc-path "
      <> test_helpers.shell_quote(dir <> "/review.path")
      <> " --write-pack "
      <> test_helpers.shell_quote(dir <> "/previous-pack.json"),
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  let assert Ok(driver_log) = simplifile.read(log)
  assert string.contains(
    driver_log,
    "refresh-base --stage prepare_revision --target execplan/liv-314-surface@fork --json",
  )
  assert !string.contains(
    driver_log,
    "scherzo/execplan/LIV-314/execplan_review_doc",
  )
}

pub fn prepare_revision_reports_revision_base_missing_when_branch_unresolved_test() {
  let dir = "test/tmp/execplan-prepare-revision-base-missing"
  test_helpers.reset_dir(dir)
  let review_path = dir <> "/docs/plans/unmerged.md"
  let #(bundle_ref, bundle_sha) = write_revision_legacy_bundle(dir, review_path)
  let driver = dir <> "/workspace-driver"
  let assert Ok(Nil) =
    simplifile.write(
      driver,
      "#!/bin/sh\n"
        <> "printf '%s\\n' '{\"version\":1,\"status\":\"base_not_found\",\"failure_code\":\"base_not_found\",\"message\":\"missing revision base\"}'\n"
        <> "exit 1\n",
    )
  let chmod = run_shell("chmod +x " <> test_helpers.shell_quote(driver))
  assert chmod.status == step_artifact.StepSucceeded
  let issue_context =
    "Bundle ref: " <> bundle_ref <> "\nBundle sha256: " <> bundle_sha <> "\n"

  let artifact =
    run_shell(
      "env SCHERZO_REPO_ROOT="
      <> test_helpers.shell_quote(dir <> "/repo")
      <> " SCHERZO_WORKSPACE_DRIVER="
      <> test_helpers.shell_quote(driver)
      <> " SCHERZO_JJ_WORKSPACE_REMOTE=origin SCHERZO_ISSUE_CONTEXT="
      <> test_helpers.shell_quote(issue_context)
      <> " .scherzo/workflows/scripts/scherzo-execplan prepare-revision --from-issue-context --write-bundle "
      <> test_helpers.shell_quote(dir <> "/previous-bundle.json")
      <> " --write-review-doc-path "
      <> test_helpers.shell_quote(dir <> "/review.path")
      <> " --write-pack "
      <> test_helpers.shell_quote(dir <> "/previous-pack.json"),
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
  test_helpers.reset_dir(dir)
  let review_path = dir <> "/docs/plans/unmerged.md"
  let #(bundle_ref, bundle_sha) = write_revision_legacy_bundle(dir, review_path)
  let driver = dir <> "/workspace-driver"
  let assert Ok(Nil) =
    simplifile.write(
      driver,
      "#!/usr/bin/env python3\nimport time\ntime.sleep(5)\n",
    )
  let chmod = run_shell("chmod +x " <> test_helpers.shell_quote(driver))
  assert chmod.status == step_artifact.StepSucceeded
  let issue_context =
    "Bundle ref: " <> bundle_ref <> "\nBundle sha256: " <> bundle_sha <> "\n"

  let artifact =
    run_shell(
      "env SCHERZO_REPO_ROOT="
      <> test_helpers.shell_quote(dir <> "/repo")
      <> " SCHERZO_WORKSPACE_DRIVER="
      <> test_helpers.shell_quote(driver)
      <> " SCHERZO_EXECPLAN_REVISION_REFRESH_TIMEOUT_SECONDS=0.1 SCHERZO_ISSUE_CONTEXT="
      <> test_helpers.shell_quote(issue_context)
      <> " .scherzo/workflows/scripts/scherzo-execplan prepare-revision --from-issue-context --write-bundle "
      <> test_helpers.shell_quote(dir <> "/previous-bundle.json")
      <> " --write-review-doc-path "
      <> test_helpers.shell_quote(dir <> "/review.path")
      <> " --write-pack "
      <> test_helpers.shell_quote(dir <> "/previous-pack.json"),
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
  test_helpers.reset_dir(dir)
  let review_path = dir <> "/docs/plans/unmerged.md"
  let #(bundle_ref, bundle_sha) =
    write_revision_legacy_bundle_with_surface(
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
        <> test_helpers.shell_quote(log)
        <> "\n"
        <> "printf '%s\\n' '{\"version\":1,\"status\":\"rebased_clean\"}'\n",
    )
  let chmod = run_shell("chmod +x " <> test_helpers.shell_quote(driver))
  assert chmod.status == step_artifact.StepSucceeded
  let issue_context =
    "Bundle ref: " <> bundle_ref <> "\nBundle sha256: " <> bundle_sha <> "\n"

  let artifact =
    run_shell(
      "env SCHERZO_REPO_ROOT="
      <> test_helpers.shell_quote(dir <> "/repo")
      <> " SCHERZO_WORKSPACE_DRIVER="
      <> test_helpers.shell_quote(driver)
      <> " SCHERZO_ISSUE_CONTEXT="
      <> test_helpers.shell_quote(issue_context)
      <> " .scherzo/workflows/scripts/scherzo-execplan prepare-revision --from-issue-context --write-bundle "
      <> test_helpers.shell_quote(dir <> "/previous-bundle.json")
      <> " --write-review-doc-path "
      <> test_helpers.shell_quote(dir <> "/review.path")
      <> " --write-pack "
      <> test_helpers.shell_quote(dir <> "/previous-pack.json"),
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
    mutated_legacy_bundle(
      "test/tmp/execplan-review-hash-mismatch",
      each: "6f1b07718f377d21629aca606b39beb2424d6b8503b419b7e65166c003389674",
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
      each: "bb23c8a36dc5e9b3d46d8062fd073dc6acf3db208138d2e4b14530d092aa0f40",
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
  test_helpers.reset_dir(dir)
  let assert Ok(source) =
    simplifile.read("test/fixtures/execplan_v2/exec-plan-bundle.valid.json")
  let mutated =
    string.replace(
      source,
      each: "  \"artifact_type\": \"scherzo.exec_plan_bundle.v2\",\n",
      with: "  \"artifact_type\": \"scherzo.exec_plan_bundle.v2\",\n  \"sha256\": \"0000000000000000000000000000000000000000000000000000000000000000\",\n",
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
  test_helpers.reset_dir(dir)
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

pub fn validate_review_doc_rejects_liv_503_missing_open_questions_regression_test() {
  let dir = "test/tmp/execplan-missing-open-questions"
  test_helpers.reset_dir(dir)
  let path = dir <> "/review.md"
  let assert Ok(valid) =
    simplifile.read("test/fixtures/execplan_v2/review-doc.valid.md")
  let review =
    string.replace(
      valid,
      each: "\n## Open Questions and Clarifications Needed\n\nNone.\n",
      with: "\n",
    )
  let assert Ok(Nil) = simplifile.write(path, review)

  let artifact = run_helper("validate-review-doc --path " <> path)

  assert string.contains(
    artifact.stderr,
    "SCHERZO_FAILURE_CODE=execplan_review_doc_required_section_missing",
  )
  assert_review_doc_section_failed(
    artifact,
    "review doc missing required section",
    "Open Questions and Clarifications Needed",
  )
}

pub fn validate_review_doc_rejects_empty_open_questions_test() {
  let dir = "test/tmp/execplan-empty-open-questions"
  test_helpers.reset_dir(dir)
  let path = dir <> "/review.md"
  let assert Ok(valid) =
    simplifile.read("test/fixtures/execplan_v2/review-doc.valid.md")
  let review =
    string.replace(
      valid,
      each: "## Open Questions and Clarifications Needed\n\nNone.\n",
      with: "## Open Questions and Clarifications Needed\n\n<!-- intentionally blank -->\n",
    )
  let assert Ok(Nil) = simplifile.write(path, review)

  let artifact = run_helper("validate-review-doc --path " <> path)

  assert string.contains(
    artifact.stderr,
    "SCHERZO_FAILURE_CODE=execplan_review_doc_required_section_empty",
  )
  assert_review_doc_section_failed(
    artifact,
    "review doc required section has no meaningful content",
    "Open Questions and Clarifications Needed",
  )
}

pub fn validate_review_doc_rejects_missing_surprises_and_discoveries_test() {
  let dir = "test/tmp/execplan-missing-surprises-discoveries"
  test_helpers.reset_dir(dir)
  let path = dir <> "/review.md"
  let assert Ok(valid) =
    simplifile.read("test/fixtures/execplan_v2/review-doc.valid.md")
  let review =
    string.replace(
      valid,
      each: "\n## Surprises & Discoveries\n\nNone.\n",
      with: "\n",
    )
  let assert Ok(Nil) = simplifile.write(path, review)

  let artifact = run_helper("validate-review-doc --path " <> path)

  assert_review_doc_missing_section_failed(artifact, "Surprises & Discoveries")
}

pub fn validate_review_doc_rejects_empty_surprises_and_discoveries_test() {
  let dir = "test/tmp/execplan-empty-surprises-discoveries"
  test_helpers.reset_dir(dir)
  let path = dir <> "/review.md"
  let assert Ok(valid) =
    simplifile.read("test/fixtures/execplan_v2/review-doc.valid.md")
  let review =
    string.replace(
      valid,
      each: "## Surprises & Discoveries\n\nNone.\n\n## Decision Log",
      with: "## Surprises & Discoveries\n\n<!-- intentionally blank -->\n\n## Decision Log",
    )
  let assert Ok(Nil) = simplifile.write(path, review)

  let artifact = run_helper("validate-review-doc --path " <> path)

  assert_review_doc_empty_section_failed(artifact, "Surprises & Discoveries")
}

pub fn validate_review_doc_rejects_missing_outcomes_retrospective_test() {
  let dir = "test/tmp/execplan-missing-outcomes-retrospective"
  test_helpers.reset_dir(dir)
  let path = dir <> "/review.md"
  let assert Ok(valid) =
    simplifile.read("test/fixtures/execplan_v2/review-doc.valid.md")
  let review =
    string.replace(
      valid,
      each: "\n## Outcomes & Retrospective\n\nPending implementation.\n",
      with: "\n",
    )
  let assert Ok(Nil) = simplifile.write(path, review)

  let artifact = run_helper("validate-review-doc --path " <> path)

  assert_review_doc_missing_section_failed(artifact, "Outcomes & Retrospective")
}

pub fn validate_review_doc_rejects_empty_outcomes_retrospective_test() {
  let dir = "test/tmp/execplan-empty-outcomes-retrospective"
  test_helpers.reset_dir(dir)
  let path = dir <> "/review.md"
  let assert Ok(valid) =
    simplifile.read("test/fixtures/execplan_v2/review-doc.valid.md")
  let review =
    string.replace(
      valid,
      each: "## Outcomes & Retrospective\n\nPending implementation.\n\n## Validation and Acceptance",
      with: "## Outcomes & Retrospective\n\n<!-- intentionally blank -->\n\n## Validation and Acceptance",
    )
  let assert Ok(Nil) = simplifile.write(path, review)

  let artifact = run_helper("validate-review-doc --path " <> path)

  assert_review_doc_empty_section_failed(artifact, "Outcomes & Retrospective")
}

pub fn validate_review_doc_rejects_missing_strategy_overview_test() {
  let dir = "test/tmp/execplan-missing-strategy-overview"
  test_helpers.reset_dir(dir)
  let path = dir <> "/review.md"
  let assert Ok(valid) =
    simplifile.read("test/fixtures/execplan_v2/review-doc.valid.md")
  let review =
    string.replace(
      valid,
      each: "## Strategy Overview\n\nAdd one small documentation note and validate it with helper-level tests. The implementation pack contains the exact commands and file list.\n\n",
      with: "",
    )
  let assert Ok(Nil) = simplifile.write(path, review)

  let artifact = run_helper("validate-review-doc --path " <> path)

  assert_review_doc_section_failed(
    artifact,
    "review doc missing required section",
    "Strategy Overview",
  )
}

pub fn validate_review_doc_rejects_empty_scope_boundaries_test() {
  let dir = "test/tmp/execplan-empty-scope-boundaries"
  test_helpers.reset_dir(dir)
  let path = dir <> "/review.md"
  let assert Ok(valid) =
    simplifile.read("test/fixtures/execplan_v2/review-doc.valid.md")
  let review =
    string.replace(
      valid,
      each: "## Scope Boundaries\n\nOnly fixture files under `test/fixtures/execplan_v2/` are in scope for this sample. No production workflow is exercised by the fixture itself.\n\n## Milestones",
      with: "## Scope Boundaries\n\n1. \n<!-- intentionally blank -->\n\n## Milestones",
    )
  let assert Ok(Nil) = simplifile.write(path, review)

  let artifact = run_helper("validate-review-doc --path " <> path)

  assert_review_doc_section_failed(
    artifact,
    "review doc required section has no meaningful content",
    "Scope Boundaries",
  )
}

pub fn validate_review_doc_rejects_unchecked_required_progress_test() {
  let dir = "test/tmp/execplan-progress-preflight"
  test_helpers.reset_dir(dir)
  let path = dir <> "/review.md"
  let assert Ok(valid) =
    simplifile.read("test/fixtures/execplan_v2/review-doc.valid.md")
  let review =
    string.replace(
      valid,
      each: "## Progress\n\n- [x] 2026-05-15: Created the fixture review document.\n\n## Surprises & Discoveries",
      with: "## Progress\n\n- [ ] Run full validation before rollout.\n\n## Surprises & Discoveries",
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
  test_helpers.reset_dir(dir)
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
  test_helpers.reset_dir(dir)
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
  test_helpers.reset_dir(dir)
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
  test_helpers.reset_dir(dir)
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
  test_helpers.reset_dir(dir)
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
  test_helpers.reset_dir(dir)
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
  test_helpers.reset_dir(dir)
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

pub fn materialize_pack_accepts_camel_case_idempotent_test_name_test() {
  let dir = "test/tmp/execplan-pack-camel-case-idempotent"
  test_helpers.reset_dir(dir)
  let review_path = dir <> "/review.md"
  let submission_path = dir <> "/submission.json"
  let output_path = dir <> "/pack.json"
  let review =
    review_doc_with_validation(
      "Acceptance requires idempotent migration evidence before implementation is complete.",
    )
  let assert Ok(Nil) = simplifile.write(review_path, review)
  let assert Ok(Nil) =
    simplifile.write(
      submission_path,
      pack_submission_with_commands_and_testing(
        "Camel-case idempotent evidence",
        "[\"go test ./internal/store/sqlite -run TestMigrateIsIdempotent\"]",
        "Run the named Go test and assert the migration rerun leaves data readable.",
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

pub fn materialize_pack_accepts_manual_screenshot_evidence_without_commands_test() {
  let dir = "test/tmp/execplan-pack-manual-evidence"
  test_helpers.reset_dir(dir)
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
  test_helpers.reset_dir(dir)
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
  test_helpers.reset_dir(dir)

  let artifact =
    run_shell_in(
      dir,
      "env SCHERZO_ISSUE_CONTEXT="
        <> test_helpers.shell_quote("Create an execplan at doobar/docs/plans")
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
  test_helpers.reset_dir(dir)

  let artifact =
    run_shell_in(
      dir,
      "env SCHERZO_ISSUE_CONTEXT="
        <> test_helpers.shell_quote(
        "Create an execplan at doobar/docs/plans/exact.md",
      )
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
  test_helpers.reset_dir(dir)

  let artifact =
    run_shell_in(
      dir,
      "env SCHERZO_ISSUE_CONTEXT="
        <> test_helpers.shell_quote(
        "Create an execplan to add custom target support",
      )
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
  test_helpers.reset_dir(dir)

  let artifact =
    run_shell_in(
      dir,
      "env SCHERZO_ISSUE_CONTEXT="
        <> test_helpers.shell_quote(
        "Create an execplan\n\nDestination: production",
      )
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
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/docs/plans")
  write_valid_review_doc(dir <> "/docs/plans/default.md")

  let artifact =
    run_shell_in(
      dir,
      "env SCHERZO_WORKSPACE_DRIVER="
        <> test_helpers.shell_quote(tmp_repo_path(
        "scripts/scherzo-workspace-noop",
      ))
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
  test_helpers.reset_dir(dir)
  let helper = tmp_repo_path(".scherzo/workflows/scripts/scherzo-execplan")
  let driver = tmp_repo_path("scripts/scherzo-workspace-noop")

  let prepare =
    run_shell_in(
      dir,
      "env SCHERZO_ISSUE_CONTEXT="
        <> test_helpers.shell_quote("Create an execplan at doobar/docs/plans")
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
        <> test_helpers.shell_quote(driver)
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
  test_helpers.reset_dir(dir)
  let helper = tmp_repo_path(".scherzo/workflows/scripts/scherzo-execplan")
  let driver = tmp_repo_path("scripts/scherzo-workspace-noop")

  let prepare =
    run_shell_in(
      dir,
      "env SCHERZO_ISSUE_CONTEXT="
        <> test_helpers.shell_quote(
        "Create an execplan at doobar/docs/plans/exact.md",
      )
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
        <> test_helpers.shell_quote(driver)
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
  test_helpers.reset_dir(dir)
  let helper = tmp_repo_path(".scherzo/workflows/scripts/scherzo-execplan")
  let driver = tmp_repo_path("scripts/scherzo-workspace-noop")

  let prepare =
    run_shell_in(
      dir,
      "env SCHERZO_ISSUE_CONTEXT="
        <> test_helpers.shell_quote(
        "Create an execplan at doobar/docs/plans/exact.md",
      )
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
        <> test_helpers.shell_quote(driver)
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
  test_helpers.reset_dir(dir)

  let artifact =
    run_shell_in(
      dir,
      "env SCHERZO_ISSUE_CONTEXT="
        <> test_helpers.shell_quote("Create an execplan at ../outside")
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
  test_helpers.reset_dir(dir)

  let artifact =
    run_shell(
      "env SCHERZO_WORKSPACE_DRIVER=scripts/scherzo-workspace-noop SCHERZO_WORKSPACE_PATH="
      <> test_helpers.shell_quote(dir)
      <> " .scherzo/workflows/scripts/scherzo-execplan validate-review-doc --discover-changed-review-doc --write-path "
      <> test_helpers.shell_quote(dir <> "/review.path"),
    )

  assert artifact.status == step_artifact.StepFailed
  assert string.contains(artifact.stderr, "expected exactly one")
}

pub fn discover_changed_review_doc_rejects_multiple_candidates_test() {
  let dir = "test/tmp/execplan-discovery-multiple"
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/docs/plans")
  let assert Ok(Nil) = simplifile.write(dir <> "/docs/plans/a.md", "# A\n")
  let assert Ok(Nil) = simplifile.write(dir <> "/docs/plans/b.md", "# B\n")

  let artifact =
    run_shell(
      "env SCHERZO_WORKSPACE_DRIVER=scripts/scherzo-workspace-noop SCHERZO_WORKSPACE_PATH="
      <> test_helpers.shell_quote(dir)
      <> " .scherzo/workflows/scripts/scherzo-execplan validate-review-doc --discover-changed-review-doc --write-path "
      <> test_helpers.shell_quote(dir <> "/review.path"),
    )

  assert artifact.status == step_artifact.StepFailed
  assert string.contains(artifact.stderr, "expected exactly one")
  assert string.contains(artifact.stderr, "found 2")
}

pub fn materialize_pack_discovers_latest_structured_submission_test() {
  let dir = "test/tmp/execplan-structured-latest"
  test_helpers.reset_dir(dir)
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
      <> test_helpers.shell_quote(run_dir)
      <> " .scherzo/workflows/scripts/scherzo-execplan materialize-pack --review-doc test/fixtures/execplan_v2/review-doc.valid.md --submission-step incorporate_review --submission-artifact implementation_pack_submission --output "
      <> test_helpers.shell_quote(output),
    )

  assert artifact.status == step_artifact.StepSucceeded
  let assert Ok(pack) = simplifile.read(output)
  assert string.contains(pack, "Latest Pack")
  assert !string.contains(pack, "Old Pack")
}

pub fn materialize_commit_stack_writes_manifest_test() {
  let dir = "test/tmp/execplan-materialize-commit-stack"
  let review_path = "docs/plans/LIV-910-plan.md"
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  write_valid_review_doc(dir <> "/" <> review_path)
  let assert Ok(Nil) =
    simplifile.write(dir <> "/review.path", review_path <> "\n")
  write_fake_commit_stack_driver(dir <> "/bin/fake-driver", review_path)
  write_fake_commit_stack_jj(dir <> "/bin/jj")
  write_fake_commit_stack_git(dir <> "/bin/git")
  test_helpers.chmod_executable(dir <> "/bin/fake-driver")
  test_helpers.chmod_executable(dir <> "/bin/jj")
  test_helpers.chmod_executable(dir <> "/bin/git")

  let artifact =
    run_shell_in(
      dir,
      "env SCHERZO_WORKSPACE_DRIVER=./bin/fake-driver SCHERZO_GITHUB_REPO=example/repo SCHERZO_JJ_WORKSPACE_BASE_BRANCH=main SCHERZO_RUN_ID=run-1 PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-execplan materialize-commit-stack --review-doc-path-file review.path --output tmp/execplan-commit-stack.json",
    )

  assert artifact.status == step_artifact.StepSucceeded
  let assert Ok(commit_stack) =
    simplifile.read(dir <> "/tmp/execplan-commit-stack.json")
  assert string.contains(
    commit_stack,
    "\"artifact_type\": \"scherzo.git_commit_stack.v1\"",
  )
  assert string.contains(
    commit_stack,
    "\"ref\": \"tmp/execplan-review-doc.bundle\"",
  )
  assert string.contains(
    commit_stack,
    "\"sha\": \"2222222222222222222222222222222222222222\"",
  )
  assert string.contains(
    commit_stack,
    "\"sha\": \"3333333333333333333333333333333333333333\"",
  )
  assert string.contains(
    commit_stack,
    "\"tree\": \"4444444444444444444444444444444444444444\"",
  )
}

pub fn materialize_commit_stack_describes_empty_head_before_manifest_test() {
  let dir = "test/tmp/execplan-materialize-commit-stack-empty-description"
  let review_path = "docs/plans/LIV-910-plan.md"
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  write_valid_review_doc(dir <> "/" <> review_path)
  let assert Ok(Nil) =
    simplifile.write(dir <> "/review.path", review_path <> "\n")
  write_fake_commit_stack_driver(dir <> "/bin/fake-driver", review_path)
  write_fake_commit_stack_jj(dir <> "/bin/jj")
  write_fake_commit_stack_git(dir <> "/bin/git")
  test_helpers.chmod_executable(dir <> "/bin/fake-driver")
  test_helpers.chmod_executable(dir <> "/bin/jj")
  test_helpers.chmod_executable(dir <> "/bin/git")

  let artifact =
    run_shell_in(
      dir,
      "env SCHERZO_FAKE_EMPTY_DESCRIPTION=1 SCHERZO_WORKSPACE_DRIVER=./bin/fake-driver SCHERZO_GITHUB_REPO=example/repo SCHERZO_JJ_WORKSPACE_BASE_BRANCH=main SCHERZO_RUN_ID=run-1 PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-execplan materialize-commit-stack --review-doc-path-file review.path --output tmp/execplan-commit-stack.json",
    )

  assert artifact.status == step_artifact.StepSucceeded
  let assert Ok(jj_log) = simplifile.read(dir <> "/jj.log")
  assert string.contains(jj_log, "log -r @ --no-graph -T description")
  assert string.contains(
    jj_log,
    "describe -m ExecPlan review doc: docs/plans/LIV-910-plan.md",
  )
  let assert Ok(commit_stack) =
    simplifile.read(dir <> "/tmp/execplan-commit-stack.json")
  assert string.contains(
    commit_stack,
    "\"sha\": \"5555555555555555555555555555555555555555\"",
  )
  assert !string.contains(
    commit_stack,
    "\"sha\": \"3333333333333333333333333333333333333333\"",
  )
}

pub fn materialize_commit_stack_writes_retained_carrier_ref_test() {
  let dir = "test/tmp/execplan-materialize-commit-stack-retained-carrier"
  let review_path = "docs/plans/LIV-910-plan.md"
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  write_valid_review_doc(dir <> "/" <> review_path)
  let assert Ok(Nil) =
    simplifile.write(dir <> "/review.path", review_path <> "\n")
  write_fake_commit_stack_driver(dir <> "/bin/fake-driver", review_path)
  write_fake_commit_stack_jj(dir <> "/bin/jj")
  write_fake_commit_stack_git(dir <> "/bin/git")
  test_helpers.chmod_executable(dir <> "/bin/fake-driver")
  test_helpers.chmod_executable(dir <> "/bin/jj")
  test_helpers.chmod_executable(dir <> "/bin/git")

  let artifact =
    run_shell_in(
      dir,
      "env SCHERZO_WORKSPACE_DRIVER=./bin/fake-driver SCHERZO_GITHUB_REPO=example/repo SCHERZO_JJ_WORKSPACE_BASE_BRANCH=main SCHERZO_RUN_ID=run-1 SCHERZO_RUN_ARTIFACT_DIR=artifacts/runs/run-1 PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-execplan materialize-commit-stack --review-doc-path-file review.path --output tmp/execplan-commit-stack.json",
    )

  assert artifact.status == step_artifact.StepSucceeded
  let assert Ok(commit_stack) =
    simplifile.read(dir <> "/tmp/execplan-commit-stack.json")
  assert string.contains(
    commit_stack,
    "\"ref\": \"runs/run-1/outputs/execplan-review-doc.bundle\"",
  )
  let assert Ok(True) =
    simplifile.is_file(
      dir <> "/artifacts/runs/run-1/outputs/execplan-review-doc.bundle",
    )
}

pub fn materialize_commit_stack_removes_oversized_carrier_on_failure_test() {
  let dir = "test/tmp/execplan-materialize-commit-stack-oversized"
  let review_path = "docs/plans/LIV-910-plan.md"
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  write_valid_review_doc(dir <> "/" <> review_path)
  let assert Ok(Nil) =
    simplifile.write(dir <> "/review.path", review_path <> "\n")
  write_fake_commit_stack_driver(dir <> "/bin/fake-driver", review_path)
  write_fake_commit_stack_jj(dir <> "/bin/jj")
  write_fake_commit_stack_git_with_oversized_bundle(dir <> "/bin/git")
  test_helpers.chmod_executable(dir <> "/bin/fake-driver")
  test_helpers.chmod_executable(dir <> "/bin/jj")
  test_helpers.chmod_executable(dir <> "/bin/git")

  let artifact =
    run_shell_in(
      dir,
      "env SCHERZO_WORKSPACE_DRIVER=./bin/fake-driver SCHERZO_GITHUB_REPO=example/repo SCHERZO_JJ_WORKSPACE_BASE_BRANCH=main SCHERZO_RUN_ID=run-1 PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-execplan materialize-commit-stack --review-doc-path-file review.path --output tmp/execplan-commit-stack.json",
    )

  assert artifact.status == step_artifact.StepFailed
  assert string.contains(artifact.stderr, "commit_stack carrier bundle exceeds")
  let assert Ok(False) =
    simplifile.is_file(dir <> "/tmp/execplan-review-doc.bundle")
}

pub fn materialize_commit_stack_rejects_unrelated_changed_files_test() {
  let dir = "test/tmp/execplan-materialize-commit-stack-extra"
  let review_path = "docs/plans/LIV-910-plan.md"
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  write_valid_review_doc(dir <> "/" <> review_path)
  let assert Ok(Nil) =
    simplifile.write(dir <> "/review.path", review_path <> "\n")
  write_fake_commit_stack_driver_with_extra_path(
    dir <> "/bin/fake-driver",
    review_path,
    "src/unrelated.gleam",
  )
  write_fake_commit_stack_jj(dir <> "/bin/jj")
  write_fake_commit_stack_git(dir <> "/bin/git")
  test_helpers.chmod_executable(dir <> "/bin/fake-driver")
  test_helpers.chmod_executable(dir <> "/bin/jj")
  test_helpers.chmod_executable(dir <> "/bin/git")

  let artifact =
    run_shell_in(
      dir,
      "env SCHERZO_WORKSPACE_DRIVER=./bin/fake-driver SCHERZO_GITHUB_REPO=example/repo SCHERZO_JJ_WORKSPACE_BASE_BRANCH=main SCHERZO_RUN_ID=run-1 PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-execplan materialize-commit-stack --review-doc-path-file review.path --output tmp/execplan-commit-stack.json",
    )

  assert artifact.status == step_artifact.StepFailed
  assert string.contains(
    artifact.stderr,
    "workspace changed-files must contain only the review doc change",
  )
  assert string.contains(artifact.stderr, "src/unrelated.gleam")
}

pub fn materialize_commit_stack_revision_writes_existing_pr_publication_target_test() {
  let dir = "test/tmp/execplan-materialize-commit-stack-target"
  let review_path = "docs/plans/LIV-910-plan.md"
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  write_valid_review_doc(dir <> "/" <> review_path)
  let assert Ok(Nil) =
    simplifile.write(dir <> "/review.path", review_path <> "\n")
  write_fake_commit_stack_driver(dir <> "/bin/fake-driver", review_path)
  write_fake_commit_stack_jj(dir <> "/bin/jj")
  write_fake_commit_stack_git(dir <> "/bin/git")
  test_helpers.chmod_executable(dir <> "/bin/fake-driver")
  test_helpers.chmod_executable(dir <> "/bin/jj")
  test_helpers.chmod_executable(dir <> "/bin/git")
  let previous_bundle =
    write_materialize_previous_bundle_with_surface_head(
      dir,
      "2222222222222222222222222222222222222222",
    )

  let artifact =
    run_shell_in(
      dir,
      "env SCHERZO_WORKSPACE_DRIVER=./bin/fake-driver SCHERZO_GITHUB_REPO=example/repo SCHERZO_JJ_WORKSPACE_BASE_BRANCH=main SCHERZO_RUN_ID=run-1 PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-execplan materialize-commit-stack --review-doc-path-file review.path --previous-bundle "
        <> test_helpers.shell_quote(previous_bundle)
        <> " --target-output tmp/execplan-publication-target.json --output tmp/execplan-commit-stack.json",
    )

  assert artifact.status == step_artifact.StepSucceeded
  let assert Ok(target) =
    simplifile.read(dir <> "/tmp/execplan-publication-target.json")
  assert string.contains(
    target,
    "\"artifact_type\": \"scherzo.github_publication_target.v1\"",
  )
  assert string.contains(target, "\"kind\": \"existing_pr_branch\"")
  assert string.contains(target, "\"branch\": \"execplan/liv-314\"")
  assert string.contains(
    target,
    "\"url\": \"https://github.com/living-systems/scherzo/pull/314\"",
  )
  assert string.contains(
    target,
    "\"sha\": \"2222222222222222222222222222222222222222\"",
  )
}

pub fn materialize_commit_stack_revision_prefers_review_surface_over_conflicting_retained_manifest_test() {
  let dir = "test/tmp/execplan-materialize-commit-stack-prefers-surface"
  let review_path = "docs/plans/LIV-910-plan.md"
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  write_valid_review_doc(dir <> "/" <> review_path)
  let assert Ok(Nil) =
    simplifile.write(dir <> "/review.path", review_path <> "\n")
  write_fake_commit_stack_driver(dir <> "/bin/fake-driver", review_path)
  write_fake_commit_stack_jj(dir <> "/bin/jj")
  write_fake_commit_stack_git(dir <> "/bin/git")
  test_helpers.chmod_executable(dir <> "/bin/fake-driver")
  test_helpers.chmod_executable(dir <> "/bin/jj")
  test_helpers.chmod_executable(dir <> "/bin/git")
  let previous_bundle =
    write_materialize_previous_bundle_with_surface_head(
      dir,
      "2222222222222222222222222222222222222222",
    )
  write_execplan_publication_manifest(
    dir,
    "run-1",
    "published",
    "scherzo/execplan/LIV-314/execplan_review_doc",
    "https://github.com/example/repo/pull/314",
    "3333333333333333333333333333333333333333",
  )

  let artifact =
    run_shell_in(
      dir,
      "env SCHERZO_WORKSPACE_DRIVER=./bin/fake-driver SCHERZO_GITHUB_REPO=example/repo SCHERZO_JJ_WORKSPACE_BASE_BRANCH=main SCHERZO_RUN_ID=run-1 PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-execplan materialize-commit-stack --review-doc-path-file review.path --previous-bundle "
        <> test_helpers.shell_quote(previous_bundle)
        <> " --target-output tmp/execplan-publication-target.json --output tmp/execplan-commit-stack.json",
    )

  assert artifact.status == step_artifact.StepSucceeded
  let assert Ok(target) =
    simplifile.read(dir <> "/tmp/execplan-publication-target.json")
  assert string.contains(target, "\"kind\": \"existing_pr_branch\"")
  assert string.contains(target, "\"branch\": \"execplan/liv-314\"")
  assert string.contains(
    target,
    "\"url\": \"https://github.com/living-systems/scherzo/pull/314\"",
  )
  assert !string.contains(
    target,
    "scherzo/execplan/LIV-314/execplan_review_doc",
  )
  assert !string.contains(target, "https://github.com/example/repo/pull/314")
}

pub fn materialize_commit_stack_revision_uses_retained_publication_manifest_when_surface_missing_test() {
  let dir = "test/tmp/execplan-materialize-commit-stack-retained-target"
  let review_path = "docs/plans/LIV-910-plan.md"
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  write_valid_review_doc(dir <> "/" <> review_path)
  let assert Ok(Nil) =
    simplifile.write(dir <> "/review.path", review_path <> "\n")
  write_fake_commit_stack_driver(dir <> "/bin/fake-driver", review_path)
  write_fake_commit_stack_jj(dir <> "/bin/jj")
  write_fake_commit_stack_git(dir <> "/bin/git")
  test_helpers.chmod_executable(dir <> "/bin/fake-driver")
  test_helpers.chmod_executable(dir <> "/bin/jj")
  test_helpers.chmod_executable(dir <> "/bin/git")
  let assert Ok(source_bundle) =
    simplifile.read("test/fixtures/execplan_v2/exec-plan-bundle.valid.json")
  let previous_bundle = dir <> "/previous-bundle.json"
  let without_surface = bundle_without_review_surface_publication(source_bundle)
  let assert Ok(Nil) = simplifile.write(previous_bundle, without_surface)
  write_execplan_publication_manifest(
    dir,
    "run-1",
    "published",
    "scherzo/execplan/LIV-314/execplan_review_doc",
    "https://github.com/example/repo/pull/314",
    "3333333333333333333333333333333333333333",
  )

  let artifact =
    run_shell_in(
      dir,
      "env SCHERZO_WORKSPACE_DRIVER=./bin/fake-driver SCHERZO_GITHUB_REPO=example/repo SCHERZO_JJ_WORKSPACE_BASE_BRANCH=main SCHERZO_RUN_ID=run-1 PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-execplan materialize-commit-stack --review-doc-path-file review.path --previous-bundle previous-bundle.json --target-output tmp/execplan-publication-target.json --output tmp/execplan-commit-stack.json",
    )

  assert artifact.status == step_artifact.StepSucceeded
  let assert Ok(target) =
    simplifile.read(dir <> "/tmp/execplan-publication-target.json")
  assert string.contains(target, "\"kind\": \"existing_pr_branch\"")
  assert string.contains(
    target,
    "\"branch\": \"scherzo/execplan/LIV-314/execplan_review_doc\"",
  )
  assert string.contains(
    target,
    "\"url\": \"https://github.com/example/repo/pull/314\"",
  )
  assert string.contains(
    target,
    "\"sha\": \"3333333333333333333333333333333333333333\"",
  )
}

pub fn materialize_commit_stack_revision_ignores_unsafe_retained_publication_manifest_test() {
  let dir = "test/tmp/execplan-materialize-commit-stack-unsafe-retained-target"
  let review_path = "docs/plans/LIV-910-plan.md"
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  write_valid_review_doc(dir <> "/" <> review_path)
  let assert Ok(Nil) =
    simplifile.write(dir <> "/review.path", review_path <> "\n")
  write_fake_commit_stack_driver(dir <> "/bin/fake-driver", review_path)
  write_fake_commit_stack_jj(dir <> "/bin/jj")
  write_fake_commit_stack_git(dir <> "/bin/git")
  test_helpers.chmod_executable(dir <> "/bin/fake-driver")
  test_helpers.chmod_executable(dir <> "/bin/jj")
  test_helpers.chmod_executable(dir <> "/bin/git")
  let assert Ok(source_bundle) =
    simplifile.read("test/fixtures/execplan_v2/exec-plan-bundle.valid.json")
  let previous_bundle = dir <> "/previous-bundle.json"
  let without_surface = bundle_without_review_surface_publication(source_bundle)
  let assert Ok(Nil) = simplifile.write(previous_bundle, without_surface)
  write_execplan_publication_manifest(
    dir,
    "run-1",
    "published",
    "../not-safe",
    "https://github.com/example/repo/pull/314",
    "3333333333333333333333333333333333333333",
  )

  let artifact =
    run_shell_in(
      dir,
      "env SCHERZO_WORKSPACE_DRIVER=./bin/fake-driver SCHERZO_GITHUB_REPO=example/repo SCHERZO_JJ_WORKSPACE_BASE_BRANCH=main SCHERZO_RUN_ID=run-1 PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-execplan materialize-commit-stack --review-doc-path-file review.path --previous-bundle previous-bundle.json --target-output tmp/execplan-publication-target.json --output tmp/execplan-commit-stack.json",
    )

  assert artifact.status == step_artifact.StepSucceeded
  let assert Ok(target) =
    simplifile.read(dir <> "/tmp/execplan-publication-target.json")
  assert string.contains(target, "\"kind\": \"stable_branch\"")
  assert !string.contains(target, "existing_pr_branch")
}

pub fn materialize_commit_stack_revision_falls_back_to_stable_branch_target_test() {
  let dir = "test/tmp/execplan-materialize-commit-stack-stable-target"
  let review_path = "docs/plans/LIV-910-plan.md"
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  write_valid_review_doc(dir <> "/" <> review_path)
  let assert Ok(Nil) =
    simplifile.write(dir <> "/review.path", review_path <> "\n")
  write_fake_commit_stack_driver(dir <> "/bin/fake-driver", review_path)
  write_fake_commit_stack_jj(dir <> "/bin/jj")
  write_fake_commit_stack_git(dir <> "/bin/git")
  test_helpers.chmod_executable(dir <> "/bin/fake-driver")
  test_helpers.chmod_executable(dir <> "/bin/jj")
  test_helpers.chmod_executable(dir <> "/bin/git")
  let assert Ok(source_bundle) =
    simplifile.read("test/fixtures/execplan_v2/exec-plan-bundle.valid.json")
  let previous_bundle = dir <> "/previous-bundle.json"
  let without_pr =
    source_bundle
    |> string.replace(
      each: "    \"branch\": \"execplan/liv-314\",\n",
      with: "    \"branch\": null,\n",
    )
    |> string.replace(
      each: "    \"pr_url\": \"https://github.com/living-systems/scherzo/pull/314\",\n",
      with: "    \"pr_url\": null,\n",
    )
  let assert Ok(Nil) = simplifile.write(previous_bundle, without_pr)

  let artifact =
    run_shell_in(
      dir,
      "env SCHERZO_WORKSPACE_DRIVER=./bin/fake-driver SCHERZO_GITHUB_REPO=example/repo SCHERZO_JJ_WORKSPACE_BASE_BRANCH=main SCHERZO_RUN_ID=run-1 PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-execplan materialize-commit-stack --review-doc-path-file review.path --previous-bundle "
        <> test_helpers.shell_quote("previous-bundle.json")
        <> " --target-output tmp/execplan-publication-target.json --output tmp/execplan-commit-stack.json",
    )

  assert artifact.status == step_artifact.StepSucceeded
  let assert Ok(target) =
    simplifile.read(dir <> "/tmp/execplan-publication-target.json")
  assert string.contains(
    target,
    "\"artifact_type\": \"scherzo.github_publication_target.v1\"",
  )
  assert string.contains(target, "\"kind\": \"stable_branch\"")
  assert !string.contains(target, "existing_pr_branch")
}

pub fn publish_review_doc_command_is_removed_test() {
  let artifact =
    run_shell(".scherzo/workflows/scripts/scherzo-execplan publish-review-doc")

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(2)
  assert string.contains(artifact.stderr, "Usage: scherzo-execplan")
  assert !string.contains(artifact.stdout, "PUBLISH_REVIEW_DOC_STATUS")
}

pub fn materialize_bundle_without_publish_context_emits_destination_metadata_test() {
  let dir = "test/tmp/execplan-v2-materialize-no-publish-context"
  test_helpers.reset_dir(dir)
  let path_file = dir <> "/review.path"
  let output = dir <> "/bundle.json"
  write_valid_review_doc(dir <> "/docs/custom/plan.md")
  let assert Ok(Nil) =
    simplifile.write(path_file, dir <> "/docs/custom/plan.md\n")

  let artifact =
    run_shell(
      "env SCHERZO_EXECPLAN_OFFLINE_LINEAR=1 SCHERZO_RUN_ID=run-no-publish-context .scherzo/workflows/scripts/scherzo-execplan materialize-bundle --review-doc-path-file "
      <> test_helpers.shell_quote(path_file)
      <> " --pack test/fixtures/execplan_v2/implementation-pack.valid.json --output "
      <> test_helpers.shell_quote(output),
    )

  assert artifact.status == step_artifact.StepSucceeded
  let assert Ok(bundle) = simplifile.read(output)
  assert string.contains(
    bundle,
    "\"destination_path\": \"test/tmp/execplan-v2-materialize-no-publish-context/docs/custom/plan.md\"",
  )
  assert string.contains(bundle, "\"status\": \"not_applicable\"")
  assert string.contains(bundle, "\"pr_url\": null")
  assert string.contains(bundle, "\"branch\": null")
}

pub fn materialize_revision_without_publish_context_emits_not_applicable_review_surface_test() {
  let dir = "test/tmp/execplan-v2-revision-no-publish-context"
  test_helpers.reset_dir(dir)
  let path_file = dir <> "/review.path"
  let output = dir <> "/bundle.json"
  let assert Ok(Nil) =
    simplifile.write(
      path_file,
      "test/fixtures/execplan_v2/review-doc.valid.md\n",
    )

  let artifact =
    run_shell(
      "env SCHERZO_EXECPLAN_OFFLINE_LINEAR=1 SCHERZO_RUN_ID=run-revision-no-publish-context .scherzo/workflows/scripts/scherzo-execplan materialize-revision --previous-bundle test/fixtures/execplan_v2/exec-plan-bundle.valid.json --review-doc-path-file "
      <> test_helpers.shell_quote(path_file)
      <> " --pack test/fixtures/execplan_v2/implementation-pack.valid.json --status auto --output "
      <> test_helpers.shell_quote(output),
    )

  assert artifact.status == step_artifact.StepSucceeded
  let assert Ok(bundle) = simplifile.read(output)
  assert string.contains(bundle, "\"status\": \"unchanged\"")
  assert string.contains(bundle, "\"review_surface\": {")
  assert string.contains(bundle, "\"status\": \"not_applicable\"")
  assert string.contains(bundle, "\"pr_url\": null")
  assert string.contains(bundle, "\"branch\": null")
  assert string.contains(
    bundle,
    "\"destination_path\": \"test/fixtures/execplan_v2/review-doc.valid.md\"",
  )
}

pub fn materialize_bundle_prefers_pack_source_issue_over_publish_context_test() {
  let dir = "test/tmp/execplan-v2-materialize-pack-source"
  test_helpers.reset_dir(dir)
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
      <> test_helpers.shell_quote(path_file)
      <> " --pack test/fixtures/execplan_v2/implementation-pack.valid.json --publish-context "
      <> test_helpers.shell_quote(context_path)
      <> " --output "
      <> test_helpers.shell_quote(output),
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
  test_helpers.reset_dir(dir)
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
      <> test_helpers.shell_quote(path_file)
      <> " --pack test/fixtures/execplan_v2/implementation-pack.valid.json --publish-context "
      <> test_helpers.shell_quote(context_path)
      <> " --output "
      <> test_helpers.shell_quote(output),
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
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  let log = dir <> "/linear.log"
  let update_desc = dir <> "/updated-description.md"
  let linear = dir <> "/bin/linear"
  let assert Ok(Nil) =
    simplifile.write(
      linear,
      "#!/bin/sh\n"
        <> "printf '%s\\n' \"$*\" >> "
        <> test_helpers.shell_quote(log)
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
        <> test_helpers.shell_quote(update_desc)
        <> "\n"
        <> "  grep -Eq '^Bundle sha256: [a-f0-9]{64}$' \"$desc\" || { echo 'missing final bundle sha' >&2; exit 3; }\n"
        <> "  grep -q '^Bundle sha256: pending$' \"$desc\" && { echo 'pending bundle sha' >&2; exit 4; }\n"
        <> "  exit 0\n"
        <> "fi\n"
        <> "if [ \"$1 $2 $3\" = 'issue comment add' ]; then exit 0; fi\n"
        <> "exit 1\n",
    )
  let chmod = run_shell("chmod +x " <> test_helpers.shell_quote(linear))
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
      <> test_helpers.shell_quote(dir <> "/bin")
      <> ":$PATH SCHERZO_RUN_ID=run-online .scherzo/workflows/scripts/scherzo-execplan materialize-bundle --review-doc-path-file "
      <> test_helpers.shell_quote(path_file)
      <> " --pack test/fixtures/execplan_v2/implementation-pack.valid.json --publish-context "
      <> test_helpers.shell_quote(context_path)
      <> " --output "
      <> test_helpers.shell_quote(output),
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
  test_helpers.reset_dir(dir)
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
        <> "  \"pr\": {\"url\": \"https://github.com/living-systems/scherzo/pull/314\", \"branch\": \"execplan/liv-314\", \"base_revision\": \"reused\", \"head_revision\": \"reused\"},\n"
        <> "  \"review_surface\": {\"status\": \"reused\", \"source_bundle_ref\": \"runs/run-1/outputs/exec_plan_bundle.json\", \"head_revision\": \"reused\"}\n"
        <> "}\n",
    )

  let revision =
    run_shell(
      "env SCHERZO_EXECPLAN_OFFLINE_LINEAR=1 SCHERZO_RUN_ID=run-revision .scherzo/workflows/scripts/scherzo-execplan materialize-revision --previous-bundle test/fixtures/execplan_v2/exec-plan-bundle.valid.json --review-doc-path-file "
      <> test_helpers.shell_quote(path_file)
      <> " --pack test/fixtures/execplan_v2/implementation-pack.valid.json --publish-context "
      <> test_helpers.shell_quote(context_path)
      <> " --status auto --output "
      <> test_helpers.shell_quote(output),
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
    "\"sha256\": \"591e1f2fff461dfb467f27c21122b1dea94cf903946acf5c644937b1b1f9afdc\"",
  )
  assert string.contains(bundle, "\"head_revision\": \"reused\"")
}

pub fn materialize_revision_prefers_pack_source_issue_title_and_url_test() {
  let dir = "test/tmp/execplan-v2-revision-pack-source"
  test_helpers.reset_dir(dir)
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

  let assert Ok(Nil) =
    simplifile.write(
      context_path,
      "{\n"
        <> "  \"artifact_type\": \"execplan_v2_publish_context\",\n"
        <> "  \"source_issue\": {\"identifier\": \"LIV-314\", \"title\": \"Untitled source task\", \"url\": \"https://linear.app/living-systems/issue/LIV-314\"},\n"
        <> "  \"pr\": {\"url\": \"https://github.com/living-systems/scherzo/pull/314\", \"branch\": \"execplan/liv-314\", \"base_revision\": \"reused\", \"head_revision\": \"reused\"},\n"
        <> "  \"review_surface\": {\"status\": \"reused\", \"source_bundle_ref\": \"runs/run-1/outputs/exec_plan_bundle.json\", \"head_revision\": \"reused\"}\n"
        <> "}\n",
    )

  let revision =
    run_shell(
      "env SCHERZO_EXECPLAN_OFFLINE_LINEAR=1 SCHERZO_RUN_ID=run-revision-pack-source .scherzo/workflows/scripts/scherzo-execplan materialize-revision --previous-bundle "
      <> test_helpers.shell_quote(previous_bundle)
      <> " --review-doc-path-file "
      <> test_helpers.shell_quote(path_file)
      <> " --pack test/fixtures/execplan_v2/implementation-pack.valid.json --publish-context "
      <> test_helpers.shell_quote(context_path)
      <> " --status auto --output "
      <> test_helpers.shell_quote(output),
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
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  let log = dir <> "/linear.log"
  let update_desc = dir <> "/updated-description.md"
  let linear = dir <> "/bin/linear"
  let assert Ok(Nil) =
    simplifile.write(
      linear,
      "#!/bin/sh\n"
        <> "printf '%s\\n' \"$*\" >> "
        <> test_helpers.shell_quote(log)
        <> "\n"
        <> "if [ \"$1 $2 $3\" = 'issue update LIV-315' ]; then\n"
        <> "  desc=''\n"
        <> "  prev=''\n"
        <> "  for arg in \"$@\"; do if [ \"$prev\" = --description-file ]; then desc=$arg; fi; prev=$arg; done\n"
        <> "  if [ -z \"$desc\" ]; then echo 'missing description file' >&2; exit 2; fi\n"
        <> "  cp \"$desc\" "
        <> test_helpers.shell_quote(update_desc)
        <> "\n"
        <> "  grep -q '^Bundle ref: runs/run-revision-update/outputs/exec_plan_bundle.json$' \"$desc\" || { echo 'missing revised bundle ref' >&2; exit 3; }\n"
        <> "  grep -Eq '^Bundle sha256: [a-f0-9]{64}$' \"$desc\" || { echo 'missing revised bundle sha' >&2; exit 4; }\n"
        <> "  grep -q '^Bundle sha256: pending$' \"$desc\" && { echo 'pending bundle sha' >&2; exit 5; }\n"
        <> "  exit 0\n"
        <> "fi\n"
        <> "if [ \"$1 $2 $3 $4\" = 'issue comment add LIV-315' ]; then exit 0; fi\n"
        <> "exit 1\n",
    )
  let chmod = run_shell("chmod +x " <> test_helpers.shell_quote(linear))
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
      <> test_helpers.shell_quote(dir <> "/bin")
      <> ":$PATH SCHERZO_RUN_ID=run-revision-update .scherzo/workflows/scripts/scherzo-execplan materialize-revision --previous-bundle test/fixtures/execplan_v2/exec-plan-bundle.valid.json --review-doc-path-file "
      <> test_helpers.shell_quote(path_file)
      <> " --pack test/fixtures/execplan_v2/implementation-pack.valid.json --publish-context "
      <> test_helpers.shell_quote(context_path)
      <> " --status auto --output "
      <> test_helpers.shell_quote(output),
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
  test_helpers.reset_dir(dir)
  let artifact_root = dir <> "/artifacts/runs/run-2"
  let run_root = dir <> "/run-root"
  let assert Ok(Nil) = simplifile.create_directory_all(artifact_root)
  let assert Ok(Nil) =
    simplifile.create_directory_all(
      run_root <> "/artifacts/review/final_dispositions",
    )
  let assert Ok(Nil) =
    simplifile.create_directory_all(run_root <> "/state/implementation")
  let assert Ok(Nil) = simplifile.create_directory_all("tmp")
  let assert Ok(Nil) =
    simplifile.write(
      run_root <> "/state/implementation/scherzo-implementation-publish.json",
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
      run_root <> "/state/implementation/scherzo-implementation-validation.json",
      "{\"status\":\"passed\"}\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      run_root <> "/state/implementation/scherzo-plan-completion-verdict.json",
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
      <> test_helpers.shell_quote(artifact_root)
      <> " SCHERZO_RUN_ROOT="
      <> test_helpers.shell_quote(run_root)
      <> " SCHERZO_EXECPLAN_DIFF_PATH=test/fixtures/execplan_v2/artifacts/runs/run-2/execplan/code-change/diff.patch .scherzo/workflows/scripts/scherzo-execplan materialize-code-change-bundle --bundle test/fixtures/execplan_v2/exec-plan-bundle.valid.json --output "
      <> test_helpers.shell_quote(output),
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  let assert Ok(bundle) = simplifile.read(output)
  assert string.contains(
    bundle,
    "\"artifact_type\": \"scherzo.code_change_bundle.v2\"",
  )
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

pub fn materialize_code_change_bundle_uses_run_root_artifact_store_test() {
  let dir = "test/tmp/execplan-code-change-run-root-store"
  test_helpers.reset_dir(dir)
  let run_root = dir <> "/run-root"
  let durable_run_dir = run_root <> "/.scherzo-state/artifacts/runs/run-durable"
  let assert Ok(Nil) = simplifile.create_directory_all(durable_run_dir)
  let assert Ok(Nil) =
    simplifile.create_directory_all(
      run_root <> "/artifacts/review/final_dispositions",
    )
  let assert Ok(Nil) =
    simplifile.create_directory_all(run_root <> "/state/implementation")
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/tmp")
  let assert Ok(Nil) =
    simplifile.write(
      run_root <> "/state/implementation/scherzo-implementation-publish.json",
      "{\"changed_files\":[\"src/example.gleam\"]}\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      run_root <> "/state/implementation/scherzo-implementation-validation.json",
      "{\"status\":\"passed\"}\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      run_root <> "/state/implementation/scherzo-plan-completion-verdict.json",
      "{\"verdict\":\"pass\"}\n",
    )
  let assert Ok(Nil) = simplifile.write(dir <> "/input.diff", "diff\n")
  let output = "tmp/execplan-code-change-bundle.json"

  let artifact =
    run_shell_in(
      dir,
      "env SCHERZO_RUN_ID=run-durable SCHERZO_RUN_ROOT="
        <> test_helpers.shell_quote("run-root")
        <> " SCHERZO_EXECPLAN_DIFF_PATH=input.diff ../../../.scherzo/workflows/scripts/scherzo-execplan materialize-code-change-bundle --bundle ../../../test/fixtures/execplan_v2/exec-plan-bundle.valid.json --output "
        <> test_helpers.shell_quote(output),
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  let assert Ok(bundle) = simplifile.read(dir <> "/" <> output)
  assert string.contains(
    bundle,
    "runs/run-durable/execplan/code-change/diff.patch",
  )
  let assert Ok(True) =
    simplifile.is_file(durable_run_dir <> "/execplan/code-change/diff.patch")
  let assert Ok(True) =
    simplifile.is_file(
      durable_run_dir <> "/execplan/code-change/validation/scherzo-ci.json",
    )
  let assert Ok(False) =
    simplifile.is_directory(dir <> "/.scherzo-state/artifacts")
}

pub fn materialize_code_change_bundle_no_env_local_store_keeps_stable_refs_test() {
  let dir = "test/tmp/execplan-code-change-local-store"
  test_helpers.reset_dir(dir)
  let local_run_dir = dir <> "/.scherzo-state/artifacts/runs/run-local"
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/tmp")
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/tmp/scherzo-implementation-publish.json",
      "{\"changed_files\":[\"src/example.gleam\"]}\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/tmp/scherzo-implementation-validation.json",
      "{\"status\":\"passed\"}\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/tmp/scherzo-plan-completion-verdict.json",
      "{\"verdict\":\"pass\"}\n",
    )
  let assert Ok(Nil) = simplifile.write(dir <> "/input.diff", "diff\n")
  let output = "tmp/execplan-code-change-bundle.json"

  let artifact =
    run_shell_in(
      dir,
      "env -u SCHERZO_RUN_ROOT -u SCHERZO_REPO_ROOT -u SCHERZO_RUN_ARTIFACT_DIR SCHERZO_RUN_ID=run-local SCHERZO_EXECPLAN_DIFF_PATH=input.diff ../../../.scherzo/workflows/scripts/scherzo-execplan materialize-code-change-bundle --bundle ../../../test/fixtures/execplan_v2/exec-plan-bundle.valid.json --output "
        <> test_helpers.shell_quote(output),
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  let assert Ok(bundle) = simplifile.read(dir <> "/" <> output)
  assert string.contains(
    bundle,
    "runs/run-local/execplan/code-change/diff.patch",
  )
  let assert Ok(True) =
    simplifile.is_file(local_run_dir <> "/execplan/code-change/diff.patch")
  let assert Ok(True) =
    simplifile.is_file(
      local_run_dir <> "/execplan/code-change/validation/scherzo-ci.json",
    )
}
