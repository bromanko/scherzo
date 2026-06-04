import gleam/bit_array
import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/artifact_publication_config
import scherzo/artifact_publication_manifest
import scherzo/artifact_publication_planner
import scherzo/artifact_repository/command_runner
import scherzo/artifact_repository/github
import scherzo/artifact_repository/types as repository_types
import scherzo/hash
import scherzo/path
import scherzo/state/artifact_store
import scherzo/state/ledger
import scherzo/state/record
import scherzo/workflow_contract
import scherzo/workflow_contract_manifest
import simplifile
import support/test_helpers

pub fn prepare_publication_input_reads_selected_artifact_bytes_test() {
  let store = store_with_contents([#(plan_ref(), plan_contents())])
  let assert Ok(manifest) = planned_manifest(True, store)
  let assert Ok(prepared) = github.prepare_publication_input(manifest, store)

  let assert [selected] = prepared.selected_files
  assert destination_path(selected) == "docs/plans/LIV-761.md"
  assert selected_bytes(selected) == bit_array.from_string(plan_contents())
}

pub fn prepare_publication_input_fails_when_artifact_bytes_are_missing_test() {
  let planning_store = store_with_contents([#(plan_ref(), plan_contents())])
  let assert Ok(manifest) = planned_manifest(True, planning_store)

  let execution_store = store_with_contents([])
  let assert Error(error) =
    github.prepare_publication_input(manifest, execution_store)

  assert github.code(error) == "artifact_read_failed"
}

pub fn publish_clones_materializes_commits_pushes_and_creates_draft_pr_test() {
  let root = "test/tmp/artifact-repository-github/draft-create"
  let log = root <> "/commands.log"
  test_helpers.reset_dir(root)
  let input = prepared_input(root, True)

  let manifest = github.publish(input, root, create_pr_runner(log, True), 123)

  assert artifact_publication_manifest.status_to_string(manifest.status)
    == "published"
  assert manifest.commit_sha == Some("deadbeef")
  assert manifest.pr_url == Some("https://example.test/pr/1")
  let transcript = read_file(log)
  assert string.contains(
    transcript,
    "git clone https://github.com/scherzo-systems/scherzo.git",
  )
  assert string.contains(transcript, "git fetch origin main")
  assert string.contains(
    transcript,
    "git ls-remote --exit-code --heads origin scherzo/workflow.execplan/LIV-761/review_doc",
  )
  assert string.contains(
    transcript,
    "git checkout -B scherzo/workflow.execplan/LIV-761/review_doc origin/main",
  )
  assert string.contains(transcript, "git add -- docs/plans/LIV-761.md")
  assert string.contains(
    transcript,
    "git commit -m scherzo publication workflow.execplan/review_doc ",
  )
  assert string.contains(
    transcript,
    "git push origin scherzo/workflow.execplan/LIV-761/review_doc",
  )
  assert string.contains(transcript, "gh pr create")
  assert string.contains(transcript, "--draft")
  let checkout = checkout_root(root)
  assert string.contains(
    checkout,
    ".scherzo-state/artifact-repositories/github/",
  )
}

pub fn publish_reuses_checkout_and_omits_draft_flag_when_disabled_test() {
  let root = "test/tmp/artifact-repository-github/non-draft-create"
  let log = root <> "/commands.log"
  test_helpers.reset_dir(root)
  seed_checkout(root)
  let input = prepared_input(root, False)

  let manifest = github.publish(input, root, reuse_checkout_runner(log), 123)

  assert artifact_publication_manifest.status_to_string(manifest.status)
    == "published"
  let transcript = read_file(log)
  assert !string.contains(transcript, "git clone")
  assert string.contains(transcript, "git remote get-url origin")
  assert string.contains(transcript, "gh pr create")
  assert !string.contains(transcript, "--draft")
}

pub fn publish_returns_unchanged_without_commands_when_latest_version_matches_test() {
  let root = "test/tmp/artifact-repository-github/unchanged"
  test_helpers.reset_dir(root)
  seed_latest_publication(root)
  let input = prepared_input(root, True)

  let manifest = github.publish(input, root, fail_if_called_runner(), 123)

  assert artifact_publication_manifest.status_to_string(manifest.status)
    == "unchanged"
  assert manifest.commit_sha == Some("deadbeef")
  assert manifest.pr_url == Some("https://example.test/pr/1")
}

pub fn publish_recovers_pr_when_latest_version_matches_without_pr_test() {
  let root = "test/tmp/artifact-repository-github/unchanged-missing-pr"
  let log = root <> "/commands.log"
  test_helpers.reset_dir(root)
  seed_latest_publication_without_pr(root)
  let input = prepared_input(root, True)

  let manifest = github.publish(input, root, no_diff_runner(log), 123)

  assert artifact_publication_manifest.status_to_string(manifest.status)
    == "unchanged"
  assert string.starts_with(manifest.attempt_id, "recovered-")
  assert manifest.commit_sha == Some("deadbeef")
  assert manifest.pr_url == Some("https://example.test/pr/1")
  let transcript = read_file(log)
  assert string.contains(transcript, "gh pr create")
  assert !string.contains(transcript, "git commit")
}

pub fn publish_fails_when_checkout_is_dirty_test() {
  let root = "test/tmp/artifact-repository-github/dirty-checkout"
  let log = root <> "/commands.log"
  test_helpers.reset_dir(root)
  seed_checkout(root)
  let input = prepared_input(root, True)

  let manifest = github.publish(input, root, dirty_checkout_runner(log), 123)

  assert artifact_publication_manifest.status_to_string(manifest.status)
    == "failed"
  let assert Some(error) = manifest.error
  assert error.code == "dirty_checkout"
}

pub fn publish_fails_for_duplicate_destination_paths_before_running_commands_test() {
  let root = "test/tmp/artifact-repository-github/duplicate-destinations"
  test_helpers.reset_dir(root)
  let input =
    prepared_input_for_manifest(root, duplicate_destination_manifest())

  let manifest = github.publish(input, root, fail_if_called_runner(), 123)

  assert artifact_publication_manifest.status_to_string(manifest.status)
    == "failed"
  let assert Some(error) = manifest.error
  assert error.code == "duplicate_destination_path"
}

pub fn publish_fails_when_git_add_fails_test() {
  let root = "test/tmp/artifact-repository-github/git-add-fails"
  let log = root <> "/commands.log"
  test_helpers.reset_dir(root)
  let input = prepared_input(root, True)

  let manifest = github.publish(input, root, git_add_failure_runner(log), 123)

  assert artifact_publication_manifest.status_to_string(manifest.status)
    == "failed"
  let assert Some(error) = manifest.error
  assert error.code == "git_add_failed"
}

pub fn publish_fails_when_pr_lookup_is_ambiguous_test() {
  let root = "test/tmp/artifact-repository-github/pr-ambiguous"
  let log = root <> "/commands.log"
  test_helpers.reset_dir(root)
  let input = prepared_input(root, True)

  let manifest = github.publish(input, root, ambiguous_pr_runner(log), 123)

  assert artifact_publication_manifest.status_to_string(manifest.status)
    == "failed"
  let assert Some(error) = manifest.error
  assert error.code == "pr_ambiguous"
}

pub fn publish_fails_when_pr_lookup_json_is_malformed_test() {
  let root = "test/tmp/artifact-repository-github/pr-json-malformed"
  let log = root <> "/commands.log"
  test_helpers.reset_dir(root)
  let input = prepared_input(root, True)

  let manifest = github.publish(input, root, malformed_pr_runner(log), 123)

  assert artifact_publication_manifest.status_to_string(manifest.status)
    == "failed"
  let assert Some(error) = manifest.error
  assert error.code == "pr_json_malformed"
}

pub fn publish_fails_when_pr_edit_fails_after_existing_pr_found_test() {
  let root = "test/tmp/artifact-repository-github/pr-edit-fails"
  let log = root <> "/commands.log"
  test_helpers.reset_dir(root)
  let input = prepared_input(root, True)

  let manifest = github.publish(input, root, pr_edit_failure_runner(log), 123)

  assert artifact_publication_manifest.status_to_string(manifest.status)
    == "failed"
  let assert Some(error) = manifest.error
  assert error.code == "pr_edit_failed"
  assert string.contains(read_file(log), "gh pr edit 42")
}

pub fn publish_retries_failed_latest_attempt_instead_of_short_circuiting_test() {
  let root = "test/tmp/artifact-repository-github/retry-failed-latest"
  let log = root <> "/commands.log"
  test_helpers.reset_dir(root)
  seed_latest_failed_publication(root)
  let input = prepared_input(root, True)

  let manifest = github.publish(input, root, create_pr_runner(log, True), 123)

  assert artifact_publication_manifest.status_to_string(manifest.status)
    == "published"
  assert string.contains(read_file(log), "git clone")
}

pub fn prepare_publication_input_fails_on_hash_and_byte_mismatch_test() {
  let root = "test/tmp/artifact-repository-github/hash-byte-mismatch"
  test_helpers.reset_dir(root)
  let store = store_with_contents([#(plan_ref(), plan_contents())])
  let assert Ok(base) = planned_manifest(True, store)

  let assert Error(hash_error) =
    github.prepare_publication_input(
      manifest_with_source_hash(base, "bad"),
      store,
    )
  assert github.code(hash_error) == "hash_mismatch"

  let assert Error(bytes_error) =
    github.prepare_publication_input(
      manifest_with_source_bytes(base, 999),
      store,
    )
  assert github.code(bytes_error) == "bytes_mismatch"
}

pub fn publish_writes_binary_artifact_bytes_exactly_test() {
  let root = "test/tmp/artifact-repository-github/binary-bytes"
  let log = root <> "/commands.log"
  test_helpers.reset_dir(root)
  let bytes = <<0, 255, 10, 128>>
  let input = prepared_binary_input(root, bytes)

  let manifest = github.publish(input, root, create_pr_runner(log, True), 123)

  assert artifact_publication_manifest.status_to_string(manifest.status)
    == "published"
  let assert Ok(written) =
    simplifile.read_bits(checkout_file(root, "docs/plans/LIV-761.md"))
  assert written == bytes
}

pub fn publish_reports_git_and_gh_failure_codes_test() {
  assert publish_failure_code("clone", "clone") == "git_clone_failed"
  assert publish_failure_code("fetch", "fetch") == "git_fetch_failed"
  assert publish_failure_code("checkout", "checkout") == "git_checkout_failed"
  assert publish_failure_code("status", "status") == "git_status_failed"
  assert publish_failure_code("diff", "diff") == "git_diff_failed"
  assert publish_failure_code("commit", "commit") == "git_commit_failed"
  assert publish_failure_code("push", "push") == "git_push_failed"
  assert publish_failure_code("pr-create", "pr_create") == "pr_create_failed"
}

pub fn publish_fails_when_reused_checkout_remote_mismatches_test() {
  let root = "test/tmp/artifact-repository-github/remote-mismatch"
  let log = root <> "/commands.log"
  test_helpers.reset_dir(root)
  seed_checkout(root)
  let input = prepared_input(root, True)

  let manifest = github.publish(input, root, remote_mismatch_runner(log), 123)

  assert artifact_publication_manifest.status_to_string(manifest.status)
    == "failed"
  let assert Some(error) = manifest.error
  assert error.code == "remote_mismatch"
}

pub fn publish_fails_for_unsafe_destination_before_running_commands_test() {
  let root = "test/tmp/artifact-repository-github/unsafe-destination"
  test_helpers.reset_dir(root)
  let input = prepared_input_for_manifest(root, unsafe_destination_manifest())

  let manifest = github.publish(input, root, fail_if_called_runner(), 123)

  assert artifact_publication_manifest.status_to_string(manifest.status)
    == "failed"
  let assert Some(error) = manifest.error
  assert error.code == "unsafe_destination_path"
}

pub fn publish_records_unchanged_when_materialization_has_no_diff_test() {
  let root = "test/tmp/artifact-repository-github/no-diff"
  let log = root <> "/commands.log"
  test_helpers.reset_dir(root)
  let input = prepared_input(root, True)

  let manifest = github.publish(input, root, no_diff_runner(log), 123)

  assert artifact_publication_manifest.status_to_string(manifest.status)
    == "unchanged"
  assert manifest.commit_sha == Some("deadbeef")
  assert manifest.pr_url == Some("https://example.test/pr/1")
  let transcript = read_file(log)
  assert string.contains(transcript, "git diff --cached --quiet")
  assert string.contains(transcript, "gh pr create")
  assert !string.contains(transcript, "git commit")
}

pub fn publish_updates_existing_pr_when_one_matches_test() {
  let root = "test/tmp/artifact-repository-github/pr-edit-success"
  let log = root <> "/commands.log"
  test_helpers.reset_dir(root)
  let input = prepared_input(root, True)

  let manifest = github.publish(input, root, pr_edit_success_runner(log), 123)

  assert artifact_publication_manifest.status_to_string(manifest.status)
    == "published"
  assert manifest.pr_url == Some("https://example.test/pr/42")
  let transcript = read_file(log)
  assert string.contains(transcript, "gh pr edit 42")
  assert !string.contains(transcript, "gh pr create")
}

pub fn publish_recovers_existing_pr_when_create_reports_duplicate_test() {
  let root = "test/tmp/artifact-repository-github/pr-create-duplicate"
  let log = root <> "/commands.log"
  test_helpers.reset_dir(root)
  let input = prepared_input(root, True)

  let manifest =
    github.publish(input, root, pr_create_duplicate_runner(log), 123)

  assert artifact_publication_manifest.status_to_string(manifest.status)
    == "unchanged"
  assert manifest.pr_url == Some("https://example.test/pr/42")
  let transcript = read_file(log)
  assert string.contains(transcript, "gh pr create")
  assert string.contains(transcript, "gh pr view")
  assert string.contains(transcript, "gh pr edit 42")
}

pub fn publish_deletes_stale_owned_paths_from_previous_success_test() {
  let root = "test/tmp/artifact-repository-github/stale-delete"
  let log = root <> "/commands.log"
  test_helpers.reset_dir(root)
  seed_latest_publication_with(root, "old-version", [
    "docs/plans/LIV-761.md",
    "docs/old.md",
  ])
  seed_checkout(root)
  let old_file = checkout_file(root, "docs/old.md")
  let assert Ok(dir) = path.dirname(old_file)
  let assert Ok(Nil) = simplifile.create_directory_all(dir)
  let assert Ok(Nil) = simplifile.write(old_file, "old")
  let input = prepared_input(root, True)

  let manifest = github.publish(input, root, reuse_checkout_runner(log), 123)

  assert artifact_publication_manifest.status_to_string(manifest.status)
    == "published"
  assert manifest.removed_paths == ["docs/old.md"]
  assert simplifile.is_file(old_file) == Ok(False)
  assert string.contains(read_file(log), "docs/old.md")
}

fn prepared_input(
  root: String,
  draft: Bool,
) -> repository_types.PublicationExecutionInput {
  let store = artifact_store.new(root)
  write_artifact(root, plan_ref(), plan_contents())
  let assert Ok(manifest) = planned_manifest(draft, store)
  let assert Ok(prepared) = github.prepare_publication_input(manifest, store)
  prepared
}

fn prepared_input_for_manifest(
  root: String,
  manifest: artifact_publication_planner.DryRunPublicationManifest,
) -> repository_types.PublicationExecutionInput {
  let store = artifact_store.new(root)
  write_artifact(root, plan_ref(), plan_contents())
  let assert Ok(prepared) = github.prepare_publication_input(manifest, store)
  prepared
}

fn planned_manifest(
  draft: Bool,
  store: artifact_store.Store,
) -> Result(artifact_publication_planner.DryRunPublicationManifest, Nil) {
  artifact_publication_planner.plan_publication(
    output_manifest(plan_sha(), plan_bytes()),
    repositories(draft),
    route(),
    store,
    work(),
    "run-1",
    dict.from_list([#("templates/publication.md", body_template())]),
  )
  |> result.replace_error(Nil)
}

fn duplicate_destination_manifest() -> artifact_publication_planner.DryRunPublicationManifest {
  let store = store_with_contents([#(plan_ref(), plan_contents())])
  let assert Ok(base) = planned_manifest(True, store)
  let assert [file] = base.files
  artifact_publication_planner.DryRunPublicationManifest(..base, files: [
    file,
    file,
  ])
}

fn unsafe_destination_manifest() -> artifact_publication_planner.DryRunPublicationManifest {
  let store = store_with_contents([#(plan_ref(), plan_contents())])
  let assert Ok(base) = planned_manifest(True, store)
  let assert [file] = base.files
  let artifact_publication_planner.PlannedPublicationFile(source, _) = file
  artifact_publication_planner.DryRunPublicationManifest(..base, files: [
    artifact_publication_planner.PlannedPublicationFile(source, "../evil.md"),
  ])
}

fn manifest_with_source_hash(
  base: artifact_publication_planner.DryRunPublicationManifest,
  sha256: String,
) -> artifact_publication_planner.DryRunPublicationManifest {
  let assert [file] = base.files
  let artifact_publication_planner.PlannedPublicationFile(source, destination) =
    file
  artifact_publication_planner.DryRunPublicationManifest(..base, files: [
    artifact_publication_planner.PlannedPublicationFile(
      artifact_publication_planner.SelectedArtifact(..source, sha256: sha256),
      destination,
    ),
  ])
}

fn manifest_with_source_bytes(
  base: artifact_publication_planner.DryRunPublicationManifest,
  bytes: Int,
) -> artifact_publication_planner.DryRunPublicationManifest {
  let assert [file] = base.files
  let artifact_publication_planner.PlannedPublicationFile(source, destination) =
    file
  artifact_publication_planner.DryRunPublicationManifest(..base, files: [
    artifact_publication_planner.PlannedPublicationFile(
      artifact_publication_planner.SelectedArtifact(..source, bytes: bytes),
      destination,
    ),
  ])
}

fn prepared_binary_input(
  _root: String,
  bytes: BitArray,
) -> repository_types.PublicationExecutionInput {
  let base = dry_run_manifest(True)
  let manifest =
    manifest_with_source_bytes(
      manifest_with_source_hash(base, hash.sha256_hex_bytes(bytes)),
      bit_array.byte_size(bytes),
    )
  let store = store_with_bytes([#(plan_ref(), bytes)])
  let assert Ok(prepared) = github.prepare_publication_input(manifest, store)
  prepared
}

fn route() -> artifact_publication_config.PublicationRoute {
  artifact_publication_config.PublicationRoute(
    id: "review_doc",
    repository: "github.docs",
    required: True,
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

fn repositories(
  draft: Bool,
) -> artifact_publication_config.ArtifactRepositories {
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
            draft: draft,
            title: Some("{{ work.identifier }} publication"),
            body_template: Some("templates/publication.md"),
          ),
        ),
      ),
    ]),
  )
}

fn store_with_contents(
  entries: List(#(String, String)),
) -> artifact_store.Store {
  artifact_store.custom(
    "test",
    artifact_store.StoreCallbacks(
      write: fn(_, _) { Error(artifact_store.ArtifactIo("write_unsupported")) },
      read: fn(ref) {
        entries
        |> dict.from_list
        |> dict.get(ref)
        |> result.replace_error(artifact_store.MissingStepArtifact(ref))
      },
      write_immutable_bytes: fn(_, _) {
        Error(artifact_store.ArtifactIo("write_unsupported"))
      },
      read_bytes: fn(ref) {
        entries
        |> dict.from_list
        |> dict.get(ref)
        |> result.map(bit_array.from_string)
        |> result.replace_error(artifact_store.MissingStepArtifact(ref))
      },
      locate: fn(ref) {
        Ok(artifact_store.ArtifactLocation(ref, ref, ref, None))
      },
    ),
  )
}

fn store_with_bytes(
  entries: List(#(String, BitArray)),
) -> artifact_store.Store {
  artifact_store.custom(
    "test-bytes",
    artifact_store.StoreCallbacks(
      write: fn(_, _) { Error(artifact_store.ArtifactIo("write_unsupported")) },
      read: fn(ref) {
        case dict.get(dict.from_list(entries), ref) {
          Ok(bits) ->
            bit_array.to_string(bits)
            |> result.map_error(fn(_) {
              artifact_store.ArtifactIo("artifact bytes are not utf8")
            })
          Error(_) -> Error(artifact_store.MissingStepArtifact(ref))
        }
      },
      write_immutable_bytes: fn(_, _) {
        Error(artifact_store.ArtifactIo("write_unsupported"))
      },
      read_bytes: fn(ref) {
        entries
        |> dict.from_list
        |> dict.get(ref)
        |> result.replace_error(artifact_store.MissingStepArtifact(ref))
      },
      locate: fn(ref) {
        Ok(artifact_store.ArtifactLocation(ref, ref, ref, None))
      },
    ),
  )
}

fn create_pr_runner(log: String, draft: Bool) -> command_runner.Runner {
  runner(log, fn(executable, args, _, _) {
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
      "git", ["commit", ..] -> Ok(command_runner.CommandOutput(0, "", ""))
      "git", ["rev-parse", "HEAD"] ->
        Ok(command_runner.CommandOutput(0, "deadbeef", ""))
      "git", ["push", ..] -> Ok(command_runner.CommandOutput(0, "", ""))
      "gh", ["pr", "list", ..] -> Ok(command_runner.CommandOutput(0, "[]", ""))
      "gh", ["pr", "create", ..] -> {
        assert list.contains(args, "--draft") == draft
        Ok(command_runner.CommandOutput(0, "https://example.test/pr/1", ""))
      }
      _, _ -> Error(command_runner.command_error("unexpected_command"))
    }
  })
}

fn reuse_checkout_runner(log: String) -> command_runner.Runner {
  runner(log, fn(executable, args, _, _) {
    case executable, args {
      "git", ["remote", "get-url", "origin"] ->
        Ok(command_runner.CommandOutput(
          0,
          "https://github.com/scherzo-systems/scherzo.git",
          "",
        ))
      "git", ["fetch", ..] -> Ok(command_runner.CommandOutput(0, "", ""))
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
  })
}

fn dirty_checkout_runner(log: String) -> command_runner.Runner {
  runner(log, fn(executable, args, _, _) {
    case executable, args {
      "git", ["remote", "get-url", "origin"] ->
        Ok(command_runner.CommandOutput(
          0,
          "https://github.com/scherzo-systems/scherzo.git",
          "",
        ))
      "git", ["fetch", ..] -> Ok(command_runner.CommandOutput(0, "", ""))
      "git", ["ls-remote", ..] -> Ok(command_runner.CommandOutput(2, "", ""))
      "git", ["rev-parse", "--verify", ..] ->
        Ok(command_runner.CommandOutput(1, "", ""))
      "git", ["checkout", ..] -> Ok(command_runner.CommandOutput(0, "", ""))
      "git", ["status", ..] ->
        Ok(command_runner.CommandOutput(0, " M docs/other.md", ""))
      _, _ -> Error(command_runner.command_error("unexpected_command"))
    }
  })
}

fn git_add_failure_runner(log: String) -> command_runner.Runner {
  runner(log, fn(executable, args, _, _) {
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
      "git", ["add", ..] ->
        Ok(command_runner.CommandOutput(2, "", "add failed"))
      _, _ -> Error(command_runner.command_error("unexpected_command"))
    }
  })
}

fn ambiguous_pr_runner(log: String) -> command_runner.Runner {
  runner(log, fn(executable, args, _, _) {
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
      "git", ["commit", ..] -> Ok(command_runner.CommandOutput(0, "", ""))
      "git", ["rev-parse", "HEAD"] ->
        Ok(command_runner.CommandOutput(0, "deadbeef", ""))
      "git", ["push", ..] -> Ok(command_runner.CommandOutput(0, "", ""))
      "gh", ["pr", "list", ..] ->
        Ok(command_runner.CommandOutput(
          0,
          "[{\"number\":1,\"url\":\"https://example.test/pr/1\",\"isDraft\":true,\"title\":\"one\"},{\"number\":2,\"url\":\"https://example.test/pr/2\",\"isDraft\":true,\"title\":\"two\"}]",
          "",
        ))
      _, _ -> Error(command_runner.command_error("unexpected_command"))
    }
  })
}

fn malformed_pr_runner(log: String) -> command_runner.Runner {
  runner(log, fn(executable, args, _, _) {
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
      "git", ["commit", ..] -> Ok(command_runner.CommandOutput(0, "", ""))
      "git", ["rev-parse", "HEAD"] ->
        Ok(command_runner.CommandOutput(0, "deadbeef", ""))
      "git", ["push", ..] -> Ok(command_runner.CommandOutput(0, "", ""))
      "gh", ["pr", "list", ..] ->
        Ok(command_runner.CommandOutput(0, "[{\"number\":wat}]", ""))
      _, _ -> Error(command_runner.command_error("unexpected_command"))
    }
  })
}

fn pr_edit_failure_runner(log: String) -> command_runner.Runner {
  runner(log, fn(executable, args, _, _) {
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
      "git", ["commit", ..] -> Ok(command_runner.CommandOutput(0, "", ""))
      "git", ["rev-parse", "HEAD"] ->
        Ok(command_runner.CommandOutput(0, "deadbeef", ""))
      "git", ["push", ..] -> Ok(command_runner.CommandOutput(0, "", ""))
      "gh", ["pr", "list", ..] ->
        Ok(command_runner.CommandOutput(
          0,
          "[{\"number\":42,\"url\":\"https://example.test/pr/42\",\"isDraft\":true,\"title\":\"existing\"}]",
          "",
        ))
      "gh", ["pr", "edit", ..] ->
        Ok(command_runner.CommandOutput(1, "", "edit failed"))
      _, _ -> Error(command_runner.command_error("unexpected_command"))
    }
  })
}

fn publish_failure_code(suffix: String, stage: String) -> String {
  let root = "test/tmp/artifact-repository-github/failure-" <> suffix
  let log = root <> "/commands.log"
  test_helpers.reset_dir(root)
  let input = prepared_input(root, True)
  let manifest = github.publish(input, root, failure_runner(log, stage), 123)
  let assert Some(error) = manifest.error
  error.code
}

fn failure_runner(log: String, stage: String) -> command_runner.Runner {
  runner(log, fn(executable, args, _, _) {
    case stage, executable, args {
      "clone", "git", ["clone", _, _] ->
        Ok(command_runner.CommandOutput(2, "", "clone failed"))
      _, "git", ["clone", _, target] -> {
        let _ = simplifile.create_directory_all(target)
        Ok(command_runner.CommandOutput(0, "", ""))
      }
      "fetch", "git", ["fetch", ..] ->
        Ok(command_runner.CommandOutput(2, "", "fetch failed"))
      _, "git", ["fetch", ..] -> Ok(command_runner.CommandOutput(0, "", ""))
      _, "git", ["ls-remote", ..] -> Ok(command_runner.CommandOutput(2, "", ""))
      _, "git", ["rev-parse", "--verify", ..] ->
        Ok(command_runner.CommandOutput(1, "", ""))
      "checkout", "git", ["checkout", ..] ->
        Ok(command_runner.CommandOutput(2, "", "checkout failed"))
      _, "git", ["checkout", ..] -> Ok(command_runner.CommandOutput(0, "", ""))
      "status", "git", ["status", ..] ->
        Ok(command_runner.CommandOutput(2, "", "status failed"))
      _, "git", ["status", ..] -> Ok(command_runner.CommandOutput(0, "", ""))
      _, "git", ["add", ..] -> Ok(command_runner.CommandOutput(0, "", ""))
      "diff", "git", ["diff", ..] ->
        Ok(command_runner.CommandOutput(2, "", "diff failed"))
      _, "git", ["diff", ..] -> Ok(command_runner.CommandOutput(1, "", ""))
      "commit", "git", ["commit", ..] ->
        Ok(command_runner.CommandOutput(2, "", "commit failed"))
      _, "git", ["commit", ..] -> Ok(command_runner.CommandOutput(0, "", ""))
      _, "git", ["rev-parse", "HEAD"] ->
        Ok(command_runner.CommandOutput(0, "deadbeef", ""))
      "push", "git", ["push", ..] ->
        Ok(command_runner.CommandOutput(2, "", "push failed"))
      _, "git", ["push", ..] -> Ok(command_runner.CommandOutput(0, "", ""))
      _, "gh", ["pr", "list", ..] ->
        Ok(command_runner.CommandOutput(0, "[]", ""))
      "pr_create", "gh", ["pr", "create", ..] ->
        Ok(command_runner.CommandOutput(1, "", "create failed"))
      _, "gh", ["pr", "create", ..] ->
        Ok(command_runner.CommandOutput(0, "https://example.test/pr/1", ""))
      _, _, _ -> Error(command_runner.command_error("unexpected_command"))
    }
  })
}

fn remote_mismatch_runner(log: String) -> command_runner.Runner {
  runner(log, fn(executable, args, _, _) {
    case executable, args {
      "git", ["remote", "get-url", "origin"] ->
        Ok(command_runner.CommandOutput(
          0,
          "https://github.com/other/repo.git",
          "",
        ))
      _, _ -> Error(command_runner.command_error("unexpected_command"))
    }
  })
}

fn no_diff_runner(log: String) -> command_runner.Runner {
  runner(log, fn(executable, args, _, _) {
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
      "git", ["diff", ..] -> Ok(command_runner.CommandOutput(0, "", ""))
      "git", ["rev-parse", "HEAD"] ->
        Ok(command_runner.CommandOutput(0, "deadbeef", ""))
      "gh", ["pr", "list", ..] -> Ok(command_runner.CommandOutput(0, "[]", ""))
      "gh", ["pr", "create", ..] ->
        Ok(command_runner.CommandOutput(0, "https://example.test/pr/1", ""))
      _, _ -> Error(command_runner.command_error("unexpected_command"))
    }
  })
}

fn pr_edit_success_runner(log: String) -> command_runner.Runner {
  runner(log, fn(executable, args, _, _) {
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
      "git", ["commit", ..] -> Ok(command_runner.CommandOutput(0, "", ""))
      "git", ["rev-parse", "HEAD"] ->
        Ok(command_runner.CommandOutput(0, "deadbeef", ""))
      "git", ["push", ..] -> Ok(command_runner.CommandOutput(0, "", ""))
      "gh", ["pr", "list", ..] ->
        Ok(command_runner.CommandOutput(
          0,
          "[{\"number\":42,\"url\":\"https://example.test/pr/42\",\"isDraft\":true,\"title\":\"existing\"}]",
          "",
        ))
      "gh", ["pr", "edit", ..] -> Ok(command_runner.CommandOutput(0, "", ""))
      _, _ -> Error(command_runner.command_error("unexpected_command"))
    }
  })
}

fn pr_create_duplicate_runner(log: String) -> command_runner.Runner {
  runner(log, fn(executable, args, _, _) {
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
      "git", ["diff", ..] -> Ok(command_runner.CommandOutput(0, "", ""))
      "git", ["rev-parse", "HEAD"] ->
        Ok(command_runner.CommandOutput(0, "deadbeef", ""))
      "gh", ["pr", "list", ..] -> Ok(command_runner.CommandOutput(0, "[]", ""))
      "gh", ["pr", "create", ..] ->
        Ok(command_runner.CommandOutput(1, "", "already exists"))
      "gh", ["pr", "view", ..] ->
        Ok(command_runner.CommandOutput(
          0,
          "{\"isDraft\":false,\"number\":42,\"state\":\"OPEN\",\"title\":\"existing\",\"url\":\"https://example.test/pr/42\"}",
          "",
        ))
      "gh", ["pr", "edit", ..] -> Ok(command_runner.CommandOutput(0, "", ""))
      _, _ -> Error(command_runner.command_error("unexpected_command"))
    }
  })
}

fn fail_if_called_runner() -> command_runner.Runner {
  command_runner.Runner(run: fn(_) {
    Error(command_runner.command_error("runner should not be called"))
  })
}

fn runner(
  log: String,
  handle: fn(String, List(String), String, command_runner.CommandSpec) ->
    Result(command_runner.CommandOutput, command_runner.CommandError),
) -> command_runner.Runner {
  command_runner.Runner(run: fn(spec) {
    let command_runner.CommandSpec(executable, args, cwd, _, _) = spec
    append_log(log, command_runner.describe(spec))
    handle(executable, args, cwd, spec)
  })
}

fn append_log(path: String, line: String) -> Nil {
  let existing = simplifile.read(path) |> result.unwrap("")
  let _ = simplifile.write(path, existing <> line <> "\n")
  Nil
}

fn read_file(path: String) -> String {
  simplifile.read(path) |> result.unwrap("")
}

fn seed_checkout(root: String) -> Nil {
  let assert Ok(Nil) = simplifile.create_directory_all(checkout_root(root))
  Nil
}

fn seed_latest_publication(root: String) -> Nil {
  let planned = dry_run_manifest(True)
  seed_latest_publication_with(root, planned.version_id, [
    "docs/plans/LIV-761.md",
  ])
}

fn seed_latest_publication_without_pr(root: String) -> Nil {
  let planned = dry_run_manifest(True)
  seed_latest_publication_with_pr(
    root,
    planned.version_id,
    [
      "docs/plans/LIV-761.md",
    ],
    None,
  )
}

fn seed_latest_publication_with(
  root: String,
  version_id: String,
  selected_paths: List(String),
) -> Nil {
  seed_latest_publication_with_pr(
    root,
    version_id,
    selected_paths,
    Some("https://example.test/pr/1"),
  )
}

fn seed_latest_publication_with_pr(
  root: String,
  version_id: String,
  selected_paths: List(String),
  pr_url: Option(String),
) -> Nil {
  let planned = dry_run_manifest(True)
  let published =
    artifact_publication_manifest.published_manifest(
      artifact_publication_planner.DryRunPublicationManifest(
        ..planned,
        version_id: version_id,
      ),
      version_id,
      100,
      "deadbeef",
      pr_url,
      selected_paths,
      [],
    )
  let manifest =
    artifact_publication_manifest.PublicationManifest(
      ..published,
      selected_paths: selected_paths,
    )
  seed_publication_attempt(root, manifest, "latest-publication", 100)
}

fn seed_latest_failed_publication(root: String) -> Nil {
  let planned = dry_run_manifest(True)
  let manifest =
    artifact_publication_manifest.failed_from_planned_manifest(
      planned,
      "failed-version-1",
      100,
      True,
      Some(planned.branch),
      None,
      None,
      ["docs/plans/LIV-761.md"],
      [],
      artifact_publication_manifest.PublicationErrorInfo(
        code: "git_push_failed",
        message: "push failed",
      ),
    )
  seed_publication_attempt(root, manifest, "latest-failed-publication", 100)
}

fn seed_publication_attempt(
  root: String,
  manifest: artifact_publication_manifest.PublicationManifest,
  record_id: String,
  recorded_at_ms: Int,
) -> Nil {
  let ref =
    artifact_publication_manifest.manifest_ref(
      manifest.run_id,
      manifest.publication_id,
      manifest.attempt_id,
    )
  let payload = artifact_publication_manifest.to_string(manifest)
  write_artifact(root, ref, payload)
  let error_code = case manifest.error {
    Some(artifact_publication_manifest.PublicationErrorInfo(code, _)) ->
      Some(code)
    None -> None
  }
  let error_message = case manifest.error {
    Some(artifact_publication_manifest.PublicationErrorInfo(_, message)) ->
      Some(message)
    None -> None
  }
  let assert Ok(ledger_path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) =
    ledger.append_many(
      ledger_path,
      [
        record.with_id(
          record_id,
          recorded_at_ms,
          record.PublicationAttemptRecorded(
            run_id: manifest.run_id,
            workflow_id: manifest.workflow_id,
            publication_id: manifest.publication_id,
            series_id: manifest.series_id,
            attempt_id: manifest.attempt_id,
            status: artifact_publication_manifest.status_to_string(
              manifest.status,
            ),
            required: manifest.required,
            retryable: manifest.retryable,
            retry_execution_available: manifest.retry_execution_available,
            version_id: manifest.version_id,
            manifest_ref: Some(ref),
            manifest_sha256: Some(hash.sha256_hex(payload)),
            manifest_bytes: Some(
              bit_array.byte_size(bit_array.from_string(payload)),
            ),
            error_code: error_code,
            error_message: error_message,
          ),
        ),
      ],
      True,
    )
  Nil
}

fn checkout_root(root: String) -> String {
  let planned = dry_run_manifest(True)
  path.join(
    root,
    ".scherzo-state/artifact-repositories/github/"
      <> hash.sha256_hex(planned.repository_id <> "|main|" <> planned.series_id),
  )
}

fn checkout_file(root: String, relative: String) -> String {
  path.join(checkout_root(root), relative)
}

fn destination_path(
  selected: repository_types.SelectedArtifactBytes,
) -> String {
  let repository_types.SelectedArtifactBytes(file, _) = selected
  let artifact_publication_planner.PlannedPublicationFile(_, destination_path) =
    file
  destination_path
}

fn selected_bytes(
  selected: repository_types.SelectedArtifactBytes,
) -> BitArray {
  let repository_types.SelectedArtifactBytes(_, bytes) = selected
  bytes
}

fn write_artifact(root: String, ref: String, contents: String) -> Nil {
  let absolute = root <> "/.scherzo-state/artifacts/" <> ref
  let assert Ok(dir) = path.dirname(absolute)
  let assert Ok(Nil) = simplifile.create_directory_all(dir)
  let assert Ok(Nil) = simplifile.write(absolute, contents)
  Nil
}

fn output_manifest(
  sha256: String,
  bytes: Int,
) -> workflow_contract_manifest.ContractOutputManifest {
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
            sha256: sha256,
            bytes: bytes,
          ),
          "text/markdown",
          None,
        ),
      ),
    ],
    diagnostics: [],
  )
}

fn dry_run_manifest(
  draft: Bool,
) -> artifact_publication_planner.DryRunPublicationManifest {
  let store = store_with_contents([#(plan_ref(), plan_contents())])
  let assert Ok(planned) = planned_manifest(draft, store)
  planned
}

fn work() -> artifact_publication_planner.PublicationWork {
  artifact_publication_planner.PublicationWork(
    kind: artifact_publication_planner.TaskWork,
    id: "task-1",
    identifier: "LIV-761",
    slug: "LIV-761",
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

fn body_template() -> String {
  "Version {{ publication.version_id }}"
}
