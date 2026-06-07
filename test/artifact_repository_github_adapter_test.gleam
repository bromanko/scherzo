import gleam/bit_array
import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/artifact_publication_config
import scherzo/artifact_publication_manifest
import scherzo/artifact_publication_planner
import scherzo/artifact_repository/checkout_lock
import scherzo/artifact_repository/command_runner
import scherzo/artifact_repository/github
import scherzo/artifact_repository/github_paths
import scherzo/artifact_repository/types as repository_types
import scherzo/commit_stack_artifact
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

pub fn publish_recovers_commit_when_latest_version_matches_without_commit_test() {
  let root = "test/tmp/artifact-repository-github/unchanged-missing-commit"
  let log = root <> "/commands.log"
  test_helpers.reset_dir(root)
  seed_latest_publication_without_commit(root)
  let input = prepared_input(root, True)

  let manifest = github.publish(input, root, no_diff_runner(log), 123)

  assert artifact_publication_manifest.status_to_string(manifest.status)
    == "unchanged"
  assert string.starts_with(manifest.attempt_id, "recovered-")
  assert manifest.commit_sha == Some("deadbeef")
  assert manifest.pr_url == Some("https://example.test/pr/1")
  assert string.contains(read_file(log), "git rev-parse HEAD")
}

pub fn publish_recovers_branch_when_latest_version_matches_without_branch_test() {
  let root = "test/tmp/artifact-repository-github/unchanged-missing-branch"
  let log = root <> "/commands.log"
  test_helpers.reset_dir(root)
  seed_latest_publication_without_branch(root)
  let input = prepared_input(root, True)

  let manifest = github.publish(input, root, no_diff_runner(log), 123)

  assert artifact_publication_manifest.status_to_string(manifest.status)
    == "unchanged"
  assert string.starts_with(manifest.attempt_id, "recovered-")
  assert manifest.branch == Some("scherzo/workflow.execplan/LIV-761/review_doc")
  assert manifest.commit_sha == Some("deadbeef")
  assert manifest.pr_url == Some("https://example.test/pr/1")
  assert string.contains(read_file(log), "git rev-parse HEAD")
}

pub fn publish_fails_when_pr_create_outputs_no_url_test() {
  let root = "test/tmp/artifact-repository-github/pr-create-empty-url"
  let log = root <> "/commands.log"
  test_helpers.reset_dir(root)
  let input = prepared_input(root, True)

  let manifest =
    github.publish(input, root, pr_create_empty_url_runner(log), 123)

  assert artifact_publication_manifest.status_to_string(manifest.status)
    == "failed"
  let assert Some(error) = manifest.error
  assert error.code == "pr_create_missing_url"
}

pub fn publish_fails_when_unchanged_commit_metadata_is_missing_test() {
  let root = "test/tmp/artifact-repository-github/no-diff-missing-head"
  let log = root <> "/commands.log"
  test_helpers.reset_dir(root)
  let input = prepared_input(root, True)

  let manifest =
    github.publish(input, root, no_diff_missing_head_runner(log), 123)

  assert artifact_publication_manifest.status_to_string(manifest.status)
    == "failed"
  let assert Some(error) = manifest.error
  assert error.code == "rev_parse_failed"
}

pub fn publish_fails_when_checkout_is_dirty_test() {
  let root = "test/tmp/artifact-repository-github/dirty-checkout"
  let log = root <> "/commands.log"
  test_helpers.reset_dir(root)
  seed_checkout(root)
  let assert Ok(Nil) = simplifile.write(dirty_marker(root), "dirty")
  let input = prepared_input(root, True)

  let manifest =
    github.publish(
      input,
      root,
      unrecoverable_dirty_checkout_runner(log, root),
      123,
    )

  assert artifact_publication_manifest.status_to_string(manifest.status)
    == "failed"
  let assert Some(error) = manifest.error
  assert error.code == "dirty_checkout"
  let assert Some(cleanup) = manifest.cleanup_diagnostics
  assert cleanup.cleanup_succeeded == False
  assert cleanup.pre_cleanup_status == Some("M dirty-marker")
}

pub fn publish_self_heals_dirty_checkout_before_materialization_test() {
  let root = "test/tmp/artifact-repository-github/dirty-checkout-self-heal"
  let log = root <> "/commands.log"
  test_helpers.reset_dir(root)
  seed_checkout(root)
  let assert Ok(Nil) = simplifile.write(dirty_marker(root), "dirty")
  let input = prepared_input(root, True)

  let manifest =
    github.publish(
      input,
      root,
      self_healing_dirty_checkout_runner(log, root),
      123,
    )

  assert artifact_publication_manifest.status_to_string(manifest.status)
    == "published"
  assert manifest.cleanup_diagnostics == None
  let transcript = read_file(log)
  assert string.contains(transcript, "git reset --hard HEAD")
  assert string.contains(transcript, "git clean -fd")
}

pub fn publish_commit_failure_records_cleanup_diagnostics_test() {
  let root = "test/tmp/artifact-repository-github/commit-failure-cleanup"
  let log = root <> "/commands.log"
  test_helpers.reset_dir(root)
  let input = prepared_input(root, True)

  let manifest =
    github.publish(input, root, commit_failure_cleanup_runner(log, root), 123)

  assert artifact_publication_manifest.status_to_string(manifest.status)
    == "failed"
  let assert Some(error) = manifest.error
  assert error.code == "git_commit_failed"
  let assert Some(cleanup) = manifest.cleanup_diagnostics
  assert cleanup.cleanup_succeeded == True
  assert cleanup.reset_summary == Some("exit=0")
  assert cleanup.post_cleanup_status == Some("")
  assert simplifile.is_file(checkout_file(root, "docs/plans/LIV-761.md"))
    == Ok(False)
}

pub fn publish_returns_publication_lock_failed_when_checkout_is_already_locked_test() {
  let root = "test/tmp/artifact-repository-github/lock-held"
  test_helpers.reset_dir(root)
  let input = prepared_input(root, True)
  let lock_dir = checkout_root(root)
  let assert Ok(Nil) = simplifile.create_directory_all(lock_dir)
  let assert Ok(lock) = checkout_lock.acquire(lock_dir)

  let manifest = github.publish(input, root, fail_if_called_runner(), 123)

  let _ = checkout_lock.release(lock)
  assert artifact_publication_manifest.status_to_string(manifest.status)
    == "failed"
  let assert Some(error) = manifest.error
  assert error.code == "publication_lock_failed"
}

pub fn checkout_lock_allows_distinct_checkouts_and_releases_after_failure_test() {
  let distinct_root = "test/tmp/artifact-repository-github/lock-distinct"
  let first_root = distinct_root <> "/first"
  let second_root = distinct_root <> "/second"
  test_helpers.reset_dir(distinct_root)
  let first_dir = checkout_root(first_root)
  let second_dir = checkout_root(second_root)
  let assert Ok(Nil) = simplifile.create_directory_all(first_dir)
  let assert Ok(Nil) = simplifile.create_directory_all(second_dir)

  let assert Ok(first_lock) = checkout_lock.acquire(first_dir)
  let assert Error(checkout_lock.LockAlreadyHeld(_)) =
    checkout_lock.acquire(first_dir)
  let assert Ok(second_lock) = checkout_lock.acquire(second_dir)
  let _ = checkout_lock.release(second_lock)
  let _ = checkout_lock.release(first_lock)

  let retry_root =
    "test/tmp/artifact-repository-github/lock-release-after-failure"
  let first_log = retry_root <> "/first.log"
  let second_log = retry_root <> "/second.log"
  test_helpers.reset_dir(retry_root)
  let input = prepared_input(retry_root, True)

  let failed =
    github.publish(
      input,
      retry_root,
      unrecoverable_dirty_checkout_runner(first_log, retry_root),
      123,
    )

  assert artifact_publication_manifest.status_to_string(failed.status)
    == "failed"
  let retried =
    github.publish(input, retry_root, create_pr_runner(second_log, True), 456)
  assert artifact_publication_manifest.status_to_string(retried.status)
    == "published"
}

pub fn publish_materialization_failure_records_cleanup_diagnostics_test() {
  let root =
    "test/tmp/artifact-repository-github/materialization-failure-cleanup"
  let log = root <> "/commands.log"
  test_helpers.reset_dir(root)
  let input = materialization_failure_input(root)

  let manifest =
    github.publish(
      input,
      root,
      cleanup_failure_runner(log, root, "materialize"),
      123,
    )

  assert_cleanup_failure(
    manifest,
    "destination_dir_failed",
    checkout_file(root, "docs"),
  )
}

pub fn publish_git_add_failure_records_cleanup_diagnostics_test() {
  let root = "test/tmp/artifact-repository-github/git-add-fails"
  let log = root <> "/commands.log"
  test_helpers.reset_dir(root)
  let input = prepared_input(root, True)

  let manifest =
    github.publish(input, root, cleanup_failure_runner(log, root, "add"), 123)

  assert_cleanup_failure(
    manifest,
    "git_add_failed",
    checkout_file(root, "docs/plans/LIV-761.md"),
  )
}

pub fn publish_git_diff_failure_records_cleanup_diagnostics_test() {
  let root = "test/tmp/artifact-repository-github/git-diff-fails"
  let log = root <> "/commands.log"
  test_helpers.reset_dir(root)
  let input = prepared_input(root, True)

  let manifest =
    github.publish(input, root, cleanup_failure_runner(log, root, "diff"), 123)

  assert_cleanup_failure(
    manifest,
    "git_diff_failed",
    checkout_file(root, "docs/plans/LIV-761.md"),
  )
}

pub fn publish_push_failure_records_cleanup_diagnostics_test() {
  let root = "test/tmp/artifact-repository-github/git-push-fails"
  let log = root <> "/commands.log"
  test_helpers.reset_dir(root)
  let input = prepared_input(root, True)

  let manifest =
    github.publish(input, root, cleanup_failure_runner(log, root, "push"), 123)

  assert_cleanup_failure(
    manifest,
    "git_push_failed",
    checkout_file(root, "docs/plans/LIV-761.md"),
  )
  assert manifest.commit_sha == Some("deadbeef")
}

pub fn publish_pr_create_failure_records_cleanup_diagnostics_test() {
  let root = "test/tmp/artifact-repository-github/pr-create-fails"
  let log = root <> "/commands.log"
  test_helpers.reset_dir(root)
  let input = prepared_input(root, True)

  let manifest =
    github.publish(
      input,
      root,
      cleanup_failure_runner(log, root, "pr_create"),
      123,
    )

  assert_cleanup_failure(
    manifest,
    "pr_create_failed",
    checkout_file(root, "docs/plans/LIV-761.md"),
  )
  assert manifest.commit_sha == Some("deadbeef")
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

  let manifest =
    github.publish(
      input,
      root,
      cleanup_failure_runner(log, root, "pr_edit"),
      123,
    )

  assert artifact_publication_manifest.status_to_string(manifest.status)
    == "failed"
  let assert Some(error) = manifest.error
  assert error.code == "pr_edit_failed"
  let assert Some(cleanup) = manifest.cleanup_diagnostics
  assert cleanup.cleanup_succeeded == True
  assert manifest.commit_sha == Some("deadbeef")
  assert manifest.pr_url == Some("https://example.test/pr/42")
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

pub fn publish_decodes_reordered_escaped_pr_list_json_test() {
  let root = "test/tmp/artifact-repository-github/pr-list-reordered-json"
  let log = root <> "/commands.log"
  test_helpers.reset_dir(root)
  let input = prepared_input(root, True)

  let manifest = github.publish(input, root, pr_list_reordered_runner(log), 123)

  assert artifact_publication_manifest.status_to_string(manifest.status)
    == "published"
  assert manifest.pr_url == Some("https://example.test/pr/42?label=\"review\"")
  let transcript = read_file(log)
  assert string.contains(transcript, "gh pr edit 42")
  assert !string.contains(transcript, "gh pr create")
}

pub fn publish_maps_github_auth_env_and_pr_body_stdin_test() {
  let first_root = "test/tmp/artifact-repository-github/auth-env-primary"
  let first_log = first_root <> "/commands.log"
  test_helpers.reset_dir(first_root)
  let first_input = prepared_input(first_root, True)
  let first =
    with_github_env(
      Some("primary-token"),
      Some("github-token"),
      Some("agent-token"),
      fn() {
        github.publish(
          first_input,
          first_root,
          auth_env_runner(first_log, "primary-token"),
          123,
        )
      },
    )
  assert artifact_publication_manifest.status_to_string(first.status)
    == "published"

  let second_root = "test/tmp/artifact-repository-github/auth-env-fallback"
  let second_log = second_root <> "/commands.log"
  test_helpers.reset_dir(second_root)
  let second_input = prepared_input(second_root, True)
  let second =
    with_github_env(None, Some("github-token"), Some("agent-token"), fn() {
      github.publish(
        second_input,
        second_root,
        auth_env_runner(second_log, "github-token"),
        123,
      )
    })
  assert artifact_publication_manifest.status_to_string(second.status)
    == "published"
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

pub fn publish_commit_stack_updates_existing_branch_without_new_pr_test() {
  let root = "test/tmp/artifact-repository-github/commit-stack-existing"
  let log = root <> "/commands.log"
  test_helpers.reset_dir(root)
  let input = commit_stack_input(root)

  let manifest =
    github.publish(input, root, commit_stack_success_runner(log), 123)

  assert artifact_publication_manifest.status_to_string(manifest.status)
    == "published"
  assert manifest.branch == Some(existing_branch())
  assert manifest.commit_sha == Some(commit_stack_head_sha())
  assert manifest.pr_url == Some(existing_pr_url())
  assert manifest.pr_number == Some(42)
  let transcript = read_file(log)
  assert string.contains(transcript, "git bundle verify")
  assert string.contains(transcript, commit_stack_head_sha())
  assert string.contains(
    transcript,
    "git push origin "
      <> commit_stack_head_sha()
      <> ":refs/heads/"
      <> existing_branch(),
  )
  assert !string.contains(transcript, "gh pr create")
  assert !string.contains(transcript, "gh pr edit")
}

pub fn publish_commit_stack_returns_publication_lock_failed_when_checkout_is_already_locked_test() {
  let root = "test/tmp/artifact-repository-github/commit-stack-lock-held"
  test_helpers.reset_dir(root)
  let input = commit_stack_input(root)
  let checkout_dir =
    github_paths.checkout_dir(root, commit_stack_publication_manifest())
  let assert Ok(Nil) = simplifile.create_directory_all(checkout_dir)
  let assert Ok(lock) = checkout_lock.acquire(checkout_dir)

  let manifest = github.publish(input, root, fail_if_called_runner(), 123)

  let _ = checkout_lock.release(lock)
  assert artifact_publication_manifest.status_to_string(manifest.status)
    == "failed"
  let assert Some(error) = manifest.error
  assert error.code == "publication_lock_failed"
  assert manifest.pr_url == Some(existing_pr_url())
}

pub fn publish_commit_stack_retry_recovers_from_retained_carrier_test() {
  let root = "test/tmp/artifact-repository-github/commit-stack-retry"
  let first_log = root <> "/first-commands.log"
  let second_log = root <> "/second-commands.log"
  test_helpers.reset_dir(root)
  let input = commit_stack_input(root)

  let first =
    github.publish(
      input,
      root,
      commit_stack_push_failure_runner(first_log),
      123,
    )

  assert artifact_publication_manifest.status_to_string(first.status)
    == "failed"
  let assert Some(error) = first.error
  assert error.code == "git_push_failed"

  let second =
    github.publish(input, root, commit_stack_success_runner(second_log), 456)

  assert artifact_publication_manifest.status_to_string(second.status)
    == "published"
  assert second.commit_sha == Some(commit_stack_head_sha())
  assert second.pr_url == Some(existing_pr_url())
}

pub fn publish_commit_stack_refuses_stale_existing_branch_test() {
  let root = "test/tmp/artifact-repository-github/commit-stack-stale"
  let log = root <> "/commands.log"
  test_helpers.reset_dir(root)
  let input = commit_stack_input(root)

  let manifest =
    github.publish(input, root, commit_stack_stale_runner(log), 123)

  assert artifact_publication_manifest.status_to_string(manifest.status)
    == "failed"
  let assert Some(error) = manifest.error
  assert error.code == "stale_existing_branch"
  assert manifest.pr_url == Some(existing_pr_url())
  assert manifest.pr_number == Some(42)
  let transcript = read_file(log)
  assert !string.contains(transcript, "git push origin")
  assert !string.contains(transcript, "gh pr create")
}

pub fn publish_commit_stack_rechecks_existing_branch_after_prior_success_test() {
  let root =
    "test/tmp/artifact-repository-github/commit-stack-stale-after-success"
  let log = root <> "/commands.log"
  test_helpers.reset_dir(root)
  seed_latest_commit_stack_publication(root)
  let input = commit_stack_input(root)

  let manifest =
    github.publish(input, root, commit_stack_stale_runner(log), 456)

  assert artifact_publication_manifest.status_to_string(manifest.status)
    == "failed"
  let assert Some(error) = manifest.error
  assert error.code == "stale_existing_branch"
  let transcript = read_file(log)
  assert string.contains(transcript, "gh pr view 42")
  assert string.contains(
    transcript,
    "git rev-parse origin/" <> existing_branch(),
  )
  assert !string.contains(transcript, "git push origin")
}

pub fn publish_commit_stack_refuses_unverified_pr_target_test() {
  let root = "test/tmp/artifact-repository-github/commit-stack-pr-mismatch"
  let log = root <> "/commands.log"
  test_helpers.reset_dir(root)
  let input = commit_stack_input(root)

  let manifest =
    github.publish(
      input,
      root,
      commit_stack_pr_branch_mismatch_runner(log),
      123,
    )

  assert artifact_publication_manifest.status_to_string(manifest.status)
    == "failed"
  let assert Some(error) = manifest.error
  assert error.code == "existing_pr_branch_mismatch"
  let transcript = read_file(log)
  assert string.contains(transcript, "gh pr view 42")
  assert !string.contains(transcript, "git push origin")
  assert !string.contains(transcript, "gh pr create")
}

pub fn prepare_commit_stack_refuses_oversized_carrier_before_read_test() {
  let store =
    artifact_store.new("test/tmp/artifact-repository-github/oversized")
  let manifest =
    artifact_publication_planner.DryRunPublicationManifest(
      ..commit_stack_publication_manifest(),
      commit_stack: Some(planned_commit_stack_with_carrier_bytes(
        commit_stack_artifact.max_bundle_bytes + 1,
      )),
    )

  let assert Error(error) = github.prepare_publication_input(manifest, store)

  assert github.code(error) == "commit_stack_carrier_too_large"
}

pub fn prepare_commit_stack_refuses_carrier_size_mismatch_before_hash_test() {
  let root = "test/tmp/artifact-repository-github/carrier-size-mismatch"
  test_helpers.reset_dir(root)
  let store = artifact_store.new(root)
  write_artifact(
    root,
    commit_stack_carrier_ref(),
    commit_stack_carrier() <> " with unexpected bytes",
  )

  let assert Error(error) =
    github.prepare_publication_input(commit_stack_publication_manifest(), store)

  assert github.code(error) == "bytes_mismatch"
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

fn prepared_input_with_bytes(
  manifest: artifact_publication_planner.DryRunPublicationManifest,
  entries: List(#(String, String)),
) -> repository_types.PublicationExecutionInput {
  let assert Ok(prepared) =
    github.prepare_publication_input(manifest, store_with_contents(entries))
  prepared
}

fn materialization_failure_input(
  root: String,
) -> repository_types.PublicationExecutionInput {
  let first =
    artifact_publication_planner.SelectedArtifact(
      output: "review_doc",
      entry: None,
      name: "first",
      artifact_type: None,
      metadata: None,
      ref: "runs/run-1/outputs/first.md",
      sha256: hash.sha256_hex("file"),
      bytes: 4,
      media_type: "text/markdown",
    )
  let second =
    artifact_publication_planner.SelectedArtifact(
      output: "review_doc",
      entry: None,
      name: "second",
      artifact_type: None,
      metadata: None,
      ref: "runs/run-1/outputs/second.md",
      sha256: hash.sha256_hex("child"),
      bytes: 5,
      media_type: "text/markdown",
    )
  let manifest =
    artifact_publication_planner.DryRunPublicationManifest(
      ..dry_run_manifest(True),
      files: [
        artifact_publication_planner.PlannedPublicationFile(first, "docs"),
        artifact_publication_planner.PlannedPublicationFile(
          second,
          "docs/child.md",
        ),
      ],
    )
  let _ = root
  prepared_input_with_bytes(manifest, [
    #("runs/run-1/outputs/first.md", "file"),
    #("runs/run-1/outputs/second.md", "child"),
  ])
}

fn commit_stack_input(
  root: String,
) -> repository_types.PublicationExecutionInput {
  let store = artifact_store.new(root)
  write_artifact(root, commit_stack_carrier_ref(), commit_stack_carrier())
  let assert Ok(prepared) =
    github.prepare_publication_input(commit_stack_publication_manifest(), store)
  prepared
}

fn commit_stack_publication_manifest() -> artifact_publication_planner.DryRunPublicationManifest {
  artifact_publication_planner.DryRunPublicationManifest(
    run_id: "run-1",
    workflow_id: "workflow.implementation",
    publication_id: "conflict_resolution",
    series_id: "work/task-1/workflow/workflow.implementation/publication/conflict_resolution",
    version_id: "commit-stack-version-1",
    required: True,
    dry_run: True,
    repository_kind: "github",
    repository_id: "github.code",
    github_repo: Some("scherzo-systems/scherzo"),
    github_base: Some("main"),
    branch: existing_branch(),
    target: artifact_publication_planner.ExistingPrBranchTargetPlan(
      existing_target(),
    ),
    pull_request: artifact_publication_planner.PlannedPullRequest(
      enabled: False,
      draft: False,
      title: None,
      body: None,
    ),
    files: [],
    commit_stack: Some(planned_commit_stack()),
  )
}

fn planned_commit_stack() -> artifact_publication_planner.PlannedCommitStack {
  planned_commit_stack_with_carrier_bytes(bytes_of(commit_stack_carrier()))
}

fn planned_commit_stack_with_carrier_bytes(
  carrier_bytes: Int,
) -> artifact_publication_planner.PlannedCommitStack {
  artifact_publication_planner.PlannedCommitStack(
    output: "commit_stack",
    manifest_ref: "runs/run-1/outputs/commit_stack.json",
    manifest_sha256: hash.sha256_hex("commit-stack-manifest"),
    manifest_bytes: bytes_of("commit-stack-manifest"),
    stack: commit_stack_artifact.CommitStackArtifact(
      repository: "scherzo-systems/scherzo",
      base_ref: existing_branch(),
      base_sha: expected_existing_head_sha(),
      head_sha: commit_stack_head_sha(),
      head_tree: commit_stack_head_tree(),
      carrier: commit_stack_artifact.CommitStackCarrier(
        ref: commit_stack_carrier_ref(),
        sha256: hash.sha256_hex(commit_stack_carrier()),
        bytes: carrier_bytes,
        media_type: commit_stack_artifact.bundle_media_type,
      ),
    ),
  )
}

fn existing_target() -> commit_stack_artifact.ExistingPrBranchTarget {
  commit_stack_artifact.ExistingPrBranchTarget(
    repository: "scherzo-systems/scherzo",
    head_repo: "scherzo-systems/scherzo",
    head_branch: existing_branch(),
    expected_head_sha: expected_existing_head_sha(),
    base_branch: "main",
    base_sha: expected_base_sha(),
    pr_number: 42,
    pr_url: existing_pr_url(),
  )
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
      write_bytes: fn(_, _) {
        Error(artifact_store.ArtifactIo("write_unsupported"))
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
      write_bytes: fn(_, _) {
        Error(artifact_store.ArtifactIo("write_unsupported"))
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

fn commit_stack_success_runner(log: String) -> command_runner.Runner {
  commit_stack_runner(log, expected_existing_head_sha())
}

fn commit_stack_stale_runner(log: String) -> command_runner.Runner {
  commit_stack_runner(log, "9999999999999999999999999999999999999999")
}

fn commit_stack_pr_branch_mismatch_runner(
  log: String,
) -> command_runner.Runner {
  commit_stack_runner_with_pr(
    log,
    expected_existing_head_sha(),
    expected_existing_head_sha(),
    "feature/other",
    False,
  )
}

fn commit_stack_push_failure_runner(log: String) -> command_runner.Runner {
  runner(log, fn(executable, args, _, spec) {
    case executable, args {
      "git", ["push", ..] ->
        Ok(command_runner.CommandOutput(2, "", "push failed"))
      _, _ -> {
        let command_runner.Runner(run: run_success) =
          commit_stack_success_runner(log)
        run_success(spec)
      }
    }
  })
}

fn commit_stack_runner(
  log: String,
  remote_head: String,
) -> command_runner.Runner {
  commit_stack_runner_with_pr(
    log,
    remote_head,
    expected_existing_head_sha(),
    existing_branch(),
    False,
  )
}

fn commit_stack_runner_with_pr(
  log: String,
  remote_head: String,
  pr_head: String,
  pr_branch: String,
  is_cross_repository: Bool,
) -> command_runner.Runner {
  runner(log, fn(executable, args, _, _) {
    case executable, args {
      "git", ["clone", _, target] -> {
        let _ = simplifile.create_directory_all(target)
        Ok(command_runner.CommandOutput(0, "", ""))
      }
      "git", ["remote", "get-url", "origin"] ->
        Ok(command_runner.CommandOutput(
          0,
          "https://github.com/scherzo-systems/scherzo.git",
          "",
        ))
      "git", ["fetch", "origin", _] ->
        Ok(command_runner.CommandOutput(0, "", ""))
      "git", ["ls-remote", ..] -> Ok(command_runner.CommandOutput(0, "", ""))
      "git", ["checkout", ..] -> Ok(command_runner.CommandOutput(0, "", ""))
      "git", ["status", ..] -> Ok(command_runner.CommandOutput(0, "", ""))
      "git", ["bundle", "verify", _] ->
        Ok(command_runner.CommandOutput(0, "", ""))
      "git", ["fetch", _, head] ->
        case head == commit_stack_head_sha() {
          True -> Ok(command_runner.CommandOutput(0, "", ""))
          False -> Error(command_runner.command_error("unexpected_command"))
        }
      "git", ["rev-parse", value] ->
        case value == commit_stack_head_sha() <> "^{tree}" {
          True ->
            Ok(command_runner.CommandOutput(0, commit_stack_head_tree(), ""))
          False ->
            case value == "origin/" <> existing_branch() {
              True -> Ok(command_runner.CommandOutput(0, remote_head, ""))
              False -> Error(command_runner.command_error("unexpected_command"))
            }
        }
      "git", ["merge-base", "--is-ancestor", _, _] ->
        Ok(command_runner.CommandOutput(0, "", ""))
      "git", ["push", ..] -> Ok(command_runner.CommandOutput(0, "", ""))
      "gh", ["pr", "view", "42", ..] ->
        Ok(command_runner.CommandOutput(
          0,
          existing_pr_view_json(pr_head, pr_branch, is_cross_repository),
          "",
        ))
      _, _ -> Error(command_runner.command_error("unexpected_command"))
    }
  })
}

fn self_healing_dirty_checkout_runner(
  log: String,
  root: String,
) -> command_runner.Runner {
  dirty_checkout_cleanup_runner(log, root, True)
}

fn unrecoverable_dirty_checkout_runner(
  log: String,
  root: String,
) -> command_runner.Runner {
  dirty_checkout_cleanup_runner(log, root, False)
}

fn dirty_checkout_cleanup_runner(
  log: String,
  root: String,
  heal: Bool,
) -> command_runner.Runner {
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
        case simplifile.is_file(dirty_marker(root)) {
          Ok(True) -> Ok(command_runner.CommandOutput(0, "M dirty-marker", ""))
          _ -> Ok(command_runner.CommandOutput(0, "", ""))
        }
      "git", ["reset", "--hard", "HEAD"] -> {
        case heal {
          True -> {
            let _ = simplifile.delete(dirty_marker(root))
            Ok(command_runner.CommandOutput(0, "", ""))
          }
          False -> Ok(command_runner.CommandOutput(1, "", "reset failed"))
        }
      }
      "git", ["clean", "-fd"] -> Ok(command_runner.CommandOutput(0, "", ""))
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

fn commit_failure_cleanup_runner(
  log: String,
  root: String,
) -> command_runner.Runner {
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
      "git", ["status", ..] ->
        case simplifile.is_file(checkout_file(root, "docs/plans/LIV-761.md")) {
          Ok(True) ->
            Ok(command_runner.CommandOutput(0, "M docs/plans/LIV-761.md", ""))
          _ -> Ok(command_runner.CommandOutput(0, "", ""))
        }
      "git", ["add", ..] -> Ok(command_runner.CommandOutput(0, "", ""))
      "git", ["diff", ..] -> Ok(command_runner.CommandOutput(1, "", ""))
      "git", ["commit", ..] ->
        Ok(command_runner.CommandOutput(2, "", "commit failed"))
      "git", ["reset", "--hard", "HEAD"] -> {
        let _ = simplifile.delete(checkout_file(root, "docs/plans/LIV-761.md"))
        Ok(command_runner.CommandOutput(0, "", ""))
      }
      "git", ["clean", "-fd"] -> Ok(command_runner.CommandOutput(0, "", ""))
      _, _ -> Error(command_runner.command_error("unexpected_command"))
    }
  })
}

fn assert_cleanup_failure(
  manifest: artifact_publication_manifest.PublicationManifest,
  expected_code: String,
  dirty_path: String,
) -> Nil {
  assert artifact_publication_manifest.status_to_string(manifest.status)
    == "failed"
  let assert Some(error) = manifest.error
  assert error.code == expected_code
  let assert Some(cleanup) = manifest.cleanup_diagnostics
  assert cleanup.cleanup_succeeded == True
  assert cleanup.reset_summary == Some("exit=0")
  assert cleanup.post_cleanup_status == Some("")
  assert simplifile.is_file(dirty_path) == Ok(False)
}

fn cleanup_failure_runner(
  log: String,
  root: String,
  stage: String,
) -> command_runner.Runner {
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
      "git", ["status", ..] ->
        Ok(command_runner.CommandOutput(0, dirty_status(root), ""))
      "git", ["add", ..] ->
        case stage == "add" {
          True -> Ok(command_runner.CommandOutput(2, "", "add failed"))
          False -> Ok(command_runner.CommandOutput(0, "", ""))
        }
      "git", ["diff", ..] ->
        case stage == "diff" {
          True -> Ok(command_runner.CommandOutput(2, "", "diff failed"))
          False -> Ok(command_runner.CommandOutput(1, "", ""))
        }
      "git", ["commit", ..] -> Ok(command_runner.CommandOutput(0, "", ""))
      "git", ["rev-parse", "HEAD"] ->
        Ok(command_runner.CommandOutput(0, "deadbeef", ""))
      "git", ["push", ..] ->
        case stage == "push" {
          True -> Ok(command_runner.CommandOutput(2, "", "push failed"))
          False -> Ok(command_runner.CommandOutput(0, "", ""))
        }
      "gh", ["pr", "list", ..] ->
        case stage == "pr_edit" {
          True ->
            Ok(command_runner.CommandOutput(
              0,
              "[{\"number\":42,\"url\":\"https://example.test/pr/42\",\"isDraft\":true,\"title\":\"existing\"}]",
              "",
            ))
          False -> Ok(command_runner.CommandOutput(0, "[]", ""))
        }
      "gh", ["pr", "create", ..] ->
        case stage == "pr_create" {
          True -> Ok(command_runner.CommandOutput(1, "", "create failed"))
          False ->
            Ok(command_runner.CommandOutput(0, "https://example.test/pr/1", ""))
        }
      "gh", ["pr", "edit", ..] ->
        Ok(command_runner.CommandOutput(1, "", "edit failed"))
      "git", ["reset", "--hard", "HEAD"] -> {
        let _ = simplifile.delete(checkout_file(root, "docs/plans/LIV-761.md"))
        let _ = simplifile.delete(checkout_file(root, "docs"))
        Ok(command_runner.CommandOutput(0, "", ""))
      }
      "git", ["clean", "-fd"] -> Ok(command_runner.CommandOutput(0, "", ""))
      _, _ -> Error(command_runner.command_error("unexpected_command"))
    }
  })
}

fn dirty_status(root: String) -> String {
  case simplifile.is_file(checkout_file(root, "docs/plans/LIV-761.md")) {
    Ok(True) -> "M docs/plans/LIV-761.md"
    _ ->
      case simplifile.is_file(checkout_file(root, "docs")) {
        Ok(True) -> "?? docs"
        _ -> ""
      }
  }
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

fn no_diff_missing_head_runner(log: String) -> command_runner.Runner {
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
        Ok(command_runner.CommandOutput(0, "", ""))
      _, _ -> Error(command_runner.command_error("unexpected_command"))
    }
  })
}

fn pr_create_empty_url_runner(log: String) -> command_runner.Runner {
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
      "gh", ["pr", "create", ..] ->
        Ok(command_runner.CommandOutput(0, "  \n", ""))
      "gh", ["pr", "view", ..] -> Ok(command_runner.CommandOutput(1, "", ""))
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

fn pr_list_reordered_runner(log: String) -> command_runner.Runner {
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
          "[\n  { \"title\": \"existing, \\\"quoted\\\"\", \"url\": \"https://example.test/pr/42?label=\\\"review\\\"\", \"isDraft\": null, \"number\": 42 }\n]",
          "",
        ))
      "gh", ["pr", "edit", ..] -> Ok(command_runner.CommandOutput(0, "", ""))
      _, _ -> Error(command_runner.command_error("unexpected_command"))
    }
  })
}

fn auth_env_runner(
  log: String,
  expected_token: String,
) -> command_runner.Runner {
  runner(log, fn(executable, args, _, spec) {
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
      "gh", ["pr", "list", ..] -> {
        assert gh_token(spec) == Some(expected_token)
        assert gh_stdin(spec) == None
        Ok(command_runner.CommandOutput(0, "[]", ""))
      }
      "gh", ["pr", "create", ..] -> {
        assert gh_token(spec) == Some(expected_token)
        let assert Some(body) = gh_stdin(spec)
        assert string.contains(body, "Version")
        Ok(command_runner.CommandOutput(0, "https://example.test/pr/1", ""))
      }
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
          "{\n  \"title\": null,\n  \"url\": \"https://example.test/pr/42\",\n  \"state\": \"OPEN\",\n  \"number\": 42,\n  \"isDraft\": null\n}",
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

fn gh_token(spec: command_runner.CommandSpec) -> Option(String) {
  let command_runner.CommandSpec(env: env, ..) = spec
  case
    list.find(env, fn(pair) {
      let #(key, _) = pair
      key == "GH_TOKEN"
    })
  {
    Ok(#(_, token)) -> Some(token)
    Error(_) -> None
  }
}

fn gh_stdin(spec: command_runner.CommandSpec) -> Option(String) {
  let command_runner.CommandSpec(stdin: stdin, ..) = spec
  stdin
}

fn with_github_env(
  gh_token: Option(String),
  github_token: Option(String),
  agent_token: Option(String),
  run: fn() -> a,
) -> a {
  let previous_gh = path.env("GH_TOKEN")
  let previous_github = path.env("GITHUB_TOKEN")
  let previous_agent = path.env("SCHERZO_AGENT_GITHUB_TOKEN")
  set_env_option("GH_TOKEN", gh_token)
  set_env_option("GITHUB_TOKEN", github_token)
  set_env_option("SCHERZO_AGENT_GITHUB_TOKEN", agent_token)
  let result = run()
  set_env_option("SCHERZO_AGENT_GITHUB_TOKEN", previous_agent)
  set_env_option("GITHUB_TOKEN", previous_github)
  set_env_option("GH_TOKEN", previous_gh)
  result
}

fn set_env_option(key: String, value: Option(String)) -> Nil {
  case value {
    Some(value) -> {
      let assert Ok(Nil) = path.set_env(key, value)
      Nil
    }
    None -> {
      let assert Ok(Nil) = path.unset_env(key)
      Nil
    }
  }
}

fn runner(
  log: String,
  handle: fn(String, List(String), String, command_runner.CommandSpec) ->
    Result(command_runner.CommandOutput, command_runner.CommandError),
) -> command_runner.Runner {
  command_runner.Runner(run: fn(spec) {
    let command_runner.CommandSpec(
      executable: executable,
      args: args,
      cwd: cwd,
      ..,
    ) = spec
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

fn seed_latest_publication_without_commit(root: String) -> Nil {
  let planned = dry_run_manifest(True)
  let published =
    artifact_publication_manifest.published_manifest(
      planned,
      planned.version_id,
      100,
      "deadbeef",
      Some("https://example.test/pr/1"),
      ["docs/plans/LIV-761.md"],
      [],
    )
  seed_publication_attempt(
    root,
    artifact_publication_manifest.PublicationManifest(
      ..published,
      commit_sha: None,
    ),
    "latest-publication-missing-commit",
    100,
  )
}

fn seed_latest_publication_without_branch(root: String) -> Nil {
  let planned = dry_run_manifest(True)
  let published =
    artifact_publication_manifest.published_manifest(
      planned,
      planned.version_id,
      100,
      "deadbeef",
      Some("https://example.test/pr/1"),
      ["docs/plans/LIV-761.md"],
      [],
    )
  seed_publication_attempt(
    root,
    artifact_publication_manifest.PublicationManifest(..published, branch: None),
    "latest-publication-missing-branch",
    100,
  )
}

fn seed_latest_commit_stack_publication(root: String) -> Nil {
  let planned = commit_stack_publication_manifest()
  let published =
    artifact_publication_manifest.published_manifest(
      planned,
      planned.version_id,
      100,
      commit_stack_head_sha(),
      Some(existing_pr_url()),
      [],
      [],
    )
  seed_publication_attempt(root, published, "latest-commit-stack", 100)
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

fn dirty_marker(root: String) -> String {
  checkout_file(root, "dirty-marker")
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

fn bytes_of(contents: String) -> Int {
  bit_array.byte_size(bit_array.from_string(contents))
}

fn existing_branch() -> String {
  "feature/conflict-resolution"
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

fn commit_stack_carrier_ref() -> String {
  "runs/run-1/outputs/commit_stack.bundle"
}

fn commit_stack_carrier() -> String {
  "bundle bytes"
}

fn existing_pr_url() -> String {
  "https://example.test/pr/42"
}

fn existing_pr_view_json(
  head_sha: String,
  head_branch: String,
  is_cross_repository: Bool,
) -> String {
  let cross_repository = case is_cross_repository {
    True -> "true"
    False -> "false"
  }
  "{\"number\":42,\"url\":\""
  <> existing_pr_url()
  <> "\",\"state\":\"OPEN\",\"headRefName\":\""
  <> head_branch
  <> "\",\"headRefOid\":\""
  <> head_sha
  <> "\",\"baseRefName\":\"main\",\"isCrossRepository\":"
  <> cross_repository
  <> "}"
}

fn body_template() -> String {
  "Version {{ publication.version_id }}"
}
