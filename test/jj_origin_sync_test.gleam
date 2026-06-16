import gleam/list
import gleam/option.{Some}
import gleam/string
import scherzo/command_step
import scherzo/step_artifact
import simplifile
import support/test_helpers
import workflow_context_test_support

fn prepare_fake_repo(dir: String) -> Nil {
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/.jj")
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  write_fake_jj(dir <> "/bin/jj")
  test_helpers.chmod_executable(dir <> "/bin/jj")
}

fn run_sync_in(dir: String, env_prefix: String) -> step_artifact.StepArtifact {
  let command =
    "env -u SCHERZO_ORIGIN_SYNC_REMOTE -u SCHERZO_ORIGIN_SYNC_BRANCH -u SCHERZO_ORIGIN_SYNC_DESTINATION -u SCHERZO_ORIGIN_SYNC_REBASE_REVSET -u SCHERZO_JJ_WORKSPACE_REMOTE -u SCHERZO_JJ_WORKSPACE_BASE_BRANCH "
    <> env_prefix
    <> " SCHERZO_REPO_ROOT=\"$PWD\" PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-jj-origin-sync"

  command_step.run(
    "origin_sync",
    workflow_context_test_support.without_workflow_context(command),
    dir,
    10_000,
    [],
    test_helpers.default_artifact_limits(),
  )
}

fn occurrence_count(text: String, needle: String) -> Int {
  string.split(text, needle) |> list.length |> subtract_one
}

fn subtract_one(value: Int) -> Int {
  value - 1
}

fn assert_fetch_attempt_count(jj_log: String, expected_count: Int) -> Nil {
  assert occurrence_count(jj_log, "git fetch --remote origin") == expected_count
}

fn assert_main_advanced(jj_log: String) -> Nil {
  assert string.contains(
    jj_log,
    "bookmark advance main --to main@origin --color=never",
  )
}

fn assert_origin_sync_skips_transient_ssh_failure(
  dir: String,
  failure_kind: String,
  expected_stderr: String,
  expected_status_kind: String,
) -> Nil {
  prepare_fake_repo(dir)

  let artifact =
    run_sync_in(
      dir,
      "SCHERZO_FAKE_ORIGIN_SYNC_FETCH_SSH_AGENT_FAIL=" <> failure_kind,
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(artifact.stdout, "ORIGIN_SYNC_STEP=fetch_retry")
  assert string.contains(
    artifact.stdout,
    "ORIGIN_SYNC_STATUS=skipped_transient_ssh_agent_failure",
  )
  assert string.contains(
    artifact.stdout,
    "ORIGIN_SYNC_FETCH_FAILURE_KIND=" <> expected_status_kind,
  )
  assert string.contains(artifact.stdout, "next scheduled run will retry")
  assert string.contains(artifact.stderr, expected_stderr)
  let assert Ok(jj_log) = simplifile.read(dir <> "/jj.log")
  assert_fetch_attempt_count(jj_log, 2)
  assert !string.contains(jj_log, "bookmark advance")
  assert !string.contains(jj_log, "rebase -r")
}

pub fn origin_sync_rebases_empty_working_copy_test() {
  let dir = "test/tmp/jj-origin-sync-clean"
  prepare_fake_repo(dir)

  let artifact = run_sync_in(dir, "")

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(artifact.stdout, "ORIGIN_SYNC_STEP=advance_main")
  assert string.contains(artifact.stdout, "ORIGIN_SYNC_STATUS=rebased_clean")
  let assert Ok(jj_log) = simplifile.read(dir <> "/jj.log")
  assert string.contains(jj_log, "git fetch --remote origin")
  assert_main_advanced(jj_log)
  assert string.contains(
    jj_log,
    "log -r conflicts() & (main@origin..@) --no-graph",
  )
  assert string.contains(jj_log, "diff -r @ --name-only --color=never")
  assert string.contains(
    jj_log,
    "rebase -r main@origin..@ -d main@origin --color=never",
  )
}

pub fn origin_sync_uses_canonical_jj_workspace_remote_when_workflow_remote_unset_test() {
  let dir = "test/tmp/jj-origin-sync-canonical-remote"
  prepare_fake_repo(dir)

  let artifact =
    run_sync_in(
      dir,
      "SCHERZO_JJ_WORKSPACE_REMOTE=scherzo-agent SCHERZO_JJ_WORKSPACE_BASE_BRANCH=main",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(artifact.stdout, "ORIGIN_SYNC_REMOTE=scherzo-agent")
  assert string.contains(
    artifact.stdout,
    "ORIGIN_SYNC_DESTINATION=main@scherzo-agent",
  )
  assert string.contains(artifact.stdout, "ORIGIN_SYNC_STATUS=rebased_clean")
  let assert Ok(jj_log) = simplifile.read(dir <> "/jj.log")
  assert string.contains(jj_log, "git fetch --remote scherzo-agent")
  assert string.contains(
    jj_log,
    "bookmark advance main --to main@scherzo-agent --color=never",
  )
  assert string.contains(
    jj_log,
    "rebase -r main@scherzo-agent..@ -d main@scherzo-agent --color=never",
  )
}

pub fn origin_sync_skips_rebase_when_working_copy_has_changes_test() {
  let dir = "test/tmp/jj-origin-sync-dirty"
  prepare_fake_repo(dir)

  let artifact = run_sync_in(dir, "SCHERZO_FAKE_ORIGIN_SYNC_DIRTY=1")

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(
    artifact.stdout,
    "ORIGIN_SYNC_STATUS=skipped_dirty_working_copy",
  )
  assert string.contains(artifact.stdout, "src/dirty.gleam")
  let assert Ok(jj_log) = simplifile.read(dir <> "/jj.log")
  assert string.contains(jj_log, "git fetch --remote origin")
  assert_main_advanced(jj_log)
  assert !string.contains(jj_log, "rebase -r")
}

pub fn origin_sync_retries_transient_fetch_failure_test() {
  let dir = "test/tmp/jj-origin-sync-fetch-retry"
  prepare_fake_repo(dir)

  let artifact = run_sync_in(dir, "SCHERZO_FAKE_ORIGIN_SYNC_FETCH_FAIL_ONCE=1")

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(artifact.stdout, "ORIGIN_SYNC_STEP=fetch_retry")
  assert string.contains(artifact.stdout, "ORIGIN_SYNC_STATUS=rebased_clean")
  let assert Ok(jj_log) = simplifile.read(dir <> "/jj.log")
  assert_fetch_attempt_count(jj_log, 2)
  assert_main_advanced(jj_log)
  assert string.contains(
    jj_log,
    "rebase -r main@origin..@ -d main@origin --color=never",
  )
}

pub fn origin_sync_skips_known_transient_ssh_agent_fetch_failures_test() {
  assert_origin_sync_skips_transient_ssh_failure(
    "test/tmp/jj-origin-sync-fetch-ssh-agent-communication",
    "communication",
    "communication with agent failed",
    "ssh_agent_communication_failed",
  )
  assert_origin_sync_skips_transient_ssh_failure(
    "test/tmp/jj-origin-sync-fetch-ssh-agent-refused",
    "refused",
    "agent refused operation",
    "ssh_agent_refused_operation",
  )
  assert_origin_sync_skips_transient_ssh_failure(
    "test/tmp/jj-origin-sync-fetch-public-identity-invalid-format",
    "public_identity_invalid_format",
    "Load key \"/Users/bromanko/.ssh/github-bromanko.pub\": invalid format",
    "ssh_public_identity_invalid_format",
  )
  assert_origin_sync_skips_transient_ssh_failure(
    "test/tmp/jj-origin-sync-fetch-public-identity-libcrypto-unsupported",
    "public_identity_libcrypto_unsupported",
    "Load key \"/Users/bromanko/.ssh/github-bromanko.pub\": error in libcrypto: unsupported",
    "ssh_public_identity_libcrypto_unsupported",
  )
}

pub fn origin_sync_reports_bare_publickey_denied_fetch_failure_test() {
  let dir = "test/tmp/jj-origin-sync-fetch-publickey-denied"
  prepare_fake_repo(dir)

  let artifact =
    run_sync_in(
      dir,
      "SCHERZO_FAKE_ORIGIN_SYNC_FETCH_SSH_AGENT_FAIL=permission_denied",
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(1)
  assert string.contains(artifact.stdout, "ORIGIN_SYNC_STEP=fetch_retry")
  assert string.contains(artifact.stdout, "ORIGIN_SYNC_STATUS=fetch_failed")
  assert string.contains(artifact.stderr, "Permission denied (publickey)")
  let assert Ok(jj_log) = simplifile.read(dir <> "/jj.log")
  assert_fetch_attempt_count(jj_log, 2)
  assert !string.contains(jj_log, "bookmark advance")
  assert !string.contains(jj_log, "rebase -r")
}

pub fn origin_sync_reports_persistent_non_ssh_fetch_failure_test() {
  let dir = "test/tmp/jj-origin-sync-fetch-generic-failure"
  prepare_fake_repo(dir)

  let artifact =
    run_sync_in(dir, "SCHERZO_FAKE_ORIGIN_SYNC_FETCH_FAIL_ALWAYS=1")

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(1)
  assert string.contains(artifact.stdout, "ORIGIN_SYNC_STEP=fetch_retry")
  assert string.contains(artifact.stdout, "ORIGIN_SYNC_STATUS=fetch_failed")
  assert string.contains(artifact.stderr, "persistent fetch failure")
  let assert Ok(jj_log) = simplifile.read(dir <> "/jj.log")
  assert_fetch_attempt_count(jj_log, 2)
  assert !string.contains(jj_log, "bookmark advance")
  assert !string.contains(jj_log, "rebase -r")
}

pub fn origin_sync_reports_missing_origin_main_test() {
  let dir = "test/tmp/jj-origin-sync-missing-origin-main"
  prepare_fake_repo(dir)

  let artifact = run_sync_in(dir, "SCHERZO_FAKE_ORIGIN_SYNC_ORIGIN_MISSING=1")

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(1)
  assert string.contains(
    artifact.stdout,
    "ORIGIN_SYNC_STEP=inspect_main_policy",
  )
  assert string.contains(
    artifact.stdout,
    "ORIGIN_SYNC_STATUS=destination_missing",
  )
  let assert Ok(jj_log) = simplifile.read(dir <> "/jj.log")
  assert string.contains(jj_log, "git fetch --remote origin")
  assert !string.contains(jj_log, "bookmark advance")
  assert !string.contains(jj_log, "rebase -r")
}

pub fn origin_sync_reports_missing_local_main_test() {
  let dir = "test/tmp/jj-origin-sync-missing-local-main"
  prepare_fake_repo(dir)

  let artifact =
    run_sync_in(dir, "SCHERZO_FAKE_ORIGIN_SYNC_LOCAL_MAIN_MISSING=1")

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(1)
  assert string.contains(
    artifact.stdout,
    "ORIGIN_SYNC_STATUS=local_main_missing",
  )
  assert string.contains(artifact.stdout, "create or restore it")
  let assert Ok(jj_log) = simplifile.read(dir <> "/jj.log")
  assert string.contains(jj_log, "git fetch --remote origin")
  assert !string.contains(jj_log, "bookmark advance")
  assert !string.contains(jj_log, "rebase -r")
}

pub fn origin_sync_reports_local_main_conflicts_test() {
  let dir = "test/tmp/jj-origin-sync-local-main-conflicts"
  prepare_fake_repo(dir)

  let artifact =
    run_sync_in(dir, "SCHERZO_FAKE_ORIGIN_SYNC_LOCAL_MAIN_CONFLICTS=1")

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(1)
  assert string.contains(
    artifact.stdout,
    "ORIGIN_SYNC_STATUS=local_main_conflicted",
  )
  assert string.contains(artifact.stdout, "mainconflict")
  let assert Ok(jj_log) = simplifile.read(dir <> "/jj.log")
  assert string.contains(jj_log, "git fetch --remote origin")
  assert !string.contains(jj_log, "bookmark advance")
  assert !string.contains(jj_log, "rebase -r")
}

pub fn origin_sync_reports_local_main_advance_failure_test() {
  let dir = "test/tmp/jj-origin-sync-main-advance-failure"
  prepare_fake_repo(dir)

  let artifact = run_sync_in(dir, "SCHERZO_FAKE_ORIGIN_SYNC_ADVANCE_FAIL=1")

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(1)
  assert string.contains(
    artifact.stdout,
    "ORIGIN_SYNC_STATUS=local_main_advance_failed",
  )
  assert string.contains(artifact.stdout, "sideways/backward move")
  assert string.contains(artifact.stderr, "Refusing to advance bookmark")
  let assert Ok(jj_log) = simplifile.read(dir <> "/jj.log")
  assert string.contains(jj_log, "git fetch --remote origin")
  assert_main_advanced(jj_log)
  assert !string.contains(jj_log, "rebase -r")
}

pub fn origin_sync_reports_rebase_failure_test() {
  let dir = "test/tmp/jj-origin-sync-rebase-failure"
  prepare_fake_repo(dir)

  let artifact = run_sync_in(dir, "SCHERZO_FAKE_ORIGIN_SYNC_REBASE_FAIL=1")

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(1)
  assert string.contains(artifact.stdout, "ORIGIN_SYNC_STATUS=rebase_failed")
  let assert Ok(jj_log) = simplifile.read(dir <> "/jj.log")
  assert string.contains(jj_log, "git fetch --remote origin")
  assert_main_advanced(jj_log)
  assert string.contains(
    jj_log,
    "rebase -r main@origin..@ -d main@origin --color=never",
  )
}

pub fn origin_sync_reports_rebase_conflicts_test() {
  let dir = "test/tmp/jj-origin-sync-rebase-conflicts"
  prepare_fake_repo(dir)

  let artifact =
    run_sync_in(dir, "SCHERZO_FAKE_ORIGIN_SYNC_REBASE_CREATES_CONFLICTS=1")

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(1)
  assert string.contains(
    artifact.stdout,
    "ORIGIN_SYNC_STATUS=rebased_with_conflicts",
  )
  assert string.contains(artifact.stdout, "rebaseconflict")
  let assert Ok(jj_log) = simplifile.read(dir <> "/jj.log")
  assert string.contains(jj_log, "git fetch --remote origin")
  assert_main_advanced(jj_log)
  assert string.contains(
    jj_log,
    "rebase -r main@origin..@ -d main@origin --color=never",
  )
}

pub fn origin_sync_fails_when_current_change_has_conflicts_test() {
  let dir = "test/tmp/jj-origin-sync-existing-conflicts"
  prepare_fake_repo(dir)

  let artifact = run_sync_in(dir, "SCHERZO_FAKE_ORIGIN_SYNC_CONFLICTS=1")

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(1)
  assert string.contains(
    artifact.stdout,
    "ORIGIN_SYNC_STATUS=blocked_existing_conflicts",
  )
  assert string.contains(artifact.stdout, "root working-copy stack")
  let assert Ok(jj_log) = simplifile.read(dir <> "/jj.log")
  assert string.contains(jj_log, "git fetch --remote origin")
  assert_main_advanced(jj_log)
  assert !string.contains(jj_log, "rebase -r")
}

fn write_fake_jj(path: String) -> Nil {
  let assert Ok(Nil) =
    simplifile.write(
      path,
      "#!/bin/sh\n"
        <> "printf '%s\\n' \"$*\" >> jj.log\n"
        <> "if [ \"$1\" = git ] && [ \"$2\" = fetch ]; then\n"
        <> "  if [ \"${SCHERZO_FAKE_ORIGIN_SYNC_FETCH_FAIL_ONCE:-}\" = 1 ] && [ ! -f fetch-failed-once ]; then touch fetch-failed-once; echo simulated fetch race >&2; exit 1; fi\n"
        <> "  if [ \"${SCHERZO_FAKE_ORIGIN_SYNC_FETCH_FAIL_ALWAYS:-}\" = 1 ]; then echo persistent fetch failure >&2; exit 1; fi\n"
        <> "  case \"${SCHERZO_FAKE_ORIGIN_SYNC_FETCH_SSH_AGENT_FAIL:-}\" in\n"
        <> "    communication) echo 'sign_and_send_pubkey: signing failed for ED25519 SHA256:fake from agent: communication with agent failed' >&2; echo 'git@github.com: Permission denied (publickey).' >&2; exit 1 ;;\n"
        <> "    refused) echo 'sign_and_send_pubkey: signing failed for ED25519 SHA256:fake from agent: agent refused operation' >&2; echo 'git@github.com: Permission denied (publickey).' >&2; exit 1 ;;\n"
        <> "    public_identity_invalid_format) echo 'git: Load key \"/Users/bromanko/.ssh/github-bromanko.pub\": invalid format' >&2; echo 'git: git@github.com: Permission denied (publickey).' >&2; exit 1 ;;\n"
        <> "    public_identity_libcrypto_unsupported) echo 'git: Load key \"/Users/bromanko/.ssh/github-bromanko.pub\": error in libcrypto: unsupported' >&2; echo 'git: git@github.com: Permission denied (publickey).' >&2; exit 1 ;;\n"
        <> "    permission_denied) echo 'git@github.com: Permission denied (publickey).' >&2; exit 1 ;;\n"
        <> "    '') ;;\n"
        <> "    *) echo unexpected fake SSH agent failure kind >&2; exit 1 ;;\n"
        <> "  esac\n"
        <> "  exit 0\n"
        <> "fi\n"
        <> "if [ \"$1\" = log ] && [ \"$2\" = -r ] && [ \"$3\" = main@origin ]; then\n"
        <> "  if [ \"${SCHERZO_FAKE_ORIGIN_SYNC_ORIGIN_MISSING:-}\" = 1 ]; then echo missing main@origin >&2; exit 1; fi\n"
        <> "  echo commit-main-origin\n"
        <> "  exit 0\n"
        <> "fi\n"
        <> "if [ \"$1\" = log ] && [ \"$2\" = -r ] && [ \"$3\" = main ]; then\n"
        <> "  if [ \"${SCHERZO_FAKE_ORIGIN_SYNC_LOCAL_MAIN_MISSING:-}\" = 1 ]; then echo missing local main >&2; exit 1; fi\n"
        <> "  echo commit-main\n"
        <> "  exit 0\n"
        <> "fi\n"
        <> "if [ \"$1\" = log ] && [ \"$2\" = -r ] && [ \"$3\" = 'conflicts() & (main)' ]; then\n"
        <> "  if [ \"${SCHERZO_FAKE_ORIGIN_SYNC_LOCAL_MAIN_CONFLICTS:-}\" = 1 ]; then echo mainconflict; fi\n"
        <> "  exit 0\n"
        <> "fi\n"
        <> "if [ \"$1\" = bookmark ] && [ \"$2\" = advance ]; then\n"
        <> "  if [ \"${SCHERZO_FAKE_ORIGIN_SYNC_ADVANCE_FAIL:-}\" = 1 ]; then echo 'Error: Refusing to advance bookmark backwards or sideways: main' >&2; exit 1; fi\n"
        <> "  echo advanced main\n"
        <> "  exit 0\n"
        <> "fi\n"
        <> "if [ \"$1\" = log ] && [ \"$2\" = -r ] && [ \"$3\" = '(main) & (main@origin)' ]; then\n"
        <> "  echo commit-main-origin\n"
        <> "  exit 0\n"
        <> "fi\n"
        <> "if [ \"$1\" = log ] && [ \"$2\" = -r ]; then\n"
        <> "  case \"$3\" in '(main) & ('*')') echo commit-main-destination; exit 0 ;; esac\n"
        <> "fi\n"
        <> "if [ \"$1\" = log ] && [ \"$2\" = -r ] && [ \"$3\" = 'conflicts() & (main@origin..@)' ]; then\n"
        <> "  if [ \"${SCHERZO_FAKE_ORIGIN_SYNC_CONFLICTS:-}\" = 1 ]; then echo stackconflict; fi\n"
        <> "  if [ \"${SCHERZO_FAKE_ORIGIN_SYNC_REBASE_CREATES_CONFLICTS:-}\" = 1 ] && [ -f rebased ]; then echo rebaseconflict; fi\n"
        <> "  exit 0\n"
        <> "fi\n"
        <> "if [ \"$1\" = diff ] && [ \"$2\" = -r ]; then\n"
        <> "  if [ \"${SCHERZO_FAKE_ORIGIN_SYNC_DIRTY:-}\" = 1 ]; then echo src/dirty.gleam; fi\n"
        <> "  exit 0\n"
        <> "fi\n"
        <> "if [ \"$1\" = rebase ]; then\n"
        <> "  if [ \"${SCHERZO_FAKE_ORIGIN_SYNC_REBASE_FAIL:-}\" = 1 ]; then echo simulated rebase failure >&2; exit 1; fi\n"
        <> "  touch rebased\n"
        <> "  echo rebased\n"
        <> "  exit 0\n"
        <> "fi\n"
        <> "if [ \"$1\" = log ] && [ \"$2\" = -r ]; then exit 0; fi\n"
        <> "echo unexpected jj args: $* >&2\n"
        <> "exit 1\n",
    )
  Nil
}
