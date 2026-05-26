import gleam/option.{Some}
import gleam/string
import scherzo/command_step
import scherzo/step_artifact
import simplifile
import support/test_helpers
import workflow_context_test_support

fn run_agent_helper_in(
  cwd: String,
  command: String,
) -> step_artifact.StepArtifact {
  command_step.run(
    "agent-helper",
    workflow_context_test_support.without_workflow_context(command),
    cwd,
    10_000,
    [],
    test_helpers.default_artifact_limits(),
  )
}

fn clean_agent_env_prefix() -> String {
  "env -u SCHERZO_AGENT_JJ_WORKSPACE_REMOTE"
  <> " -u SCHERZO_AGENT_JJ_WORKSPACE_PUBLISH_REMOTE"
  <> " -u SCHERZO_AGENT_JJ_WORKSPACE_BASE_BRANCH"
  <> " -u SCHERZO_AGENT_GITHUB_REPO"
  <> " -u SCHERZO_AGENT_PR_REMOTE"
  <> " -u SCHERZO_AGENT_PR_REPO"
  <> " -u SCHERZO_AGENT_GIT_NAME"
  <> " -u SCHERZO_AGENT_GIT_EMAIL"
  <> " -u SCHERZO_AGENT_SSH_HOST"
  <> " -u SCHERZO_AGENT_GITHUB_TOKEN"
  <> " -u SCHERZO_AGENT_GITHUB_LOGIN"
  <> " -u SCHERZO_AGENT_LINEAR_API_KEY"
  <> " -u GH_TOKEN"
  <> " -u GITHUB_TOKEN"
  <> " -u LINEAR_API_KEY"
  <> " -u JJ_CONFIG"
  <> " -u GIT_SSH_COMMAND "
}

fn agent_identity_env() -> String {
  "SCHERZO_REPO_ROOT=\"$PWD\" "
  <> "SCHERZO_AGENT_GITHUB_TOKEN=github-token "
  <> "SCHERZO_AGENT_GITHUB_LOGIN=agent-user "
  <> "SCHERZO_AGENT_LINEAR_API_KEY=linear-token "
  <> "SCHERZO_AGENT_GIT_EMAIL=agent@example.com "
  <> "PATH=\"$PWD/bin:$PATH\" "
}

fn setup_agent_fixture(dir: String, remotes: String) -> Nil {
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  write_fake_jj(dir <> "/bin/jj", remotes)
  write_fake_git(dir <> "/bin/git")
  write_fake_gh(dir <> "/bin/gh")
  write_fake_curl(dir <> "/bin/curl")
  write_fake_jq(dir <> "/bin/jq")
  write_fake_ssh(dir <> "/bin/ssh")
  test_helpers.chmod_executable(dir <> "/bin/jj")
  test_helpers.chmod_executable(dir <> "/bin/git")
  test_helpers.chmod_executable(dir <> "/bin/gh")
  test_helpers.chmod_executable(dir <> "/bin/curl")
  test_helpers.chmod_executable(dir <> "/bin/jq")
  test_helpers.chmod_executable(dir <> "/bin/ssh")
}

pub fn agent_whoami_uses_canonical_jj_and_github_env_test() {
  let dir = "test/tmp/agent-helper-canonical-env"
  setup_agent_fixture(
    dir,
    "scherzo-agent git@github-scherzo-agent:scherzo-systems/scherzo.git",
  )

  let artifact =
    run_agent_helper_in(
      dir,
      clean_agent_env_prefix()
        <> agent_identity_env()
        <> "SCHERZO_AGENT_JJ_WORKSPACE_REMOTE=scherzo-agent "
        <> "SCHERZO_AGENT_JJ_WORKSPACE_PUBLISH_REMOTE=scherzo-agent "
        <> "SCHERZO_AGENT_JJ_WORKSPACE_BASE_BRANCH=main "
        <> "SCHERZO_AGENT_GITHUB_REPO=scherzo-systems/scherzo "
        <> "../../../scripts/scherzo-agent-whoami",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(
    artifact.stdout,
    "GitHub repository=scherzo-systems/scherzo",
  )
  assert string.contains(
    artifact.stdout,
    "SCHERZO_AGENT_JJ_WORKSPACE_PUBLISH_REMOTE=scherzo-agent",
  )
  assert string.contains(
    artifact.stdout,
    "remote url=git@github-scherzo-agent:scherzo-systems/scherzo.git",
  )
  assert !string.contains(artifact.stdout, "SCHERZO_PR_REMOTE=")
}

pub fn agent_whoami_keeps_legacy_agent_aliases_as_compatibility_inputs_test() {
  let dir = "test/tmp/agent-helper-legacy-compat-env"
  setup_agent_fixture(
    dir,
    "legacy-agent git@github-scherzo-agent:scherzo-systems/scherzo.git",
  )

  let artifact =
    run_agent_helper_in(
      dir,
      clean_agent_env_prefix()
        <> agent_identity_env()
        <> "SCHERZO_AGENT_PR_REMOTE=legacy-agent "
        <> "SCHERZO_AGENT_PR_REPO=scherzo-systems/scherzo "
        <> "../../../scripts/scherzo-agent-whoami",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(
    artifact.stdout,
    "SCHERZO_AGENT_JJ_WORKSPACE_PUBLISH_REMOTE=legacy-agent",
  )
  assert string.contains(
    artifact.stdout,
    "remote url=git@github-scherzo-agent:scherzo-systems/scherzo.git",
  )
  assert string.contains(
    artifact.stdout,
    "GitHub repository=scherzo-systems/scherzo",
  )
}

pub fn agent_whoami_reports_missing_canonical_publish_remote_test() {
  let dir = "test/tmp/agent-helper-missing-remote"
  setup_agent_fixture(
    dir,
    "scherzo-agent git@github-scherzo-agent:scherzo-systems/scherzo.git",
  )

  let artifact =
    run_agent_helper_in(
      dir,
      clean_agent_env_prefix()
        <> agent_identity_env()
        <> "SCHERZO_AGENT_JJ_WORKSPACE_REMOTE=scherzo-agent "
        <> "SCHERZO_AGENT_JJ_WORKSPACE_PUBLISH_REMOTE=missing "
        <> "SCHERZO_AGENT_GITHUB_REPO=scherzo-systems/scherzo "
        <> "../../../scripts/scherzo-agent-whoami",
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(1)
  assert string.contains(
    artifact.stderr,
    "remote 'missing' was not found; add it with: jj git remote add missing git@github-scherzo-agent:scherzo-systems/scherzo.git",
  )
}

fn write_fake_jj(path: String, remotes: String) -> Nil {
  let assert Ok(Nil) =
    simplifile.write(
      path <> ".env",
      "SCHERZO_FAKE_JJ_REMOTES='" <> remotes <> "'\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      path,
      "#!/bin/sh\n"
        <> ". \"$0.env\"\n"
        <> "if [ \"$1\" = git ] && [ \"$2\" = remote ] && [ \"$3\" = list ]; then printf '%s\\n' \"$SCHERZO_FAKE_JJ_REMOTES\"; exit 0; fi\n"
        <> "if [ \"$1\" = config ] && [ \"$2\" = get ]; then\n"
        <> "  case \"$3\" in user.name) echo \"${SCHERZO_AGENT_GIT_NAME:-Scherzo Agent}\"; exit 0;; user.email) echo \"$SCHERZO_AGENT_GIT_EMAIL\"; exit 0;; esac\n"
        <> "fi\n"
        <> "exit 1\n",
    )
  Nil
}

fn write_fake_git(path: String) -> Nil {
  let assert Ok(Nil) =
    simplifile.write(
      path,
      "#!/bin/sh\n"
        <> "if [ \"$1\" = var ]; then echo \"${SCHERZO_AGENT_GIT_NAME:-Scherzo Agent} <$SCHERZO_AGENT_GIT_EMAIL> 0 +0000\"; exit 0; fi\n"
        <> "if [ \"$1\" = remote ] && [ \"$2\" = get-url ]; then exit 1; fi\n"
        <> "exit 1\n",
    )
  Nil
}

fn write_fake_gh(path: String) -> Nil {
  let assert Ok(Nil) =
    simplifile.write(
      path,
      "#!/bin/sh\n"
        <> "if [ \"$1\" = api ] && [ \"$2\" = user ]; then echo \"${SCHERZO_FAKE_GH_LOGIN:-agent-user}\"; exit 0; fi\n"
        <> "if [ \"$1\" = api ]; then\n"
        <> "  case \"$2\" in repos/*) repo=${SCHERZO_FAKE_GH_REPO:-$SCHERZO_GITHUB_REPO}; [ -n \"$repo\" ] || repo=scherzo-systems/scherzo; echo \"$repo\"; exit 0;; esac\n"
        <> "fi\n"
        <> "exit 1\n",
    )
  Nil
}

fn write_fake_curl(path: String) -> Nil {
  let assert Ok(Nil) =
    simplifile.write(
      path,
      "#!/bin/sh\n"
        <> "out=\nheaders=\nprev=\n"
        <> "for arg in \"$@\"; do\n"
        <> "  if [ \"$prev\" = -o ]; then out=$arg; fi\n"
        <> "  if [ \"$prev\" = -D ]; then headers=$arg; fi\n"
        <> "  prev=$arg\n"
        <> "done\n"
        <> "case \"$*\" in *api.linear.app/graphql*) printf '%s\\n' '{\"data\":{\"viewer\":{\"name\":\"Linear Agent\"}}}'; exit 0;; esac\n"
        <> "[ -n \"$out\" ] && printf '%s\\n' '{\"message\":\"Validation Failed\"}' > \"$out\"\n"
        <> "[ -n \"$headers\" ] && printf '%s\\n' 'x-accepted-github-permissions: pull_requests=write' > \"$headers\"\n"
        <> "printf '%s' 422\n"
        <> "exit 0\n",
    )
  Nil
}

fn write_fake_jq(path: String) -> Nil {
  let assert Ok(Nil) =
    simplifile.write(
      path,
      "#!/bin/sh\n"
        <> "if [ \"$1\" = -e ]; then exit 0; fi\n"
        <> "if [ \"$1\" = -r ]; then echo 'Linear Agent'; exit 0; fi\n"
        <> "exit 0\n",
    )
  Nil
}

fn write_fake_ssh(path: String) -> Nil {
  let assert Ok(Nil) =
    simplifile.write(
      path,
      "#!/bin/sh\n"
        <> "echo \"Hi ${SCHERZO_AGENT_GITHUB_LOGIN:-agent-user}! You've successfully authenticated, but GitHub does not provide shell access.\"\n"
        <> "exit 1\n",
    )
  Nil
}
