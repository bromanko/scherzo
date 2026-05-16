import gleam/int
import gleam/option.{Some}
import gleam/string
import scherzo/command_step
import scherzo/config/types as config_types
import scherzo/step_artifact
import simplifile

const observed_at = "2026-05-09T20:00:00Z"

fn limits() -> config_types.ArtifactLimits {
  config_types.ArtifactLimits(
    command_stream_max_chars: 20_000,
    template_field_max_chars: 20_000,
    workflow_summary_max_chars: 20_000,
  )
}

fn reset_dir(path: String) -> Nil {
  let _ = simplifile.delete(path)
  let assert Ok(Nil) = simplifile.create_directory_all(path)
  Nil
}

fn write_fixture(dir: String, body: String) -> String {
  reset_dir(dir)
  let path = dir <> "/fixture.json"
  let assert Ok(Nil) = simplifile.write(path, body)
  path
}

fn run_scout(command: String) -> step_artifact.StepArtifact {
  command_step.run(
    "github-pr-conflict-scout",
    "scripts/scherzo-github-pr-conflict-scout "
      <> command
      <> " --repo scherzo-systems/scherzo --linear-project-slug test-project",
    ".",
    10_000,
    [],
    limits(),
  )
}

fn run_scout_raw(command: String) -> step_artifact.StepArtifact {
  command_step.run(
    "github-pr-conflict-scout",
    command,
    ".",
    10_000,
    [],
    limits(),
  )
}

fn noop_fixture() -> String {
  "{\n"
  <> "  \"observed_at\": \""
  <> observed_at
  <> "\",\n"
  <> "  \"github\": {\"pulls\": []},\n"
  <> "  \"linear\": {\"fail_if_called\": true}\n"
  <> "}\n"
}

fn safe_dirty_fixture(linear_body: String) -> String {
  "{\n"
  <> "  \"observed_at\": \""
  <> observed_at
  <> "\",\n"
  <> "  \"github\": {\n"
  <> "    \"pulls\": ["
  <> safe_pr_json(123, "feature/conflicted-change")
  <> "],\n"
  <> "    \"details\": {\n"
  <> "      \"123\": {\"mergeable\": false, \"mergeable_state\": \"dirty\", \"base\": {\"sha\": \"base-sha\"}, \"head\": {\"sha\": \"head-sha\"}}\n"
  <> "    }\n"
  <> "  },\n"
  <> "  \"linear\": "
  <> linear_body
  <> "\n"
  <> "}\n"
}

fn safe_pr_json(number: Int, head_branch: String) -> String {
  let number_string = int.to_string(number)
  "{\n"
  <> "  \"number\": "
  <> number_string
  <> ",\n"
  <> "  \"html_url\": \"https://github.com/scherzo-systems/scherzo/pull/"
  <> number_string
  <> "\",\n"
  <> "  \"draft\": false,\n"
  <> "  \"base\": {\"repo\": {\"full_name\": \"scherzo-systems/scherzo\"}, \"ref\": \"main\", \"sha\": \"base-sha\"},\n"
  <> "  \"head\": {\"repo\": {\"full_name\": \"scherzo-systems/scherzo\"}, \"ref\": \""
  <> head_branch
  <> "\", \"sha\": \"head-sha\"}\n"
  <> "}"
}

fn linear_project_with_issues(issues_json: String) -> String {
  "{\n"
  <> "  \"project\": {\n"
  <> "    \"id\": \"project-id\",\n"
  <> "    \"teams\": [{\n"
  <> "      \"id\": \"team-id\",\n"
  <> "      \"states\": [\n"
  <> "        {\"id\": \"todo-state-id\", \"name\": \"Todo\", \"type\": \"unstarted\"},\n"
  <> "        {\"id\": \"in-progress-state-id\", \"name\": \"In Progress\", \"type\": \"started\"},\n"
  <> "        {\"id\": \"triage-state-id\", \"name\": \"Triage\", \"type\": \"triage\"}\n"
  <> "      ],\n"
  <> "      \"labels\": [\n"
  <> "        {\"id\": \"workflow-label-id\", \"name\": \"workflow:merge-conflict-resolution\"},\n"
  <> "        {\"id\": \"support-label-id\", \"name\": \"needs-workflow\"}\n"
  <> "      ]\n"
  <> "    }]\n"
  <> "  },\n"
  <> "  \"issues\": "
  <> issues_json
  <> "\n"
  <> "}"
}

fn existing_issue(
  id: String,
  identifier: String,
  state_name: String,
  description: String,
) -> String {
  "{\n"
  <> "  \"id\": \""
  <> id
  <> "\",\n"
  <> "  \"identifier\": \""
  <> identifier
  <> "\",\n"
  <> "  \"title\": \"Resolve merge conflicts for PR #123\",\n"
  <> "  \"description\": "
  <> json_string(description)
  <> ",\n"
  <> "  \"url\": \"https://linear.app/living-systems/issue/"
  <> identifier
  <> "\",\n"
  <> "  \"createdAt\": \"2026-05-09T19:00:00Z\",\n"
  <> "  \"updatedAt\": \"2026-05-09T19:00:00Z\",\n"
  <> "  \"state\": {\"name\": \""
  <> state_name
  <> "\", \"type\": \""
  <> state_type(state_name)
  <> "\"},\n"
  <> "  \"labels\": {\"nodes\": [{\"id\": \"workflow-label-id\", \"name\": \"workflow:merge-conflict-resolution\"}]}\n"
  <> "}"
}

fn state_type(name: String) -> String {
  case name {
    "Todo" -> "unstarted"
    "In Progress" -> "started"
    "Triage" -> "triage"
    _ -> "unstarted"
  }
}

fn generated_description(detection: String) -> String {
  "github-pr-conflict:scherzo-systems/scherzo#123\n"
  <> "\n"
  <> "GitHub PR: https://github.com/scherzo-systems/scherzo/pull/123\n"
  <> "Repository: scherzo-systems/scherzo\n"
  <> "PR base ref: main\n"
  <> "PR head ref: feature/conflicted-change\n"
  <> "Base SHA: base-sha\n"
  <> "Head SHA: head-sha\n"
  <> "Detection: "
  <> detection
  <> "\n"
  <> "Observed at: "
  <> observed_at
  <> "\n"
  <> "\n"
  <> "Scherzo's workflow:merge-conflict-resolution resolver should repair this same-repository PR."
}

fn json_string(value: String) -> String {
  "\""
  <> string.replace(string.replace(value, "\\", "\\\\"), "\n", "\\n")
  <> "\""
}

pub fn scout_noop_open_prs_succeeds_without_linear_test() {
  let fixture =
    write_fixture("test/tmp/github-pr-conflict-scout-noop", noop_fixture())

  let artifact = run_scout("scan-fixture " <> fixture <> " --json-summary")

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(artifact.stdout, "\"created\": []")
  assert string.contains(artifact.stdout, "\"updated\": []")
  assert artifact.stderr == ""
}

pub fn scout_default_noop_is_silent_test() {
  let fixture =
    write_fixture("test/tmp/github-pr-conflict-scout-silent", noop_fixture())

  let artifact = run_scout("scan-fixture " <> fixture)

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert artifact.stdout == ""
  assert artifact.stderr == ""
}

pub fn scout_uses_environment_configuration_when_flags_are_omitted_test() {
  let fixture =
    write_fixture(
      "test/tmp/github-pr-conflict-scout-env-config",
      safe_dirty_fixture(linear_project_with_issues("[]")),
    )

  let artifact =
    run_scout_raw(
      "env SCHERZO_GITHUB_REPO=scherzo-systems/scherzo "
      <> "SCHERZO_LINEAR_PROJECT_SLUG=test-project "
      <> "scripts/scherzo-github-pr-conflict-scout scan-fixture "
      <> fixture
      <> " --json-summary",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(artifact.stdout, "\"created\": [")
  assert string.contains(
    artifact.stdout,
    "github-pr-conflict:scherzo-systems/scherzo#123",
  )
  assert artifact.stderr == ""
}

pub fn scout_requires_repo_configuration_when_flags_and_env_are_missing_test() {
  let fixture =
    write_fixture(
      "test/tmp/github-pr-conflict-scout-missing-repo",
      noop_fixture(),
    )

  let artifact =
    run_scout_raw(
      "env -u SCHERZO_GITHUB_REPO -u GITHUB_REPOSITORY "
      <> "-u SCHERZO_LINEAR_PROJECT_SLUG -u LINEAR_PROJECT_SLUG "
      <> "scripts/scherzo-github-pr-conflict-scout scan-fixture "
      <> fixture
      <> " --linear-project-slug test-project",
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(1)
  assert string.contains(
    artifact.stderr,
    "set --repo, SCHERZO_GITHUB_REPO, or GITHUB_REPOSITORY",
  )
}

pub fn scout_requires_project_slug_configuration_when_flags_and_env_are_missing_test() {
  let fixture =
    write_fixture(
      "test/tmp/github-pr-conflict-scout-missing-project",
      noop_fixture(),
    )

  let artifact =
    run_scout_raw(
      "env -u SCHERZO_GITHUB_REPO -u GITHUB_REPOSITORY "
      <> "-u SCHERZO_LINEAR_PROJECT_SLUG -u LINEAR_PROJECT_SLUG "
      <> "scripts/scherzo-github-pr-conflict-scout scan-fixture "
      <> fixture
      <> " --repo scherzo-systems/scherzo",
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(1)
  assert string.contains(
    artifact.stderr,
    "set --linear-project-slug, SCHERZO_LINEAR_PROJECT_SLUG, or LINEAR_PROJECT_SLUG",
  )
}

pub fn scout_conflicted_same_repo_pr_creates_resolver_issue_test() {
  let fixture =
    write_fixture(
      "test/tmp/github-pr-conflict-scout-create",
      safe_dirty_fixture(linear_project_with_issues("[]")),
    )

  let artifact = run_scout("scan-fixture " <> fixture <> " --json-summary")

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(artifact.stdout, "\"created\": [")
  assert string.contains(artifact.stdout, "Resolve merge conflicts for PR #123")
  assert string.contains(
    artifact.stdout,
    "https://github.com/scherzo-systems/scherzo/pull/123",
  )
  assert string.contains(
    artifact.stdout,
    "github-pr-conflict:scherzo-systems/scherzo#123",
  )
  assert string.contains(
    artifact.stdout,
    "PR head ref: feature/conflicted-change",
  )
  assert !string.contains(artifact.stdout, "Head branch:")
  assert string.contains(artifact.stdout, "workflow-label-id")
  assert !string.contains(artifact.stdout, "support-label-id")
  assert artifact.stderr == ""
}

pub fn scout_max_open_prs_caps_fixture_scan_test() {
  let fixture =
    write_fixture(
      "test/tmp/github-pr-conflict-scout-max-open-prs",
      "{\n"
        <> "  \"observed_at\": \""
        <> observed_at
        <> "\",\n"
        <> "  \"github\": {\n"
        <> "    \"pulls\": ["
        <> safe_pr_json(123, "feature/first-conflict")
        <> ","
        <> safe_pr_json(124, "feature/second-conflict")
        <> "],\n"
        <> "    \"details\": {\n"
        <> "      \"123\": {\"mergeable\": false, \"mergeable_state\": \"dirty\", \"base\": {\"sha\": \"base-sha\"}, \"head\": {\"sha\": \"head-sha\"}},\n"
        <> "      \"124\": {\"mergeable\": false, \"mergeable_state\": \"dirty\", \"base\": {\"sha\": \"base-sha-2\"}, \"head\": {\"sha\": \"head-sha-2\"}}\n"
        <> "    }\n"
        <> "  },\n"
        <> "  \"linear\": "
        <> linear_project_with_issues("[]")
        <> "\n"
        <> "}\n",
    )

  let artifact =
    run_scout("scan-fixture " <> fixture <> " --max-open-prs 1 --json-summary")

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(artifact.stdout, "max_open_prs_reached")
  assert string.contains(artifact.stdout, "Resolve merge conflicts for PR #123")
  assert !string.contains(
    artifact.stdout,
    "Resolve merge conflicts for PR #124",
  )
  assert !string.contains(
    artifact.stdout,
    "github-pr-conflict:scherzo-systems/scherzo#124",
  )
  assert artifact.stderr == ""
}

pub fn scout_existing_dispatchable_marker_updates_or_noops_test() {
  let issue =
    existing_issue(
      "existing-issue-id",
      "LIV-500",
      "Todo",
      "github-pr-conflict:scherzo-systems/scherzo#123\n\nStale body.\n",
    )
  let fixture =
    write_fixture(
      "test/tmp/github-pr-conflict-scout-update",
      safe_dirty_fixture(linear_project_with_issues("[" <> issue <> "]")),
    )

  let artifact = run_scout("scan-fixture " <> fixture <> " --json-summary")

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(artifact.stdout, "\"updated\": [")
  assert string.contains(artifact.stdout, "existing-issue-id")
  assert string.contains(artifact.stdout, "LIV-500")
  assert string.contains(artifact.stdout, "\"created\": []")
  assert artifact.stderr == ""
}

pub fn scout_identical_existing_marker_is_noop_test() {
  let issue =
    existing_issue(
      "existing-issue-id",
      "LIV-501",
      "Todo",
      generated_description("github:dirty"),
    )
  let fixture =
    write_fixture(
      "test/tmp/github-pr-conflict-scout-noop-existing",
      safe_dirty_fixture(linear_project_with_issues("[" <> issue <> "]")),
    )

  let artifact = run_scout("scan-fixture " <> fixture <> " --json-summary")

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(artifact.stdout, "\"noop\": [")
  assert string.contains(artifact.stdout, "existing-issue-id")
  assert string.contains(artifact.stdout, "\"created\": []")
  assert string.contains(artifact.stdout, "\"updated\": []")
  assert artifact.stderr == ""
}

pub fn scout_ignores_triage_marker_and_creates_dispatchable_issue_test() {
  let triage_issue =
    existing_issue(
      "triage-issue-id",
      "LIV-502",
      "Triage",
      "github-pr-conflict:scherzo-systems/scherzo#123\n\nOld triage copy.\n",
    )
  let fixture =
    write_fixture(
      "test/tmp/github-pr-conflict-scout-triage",
      safe_dirty_fixture(linear_project_with_issues("[" <> triage_issue <> "]")),
    )

  let artifact = run_scout("scan-fixture " <> fixture <> " --json-summary")

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(artifact.stdout, "historical_marker_issues_ignored")
  assert string.contains(artifact.stdout, "LIV-502")
  assert string.contains(artifact.stdout, "\"created\": [")
  assert string.contains(artifact.stdout, "\"state\": \"Todo\"")
  assert artifact.stderr == ""
}

pub fn scout_skips_unsafe_prs_test() {
  let fixture =
    write_fixture(
      "test/tmp/github-pr-conflict-scout-unsafe",
      "{\n"
        <> "  \"observed_at\": \""
        <> observed_at
        <> "\",\n"
        <> "  \"github\": {\n"
        <> "    \"pulls\": [\n"
        <> "      {\"number\": 201, \"html_url\": \"https://github.com/scherzo-systems/scherzo/pull/201\", \"draft\": true, \"base\": {\"repo\": {\"full_name\": \"scherzo-systems/scherzo\"}, \"ref\": \"main\"}, \"head\": {\"repo\": {\"full_name\": \"scherzo-systems/scherzo\"}, \"ref\": \"draft\"}},\n"
        <> "      {\"number\": 202, \"html_url\": \"https://github.com/scherzo-systems/scherzo/pull/202\", \"draft\": false, \"base\": {\"repo\": {\"full_name\": \"scherzo-systems/scherzo\"}, \"ref\": \"main\"}, \"head\": {\"repo\": {\"full_name\": \"someone/fork\"}, \"ref\": \"fork\"}},\n"
        <> "      {\"number\": 203, \"html_url\": \"https://github.com/scherzo-systems/scherzo/pull/203\", \"draft\": false, \"base\": {\"repo\": {\"full_name\": \"scherzo-systems/scherzo\"}, \"ref\": \"main\"}, \"head\": {\"repo\": {\"full_name\": \"bromanko/other\"}, \"ref\": \"cross\"}},\n"
        <> "      {\"number\": 204, \"html_url\": \"https://github.com/scherzo-systems/scherzo/pull/204\", \"draft\": false, \"base\": {\"repo\": {\"full_name\": \"scherzo-systems/scherzo\"}, \"ref\": \"main\"}, \"head\": {\"repo\": null, \"ref\": \"deleted\"}}\n"
        <> "    ]\n"
        <> "  },\n"
        <> "  \"linear\": {\"fail_if_called\": true}\n"
        <> "}\n",
    )

  let artifact = run_scout("scan-fixture " <> fixture <> " --json-summary")

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(artifact.stdout, "draft")
  assert string.contains(artifact.stdout, "cross_repository")
  assert string.contains(artifact.stdout, "deleted_head_repo")
  assert string.contains(artifact.stdout, "\"created\": []")
  assert string.contains(artifact.stdout, "\"updated\": []")
  assert artifact.stderr == ""
}

pub fn scout_inconclusive_metadata_uses_preflight_test() {
  let fixture =
    write_fixture(
      "test/tmp/github-pr-conflict-scout-preflight-conflict",
      "{\n"
        <> "  \"observed_at\": \""
        <> observed_at
        <> "\",\n"
        <> "  \"github\": {\n"
        <> "    \"pulls\": ["
        <> safe_pr_json(123, "feature/conflicted-change")
        <> "],\n"
        <> "    \"details\": {\"123\": {\"mergeable\": false, \"mergeable_state\": \"blocked\"}}\n"
        <> "  },\n"
        <> "  \"preflight\": {\"123\": \"conflicted\"},\n"
        <> "  \"linear\": "
        <> linear_project_with_issues("[]")
        <> "\n"
        <> "}\n",
    )

  let artifact = run_scout("scan-fixture " <> fixture <> " --json-summary")

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(artifact.stdout, "\"created\": [")
  assert string.contains(artifact.stdout, "local-preflight")
  assert artifact.stderr == ""
}

pub fn scout_preflight_unavailable_skips_without_linear_test() {
  let fixture =
    write_fixture(
      "test/tmp/github-pr-conflict-scout-preflight-unavailable",
      "{\n"
        <> "  \"observed_at\": \""
        <> observed_at
        <> "\",\n"
        <> "  \"github\": {\n"
        <> "    \"pulls\": ["
        <> safe_pr_json(123, "feature/conflicted-change")
        <> "],\n"
        <> "    \"details\": {\"123\": {\"mergeable\": false, \"mergeable_state\": \"blocked\"}}\n"
        <> "  },\n"
        <> "  \"preflight\": {\"123\": \"unavailable\"},\n"
        <> "  \"linear\": {\"fail_if_called\": true}\n"
        <> "}\n",
    )

  let artifact = run_scout("scan-fixture " <> fixture <> " --json-summary")

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(artifact.stdout, "preflight_unavailable")
  assert string.contains(artifact.stdout, "\"created\": []")
  assert string.contains(artifact.stdout, "\"updated\": []")
  assert artifact.stderr == ""
}

pub fn scout_malformed_github_payload_fails_test() {
  let fixture =
    write_fixture(
      "test/tmp/github-pr-conflict-scout-malformed",
      "{\n"
        <> "  \"github\": {\n"
        <> "    \"pulls\": [{\"number\": \"123\", \"html_url\": \"https://github.com/scherzo-systems/scherzo/pull/123\", \"draft\": false, \"base\": {\"repo\": {\"full_name\": \"scherzo-systems/scherzo\"}, \"ref\": \"main\"}, \"head\": {\"repo\": {\"full_name\": \"scherzo-systems/scherzo\"}, \"ref\": \"feature\"}}]\n"
        <> "  },\n"
        <> "  \"linear\": {\"fail_if_called\": true}\n"
        <> "}\n",
    )

  let artifact = run_scout("scan-fixture " <> fixture <> " --json-summary")

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(1)
  assert string.contains(artifact.stderr, "malformed GitHub PR payload")
}
