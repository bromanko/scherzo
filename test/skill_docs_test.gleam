import gleam/string
import simplifile

const skill_path = ".pi/skills/scherzo-operator/SKILL.md"

const reference_path = ".pi/skills/scherzo-operator/references/commands.md"

fn read_file(path: String) -> String {
  let assert Ok(contents) = simplifile.read(path)
  contents
}

fn assert_contains(contents: String, expected: String) {
  assert string.contains(contents, expected)
}

fn assert_not_contains(contents: String, unexpected: String) {
  assert !string.contains(contents, unexpected)
}

pub fn scherzo_operator_skill_frontmatter_is_valid_test() {
  let skill = read_file(skill_path)

  assert string.starts_with(skill, "---\n")
  assert_contains(skill, "\nname: scherzo-operator\n")
  assert_contains(skill, "description:")
  assert_contains(skill, "\n---\n\n# Scherzo Operator")
  assert_contains(skill, "references/commands.md")
  assert_not_contains(skill, "TODO")
  assert_not_contains(skill, "TBD")
  assert_not_contains(skill, "[CLARIFY]")
}

pub fn scherzo_operator_skill_requires_safe_operating_rules_test() {
  let skill = read_file(skill_path)

  assert_contains(skill, "scripts/scherzoctl")
  assert_contains(skill, "--json")
  assert_contains(skill, "SCHERZO_CONTROL_FILE")
  assert_contains(skill, "--control-file")
  assert_contains(skill, "events --json")
  assert_contains(skill, "workflow-step-")
  assert_contains(skill, "top-level issue")
  assert_contains(skill, ".scherzo-keep-workspace")
  assert_contains(skill, "command-step-diagnostics")
  assert_contains(skill, "cleanup --json --dry-run")
  assert_contains(skill, "never reveal")
  assert_contains(skill, "token")
  assert_contains(skill, "LINEAR_API_KEY")
  assert_contains(skill, "SCHERZO_AGENT_LINEAR_API_KEY")
  assert_contains(skill, "direnv exec . linear issue view")
  assert_contains(skill, "direnv exec . linear issue comment list")
  assert_contains(skill, "raw GraphQL")
  assert_contains(skill, "pause")
  assert_contains(skill, "resume")
  assert_contains(skill, "reload")
  assert_contains(skill, "retry")
  assert_contains(skill, "park")
  assert_contains(skill, "unpark")
  assert_contains(skill, "abort")
  assert_contains(skill, "stop-after-turn")
  assert_contains(skill, "prompt")
  assert_contains(skill, "ui respond")
}

pub fn scherzo_operator_reference_matches_current_ctl_surface_test() {
  let reference = read_file(reference_path)

  assert_contains(reference, "scripts/scherzoctl ping --json")
  assert_contains(reference, "scripts/scherzoctl ps --json")
  assert_contains(reference, "scripts/scherzoctl session <session-id> --json")
  assert_contains(reference, "scripts/scherzoctl events <session-id> --json")
  assert_contains(
    reference,
    "scripts/scherzoctl attach --json --no-follow <session-id>",
  )
  assert_contains(reference, "scripts/scherzoctl pause --json")
  assert_contains(reference, "scripts/scherzoctl resume --json")
  assert_contains(reference, "scripts/scherzoctl reload --json")
  assert_contains(reference, "scripts/scherzoctl retry ABC-123 --json")
  assert_contains(reference, "scripts/scherzoctl park ABC-123 --reason")
  assert_contains(reference, "scripts/scherzoctl unpark ABC-123 --json")
  assert_contains(
    reference,
    "scripts/scherzoctl abort <session-id> --yes --json",
  )
  assert_contains(
    reference,
    "scripts/scherzoctl stop-after-turn <session-id> --yes --json",
  )
  assert_contains(
    reference,
    "scripts/scherzoctl prompt <session-id> \"summarize progress\" --json",
  )
  assert_contains(
    reference,
    "scripts/scherzoctl ui respond <session-id> <request-id> --cancel --json",
  )
  assert_contains(
    reference,
    "scripts/scherzoctl ui respond <session-id> <request-id> --value \"approved\" --json",
  )
  assert_contains(reference, "scripts/scherzoctl cleanup --json --dry-run")
  assert_contains(
    reference,
    "scripts/scherzoctl state status --root .scherzo/workspaces --json",
  )
  assert_contains(
    reference,
    "scripts/scherzoctl state archive-old --root .scherzo/workspaces --yes --json",
  )
  assert_not_contains(reference, "scripts/scherzoctl " <> "--control-file")
}

pub fn scherzo_operator_reference_documents_artifacts_and_linear_cli_test() {
  let reference = read_file(reference_path)

  assert_contains(reference, "workflow-step-")
  assert_contains(reference, ".scherzo-keep-workspace")
  assert_contains(reference, "command-step-diagnostics")
  assert_contains(reference, ".scherzo-state/artifacts/runs")
  assert_contains(reference, "scripts/scherzo-workspace-jj lifecycle remove")
  assert_contains(reference, "direnv exec . linear issue view LIV-104 --json")
  assert_contains(
    reference,
    "direnv exec . linear issue comment list LIV-104 --json",
  )
  assert_contains(reference, "direnv exec . linear issue comment add LIV-104")
  assert_contains(reference, "direnv exec . linear issue update LIV-104")
  assert_contains(reference, "direnv exec . linear api")
  assert_contains(reference, "curl https://api.linear.app/graphql")
  assert_contains(reference, "LINEAR_API_KEY")
  assert_contains(reference, "SCHERZO_AGENT_LINEAR_API_KEY")
  assert_contains(reference, "linear issue get")
  assert_contains(reference, "linear comment list/add")
  assert_contains(reference, "top-level `linear comment")
}

pub fn scherzo_operator_reference_explains_response_statuses_test() {
  let reference = read_file(reference_path)

  assert_contains(reference, "applied")
  assert_contains(reference, "queued")
  assert_contains(reference, "rejected")
  assert_contains(reference, "not_found")
  assert_contains(reference, "not_allowed")
  assert_contains(reference, "ok: true")
  assert_contains(reference, "ok: false")
}

pub fn readme_documents_pi_operator_skill_test() {
  let readme = read_file("README.md")

  assert_contains(readme, "Using pi as an operator UI")
  assert_contains(readme, "/skill:scherzo-operator")
  assert_contains(readme, "pi --skill .pi/skills/scherzo-operator")
  assert_contains(readme, "SCHERZO_CONTROL_FILE")
  assert_contains(readme, "scripts/scherzoctl ps --json")
  assert_contains(readme, "read-only summaries first")
  assert_contains(readme, "exact issue ids")
}
