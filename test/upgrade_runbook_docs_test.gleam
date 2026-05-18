import support/docs_assert.{assert_contains, assert_contains_all, read_file}

pub fn upgrade_runbook_documents_breaking_change_policy_test() {
  let path = "docs/runbooks/upgrades.md"
  let runbook = read_file(path)

  assert_contains_all(path, runbook, [
    "# Breaking-change upgrade policy",
    "fail fast",
    "stable diagnostic codes",
    "`doctor`",
    "`archive-old`",
    "`discard-old`",
    "`reinitialize`",
    "workspace.hooks",
    "workspace.profiles.<name>.hooks",
    "workspace.profiles.<name>.driver",
    "legacy_workspace_hooks",
    "tracker.api_key",
    "tracker.credentials.api_key_env",
    "legacy_tracker_field_ignored",
    "old_state_reset_required",
    "scherzoctl state status",
    "Do not build a generic migration framework",
    "Do not add silent automatic compatibility behavior",
  ])
}

pub fn upgrade_runbook_is_linked_from_contributor_docs_test() {
  let readme_path = "README.md"
  let architecture_path = "docs/ARCHITECTURE.md"
  let getting_started_path = "docs/GETTING_STARTED.md"
  let runbook_path = "docs/runbooks/upgrades.md"
  let getting_started = read_file(getting_started_path)

  assert_contains(
    readme_path,
    read_file(readme_path),
    "docs/runbooks/upgrades.md",
  )
  assert_contains(
    architecture_path,
    read_file(architecture_path),
    "runbooks/upgrades.md",
  )
  assert_contains(
    architecture_path,
    read_file(architecture_path),
    "detect old shapes",
  )
  assert_contains(
    getting_started_path,
    getting_started,
    "| Legacy or unsupported shape | Config, workflow, driver, tracker, or local state uses an old shape | Read the diagnostic path/code and the [upgrade policy](runbooks/upgrades.md) or linked specific runbook |",
  )
  assert_contains(
    getting_started_path,
    getting_started,
    "| Upgrade or breaking-change diagnostic | `scherzo doctor .scherzo/scherzo.yaml` or `scherzoctl state status --root <workspace-root>` | Follow the [upgrade policy](runbooks/upgrades.md) and any specific runbook named by the diagnostic |",
  )
  assert_contains(
    runbook_path,
    read_file(runbook_path),
    "workspace-driver-migration.md",
  )
  assert_contains(runbook_path, read_file(runbook_path), "tracker-adapters.md")
  assert_contains(runbook_path, read_file(runbook_path), "workflow-recovery.md")
}
