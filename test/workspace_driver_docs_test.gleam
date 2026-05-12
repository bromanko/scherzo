import gleam/string
import simplifile

fn read_file(path: String) -> String {
  case simplifile.read(path) {
    Ok(contents) -> contents
    Error(_) -> {
      let message = path <> " could not be read"
      panic as message
    }
  }
}

fn assert_contains(path: String, contents: String, expected: String) -> Nil {
  case string.contains(contents, expected) {
    True -> Nil
    False -> {
      let message = path <> " is missing expected text: " <> expected
      panic as message
    }
  }
}

fn assert_not_contains(
  path: String,
  contents: String,
  unexpected: String,
) -> Nil {
  case string.contains(contents, unexpected) {
    False -> Nil
    True -> {
      let message = path <> " still contains unexpected text: " <> unexpected
      panic as message
    }
  }
}

fn assert_no_local_absolute_path_prefixes(
  path: String,
  contents: String,
) -> Nil {
  assert_not_contains(path, contents, "/Users/")
  assert_not_contains(path, contents, "/home/")
  assert_not_contains(path, contents, "C:\\Users\\")
}

pub fn readme_documents_workspace_driver_model_test() {
  let path = "README.md"
  let readme = read_file(path)

  assert_contains(path, readme, "Workspace profiles and drivers")
  assert_contains(path, readme, "workspace.profiles.<name>.driver.command")
  assert_contains(path, readme, "workspace_capabilities")
  assert_contains(path, readme, "SCHERZO_WORKSPACE_DRIVER")
  assert_contains(path, readme, "SCHERZO_WORKSPACE_CAPABILITIES")
  assert_contains(path, readme, "legacy workspace.hooks")
  assert_contains(path, readme, "docs/runbooks/workspace-driver-migration.md")
  assert_contains(path, readme, "docs/specs/WORKSPACE_DRIVER_SPEC.md")
  assert_contains(path, readme, "command: scherzo-workspace-noop")
}

pub fn workspace_driver_spec_is_normative_contract_test() {
  let path = "docs/specs/WORKSPACE_DRIVER_SPEC.md"
  let spec = read_file(path)

  assert_contains(path, spec, "RFC 2119")
  assert_contains(path, spec, "workspace.profiles.<name>.driver.command")
  assert_contains(path, spec, "<driver> describe --json")
  assert_contains(path, spec, "<driver> lifecycle create")
  assert_contains(path, spec, "<driver> changed-files --json")
  assert_contains(path, spec, "<driver> publish-change")
  assert_contains(path, spec, "Exit code")
  assert_contains(path, spec, "scripts/scherzo-workspace-noop")
  assert_contains(path, spec, "scripts/scherzo-workspace-jj")
}

pub fn old_workspace_driver_contract_points_to_spec_test() {
  let path = "docs/runbooks/workspace-driver-contract.md"
  let contract = read_file(path)

  assert_contains(path, contract, "docs/specs/WORKSPACE_DRIVER_SPEC.md")
  assert_contains(path, contract, "contract now lives")
}

pub fn migration_guide_is_actionable_test() {
  let path = "docs/runbooks/workspace-driver-migration.md"
  let guide = read_file(path)

  assert_contains(path, guide, "Before")
  assert_contains(path, guide, "After")
  assert_contains(path, guide, "workspace.hooks")
  assert_contains(path, guide, "driver:")
  assert_contains(path, guide, "describe --json")
  assert_contains(path, guide, "docs/specs/WORKSPACE_DRIVER_SPEC.md")
  assert_contains(
    path,
    guide,
    "direnv exec . gleam run -- doctor --check workflow-config",
  )
  assert_contains(path, guide, "direnv exec . gleam test")
  assert_contains(path, guide, "Rollback")
  assert_contains(path, guide, "Troubleshooting")
}

pub fn examples_use_driver_profiles_test() {
  let path = "examples/scherzo.yaml"
  let example = read_file(path)

  assert_contains(path, example, "driver:")
  assert_contains(path, example, "command: ../scripts/scherzo-workspace-jj")
  assert_contains(path, example, "command: ../scripts/scherzo-workspace-noop")
  assert_contains(path, example, "lifecycle:")
  assert_not_contains(path, example, "capabilities:")
  assert_not_contains(path, example, "    isolated:\n      hooks:")
  assert_not_contains(path, example, "    noop:\n      hooks:")
  assert_no_local_absolute_path_prefixes(path, example)
}

pub fn packaged_noop_example_uses_installed_command_test() {
  let path = "examples/scherzo-packaged-noop.yaml"
  let example = read_file(path)

  assert_contains(path, example, "default_profile: noop")
  assert_contains(path, example, "command: scherzo-workspace-noop")
  assert_contains(
    path,
    example,
    "lifecycle: [create, before-step, after-step, remove]",
  )
  assert_contains(path, example, "research: workflows/research.yaml")
  assert_not_contains(path, example, "../scripts/scherzo-workspace-noop")
  assert_not_contains(path, example, "scripts/scherzo-workspace-noop")
  assert_no_local_absolute_path_prefixes(path, example)
}

pub fn packaged_and_source_tree_noop_docs_are_distinct_test() {
  let migration_path = "docs/runbooks/workspace-driver-migration.md"
  let migration = read_file(migration_path)
  let portable_path = "docs/runbooks/portable-research-workflow.md"
  let portable = read_file(portable_path)
  let capabilities_path = "docs/runbooks/workspace-driver-capabilities.md"
  let capabilities = read_file(capabilities_path)

  assert_contains(migration_path, migration, "command: scherzo-workspace-noop")
  assert_contains(
    migration_path,
    migration,
    "command: ../scripts/scherzo-workspace-noop",
  )
  assert_contains(
    migration_path,
    migration,
    "examples/scherzo-packaged-noop.yaml",
  )
  assert_contains(portable_path, portable, "command: scherzo-workspace-noop")
  assert_contains(portable_path, portable, "../scripts/scherzo-workspace-noop")
  assert_contains(
    capabilities_path,
    capabilities,
    "command: scherzo-workspace-noop",
  )
  assert_contains(
    capabilities_path,
    capabilities,
    "result/bin/scherzo-workspace-noop describe --json",
  )
  assert_no_local_absolute_path_prefixes(migration_path, migration)
  assert_no_local_absolute_path_prefixes(portable_path, portable)
  assert_no_local_absolute_path_prefixes(capabilities_path, capabilities)
}

pub fn research_workflow_resolves_relative_driver_test() {
  let path = "examples/workflows/research.yaml"
  let workflow = read_file(path)

  assert_contains(path, workflow, "driver_command=${SCHERZO_WORKSPACE_DRIVER")
  assert_contains(path, workflow, "SCHERZO_CONFIG_DIR")
  assert_contains(
    path,
    workflow,
    "\"$driver\" assert-only --path \"$findings\"",
  )
  assert_not_contains(
    path,
    workflow,
    "\"$SCHERZO_WORKSPACE_DRIVER\" assert-only",
  )
}

pub fn driver_scripts_are_present_test() {
  let assert Ok(True) = simplifile.is_file("scripts/scherzo-workspace-jj")
  let assert Ok(True) = simplifile.is_file("scripts/scherzo-workspace-noop")
}

pub fn dogfood_readme_documents_driver_profile_test() {
  let path = ".scherzo/README.md"
  let readme = read_file(path)

  assert_contains(path, readme, "workspace.profiles")
  assert_contains(path, readme, "workspace driver")
  assert_contains(path, readme, "scripts/scherzo-workspace-jj")
  assert_contains(path, readme, "doctor --check workflow-config")
}

pub fn architecture_uses_driver_vocabulary_test() {
  let path = "docs/ARCHITECTURE.md"
  let architecture = read_file(path)

  assert_contains(path, architecture, "workspace driver")
  assert_contains(path, architecture, "workspace capability")
  assert_not_contains(path, architecture, "workspace hook profiles")
}
