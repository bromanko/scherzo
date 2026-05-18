import simplifile
import support/docs_assert.{assert_contains, assert_not_contains, read_file}

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
  assert_contains(path, readme, "command: scherzo-workspace-jj")
  assert_contains(path, readme, "SCHERZO_JJ_WORKSPACE_PUBLISH_REMOTE")
  assert_contains(path, readme, "driver.env")
  assert_contains(path, readme, "not a secret store")
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
  assert_contains(path, spec, "driver.env")
  assert_contains(path, spec, "literal strings")
  assert_contains(path, spec, "PATH")
  assert_contains(path, spec, "not a secret store")
  assert_contains(path, spec, "limited redaction")
  assert_contains(path, spec, "value_sha256")
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
  assert_contains(path, guide, "driver.env")
  assert_contains(path, guide, "SCHERZO_JJ_WORKSPACE_BASE")
  assert_contains(path, guide, "wrapper")
  assert_contains(path, guide, "not a secret store")
}

pub fn examples_use_driver_profiles_test() {
  let path = "examples/scherzo.yaml"
  let example = read_file(path)

  assert_contains(path, example, "driver:")
  assert_contains(path, example, "command: ../scripts/scherzo-workspace-jj")
  assert_contains(path, example, "command: ../scripts/scherzo-workspace-noop")
  assert_contains(path, example, "SCHERZO_JJ_WORKSPACE_BASE")
  assert_contains(path, example, "SCHERZO_JJ_WORKSPACE_REMOTE")
  assert_contains(path, example, "SCHERZO_JJ_WORKSPACE_BASE_BRANCH")
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

pub fn packaged_jj_example_uses_installed_command_test() {
  let path = "examples/scherzo-packaged-jj.yaml"
  let example = read_file(path)

  assert_contains(path, example, "default_profile: isolated")
  assert_contains(path, example, "command: scherzo-workspace-jj")
  assert_contains(
    path,
    example,
    "lifecycle: [create, before-step, after-step, remove]",
  )
  assert_contains(path, example, "SCHERZO_JJ_WORKSPACE_BASE")
  assert_contains(path, example, "SCHERZO_JJ_WORKSPACE_FETCH_BASE")
  assert_contains(path, example, "SCHERZO_JJ_WORKSPACE_REMOTE")
  assert_contains(path, example, "SCHERZO_JJ_WORKSPACE_BASE_BRANCH")
  assert_contains(path, example, "SCHERZO_JJ_WORKSPACE_PUBLISH_REMOTE")
  assert_contains(
    path,
    example,
    "implementation: workflows/implementation.yaml",
  )
  assert_not_contains(path, example, "../scripts/scherzo-workspace-jj")
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
  assert_contains(migration_path, migration, "command: scherzo-workspace-jj")
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
  assert_contains(portable_path, portable, "command: scherzo-workspace-jj")
  assert_contains(portable_path, portable, "../scripts/scherzo-workspace-noop")
  assert_contains(
    capabilities_path,
    capabilities,
    "command: scherzo-workspace-noop",
  )
  assert_contains(
    capabilities_path,
    capabilities,
    "command: scherzo-workspace-jj",
  )
  assert_contains(
    capabilities_path,
    capabilities,
    "result/bin/scherzo-workspace-noop describe --json",
  )
  assert_contains(
    capabilities_path,
    capabilities,
    "result/bin/scherzo-workspace-jj describe --json",
  )
  assert_no_local_absolute_path_prefixes(migration_path, migration)
  assert_no_local_absolute_path_prefixes(portable_path, portable)
  assert_no_local_absolute_path_prefixes(capabilities_path, capabilities)
}

pub fn packaged_jj_docs_cover_base_fetch_and_publication_policy_test() {
  let readme_path = "README.md"
  let readme = read_file(readme_path)
  let migration_path = "docs/runbooks/workspace-driver-migration.md"
  let migration = read_file(migration_path)
  let capabilities_path = "docs/runbooks/workspace-driver-capabilities.md"
  let capabilities = read_file(capabilities_path)
  let docs = readme <> "\n" <> migration <> "\n" <> capabilities

  assert_contains(readme_path, docs, "SCHERZO_JJ_WORKSPACE_BASE")
  assert_contains(readme_path, docs, "SCHERZO_JJ_WORKSPACE_REMOTE")
  assert_contains(readme_path, docs, "SCHERZO_JJ_WORKSPACE_BASE_BRANCH")
  assert_contains(readme_path, docs, "SCHERZO_JJ_WORKSPACE_FETCH_BASE")
  assert_contains(readme_path, docs, "SCHERZO_JJ_WORKSPACE_PUBLISH_REMOTE")
  assert_contains(readme_path, docs, "SCHERZO_PR_REMOTE")
  assert_contains(readme_path, docs, "SCHERZO_PR_BASE")
  assert_contains(readme_path, docs, "SCHERZO_JJ_WORKSPACE_BASE=@")
  assert_contains(readme_path, docs, "trunk")
  assert_contains(readme_path, docs, "upstream")
  assert_contains(readme_path, docs, "origin")
  assert_contains(readme_path, docs, "publish-change")
  assert_contains(readme_path, docs, "requires `gh`")
  assert_contains(readme_path, docs, "trunk@upstream")
  assert_contains(readme_path, docs, "publishes through `origin`")
  assert_contains(readme_path, docs, "fork remote")
  assert_no_local_absolute_path_prefixes(readme_path, docs)
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
