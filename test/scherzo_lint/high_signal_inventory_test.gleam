import gleam/string
import scherzo_lint/high_signal/agent_pi_guard
import scherzo_lint/high_signal/inventory
import scherzo_lint/high_signal/report
import scherzo_lint_high_signal_inventory as inventory_cli
import simplifile

fn findings_for(source: String) -> List(inventory.Finding) {
  let assert Ok(findings) =
    inventory.findings_for_source(
      path: "src/scherzo/agent/example.gleam",
      source: source,
    )

  findings
}

fn assert_subsystem_module(
  path path: String,
  subsystem subsystem: String,
  module_name module_name: String,
) {
  assert inventory.subsystem_for_path(path) == subsystem
  assert inventory.module_name_for_path(path) == module_name
}

pub fn stringly_typed_error_is_reported_with_subsystem_and_module_test() {
  let assert [finding] =
    findings_for(
      "pub fn load() -> Result(Nil, String) {
  Ok(Nil)
}",
    )

  assert finding.rule == inventory.stringly_typed_error_rule
  assert finding.subsystem == "Agent / pi execution"
  assert finding.module_name == "scherzo.agent.example"
  assert finding.line == 1
  assert string.contains(does: finding.message, contain: "String as error type")
}

pub fn nolint_suppresses_tracked_rule_in_inventory_test() {
  assert findings_for(
      "// nolint: stringly_typed_error -- fixture keeps a compact string error
pub fn load() -> Result(Nil, String) {
  Ok(Nil)
}",
    )
    == []
}

pub fn subsystem_map_assigns_guarded_agent_and_pi_paths_test() {
  assert inventory.is_agent_pi_path("src/scherzo/agent/runner.gleam")
  assert inventory.is_agent_pi_path("src/scherzo/pi/client.gleam")
  assert !inventory.is_agent_pi_path("src/scherzo/workflow_run.gleam")
}

pub fn subsystem_map_assigns_each_report_bucket_and_fallback_test() {
  assert_subsystem_module(
    path: "src/scherzo/pi/client.gleam",
    subsystem: "Agent / pi execution",
    module_name: "scherzo.pi.client",
  )
  assert_subsystem_module(
    path: "src/scherzo/orchestrator/daemon.gleam",
    subsystem: "Orchestrator / daemon / transition / effect runner",
    module_name: "scherzo.orchestrator.daemon",
  )
  assert_subsystem_module(
    path: "src/scherzo/workflow_repair.gleam",
    subsystem: "Workflow execution",
    module_name: "scherzo.workflow_repair",
  )
  assert_subsystem_module(
    path: "src/scherzo/state/ledger.gleam",
    subsystem: "State ledger / projection / recovery / artifacts",
    module_name: "scherzo.state.ledger",
  )
  assert_subsystem_module(
    path: "src/scherzo_linear_conformance_live_driver.gleam",
    subsystem: "Tracker / Linear / control boundaries",
    module_name: "scherzo_linear_conformance_live_driver",
  )
  assert_subsystem_module(
    path: "src/scherzo/config/types.gleam",
    subsystem: "Config / parsing / operator CLI",
    module_name: "scherzo.config.types",
  )
  assert_subsystem_module(
    path: "src/scherzo/workspace_manifest.gleam",
    subsystem: "Workspace / workspace drivers",
    module_name: "scherzo.workspace_manifest",
  )
  assert_subsystem_module(
    path: "src/scherzo/artifact_publication_executor.gleam",
    subsystem: "Artifact publication / repository",
    module_name: "scherzo.artifact_publication_executor",
  )
  assert_subsystem_module(
    path: "src/scherzo/workstream/start_manual.gleam",
    subsystem: "Workstream",
    module_name: "scherzo.workstream.start_manual",
  )
  assert_subsystem_module(
    path: "src/scherzo/daemon_identity.gleam",
    subsystem: "Top-level utilities / other",
    module_name: "scherzo.daemon_identity",
  )
}

pub fn report_includes_subsystem_and_module_matrices_test() {
  let assert [finding] =
    findings_for(
      "pub fn load() -> Result(Nil, String) {
  Ok(Nil)
}",
    )
  let markdown = report.render_markdown([finding])

  assert string.contains(does: markdown, contain: "## Counts by subsystem")
  assert string.contains(
    does: markdown,
    contain: "| Agent / pi execution | 1 | 0 | 0 | 1 | 0 | 0 |",
  )
  assert string.contains(does: markdown, contain: "## Counts by module")
  assert string.contains(
    does: markdown,
    contain: "| Agent / pi execution | `scherzo.agent.example` | 1 | 0 | 0 | 1 | 0 | 0 |",
  )
}

pub fn high_signal_inventory_cli_help_does_not_scan_source_test() {
  assert inventory_cli.run(["--help"]) == Ok(Nil)
}

pub fn high_signal_inventory_cli_rejects_unknown_argument_test() {
  assert inventory_cli.run(["--unknown"])
    == Error(inventory_cli.UsageError("unknown argument: --unknown"))
}

pub fn high_signal_inventory_cli_writes_markdown_report_test() {
  let root = "test/tmp/scherzo-lint-high-signal-cli-success"
  let source_dir = root <> "/src/scherzo/agent"
  let source_path = source_dir <> "/fixture.gleam"
  let output_path = root <> "/baseline.md"
  let _ = simplifile.delete(root)
  let assert Ok(Nil) = simplifile.create_directory_all(source_dir)
  let assert Ok(Nil) =
    simplifile.write(
      source_path,
      "pub fn load() -> Result(Nil, String) {\n  Ok(Nil)\n}",
    )

  assert inventory_cli.run(["--path", root <> "/src", "--output", output_path])
    == Ok(Nil)
  let assert Ok(markdown) = simplifile.read(output_path)
  assert string.contains(does: markdown, contain: "Total tracked findings: 1")
  assert string.contains(does: markdown, contain: "## Counts by module")
  let _ = simplifile.delete(root)
}

pub fn high_signal_inventory_cli_reports_parse_failure_test() {
  let root = "test/tmp/scherzo-lint-high-signal-cli-parse"
  let source_path = root <> "/broken.gleam"
  let _ = simplifile.delete(root)
  let assert Ok(Nil) = simplifile.create_directory_all(root)
  let assert Ok(Nil) = simplifile.write(source_path, "pub fn broken(")

  let assert Error(inventory_cli.ParseFailure(path, message)) =
    inventory_cli.run(["--path", root])
  assert path == source_path
  assert string.contains(does: message, contain: "unexpected")
  let _ = simplifile.delete(root)
}

pub fn agent_pi_guard_message_names_strict_ratchet_test() {
  let assert [finding] =
    findings_for(
      "pub fn load() -> Result(Nil, String) {
  Ok(Nil)
}",
    )
  let message =
    agent_pi_guard.error_message(agent_pi_guard.BaselineExceeded([finding]))

  assert string.contains(does: message, contain: agent_pi_guard.guard_name)
  assert string.contains(does: message, contain: "must stay at zero")
  assert string.contains(
    does: message,
    contain: "does not relax this strict guard",
  )
  assert string.contains(
    does: message,
    contain: "src/scherzo/agent/example.gleam:1",
  )
}

pub fn subsystem_module_count_rows_aggregate_multiple_rules_test() {
  let assert Ok(findings) =
    inventory.findings_for_source(
      path: "src/scherzo/workflow_repair.gleam",
      source: "pub fn load() -> Result(Nil, String) {
  let value = result.unwrap(Ok(1), 0)
  Ok(Nil)
}",
    )

  let assert [module_row] = inventory.module_count_rows(findings)
  assert module_row.subsystem == "Workflow execution"
  assert module_row.module_name == "scherzo.workflow_repair"
  assert module_row.counts.total == 2
  assert module_row.counts.stringly_typed_error == 1
  assert module_row.counts.unwrap_used == 1
}
