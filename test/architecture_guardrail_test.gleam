import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import simplifile

const source_root = "src"

const policy_doc = "docs/ARCHITECTURE.md#import-boundary-guardrails"

type SourceImport {
  SourceImport(path: String, imported_module: String, line: Int)
}

type BoundaryRule {
  StateMustNotImportOrchestrator
  TrackerMustNotImportLinear
  OrchestratorMustNotImportDaemon
  WorkflowCoreMustNotImportControlOrDaemon
  LegacyAdapterImportsRequireAllowlist
}

type AllowlistEntry {
  AllowlistEntry(
    path: String,
    imported_module: String,
    rule_id: String,
    reason: String,
  )
}

pub fn architectural_import_boundaries_match_policy_test() {
  let imports = scan_imports()
  let failures = boundary_failures(imports)

  case failures {
    [] -> Nil
    _ -> io.println_error(failure_report(failures))
  }

  assert failures == []
}

pub fn architecture_guardrail_flags_synthetic_forbidden_import_test() {
  let failures =
    boundary_failures_for(
      [
        SourceImport(
          path: "src/scherzo/state/new_projection.gleam",
          imported_module: "scherzo/orchestrator/daemon",
          line: 7,
        ),
      ],
      [],
    )

  let assert [failure] = failures
  assert string.contains(failure, "src/scherzo/state/new_projection.gleam:7")
  assert string.contains(failure, "scherzo/orchestrator/daemon")
  assert string.contains(failure, "state_must_not_import_orchestrator")
  assert string.contains(failure, "Remediation:")
}

pub fn architecture_guardrail_allows_explicit_exception_test() {
  let source_import =
    SourceImport(
      path: "src/scherzo/state/legacy_runtime_bridge.gleam",
      imported_module: "scherzo/orchestrator/state",
      line: 10,
    )

  let allowlist = [
    AllowlistEntry(
      path: "src/scherzo/state/legacy_runtime_bridge.gleam",
      imported_module: "scherzo/orchestrator/state",
      rule_id: "state_must_not_import_orchestrator",
      reason: "documented compatibility seam",
    ),
  ]

  assert boundary_failures_for([source_import], allowlist) == []
}

pub fn architecture_guardrail_has_no_state_orchestrator_allowlist_entries_test() {
  let state_orchestrator_entries =
    boundary_allowlist()
    |> list.filter(fn(entry) {
      entry.rule_id == "state_must_not_import_orchestrator"
    })

  assert state_orchestrator_entries == []
}

pub fn architecture_guardrail_flags_tracker_linear_import_test() {
  let failures =
    boundary_failures_for(
      [
        SourceImport(
          path: "src/scherzo/tracker/cache.gleam",
          imported_module: "scherzo/linear/client",
          line: 8,
        ),
      ],
      [],
    )

  let assert [failure] = failures
  assert string.contains(failure, "src/scherzo/tracker/cache.gleam:8")
  assert string.contains(failure, "scherzo/linear/client")
  assert string.contains(failure, "tracker_must_not_import_linear")
  assert string.contains(failure, "Remediation:")
}

pub fn architecture_guardrail_flags_orchestrator_daemon_import_test() {
  let failures =
    boundary_failures_for(
      [
        SourceImport(
          path: "src/scherzo/orchestrator/worker.gleam",
          imported_module: "scherzo/orchestrator/daemon",
          line: 12,
        ),
      ],
      [],
    )

  let assert [failure] = failures
  assert string.contains(failure, "src/scherzo/orchestrator/worker.gleam:12")
  assert string.contains(failure, "scherzo/orchestrator/daemon")
  assert string.contains(failure, "orchestrator_must_not_import_daemon")
  assert string.contains(failure, "Remediation:")
}

pub fn architecture_guardrail_flags_workflow_core_imports_test() {
  let failures =
    boundary_failures_for(
      [
        SourceImport(
          path: "src/scherzo/workflow_runner.gleam",
          imported_module: "scherzo/control/server",
          line: 11,
        ),
        SourceImport(
          path: "src/scherzo/workspace_run.gleam",
          imported_module: "scherzo/control/client",
          line: 12,
        ),
        SourceImport(
          path: "src/scherzo/command_step.gleam",
          imported_module: "scherzo/orchestrator/daemon",
          line: 13,
        ),
      ],
      [],
    )

  let failure_text = string.join(failures, with: "\n")
  assert list.length(failures) == 3
  assert string.contains(failure_text, "src/scherzo/workflow_runner.gleam:11")
  assert string.contains(failure_text, "scherzo/control/server")
  assert string.contains(failure_text, "src/scherzo/workspace_run.gleam:12")
  assert string.contains(failure_text, "scherzo/control/client")
  assert string.contains(failure_text, "src/scherzo/command_step.gleam:13")
  assert string.contains(failure_text, "scherzo/orchestrator/daemon")
  assert string.contains(
    failure_text,
    "workflow_core_must_not_import_control_or_daemon",
  )
  assert string.contains(failure_text, "Remediation:")
}

pub fn architecture_guardrail_requires_legacy_adapter_allowlist_test() {
  let failures =
    boundary_failures_for(
      [
        SourceImport(
          path: "src/scherzo/orchestrator/new_bridge.gleam",
          imported_module: "scherzo/tracker/adapter_legacy",
          line: 20,
        ),
      ],
      [],
    )

  let assert [failure] = failures
  assert string.contains(
    failure,
    "src/scherzo/orchestrator/new_bridge.gleam:20",
  )
  assert string.contains(failure, "scherzo/tracker/adapter_legacy")
  assert string.contains(failure, "legacy_adapter_imports_require_allowlist")
  assert string.contains(failure, "Remediation:")
}

pub fn architecture_guardrail_allows_legacy_adapter_exception_test() {
  let source_import =
    SourceImport(
      path: "src/scherzo/orchestrator/daemon.gleam",
      imported_module: "scherzo/tracker/adapter_legacy",
      line: 9,
    )

  let allowlist = [
    AllowlistEntry(
      path: "src/scherzo/orchestrator/daemon.gleam",
      imported_module: "scherzo/tracker/adapter_legacy",
      rule_id: "legacy_adapter_imports_require_allowlist",
      reason: "documented migration seam",
    ),
  ]

  assert boundary_failures_for([source_import], allowlist) == []
}

pub fn architecture_guardrail_reports_stale_allowlist_entries_test() {
  let allowlist = [
    AllowlistEntry(
      path: "src/scherzo/state/deleted_projection.gleam",
      imported_module: "scherzo/orchestrator/state",
      rule_id: "state_must_not_import_orchestrator",
      reason: "old compatibility seam",
    ),
  ]

  let failures = boundary_failures_for([], allowlist)

  let assert [failure] = failures
  assert string.contains(failure, "stale architecture allowlist entry")
  assert string.contains(
    failure,
    "src/scherzo/state/deleted_projection.gleam imports scherzo/orchestrator/state for state_must_not_import_orchestrator",
  )
}

pub fn architecture_guardrail_reports_blank_allowlist_rationale_test() {
  let source_import =
    SourceImport(
      path: "src/scherzo/state/projection.gleam",
      imported_module: "scherzo/orchestrator/state",
      line: 10,
    )

  let allowlist = [
    AllowlistEntry(
      path: "src/scherzo/state/projection.gleam",
      imported_module: "scherzo/orchestrator/state",
      rule_id: "state_must_not_import_orchestrator",
      reason: "   ",
    ),
  ]

  let failures = boundary_failures_for([source_import], allowlist)

  let assert [failure] = failures
  assert string.contains(
    failure,
    "architecture allowlist entry is missing a rationale",
  )
  assert string.contains(
    failure,
    "src/scherzo/state/projection.gleam imports scherzo/orchestrator/state for state_must_not_import_orchestrator",
  )
}

fn boundary_failures(imports: List(SourceImport)) -> List(String) {
  boundary_failures_for(imports, boundary_allowlist())
}

fn boundary_failures_for(
  imports: List(SourceImport),
  allowlist: List(AllowlistEntry),
) -> List(String) {
  list.append(
    violation_failures(imports, allowlist),
    list.append(
      stale_allowlist_failures(imports, allowlist),
      reason_failures(allowlist),
    ),
  )
}

fn violation_failures(
  imports: List(SourceImport),
  allowlist: List(AllowlistEntry),
) -> List(String) {
  imports
  |> list.flat_map(fn(source_import) {
    rules()
    |> list.filter_map(fn(rule) {
      case rule_violates(rule, source_import) {
        False -> Error(Nil)
        True ->
          case is_allowlisted(source_import, rule, allowlist) {
            True -> Error(Nil)
            False -> Ok(violation_message(source_import, rule))
          }
      }
    })
  })
}

fn stale_allowlist_failures(
  imports: List(SourceImport),
  allowlist: List(AllowlistEntry),
) -> List(String) {
  allowlist
  |> list.filter_map(fn(entry) {
    case allowlist_entry_matches_import(entry, imports) {
      True -> Error(Nil)
      False ->
        Ok("stale architecture allowlist entry: " <> allowlist_entry_key(entry))
    }
  })
}

fn reason_failures(allowlist: List(AllowlistEntry)) -> List(String) {
  allowlist
  |> list.filter_map(fn(entry) {
    case string.trim(entry.reason) == "" {
      False -> Error(Nil)
      True ->
        Ok(
          "architecture allowlist entry is missing a rationale: "
          <> allowlist_entry_key(entry),
        )
    }
  })
}

fn allowlist_entry_matches_import(
  entry: AllowlistEntry,
  imports: List(SourceImport),
) -> Bool {
  list.any(imports, fn(source_import) {
    source_import.path == entry.path
    && source_import.imported_module == entry.imported_module
    && list.any(rules(), fn(rule) {
      rule_id(rule) == entry.rule_id && rule_violates(rule, source_import)
    })
  })
}

fn is_allowlisted(
  source_import: SourceImport,
  rule: BoundaryRule,
  allowlist: List(AllowlistEntry),
) -> Bool {
  list.any(allowlist, fn(entry) {
    entry.path == source_import.path
    && entry.imported_module == source_import.imported_module
    && entry.rule_id == rule_id(rule)
  })
}

fn allowlist_entry_key(entry: AllowlistEntry) -> String {
  entry.path <> " imports " <> entry.imported_module <> " for " <> entry.rule_id
}

fn violation_message(
  source_import: SourceImport,
  rule: BoundaryRule,
) -> String {
  source_import.path
  <> ":"
  <> int.to_string(source_import.line)
  <> " imports "
  <> source_import.imported_module
  <> " but violates "
  <> rule_id(rule)
  <> " ("
  <> rule_summary(rule)
  <> "). Remediation: "
  <> rule_remediation(rule)
}

fn rules() -> List(BoundaryRule) {
  [
    StateMustNotImportOrchestrator,
    TrackerMustNotImportLinear,
    OrchestratorMustNotImportDaemon,
    WorkflowCoreMustNotImportControlOrDaemon,
    LegacyAdapterImportsRequireAllowlist,
  ]
}

fn rule_violates(rule: BoundaryRule, source_import: SourceImport) -> Bool {
  case rule {
    StateMustNotImportOrchestrator ->
      string.starts_with(source_import.path, "src/scherzo/state/")
      && string.starts_with(
        source_import.imported_module,
        "scherzo/orchestrator/",
      )

    TrackerMustNotImportLinear ->
      string.starts_with(source_import.path, "src/scherzo/tracker/")
      && string.starts_with(source_import.imported_module, "scherzo/linear")

    OrchestratorMustNotImportDaemon ->
      string.starts_with(source_import.path, "src/scherzo/orchestrator/")
      && source_import.path != "src/scherzo/orchestrator/daemon.gleam"
      && source_import.imported_module == "scherzo/orchestrator/daemon"

    WorkflowCoreMustNotImportControlOrDaemon ->
      is_workflow_core_source(source_import.path)
      && forbidden_workflow_core_import(source_import.imported_module)

    LegacyAdapterImportsRequireAllowlist ->
      source_import.imported_module == "scherzo/tracker/adapter_legacy"
  }
}

fn forbidden_workflow_core_import(imported_module: String) -> Bool {
  imported_module == "scherzo/orchestrator/daemon"
  || imported_module == "scherzo/control/server"
  || imported_module == "scherzo/control/client"
}

fn is_workflow_core_source(path: String) -> Bool {
  string.starts_with(path, "src/scherzo/workflow_")
  || path == "src/scherzo/command_step.gleam"
  || path == "src/scherzo/workspace_run.gleam"
  || path == "src/scherzo/step_artifact.gleam"
}

fn rule_id(rule: BoundaryRule) -> String {
  case rule {
    StateMustNotImportOrchestrator -> "state_must_not_import_orchestrator"
    TrackerMustNotImportLinear -> "tracker_must_not_import_linear"
    OrchestratorMustNotImportDaemon -> "orchestrator_must_not_import_daemon"
    WorkflowCoreMustNotImportControlOrDaemon ->
      "workflow_core_must_not_import_control_or_daemon"
    LegacyAdapterImportsRequireAllowlist ->
      "legacy_adapter_imports_require_allowlist"
  }
}

fn rule_summary(rule: BoundaryRule) -> String {
  case rule {
    StateMustNotImportOrchestrator ->
      "state modules must not depend on orchestrator modules"
    TrackerMustNotImportLinear ->
      "generic tracker modules must not depend on Linear implementation modules"
    OrchestratorMustNotImportDaemon ->
      "orchestrator subsystem modules must not import the daemon actor implementation"
    WorkflowCoreMustNotImportControlOrDaemon ->
      "workflow execution/core modules must not import control client/server implementations or the daemon"
    LegacyAdapterImportsRequireAllowlist ->
      "legacy tracker adapter compatibility imports require explicit approval"
  }
}

fn rule_remediation(rule: BoundaryRule) -> String {
  case rule {
    StateMustNotImportOrchestrator ->
      "Move shared durable-state types or pure helpers below state or into a neutral module; otherwise add a narrow allowlist entry with rationale."
    TrackerMustNotImportLinear ->
      "Keep generic tracker code behind tracker/adapter, or isolate Linear code in tracker/linear_adapter.gleam with an explicit allowlist entry."
    OrchestratorMustNotImportDaemon ->
      "Depend on orchestrator core, state, effects, or explicit command/result types instead; only service startup should launch the daemon through an allowlist."
    WorkflowCoreMustNotImportControlOrDaemon ->
      "Pass required capabilities through workflow dependencies or neutral command types; keep local control and daemon implementations at the orchestration edge."
    LegacyAdapterImportsRequireAllowlist ->
      "Use tracker/adapter capabilities directly, or add a narrow allowlist entry with migration rationale and remove it when the compatibility seam is gone."
  }
}

fn scan_imports() -> List(SourceImport) {
  source_files(source_root)
  |> list.sort(by: string.compare)
  |> list.flat_map(scan_source_file)
}

fn source_files(root: String) -> List(String) {
  let assert Ok(entries) = simplifile.read_directory(root)

  entries
  |> list.sort(by: string.compare)
  |> list.fold([], fn(paths, entry) {
    let full_path = root <> "/" <> entry
    let assert Ok(is_directory) = simplifile.is_directory(full_path)

    case is_directory {
      True -> list.append(source_files(full_path), paths)
      False ->
        case is_gleam_source_path(full_path) {
          True -> [full_path, ..paths]
          False -> paths
        }
    }
  })
}

fn is_gleam_source_path(path: String) -> Bool {
  string.ends_with(path, ".gleam")
}

fn scan_source_file(path: String) -> List(SourceImport) {
  let assert Ok(contents) = simplifile.read(path)
  imports_from_contents(path, contents)
}

fn imports_from_contents(path: String, contents: String) -> List(SourceImport) {
  let #(_, imports) =
    contents
    |> string.split(on: "\n")
    |> list.fold(#(1, []), fn(acc, line) {
      let #(line_number, imports) = acc
      case import_module(line) {
        None -> #(line_number + 1, imports)
        Some(imported_module) -> #(line_number + 1, [
          SourceImport(
            path: path,
            imported_module: imported_module,
            line: line_number,
          ),
          ..imports
        ])
      }
    })

  list.reverse(imports)
}

fn import_module(line: String) -> Option(String) {
  let trimmed = string.trim(line)
  case string.starts_with(trimmed, "import ") {
    False -> None
    True ->
      trimmed
      |> string.drop_start(string.length("import "))
      |> string.trim
      |> string.split(on: " ")
      |> first
      |> option_map(strip_import_members)
  }
}

fn strip_import_members(imported_module: String) -> String {
  case string.split_once(imported_module, on: ".{") {
    Ok(#(module_name, _)) -> module_name
    Error(_) -> imported_module
  }
}

fn first(values: List(a)) -> Option(a) {
  case values {
    [] -> None
    [first, ..] -> Some(first)
  }
}

fn option_map(value: Option(a), transform: fn(a) -> b) -> Option(b) {
  case value {
    None -> None
    Some(inner) -> Some(transform(inner))
  }
}

fn failure_report(failures: List(String)) -> String {
  [
    "Architectural import-boundary guardrail failed.",
    "",
    "Scherzo enforces dependency direction by scanning src/**/*.gleam imports during the deterministic test suite.",
    "Policy and allowlist process: " <> policy_doc,
    "",
    "Violations:",
  ]
  |> list.append(list.map(failures, fn(failure) { "- " <> failure }))
  |> string.join(with: "\n")
}

fn boundary_allowlist() -> List(AllowlistEntry) {
  [
    AllowlistEntry(
      path: "src/scherzo/tracker/linear_adapter.gleam",
      imported_module: "scherzo/linear",
      rule_id: "tracker_must_not_import_linear",
      reason: "This file is the Linear adapter boundary; generic tracker modules must not copy this dependency.",
    ),
    AllowlistEntry(
      path: "src/scherzo/tracker/linear_adapter.gleam",
      imported_module: "scherzo/linear/task_query",
      rule_id: "tracker_must_not_import_linear",
      reason: "Task query GraphQL helpers remain Linear-specific and are only consumed by the Linear adapter boundary.",
    ),
    AllowlistEntry(
      path: "src/scherzo/orchestrator/service.gleam",
      imported_module: "scherzo/orchestrator/daemon",
      rule_id: "orchestrator_must_not_import_daemon",
      reason: "Service startup is the process edge that launches the daemon actor; lower orchestrator subsystems should depend on core/effects/runtime state instead.",
    ),
    AllowlistEntry(
      path: "src/scherzo/orchestrator/daemon.gleam",
      imported_module: "scherzo/tracker/adapter_legacy",
      rule_id: "legacy_adapter_imports_require_allowlist",
      reason: "The daemon still bridges tracker adapters to legacy issue-shaped workflow runtime APIs during the adapter migration.",
    ),
  ]
}
