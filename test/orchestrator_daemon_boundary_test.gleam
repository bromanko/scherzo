import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import simplifile

const daemon_path = "src/scherzo/orchestrator/daemon.gleam"

const boundary_doc_path = "docs/architecture/daemon-boundary.md"

const source_guardrail_path = "test/source_guardrail_test.gleam"

const service_path = "src/scherzo/orchestrator/service.gleam"

const daemon_module = "scherzo/orchestrator/daemon"

const max_daemon_lines = 7319

type ShellException {
  ShellException(name: String)
}

type OwnerRule {
  OwnerRule(
    module_path: String,
    prefixes: List(String),
    exceptions: List(ShellException),
  )
}

type SourceImport {
  SourceImport(path: String, imported_module: String, line: Int)
}

pub fn daemon_boundary_doc_matches_checked_in_policy_test() {
  let contents = read_required_file(boundary_doc_path)
  let missing = missing_doc_fragments(contents)

  case missing {
    [] -> Nil
    _ -> io.println_error(missing_doc_report(missing))
  }

  assert missing == []
}

pub fn daemon_boundary_line_ratchet_matches_current_tree_test() {
  let line_total = daemon_path |> read_required_file |> line_count
  let failure = line_count_sync_failure(line_total)

  case failure {
    None -> Nil
    Some(message) -> io.println_error(message)
  }

  assert failure == None
}

pub fn daemon_boundary_line_ratchet_sources_stay_in_sync_test() {
  let doc_contents = read_required_file(boundary_doc_path)
  let source_guardrail_contents = read_required_file(source_guardrail_path)

  assert string.contains(doc_contents, documented_line_ratchet_fragment())
  assert string.contains(
    source_guardrail_contents,
    source_guardrail_line_ratchet_fragment(),
  )
}

pub fn daemon_boundary_line_ratchet_flags_synthetic_growth_test() {
  let assert Some(failure) = line_count_failure(max_daemon_lines + 1)
  assert string.contains(failure, daemon_path)
  assert string.contains(failure, int.to_string(max_daemon_lines + 1))
  assert string.contains(failure, int.to_string(max_daemon_lines))
}

pub fn daemon_boundary_prefix_guardrail_matches_current_tree_test() {
  let names = daemon_path |> read_required_file |> top_level_function_names
  let failures = prefix_failures_for(daemon_path, names, owner_rules())

  case failures {
    [] -> Nil
    _ -> io.println_error(prefix_failure_report(failures))
  }

  assert failures == []
}

pub fn daemon_boundary_prefix_guardrail_flags_synthetic_regressions_test() {
  let failures =
    prefix_failures_for(
      daemon_path,
      [
        "scheduled_new_helper",
        "recovered_new_helper",
        "worker_new_helper",
        "yaml_new_helper",
        "operator_new_helper",
        "start_remote_client_shadow",
      ],
      owner_rules(),
    )

  let report = string.join(failures, with: "\n")
  assert list.length(failures) == 6
  assert string.contains(report, "scheduled_new_helper")
  assert string.contains(report, "recovered_new_helper")
  assert string.contains(report, "worker_new_helper")
  assert string.contains(report, "yaml_new_helper")
  assert string.contains(report, "operator_new_helper")
  assert string.contains(report, "start_remote_client_shadow")
}

pub fn daemon_boundary_service_startup_edge_exception_test() {
  let service_imports = scan_source_file(service_path)
  assert list.any(service_imports, fn(source_import) {
    source_import.imported_module == daemon_module
  })
  assert daemon_import_failures_for(service_imports, [service_path]) == []
}

pub fn daemon_boundary_extracted_modules_do_not_import_daemon_test() {
  let failures =
    extracted_module_paths()
    |> list.flat_map(scan_source_file)
    |> daemon_import_failures_for([service_path])

  case failures {
    [] -> Nil
    _ -> io.println_error(import_failure_report(failures))
  }

  assert failures == []
}

pub fn daemon_boundary_import_guardrail_flags_synthetic_regressions_test() {
  let failures =
    daemon_import_failures_for(
      [
        SourceImport(
          path: "src/scherzo/orchestrator/scheduled_runtime.gleam",
          imported_module: daemon_module,
          line: 12,
        ),
      ],
      [service_path],
    )

  let assert [failure] = failures
  assert string.contains(failure, "scheduled_runtime.gleam:12")
  assert string.contains(failure, daemon_module)
}

fn missing_doc_fragments(contents: String) -> List(String) {
  list.append(
    missing_global_doc_fragments(contents),
    missing_owner_doc_fragments(contents),
  )
}

fn missing_global_doc_fragments(contents: String) -> List(String) {
  global_doc_fragments()
  |> list.filter(fn(fragment) { !string.contains(contents, fragment) })
}

fn global_doc_fragments() -> List(String) {
  [
    "public actor startup",
    "public message receipt",
    "compatibility types",
    "dependency injection",
    "control-plane/process/timer edges",
    "top-level logging/redaction context",
    "handoff between subsystem outcomes",
    documented_line_ratchet_fragment(),
    "src/scherzo/orchestrator/service.gleam",
    daemon_module,
  ]
}

fn missing_owner_doc_fragments(contents: String) -> List(String) {
  owner_rules()
  |> list.flat_map(fn(rule) { missing_owner_doc_fragments_for(contents, rule) })
}

fn missing_owner_doc_fragments_for(
  contents: String,
  rule: OwnerRule,
) -> List(String) {
  case owner_section(contents, rule.module_path) {
    None -> ["owner section: " <> rule.module_path]
    Some(section) ->
      list.append(
        missing_prefix_fragments(section, rule),
        missing_exception_bullets(section, rule),
      )
  }
}

fn missing_prefix_fragments(section: String, rule: OwnerRule) -> List(String) {
  rule.prefixes
  |> list.filter(fn(prefix) {
    !string.contains(section, prefix_fragment(prefix))
  })
  |> list.map(fn(prefix) {
    rule.module_path <> " forbidden prefix " <> prefix_fragment(prefix)
  })
}

fn missing_exception_bullets(section: String, rule: OwnerRule) -> List(String) {
  rule.exceptions
  |> list.filter(fn(exception) {
    !exception_bullet_present(section, exception.name)
  })
  |> list.map(fn(exception) {
    rule.module_path <> " exception bullet " <> exception.name
  })
}

fn owner_section(contents: String, module_path: String) -> Option(String) {
  let header = "### `" <> module_path <> "`"

  case string.split_once(contents, on: header) {
    Error(_) -> None
    Ok(#(_, after_header)) -> Some(section_before_next_heading(after_header))
  }
}

fn section_before_next_heading(contents: String) -> String {
  case string.split_once(contents, on: "\n### ") {
    Ok(#(section, _)) -> section
    Error(_) ->
      case string.split_once(contents, on: "\n## ") {
        Ok(#(section, _)) -> section
        Error(_) -> contents
      }
  }
}

fn prefix_fragment(prefix: String) -> String {
  "`" <> prefix <> "`"
}

fn exception_bullet_present(section: String, exception_name: String) -> Bool {
  let marker = "- `" <> exception_name <> "`:"
  case string.split_once(section, on: marker) {
    Error(_) -> False
    Ok(#(_, after_marker)) -> exception_rationale_present(after_marker)
  }
}

fn exception_rationale_present(after_marker: String) -> Bool {
  let rationale =
    after_marker
    |> string.split(on: "\n")
    |> first
    |> option_map(string.trim)

  case rationale {
    None -> False
    Some(text) -> text != ""
  }
}

fn documented_line_ratchet_fragment() -> String {
  "max_daemon_lines: " <> int.to_string(max_daemon_lines)
}

fn source_guardrail_line_ratchet_fragment() -> String {
  "SourceLimit(\""
  <> daemon_path
  <> "\", "
  <> int.to_string(max_daemon_lines)
  <> ","
}

fn read_required_file(path: String) -> String {
  let assert Ok(contents) = simplifile.read(path)
  contents
}

fn line_count_sync_failure(actual_lines: Int) -> Option(String) {
  case actual_lines == max_daemon_lines {
    True -> None
    False ->
      Some(
        daemon_path
        <> " has "
        <> int.to_string(actual_lines)
        <> " lines, but the daemon-boundary ratchet is "
        <> int.to_string(max_daemon_lines)
        <> "; lower max_daemon_lines whenever the daemon shrinks",
      )
  }
}

fn line_count_failure(actual_lines: Int) -> Option(String) {
  case actual_lines > max_daemon_lines {
    True ->
      Some(
        daemon_path
        <> " grew beyond its daemon-boundary ratchet: "
        <> int.to_string(actual_lines)
        <> " > "
        <> int.to_string(max_daemon_lines),
      )
    False -> None
  }
}

fn top_level_function_names(contents: String) -> List(String) {
  contents
  |> string.split(on: "\n")
  |> list.fold([], fn(names, line) {
    case top_level_function_name(line) {
      Some(name) -> [name, ..names]
      None -> names
    }
  })
  |> list.reverse
}

fn top_level_function_name(line: String) -> Option(String) {
  let trimmed = string.trim(line)

  let declaration = case string.starts_with(trimmed, "pub fn ") {
    True -> Some(string.drop_start(trimmed, string.length("pub fn ")))
    False ->
      case string.starts_with(trimmed, "fn ") {
        True -> Some(string.drop_start(trimmed, string.length("fn ")))
        False -> None
      }
  }

  case declaration {
    None -> None
    Some(signature) ->
      signature
      |> string.split(on: "(")
      |> first
      |> option_map(string.trim)
  }
}

fn prefix_failures_for(
  path: String,
  names: List(String),
  rules: List(OwnerRule),
) -> List(String) {
  rules
  |> list.flat_map(fn(rule) { prefix_failures_for_rule(path, names, rule) })
}

fn prefix_failures_for_rule(
  path: String,
  names: List(String),
  rule: OwnerRule,
) -> List(String) {
  names
  |> list.filter(fn(name) { matches_forbidden_prefix(name, rule) })
  |> list.map(fn(name) {
    path
    <> " reintroduced extracted prefix via "
    <> name
    <> "; owner module: "
    <> rule.module_path
    <> "; allowed prefixes: "
    <> string.join(rule.prefixes, with: ", ")
  })
}

fn matches_forbidden_prefix(name: String, rule: OwnerRule) -> Bool {
  list.any(rule.prefixes, fn(prefix) { string.starts_with(name, prefix) })
  && !list.any(rule.exceptions, fn(exception) { exception.name == name })
}

fn extracted_module_paths() -> List(String) {
  owner_rules() |> list.map(fn(rule) { rule.module_path })
}

fn scan_source_file(path: String) -> List(SourceImport) {
  imports_from_contents(path, read_required_file(path))
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

fn daemon_import_failures_for(
  imports: List(SourceImport),
  allowlisted_paths: List(String),
) -> List(String) {
  imports
  |> list.filter(fn(source_import) {
    source_import.imported_module == daemon_module
    && !list.contains(allowlisted_paths, source_import.path)
  })
  |> list.map(fn(source_import) {
    source_import.path
    <> ":"
    <> int.to_string(source_import.line)
    <> " imports "
    <> source_import.imported_module
    <> " but extracted orchestrator modules must not depend on the daemon actor"
  })
}

fn owner_rules() -> List(OwnerRule) {
  [
    OwnerRule(
      module_path: "src/scherzo/orchestrator/scheduled_runtime.gleam",
      prefixes: ["scheduled_"],
      exceptions: [
        ShellException(name: "scheduled_failure_paths"),
        ShellException(name: "scheduled_job_by_id"),
        ShellException(name: "scheduled_worker_down_context"),
        ShellException(name: "scheduled_worker_active_for_job"),
        ShellException(name: "scheduled_slot_available_for_start"),
        ShellException(name: "scheduled_worker_spawn_context"),
        ShellException(name: "scheduled_worker_finished_context"),
        ShellException(name: "scheduled_worker_success_context"),
        ShellException(name: "scheduled_worker_needs_human_context"),
        ShellException(name: "scheduled_worker_failure_context"),
        ShellException(name: "scheduled_worker_failure_follow_up"),
        ShellException(name: "scheduled_failure_ledger_append"),
        ShellException(name: "scheduled_failure_dedupe_key"),
        ShellException(name: "scheduled_failure_issue_id_for_state"),
      ],
    ),
    OwnerRule(
      module_path: "src/scherzo/orchestrator/startup_recovery.gleam",
      prefixes: ["recovered_"],
      exceptions: [
        ShellException(name: "recovered_contract_manifest"),
        ShellException(name: "recovered_workflow_identity_matches"),
        ShellException(name: "recovered_workspaces_to_prepared"),
      ],
    ),
    OwnerRule(
      module_path: "src/scherzo/orchestrator/worker_lifecycle.gleam",
      prefixes: ["worker_"],
      exceptions: [
        ShellException(name: "worker_issue_state_name"),
        ShellException(name: "worker_run_id_from_resolution"),
        ShellException(name: "worker_issue_state_name_from_resolution"),
        ShellException(name: "worker_for_session"),
        ShellException(name: "worker_spawn_context"),
        ShellException(name: "worker_command_ready_context"),
        ShellException(name: "worker_update_context"),
        ShellException(name: "worker_finished_context"),
        ShellException(name: "worker_down_context"),
      ],
    ),
    OwnerRule(
      module_path: "src/scherzo/orchestrator/yaml_workflow_lifecycle.gleam",
      prefixes: ["yaml_", "handle_yaml_", "log_yaml_"],
      exceptions: [
        ShellException(name: "handle_yaml_step_command_ready"),
        ShellException(name: "handle_yaml_step_started"),
        ShellException(name: "handle_yaml_step_finished"),
        ShellException(name: "yaml_child_recovery_info"),
        ShellException(name: "yaml_step_callbacks"),
        ShellException(name: "yaml_scheduled_workflow_dependencies"),
        ShellException(name: "yaml_workflow_dependencies"),
        ShellException(name: "yaml_worker_failure"),
        ShellException(name: "yaml_workflow_failure"),
        ShellException(name: "log_yaml_step_update"),
      ],
    ),
    OwnerRule(
      module_path: "src/scherzo/orchestrator/operator_runtime.gleam",
      prefixes: ["operator_", "parked_"],
      exceptions: [
        ShellException(name: "operator_command_reply"),
        ShellException(name: "operator_issue_resolution"),
        ShellException(name: "parked_issue_resolution"),
        ShellException(name: "parked_issue_id_for_ref"),
        ShellException(name: "parked_issue_id_for_identifier"),
      ],
    ),
    OwnerRule(
      module_path: "src/scherzo/orchestrator/remote_command_runtime.gleam",
      prefixes: ["start_remote_client", "restart_remote_client"],
      exceptions: [
        ShellException(name: "start_remote_client_now"),
        ShellException(name: "restart_remote_client_if_enabled"),
      ],
    ),
  ]
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

fn line_count(contents: String) -> Int {
  case contents == "" {
    True -> 0
    False -> {
      let split_line_count = contents |> string.split(on: "\n") |> list.length
      case string.ends_with(contents, "\n") {
        True -> split_line_count - 1
        False -> split_line_count
      }
    }
  }
}

fn missing_doc_report(missing: List(String)) -> String {
  [
    "Daemon boundary document is missing required fragments:",
    ..list.map(missing, fn(fragment) { "- " <> fragment })
  ]
  |> string.join(with: "\n")
}

fn prefix_failure_report(failures: List(String)) -> String {
  [
    "Daemon boundary prefix guardrail failed.",
    ..list.map(failures, fn(failure) { "- " <> failure })
  ]
  |> string.join(with: "\n")
}

fn import_failure_report(failures: List(String)) -> String {
  [
    "Daemon boundary import guardrail failed.",
    ..list.map(failures, fn(failure) { "- " <> failure })
  ]
  |> string.join(with: "\n")
}
