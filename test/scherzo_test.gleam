import gleam/io
import gleam/list
import gleam/result
import gleam/string

// gleeunit.main runs every *_test function under test/. Scherzo keeps
// explicit non-unit suites under test/ so they compile with the project, but
// filters them out of the default unit run unless a suite is requested.
const contract_prefix = "contract/"

const local_integration_prefix = "local_integration/"

const real_pi_validation_prefix = "real_pi_validation/"

pub fn main() -> Nil {
  case args() {
    [] -> run_suite(Unit)
    ["unit"] | ["--suite", "unit"] -> run_suite(Unit)
    ["contract"] | ["--suite", "contract"] -> run_suite(Contract)
    ["local-integration"] | ["--suite", "local-integration"] ->
      run_suite(LocalIntegration)
    ["real-pi-validation"] | ["--suite", "real-pi-validation"] ->
      run_suite(RealPiValidation)
    ["all"] | ["--suite", "all"] -> run_suite(All)
    ["help"] | ["--help"] -> {
      io.println(test_usage())
      halt(0)
    }
    _ -> {
      io.println_error(test_usage())
      halt(2)
    }
  }
}

type Suite {
  Unit
  Contract
  LocalIntegration
  RealPiValidation
  All
}

fn run_suite(suite: Suite) -> Nil {
  let options = [
    Verbose,
    NoTty,
    Report(#(GleeunitProgress, [Colored(True)])),
    ScaleTimeouts(10),
  ]
  let files =
    find_files(matching: "**/*.{erl,gleam}", in: "test")
    |> list.filter(file_belongs_to_suite(_, suite))

  case files {
    [] -> {
      io.println_error(
        "No test files matched suite " <> suite_name(suite) <> ".",
      )
      halt(1)
    }
    _ -> {
      let result =
        files
        |> list.map(gleam_to_erlang_module_name)
        |> list.map(dangerously_convert_string_to_atom(_, Utf8))
        |> run_eunit(options)

      case result {
        Ok(_) -> halt(0)
        Error(_) -> halt(1)
      }
    }
  }
}

fn file_belongs_to_suite(path: String, suite: Suite) -> Bool {
  case suite {
    Unit -> is_unit_file(path)
    Contract -> is_contract_file(path)
    LocalIntegration -> string.starts_with(path, local_integration_prefix)
    RealPiValidation -> string.starts_with(path, real_pi_validation_prefix)
    All -> True
  }
}

pub fn is_unit_file(path: String) -> Bool {
  !is_contract_file(path)
  && !string.starts_with(path, local_integration_prefix)
  && !string.starts_with(path, real_pi_validation_prefix)
}

pub fn is_contract_file(path: String) -> Bool {
  string.starts_with(path, contract_prefix)
  || list.contains(contract_test_files(), path)
}

pub fn contract_test_files() -> List(String) {
  [
    "agent_helper_script_test.gleam",
    "command_step_test.gleam",
    "control_server_test.gleam",
    "execplan_implementation_helper_test.gleam",
    "execplan_html_renderer_test.gleam",
    "execplan_v2_bundle_test.gleam",
    "github_pr_conflict_scout_test.gleam",
    "jj_origin_sync_test.gleam",
    "jj_workspace_driver_test.gleam",
    "jj_workspace_hook_test.gleam",
    "linear_cli_wrapper_test.gleam",
    "merge_conflict_helper_test.gleam",
    "orchestrator_daemon_control_test.gleam",
    "orchestrator_daemon_linear_command_test.gleam",
    "orchestrator_daemon_retry_step_test.gleam",
    "orchestrator_daemon_session_event_test.gleam",
    "orchestrator_daemon_test.gleam",
    "orchestrator_service_doctor_test.gleam",
    "orchestrator_service_lifecycle_test.gleam",
    "orchestrator_service_test.gleam",
    "pi_client_test.gleam",
    "port_test.gleam",
    "portable_research_workflow_test.gleam",
    "review_artifacts_test.gleam",
    "review_lane_contract_test.gleam",
    "scherzoctl_wrapper_test.gleam",
    "structured_output_contract_command_test.gleam",
    "tracker_conformance_cli_test.gleam",
    "tracker_conformance_comments_pack_test.gleam",
    "tracker_conformance_fixture_probe_test.gleam",
    "tracker_conformance_handoff_pack_test.gleam",
    "tracker_conformance_optional_profile_test.gleam",
    "tracker_conformance_remote_commands_pack_test.gleam",
    "tracker_conformance_routing_metadata_pack_test.gleam",
    "tracker_conformance_scheduled_failures_pack_test.gleam",
    "tracker_conformance_state_transition_pack_test.gleam",
    "tracker_conformance_task_source_test.gleam",
    "workflow_portability_test.gleam",
    "workflow_run_test.gleam",
    "workspace_cleanup_helper_test.gleam",
    "workspace_driver_contract_test.gleam",
    "workspace_driver_discovery_test.gleam",
    "workspace_driver_lifecycle_test.gleam",
    "workspace_run_test.gleam",
  ]
}

fn test_usage() -> String {
  "Usage: gleam test [-- --suite unit|contract|local-integration|real-pi-validation|all]\n"
  <> "Default with no suite runs the deterministic unit suite. Contract runs shell-heavy script/workflow/daemon/process/driver coverage."
}

fn suite_name(suite: Suite) -> String {
  case suite {
    Unit -> "unit"
    Contract -> "contract"
    LocalIntegration -> "local-integration"
    RealPiValidation -> "real-pi-validation"
    All -> "all"
  }
}

fn gleam_to_erlang_module_name(path: String) -> String {
  case string.ends_with(path, ".gleam") {
    True ->
      path
      |> string.replace(".gleam", "")
      |> string.replace("/", "@")

    False ->
      path
      |> string.split("/")
      |> list.last
      |> result.unwrap(path)
      |> string.replace(".erl", "")
  }
}

@external(erlang, "scherzo_main_ffi", "args")
fn args() -> List(String)

@external(erlang, "erlang", "halt")
fn halt(code: Int) -> Nil

@external(erlang, "gleeunit_ffi", "find_files")
fn find_files(matching matching: String, in in_: String) -> List(String)

type Atom

type Encoding {
  Utf8
}

@external(erlang, "erlang", "binary_to_atom")
fn dangerously_convert_string_to_atom(value: String, encoding: Encoding) -> Atom

type ReportModuleName {
  GleeunitProgress
}

type GleeunitProgressOption {
  Colored(Bool)
}

type EunitOption {
  Verbose
  NoTty
  Report(#(ReportModuleName, List(GleeunitProgressOption)))
  ScaleTimeouts(Int)
}

@external(erlang, "gleeunit_ffi", "run_eunit")
fn run_eunit(modules: List(Atom), options: List(EunitOption)) -> Result(Nil, a)
