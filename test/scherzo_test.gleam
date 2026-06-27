import gleam/erlang/process
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import simplifile

// gleeunit.main runs every *_test function under test/. Scherzo keeps
// explicit non-unit suites under test/ so they compile with the project, but
// filters them out of the default unit run unless a suite is requested.
const contract_prefix = "contract/"

const local_integration_prefix = "local_integration/"

const real_pi_validation_prefix = "real_pi_validation/"

const shared_tmp_dir = "test/tmp"

const shared_tmp_lock_dir = "test/.tmp-suite-lock"

const shared_tmp_lock_owner = "test/.tmp-suite-lock/owner"

const shared_tmp_lock_wait_ms = 100

const shared_tmp_lock_attempts = 600

pub fn main() -> Nil {
  case args() {
    [] -> run_suite(Unit)
    ["unit"] | ["--suite", "unit"] -> run_suite(Unit)
    ["contract"] | ["--suite", "contract"] -> run_suite(Contract)
    ["contract-runtime"] | ["--suite", "contract-runtime"] ->
      run_suite(ContractRuntime)
    ["contract-orchestrator"] | ["--suite", "contract-orchestrator"] ->
      run_suite(ContractOrchestrator)
    ["contract-tracker"] | ["--suite", "contract-tracker"] ->
      run_suite(ContractTracker)
    ["contract-workflow"] | ["--suite", "contract-workflow"] ->
      run_suite(ContractWorkflow)
    ["contract-repository"] | ["--suite", "contract-repository"] ->
      run_suite(ContractRepository)
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
  ContractRuntime
  ContractOrchestrator
  ContractTracker
  ContractWorkflow
  ContractRepository
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
      let result = run_files_with_shared_tmp_guard(files, options, suite)

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
    ContractRuntime -> is_contract_runtime_file(path)
    ContractOrchestrator -> is_contract_orchestrator_file(path)
    ContractTracker -> is_contract_tracker_file(path)
    ContractWorkflow -> is_contract_workflow_file(path)
    ContractRepository -> is_contract_repository_file(path)
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

fn is_contract_runtime_file(path: String) -> Bool {
  string.starts_with(path, contract_prefix)
  || list.contains(contract_runtime_test_files(), path)
}

fn is_contract_orchestrator_file(path: String) -> Bool {
  list.contains(contract_orchestrator_test_files(), path)
}

fn is_contract_tracker_file(path: String) -> Bool {
  list.contains(contract_tracker_test_files(), path)
}

fn is_contract_workflow_file(path: String) -> Bool {
  list.contains(contract_workflow_test_files(), path)
}

fn is_contract_repository_file(path: String) -> Bool {
  list.contains(contract_repository_test_files(), path)
}

pub fn contract_test_files() -> List(String) {
  contract_runtime_test_files()
  |> list.append(contract_orchestrator_test_files())
  |> list.append(contract_tracker_test_files())
  |> list.append(contract_workflow_test_files())
  |> list.append(contract_repository_test_files())
}

pub fn contract_runtime_test_files() -> List(String) {
  [
    "agent_helper_script_test.gleam",
    "command_step_test.gleam",
    "control_server_test.gleam",
    "pi_client_test.gleam",
    "port_test.gleam",
    "scherzo_launcher_test.gleam",
    "scherzoctl_wrapper_test.gleam",
    "workspace_cleanup_helper_test.gleam",
    "workspace_driver_contract_test.gleam",
    "workspace_driver_discovery_test.gleam",
    "workspace_driver_lifecycle_test.gleam",
    "workspace_run_test.gleam",
  ]
}

pub fn contract_orchestrator_test_files() -> List(String) {
  [
    "orchestrator_daemon_control_test.gleam",
    "orchestrator_daemon_retry_step_test.gleam",
    "orchestrator_daemon_session_event_test.gleam",
    "orchestrator_daemon_test.gleam",
    "orchestrator_service_doctor_test.gleam",
    "orchestrator_service_lifecycle_test.gleam",
    "orchestrator_service_test.gleam",
  ]
}

pub fn contract_tracker_test_files() -> List(String) {
  [
    "linear_cli_wrapper_test.gleam",
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
  ]
}

pub fn contract_workflow_test_files() -> List(String) {
  [
    "portable_research_workflow_test.gleam",
    "structured_output_contract_command_test.gleam",
    "workflow_portability_test.gleam",
    "workflow_run_test.gleam",
  ]
}

pub fn contract_repository_test_files() -> List(String) {
  [
    "execplan_implementation_helper_test.gleam",
    "execplan_html_renderer_test.gleam",
    "execplan_v2_bundle_test.gleam",
    "github_pr_conflict_scout_test.gleam",
    "jj_origin_sync_test.gleam",
    "jj_workspace_driver_test.gleam",
    "jj_workspace_hook_test.gleam",
    "merge_conflict_helper_test.gleam",
    "review_artifacts_test.gleam",
    "review_lane_contract_test.gleam",
  ]
}

fn test_usage() -> String {
  "Usage: gleam test [-- --suite unit|contract|contract-runtime|contract-orchestrator|contract-tracker|contract-workflow|contract-repository|local-integration|real-pi-validation|all]\n"
  <> "Default with no suite runs the deterministic unit suite. Contract shards split shell-heavy coverage for CI timeouts."
}

fn suite_name(suite: Suite) -> String {
  case suite {
    Unit -> "unit"
    Contract -> "contract"
    ContractRuntime -> "contract-runtime"
    ContractOrchestrator -> "contract-orchestrator"
    ContractTracker -> "contract-tracker"
    ContractWorkflow -> "contract-workflow"
    ContractRepository -> "contract-repository"
    LocalIntegration -> "local-integration"
    RealPiValidation -> "real-pi-validation"
    All -> "all"
  }
}

fn run_files_with_shared_tmp_guard(
  files: List(String),
  options: List(EunitOption),
  suite: Suite,
) -> Result(Nil, a) {
  let suite = suite_name(suite)
  retry_acquire_shared_tmp_lock(suite, shared_tmp_lock_attempts)
  reset_shared_tmp_or_halt(suite)
  let result = run_files(files, options)
  release_shared_tmp_lock()
  result
}

fn run_files(
  files: List(String),
  options: List(EunitOption),
) -> Result(Nil, a) {
  files
  |> list.map(gleam_to_erlang_module_name)
  |> list.map(dangerously_convert_string_to_atom(_, Utf8))
  |> run_eunit(options)
}

fn retry_acquire_shared_tmp_lock(suite: String, attempts: Int) -> Nil {
  case simplifile.create_directory(shared_tmp_lock_dir) {
    Ok(Nil) -> {
      let _ = simplifile.write(shared_tmp_lock_owner, "suite=" <> suite <> "\n")
      Nil
    }
    Error(simplifile.Eexist) -> {
      case attempts <= 0 {
        True -> {
          io.println_error(
            "Timed out waiting for "
            <> shared_tmp_lock_dir
            <> "; another test suite may still be using "
            <> shared_tmp_dir
            <> ". Remove the lock directory only if no test suite is running.",
          )
          halt(1)
        }
        False -> {
          process.sleep(shared_tmp_lock_wait_ms)
          retry_acquire_shared_tmp_lock(suite, attempts - 1)
        }
      }
    }
    Error(error) -> {
      io.println_error(
        "Could not acquire "
        <> shared_tmp_lock_dir
        <> ": "
        <> simplifile.describe_error(error),
      )
      halt(1)
    }
  }
}

fn reset_shared_tmp_or_halt(suite: String) -> Nil {
  case simplifile.delete_all([shared_tmp_dir]) {
    Ok(Nil) ->
      case simplifile.create_directory_all(shared_tmp_dir) {
        Ok(Nil) -> Nil
        Error(error) -> {
          release_shared_tmp_lock()
          io.println_error(
            "Could not recreate "
            <> shared_tmp_dir
            <> " for suite "
            <> suite
            <> ": "
            <> simplifile.describe_error(error),
          )
          halt(1)
        }
      }
    Error(error) -> {
      release_shared_tmp_lock()
      io.println_error(
        "Could not reset "
        <> shared_tmp_dir
        <> " for suite "
        <> suite
        <> ": "
        <> simplifile.describe_error(error),
      )
      halt(1)
    }
  }
}

fn release_shared_tmp_lock() -> Nil {
  let _ = simplifile.delete_all([shared_tmp_lock_dir])
  Nil
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
