import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/result
import scherzo/path
import scherzo/port
import scherzo/task
import scherzo/tracker/conformance
import scherzo/tracker/conformance/process_capture
import scherzo/tracker/conformance/types
import simplifile

pub type FixtureError {
  FixtureError(code: String, message: String)
}

const setup_recovery_guidance = "Fix fixture setup or backend credentials before judging adapter conformance."

const cleanup_recovery_guidance = "Repair teardown and rerun because cleanup failures can contaminate later conformance results."

pub fn load_tasks(
  manifest: types.Manifest,
) -> Result(List(task.Task), FixtureError) {
  let types.Manifest(fixtures: fixtures, ..) = manifest
  let types.FixtureConfig(task_file: task_file, tasks: declarations) = fixtures
  use Nil <- result.try(validate_task_file_path(task_file))
  use contents <- result.try(case simplifile.read(task_file) {
    Ok(contents) -> Ok(contents)
    Error(_) ->
      Error(FixtureError(
        code: "fixture_read_failed",
        message: "could not read fixture task file: " <> task_file,
      ))
  })

  case conformance.decode_response(contents) {
    Ok(types.DriverResponseSuccess(
      result: types.TaskListResult(tasks: tasks),
      ..,
    )) -> {
      use Nil <- result.try(validate_task_inventory(tasks))
      use Nil <- result.try(validate_fixture_declarations(declarations, tasks))
      Ok(tasks)
    }
    _ ->
      Error(FixtureError(
        code: "fixture_decode_failed",
        message: "fixture task file must decode as a task-list driver success response",
      ))
  }
}

fn validate_task_inventory(
  tasks: List(task.Task),
) -> Result(Nil, FixtureError) {
  case tasks {
    [] ->
      Error(FixtureError(
        code: "fixture_task_file_empty",
        message: "fixtures.task_file must contain at least one task for task_source conformance",
      ))
    _ -> Ok(Nil)
  }
}

fn validate_fixture_declarations(
  declarations: List(types.FixtureTaskDeclaration),
  tasks: List(task.Task),
) -> Result(Nil, FixtureError) {
  case declarations {
    [] -> Ok(Nil)
    [types.FixtureTaskDeclaration(name: name, ref: ref, ..), ..rest] ->
      case task_ref_in_tasks(tasks, ref) {
        True -> validate_fixture_declarations(rest, tasks)
        False ->
          Error(FixtureError(
            code: "fixture_declaration_missing_task",
            message: "fixture declaration does not match any task in fixtures.task_file: "
              <> name,
          ))
      }
  }
}

fn task_ref_in_tasks(tasks: List(task.Task), target: task.TaskRef) -> Bool {
  case tasks {
    [] -> False
    [task.Task(ref: ref, ..), ..rest] ->
      same_ref(ref, target) || task_ref_in_tasks(rest, target)
  }
}

fn same_ref(left: task.TaskRef, right: task.TaskRef) -> Bool {
  let task.TaskRef(
    backend_kind: left_backend_kind,
    remote_id: left_remote_id,
    ..,
  ) = left
  let task.TaskRef(
    backend_kind: right_backend_kind,
    remote_id: right_remote_id,
    ..,
  ) = right
  left_backend_kind == right_backend_kind && left_remote_id == right_remote_id
}

pub fn run_setup(manifest: types.Manifest) -> Option(types.HookResult) {
  let types.Manifest(hooks: hooks, ..) = manifest
  let types.HooksConfig(setup: setup, ..) = hooks
  run_optional_hook(
    setup,
    "setup",
    types.SetupFailedStatus,
    setup_recovery_guidance,
  )
}

pub fn run_cleanup(manifest: types.Manifest) -> Option(types.HookResult) {
  let types.Manifest(hooks: hooks, ..) = manifest
  let types.HooksConfig(cleanup: cleanup, ..) = hooks
  run_optional_hook(
    cleanup,
    "cleanup",
    types.CleanupFailedStatus,
    cleanup_recovery_guidance,
  )
}

fn run_optional_hook(
  hook: Option(types.HookCommand),
  phase: String,
  failure_status: types.CaseStatus,
  recovery_guidance: String,
) -> Option(types.HookResult) {
  case hook {
    None -> None
    Some(command) ->
      Some(run_hook(command, phase, failure_status, recovery_guidance))
  }
}

fn run_hook(
  command: types.HookCommand,
  phase: String,
  failure_status: types.CaseStatus,
  recovery_guidance: String,
) -> types.HookResult {
  let types.HookCommand(executable: executable, args: args, cwd: cwd) = command
  case port.start_argv(executable, args, cwd, []) {
    Error(error) ->
      types.HookResult(
        phase: phase,
        status: failure_status,
        message: phase
          <> " hook spawn failed: "
          <> port.port_error_to_string(error),
        diagnostics: "",
        recovery_guidance: recovery_guidance,
      )
    Ok(process) ->
      case port.await_exit(process, 1000) {
        Ok(0) ->
          types.HookResult(
            phase: phase,
            status: types.PassedStatus,
            message: phase <> " hook passed",
            diagnostics: process_capture.truncate_diagnostics(
              diagnostics_or_empty(process),
            ),
            recovery_guidance: recovery_guidance,
          )
        Ok(status) ->
          types.HookResult(
            phase: phase,
            status: failure_status,
            message: phase
              <> " hook exited with status "
              <> int.to_string(status),
            diagnostics: process_capture.truncate_diagnostics(
              diagnostics_or_empty(process),
            ),
            recovery_guidance: recovery_guidance,
          )
        Error(error) -> {
          let diagnostics =
            diagnostics_or_empty(process) <> terminate_note(process)
          types.HookResult(
            phase: phase,
            status: failure_status,
            message: phase
              <> " hook failed: "
              <> port.port_error_to_string(error),
            diagnostics: process_capture.truncate_diagnostics(diagnostics),
            recovery_guidance: recovery_guidance,
          )
        }
      }
  }
}

pub fn ensure_report_directory(path_text: String) -> Result(Nil, FixtureError) {
  case path.dirname(path_text) {
    Error(_) ->
      Error(FixtureError(
        code: "report_directory_failed",
        message: "could not resolve report directory",
      ))
    Ok(dir) ->
      case simplifile.create_directory_all(dir) {
        Ok(Nil) -> Ok(Nil)
        Error(_) ->
          Error(FixtureError(
            code: "report_directory_failed",
            message: "could not create report directory: " <> dir,
          ))
      }
  }
}

fn validate_task_file_path(task_file: String) -> Result(Nil, FixtureError) {
  use repo_root <- result.try(case path.realpath(".") {
    Ok(repo_root) -> Ok(repo_root)
    Error(_) ->
      Error(FixtureError(
        code: "fixture_path_invalid",
        message: "could not resolve repository root for fixture task file validation",
      ))
  })
  use fixture_path <- result.try(case path.realpath(task_file) {
    Ok(fixture_path) -> Ok(fixture_path)
    Error(_) ->
      Error(FixtureError(
        code: "fixture_read_failed",
        message: "could not read fixture task file: " <> task_file,
      ))
  })
  case path.contains(repo_root, fixture_path) {
    True -> Ok(Nil)
    False ->
      Error(FixtureError(
        code: "fixture_path_invalid",
        message: "fixture task file must stay within the repository: "
          <> task_file,
      ))
  }
}

fn diagnostics_or_empty(process: port.Process) -> String {
  case port.read_diagnostics(process) {
    Ok(diagnostics) -> diagnostics
    Error(error) ->
      "diagnostics_unavailable: " <> port.port_error_to_string(error)
  }
}

fn terminate_note(process: port.Process) -> String {
  case port.terminate(process) {
    Ok(Nil) -> ""
    Error(error) -> " terminate_failed: " <> port.port_error_to_string(error)
  }
}
