import gleam/io
import scherzo/tracker/conformance/runner
import scherzo/tracker/conformance/types

pub type CliCommand {
  Run(manifest_path: String, report_path: String)
}

pub type CliError {
  UsageError
  RunError(code: String, message: String)
}

pub fn parse_args(args: List(String)) -> Result(CliCommand, CliError) {
  case args {
    [manifest_path, "--report", report_path] ->
      Ok(Run(manifest_path: manifest_path, report_path: report_path))
    _ -> Error(UsageError)
  }
}

pub fn usage() -> String {
  "Usage: scherzo __tracker-conformance-run <manifest.json> --report <report.json>"
}

pub fn run(args: List(String)) -> Result(types.RunResult, CliError) {
  case parse_args(args) {
    Ok(Run(manifest_path: manifest_path, report_path: report_path)) ->
      case runner.run_manifest_path(manifest_path, report_path) {
        Ok(result) -> Ok(result)
        Error(runner.RunError(code: code, message: message)) ->
          Error(RunError(code: code, message: message))
      }
    Error(error) -> Error(error)
  }
}

pub fn print_summary_or_error(
  result: Result(types.RunResult, CliError),
) -> Nil {
  case result {
    Ok(types.RunResult(summary: summary, ..)) -> io.println(summary)
    Error(UsageError) -> io.println_error(usage())
    Error(RunError(code: code, message: message)) ->
      io.println_error(
        "tracker-conformance failed code=" <> code <> " message=" <> message,
      )
  }
}

pub fn exit_code(result: Result(types.RunResult, CliError)) -> Int {
  case result {
    Ok(types.RunResult(exit_code: exit_code, ..)) -> exit_code
    Error(UsageError) -> 2
    Error(RunError(..)) -> 1
  }
}
