import gleam/int
import gleam/io
import scherzo/control/remote_harness

pub fn help_text() -> String {
  "Usage: scherzo-ui-control-harness demo --token <token> --transcript <path>\n       scherzo-ui-control-harness command-demo --token <token> --transcript <path>\n       scherzo-ui-control-harness query-demo --token <token> --transcript <path>\n\nDevelopment-only loopback harness for the remote control path. It binds 127.0.0.1 on an ephemeral port, runs the real outbound client, and writes a redacted transcript derived from live socket traffic."
}

pub fn main() -> Nil {
  case parse_args(args()) {
    Ok(Help) -> io.println(help_text())
    Ok(Demo(token, transcript_path)) ->
      print_report(
        remote_harness.run_demo(token, transcript_path),
        transcript_path,
      )
    Ok(CommandDemo(token, transcript_path)) ->
      print_report(
        remote_harness.run_command_demo(token, transcript_path),
        transcript_path,
      )
    Ok(QueryDemo(token, transcript_path)) ->
      print_report(
        remote_harness.run_query_demo(token, transcript_path),
        transcript_path,
      )
    Error(err) -> {
      io.println_error(err.message)
      io.println_error(help_text())
      halt(2)
    }
  }
}

fn print_report(
  result: Result(remote_harness.Report, remote_harness.HarnessError),
  transcript_path: String,
) -> Nil {
  case result {
    Ok(report) -> {
      io.println("REMOTE_HARNESS_RUN_NONCE=" <> report.run_nonce)
      io.println(
        "REMOTE_HARNESS_BOUND_PORT=" <> int.to_string(report.bound_port),
      )
      io.println("REMOTE_HARNESS_TRANSCRIPT=" <> transcript_path)
    }
    Error(err) -> {
      io.println_error(err.code <> ": " <> err.message)
      halt(1)
    }
  }
}

type CliAction {
  Help
  Demo(String, String)
  CommandDemo(String, String)
  QueryDemo(String, String)
}

type CliError {
  CliError(message: String)
}

fn parse_args(args: List(String)) -> Result(CliAction, CliError) {
  case args {
    [] -> Ok(Help)
    ["--help"] | ["-h"] -> Ok(Help)
    ["demo", "--token", token, "--transcript", transcript_path] ->
      Ok(Demo(token, transcript_path))
    ["demo", "--transcript", transcript_path, "--token", token] ->
      Ok(Demo(token, transcript_path))
    ["command-demo", "--token", token, "--transcript", transcript_path] ->
      Ok(CommandDemo(token, transcript_path))
    ["command-demo", "--transcript", transcript_path, "--token", token] ->
      Ok(CommandDemo(token, transcript_path))
    ["query-demo", "--token", token, "--transcript", transcript_path] ->
      Ok(QueryDemo(token, transcript_path))
    ["query-demo", "--transcript", transcript_path, "--token", token] ->
      Ok(QueryDemo(token, transcript_path))
    _ -> Error(CliError("invalid arguments"))
  }
}

@external(erlang, "scherzo_main_ffi", "args")
fn args() -> List(String)

@external(erlang, "scherzo_main_ffi", "halt")
fn halt(code: Int) -> Nil
