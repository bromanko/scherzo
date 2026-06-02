import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo_lint/high_signal/inventory
import scherzo_lint/high_signal/report
import simplifile

pub type Options {
  Options(path: String, output: Option(String), format: OutputFormat)
}

pub type OutputFormat {
  Markdown
}

pub type CliResult {
  Run(Options)
  Help
}

pub type RunError {
  UsageError(message: String)
  FileError(context: String, path: String, message: String)
  ParseFailure(path: String, message: String)
}

pub fn main() -> Nil {
  case run(args()) {
    Ok(Nil) -> Nil
    Error(UsageError(message)) -> {
      io.println_error(message <> "\n\n" <> usage())
      halt(2)
    }
    Error(error) -> {
      io.println_error(error_message(error))
      halt(1)
    }
  }
}

pub fn run(args: List(String)) -> Result(Nil, RunError) {
  case parse_args(args) {
    Ok(Help) -> {
      io.println(usage())
      Ok(Nil)
    }
    Ok(Run(options)) -> run_with_options(options)
    Error(error) -> Error(error)
  }
}

fn parse_args(args: List(String)) -> Result(CliResult, RunError) {
  parse_args_loop(args, Options(path: "src", output: None, format: Markdown))
}

fn parse_args_loop(
  args: List(String),
  options: Options,
) -> Result(CliResult, RunError) {
  case args {
    [] -> Ok(Run(options))
    ["--help"] | ["-h"] -> Ok(Help)
    ["--path", path, ..rest] ->
      parse_args_loop(rest, Options(..options, path: path))
    ["--output", output, ..rest] ->
      parse_args_loop(rest, Options(..options, output: Some(output)))
    ["--format", "markdown", ..rest] ->
      parse_args_loop(rest, Options(..options, format: Markdown))
    ["--format", other, ..] ->
      Error(UsageError("unsupported format: " <> other))
    ["--path"] -> Error(UsageError("--path requires a value"))
    ["--output"] -> Error(UsageError("--output requires a value"))
    ["--format"] -> Error(UsageError("--format requires a value"))
    [unknown, ..] -> Error(UsageError("unknown argument: " <> unknown))
  }
}

fn run_with_options(options: Options) -> Result(Nil, RunError) {
  use files <- result.try(gleam_files(options.path))
  use findings <- result.try(analyze_files(files))
  let contents = case options.format {
    Markdown -> report.render_markdown(findings)
  }

  case options.output {
    None -> {
      io.println(contents)
      Ok(Nil)
    }
    Some(output) ->
      simplifile.write(output, contents: contents)
      |> result.map_error(fn(error) {
        FileError("write report", output, simplifile.describe_error(error))
      })
  }
}

fn gleam_files(path: String) -> Result(List(String), RunError) {
  simplifile.get_files(path)
  |> result.map_error(fn(error) {
    FileError("scan", path, simplifile.describe_error(error))
  })
  |> result.map(fn(files) {
    files
    |> list.filter(keeping: fn(file) { string.ends_with(file, ".gleam") })
    |> list.sort(by: string.compare)
  })
}

fn analyze_files(
  files: List(String),
) -> Result(List(inventory.Finding), RunError) {
  use findings, file <- list.try_fold(over: files, from: [])
  use source <- result.try(
    simplifile.read(file)
    |> result.map_error(fn(error) {
      FileError("read source", file, simplifile.describe_error(error))
    }),
  )

  use file_findings <- result.try(
    inventory.findings_for_source(path: file, source: source)
    |> result.map_error(fn(error) {
      case error {
        inventory.ParseError(path, message) -> ParseFailure(path, message)
      }
    }),
  )

  Ok(list.append(findings, file_findings))
}

fn usage() -> String {
  "Usage: gleam run -m scherzo_lint_high_signal_inventory -- [--path src] [--format markdown] [--output docs/lint/glinter-high-signal-baseline.md]\n\nScans production Gleam source for high-signal glinter warning inventory rules and writes subsystem/module counts. Findings are baseline data; the checked scherzo_lint command separately enforces the agent/Pi zero-new-warning guard."
}

fn error_message(error: RunError) -> String {
  case error {
    UsageError(message) -> message
    FileError(context, path, message) ->
      "failed to " <> context <> " `" <> path <> "`: " <> message
    ParseFailure(path, message) ->
      "failed to parse `" <> path <> "`: " <> message
  }
}

@external(erlang, "scherzo_main_ffi", "args")
fn args() -> List(String)

@external(erlang, "scherzo_main_ffi", "halt")
fn halt(code: Int) -> Nil
