import gleam/int
import gleam/list
import gleam/result
import gleam/string
import scherzo_lint/high_signal/inventory.{type Finding}
import simplifile

pub const guard_name = "scherzo_lint_agent_pi_high_signal_zero"

pub type GuardError {
  FileError(context: String, path: String, message: String)
  ParseFailure(path: String, message: String)
  BaselineExceeded(findings: List(Finding))
}

pub fn run() -> Result(Nil, GuardError) {
  use files <- result.try(guarded_files())
  use findings <- result.try(analyze_files(files))

  case findings {
    [] -> Ok(Nil)
    _ -> Error(BaselineExceeded(inventory.sort_findings(findings)))
  }
}

pub fn error_message(error: GuardError) -> String {
  case error {
    FileError(context, path, message) ->
      "failed to " <> context <> " `" <> path <> "`: " <> message
    ParseFailure(path, message) ->
      "failed to parse `" <> path <> "`: " <> message
    BaselineExceeded(findings) -> baseline_exceeded_message(findings)
  }
}

fn guarded_files() -> Result(List(String), GuardError) {
  use agent_files <- result.try(gleam_files("src/scherzo/agent"))
  use pi_files <- result.try(gleam_files("src/scherzo/pi"))

  Ok(list.append(agent_files, pi_files) |> list.sort(by: string.compare))
}

fn gleam_files(path: String) -> Result(List(String), GuardError) {
  simplifile.get_files(path)
  |> result.map_error(fn(error) {
    FileError("scan", path, simplifile.describe_error(error))
  })
  |> result.map(fn(files) {
    files
    |> list.filter(keeping: fn(file) { string.ends_with(file, ".gleam") })
  })
}

fn analyze_files(files: List(String)) -> Result(List(Finding), GuardError) {
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

fn baseline_exceeded_message(findings: List(Finding)) -> String {
  guard_name
  <> " failed: agent/Pi source must stay at zero tracked high-signal glinter warnings.\n\n"
  <> "This strict ratchet covers `src/scherzo/agent/` and `src/scherzo/pi/` for `discarded_result`, `error_context_lost`, `stringly_typed_error`, `thrown_away_error`, and selected `unwrap_used` findings.\n\n"
  <> "New findings:\n"
  <> render_findings(findings)
  <> "\nResolve agent/Pi findings by fixing the code. If a warning is an intentional exception, add a narrow `// nolint: <rule> -- reason` directly above the target. Regenerate `docs/lint/glinter-high-signal-baseline.md` only for intentional non-agent/Pi inventory changes; it does not relax this strict guard."
}

fn render_findings(findings: List(Finding)) -> String {
  findings
  |> list.map(fn(finding) {
    "- `"
    <> finding.path
    <> ":"
    <> int.to_string(finding.line)
    <> "` "
    <> finding.rule
    <> ": "
    <> finding.message
  })
  |> string.join("\n")
}
