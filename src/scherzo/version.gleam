import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/path
import scherzo/port

pub const unknown = "unknown"

const source_revision_env = "SCHERZO_SOURCE_REVISION"

const source_date_env = "SCHERZO_SOURCE_DATE"

const source_dirty_env = "SCHERZO_SOURCE_DIRTY"

const vcs_timeout_ms = 1000

pub type SourceIdentity {
  SourceIdentity(revision: String, date: String, dirty: Option(Bool))
}

pub type VcsQuery {
  JjRevision
  JjDate
  JjDirty
  GitRevision
  GitDate
  GitDirty
}

pub fn string() -> String {
  detect() |> format
}

pub fn detect() -> SourceIdentity {
  detect_with(path.env, run_vcs_query)
}

pub fn detect_with(
  env: fn(String) -> Option(String),
  run_vcs: fn(VcsQuery) -> Result(String, Nil),
) -> SourceIdentity {
  case identity_from_env(env) {
    Some(identity) -> identity
    None ->
      case identity_from_vcs(run_vcs, JjRevision, JjDate, JjDirty) {
        Some(identity) -> identity
        None ->
          case identity_from_vcs(run_vcs, GitRevision, GitDate, GitDirty) {
            Some(identity) -> identity
            None -> fallback()
          }
      }
  }
}

pub fn format(identity: SourceIdentity) -> String {
  "scherzo revision="
  <> field_or_unknown(identity.revision)
  <> " date="
  <> field_or_unknown(identity.date)
  <> " dirty="
  <> format_dirty(identity.dirty)
}

pub fn fallback() -> SourceIdentity {
  SourceIdentity(revision: unknown, date: unknown, dirty: None)
}

fn identity_from_env(
  env: fn(String) -> Option(String),
) -> Option(SourceIdentity) {
  case env(source_revision_env) {
    Some(revision) ->
      Some(SourceIdentity(
        revision: field_or_unknown(revision),
        date: env_field_or_unknown(env, source_date_env),
        dirty: env(source_dirty_env) |> option.then(parse_dirty),
      ))
    None -> None
  }
}

fn identity_from_vcs(
  run_vcs: fn(VcsQuery) -> Result(String, Nil),
  revision_query: VcsQuery,
  date_query: VcsQuery,
  dirty_query: VcsQuery,
) -> Option(SourceIdentity) {
  case run_vcs(revision_query) {
    Ok(raw_revision) -> {
      let revision = field_or_unknown(raw_revision)
      case revision == unknown {
        True -> None
        False ->
          Some(SourceIdentity(
            revision: revision,
            date: vcs_field_or_unknown(run_vcs, date_query),
            dirty: vcs_dirty(run_vcs, dirty_query),
          ))
      }
    }
    Error(_) -> None
  }
}

fn env_field_or_unknown(
  env: fn(String) -> Option(String),
  name: String,
) -> String {
  case env(name) {
    Some(value) -> field_or_unknown(value)
    None -> unknown
  }
}

fn vcs_field_or_unknown(
  run_vcs: fn(VcsQuery) -> Result(String, Nil),
  query: VcsQuery,
) -> String {
  case run_vcs(query) {
    Ok(value) -> field_or_unknown(value)
    Error(_) -> unknown
  }
}

fn vcs_dirty(
  run_vcs: fn(VcsQuery) -> Result(String, Nil),
  query: VcsQuery,
) -> Option(Bool) {
  case run_vcs(query) {
    Ok(value) -> Some(string.trim(value) != "")
    Error(_) -> None
  }
}

fn parse_dirty(value: String) -> Option(Bool) {
  case value |> string.trim |> string.lowercase {
    "true" | "1" | "yes" | "dirty" -> Some(True)
    "false" | "0" | "no" | "clean" -> Some(False)
    _ -> None
  }
}

fn field_or_unknown(value: String) -> String {
  let value = value |> string.trim |> one_line
  case value == "" {
    True -> unknown
    False -> value
  }
}

fn one_line(value: String) -> String {
  value
  |> string.replace(each: "\n", with: "_")
  |> string.replace(each: "\r", with: "_")
  |> string.replace(each: "\t", with: "_")
  |> string.replace(each: " ", with: "_")
}

fn format_dirty(dirty: Option(Bool)) -> String {
  case dirty {
    Some(True) -> "true"
    Some(False) -> "false"
    None -> unknown
  }
}

fn run_vcs_query(query: VcsQuery) -> Result(String, Nil) {
  let #(executable, args) = command(query)
  run_argv(executable, args)
}

fn command(query: VcsQuery) -> #(String, List(String)) {
  case query {
    JjRevision -> #("jj", [
      "log",
      "-r",
      "@",
      "--no-graph",
      "--color=never",
      "-T",
      "commit_id.short(12)",
    ])
    JjDate -> #("jj", [
      "log",
      "-r",
      "@",
      "--no-graph",
      "--color=never",
      "-T",
      "committer.timestamp().format(\"%Y-%m-%d\") ++ \"\\n\"",
    ])
    JjDirty -> #("jj", ["diff", "--summary"])
    GitRevision -> #("git", ["rev-parse", "--short=12", "HEAD"])
    GitDate -> #("git", ["show", "-s", "--format=%cs", "HEAD"])
    GitDirty -> #("git", ["status", "--porcelain", "--untracked-files=no"])
  }
}

fn run_argv(executable: String, args: List(String)) -> Result(String, Nil) {
  case port.start_argv(executable, args, ".", []) {
    Ok(process) -> read_first_stdout(process)
    Error(_) -> Error(Nil)
  }
}

fn read_first_stdout(process: port.Process) -> Result(String, Nil) {
  case port.read_stdout_line(process, vcs_timeout_ms) {
    Ok(line) -> {
      let _ = port.terminate(process)
      Ok(string.trim(line))
    }
    Error(port.ProcessExited(0)) -> Ok("")
    Error(_) -> {
      let _ = port.terminate(process)
      Error(Nil)
    }
  }
}
