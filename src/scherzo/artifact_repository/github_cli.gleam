import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/artifact_publication_manifest
import scherzo/artifact_publication_planner
import scherzo/artifact_repository/command_runner
import scherzo/artifact_repository/types

pub fn ensure_pull_request(
  manifest: artifact_publication_planner.DryRunPublicationManifest,
  checkout_dir: String,
  runner: command_runner.Runner,
) -> Result(Option(String), artifact_publication_manifest.PublicationErrorInfo) {
  case manifest.pull_request.enabled {
    False -> Ok(None)
    True -> {
      let title = default_pr_title(manifest)
      let body = default_pr_body(manifest)
      case lookup_open_pr(manifest, checkout_dir, runner) {
        Error(error) -> Error(error)
        Ok([]) -> create_pr(manifest, checkout_dir, runner, title, body)
        Ok([pr]) -> edit_pr(checkout_dir, runner, pr, title, body)
        Ok(_) ->
          Error(artifact_publication_manifest.PublicationErrorInfo(
            code: "pr_ambiguous",
            message: "multiple open pull requests matched publication branch",
          ))
      }
    }
  }
}

pub fn existing_pr_url(
  manifest: artifact_publication_planner.DryRunPublicationManifest,
  checkout_dir: String,
  runner: command_runner.Runner,
) -> Option(String) {
  case manifest.pull_request.enabled {
    False -> None
    True ->
      case lookup_open_pr(manifest, checkout_dir, runner) {
        Ok([pr]) -> Some(pr.url)
        _ -> None
      }
  }
}

pub fn current_head(
  checkout_dir: String,
  runner: command_runner.Runner,
) -> Option(String) {
  case
    run_stdout(
      runner,
      command_runner.sh("git", ["rev-parse", "HEAD"], checkout_dir),
      True,
    )
  {
    Ok(stdout) -> url_option(stdout)
    Error(_) -> None
  }
}

pub fn run_ok(
  runner: command_runner.Runner,
  spec: command_runner.CommandSpec,
  _retryable: Bool,
) -> Result(
  command_runner.CommandOutput,
  artifact_publication_manifest.PublicationErrorInfo,
) {
  let command_runner.Runner(run: run_fn) = runner
  use output <- result.try(
    run_fn(spec)
    |> result.map_error(fn(error) {
      artifact_publication_manifest.PublicationErrorInfo(
        code: "command_spawn_failed",
        message: command_runner.error_message(error),
      )
    }),
  )
  let command_runner.CommandSpec(executable, args, ..) = spec
  case output.exit_code == 0 {
    True -> Ok(output)
    False ->
      Error(artifact_publication_manifest.PublicationErrorInfo(
        code: failed_code(executable, args),
        message: command_runner.summarize(output),
      ))
  }
}

pub fn run_stdout(
  runner: command_runner.Runner,
  spec: command_runner.CommandSpec,
  retryable: Bool,
) -> Result(String, artifact_publication_manifest.PublicationErrorInfo) {
  use output <- result.try(run_ok(runner, spec, retryable))
  Ok(string.trim(output.stdout))
}

pub fn run(
  runner: command_runner.Runner,
  spec: command_runner.CommandSpec,
  _retryable: Bool,
) -> Result(
  command_runner.CommandOutput,
  artifact_publication_manifest.PublicationErrorInfo,
) {
  let command_runner.Runner(run: run_fn) = runner
  run_fn(spec)
  |> result.map_error(fn(error) {
    artifact_publication_manifest.PublicationErrorInfo(
      code: "command_spawn_failed",
      message: command_runner.error_message(error),
    )
  })
}

fn create_pr(
  manifest: artifact_publication_planner.DryRunPublicationManifest,
  checkout_dir: String,
  runner: command_runner.Runner,
  title: String,
  body: String,
) -> Result(Option(String), artifact_publication_manifest.PublicationErrorInfo) {
  use repo <- result.try(require_option(
    manifest.github_repo,
    artifact_publication_manifest.PublicationErrorInfo(
      code: "missing_github_repo",
      message: "planned github publication is missing github_repo",
    ),
  ))
  use base <- result.try(require_option(
    manifest.github_base,
    artifact_publication_manifest.PublicationErrorInfo(
      code: "missing_github_base",
      message: "planned github publication is missing github_base",
    ),
  ))
  let args = case manifest.pull_request.draft {
    True -> [
      "pr",
      "create",
      "--repo",
      repo,
      "--head",
      manifest.branch,
      "--base",
      base,
      "--title",
      title,
      "--body-file",
      "-",
      "--draft",
    ]
    False -> [
      "pr",
      "create",
      "--repo",
      repo,
      "--head",
      manifest.branch,
      "--base",
      base,
      "--title",
      title,
      "--body-file",
      "-",
    ]
  }
  case
    run_stdout(
      runner,
      command_runner.with_input(
        command_runner.sh("gh", args, checkout_dir),
        body,
      ),
      True,
    )
  {
    Ok(url) -> Ok(url_option(url))
    Error(error) -> Error(error)
  }
}

fn edit_pr(
  checkout_dir: String,
  runner: command_runner.Runner,
  pr: types.GithubPullRequestMatch,
  title: String,
  body: String,
) -> Result(Option(String), artifact_publication_manifest.PublicationErrorInfo) {
  let args = [
    "pr",
    "edit",
    int.to_string(pr.number),
    "--title",
    title,
    "--body-file",
    "-",
  ]
  case
    run_ok(
      runner,
      command_runner.with_input(
        command_runner.sh("gh", args, checkout_dir),
        body,
      ),
      True,
    )
  {
    Ok(_) -> Ok(Some(pr.url))
    Error(error) ->
      Error(artifact_publication_manifest.PublicationErrorInfo(
        code: "pr_edit_failed",
        message: error.message,
      ))
  }
}

fn lookup_open_pr(
  manifest: artifact_publication_planner.DryRunPublicationManifest,
  checkout_dir: String,
  runner: command_runner.Runner,
) -> Result(
  List(types.GithubPullRequestMatch),
  artifact_publication_manifest.PublicationErrorInfo,
) {
  use repo <- result.try(require_option(
    manifest.github_repo,
    artifact_publication_manifest.PublicationErrorInfo(
      code: "missing_github_repo",
      message: "planned github publication is missing github_repo",
    ),
  ))
  use base <- result.try(require_option(
    manifest.github_base,
    artifact_publication_manifest.PublicationErrorInfo(
      code: "missing_github_base",
      message: "planned github publication is missing github_base",
    ),
  ))
  let args = [
    "pr",
    "list",
    "--repo",
    repo,
    "--head",
    manifest.branch,
    "--base",
    base,
    "--state",
    "open",
    "--json",
    "number,url,isDraft,title",
  ]
  use stdout <- result.try(run_stdout(
    runner,
    command_runner.sh("gh", args, checkout_dir),
    True,
  ))
  decode_pr_list(stdout)
}

fn decode_pr_list(
  stdout: String,
) -> Result(
  List(types.GithubPullRequestMatch),
  artifact_publication_manifest.PublicationErrorInfo,
) {
  case string.trim(stdout) {
    "" -> Ok([])
    text -> {
      let json = string.replace(text, each: " ", with: "")
      case json {
        "[]" -> Ok([])
        _ ->
          decode_pr_segments(
            stdout
              |> string.replace(each: "[{", with: "")
              |> string.replace(each: "}]", with: "")
              |> string.split(on: "},{")
              |> list.filter(fn(segment) { string.trim(segment) != "" }),
            [],
          )
      }
    }
  }
}

fn decode_pr_segments(
  segments: List(String),
  acc: List(types.GithubPullRequestMatch),
) -> Result(
  List(types.GithubPullRequestMatch),
  artifact_publication_manifest.PublicationErrorInfo,
) {
  case segments {
    [] -> Ok(list.reverse(acc))
    [segment, ..rest] -> {
      use number <- result.try(extract_json_int(segment, "number"))
      use url <- result.try(extract_json_string(segment, "url"))
      let draft = extract_json_bool(segment, "isDraft") |> result.unwrap(False)
      decode_pr_segments(rest, [
        types.GithubPullRequestMatch(number, url, draft),
        ..acc
      ])
    }
  }
}

fn extract_json_string(
  body: String,
  key: String,
) -> Result(String, artifact_publication_manifest.PublicationErrorInfo) {
  let prefix = "\"" <> key <> "\":\""
  case string.split_once(body, on: prefix) {
    Ok(#(_, rest)) ->
      case string.split_once(rest, on: "\"") {
        Ok(#(value, _)) -> Ok(value)
        Error(_) -> malformed_pr_json()
      }
    Error(_) -> malformed_pr_json()
  }
}

fn extract_json_int(
  body: String,
  key: String,
) -> Result(Int, artifact_publication_manifest.PublicationErrorInfo) {
  let prefix = "\"" <> key <> "\":"
  case string.split_once(body, on: prefix) {
    Ok(#(_, rest)) ->
      case int.parse(take_until_delimiter(rest)) {
        Ok(value) -> Ok(value)
        Error(_) -> malformed_pr_json()
      }
    Error(_) -> malformed_pr_json()
  }
}

fn extract_json_bool(
  body: String,
  key: String,
) -> Result(Bool, artifact_publication_manifest.PublicationErrorInfo) {
  let prefix = "\"" <> key <> "\":"
  case string.split_once(body, on: prefix) {
    Ok(#(_, rest)) ->
      case take_until_delimiter(rest) {
        "true" -> Ok(True)
        "false" -> Ok(False)
        _ -> malformed_pr_json()
      }
    Error(_) -> malformed_pr_json()
  }
}

fn take_until_delimiter(value: String) -> String {
  value
  |> string.split(on: ",")
  |> list.first
  |> result.unwrap(value)
  |> string.trim
}

fn malformed_pr_json() -> Result(
  a,
  artifact_publication_manifest.PublicationErrorInfo,
) {
  Error(artifact_publication_manifest.PublicationErrorInfo(
    code: "pr_json_malformed",
    message: "gh pr list returned malformed json",
  ))
}

fn default_pr_title(
  manifest: artifact_publication_planner.DryRunPublicationManifest,
) -> String {
  case manifest.pull_request.title {
    Some(title) ->
      case string.trim(title) == "" {
        True ->
          "Scherzo publication "
          <> manifest.publication_id
          <> " "
          <> manifest.version_id
        False -> title
      }
    None ->
      "Scherzo publication "
      <> manifest.publication_id
      <> " "
      <> manifest.version_id
  }
}

fn default_pr_body(
  manifest: artifact_publication_planner.DryRunPublicationManifest,
) -> String {
  case manifest.pull_request.body {
    Some(body) ->
      case string.trim(body) == "" {
        True -> "Published by Scherzo.\n\nVersion: " <> manifest.version_id
        False -> body
      }
    None -> "Published by Scherzo.\n\nVersion: " <> manifest.version_id
  }
}

fn require_option(
  value: Option(a),
  error: artifact_publication_manifest.PublicationErrorInfo,
) -> Result(a, artifact_publication_manifest.PublicationErrorInfo) {
  case value {
    Some(value) -> Ok(value)
    None -> Error(error)
  }
}

fn url_option(value: String) -> Option(String) {
  case string.trim(value) {
    "" -> None
    other -> Some(other)
  }
}

fn failed_code(executable: String, args: List(String)) -> String {
  case executable, args {
    "git", ["diff", ..] -> "git_diff_failed"
    "git", ["add", ..] -> "git_add_failed"
    "git", ["commit", ..] -> "git_commit_failed"
    "git", ["push", ..] -> "git_push_failed"
    "git", ["fetch", ..] -> "git_fetch_failed"
    "git", ["clone", ..] -> "git_clone_failed"
    "git", ["checkout", ..] -> "git_checkout_failed"
    "git", ["status", ..] -> "git_status_failed"
    "git", ["remote", ..] -> "git_remote_failed"
    "gh", ["pr", "list", ..] -> "pr_list_failed"
    "gh", ["pr", "create", ..] -> "pr_create_failed"
    "gh", ["pr", "edit", ..] -> "pr_edit_failed"
    _, _ -> executable <> "_failed"
  }
}
