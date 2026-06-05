import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/artifact_publication_manifest
import scherzo/artifact_publication_planner
import scherzo/artifact_repository/command_runner
import scherzo/artifact_repository/types
import scherzo/path

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
        Ok([]) ->
          create_pr(manifest, checkout_dir, runner, title, body)
          |> result.map(Some)
        Ok([pr]) ->
          edit_pr(checkout_dir, runner, pr, title, body)
          |> result.map(Some)
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

fn gh_command(
  args: List(String),
  checkout_dir: String,
) -> command_runner.CommandSpec {
  command_runner.sh("gh", args, checkout_dir)
  |> command_runner.with_env(github_auth_env())
}

fn github_auth_env() -> List(#(String, String)) {
  case path.env("GH_TOKEN"), path.env("GITHUB_TOKEN") {
    Some(token), _ -> [#("GH_TOKEN", token)]
    None, Some(token) -> [#("GH_TOKEN", token)]
    None, None ->
      case path.env("SCHERZO_AGENT_GITHUB_TOKEN") {
        Some(token) -> [#("GH_TOKEN", token)]
        None -> []
      }
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
) -> Result(String, artifact_publication_manifest.PublicationErrorInfo) {
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
      command_runner.with_input(gh_command(args, checkout_dir), body),
      True,
    )
  {
    Ok(url) ->
      case url_option(url) {
        Some(url) -> Ok(url)
        None ->
          case view_open_pr_by_branch(manifest, checkout_dir, runner) {
            Ok(Some(pr)) -> edit_pr(checkout_dir, runner, pr, title, body)
            Ok(None) ->
              Error(artifact_publication_manifest.PublicationErrorInfo(
                code: "pr_create_missing_url",
                message: "gh pr create produced no pull request url",
              ))
            Error(error) -> Error(error)
          }
      }
    Error(error) ->
      case view_open_pr_by_branch(manifest, checkout_dir, runner) {
        Ok(Some(pr)) -> edit_pr(checkout_dir, runner, pr, title, body)
        Ok(None) -> Error(error)
        Error(view_error) -> {
          let _ = view_error
          Error(error)
        }
      }
  }
}

fn view_open_pr_by_branch(
  manifest: artifact_publication_planner.DryRunPublicationManifest,
  checkout_dir: String,
  runner: command_runner.Runner,
) -> Result(
  Option(types.GithubPullRequestMatch),
  artifact_publication_manifest.PublicationErrorInfo,
) {
  use repo <- result.try(require_option(
    manifest.github_repo,
    artifact_publication_manifest.PublicationErrorInfo(
      code: "missing_github_repo",
      message: "planned github publication is missing github_repo",
    ),
  ))
  let args = [
    "pr",
    "view",
    manifest.branch,
    "--repo",
    repo,
    "--json",
    "number,url,isDraft,state,title",
  ]
  use output <- result.try(run(runner, gh_command(args, checkout_dir), True))
  case output.exit_code == 0 {
    True -> decode_pr_view(output.stdout)
    False -> Ok(None)
  }
}

fn decode_pr_view(
  stdout: String,
) -> Result(
  Option(types.GithubPullRequestMatch),
  artifact_publication_manifest.PublicationErrorInfo,
) {
  json.parse(stdout, pr_view_decoder())
  |> result.map_error(fn(_) { malformed_pr_json_error() })
}

fn edit_pr(
  checkout_dir: String,
  runner: command_runner.Runner,
  pr: types.GithubPullRequestMatch,
  title: String,
  body: String,
) -> Result(String, artifact_publication_manifest.PublicationErrorInfo) {
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
      command_runner.with_input(gh_command(args, checkout_dir), body),
      True,
    )
  {
    Ok(_) -> Ok(pr.url)
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
    gh_command(args, checkout_dir),
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
    _ ->
      json.parse(stdout, decode.list(pr_match_decoder()))
      |> result.map_error(fn(_) { malformed_pr_json_error() })
  }
}

fn pr_view_decoder() -> decode.Decoder(Option(types.GithubPullRequestMatch)) {
  use state <- decode.optional_field(
    "state",
    None,
    decode.optional(decode.string),
  )
  case state {
    Some("OPEN") -> pr_match_decoder() |> decode.map(Some)
    _ -> decode.success(None)
  }
}

fn pr_match_decoder() -> decode.Decoder(types.GithubPullRequestMatch) {
  use number <- decode.field("number", decode.int)
  use url <- decode.field("url", decode.string)
  use is_draft <- decode.optional_field(
    "isDraft",
    None,
    decode.optional(decode.bool),
  )
  let draft = case is_draft {
    Some(value) -> value
    None -> False
  }
  decode.success(types.GithubPullRequestMatch(number, url, draft))
}

fn malformed_pr_json_error() -> artifact_publication_manifest.PublicationErrorInfo {
  artifact_publication_manifest.PublicationErrorInfo(
    code: "pr_json_malformed",
    message: "gh pr command returned malformed json",
  )
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
