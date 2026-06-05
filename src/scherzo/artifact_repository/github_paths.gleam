import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/artifact_publication_planner
import scherzo/hash
import scherzo/path

pub fn remote_url(repo: String) -> String {
  "https://github.com/" <> repo <> ".git"
}

pub fn same_remote(origin: String, remote_url: String, repo: String) -> Bool {
  let normalized = normalize_remote(origin)
  normalized == normalize_remote(remote_url) || normalized == repo
}

fn normalize_remote(remote: String) -> String {
  remote
  |> string.replace(each: "https://github.com/", with: "")
  |> string.replace(each: "git@github.com:", with: "")
  |> string.replace(each: ".git", with: "")
  |> string.trim
}

pub fn checkout_dir(
  workspace_root: String,
  manifest: artifact_publication_planner.DryRunPublicationManifest,
) -> String {
  let key =
    hash.sha256_hex(
      manifest.repository_id
      <> "|"
      <> option_or_empty(manifest.github_base)
      <> "|"
      <> manifest.series_id,
    )
  path.join(
    path.absolute_or_original(workspace_root),
    ".scherzo-state/artifact-repositories/github/" <> key,
  )
}

pub fn workspace_root_for(checkout_dir: String) -> String {
  string.split(checkout_dir, on: "/.scherzo-state/")
  |> list.first
  |> result.unwrap(".")
}

fn option_or_empty(value: Option(String)) -> String {
  case value {
    Some(value) -> value
    None -> ""
  }
}
