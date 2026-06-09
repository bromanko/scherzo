import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/artifact_publication_planner

pub fn title(
  manifest: artifact_publication_planner.DryRunPublicationManifest,
) -> String {
  case non_empty_option(manifest.pull_request.title) {
    Some(title) -> title
    None -> fallback_title(manifest)
  }
}

pub fn body(
  manifest: artifact_publication_planner.DryRunPublicationManifest,
) -> String {
  case non_empty_body(manifest.pull_request.body) {
    Some(body) -> body
    None -> fallback_body(manifest)
  }
}

fn fallback_title(
  manifest: artifact_publication_planner.DryRunPublicationManifest,
) -> String {
  let action = "publish " <> humanize_token(manifest.publication_id)
  case work_title_prefix(manifest.work) {
    Some(prefix) -> prefix <> ": " <> action
    None -> "Scherzo: " <> action
  }
}

fn fallback_body(
  manifest: artifact_publication_planner.DryRunPublicationManifest,
) -> String {
  "Published by Scherzo.\n\n"
  <> "## Work\n\n"
  <> "- Identifier: "
  <> work_identifier_text(manifest.work)
  <> "\n- Title: "
  <> work_title_text(manifest.work)
  <> "\n- Source: "
  <> work_source_text(manifest.work)
  <> "\n\n## Publication route\n\n"
  <> "- Workflow: `"
  <> manifest.workflow_id
  <> "`\n- Publication: `"
  <> manifest.publication_id
  <> "`\n- Run: `"
  <> manifest.run_id
  <> "`\n- Version: `"
  <> manifest.version_id
  <> "`\n- Series: `"
  <> manifest.series_id
  <> "`\n- Repository: "
  <> repository_text(manifest)
  <> "\n- Branch: `"
  <> manifest.branch
  <> "`\n\n## Reviewed content\n\n"
  <> "Published artifact paths:\n"
  <> unavailable_if_empty(published_files_markdown(manifest))
  <> "\n\nChanged files:\n"
  <> unavailable_if_empty(changed_files_markdown(manifest))
  <> "\n\n## Validation and review evidence\n\n"
  <> "Review the retained Scherzo workflow run artifacts for `"
  <> manifest.run_id
  <> "`, publication attempt records for `"
  <> manifest.publication_id
  <> "`, and the Linear result attachment/comment when available.\n"
}

fn non_empty_option(value: Option(String)) -> Option(String) {
  case value {
    Some(value) -> non_empty(value)
    None -> None
  }
}

fn non_empty_body(value: Option(String)) -> Option(String) {
  case value {
    Some(value) ->
      case string.trim(value) {
        "" -> None
        _ -> Some(value)
      }
    None -> None
  }
}

fn non_empty(value: String) -> Option(String) {
  case string.trim(value) {
    "" -> None
    value -> Some(value)
  }
}

fn work_title_prefix(
  work: artifact_publication_planner.PublicationWork,
) -> Option(String) {
  case non_empty(work.identifier) {
    Some(identifier) -> Some(identifier)
    None ->
      case non_empty_option(work.title) {
        Some(title) -> Some(title)
        None -> non_empty(work.id)
      }
  }
}

fn work_identifier_text(
  work: artifact_publication_planner.PublicationWork,
) -> String {
  case non_empty(work.identifier) {
    Some(identifier) -> "`" <> identifier <> "`"
    None -> "Unavailable"
  }
}

fn work_title_text(
  work: artifact_publication_planner.PublicationWork,
) -> String {
  case non_empty_option(work.title) {
    Some(title) -> title
    None -> "Unavailable"
  }
}

fn work_source_text(
  work: artifact_publication_planner.PublicationWork,
) -> String {
  case non_empty_option(work.url) {
    Some(url) ->
      case non_empty(work.identifier) {
        Some(identifier) -> "[" <> identifier <> "](" <> url <> ")"
        None -> url
      }
    None -> "Unavailable"
  }
}

fn repository_text(
  manifest: artifact_publication_planner.DryRunPublicationManifest,
) -> String {
  case manifest.github_repo, manifest.github_base {
    Some(repo), Some(base) -> "`" <> repo <> "` on `" <> base <> "`"
    Some(repo), None -> "`" <> repo <> "`"
    None, Some(base) -> "base `" <> base <> "`"
    None, None -> "Unavailable"
  }
}

fn published_files_markdown(
  manifest: artifact_publication_planner.DryRunPublicationManifest,
) -> String {
  manifest.files
  |> list.map(fn(file) {
    let artifact_publication_planner.PlannedPublicationFile(source, path) = file
    let selector = case source.entry {
      Some(entry) -> source.output <> "/" <> entry
      None -> source.output
    }
    "- `" <> path <> "` from `" <> selector <> "`"
  })
  |> string.join(with: "\n")
}

fn changed_files_markdown(
  manifest: artifact_publication_planner.DryRunPublicationManifest,
) -> String {
  case manifest.commit_stack {
    Some(stack) -> markdown_list(stack.stack.changed_files)
    None -> ""
  }
}

fn markdown_list(values: List(String)) -> String {
  values
  |> list.map(fn(value) { "- `" <> value <> "`" })
  |> string.join(with: "\n")
}

fn unavailable_if_empty(value: String) -> String {
  case string.trim(value) {
    "" -> "Unavailable from this publication route."
    value -> value
  }
}

fn humanize_token(value: String) -> String {
  value
  |> string.replace(each: "_", with: " ")
  |> string.replace(each: "-", with: " ")
}
