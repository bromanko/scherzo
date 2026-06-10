import gleam/list
import gleam/option.{type Option, Some}
import gleam/string
import scherzo/artifact_publication_planner

pub fn title(
  manifest: artifact_publication_planner.DryRunPublicationManifest,
) -> String {
  case manifest.pull_request.title {
    Some(title) ->
      case string.trim(title) {
        "" -> fallback_title(manifest)
        trimmed -> trimmed
      }
    _ -> fallback_title(manifest)
  }
}

pub fn body(
  manifest: artifact_publication_planner.DryRunPublicationManifest,
) -> String {
  case manifest.pull_request.body {
    Some(body) ->
      case string.trim(body) {
        "" -> fallback_body(manifest)
        _ -> body
      }
    _ -> fallback_body(manifest)
  }
}

fn fallback_title(
  manifest: artifact_publication_planner.DryRunPublicationManifest,
) -> String {
  work_label(manifest.work)
  <> ": publish "
  <> publication_description(manifest.publication_id)
}

fn fallback_body(
  manifest: artifact_publication_planner.DryRunPublicationManifest,
) -> String {
  "## Work\n\n"
  <> "- Identifier: `"
  <> unavailable_if_empty(manifest.work.identifier)
  <> "`\n"
  <> "- Title: "
  <> work_title(manifest.work)
  <> "\n"
  <> "- Source URL: "
  <> work_url(manifest.work)
  <> "\n\n"
  <> "## Published change\n\n"
  <> "Scherzo published `"
  <> manifest.publication_id
  <> "` from workflow `"
  <> manifest.workflow_id
  <> "`.\n\n"
  <> "## Workflow route\n\n"
  <> "- Workflow: `"
  <> manifest.workflow_id
  <> "`\n"
  <> "- Publication: `"
  <> manifest.publication_id
  <> "`\n"
  <> "- Run: `"
  <> manifest.run_id
  <> "`\n"
  <> "- Version: `"
  <> manifest.version_id
  <> "`\n\n"
  <> "## Artifacts and files\n\n"
  <> files_or_commit_stack_markdown(manifest)
  <> "\n\n"
  <> "## Validation and review evidence\n\n"
  <> "Review the retained Scherzo workflow run artifacts for run `"
  <> manifest.run_id
  <> "`, including validation, review, and publication attempt records. "
  <> "When this PR is linked from Linear, the result attachment there is also "
  <> "part of the review evidence.\n"
}

fn files_or_commit_stack_markdown(
  manifest: artifact_publication_planner.DryRunPublicationManifest,
) -> String {
  case manifest.files {
    [] -> commit_stack_markdown(manifest.commit_stack)
    files -> planned_files_markdown(files)
  }
}

fn planned_files_markdown(
  files: List(artifact_publication_planner.PlannedPublicationFile),
) -> String {
  files
  |> list.map(fn(file) {
    let artifact_publication_planner.PlannedPublicationFile(
      source,
      destination_path,
    ) = file
    let selector = case source.entry {
      Some(entry) -> source.output <> "/" <> entry
      _ -> source.output
    }
    "- `" <> destination_path <> "` ← `" <> selector <> "`"
  })
  |> string.join(with: "\n")
}

fn commit_stack_markdown(
  commit_stack: Option(artifact_publication_planner.PlannedCommitStack),
) -> String {
  case commit_stack {
    Some(stack) ->
      "- Commit stack output: `"
      <> stack.output
      <> "`\n"
      <> "- Base: `"
      <> stack.stack.base_ref
      <> "` @ `"
      <> stack.stack.base_sha
      <> "`\n"
      <> "- Head: `"
      <> stack.stack.head_sha
      <> "`\n"
      <> "- Changed files: review the GitHub PR Files changed tab."
    _ -> "- No published artifact or changed-file list is available."
  }
}

fn work_label(work: artifact_publication_planner.PublicationWork) -> String {
  case string.trim(work.identifier) {
    "" ->
      case string.trim(work.id) {
        "" -> "Scherzo work"
        id -> id
      }
    identifier -> identifier
  }
}

fn publication_description(publication_id: String) -> String {
  case string.trim(publication_id) {
    "" -> "workflow output"
    id -> string.replace(id, each: "_", with: " ")
  }
}

fn work_title(work: artifact_publication_planner.PublicationWork) -> String {
  case work.title {
    Some(title) -> unavailable_if_empty(title)
    _ -> "Unavailable"
  }
}

fn work_url(work: artifact_publication_planner.PublicationWork) -> String {
  case work.url {
    Some(url) -> unavailable_if_empty(url)
    _ -> "Unavailable"
  }
}

fn unavailable_if_empty(value: String) -> String {
  case string.trim(value) {
    "" -> "Unavailable"
    trimmed -> trimmed
  }
}
