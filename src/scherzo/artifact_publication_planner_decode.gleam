import gleam/dynamic/decode
import gleam/json
import gleam/option.{type Option, None}
import gleam/result
import scherzo/artifact_publication_config
import scherzo/artifact_publication_planner
import scherzo/json_value

pub fn decode_manifest_json(
  payload_json: String,
) -> Result(artifact_publication_planner.DryRunPublicationManifest, String) {
  json.parse(payload_json, dry_run_manifest_decoder())
  |> result.map_error(fn(_) { "invalid_publication_manifest_json" })
}

pub fn dry_run_manifest_decoder() -> decode.Decoder(
  artifact_publication_planner.DryRunPublicationManifest,
) {
  use schema_version <- decode.field("schema_version", decode.int)
  use artifact_type <- decode.field("artifact_type", decode.string)
  use run_id <- decode.field("run_id", decode.string)
  use work_title <- decode.optional_field(
    "work_title",
    None,
    decode.optional(decode.string),
  )
  use workflow_id <- decode.field("workflow_id", decode.string)
  use publication_id <- decode.field("publication_id", decode.string)
  use series_id <- decode.field("series_id", decode.string)
  use version_id <- decode.field("version_id", decode.string)
  use required <- decode.field("required", decode.bool)
  use dry_run <- decode.field("dry_run", decode.bool)
  use mode_text <- decode.optional_field("mode", "files", decode.string)
  use repository <- decode.field("repository", repository_decoder())
  use branch <- decode.field("branch", decode.string)
  use pull_request <- decode.field("pull_request", pull_request_decoder())
  use files <- decode.field("files", decode.list(planned_file_decoder()))
  use commit_stack <- decode.optional_field(
    "commit_stack",
    None,
    decode.optional(selected_artifact_decoder()),
  )
  let _ = schema_version
  let _ = artifact_type
  let #(repository_kind, repository_id, github_repo, github_base) = repository
  let default_manifest =
    artifact_publication_planner.DryRunPublicationManifest(
      run_id: run_id,
      work_title: work_title,
      workflow_id: workflow_id,
      publication_id: publication_id,
      series_id: series_id,
      version_id: version_id,
      required: required,
      dry_run: dry_run,
      mode: artifact_publication_config.FilesPublication,
      repository_kind: repository_kind,
      repository_id: repository_id,
      github_repo: github_repo,
      github_base: github_base,
      branch: branch,
      pull_request: pull_request,
      files: files,
      commit_stack: commit_stack,
    )
  case mode_text {
    "files" -> decode.success(default_manifest)
    "commit_stack" ->
      decode.success(
        artifact_publication_planner.DryRunPublicationManifest(
          ..default_manifest,
          mode: artifact_publication_config.CommitStackPublication,
        ),
      )
    _ -> decode.failure(default_manifest, expected: "publication mode")
  }
}

fn planned_file_decoder() -> decode.Decoder(
  artifact_publication_planner.PlannedPublicationFile,
) {
  use source <- decode.field("source", selected_artifact_decoder())
  use destination_path <- decode.field("destination_path", decode.string)
  decode.success(artifact_publication_planner.PlannedPublicationFile(
    source: source,
    destination_path: destination_path,
  ))
}

fn selected_artifact_decoder() -> decode.Decoder(
  artifact_publication_planner.SelectedArtifact,
) {
  use output <- decode.field("output", decode.string)
  use entry <- decode.field("entry", decode.optional(decode.string))
  use name <- decode.field("name", decode.string)
  use artifact_type <- decode.field(
    "artifact_type",
    decode.optional(decode.string),
  )
  use metadata <- decode.field(
    "metadata",
    decode.optional(json_value.decoder()),
  )
  use ref <- decode.field("ref", decode.string)
  use sha256 <- decode.field("sha256", decode.string)
  use bytes <- decode.field("bytes", decode.int)
  use media_type <- decode.field("media_type", decode.string)
  decode.success(artifact_publication_planner.SelectedArtifact(
    output: output,
    entry: entry,
    name: name,
    artifact_type: artifact_type,
    metadata: metadata,
    ref: ref,
    sha256: sha256,
    bytes: bytes,
    media_type: media_type,
  ))
}

fn repository_decoder() -> decode.Decoder(
  #(String, String, Option(String), Option(String)),
) {
  use kind <- decode.field("kind", decode.string)
  use id <- decode.field("id", decode.string)
  use github_repo <- decode.field("github_repo", decode.optional(decode.string))
  use github_base <- decode.field("github_base", decode.optional(decode.string))
  decode.success(#(kind, id, github_repo, github_base))
}

fn pull_request_decoder() -> decode.Decoder(
  artifact_publication_planner.PlannedPullRequest,
) {
  use enabled <- decode.field("enabled", decode.bool)
  use draft <- decode.field("draft", decode.bool)
  use title <- decode.field("title", decode.optional(decode.string))
  use body <- decode.field("body", decode.optional(decode.string))
  decode.success(artifact_publication_planner.PlannedPullRequest(
    enabled: enabled,
    draft: draft,
    title: title,
    body: body,
  ))
}
