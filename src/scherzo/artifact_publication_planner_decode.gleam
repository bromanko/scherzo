import gleam/dynamic/decode
import gleam/json
import gleam/option.{type Option, None, Some}
import gleam/result
import scherzo/artifact_publication_planner
import scherzo/commit_stack_artifact
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
  use workflow_id <- decode.field("workflow_id", decode.string)
  use publication_id <- decode.field("publication_id", decode.string)
  use series_id <- decode.field("series_id", decode.string)
  use version_id <- decode.field("version_id", decode.string)
  use required <- decode.field("required", decode.bool)
  use dry_run <- decode.field("dry_run", decode.bool)
  use repository <- decode.field("repository", repository_decoder())
  use branch <- decode.field("branch", decode.string)
  use target <- decode.optional_field(
    "target",
    artifact_publication_planner.StableBranchTargetPlan,
    target_decoder(),
  )
  use pull_request <- decode.field("pull_request", pull_request_decoder())
  use files <- decode.field("files", decode.list(planned_file_decoder()))
  use commit_stack <- decode.optional_field(
    "commit_stack",
    None,
    decode.optional(planned_commit_stack_decoder()),
  )
  use work <- decode.optional_field(
    "work",
    legacy_publication_work(),
    publication_work_decoder(),
  )
  let _ = schema_version
  let _ = artifact_type
  let #(repository_kind, repository_id, github_repo, github_base) = repository
  let build_manifest = fn(publication) {
    artifact_publication_planner.DryRunPublicationManifest(
      run_id: run_id,
      workflow_id: workflow_id,
      publication_id: publication_id,
      series_id: series_id,
      version_id: version_id,
      required: required,
      dry_run: dry_run,
      repository_kind: repository_kind,
      repository_id: repository_id,
      github_repo: github_repo,
      github_base: github_base,
      branch: branch,
      target: target,
      pull_request: pull_request,
      publication: publication,
      work: work,
    )
  }
  case decode_publication(files, commit_stack) {
    Ok(publication) -> decode.success(build_manifest(publication))
    Error(Nil) ->
      decode.failure(
        build_manifest(
          artifact_publication_planner.PlannedFilePublication(files: []),
        ),
        expected: "publication manifest must not include both files and commit_stack",
      )
  }
}

fn decode_publication(
  files: List(artifact_publication_planner.PlannedPublicationFile),
  commit_stack: Option(artifact_publication_planner.PlannedCommitStack),
) -> Result(artifact_publication_planner.PlannedPublication, Nil) {
  case files, commit_stack {
    [_, ..], Some(_) -> Error(Nil)
    _, Some(commit_stack) ->
      Ok(artifact_publication_planner.PlannedCommitStackPublication(
        commit_stack: commit_stack,
      ))
    _, None ->
      Ok(artifact_publication_planner.PlannedFilePublication(files: files))
  }
}

fn publication_work_decoder() -> decode.Decoder(
  artifact_publication_planner.PublicationWork,
) {
  use kind <- decode.field("kind", decode.string)
  use id <- decode.field("id", decode.string)
  use identifier <- decode.field("identifier", decode.string)
  use slug <- decode.field("slug", decode.string)
  use title <- decode.optional_field(
    "title",
    None,
    decode.optional(decode.string),
  )
  use url <- decode.optional_field("url", None, decode.optional(decode.string))
  case kind {
    "task" ->
      decode.success(artifact_publication_planner.PublicationWork(
        kind: artifact_publication_planner.TaskWork,
        id: id,
        identifier: identifier,
        slug: slug,
        title: title,
        url: url,
      ))
    "scheduled" ->
      decode.success(artifact_publication_planner.PublicationWork(
        kind: artifact_publication_planner.ScheduledWork,
        id: id,
        identifier: identifier,
        slug: slug,
        title: title,
        url: url,
      ))
    _ -> decode.failure(legacy_publication_work(), expected: "publication work")
  }
}

fn legacy_publication_work() -> artifact_publication_planner.PublicationWork {
  artifact_publication_planner.PublicationWork(
    kind: artifact_publication_planner.ScheduledWork,
    id: "",
    identifier: "",
    slug: "",
    title: None,
    url: None,
  )
}

fn target_decoder() -> decode.Decoder(
  artifact_publication_planner.PlannedPublicationTarget,
) {
  use kind <- decode.field("kind", decode.string)
  case kind {
    "stable_branch" ->
      decode.success(artifact_publication_planner.StableBranchTargetPlan)
    "existing_pr_branch" -> {
      use existing <- decode.field(
        "existing_pr_branch",
        existing_pr_branch_target_decoder(),
      )
      decode.success(artifact_publication_planner.ExistingPrBranchTargetPlan(
        existing,
      ))
    }
    _ ->
      decode.failure(
        artifact_publication_planner.StableBranchTargetPlan,
        expected: "publication target",
      )
  }
}

fn planned_commit_stack_decoder() -> decode.Decoder(
  artifact_publication_planner.PlannedCommitStack,
) {
  use output <- decode.field("output", decode.string)
  use manifest_ref <- decode.field("manifest_ref", decode.string)
  use manifest_sha256 <- decode.field("manifest_sha256", decode.string)
  use manifest_bytes <- decode.field("manifest_bytes", decode.int)
  use stack <- decode.field("stack", commit_stack_decoder())
  decode.success(artifact_publication_planner.PlannedCommitStack(
    output: output,
    manifest_ref: manifest_ref,
    manifest_sha256: manifest_sha256,
    manifest_bytes: manifest_bytes,
    stack: stack,
  ))
}

fn commit_stack_decoder() -> decode.Decoder(
  commit_stack_artifact.CommitStackArtifact,
) {
  use repository <- decode.field("repository", decode.string)
  use base_ref <- decode.field("base_ref", decode.string)
  use base_sha <- decode.field("base_sha", decode.string)
  use head_sha <- decode.field("head_sha", decode.string)
  use head_tree <- decode.field("head_tree", decode.string)
  use carrier <- decode.field("carrier", carrier_decoder())
  let stack =
    commit_stack_artifact.CommitStackArtifact(
      repository: repository,
      base_ref: base_ref,
      base_sha: base_sha,
      head_sha: head_sha,
      head_tree: head_tree,
      carrier: carrier,
    )
  case commit_stack_artifact.validate_commit_stack(stack) {
    Ok(stack) -> decode.success(stack)
    Error(_) -> decode.failure(stack, expected: "valid commit stack artifact")
  }
}

fn carrier_decoder() -> decode.Decoder(commit_stack_artifact.CommitStackCarrier) {
  use ref <- decode.field("ref", decode.string)
  use sha256 <- decode.field("sha256", decode.string)
  use bytes <- decode.field("bytes", decode.int)
  use media_type <- decode.field("media_type", decode.string)
  decode.success(commit_stack_artifact.CommitStackCarrier(
    ref: ref,
    sha256: sha256,
    bytes: bytes,
    media_type: media_type,
  ))
}

fn existing_pr_branch_target_decoder() -> decode.Decoder(
  commit_stack_artifact.ExistingPrBranchTarget,
) {
  use repository <- decode.field("repository", decode.string)
  use head_repo <- decode.field("head_repo", decode.string)
  use head_branch <- decode.field("head_branch", decode.string)
  use expected_head_sha <- decode.field("expected_head_sha", decode.string)
  use base_branch <- decode.field("base_branch", decode.string)
  use base_sha <- decode.field("base_sha", decode.string)
  use pr_number <- decode.field("pr_number", decode.int)
  use pr_url <- decode.field("pr_url", decode.string)
  let target =
    commit_stack_artifact.ExistingPrBranchTarget(
      repository: repository,
      head_repo: head_repo,
      head_branch: head_branch,
      expected_head_sha: expected_head_sha,
      base_branch: base_branch,
      base_sha: base_sha,
      pr_number: pr_number,
      pr_url: pr_url,
    )
  case commit_stack_artifact.validate_existing_pr_branch_target(target) {
    Ok(target) -> decode.success(target)
    Error(_) ->
      decode.failure(target, expected: "valid existing PR branch target")
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
