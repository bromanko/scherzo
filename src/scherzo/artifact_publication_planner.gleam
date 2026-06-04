import gleam/bit_array
import gleam/dict.{type Dict}
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/artifact_publication_config
import scherzo/artifact_publication_planner_support as planner_support
import scherzo/error
import scherzo/hash
import scherzo/json_value
import scherzo/state/artifact_store
import scherzo/template
import scherzo/workflow_artifact_descriptor as artifact_descriptor
import scherzo/workflow_contract_manifest

pub const schema_version = 1

pub const dry_run_artifact_type = "scherzo.artifact_publication.v1"

pub type PlannerError {
  PlannerError(code: String, message: String)
}

pub fn code(error: PlannerError) -> String {
  let PlannerError(code: code, ..) = error
  code
}

pub type WorkKind {
  TaskWork
  ScheduledWork
}

pub type PublicationWork {
  PublicationWork(kind: WorkKind, id: String, identifier: String, slug: String)
}

pub type SelectedArtifact {
  SelectedArtifact(
    output: String,
    entry: Option(String),
    name: String,
    artifact_type: Option(String),
    metadata: Option(json_value.JsonValue),
    ref: String,
    sha256: String,
    bytes: Int,
    media_type: String,
  )
}

pub type PlannedPublicationFile {
  PlannedPublicationFile(source: SelectedArtifact, destination_path: String)
}

pub type PlannedPullRequest {
  PlannedPullRequest(
    enabled: Bool,
    draft: Bool,
    title: Option(String),
    body: Option(String),
  )
}

pub type DryRunPublicationManifest {
  DryRunPublicationManifest(
    run_id: String,
    workflow_id: String,
    publication_id: String,
    series_id: String,
    version_id: String,
    required: Bool,
    dry_run: Bool,
    repository_kind: String,
    repository_id: String,
    github_repo: Option(String),
    github_base: Option(String),
    branch: String,
    pull_request: PlannedPullRequest,
    files: List(PlannedPublicationFile),
  )
}

pub fn plan_publication(
  manifest: workflow_contract_manifest.ContractOutputManifest,
  repositories: artifact_publication_config.ArtifactRepositories,
  route: artifact_publication_config.PublicationRoute,
  store: artifact_store.Store,
  work: PublicationWork,
  run_id: String,
  body_templates: Dict(String, String),
) -> Result(DryRunPublicationManifest, PlannerError) {
  use repository <- result.try(resolve_repository(
    repositories,
    route.repository,
  ))
  let series_id =
    planner_support.make_series_id(work.id, manifest.workflow_id, route.id)
  use selected <- result.try(select_files(manifest, route.files, store, []))
  use version_id <- result.try(compute_version_id(
    manifest.workflow_id,
    route,
    repository,
    work,
    selected,
    body_templates,
  ))
  use files <- result.try(
    render_files(
      route.files,
      selected,
      repository,
      route.id,
      series_id,
      version_id,
      manifest.workflow_id,
      run_id,
      work,
      [],
      [],
    ),
  )
  let files_markdown =
    files
    |> list.map(fn(file) {
      let PlannedPublicationFile(source, destination_path) = file
      let selector = case source.entry {
        Some(entry) -> source.output <> "/" <> entry
        None -> source.output
      }
      #(destination_path, selector, source.sha256)
    })
    |> planner_support.render_files_markdown
  use branch <- result.try(render_branch(
    repository,
    route.id,
    series_id,
    version_id,
    manifest.workflow_id,
    run_id,
    work,
  ))
  use pull_request <- result.try(render_pull_request(
    route,
    repository,
    series_id,
    version_id,
    manifest.workflow_id,
    run_id,
    work,
    files_markdown,
    body_templates,
  ))
  Ok(DryRunPublicationManifest(
    run_id: run_id,
    workflow_id: manifest.workflow_id,
    publication_id: route.id,
    series_id: series_id,
    version_id: version_id,
    required: route.required,
    dry_run: True,
    repository_kind: "github",
    repository_id: route.repository,
    github_repo: Some(repository.repo),
    github_base: Some(repository.base),
    branch: branch,
    pull_request: pull_request,
    files: files,
  ))
}

pub fn manifest_to_json(manifest: DryRunPublicationManifest) -> json.Json {
  json.object([
    #("schema_version", json.int(schema_version)),
    #("artifact_type", json.string(dry_run_artifact_type)),
    #("run_id", json.string(manifest.run_id)),
    #("workflow_id", json.string(manifest.workflow_id)),
    #("publication_id", json.string(manifest.publication_id)),
    #("series_id", json.string(manifest.series_id)),
    #("version_id", json.string(manifest.version_id)),
    #("required", json.bool(manifest.required)),
    #("dry_run", json.bool(manifest.dry_run)),
    #(
      "repository",
      planner_support.repository_to_json(
        manifest.repository_kind,
        manifest.repository_id,
        manifest.github_repo,
        manifest.github_base,
      ),
    ),
    #("branch", json.string(manifest.branch)),
    #("pull_request", pull_request_to_json(manifest.pull_request)),
    #("files", json.array(manifest.files, of: planned_file_to_json)),
  ])
}

pub fn manifest_to_string(manifest: DryRunPublicationManifest) -> String {
  manifest |> manifest_to_json |> json.to_string
}

type ResolvedGithubRepository {
  ResolvedGithubRepository(
    id: String,
    repo: String,
    base: String,
    branch_template: String,
    pull_request_enabled: Bool,
    pull_request_draft: Bool,
    pull_request_title: Option(String),
    pull_request_body_template: Option(String),
  )
}

fn resolve_repository(
  repositories: artifact_publication_config.ArtifactRepositories,
  repository_ref: String,
) -> Result(ResolvedGithubRepository, PlannerError) {
  case
    artifact_publication_config.repository_ref_parts(
      repository_ref,
      "publication.repository",
    )
  {
    Ok(#("github", name)) ->
      case dict.get(repositories.github, name) {
        Ok(target) -> {
          let artifact_publication_config.GithubRepositoryTarget(
            repo: repo,
            base: base,
            branch: artifact_publication_config.GithubBranchConfig(
              template: branch_template,
              ..,
            ),
            pull_request: artifact_publication_config.GithubPullRequestConfig(
              enabled: enabled,
              draft: draft,
              title: title,
              body_template: body_template,
              ..,
            ),
            ..,
          ) = target
          Ok(ResolvedGithubRepository(
            id: repository_ref,
            repo: repo,
            base: base,
            branch_template: branch_template,
            pull_request_enabled: enabled,
            pull_request_draft: draft,
            pull_request_title: title,
            pull_request_body_template: body_template,
          ))
        }
        Error(Nil) ->
          error(
            "unknown_repository",
            "unknown artifact repository: " <> repository_ref,
          )
      }
    Ok(#(backend, _)) ->
      error(
        "unsupported_repository_backend",
        "unsupported artifact repository backend: " <> backend,
      )
    Error(config_error) ->
      error(
        artifact_publication_config.error_code(config_error),
        artifact_publication_config.error_message(config_error),
      )
  }
}

fn select_files(
  manifest: workflow_contract_manifest.ContractOutputManifest,
  routes: List(artifact_publication_config.PublicationFileRoute),
  store: artifact_store.Store,
  acc: List(SelectedArtifact),
) -> Result(List(SelectedArtifact), PlannerError) {
  case routes {
    [] -> Ok(list.reverse(acc))
    [artifact_publication_config.PublicationFileRoute(selector:, ..), ..rest] -> {
      use selected <- result.try(select_artifact(manifest, selector, store))
      select_files(manifest, rest, store, [selected, ..acc])
    }
  }
}

fn select_artifact(
  manifest: workflow_contract_manifest.ContractOutputManifest,
  selector: artifact_publication_config.PublicationFileSelector,
  store: artifact_store.Store,
) -> Result(SelectedArtifact, PlannerError) {
  let artifact_publication_config.PublicationFileSelector(output:, entry:) =
    selector
  use named <- result.try(find_named_output(manifest.outputs, output))
  case named.value.status {
    workflow_contract_manifest.Absent ->
      error("absent_output", "publication output is absent: " <> output)
    workflow_contract_manifest.Present ->
      case entry {
        None -> select_output_file(output, named.value, store)
        Some(entry_name) ->
          select_output_entry(output, entry_name, named.value, store)
      }
  }
}

fn find_named_output(
  outputs: List(workflow_contract_manifest.NamedManifestValue),
  name: String,
) -> Result(workflow_contract_manifest.NamedManifestValue, PlannerError) {
  case outputs {
    [] -> error("unknown_output", "unknown publication output: " <> name)
    [output, ..rest] ->
      case output.name == name {
        True -> Ok(output)
        False -> find_named_output(rest, name)
      }
  }
}

fn select_output_file(
  output: String,
  value: workflow_contract_manifest.ManifestValue,
  store: artifact_store.Store,
) -> Result(SelectedArtifact, PlannerError) {
  use descriptor <- result.try(required_descriptor(output, value))
  case descriptor.kind {
    artifact_descriptor.FileKind ->
      descriptor_to_selected_artifact(output, None, descriptor, store)
    _ ->
      error(
        "non_file_descriptor",
        "publication selector resolved to a non-file artifact: " <> output,
      )
  }
}

fn select_output_entry(
  output: String,
  entry_name: String,
  value: workflow_contract_manifest.ManifestValue,
  store: artifact_store.Store,
) -> Result(SelectedArtifact, PlannerError) {
  use descriptor <- result.try(required_descriptor(output, value))
  case descriptor.kind {
    artifact_descriptor.ArtifactSetKind -> {
      use bundle_descriptor <- result.try(load_artifact_set_descriptor(
        output,
        descriptor,
        store,
      ))
      use entry_descriptor <- result.try(find_entry_descriptor(
        bundle_descriptor.entries,
        entry_name,
      ))
      case entry_descriptor.kind {
        artifact_descriptor.FileKind ->
          descriptor_to_selected_artifact(
            output,
            Some(entry_name),
            entry_descriptor,
            store,
          )
        _ ->
          error(
            "non_file_descriptor",
            "publication selector resolved to a non-file artifact: "
              <> output
              <> "/"
              <> entry_name,
          )
      }
    }
    _ ->
      error(
        "invalid_selector_entry",
        "publication output does not support entry selection: " <> output,
      )
  }
}

fn required_descriptor(
  output: String,
  value: workflow_contract_manifest.ManifestValue,
) -> Result(artifact_descriptor.ArtifactDescriptor, PlannerError) {
  case workflow_contract_manifest.descriptor_for_named_value(output, value) {
    Some(descriptor) -> Ok(descriptor)
    None ->
      error(
        "missing_ref",
        "publication output is missing a retained artifact ref: " <> output,
      )
  }
}

fn load_artifact_set_descriptor(
  output: String,
  descriptor: artifact_descriptor.ArtifactDescriptor,
  store: artifact_store.Store,
) -> Result(artifact_descriptor.ArtifactDescriptor, PlannerError) {
  use ref <- result.try(require_option(
    descriptor.ref,
    "missing_ref",
    "artifact_set is missing ref: " <> output,
  ))
  use expected_sha <- result.try(require_option(
    descriptor.sha256,
    "missing_ref",
    "artifact_set is missing sha256: " <> output,
  ))
  use expected_bytes <- result.try(require_option(
    descriptor.bytes,
    "missing_ref",
    "artifact_set is missing bytes: " <> output,
  ))
  use contents <- result.try(read_artifact_text(ref, store))
  use Nil <- result.try(verify_text_contents(
    ref,
    contents,
    expected_sha,
    expected_bytes,
  ))
  case artifact_descriptor.parse_retained_artifact_set(contents, descriptor) {
    Ok(parsed) -> Ok(parsed)
    Error(parse_error) ->
      error("invalid_artifact_set_descriptor", parse_error.message)
  }
}

fn descriptor_to_selected_artifact(
  output: String,
  entry: Option(String),
  descriptor: artifact_descriptor.ArtifactDescriptor,
  store: artifact_store.Store,
) -> Result(SelectedArtifact, PlannerError) {
  use ref <- result.try(require_option(
    descriptor.ref,
    "missing_ref",
    descriptor.name <> " is missing ref",
  ))
  use sha256 <- result.try(require_option(
    descriptor.sha256,
    "missing_ref",
    descriptor.name <> " is missing sha256",
  ))
  use bytes <- result.try(require_option(
    descriptor.bytes,
    "missing_ref",
    descriptor.name <> " is missing bytes",
  ))
  use media_type <- result.try(require_option(
    descriptor.media_type,
    "missing_ref",
    descriptor.name <> " is missing media_type",
  ))
  use contents <- result.try(read_artifact_bytes(ref, store))
  use Nil <- result.try(verify_artifact_bytes(ref, contents, sha256, bytes))
  Ok(SelectedArtifact(
    output: output,
    entry: entry,
    name: descriptor.name,
    artifact_type: descriptor.artifact_type,
    metadata: descriptor.metadata,
    ref: ref,
    sha256: sha256,
    bytes: bytes,
    media_type: media_type,
  ))
}

fn read_artifact_text(
  ref: String,
  store: artifact_store.Store,
) -> Result(String, PlannerError) {
  case artifact_store.read_artifact_unverified(store, ref) {
    Ok(contents) -> Ok(contents)
    Error(read_error) ->
      error(
        "missing_artifact_bytes",
        "artifact bytes could not be read for ref: "
          <> ref
          <> " ("
          <> planner_support.artifact_error_summary(read_error)
          <> ")",
      )
  }
}

fn read_artifact_bytes(
  ref: String,
  store: artifact_store.Store,
) -> Result(BitArray, PlannerError) {
  case artifact_store.read_artifact_bytes_unverified(store, ref) {
    Ok(contents) -> Ok(contents)
    Error(read_error) ->
      error(
        "missing_artifact_bytes",
        "artifact bytes could not be read for ref: "
          <> ref
          <> " ("
          <> planner_support.artifact_error_summary(read_error)
          <> ")",
      )
  }
}

fn verify_text_contents(
  ref: String,
  contents: String,
  expected_sha256: String,
  expected_bytes: Int,
) -> Result(Nil, PlannerError) {
  let actual_sha256 = hash.sha256_hex(contents)
  let actual_bytes = bit_array.byte_size(bit_array.from_string(contents))
  use Nil <- result.try(check_sha256(ref, actual_sha256, expected_sha256))
  check_bytes(ref, actual_bytes, expected_bytes)
}

fn verify_artifact_bytes(
  ref: String,
  contents: BitArray,
  expected_sha256: String,
  expected_bytes: Int,
) -> Result(Nil, PlannerError) {
  let actual_sha256 = hash.sha256_hex_bytes(contents)
  let actual_bytes = bit_array.byte_size(contents)
  use Nil <- result.try(check_sha256(ref, actual_sha256, expected_sha256))
  check_bytes(ref, actual_bytes, expected_bytes)
}

fn check_sha256(
  ref: String,
  actual: String,
  expected: String,
) -> Result(Nil, PlannerError) {
  case actual == expected {
    True -> Ok(Nil)
    False ->
      error("hash_mismatch", "artifact sha256 did not match for ref: " <> ref)
  }
}

fn check_bytes(
  ref: String,
  actual: Int,
  expected: Int,
) -> Result(Nil, PlannerError) {
  case actual == expected {
    True -> Ok(Nil)
    False ->
      error(
        "byte_count_mismatch",
        "artifact byte count did not match for ref: " <> ref,
      )
  }
}

fn find_entry_descriptor(
  entries: List(artifact_descriptor.ArtifactDescriptor),
  entry_name: String,
) -> Result(artifact_descriptor.ArtifactDescriptor, PlannerError) {
  case entries {
    [] ->
      error(
        "missing_artifact_set_entry",
        "artifact_set entry not found: " <> entry_name,
      )
    [entry, ..rest] ->
      case entry.name == entry_name {
        True -> Ok(entry)
        False -> find_entry_descriptor(rest, entry_name)
      }
  }
}

fn compute_version_id(
  workflow_id: String,
  route: artifact_publication_config.PublicationRoute,
  repository: ResolvedGithubRepository,
  work: PublicationWork,
  selected: List(SelectedArtifact),
  body_templates: Dict(String, String),
) -> Result(String, PlannerError) {
  let body_template = effective_body_template_path(route, repository)
  use body_template_contents <- result.try(
    case repository.pull_request_enabled {
      False -> Ok(None)
      True ->
        case body_template {
          Some(template_path) ->
            resolve_body_template(template_path, body_templates)
            |> result.map(Some)
          None -> Ok(None)
        }
    },
  )
  let title_template = effective_title_template(route, repository)
  let payload =
    json.object([
      #("workflow_id", json.string(workflow_id)),
      #("publication_id", json.string(route.id)),
      #("repository_id", json.string(repository.id)),
      #("github_repo", json.string(repository.repo)),
      #("github_base", json.string(repository.base)),
      #("branch_template", json.string(repository.branch_template)),
      #("pull_request_enabled", json.bool(repository.pull_request_enabled)),
      #("pull_request_draft", json.bool(repository.pull_request_draft)),
      #("title_template", planner_support.option_string_to_json(title_template)),
      #(
        "body_template_path",
        planner_support.option_string_to_json(body_template),
      ),
      #(
        "body_template_contents",
        planner_support.option_string_to_json(body_template_contents),
      ),
      #(
        "work",
        planner_support.work_identity_to_json(
          work_kind_to_string(work.kind),
          work.id,
          work.identifier,
          work.slug,
        ),
      ),
      #(
        "files",
        json.array(
          route.files,
          of: planner_support.publication_file_route_to_version_json,
        ),
      ),
      #("selected", json.array(selected, of: selected_for_version_json)),
    ])
    |> json.to_string
  Ok(hash.sha256_hex(payload))
}

fn render_files(
  routes: List(artifact_publication_config.PublicationFileRoute),
  selected: List(SelectedArtifact),
  repository: ResolvedGithubRepository,
  publication_id: String,
  series_id: String,
  version_id: String,
  workflow_id: String,
  run_id: String,
  work: PublicationWork,
  seen_paths: List(String),
  acc: List(PlannedPublicationFile),
) -> Result(List(PlannedPublicationFile), PlannerError) {
  case routes, selected {
    [], [] -> Ok(list.reverse(acc))
    [artifact_publication_config.PublicationFileRoute(path:, ..), ..route_rest],
      [selected, ..selected_rest]
    -> {
      use destination_path <- result.try(render_file_path(
        path,
        selected,
        repository,
        publication_id,
        series_id,
        version_id,
        workflow_id,
        run_id,
        work,
      ))
      case list.contains(seen_paths, destination_path) {
        True ->
          error(
            "duplicate_destination_path",
            "publication produced a duplicate destination path: "
              <> destination_path,
          )
        False ->
          render_files(
            route_rest,
            selected_rest,
            repository,
            publication_id,
            series_id,
            version_id,
            workflow_id,
            run_id,
            work,
            [destination_path, ..seen_paths],
            [PlannedPublicationFile(selected, destination_path), ..acc],
          )
      }
    }
    _, _ ->
      error(
        "planner_internal_mismatch",
        "publication route and selection counts diverged",
      )
  }
}

fn render_branch(
  repository: ResolvedGithubRepository,
  publication_id: String,
  series_id: String,
  version_id: String,
  workflow_id: String,
  run_id: String,
  work: PublicationWork,
) -> Result(String, PlannerError) {
  use rendered <- result.try(
    planner_support.render_template(
      repository.branch_template,
      base_template_locals(
        publication_id,
        series_id,
        version_id,
        workflow_id,
        run_id,
        work,
        repository,
      ),
    )
    |> result.map_error(template_error_to_planner_error),
  )
  use Nil <- result.try(
    planner_support.validate_branch(rendered)
    |> result.map_error(validation_error_to_planner_error),
  )
  Ok(rendered)
}

fn render_pull_request(
  route: artifact_publication_config.PublicationRoute,
  repository: ResolvedGithubRepository,
  series_id: String,
  version_id: String,
  workflow_id: String,
  run_id: String,
  work: PublicationWork,
  files_markdown: String,
  body_templates: Dict(String, String),
) -> Result(PlannedPullRequest, PlannerError) {
  case repository.pull_request_enabled {
    False ->
      Ok(PlannedPullRequest(False, repository.pull_request_draft, None, None))
    True -> {
      let locals = [
        #("publication.files_markdown", template.VString(files_markdown)),
        ..base_template_locals(
          route.id,
          series_id,
          version_id,
          workflow_id,
          run_id,
          work,
          repository,
        )
      ]
      use rendered_title <- result.try(
        case effective_title_template(route, repository) {
          Some(title_template) ->
            planner_support.render_template(title_template, locals)
            |> result.map_error(template_error_to_planner_error)
            |> result.map(Some)
          None -> Ok(None)
        },
      )
      use rendered_body <- result.try(
        case effective_body_template_path(route, repository) {
          Some(template_path) -> {
            use body_template <- result.try(resolve_body_template(
              template_path,
              body_templates,
            ))
            planner_support.render_template(body_template, locals)
            |> result.map_error(template_error_to_planner_error)
            |> result.map(Some)
          }
          None -> Ok(None)
        },
      )
      Ok(PlannedPullRequest(
        True,
        repository.pull_request_draft,
        rendered_title,
        rendered_body,
      ))
    }
  }
}

fn resolve_body_template(
  template_path: String,
  body_templates: Dict(String, String),
) -> Result(String, PlannerError) {
  case dict.get(body_templates, template_path) {
    Ok(contents) -> Ok(contents)
    Error(Nil) ->
      error(
        "missing_body_template",
        "publication body template was not provided: " <> template_path,
      )
  }
}

fn render_file_path(
  path_template: String,
  selected: SelectedArtifact,
  repository: ResolvedGithubRepository,
  publication_id: String,
  series_id: String,
  version_id: String,
  workflow_id: String,
  run_id: String,
  work: PublicationWork,
) -> Result(String, PlannerError) {
  let locals =
    list.append(
      base_template_locals(
        publication_id,
        series_id,
        version_id,
        workflow_id,
        run_id,
        work,
        repository,
      ),
      artifact_template_locals(selected),
    )
  use rendered <- result.try(
    planner_support.render_template(path_template, locals)
    |> result.map_error(template_error_to_planner_error),
  )
  use Nil <- result.try(
    planner_support.validate_relative_path(rendered)
    |> result.map_error(validation_error_to_planner_error),
  )
  Ok(rendered)
}

fn effective_title_template(
  route: artifact_publication_config.PublicationRoute,
  repository: ResolvedGithubRepository,
) -> Option(String) {
  case route.pull_request {
    Some(artifact_publication_config.PublicationPullRequestOverride(
      title: Some(title),
      ..,
    )) -> Some(title)
    _ -> repository.pull_request_title
  }
}

fn effective_body_template_path(
  route: artifact_publication_config.PublicationRoute,
  repository: ResolvedGithubRepository,
) -> Option(String) {
  case route.pull_request {
    Some(artifact_publication_config.PublicationPullRequestOverride(
      body_template: Some(path),
      ..,
    )) -> Some(path)
    _ -> repository.pull_request_body_template
  }
}

fn template_error_to_planner_error(
  template_error: error.TemplateError,
) -> PlannerError {
  case template_error {
    error.TemplateRenderError(message) ->
      PlannerError("template_render_failure", message)
  }
}

fn base_template_locals(
  publication_id: String,
  series_id: String,
  version_id: String,
  workflow_id: String,
  run_id: String,
  work: PublicationWork,
  repository: ResolvedGithubRepository,
) -> List(#(String, template.Value)) {
  [
    #("work.kind", template.VString(work_kind_to_string(work.kind))),
    #("work.id", template.VString(work.id)),
    #("work.identifier", template.VString(work.identifier)),
    #("work.slug", template.VString(work.slug)),
    #("workflow.id", template.VString(workflow_id)),
    #("run.id", template.VString(run_id)),
    #("publication.id", template.VString(publication_id)),
    #("publication.series_id", template.VString(series_id)),
    #("publication.version_id", template.VString(version_id)),
    #("repository.kind", template.VString("github")),
    #("repository.id", template.VString(repository.id)),
    #("github.repo", template.VString(repository.repo)),
    #("github.base", template.VString(repository.base)),
  ]
}

fn artifact_template_locals(
  selected: SelectedArtifact,
) -> List(#(String, template.Value)) {
  list.append(
    [
      #("artifact.output", template.VString(selected.output)),
      #(
        "artifact.entry",
        planner_support.option_string_to_template_value(selected.entry),
      ),
      #("artifact.name", template.VString(selected.name)),
      #("artifact.ref", template.VString(selected.ref)),
      #("artifact.media_type", template.VString(selected.media_type)),
      #(
        "artifact.artifact_type",
        planner_support.option_string_to_template_value(selected.artifact_type),
      ),
      #("artifact.sha256", template.VString(selected.sha256)),
      #(
        "artifact.sha256_short",
        template.VString(string.slice(selected.sha256, 0, 12)),
      ),
      #(
        "artifact.default_extension",
        template.VString(planner_support.default_extension(selected.media_type)),
      ),
    ],
    planner_support.json_value_string_leaf_template_locals(
      "artifact.metadata",
      selected.metadata,
    ),
  )
}

fn work_kind_to_string(kind: WorkKind) -> String {
  case kind {
    TaskWork -> "task"
    ScheduledWork -> "scheduled"
  }
}

fn validation_error_to_planner_error(pair: #(String, String)) -> PlannerError {
  let #(code, message) = pair
  PlannerError(code, message)
}

fn selected_for_version_json(selected: SelectedArtifact) -> json.Json {
  selected_to_json(selected, include_ref: False)
}

fn selected_to_json(
  selected: SelectedArtifact,
  include_ref include_ref: Bool,
) -> json.Json {
  let ref_field = case include_ref {
    True -> [#("ref", json.string(selected.ref))]
    False -> []
  }
  list.append(ref_field, [
    #("output", json.string(selected.output)),
    #("entry", planner_support.option_string_to_json(selected.entry)),
    #("name", json.string(selected.name)),
    #(
      "artifact_type",
      planner_support.option_string_to_json(selected.artifact_type),
    ),
    #("metadata", case selected.metadata {
      Some(metadata) -> json_value.to_json(metadata)
      None -> json.null()
    }),
    #("sha256", json.string(selected.sha256)),
    #("bytes", json.int(selected.bytes)),
    #("media_type", json.string(selected.media_type)),
  ])
  |> json.object
}

fn planned_file_to_json(file: PlannedPublicationFile) -> json.Json {
  let PlannedPublicationFile(source, destination_path) = file
  json.object([
    #("source", selected_to_json(source, include_ref: True)),
    #("destination_path", json.string(destination_path)),
  ])
}

fn pull_request_to_json(pull_request: PlannedPullRequest) -> json.Json {
  json.object([
    #("enabled", json.bool(pull_request.enabled)),
    #("draft", json.bool(pull_request.draft)),
    #("title", planner_support.option_string_to_json(pull_request.title)),
    #("body", planner_support.option_string_to_json(pull_request.body)),
  ])
}

fn require_option(
  value: Option(a),
  code: String,
  text: String,
) -> Result(a, PlannerError) {
  case value {
    Some(value) -> Ok(value)
    None -> error(code, text)
  }
}

fn error(code: String, message: String) -> Result(a, PlannerError) {
  Error(PlannerError(code:, message:))
}
