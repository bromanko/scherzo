import gleam/bit_array
import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/artifact_publication_config
import scherzo/artifact_publication_manifest
import scherzo/artifact_publication_planner
import scherzo/path
import scherzo/state/artifact_store
import scherzo/state/record
import scherzo/tracker/issue as tracker_issue
import scherzo/workflow_checkpoint
import scherzo/workflow_contract_manifest
import simplifile

pub type PublicationFailure {
  PublicationFailure(
    publication_id: String,
    code: String,
    message: String,
    required: Bool,
  )
}

pub type PublicationAttemptSummary {
  PublicationAttemptSummary(
    publication_id: String,
    series_id: String,
    attempt_id: String,
    status: String,
    required: Bool,
    retryable: Bool,
    retry_execution_available: Bool,
    version_id: Option(String),
    manifest_ref: String,
    manifest_sha256: String,
    manifest_bytes: Int,
    error_code: Option(String),
    error_message: Option(String),
  )
}

pub type PublicationRecordingResult {
  PublicationRecordingResult(
    required_failures: List(PublicationFailure),
    optional_failures: List(PublicationFailure),
    attempts: List(PublicationAttemptSummary),
  )
}

pub fn record_routes(
  routes: List(artifact_publication_config.PublicationRoute),
  repositories: artifact_publication_config.ArtifactRepositories,
  config_dir: String,
  workflow_bundle_dir: String,
  output_manifest: workflow_contract_manifest.ContractOutputManifest,
  issue: tracker_issue.Issue,
  run_id: String,
  checkpoint: workflow_checkpoint.Writer,
) -> Result(PublicationRecordingResult, String) {
  let work = publication_work(issue)
  record_routes_loop(
    routes,
    repositories,
    config_dir,
    workflow_bundle_dir,
    output_manifest,
    work,
    run_id,
    checkpoint,
    [],
    [],
    [],
  )
  |> result.map(fn(result) {
    let PublicationRecordingResult(
      required_failures,
      optional_failures,
      attempts,
    ) = result
    PublicationRecordingResult(
      required_failures: list.reverse(required_failures),
      optional_failures: list.reverse(optional_failures),
      attempts: list.reverse(attempts),
    )
  })
}

fn record_routes_loop(
  routes: List(artifact_publication_config.PublicationRoute),
  repositories: artifact_publication_config.ArtifactRepositories,
  config_dir: String,
  workflow_bundle_dir: String,
  output_manifest: workflow_contract_manifest.ContractOutputManifest,
  work: artifact_publication_planner.PublicationWork,
  run_id: String,
  checkpoint: workflow_checkpoint.Writer,
  required_failures: List(PublicationFailure),
  optional_failures: List(PublicationFailure),
  attempts: List(PublicationAttemptSummary),
) -> Result(PublicationRecordingResult, String) {
  case routes {
    [] ->
      Ok(PublicationRecordingResult(
        required_failures,
        optional_failures,
        attempts,
      ))
    [route, ..rest] -> {
      use outcome <- result.try(record_route(
        route,
        repositories,
        config_dir,
        workflow_bundle_dir,
        output_manifest,
        work,
        run_id,
        checkpoint,
      ))
      case outcome {
        RouteRecorded(attempt) ->
          record_routes_loop(
            rest,
            repositories,
            config_dir,
            workflow_bundle_dir,
            output_manifest,
            work,
            run_id,
            checkpoint,
            required_failures,
            optional_failures,
            [attempt, ..attempts],
          )
        RouteFailed(failure, attempt) ->
          case failure.required {
            True ->
              record_routes_loop(
                rest,
                repositories,
                config_dir,
                workflow_bundle_dir,
                output_manifest,
                work,
                run_id,
                checkpoint,
                [failure, ..required_failures],
                optional_failures,
                [attempt, ..attempts],
              )
            False ->
              record_routes_loop(
                rest,
                repositories,
                config_dir,
                workflow_bundle_dir,
                output_manifest,
                work,
                run_id,
                checkpoint,
                required_failures,
                [failure, ..optional_failures],
                [attempt, ..attempts],
              )
          }
      }
    }
  }
}

type RouteRecordingOutcome {
  RouteRecorded(PublicationAttemptSummary)
  RouteFailed(PublicationFailure, PublicationAttemptSummary)
}

fn record_route(
  route: artifact_publication_config.PublicationRoute,
  repositories: artifact_publication_config.ArtifactRepositories,
  config_dir: String,
  workflow_bundle_dir: String,
  output_manifest: workflow_contract_manifest.ContractOutputManifest,
  work: artifact_publication_planner.PublicationWork,
  run_id: String,
  checkpoint: workflow_checkpoint.Writer,
) -> Result(RouteRecordingOutcome, String) {
  case
    load_body_templates([route], repositories, config_dir, workflow_bundle_dir)
  {
    Ok(body_templates) ->
      record_route_with_templates(
        route,
        repositories,
        output_manifest,
        work,
        run_id,
        checkpoint,
        body_templates,
      )
    Error(reason) ->
      record_route_failure(
        route,
        output_manifest.workflow_id,
        work,
        run_id,
        checkpoint,
        failure_code(reason),
        failure_message(reason),
      )
  }
}

fn record_route_with_templates(
  route: artifact_publication_config.PublicationRoute,
  repositories: artifact_publication_config.ArtifactRepositories,
  output_manifest: workflow_contract_manifest.ContractOutputManifest,
  work: artifact_publication_planner.PublicationWork,
  run_id: String,
  checkpoint: workflow_checkpoint.Writer,
  body_templates: dict.Dict(String, String),
) -> Result(RouteRecordingOutcome, String) {
  case
    artifact_publication_planner.plan_publication(
      output_manifest,
      repositories,
      route,
      checkpoint_store(checkpoint),
      work,
      run_id,
      body_templates,
    )
  {
    Ok(planned) -> {
      let attempt_key =
        artifact_publication_manifest.attempt_key_for_success(
          planned.version_id,
        )
      let manifest =
        artifact_publication_manifest.planned_manifest(
          planned,
          attempt_key,
          checkpoint.now_ms(),
        )
      use attempt <- result.try(write_attempt(
        route,
        output_manifest.workflow_id,
        run_id,
        planned.series_id,
        attempt_key,
        manifest,
        False,
        None,
        None,
        checkpoint,
      ))
      Ok(RouteRecorded(attempt))
    }
    Error(planner_error) -> {
      let code = artifact_publication_planner.code(planner_error)
      let artifact_publication_planner.PlannerError(message: message, ..) =
        planner_error
      record_route_failure(
        route,
        output_manifest.workflow_id,
        work,
        run_id,
        checkpoint,
        code,
        message,
      )
    }
  }
}

pub fn record_failed_attempt(
  route: artifact_publication_config.PublicationRoute,
  workflow_id: String,
  work: artifact_publication_planner.PublicationWork,
  run_id: String,
  checkpoint: workflow_checkpoint.Writer,
  code: String,
  message: String,
) -> Result(#(PublicationFailure, PublicationAttemptSummary), String) {
  let series_id = make_series_id(work.id, workflow_id, route.id)
  let generated_at_ms = checkpoint.now_ms()
  let attempt_key =
    artifact_publication_manifest.attempt_key_for_failure(
      route.id,
      code,
      message,
      generated_at_ms,
    )
  let manifest =
    artifact_publication_manifest.failed_manifest(
      run_id,
      workflow_id,
      route.id,
      series_id,
      route.required,
      attempt_key,
      generated_at_ms,
      artifact_publication_manifest.PublicationErrorInfo(
        code: code,
        message: message,
      ),
    )
  use attempt <- result.try(write_attempt(
    route,
    workflow_id,
    run_id,
    series_id,
    attempt_key,
    manifest,
    True,
    Some(code),
    Some(message),
    checkpoint,
  ))
  Ok(#(PublicationFailure(route.id, code, message, route.required), attempt))
}

fn record_route_failure(
  route: artifact_publication_config.PublicationRoute,
  workflow_id: String,
  work: artifact_publication_planner.PublicationWork,
  run_id: String,
  checkpoint: workflow_checkpoint.Writer,
  code: String,
  message: String,
) -> Result(RouteRecordingOutcome, String) {
  use #(failure, attempt) <- result.try(record_failed_attempt(
    route,
    workflow_id,
    work,
    run_id,
    checkpoint,
    code,
    message,
  ))
  Ok(RouteFailed(failure, attempt))
}

fn failure_code(reason: String) -> String {
  case string.split_once(reason, on: ":") {
    Ok(#(code, _)) -> code
    Error(_) -> reason
  }
}

fn failure_message(reason: String) -> String {
  case string.split_once(reason, on: ":") {
    Ok(#(_, message)) -> string.trim(message)
    Error(_) -> reason
  }
}

pub fn record_planned_attempt(
  route: artifact_publication_config.PublicationRoute,
  workflow_id: String,
  run_id: String,
  planned: artifact_publication_planner.DryRunPublicationManifest,
  checkpoint: workflow_checkpoint.Writer,
) -> Result(PublicationAttemptSummary, String) {
  let attempt_key =
    artifact_publication_manifest.attempt_key_for_success(planned.version_id)
  let manifest =
    artifact_publication_manifest.planned_manifest(
      planned,
      attempt_key,
      checkpoint.now_ms(),
    )
  write_attempt(
    route,
    workflow_id,
    run_id,
    planned.series_id,
    attempt_key,
    manifest,
    False,
    None,
    None,
    checkpoint,
  )
}

pub fn record_manifest_attempt(
  route: artifact_publication_config.PublicationRoute,
  workflow_id: String,
  run_id: String,
  manifest: artifact_publication_manifest.PublicationManifest,
  checkpoint: workflow_checkpoint.Writer,
) -> Result(PublicationAttemptSummary, String) {
  let error_code = case manifest.error {
    Some(artifact_publication_manifest.PublicationErrorInfo(code, _)) ->
      Some(code)
    None -> None
  }
  let error_message = case manifest.error {
    Some(artifact_publication_manifest.PublicationErrorInfo(_, message)) ->
      Some(message)
    None -> None
  }
  write_attempt(
    route,
    workflow_id,
    run_id,
    manifest.series_id,
    manifest.attempt_id,
    manifest,
    manifest.retryable,
    error_code,
    error_message,
    checkpoint,
  )
}

fn write_attempt(
  route: artifact_publication_config.PublicationRoute,
  workflow_id: String,
  run_id: String,
  series_id: String,
  attempt_key: String,
  manifest: artifact_publication_manifest.PublicationManifest,
  retryable: Bool,
  error_code: Option(String),
  error_message: Option(String),
  checkpoint: workflow_checkpoint.Writer,
) -> Result(PublicationAttemptSummary, String) {
  let payload_json = artifact_publication_manifest.to_string(manifest)
  use written <- result.try(
    checkpoint.write_publication_manifest(
      workflow_checkpoint.WorkflowPublicationManifestWrite(
        run_id: run_id,
        publication_id: route.id,
        attempt_key: attempt_key,
        payload_json: payload_json,
      ),
    )
    |> result.map_error(workflow_checkpoint.describe_error),
  )
  let ledger_record =
    record.with_id(
      publication_record_id(run_id, route.id, attempt_key),
      checkpoint.now_ms(),
      record.PublicationAttemptRecorded(
        run_id: run_id,
        workflow_id: workflow_id,
        publication_id: route.id,
        series_id: series_id,
        attempt_id: attempt_key,
        status: artifact_publication_manifest.status_to_string(manifest.status),
        required: route.required,
        retryable: retryable,
        retry_execution_available: manifest.retry_execution_available,
        version_id: manifest.version_id,
        manifest_ref: Some(written.ref),
        manifest_sha256: Some(written.sha256),
        manifest_bytes: Some(written.bytes),
        error_code: error_code,
        error_message: error_message,
      ),
    )
  use _ <- result.try(
    checkpoint.publication_attempt_recorded(ledger_record)
    |> result.map_error(workflow_checkpoint.describe_error),
  )
  Ok(PublicationAttemptSummary(
    publication_id: route.id,
    series_id: series_id,
    attempt_id: attempt_key,
    status: artifact_publication_manifest.status_to_string(manifest.status),
    required: route.required,
    retryable: retryable,
    retry_execution_available: manifest.retry_execution_available,
    version_id: manifest.version_id,
    manifest_ref: written.ref,
    manifest_sha256: written.sha256,
    manifest_bytes: written.bytes,
    error_code: error_code,
    error_message: error_message,
  ))
}

fn publication_record_id(
  run_id: String,
  publication_id: String,
  attempt_key: String,
) -> String {
  "publication_attempt:"
  <> run_id
  <> ":"
  <> publication_id
  <> ":"
  <> attempt_key
}

pub fn publication_work(
  issue: tracker_issue.Issue,
) -> artifact_publication_planner.PublicationWork {
  artifact_publication_planner.PublicationWork(
    kind: artifact_publication_planner.TaskWork,
    id: issue.id,
    identifier: issue.identifier,
    slug: issue.identifier,
  )
}

pub fn load_body_templates(
  routes: List(artifact_publication_config.PublicationRoute),
  repositories: artifact_publication_config.ArtifactRepositories,
  config_dir: String,
  workflow_bundle_dir: String,
) -> Result(dict.Dict(String, String), String) {
  let paths = body_template_paths(routes, repositories, [])
  let config_root = path.absolute_or_original(config_dir)
  let workflow_root = case string.trim(workflow_bundle_dir) {
    "" -> config_root
    _ -> path.absolute_or_original(workflow_bundle_dir)
  }
  load_body_template_paths(paths, config_root, workflow_root, dict.new())
  |> result.map(loaded_body_templates_to_dict)
}

type BodyTemplatePath {
  RouteBodyTemplate(String)
  RepositoryBodyTemplate(String)
}

type LoadedBodyTemplate {
  LoadedBodyTemplate(root: String, contents: String)
}

fn loaded_body_templates_to_dict(
  loaded: dict.Dict(String, LoadedBodyTemplate),
) -> dict.Dict(String, String) {
  loaded
  |> dict.to_list
  |> list.map(fn(entry) {
    let #(template_path, LoadedBodyTemplate(contents: contents, ..)) = entry
    #(template_path, contents)
  })
  |> dict.from_list
}

fn load_body_template_paths(
  paths: List(BodyTemplatePath),
  config_root: String,
  workflow_root: String,
  loaded: dict.Dict(String, LoadedBodyTemplate),
) -> Result(dict.Dict(String, LoadedBodyTemplate), String) {
  case paths {
    [] -> Ok(loaded)
    [body_template_path, ..rest] -> {
      let #(template_path, root) = case body_template_path {
        RouteBodyTemplate(template_path) -> #(template_path, workflow_root)
        RepositoryBodyTemplate(template_path) -> #(template_path, config_root)
      }
      case dict.get(loaded, template_path) {
        Ok(LoadedBodyTemplate(root: existing_root, contents: existing_contents)) ->
          case existing_root == root {
            True ->
              load_body_template_paths(rest, config_root, workflow_root, loaded)
            False -> {
              use contents <- result.try(read_body_template(root, template_path))
              case contents == existing_contents {
                True ->
                  load_body_template_paths(
                    rest,
                    config_root,
                    workflow_root,
                    loaded,
                  )
                False ->
                  Error(
                    "conflicting_publication_body_template_roots:"
                    <> template_path,
                  )
              }
            }
          }
        Error(Nil) -> {
          use contents <- result.try(read_body_template(root, template_path))
          load_body_template_paths(
            rest,
            config_root,
            workflow_root,
            dict.insert(
              loaded,
              template_path,
              LoadedBodyTemplate(root: root, contents: contents),
            ),
          )
        }
      }
    }
  }
}

fn read_body_template(
  root: String,
  template_path: String,
) -> Result(String, String) {
  use absolute <- result.try(resolve_template_path(root, template_path))
  simplifile.read(absolute)
  |> result.replace_error(
    "publication_body_template_read_failed:" <> template_path,
  )
}

fn resolve_template_path(
  root: String,
  template_path: String,
) -> Result(String, String) {
  case
    template_path == ""
    || path.is_absolute(template_path)
    || path.has_parent_segment(template_path)
  {
    True -> Error("invalid_publication_body_template_path:" <> template_path)
    False -> {
      let joined = path.join(root, template_path)
      let absolute = path.absolute_or_original(joined)
      case path.contains(root, absolute) {
        True -> Ok(absolute)
        False ->
          Error("invalid_publication_body_template_path:" <> template_path)
      }
    }
  }
}

fn body_template_paths(
  routes: List(artifact_publication_config.PublicationRoute),
  repositories: artifact_publication_config.ArtifactRepositories,
  acc: List(BodyTemplatePath),
) -> List(BodyTemplatePath) {
  case routes {
    [] -> acc
    [route, ..rest] -> {
      let next = case route.target {
        artifact_publication_config.ExistingPrBranchTarget(_) -> acc
        artifact_publication_config.StableBranchTarget ->
          case route.pull_request {
            Some(artifact_publication_config.PublicationPullRequestOverride(
              body_template: Some(body_template),
              ..,
            )) -> [RouteBodyTemplate(body_template), ..acc]
            _ -> repository_body_template(route.repository, repositories, acc)
          }
      }
      body_template_paths(rest, repositories, next)
    }
  }
}

fn repository_body_template(
  repository_ref: String,
  repositories: artifact_publication_config.ArtifactRepositories,
  acc: List(BodyTemplatePath),
) -> List(BodyTemplatePath) {
  case
    artifact_publication_config.repository_ref_parts(
      repository_ref,
      "publication.repository",
    )
  {
    Ok(#("github", name)) ->
      case dict.get(repositories.github, name) {
        Ok(target) ->
          case target.pull_request.body_template {
            Some(body_template) -> [
              RepositoryBodyTemplate(body_template),
              ..acc
            ]
            None -> acc
          }
        Error(_) -> acc
      }
    _ -> acc
  }
}

fn make_series_id(
  work_id: String,
  workflow_id: String,
  publication_id: String,
) -> String {
  work_id <> ":" <> workflow_id <> ":" <> publication_id
}

fn read_checkpoint_artifact_bytes(
  checkpoint: workflow_checkpoint.Writer,
  ref: String,
) -> Result(BitArray, artifact_store.ArtifactError) {
  case checkpoint.artifact_location(ref) {
    Ok(artifact_store.ArtifactLocation(local_path: Some(local_path), ..)) ->
      artifact_store.read_file_bytes(local_path)
    Ok(_) -> read_checkpoint_artifact_text_as_bytes(checkpoint, ref)
    Error(error) ->
      Error(
        artifact_store.ArtifactIo(workflow_checkpoint.describe_error(error)),
      )
  }
}

fn read_checkpoint_artifact_text_as_bytes(
  checkpoint: workflow_checkpoint.Writer,
  ref: String,
) -> Result(BitArray, artifact_store.ArtifactError) {
  checkpoint.read_artifact(ref)
  |> result.map(bit_array.from_string)
  |> result.map_error(fn(error) {
    artifact_store.ArtifactIo(workflow_checkpoint.describe_error(error))
  })
}

fn checkpoint_store(
  checkpoint: workflow_checkpoint.Writer,
) -> artifact_store.Store {
  artifact_store.custom(
    "workflow-checkpoint",
    artifact_store.StoreCallbacks(
      write: fn(_, _) {
        Error(artifact_store.ArtifactIo(
          "publication_recording_write_unsupported",
        ))
      },
      read: fn(ref) {
        checkpoint.read_artifact(ref)
        |> result.map_error(fn(error) {
          artifact_store.ArtifactIo(workflow_checkpoint.describe_error(error))
        })
      },
      write_bytes: fn(_, _) {
        Error(artifact_store.ArtifactIo(
          "publication_recording_write_unsupported",
        ))
      },
      write_immutable_bytes: fn(_, _) {
        Error(artifact_store.ArtifactIo(
          "publication_recording_write_unsupported",
        ))
      },
      read_bytes: fn(ref) { read_checkpoint_artifact_bytes(checkpoint, ref) },
      locate: fn(ref) {
        checkpoint.artifact_location(ref)
        |> result.map_error(fn(error) {
          artifact_store.ArtifactIo(workflow_checkpoint.describe_error(error))
        })
      },
    ),
  )
}
