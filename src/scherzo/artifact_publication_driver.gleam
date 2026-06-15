import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/artifact_publication_manifest
import scherzo/artifact_publication_planner
import scherzo/artifact_publication_pr_text
import scherzo/artifact_repository/command_runner
import scherzo/config/types as config_types
import scherzo/hash
import scherzo/json_value
import scherzo/log
import scherzo/path
import simplifile

pub type WorkspacePublicationDriver {
  WorkspacePublicationDriver(
    workspace_path: String,
    command: String,
    capabilities: List(config_types.WorkspaceCapability),
    env: List(#(String, String)),
    redaction_values: List(String),
    timeout_ms: Int,
  )
}

pub type DriverPublicationResult {
  DriverPublicationResult(
    status: String,
    url: Option(String),
    branch: String,
    base_ref: String,
    base_revision: String,
    head_revision: String,
    change_id: Option(String),
  )
}

type DriverMetadataFiles {
  DriverMetadataFiles(title_file: String, body_file: String)
}

pub fn publish_commit_stack(
  planned: artifact_publication_planner.DryRunPublicationManifest,
  publication_driver: Option(WorkspacePublicationDriver),
  runner: command_runner.Runner,
) -> Result(
  DriverPublicationResult,
  artifact_publication_manifest.PublicationErrorInfo,
) {
  use driver <- result.try(require_publication_driver(publication_driver))
  use operation <- result.try(driver_publish_operation(driver))
  use _ <- result.try(require_workspace_directory(driver.workspace_path))
  use base <- result.try(driver_base_ref(planned))
  use metadata <- result.try(write_driver_metadata_files(
    driver.workspace_path,
    planned,
  ))
  let args = driver_publish_args(operation, planned, metadata, base)
  let spec =
    command_runner.sh(driver.command, args, driver.workspace_path)
    |> command_runner.with_env(driver_publish_env(planned, driver.env))
    |> command_runner.with_timeout_ms(driver.timeout_ms)
  case runner.run(spec) {
    Error(error) ->
      Error(publication_error_info(
        "workspace_driver_publish_failed",
        "could not run workspace driver "
          <> operation
          <> ": "
          <> command_runner.error_message(error),
        driver.redaction_values,
      ))
    Ok(output) ->
      case output.exit_code == 0 {
        True ->
          parse_and_verify_driver_publish_success(
            operation,
            planned,
            output.stdout,
            driver.redaction_values,
          )
        False ->
          Error(parse_driver_publish_failure(
            operation,
            output,
            driver.redaction_values,
          ))
      }
  }
}

fn require_publication_driver(
  publication_driver: Option(WorkspacePublicationDriver),
) -> Result(
  WorkspacePublicationDriver,
  artifact_publication_manifest.PublicationErrorInfo,
) {
  case publication_driver {
    Some(driver) ->
      case string.trim(driver.command) == "" {
        True ->
          Error(artifact_publication_manifest.PublicationErrorInfo(
            code: "commit_stack_publication_driver_unavailable",
            message: "same-repo commit_stack publication requires a workspace driver command",
          ))
        False -> Ok(driver)
      }
    None ->
      Error(artifact_publication_manifest.PublicationErrorInfo(
        code: "commit_stack_publication_driver_unavailable",
        message: "same-repo commit_stack publication requires a workspace driver with publish-commit-stack",
      ))
  }
}

fn driver_publish_operation(
  driver: WorkspacePublicationDriver,
) -> Result(String, artifact_publication_manifest.PublicationErrorInfo) {
  case list.contains(driver.capabilities, config_types.WorkspacePublishChange) {
    True ->
      Error(artifact_publication_manifest.PublicationErrorInfo(
        code: "legacy_publish_change_unsupported",
        message: config_types.legacy_publish_change_migration_message(
          "workspace driver for same-repo commit_stack publication",
        ),
      ))
    False ->
      case
        list.contains(
          driver.capabilities,
          config_types.WorkspacePublishCommitStack,
        )
      {
        True -> Ok("publish-commit-stack")
        False ->
          Error(artifact_publication_manifest.PublicationErrorInfo(
            code: "commit_stack_publication_driver_unsupported",
            message: "workspace driver does not advertise publish-commit-stack",
          ))
      }
  }
}

fn require_workspace_directory(
  workspace_path: String,
) -> Result(Nil, artifact_publication_manifest.PublicationErrorInfo) {
  case simplifile.is_directory(workspace_path) {
    Ok(True) -> Ok(Nil)
    Ok(False) ->
      Error(artifact_publication_manifest.PublicationErrorInfo(
        code: "commit_stack_publication_workspace_unavailable",
        message: "retained workflow workspace is missing: " <> workspace_path,
      ))
    Error(error) ->
      Error(artifact_publication_manifest.PublicationErrorInfo(
        code: "commit_stack_publication_workspace_unavailable",
        message: "could not inspect retained workflow workspace: "
          <> workspace_path
          <> " ("
          <> simplifile.describe_error(error)
          <> ")",
      ))
  }
}

fn write_driver_metadata_files(
  workspace_path: String,
  planned: artifact_publication_planner.DryRunPublicationManifest,
) -> Result(
  DriverMetadataFiles,
  artifact_publication_manifest.PublicationErrorInfo,
) {
  let relative_dir =
    path.join(
      "tmp/scherzo-publication",
      hash.sha256_hex(planned.publication_id <> "|" <> planned.version_id),
    )
  let absolute_dir = path.join(workspace_path, relative_dir)
  let title_file = path.join(relative_dir, "title.txt")
  let body_file = path.join(relative_dir, "body.md")
  case simplifile.create_directory_all(absolute_dir) {
    Error(error) ->
      metadata_write_error(
        "could not create publication metadata directory: "
        <> simplifile.describe_error(error),
      )
    Ok(Nil) ->
      case
        simplifile.write(
          path.join(workspace_path, title_file),
          driver_pr_title(planned),
        )
      {
        Error(error) ->
          metadata_write_error(
            "could not write publication title file: "
            <> simplifile.describe_error(error),
          )
        Ok(Nil) ->
          case
            simplifile.write(
              path.join(workspace_path, body_file),
              driver_pr_body(planned),
            )
          {
            Error(error) ->
              metadata_write_error(
                "could not write publication body file: "
                <> simplifile.describe_error(error),
              )
            Ok(Nil) -> Ok(DriverMetadataFiles(title_file, body_file))
          }
      }
  }
}

fn metadata_write_error(
  message: String,
) -> Result(a, artifact_publication_manifest.PublicationErrorInfo) {
  Error(artifact_publication_manifest.PublicationErrorInfo(
    code: "workspace_driver_publish_metadata_write_failed",
    message: message,
  ))
}

fn driver_publish_args(
  operation: String,
  planned: artifact_publication_planner.DryRunPublicationManifest,
  metadata: DriverMetadataFiles,
  base: String,
) -> List(String) {
  let base_args = [
    operation,
    "--kind",
    publication_kind(planned.workflow_id),
    "--title-file",
    metadata.title_file,
    "--body-file",
    metadata.body_file,
    "--branch-prefix",
    planned.branch,
    "--base",
    base,
  ]
  list.append(
    base_args,
    list.append(target_args(operation, planned.target), [
      "--allow-no-changes",
      "true",
      "--json",
    ]),
  )
}

fn target_args(
  operation: String,
  target: artifact_publication_planner.PlannedPublicationTarget,
) -> List(String) {
  case target {
    artifact_publication_planner.ExistingPrBranchTargetPlan(existing) -> {
      let branch_args = ["--target-branch", existing.head_branch]
      let pr_args = case existing.pr_number > 0 {
        True -> ["--target-pr", int.to_string(existing.pr_number)]
        False -> []
      }
      let expected_args = case operation == "publish-commit-stack" {
        True -> ["--expected-head", existing.expected_head_sha]
        False -> []
      }
      list.append(branch_args, list.append(pr_args, expected_args))
    }
    artifact_publication_planner.StableBranchTargetPlan -> []
  }
}

fn driver_base_ref(
  planned: artifact_publication_planner.DryRunPublicationManifest,
) -> Result(String, artifact_publication_manifest.PublicationErrorInfo) {
  case planned.target {
    artifact_publication_planner.ExistingPrBranchTargetPlan(existing) ->
      Ok(existing.base_branch)
    artifact_publication_planner.StableBranchTargetPlan ->
      case planned.github_base {
        Some(base) -> Ok(base)
        None ->
          Error(artifact_publication_manifest.PublicationErrorInfo(
            code: "workspace_driver_publish_missing_base",
            message: "same-repo commit_stack publication is missing a base branch",
          ))
      }
  }
}

fn driver_publish_env(
  planned: artifact_publication_planner.DryRunPublicationManifest,
  env: List(#(String, String)),
) -> List(#(String, String)) {
  [#("SCHERZO_PR_DRAFT", bool_string(planned.pull_request.draft)), ..env]
}

fn bool_string(value: Bool) -> String {
  case value {
    True -> "true"
    False -> "false"
  }
}

fn publication_kind(workflow_id: String) -> String {
  let segments =
    string.split(workflow_id, on: ".")
    |> list.filter(fn(segment) { string.trim(segment) != "" })
  let segment = case list.reverse(segments) {
    [last, ..] -> last
    [] -> workflow_id
  }
  let normalized =
    segment
    |> string.lowercase
    |> string.replace(each: "_", with: "-")
  case string.trim(normalized) == "" {
    True -> "publication"
    False -> normalized
  }
}

fn driver_pr_title(
  planned: artifact_publication_planner.DryRunPublicationManifest,
) -> String {
  artifact_publication_pr_text.title(planned)
  |> string.trim
  |> ensure_trailing_newline
}

fn driver_pr_body(
  planned: artifact_publication_planner.DryRunPublicationManifest,
) -> String {
  artifact_publication_pr_text.body(planned)
  |> ensure_trailing_newline
}

fn ensure_trailing_newline(value: String) -> String {
  case string.ends_with(value, "\n") {
    True -> value
    False -> value <> "\n"
  }
}

fn parse_and_verify_driver_publish_success(
  operation: String,
  planned: artifact_publication_planner.DryRunPublicationManifest,
  stdout: String,
  redaction_values: List(String),
) -> Result(
  DriverPublicationResult,
  artifact_publication_manifest.PublicationErrorInfo,
) {
  use driver_result <- result.try(parse_driver_publish_success(
    operation,
    planned,
    stdout,
    redaction_values,
  ))
  verify_driver_result_matches_commit_stack(
    operation,
    planned,
    driver_result,
    redaction_values,
  )
}

fn parse_driver_publish_success(
  operation: String,
  planned: artifact_publication_planner.DryRunPublicationManifest,
  stdout: String,
  redaction_values: List(String),
) -> Result(
  DriverPublicationResult,
  artifact_publication_manifest.PublicationErrorInfo,
) {
  use entries <- result.try(parse_driver_json_object(operation, stdout))
  use _ <- result.try(require_driver_version(operation, entries))
  use status <- result.try(required_json_string(operation, entries, "status"))
  case list.contains(["published", "updated", "unchanged"], status) {
    False ->
      Error(unsuccessful_driver_status(entries, status, redaction_values))
    True -> {
      use branch <- result.try(required_json_string(
        operation,
        entries,
        "branch",
      ))
      use base_ref <- result.try(required_json_string(
        operation,
        entries,
        "base_ref",
      ))
      use base_revision <- result.try(required_json_string(
        operation,
        entries,
        "base_revision",
      ))
      use head_revision <- result.try(required_json_string(
        operation,
        entries,
        "head_revision",
      ))
      use _created <- result.try(required_json_bool(
        operation,
        entries,
        "created",
      ))
      use _updated <- result.try(required_json_bool(
        operation,
        entries,
        "updated",
      ))
      let url = optional_json_string(entries, "url")
      case status_requires_url(status, planned), url {
        True, None ->
          Error(malformed_driver_output(
            operation,
            "successful status " <> status <> " requires a non-empty url",
          ))
        _, _ ->
          Ok(DriverPublicationResult(
            status: status,
            url: url,
            branch: branch,
            base_ref: base_ref,
            base_revision: base_revision,
            head_revision: head_revision,
            change_id: optional_json_string(entries, "change_id"),
          ))
      }
    }
  }
}

fn status_requires_url(
  status: String,
  planned: artifact_publication_planner.DryRunPublicationManifest,
) -> Bool {
  case status {
    "published" -> True
    "updated" ->
      planned.pull_request.enabled || existing_target_has_pr(planned.target)
    _ -> False
  }
}

fn existing_target_has_pr(
  target: artifact_publication_planner.PlannedPublicationTarget,
) -> Bool {
  case target {
    artifact_publication_planner.ExistingPrBranchTargetPlan(existing) ->
      existing.pr_number > 0
    artifact_publication_planner.StableBranchTargetPlan -> False
  }
}

fn parse_driver_publish_failure(
  operation: String,
  output: command_runner.CommandOutput,
  redaction_values: List(String),
) -> artifact_publication_manifest.PublicationErrorInfo {
  case parse_driver_failure_json(output.stdout, redaction_values) {
    Some(error) -> error
    None -> {
      let summary =
        command_runner.summarize(output)
        |> redact_driver_text(redaction_values)
        |> log.truncate(1000)
      publication_error_info(
        "workspace_driver_publish_failed",
        "workspace driver " <> operation <> " failed: " <> summary,
        redaction_values,
      )
    }
  }
}

fn parse_driver_failure_json(
  stdout: String,
  redaction_values: List(String),
) -> Option(artifact_publication_manifest.PublicationErrorInfo) {
  case json_value.parse(string.trim(stdout)) {
    Ok(json_value.JObject(entries)) ->
      case
        optional_json_string(entries, "failure_code"),
        optional_json_string(entries, "message")
      {
        Some(code), Some(message) ->
          Some(publication_error_info(code, message, redaction_values))
        _, _ -> None
      }
    _ -> None
  }
}

fn parse_driver_json_object(
  operation: String,
  stdout: String,
) -> Result(
  List(#(String, json_value.JsonValue)),
  artifact_publication_manifest.PublicationErrorInfo,
) {
  case string.trim(stdout) {
    "" -> Error(malformed_driver_output(operation, "driver stdout was empty"))
    payload ->
      case json_value.parse(payload) {
        Ok(json_value.JObject(entries)) -> Ok(entries)
        Ok(_) ->
          Error(malformed_driver_output(
            operation,
            "driver stdout must be one JSON object",
          ))
        Error(Nil) ->
          Error(malformed_driver_output(
            operation,
            "driver stdout must be valid JSON",
          ))
      }
  }
}

fn require_driver_version(
  operation: String,
  entries: List(#(String, json_value.JsonValue)),
) -> Result(Nil, artifact_publication_manifest.PublicationErrorInfo) {
  case object_json_field(entries, "version") {
    Some(json_value.JInt(1)) -> Ok(Nil)
    Some(json_value.JInt(version)) ->
      Error(malformed_driver_output(
        operation,
        "unsupported driver output version " <> int.to_string(version),
      ))
    Some(_) ->
      Error(malformed_driver_output(operation, "version must be integer 1"))
    None -> Error(malformed_driver_output(operation, "missing version field"))
  }
}

fn required_json_string(
  operation: String,
  entries: List(#(String, json_value.JsonValue)),
  field: String,
) -> Result(String, artifact_publication_manifest.PublicationErrorInfo) {
  case optional_json_string(entries, field) {
    Some(value) -> Ok(value)
    None ->
      Error(malformed_driver_output(
        operation,
        "missing or empty string field: " <> field,
      ))
  }
}

fn required_json_bool(
  operation: String,
  entries: List(#(String, json_value.JsonValue)),
  field: String,
) -> Result(Bool, artifact_publication_manifest.PublicationErrorInfo) {
  case object_json_field(entries, field) {
    Some(json_value.JBool(value)) -> Ok(value)
    Some(_) ->
      Error(malformed_driver_output(operation, field <> " must be a boolean"))
    None ->
      Error(malformed_driver_output(
        operation,
        "missing boolean field: " <> field,
      ))
  }
}

fn optional_json_string(
  entries: List(#(String, json_value.JsonValue)),
  field: String,
) -> Option(String) {
  case object_json_field(entries, field) {
    Some(json_value.JString(value)) ->
      case string.trim(value) {
        "" -> None
        trimmed -> Some(trimmed)
      }
    _ -> None
  }
}

fn unsuccessful_driver_status(
  entries: List(#(String, json_value.JsonValue)),
  status: String,
  redaction_values: List(String),
) -> artifact_publication_manifest.PublicationErrorInfo {
  let code =
    option_string_or(
      optional_json_string(entries, "failure_code"),
      "workspace_driver_publish_unsuccessful",
    )
  let message =
    option_string_or(
      optional_json_string(entries, "message"),
      "workspace driver publication returned unsuccessful status " <> status,
    )
  publication_error_info(code, message, redaction_values)
}

fn verify_driver_result_matches_commit_stack(
  operation: String,
  planned: artifact_publication_planner.DryRunPublicationManifest,
  driver_result: DriverPublicationResult,
  redaction_values: List(String),
) -> Result(
  DriverPublicationResult,
  artifact_publication_manifest.PublicationErrorInfo,
) {
  case planned.commit_stack {
    Some(stack) ->
      case driver_result.head_revision == stack.stack.head_sha {
        True -> Ok(driver_result)
        False ->
          Error(publication_error_info(
            "workspace_driver_publish_head_mismatch",
            "workspace driver "
              <> operation
              <> " returned head_revision "
              <> driver_result.head_revision
              <> " but selected commit_stack head is "
              <> stack.stack.head_sha,
            redaction_values,
          ))
      }
    None -> Ok(driver_result)
  }
}

fn publication_error_info(
  code: String,
  message: String,
  redaction_values: List(String),
) -> artifact_publication_manifest.PublicationErrorInfo {
  artifact_publication_manifest.PublicationErrorInfo(
    code: redact_driver_text(code, redaction_values),
    message: redact_driver_text(message, redaction_values),
  )
}

fn redact_driver_text(value: String, redaction_values: List(String)) -> String {
  log.redact("artifact_publication_driver", value, redaction_values)
}

fn malformed_driver_output(
  operation: String,
  message: String,
) -> artifact_publication_manifest.PublicationErrorInfo {
  artifact_publication_manifest.PublicationErrorInfo(
    code: "workspace_driver_publish_malformed",
    message: "workspace driver "
      <> operation
      <> " returned malformed output: "
      <> message,
  )
}

fn object_json_field(
  entries: List(#(String, json_value.JsonValue)),
  field: String,
) -> Option(json_value.JsonValue) {
  case entries {
    [] -> None
    [#(key, value), ..rest] ->
      case key == field {
        True -> Some(value)
        False -> object_json_field(rest, field)
      }
  }
}

fn option_string_or(value: Option(String), default: String) -> String {
  case value {
    Some(value) -> value
    None -> default
  }
}
