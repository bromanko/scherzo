import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order
import gleam/result
import gleam/string
import scherzo/json_value

pub const commit_stack_artifact_type = "scherzo.git_commit_stack.v1"

pub const commit_stack_media_type = "application/vnd.scherzo.git-commit-stack+json"

pub const bundle_media_type = "application/vnd.git.bundle"

pub const max_bundle_bytes = 104_857_600

pub const existing_pr_branch_target_artifact_type = "scherzo.github_existing_pr_branch_target.v1"

pub type CommitStackCarrier {
  CommitStackCarrier(
    ref: String,
    sha256: String,
    bytes: Int,
    media_type: String,
  )
}

pub type CommitStackArtifact {
  CommitStackArtifact(
    repository: String,
    base_ref: String,
    base_sha: String,
    head_sha: String,
    head_tree: String,
    carrier: CommitStackCarrier,
  )
}

pub type ExistingPrBranchTarget {
  ExistingPrBranchTarget(
    repository: String,
    head_repo: String,
    head_branch: String,
    expected_head_sha: String,
    base_branch: String,
    base_sha: String,
    pr_number: Int,
    pr_url: String,
  )
}

pub type ArtifactParseError {
  ArtifactParseError(code: String, message: String)
}

pub fn error_code(error: ArtifactParseError) -> String {
  let ArtifactParseError(code: code, ..) = error
  code
}

pub fn error_message(error: ArtifactParseError) -> String {
  let ArtifactParseError(message: message, ..) = error
  message
}

pub fn parse_commit_stack(
  contents: String,
) -> Result(CommitStackArtifact, ArtifactParseError) {
  use value <- result.try(parse_json(contents, "commit_stack_json_invalid"))
  case value {
    json_value.JObject(entries) -> decode_commit_stack(entries)
    _ ->
      error(
        "commit_stack_not_object",
        "commit stack artifact must be a JSON object",
      )
  }
}

pub fn parse_existing_pr_branch_target(
  contents: String,
) -> Result(ExistingPrBranchTarget, ArtifactParseError) {
  use value <- result.try(parse_json(
    contents,
    "existing_pr_branch_target_json_invalid",
  ))
  case value {
    json_value.JObject(entries) -> decode_existing_pr_branch_target(entries)
    _ ->
      error(
        "existing_pr_branch_target_not_object",
        "existing PR branch target artifact must be a JSON object",
      )
  }
}

pub fn commit_stack_identity_json(stack: CommitStackArtifact) -> json.Json {
  json.object([
    #("repository", json.string(stack.repository)),
    #("base_ref", json.string(stack.base_ref)),
    #("base_sha", json.string(stack.base_sha)),
    #("head_sha", json.string(stack.head_sha)),
    #("head_tree", json.string(stack.head_tree)),
    #(
      "carrier",
      json.object([
        #("ref", json.string(stack.carrier.ref)),
        #("sha256", json.string(stack.carrier.sha256)),
        #("bytes", json.int(stack.carrier.bytes)),
        #("media_type", json.string(stack.carrier.media_type)),
      ]),
    ),
  ])
}

pub fn existing_target_identity_json(
  target: ExistingPrBranchTarget,
) -> json.Json {
  json.object([
    #("repository", json.string(target.repository)),
    #("head_repo", json.string(target.head_repo)),
    #("head_branch", json.string(target.head_branch)),
    #("expected_head_sha", json.string(target.expected_head_sha)),
    #("base_branch", json.string(target.base_branch)),
    #("base_sha", json.string(target.base_sha)),
    #("pr_number", json.int(target.pr_number)),
    #("pr_url", json.string(target.pr_url)),
  ])
}

pub fn validate_commit_stack(
  stack: CommitStackArtifact,
) -> Result(CommitStackArtifact, ArtifactParseError) {
  use _ <- result.try(validate_non_empty(stack.repository, "repository"))
  use _ <- result.try(validate_non_empty(stack.base_ref, "base.ref"))
  use _ <- result.try(validate_git_oid(stack.base_sha, "base.sha"))
  use _ <- result.try(validate_git_oid(stack.head_sha, "head.sha"))
  use _ <- result.try(validate_git_oid(stack.head_tree, "head.tree"))
  use _ <- result.try(validate_commit_stack_carrier(stack.carrier))
  Ok(stack)
}

pub fn validate_commit_stack_carrier(
  carrier: CommitStackCarrier,
) -> Result(CommitStackCarrier, ArtifactParseError) {
  use _ <- result.try(validate_non_empty(carrier.ref, "carrier.ref"))
  use _ <- result.try(validate_sha256(carrier.sha256, "carrier.sha256"))
  use _ <- result.try(validate_non_negative(carrier.bytes, "carrier.bytes"))
  use _ <- result.try(validate_max_bytes(carrier.bytes, "carrier.bytes"))
  use _ <- result.try(require_equal(
    carrier.media_type,
    bundle_media_type,
    "commit_stack_carrier_media_type_mismatch",
    "commit stack carrier media_type must be " <> bundle_media_type,
  ))
  Ok(carrier)
}

pub fn validate_existing_pr_branch_target(
  target: ExistingPrBranchTarget,
) -> Result(ExistingPrBranchTarget, ArtifactParseError) {
  use _ <- result.try(validate_non_empty(target.repository, "repository"))
  use _ <- result.try(validate_non_empty(target.head_repo, "head.repo"))
  use _ <- result.try(validate_non_empty(target.head_branch, "head.branch"))
  use _ <- result.try(validate_git_oid(target.expected_head_sha, "head.sha"))
  use _ <- result.try(validate_non_empty(target.base_branch, "base.branch"))
  use _ <- result.try(validate_git_oid(target.base_sha, "base.sha"))
  use _ <- result.try(validate_non_negative(
    target.pr_number,
    "pull_request.number",
  ))
  case target.pr_number > 0 {
    True -> {
      use _ <- result.try(validate_non_empty(target.pr_url, "pull_request.url"))
      Ok(target)
    }
    False -> Ok(target)
  }
}

fn decode_commit_stack(
  entries: List(#(String, json_value.JsonValue)),
) -> Result(CommitStackArtifact, ArtifactParseError) {
  use artifact_type <- result.try(required_string(entries, "artifact_type"))
  use _ <- result.try(require_equal(
    artifact_type,
    commit_stack_artifact_type,
    "commit_stack_artifact_type_mismatch",
    "commit stack artifact_type must be " <> commit_stack_artifact_type,
  ))
  use repository <- result.try(repository_field(entries))
  use base <- result.try(required_object(entries, "base"))
  use base_ref <- result.try(required_string(base, "ref"))
  use base_sha <- result.try(required_string(base, "sha"))
  use head <- result.try(required_object(entries, "head"))
  use head_sha <- result.try(required_string(head, "sha"))
  use head_tree <- result.try(required_string(head, "tree"))
  use carrier_entries <- result.try(required_object(entries, "carrier"))
  use carrier <- result.try(decode_carrier(carrier_entries))
  validate_commit_stack(CommitStackArtifact(
    repository: repository,
    base_ref: base_ref,
    base_sha: base_sha,
    head_sha: head_sha,
    head_tree: head_tree,
    carrier: carrier,
  ))
}

fn decode_carrier(
  entries: List(#(String, json_value.JsonValue)),
) -> Result(CommitStackCarrier, ArtifactParseError) {
  use ref <- result.try(required_string(entries, "ref"))
  use sha256 <- result.try(required_string(entries, "sha256"))
  use bytes <- result.try(required_int(entries, "bytes"))
  let media_type =
    optional_string(entries, "media_type") |> unwrap(bundle_media_type)
  validate_commit_stack_carrier(CommitStackCarrier(
    ref: ref,
    sha256: sha256,
    bytes: bytes,
    media_type: media_type,
  ))
}

fn decode_existing_pr_branch_target(
  entries: List(#(String, json_value.JsonValue)),
) -> Result(ExistingPrBranchTarget, ArtifactParseError) {
  use artifact_type <- result.try(required_string(entries, "artifact_type"))
  use _ <- result.try(require_equal(
    artifact_type,
    existing_pr_branch_target_artifact_type,
    "existing_pr_branch_target_artifact_type_mismatch",
    "existing PR branch target artifact_type must be "
      <> existing_pr_branch_target_artifact_type,
  ))
  use repository <- result.try(repository_field(entries))
  use head <- result.try(required_object(entries, "head"))
  use head_repo <- result.try(required_string(head, "repo"))
  use head_branch <- result.try(required_string(head, "branch"))
  use expected_head_sha <- result.try(required_string(head, "sha"))
  use base <- result.try(required_object(entries, "base"))
  use base_branch <- result.try(required_string(base, "branch"))
  use base_sha <- result.try(required_string(base, "sha"))
  use pull_request <- result.try(optional_pull_request(entries))
  let #(pr_number, pr_url) = pull_request
  validate_existing_pr_branch_target(ExistingPrBranchTarget(
    repository: repository,
    head_repo: head_repo,
    head_branch: head_branch,
    expected_head_sha: expected_head_sha,
    base_branch: base_branch,
    base_sha: base_sha,
    pr_number: pr_number,
    pr_url: pr_url,
  ))
}

fn optional_pull_request(
  entries: List(#(String, json_value.JsonValue)),
) -> Result(#(Int, String), ArtifactParseError) {
  case field(entries, "pull_request") {
    None -> Ok(#(0, ""))
    Some(json_value.JObject(pull_request)) -> {
      use pr_number <- result.try(required_int(pull_request, "number"))
      use pr_url <- result.try(required_string(pull_request, "url"))
      Ok(#(pr_number, pr_url))
    }
    Some(_) ->
      error(
        "pull_request_not_object",
        "pull_request must be an object when present",
      )
  }
}

fn parse_json(
  contents: String,
  code: String,
) -> Result(json_value.JsonValue, ArtifactParseError) {
  case json_value.parse(contents) {
    Ok(value) -> Ok(value)
    Error(Nil) ->
      Error(ArtifactParseError(code, "artifact payload must be valid JSON"))
  }
}

fn repository_field(
  entries: List(#(String, json_value.JsonValue)),
) -> Result(String, ArtifactParseError) {
  case field(entries, "repository") {
    Some(json_value.JString(value)) -> Ok(value)
    Some(json_value.JObject(repository_entries)) ->
      required_string(repository_entries, "repo")
    Some(_) ->
      error(
        "artifact_repository_invalid",
        "artifact repository must be a string or object with repo",
      )
    None -> missing_field("repository")
  }
}

fn required_object(
  entries: List(#(String, json_value.JsonValue)),
  key: String,
) -> Result(List(#(String, json_value.JsonValue)), ArtifactParseError) {
  case field(entries, key) {
    Some(json_value.JObject(values)) -> Ok(values)
    Some(_) ->
      error(
        "artifact_field_invalid",
        "artifact field " <> key <> " must be an object",
      )
    None -> missing_field(key)
  }
}

fn required_string(
  entries: List(#(String, json_value.JsonValue)),
  key: String,
) -> Result(String, ArtifactParseError) {
  case field(entries, key) {
    Some(json_value.JString(value)) -> Ok(value)
    Some(_) ->
      error(
        "artifact_field_invalid",
        "artifact field " <> key <> " must be a string",
      )
    None -> missing_field(key)
  }
}

fn optional_string(
  entries: List(#(String, json_value.JsonValue)),
  key: String,
) -> Option(String) {
  case field(entries, key) {
    Some(json_value.JString(value)) -> Some(value)
    _ -> None
  }
}

fn required_int(
  entries: List(#(String, json_value.JsonValue)),
  key: String,
) -> Result(Int, ArtifactParseError) {
  case field(entries, key) {
    Some(json_value.JInt(value)) -> Ok(value)
    Some(_) ->
      error(
        "artifact_field_invalid",
        "artifact field " <> key <> " must be an integer",
      )
    None -> missing_field(key)
  }
}

fn missing_field(key: String) -> Result(a, ArtifactParseError) {
  error("artifact_field_missing", "artifact is missing field " <> key)
}

fn require_equal(
  actual: String,
  expected: String,
  code: String,
  message: String,
) -> Result(Nil, ArtifactParseError) {
  case actual == expected {
    True -> Ok(Nil)
    False -> error(code, message)
  }
}

fn validate_non_empty(
  value: String,
  field_name: String,
) -> Result(Nil, ArtifactParseError) {
  case string.trim(value) == "" || has_control_character(value) {
    True ->
      error(
        "artifact_field_empty",
        "artifact field " <> field_name <> " must be non-empty",
      )
    False -> Ok(Nil)
  }
}

fn validate_sha256(
  value: String,
  field_name: String,
) -> Result(Nil, ArtifactParseError) {
  case string.length(value) == 64 && all_hex_lowercase(value) {
    True -> Ok(Nil)
    False ->
      error(
        "artifact_sha256_invalid",
        "artifact field " <> field_name <> " must be 64 lowercase hex",
      )
  }
}

fn validate_git_oid(
  value: String,
  field_name: String,
) -> Result(Nil, ArtifactParseError) {
  case valid_git_oid(value) {
    True -> Ok(Nil)
    False ->
      error(
        "artifact_git_oid_invalid",
        "artifact field "
          <> field_name
          <> " must be a full lowercase hex Git object id",
      )
  }
}

fn validate_non_negative(
  value: Int,
  field_name: String,
) -> Result(Nil, ArtifactParseError) {
  case value >= 0 {
    True -> Ok(Nil)
    False ->
      error(
        "artifact_bytes_invalid",
        "artifact field " <> field_name <> " must be non-negative",
      )
  }
}

fn validate_max_bytes(
  value: Int,
  field_name: String,
) -> Result(Nil, ArtifactParseError) {
  case value <= max_bundle_bytes {
    True -> Ok(Nil)
    False ->
      error(
        "commit_stack_carrier_too_large",
        "artifact field "
          <> field_name
          <> " exceeds maximum commit stack bundle size",
      )
  }
}

fn field(
  entries: List(#(String, json_value.JsonValue)),
  key: String,
) -> Option(json_value.JsonValue) {
  case entries {
    [] -> None
    [#(current, value), ..rest] ->
      case current == key {
        True -> Some(value)
        False -> field(rest, key)
      }
  }
}

fn unwrap(value: Option(a), default: a) -> a {
  case value {
    Some(value) -> value
    None -> default
  }
}

fn has_control_character(value: String) -> Bool {
  value
  |> string.to_graphemes
  |> list.any(fn(ch) { ch == "\n" || ch == "\r" || ch == "\t" })
}

fn all_hex_lowercase(value: String) -> Bool {
  value |> string.to_graphemes |> list.all(is_hex_lowercase)
}

fn valid_git_oid(value: String) -> Bool {
  let length = string.length(value)
  case length == 40 || length == 64 {
    True -> all_hex_lowercase(value)
    False -> False
  }
}

fn is_hex_lowercase(ch: String) -> Bool {
  is_digit(ch) || is_lower_hex(ch)
}

fn is_digit(ch: String) -> Bool {
  string.compare(ch, "0") != order.Lt && string.compare(ch, "9") != order.Gt
}

fn is_lower_hex(ch: String) -> Bool {
  string.compare(ch, "a") != order.Lt && string.compare(ch, "f") != order.Gt
}

fn error(code: String, message: String) -> Result(a, ArtifactParseError) {
  Error(ArtifactParseError(code: code, message: message))
}
