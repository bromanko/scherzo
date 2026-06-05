import gleam/bit_array
import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None}
import gleam/result
import gleam/string

pub const artifact_type = "scherzo.commit_stack.v1"

pub const max_artifact_bytes = 50_000_000

pub const max_commits = 100

pub const max_message_chars = 10_000

pub const max_patch_bytes = 5_000_000

pub type CommitStack {
  CommitStack(
    base_ref: String,
    base_revision: String,
    head_revision: String,
    commits: List(CommitStackCommit),
  )
}

pub type CommitStackCommit {
  CommitStackCommit(commit_id: Option(String), message: String, patch: String)
}

pub type CommitStackError {
  CommitStackError(code: String, message: String)
}

pub fn code(error: CommitStackError) -> String {
  let CommitStackError(code: code, ..) = error
  code
}

pub fn message(error: CommitStackError) -> String {
  let CommitStackError(message: message, ..) = error
  message
}

pub fn decode_json(payload: String) -> Result(CommitStack, CommitStackError) {
  use Nil <- result.try(validate_payload_size(payload))
  use stack <- result.try(
    json.parse(payload, decoder())
    |> result.map_error(fn(_) {
      CommitStackError(
        code: "invalid_commit_stack_json",
        message: "commit_stack artifact must be valid scherzo.commit_stack.v1 JSON",
      )
    }),
  )
  validate_stack(stack)
}

fn validate_payload_size(payload: String) -> Result(Nil, CommitStackError) {
  let bytes = byte_size(payload)
  case bytes > max_artifact_bytes {
    True ->
      Error(CommitStackError(
        code: "commit_stack_too_large",
        message: "commit_stack artifact exceeds maximum byte size",
      ))
    False -> Ok(Nil)
  }
}

fn validate_stack(stack: CommitStack) -> Result(CommitStack, CommitStackError) {
  let CommitStack(commits: commits, ..) = stack
  case commits {
    [] ->
      Error(CommitStackError(
        code: "empty_commit_stack",
        message: "commit_stack publication requires at least one commit",
      ))
    _ ->
      case list.length(commits) > max_commits {
        True ->
          Error(CommitStackError(
            code: "commit_stack_too_many_commits",
            message: "commit_stack artifact exceeds maximum commit count",
          ))
        False -> {
          use Nil <- result.try(validate_commits(commits, 1))
          Ok(stack)
        }
      }
  }
}

fn validate_commits(
  commits: List(CommitStackCommit),
  index: Int,
) -> Result(Nil, CommitStackError) {
  case commits {
    [] -> Ok(Nil)
    [CommitStackCommit(message: message, patch: patch, ..), ..rest] -> {
      use Nil <- result.try(validate_message(message, index))
      use Nil <- result.try(validate_patch(patch, index))
      validate_commits(rest, index + 1)
    }
  }
}

fn validate_message(
  message: String,
  index: Int,
) -> Result(Nil, CommitStackError) {
  case string.length(message) > max_message_chars {
    True ->
      Error(CommitStackError(
        code: "commit_stack_message_too_large",
        message: "commit_stack commit "
          <> int.to_string(index)
          <> " message exceeds maximum length",
      ))
    False -> Ok(Nil)
  }
}

fn validate_patch(patch: String, index: Int) -> Result(Nil, CommitStackError) {
  case byte_size(patch) > max_patch_bytes {
    True ->
      Error(CommitStackError(
        code: "commit_stack_patch_too_large",
        message: "commit_stack commit "
          <> int.to_string(index)
          <> " patch exceeds maximum byte size",
      ))
    False -> Ok(Nil)
  }
}

fn byte_size(value: String) -> Int {
  bit_array.byte_size(bit_array.from_string(value))
}

fn decoder() -> decode.Decoder(CommitStack) {
  use schema_version <- decode.field("schema_version", decode.int)
  use decoded_artifact_type <- decode.field("artifact_type", decode.string)
  use base_ref <- decode.optional_field("base_ref", "", decode.string)
  use base_revision <- decode.optional_field("base_revision", "", decode.string)
  use head_revision <- decode.optional_field("head_revision", "", decode.string)
  use commits <- decode.field("commits", decode.list(commit_decoder()))
  case schema_version, decoded_artifact_type {
    1, "scherzo.commit_stack.v1" ->
      decode.success(CommitStack(
        base_ref: base_ref,
        base_revision: base_revision,
        head_revision: head_revision,
        commits: commits,
      ))
    _, _ ->
      decode.failure(
        CommitStack("", "", "", []),
        expected: "scherzo.commit_stack.v1",
      )
  }
}

fn commit_decoder() -> decode.Decoder(CommitStackCommit) {
  use commit_id <- decode.optional_field(
    "commit_id",
    None,
    decode.optional(decode.string),
  )
  use message <- decode.field("message", decode.string)
  use patch <- decode.field("patch", decode.string)
  decode.success(CommitStackCommit(
    commit_id: commit_id,
    message: message,
    patch: patch,
  ))
}
