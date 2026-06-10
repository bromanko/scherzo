import gleam/bit_array
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/artifact_publication_config
import scherzo/commit_stack_artifact
import scherzo/error
import scherzo/hash
import scherzo/json_value
import scherzo/state/artifact_store
import scherzo/template
import scherzo/workflow_contract
import scherzo/workflow_contract_manifest

pub fn render_template(
  source: String,
  locals: List(#(String, template.Value)),
) -> Result(String, error.TemplateError) {
  template.render_scheduled_with_locals(
    source,
    template.ScheduledTemplateContext(
      job_id: "publication-planner",
      workflow_id: "publication-planner",
      due_at: "",
      started_at: "",
      run_id: "publication-planner",
      attempt: 0,
    ),
    locals,
  )
}

pub fn validate_relative_path(path: String) -> Result(Nil, #(String, String)) {
  case path == string.trim(path) {
    False ->
      Error(#(
        "unsafe_rendered_path",
        "rendered publication path must not contain leading or trailing whitespace",
      ))
    True ->
      case path == "" {
        True ->
          Error(#(
            "unsafe_rendered_path",
            "rendered publication path must be non-empty",
          ))
        False ->
          case string.starts_with(path, "/") {
            True ->
              Error(#(
                "unsafe_rendered_path",
                "rendered publication path must be repository-relative",
              ))
            False ->
              case has_parent_segment(path) {
                True ->
                  Error(#(
                    "unsafe_rendered_path",
                    "rendered publication path must not contain ..",
                  ))
                False ->
                  case has_control_character(path) {
                    True ->
                      Error(#(
                        "unsafe_rendered_path",
                        "rendered publication path must not contain control characters",
                      ))
                    False -> Ok(Nil)
                  }
              }
          }
      }
  }
}

pub fn validate_branch(branch: String) -> Result(Nil, #(String, String)) {
  case branch == string.trim(branch) {
    False ->
      Error(#(
        "unsafe_branch",
        "rendered branch name must not contain leading or trailing whitespace",
      ))
    True ->
      case workflow_contract.valid_git_ref(branch) {
        False -> Error(#("unsafe_branch", "rendered branch name is invalid"))
        True ->
          case has_invalid_git_ref_pattern(branch) {
            True -> Error(#("unsafe_branch", "rendered branch name is unsafe"))
            False -> Ok(Nil)
          }
      }
  }
}

pub fn make_series_id(
  work_id: String,
  workflow_id: String,
  publication_id: String,
) -> String {
  "work/"
  <> work_id
  <> "/workflow/"
  <> workflow_id
  <> "/publication/"
  <> publication_id
}

pub fn render_files_markdown(files: List(#(String, String, String))) -> String {
  files
  |> list.map(fn(file) {
    let #(destination_path, selector, sha256) = file
    "- `" <> destination_path <> "` ← `" <> selector <> "` (" <> sha256 <> ")"
  })
  |> string.join(with: "\n")
}

pub fn default_extension(media_type: String) -> String {
  case media_type {
    "text/markdown" -> ".md"
    "application/json" -> ".json"
    "text/plain" -> ".txt"
    "application/yaml" -> ".yaml"
    "text/yaml" -> ".yaml"
    "image/png" -> ".png"
    "video/webm" -> ".webm"
    _ -> ""
  }
}

pub fn option_string_to_json(value: Option(String)) -> json.Json {
  case value {
    Some(value) -> json.string(value)
    None -> json.null()
  }
}

pub fn option_string_to_template_value(
  value: Option(String),
) -> template.Value {
  case value {
    Some(value) -> template.VString(value)
    None -> template.VNil
  }
}

pub fn work_identity_to_json(
  kind: String,
  id: String,
  identifier: String,
  slug: String,
) -> json.Json {
  json.object([
    #("kind", json.string(kind)),
    #("id", json.string(id)),
    #("identifier", json.string(identifier)),
    #("slug", json.string(slug)),
  ])
}

pub fn repository_to_json(
  kind: String,
  id: String,
  github_repo: Option(String),
  github_base: Option(String),
) -> json.Json {
  json.object([
    #("kind", json.string(kind)),
    #("id", json.string(id)),
    #("github_repo", option_string_to_json(github_repo)),
    #("github_base", option_string_to_json(github_base)),
  ])
}

pub fn publication_file_route_to_version_json(
  file_route: artifact_publication_config.PublicationFileRoute,
) -> json.Json {
  let artifact_publication_config.PublicationFileRoute(selector:, path:) =
    file_route
  let artifact_publication_config.PublicationFileSelector(output:, entry:) =
    selector
  json.object([
    #("output", json.string(output)),
    #("entry", option_string_to_json(entry)),
    #("path_template", json.string(path)),
  ])
}

pub fn json_value_string_leaf_template_locals(
  prefix: String,
  metadata: Option(json_value.JsonValue),
) -> List(#(String, template.Value)) {
  case metadata {
    Some(value) -> json_value_template_locals(prefix, value)
    None -> []
  }
}

fn json_value_template_locals(
  prefix: String,
  value: json_value.JsonValue,
) -> List(#(String, template.Value)) {
  case value {
    json_value.JObject(entries) ->
      entries
      |> list.flat_map(fn(entry) {
        let #(key, nested) = entry
        json_value_template_locals(prefix <> "." <> key, nested)
      })
    json_value.JString(text) -> [#(prefix, template.VString(text))]
    _ -> []
  }
}

pub type TargetSelectionError {
  TargetSelectionError(code: String, message: String)
}

pub fn target_selection_error_code(error: TargetSelectionError) -> String {
  let TargetSelectionError(code: code, ..) = error
  code
}

pub fn target_selection_error_message(error: TargetSelectionError) -> String {
  let TargetSelectionError(message: message, ..) = error
  message
}

pub fn select_existing_pr_branch_target(
  manifest: workflow_contract_manifest.ContractOutputManifest,
  output: String,
  store: artifact_store.Store,
) -> Result(commit_stack_artifact.ExistingPrBranchTarget, TargetSelectionError) {
  use named <- result.try(find_named_output(manifest.outputs, output))
  case named.value.status {
    workflow_contract_manifest.Absent ->
      target_error("absent_output", "publication output is absent: " <> output)
    workflow_contract_manifest.Present -> {
      use contents <- result.try(read_retained_output_text(
        output,
        named.value,
        store,
      ))
      commit_stack_artifact.parse_existing_pr_branch_target(contents)
      |> result.map_error(commit_stack_parse_error_to_target_error)
    }
  }
}

fn find_named_output(
  outputs: List(workflow_contract_manifest.NamedManifestValue),
  name: String,
) -> Result(workflow_contract_manifest.NamedManifestValue, TargetSelectionError) {
  case outputs {
    [] -> target_error("unknown_output", "unknown publication output: " <> name)
    [output, ..rest] ->
      case output.name == name {
        True -> Ok(output)
        False -> find_named_output(rest, name)
      }
  }
}

fn read_retained_output_text(
  output: String,
  value: workflow_contract_manifest.ManifestValue,
  store: artifact_store.Store,
) -> Result(String, TargetSelectionError) {
  use ref <- result.try(require_option(
    value.ref,
    "missing_ref",
    "publication output is missing a retained artifact ref: " <> output,
  ))
  use sha256 <- result.try(require_option(
    value.sha256,
    "missing_ref",
    "publication output is missing a retained artifact sha256: " <> output,
  ))
  use bytes <- result.try(require_option(
    value.bytes,
    "missing_ref",
    "publication output is missing a retained artifact byte count: " <> output,
  ))
  use contents <- result.try(read_artifact_text(ref, store))
  use Nil <- result.try(verify_text_contents(ref, contents, sha256, bytes))
  Ok(contents)
}

fn read_artifact_text(
  ref: String,
  store: artifact_store.Store,
) -> Result(String, TargetSelectionError) {
  case artifact_store.read_artifact_unverified(store, ref) {
    Ok(contents) -> Ok(contents)
    Error(read_error) ->
      target_error(
        "missing_artifact_bytes",
        "artifact bytes could not be read for ref: "
          <> ref
          <> " ("
          <> artifact_error_summary(read_error)
          <> ")",
      )
  }
}

fn verify_text_contents(
  ref: String,
  contents: String,
  expected_sha256: String,
  expected_bytes: Int,
) -> Result(Nil, TargetSelectionError) {
  let actual_sha256 = hash.sha256_hex(contents)
  let actual_bytes = bit_array.byte_size(bit_array.from_string(contents))
  use Nil <- result.try(check_sha256(ref, actual_sha256, expected_sha256))
  check_bytes(ref, actual_bytes, expected_bytes)
}

fn check_sha256(
  ref: String,
  actual: String,
  expected: String,
) -> Result(Nil, TargetSelectionError) {
  case actual == expected {
    True -> Ok(Nil)
    False ->
      target_error(
        "hash_mismatch",
        "artifact sha256 did not match for ref: " <> ref,
      )
  }
}

fn check_bytes(
  ref: String,
  actual: Int,
  expected: Int,
) -> Result(Nil, TargetSelectionError) {
  case actual == expected {
    True -> Ok(Nil)
    False ->
      target_error(
        "byte_count_mismatch",
        "artifact byte count did not match for ref: " <> ref,
      )
  }
}

fn require_option(
  value: Option(a),
  code: String,
  text: String,
) -> Result(a, TargetSelectionError) {
  case value {
    Some(value) -> Ok(value)
    None -> target_error(code, text)
  }
}

fn commit_stack_parse_error_to_target_error(
  parse_error: commit_stack_artifact.ArtifactParseError,
) -> TargetSelectionError {
  TargetSelectionError(
    code: commit_stack_artifact.error_code(parse_error),
    message: commit_stack_artifact.error_message(parse_error),
  )
}

fn target_error(
  code: String,
  message: String,
) -> Result(a, TargetSelectionError) {
  Error(TargetSelectionError(code:, message:))
}

fn has_parent_segment(value: String) -> Bool {
  value == ".."
  || string.starts_with(value, "../")
  || string.ends_with(value, "/..")
  || string.contains(value, "/../")
}

fn has_invalid_git_ref_pattern(value: String) -> Bool {
  string.starts_with(value, "/")
  || string.ends_with(value, "/")
  || string.contains(value, "//")
  || string.contains(value, "..")
  || string.contains(value, "@{")
  || value == "@"
  || string.ends_with(value, ".")
  || has_invalid_git_ref_character(value)
  || has_invalid_git_ref_component(value)
}

fn has_invalid_git_ref_character(value: String) -> Bool {
  value
  |> string.to_graphemes
  |> list.any(fn(ch) {
    ch == " "
    || ch == "~"
    || ch == "^"
    || ch == ":"
    || ch == "?"
    || ch == "*"
    || ch == "["
    || ch == "]"
    || ch == "\\"
  })
}

fn has_invalid_git_ref_component(value: String) -> Bool {
  value
  |> string.split(on: "/")
  |> list.any(fn(part) {
    part == ""
    || string.starts_with(part, ".")
    || string.ends_with(part, ".lock")
  })
}

pub fn artifact_error_summary(error: artifact_store.ArtifactError) -> String {
  case error {
    artifact_store.ArtifactIo(detail) -> "io: " <> detail
    artifact_store.ArtifactWriteFailed(write_error) ->
      artifact_store.artifact_write_error_to_string(write_error)
    artifact_store.MissingStepArtifact(missing_ref) ->
      "missing: " <> missing_ref
    artifact_store.CorruptStepArtifact(corrupt_ref) ->
      "corrupt: " <> corrupt_ref
    artifact_store.InvalidArtifactRef(invalid_ref) ->
      "invalid ref: " <> invalid_ref
    artifact_store.DecodeArtifactFailed(detail) -> "decode: " <> detail
    artifact_store.DirectorySyncUnsupported(detail) -> detail
  }
}

fn has_control_character(value: String) -> Bool {
  value
  |> string.to_graphemes
  |> list.any(fn(ch) { ch == "\n" || ch == "\r" || ch == "\t" })
}
