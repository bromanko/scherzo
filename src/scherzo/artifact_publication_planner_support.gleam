import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/artifact_publication_config
import scherzo/error
import scherzo/json_value
import scherzo/state/artifact_store
import scherzo/template
import scherzo/workflow_contract

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

pub fn render_changed_files_markdown(files: List(String)) -> String {
  files
  |> list.map(fn(file) { "- `" <> file <> "`" })
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
  title: Option(String),
  url: Option(String),
) -> json.Json {
  json.object([
    #("kind", json.string(kind)),
    #("id", json.string(id)),
    #("identifier", json.string(identifier)),
    #("slug", json.string(slug)),
    #("title", option_string_to_json(title)),
    #("url", option_string_to_json(url)),
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
