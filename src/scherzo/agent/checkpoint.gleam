/// Checkpoint management for agent continuity
///
/// Checkpoints capture agent state at various points during task execution,
/// enabling recovery from context exhaustion and crash recovery.
import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/core/event
import scherzo/agent/workspace
import scherzo/core/types.{type Id, type Timestamp}
import scherzo/vcs/jj
import simplifile

/// A checkpoint capturing agent state at a point in time
pub type Checkpoint {
  Checkpoint(
    /// The task this checkpoint belongs to
    task_id: Id,
    /// The agent that created this checkpoint
    agent_id: Id,
    /// Monotonic sequence number for ordering
    sequence: Int,
    /// When the checkpoint was created
    timestamp: Timestamp,
    /// Type of checkpoint (incremental, pre-compact, final)
    checkpoint_type: CheckpointType,
    /// Files modified since last checkpoint
    files_modified: List(FileChange),
    /// Summary of work done (AI-generated)
    work_summary: String,
    /// Remaining work to do (final checkpoint only)
    next_steps: Option(String),
    /// The jj change ID at checkpoint time
    jj_change_id: String,
  )
}

/// Type of checkpoint
pub type CheckpointType {
  /// Periodic save after tool use (PostToolUse hook)
  Incremental
  /// Save before context compaction (PreCompact hook)
  PreCompact
  /// Final save when agent stops (Stop hook)
  Final
}

/// A file change tracked in a checkpoint
pub type FileChange {
  FileChange(
    /// Path to the file (relative to repo root)
    path: String,
    /// Type of change
    change_type: FileChangeType,
    /// Brief description of the change
    summary: Option(String),
  )
}

/// Type of file change
pub type FileChangeType {
  Added
  Modified
  Deleted
}

/// Configuration for checkpoint storage
pub type CheckpointConfig {
  CheckpointConfig(
    /// Base directory for checkpoints (e.g., .scherzo/checkpoints)
    checkpoints_dir: String,
    /// Repository directory (for jj operations)
    repo_dir: String,
  )
}

/// Create a checkpoint config for a repository
pub fn default_config(repo_dir: String) -> CheckpointConfig {
  CheckpointConfig(
    checkpoints_dir: repo_dir <> "/.scherzo/checkpoints",
    repo_dir: repo_dir,
  )
}

/// Create a new checkpoint
pub fn create(
  config: CheckpointConfig,
  task_id: Id,
  agent_id: Id,
  checkpoint_type: CheckpointType,
  work_summary: String,
  next_steps: Option(String),
) -> Result(Checkpoint, String) {
  // Get current jj change ID
  use jj_change_id <- result.try(jj.get_current_change(config.repo_dir))

  // Get files modified from jj status
  use files_modified <- result.try(get_file_changes(config.repo_dir))

  // Get next sequence number
  use sequence <- result.try(get_next_sequence(config, task_id))

  // Create timestamp (milliseconds since epoch)
  let timestamp = get_timestamp()

  let checkpoint =
    Checkpoint(
      task_id: task_id,
      agent_id: agent_id,
      sequence: sequence,
      timestamp: timestamp,
      checkpoint_type: checkpoint_type,
      files_modified: files_modified,
      work_summary: work_summary,
      next_steps: next_steps,
      jj_change_id: jj_change_id,
    )

  // Save the checkpoint
  use _ <- result.try(save(config, checkpoint))

  Ok(checkpoint)
}

/// Save a checkpoint to disk
pub fn save(
  config: CheckpointConfig,
  checkpoint: Checkpoint,
) -> Result(Nil, String) {
  let task_dir =
    config.checkpoints_dir <> "/" <> workspace.sanitize_id(checkpoint.task_id)

  // Ensure directory exists
  case simplifile.create_directory_all(task_dir) {
    Error(err) ->
      Error(
        "Failed to create checkpoints directory: "
        <> simplifile.describe_error(err),
      )
    Ok(_) -> {
      // Determine filename
      let filename = case checkpoint.checkpoint_type {
        Final -> "final.json"
        _ -> pad_sequence(checkpoint.sequence) <> ".json"
      }
      let filepath = task_dir <> "/" <> filename

      // Encode and write
      let json_str = encode(checkpoint)
      case simplifile.write(filepath, json_str) {
        Error(err) ->
          Error(
            "Failed to write checkpoint: " <> simplifile.describe_error(err),
          )
        Ok(_) -> Ok(Nil)
      }
    }
  }
}

/// Create a CheckpointCreated event from a checkpoint
pub fn to_event(checkpoint: Checkpoint) -> event.Event {
  let checkpoint_id =
    checkpoint.task_id <> "-" <> int.to_string(checkpoint.sequence)
  event.CheckpointCreated(
    task_id: checkpoint.task_id,
    agent_id: checkpoint.agent_id,
    checkpoint_id: checkpoint_id,
    timestamp: checkpoint.timestamp,
  )
}

/// Load the latest checkpoint for a task
pub fn load_latest(
  config: CheckpointConfig,
  task_id: Id,
) -> Result(Checkpoint, String) {
  let task_dir = config.checkpoints_dir <> "/" <> workspace.sanitize_id(task_id)

  // Check for final checkpoint first
  let final_path = task_dir <> "/final.json"
  case simplifile.read(final_path) {
    Ok(content) -> decode_checkpoint(content)
    Error(_) -> {
      // Find highest numbered checkpoint
      case simplifile.read_directory(task_dir) {
        Error(err) ->
          Error("No checkpoints found: " <> simplifile.describe_error(err))
        Ok(files) -> {
          let checkpoint_files =
            files
            |> list.filter(fn(f) {
              string.ends_with(f, ".json") && f != "final.json"
            })
            |> list.sort(string.compare)
            |> list.reverse

          case checkpoint_files {
            [] -> Error("No checkpoints found for task")
            [latest, ..] -> {
              let filepath = task_dir <> "/" <> latest
              case simplifile.read(filepath) {
                Error(err) ->
                  Error(
                    "Failed to read checkpoint: "
                    <> simplifile.describe_error(err),
                  )
                Ok(content) -> decode_checkpoint(content)
              }
            }
          }
        }
      }
    }
  }
}

/// Load all checkpoints for a task in sequence order
pub fn load_all(
  config: CheckpointConfig,
  task_id: Id,
) -> Result(List(Checkpoint), String) {
  let task_dir = config.checkpoints_dir <> "/" <> workspace.sanitize_id(task_id)

  case simplifile.read_directory(task_dir) {
    Error(err) ->
      Error("Failed to read checkpoints: " <> simplifile.describe_error(err))
    Ok(files) -> {
      let checkpoint_files =
        files
        |> list.filter(fn(f) { string.ends_with(f, ".json") })
        |> list.sort(string.compare)

      checkpoint_files
      |> list.try_map(fn(filename) {
        let filepath = task_dir <> "/" <> filename
        case simplifile.read(filepath) {
          Error(err) ->
            Error(
              "Failed to read checkpoint: " <> simplifile.describe_error(err),
            )
          Ok(content) -> decode_checkpoint(content)
        }
      })
    }
  }
}

/// Encode a checkpoint to JSON
pub fn encode(checkpoint: Checkpoint) -> String {
  json.object([
    #("task_id", json.string(checkpoint.task_id)),
    #("agent_id", json.string(checkpoint.agent_id)),
    #("sequence", json.int(checkpoint.sequence)),
    #("timestamp", json.int(checkpoint.timestamp)),
    #("checkpoint_type", encode_checkpoint_type(checkpoint.checkpoint_type)),
    #(
      "files_modified",
      json.array(checkpoint.files_modified, encode_file_change),
    ),
    #("work_summary", json.string(checkpoint.work_summary)),
    #("next_steps", encode_option(checkpoint.next_steps, json.string)),
    #("jj_change_id", json.string(checkpoint.jj_change_id)),
  ])
  |> json.to_string
}

/// Decode a checkpoint from JSON
fn decode_checkpoint(content: String) -> Result(Checkpoint, String) {
  let decoder = {
    use task_id <- decode.field("task_id", decode.string)
    use agent_id <- decode.field("agent_id", decode.string)
    use sequence <- decode.field("sequence", decode.int)
    use timestamp <- decode.field("timestamp", decode.int)
    use checkpoint_type <- decode.field(
      "checkpoint_type",
      decode_checkpoint_type(),
    )
    use files_modified <- decode.field(
      "files_modified",
      decode.list(decode_file_change()),
    )
    use work_summary <- decode.field("work_summary", decode.string)
    use next_steps <- decode.field("next_steps", decode_option(decode.string))
    use jj_change_id <- decode.field("jj_change_id", decode.string)
    decode.success(Checkpoint(
      task_id: task_id,
      agent_id: agent_id,
      sequence: sequence,
      timestamp: timestamp,
      checkpoint_type: checkpoint_type,
      files_modified: files_modified,
      work_summary: work_summary,
      next_steps: next_steps,
      jj_change_id: jj_change_id,
    ))
  }

  case json.parse(content, decoder) {
    Error(_) -> Error("Failed to parse checkpoint JSON")
    Ok(checkpoint) -> Ok(checkpoint)
  }
}

/// Encode checkpoint type to JSON
fn encode_checkpoint_type(ct: CheckpointType) -> json.Json {
  case ct {
    Incremental -> json.string("incremental")
    PreCompact -> json.string("pre_compact")
    Final -> json.string("final")
  }
}

/// Decode checkpoint type from JSON
fn decode_checkpoint_type() -> decode.Decoder(CheckpointType) {
  use s <- decode.then(decode.string)
  case s {
    "incremental" -> decode.success(Incremental)
    "pre_compact" -> decode.success(PreCompact)
    "final" -> decode.success(Final)
    _ -> decode.failure(Incremental, "CheckpointType")
  }
}

/// Encode a file change to JSON
fn encode_file_change(fc: FileChange) -> json.Json {
  json.object([
    #("path", json.string(fc.path)),
    #("change_type", encode_file_change_type(fc.change_type)),
    #("summary", encode_option(fc.summary, json.string)),
  ])
}

/// Decode a file change from JSON
fn decode_file_change() -> decode.Decoder(FileChange) {
  use path <- decode.field("path", decode.string)
  use change_type <- decode.field("change_type", decode_file_change_type())
  use summary <- decode.field("summary", decode_option(decode.string))
  decode.success(FileChange(
    path: path,
    change_type: change_type,
    summary: summary,
  ))
}

/// Encode file change type to JSON
fn encode_file_change_type(fct: FileChangeType) -> json.Json {
  case fct {
    Added -> json.string("added")
    Modified -> json.string("modified")
    Deleted -> json.string("deleted")
  }
}

/// Decode file change type from JSON
fn decode_file_change_type() -> decode.Decoder(FileChangeType) {
  use s <- decode.then(decode.string)
  case s {
    "added" -> decode.success(Added)
    "modified" -> decode.success(Modified)
    "deleted" -> decode.success(Deleted)
    _ -> decode.failure(Modified, "FileChangeType")
  }
}

/// Encode an optional value
fn encode_option(opt: Option(a), encoder: fn(a) -> json.Json) -> json.Json {
  case opt {
    None -> json.null()
    Some(value) -> encoder(value)
  }
}

/// Decode an optional value
fn decode_option(decoder: decode.Decoder(a)) -> decode.Decoder(Option(a)) {
  decode.optional(decoder)
}

/// Get file changes from jj status
fn get_file_changes(repo_dir: String) -> Result(List(FileChange), String) {
  case jj.status(repo_dir) {
    Error(err) -> Error("Failed to get jj status: " <> err)
    Ok(status_output) -> Ok(parse_jj_status(status_output))
  }
}

/// Parse jj status output into file changes
/// jj status output format:
/// Working copy changes:
/// M path/to/modified.gleam
/// A path/to/added.gleam
/// D path/to/deleted.gleam
/// R {old => new} (renamed files)
fn parse_jj_status(output: String) -> List(FileChange) {
  output
  |> string.split("\n")
  |> list.filter_map(fn(line) {
    let trimmed = string.trim(line)
    // Handle empty lines
    case trimmed {
      "" -> Error(Nil)
      _ -> parse_status_line(trimmed)
    }
  })
}

/// Parse a single status line, handling various whitespace separators
fn parse_status_line(line: String) -> Result(FileChange, Nil) {
  // Split into graphemes to handle first char as status
  let graphemes = string.to_graphemes(line)
  case graphemes {
    [] -> Error(Nil)
    [status, ..rest] -> {
      // Get the path by joining rest and trimming leading whitespace
      let path = rest |> string.join("") |> string.trim_start
      // Validate we have a path
      case path {
        "" -> Error(Nil)
        _ -> {
          // Map status character to change type
          let change_type = case status {
            "A" -> Some(Added)
            "M" -> Some(Modified)
            "D" -> Some(Deleted)
            "R" -> Some(Modified)
            // Treat rename as modification
            _ -> None
          }
          case change_type {
            None -> Error(Nil)
            Some(ct) -> Ok(FileChange(path: path, change_type: ct, summary: None))
          }
        }
      }
    }
  }
}

/// Get the next sequence number for a task
fn get_next_sequence(
  config: CheckpointConfig,
  task_id: Id,
) -> Result(Int, String) {
  let task_dir = config.checkpoints_dir <> "/" <> workspace.sanitize_id(task_id)

  case simplifile.read_directory(task_dir) {
    // No directory yet, start at 1
    Error(_) -> Ok(1)
    Ok(files) -> {
      let max_sequence =
        files
        |> list.filter(fn(f) {
          string.ends_with(f, ".json") && f != "final.json"
        })
        |> list.filter_map(fn(f) {
          f
          |> string.replace(".json", "")
          |> int.parse
        })
        |> list.fold(0, int.max)

      Ok(max_sequence + 1)
    }
  }
}

/// Pad sequence number to 3 digits (001, 002, etc.)
fn pad_sequence(seq: Int) -> String {
  let s = int.to_string(seq)
  case string.length(s) {
    1 -> "00" <> s
    2 -> "0" <> s
    _ -> s
  }
}

/// Get current timestamp in milliseconds
@external(erlang, "os", "system_time")
fn os_system_time_millisecond() -> Int

fn get_timestamp() -> Timestamp {
  os_system_time_millisecond()
}
