/// File-based agent state persistence
///
/// Each agent's state is stored in a separate JSON file under `.scherzo/agents/`.
/// This survives REPL restarts and doesn't require an actor.
import gleam/dynamic/decode
import gleam/io
import gleam/json
import gleam/list
import gleam/string
import scherzo/core/types.{
  type AgentConfig, type AgentProvider, type AgentStatus, AgentConfig, Claude,
  Codex, Gemini,
}
import simplifile

/// Default directory for agent state files
pub const default_agents_dir = ".scherzo/agents"

/// Agent state persisted to disk
pub type AgentState {
  AgentState(
    config: AgentConfig,
    status: AgentStatus,
    task_id: String,
    workspace_path: String,
    pipe_path: String,
  )
}

/// Save agent state to disk
pub fn save_agent(agents_dir: String, agent: AgentState) -> Result(Nil, String) {
  // Ensure directory exists
  case simplifile.create_directory_all(agents_dir) {
    Error(err) ->
      Error("Failed to create agents dir: " <> simplifile.describe_error(err))
    Ok(_) -> {
      let file_path = agent_file_path(agents_dir, agent.config.id)
      let json_str =
        encode_agent_state(agent)
        |> json.to_string

      case simplifile.write(file_path, json_str) {
        Error(err) ->
          Error(
            "Failed to write agent state: " <> simplifile.describe_error(err),
          )
        Ok(_) -> Ok(Nil)
      }
    }
  }
}

/// Load agent state from disk
pub fn load_agent(
  agents_dir: String,
  agent_id: String,
) -> Result(AgentState, String) {
  let file_path = agent_file_path(agents_dir, agent_id)

  case simplifile.read(file_path) {
    Error(err) ->
      Error("Failed to read agent state: " <> simplifile.describe_error(err))
    Ok(content) ->
      case json.parse(content, decode_agent_state()) {
        Error(_) -> Error("Failed to parse agent state JSON")
        Ok(agent) -> Ok(agent)
      }
  }
}

/// Load all agents from disk
pub fn load_all_agents(agents_dir: String) -> List(AgentState) {
  case simplifile.read_directory(agents_dir) {
    Error(_) -> []
    Ok(files) -> {
      files
      |> list.filter(fn(f) { string.ends_with(f, ".json") })
      |> list.filter_map(fn(filename) {
        // Remove .json extension to get agent_id
        let agent_id = string.drop_end(filename, 5)
        case load_agent(agents_dir, agent_id) {
          Ok(agent) -> Ok(agent)
          Error(err) -> {
            io.println(
              "Warning: Failed to load agent " <> agent_id <> ": " <> err,
            )
            Error(Nil)
          }
        }
      })
    }
  }
}

/// Delete agent state file
pub fn delete_agent(agents_dir: String, agent_id: String) -> Result(Nil, String) {
  let file_path = agent_file_path(agents_dir, agent_id)

  case simplifile.delete(file_path) {
    Error(err) ->
      Error("Failed to delete agent state: " <> simplifile.describe_error(err))
    Ok(_) -> Ok(Nil)
  }
}

/// Update agent status on disk
pub fn update_status(
  agents_dir: String,
  agent_id: String,
  new_status: AgentStatus,
) -> Result(Nil, String) {
  case load_agent(agents_dir, agent_id) {
    Error(err) -> {
      io.println("Warning: Agent " <> agent_id <> " not found: " <> err)
      Error(err)
    }
    Ok(agent) -> {
      let updated = AgentState(..agent, status: new_status)
      save_agent(agents_dir, updated)
    }
  }
}

/// Get the file path for an agent's state
fn agent_file_path(agents_dir: String, agent_id: String) -> String {
  agents_dir <> "/" <> agent_id <> ".json"
}

// -- JSON Encoding --

fn encode_agent_state(a: AgentState) -> json.Json {
  json.object([
    #("config", encode_agent_config(a.config)),
    #("status", encode_agent_status(a.status)),
    #("task_id", json.string(a.task_id)),
    #("workspace_path", json.string(a.workspace_path)),
    #("pipe_path", json.string(a.pipe_path)),
  ])
}

fn encode_agent_config(c: AgentConfig) -> json.Json {
  json.object([
    #("id", json.string(c.id)),
    #("provider", encode_provider(c.provider)),
    #("working_dir", json.string(c.working_dir)),
    #("max_retries", json.int(c.max_retries)),
    #("timeout_ms", json.int(c.timeout_ms)),
    #("interactive", json.bool(c.interactive)),
  ])
}

fn encode_provider(p: AgentProvider) -> json.Json {
  case p {
    Claude -> json.string("claude")
    Codex -> json.string("codex")
    Gemini -> json.string("gemini")
  }
}

fn encode_agent_status(s: AgentStatus) -> json.Json {
  case s {
    types.Idle -> json.object([#("type", json.string("idle"))])
    types.Running(task_id, started_at) ->
      json.object([
        #("type", json.string("running")),
        #("task_id", json.string(task_id)),
        #("started_at", json.int(started_at)),
      ])
    types.Failed(reason) ->
      json.object([
        #("type", json.string("failed")),
        #("reason", json.string(reason)),
      ])
  }
}

// -- JSON Decoding --

fn decode_agent_state() -> decode.Decoder(AgentState) {
  use config <- decode.field("config", decode_agent_config())
  use status <- decode.field("status", decode_agent_status())
  use task_id <- decode.field("task_id", decode.string)
  use workspace_path <- decode.field("workspace_path", decode.string)
  use pipe_path <- decode.field("pipe_path", decode.string)
  decode.success(AgentState(
    config: config,
    status: status,
    task_id: task_id,
    workspace_path: workspace_path,
    pipe_path: pipe_path,
  ))
}

fn decode_agent_config() -> decode.Decoder(AgentConfig) {
  use id <- decode.field("id", decode.string)
  use provider <- decode.field("provider", decode_provider())
  use working_dir <- decode.field("working_dir", decode.string)
  use max_retries <- decode.field("max_retries", decode.int)
  use timeout_ms <- decode.field("timeout_ms", decode.int)
  use interactive <- decode.optional_field("interactive", False, decode.bool)
  decode.success(AgentConfig(
    id: id,
    provider: provider,
    working_dir: working_dir,
    max_retries: max_retries,
    timeout_ms: timeout_ms,
    interactive: interactive,
  ))
}

fn decode_provider() -> decode.Decoder(AgentProvider) {
  use s <- decode.then(decode.string)
  case s {
    "claude" -> decode.success(Claude)
    "codex" -> decode.success(Codex)
    "gemini" -> decode.success(Gemini)
    _ -> decode.failure(Claude, "AgentProvider")
  }
}

fn decode_agent_status() -> decode.Decoder(AgentStatus) {
  use status_type <- decode.field("type", decode.string)
  case status_type {
    "idle" -> decode.success(types.Idle)
    "running" -> {
      use task_id <- decode.field("task_id", decode.string)
      use started_at <- decode.field("started_at", decode.int)
      decode.success(types.Running(task_id, started_at))
    }
    "failed" -> {
      use reason <- decode.field("reason", decode.string)
      decode.success(types.Failed(reason))
    }
    _ -> decode.failure(types.Idle, "AgentStatus")
  }
}
