/// State store actor with in-memory cache and JSON file persistence
import gleam/dict.{type Dict}
import gleam/dynamic/decode
import gleam/erlang/process.{type Subject}
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/result
import scherzo/core/task.{
  type Priority, type Task, type TaskStatus, type TaskType, Assigned, Blocked,
  Bug, Completed, Critical, Epic, Failed, Feature, High, InProgress, Low, Normal,
  Pending, Ready, RegularTask, Task,
}
import scherzo/core/types.{
  type AgentConfig, type AgentProvider, type AgentStatus, type Id, AgentConfig,
  Claude, Codex, Gemini,
}
import simplifile

/// Messages the state store can receive
pub type Message {
  // Task operations
  GetTask(reply_to: Subject(Option(Task)), task_id: Id)
  GetAllTasks(reply_to: Subject(List(Task)))
  SaveTask(task: Task)
  DeleteTask(task_id: Id)

  // Agent operations
  GetAgent(reply_to: Subject(Option(AgentState)), agent_id: Id)
  GetAllAgents(reply_to: Subject(List(AgentState)))
  SaveAgent(agent: AgentState)
  DeleteAgent(agent_id: Id)

  // Persistence
  Persist
  Load

  // Lifecycle
  Shutdown
}

/// Agent state tracked by the store
pub type AgentState {
  AgentState(config: AgentConfig, status: AgentStatus)
}

/// Internal state of the store
pub type State {
  State(tasks: Dict(Id, Task), agents: Dict(Id, AgentState), state_dir: String)
}

/// Configuration for the state store
pub type StoreConfig {
  StoreConfig(state_dir: String)
}

/// Default state directory
pub const default_state_dir = ".scherzo/state"

/// Start a new state store actor
pub fn start(config: StoreConfig) -> Result(Subject(Message), actor.StartError) {
  let initial_state =
    State(tasks: dict.new(), agents: dict.new(), state_dir: config.state_dir)
  actor.new(initial_state)
  |> actor.on_message(handle_message)
  |> actor.start
  |> result.map(fn(started) { started.data })
}

/// Start with default configuration
pub fn start_default() -> Result(Subject(Message), actor.StartError) {
  start(StoreConfig(state_dir: default_state_dir))
}

// Public API functions

pub fn get_task(store: Subject(Message), task_id: Id) -> Option(Task) {
  actor.call(store, 5000, GetTask(_, task_id))
}

pub fn get_all_tasks(store: Subject(Message)) -> List(Task) {
  actor.call(store, 5000, GetAllTasks)
}

pub fn save_task(store: Subject(Message), task: Task) -> Nil {
  actor.send(store, SaveTask(task))
}

pub fn delete_task(store: Subject(Message), task_id: Id) -> Nil {
  actor.send(store, DeleteTask(task_id))
}

pub fn get_agent(store: Subject(Message), agent_id: Id) -> Option(AgentState) {
  actor.call(store, 5000, GetAgent(_, agent_id))
}

pub fn get_all_agents(store: Subject(Message)) -> List(AgentState) {
  actor.call(store, 5000, GetAllAgents)
}

pub fn save_agent(store: Subject(Message), agent: AgentState) -> Nil {
  actor.send(store, SaveAgent(agent))
}

pub fn delete_agent(store: Subject(Message), agent_id: Id) -> Nil {
  actor.send(store, DeleteAgent(agent_id))
}

pub fn persist(store: Subject(Message)) -> Nil {
  actor.send(store, Persist)
}

pub fn load(store: Subject(Message)) -> Nil {
  actor.send(store, Load)
}

pub fn stop(store: Subject(Message)) -> Nil {
  actor.send(store, Shutdown)
}

/// Handle incoming messages
fn handle_message(state: State, message: Message) -> actor.Next(State, Message) {
  case message {
    GetTask(reply_to, task_id) -> {
      let task = dict.get(state.tasks, task_id) |> option.from_result
      process.send(reply_to, task)
      actor.continue(state)
    }

    GetAllTasks(reply_to) -> {
      let tasks = dict.values(state.tasks)
      process.send(reply_to, tasks)
      actor.continue(state)
    }

    SaveTask(task) -> {
      let new_tasks = dict.insert(state.tasks, task.id, task)
      actor.continue(State(..state, tasks: new_tasks))
    }

    DeleteTask(task_id) -> {
      let new_tasks = dict.delete(state.tasks, task_id)
      actor.continue(State(..state, tasks: new_tasks))
    }

    GetAgent(reply_to, agent_id) -> {
      let agent = dict.get(state.agents, agent_id) |> option.from_result
      process.send(reply_to, agent)
      actor.continue(state)
    }

    GetAllAgents(reply_to) -> {
      let agents = dict.values(state.agents)
      process.send(reply_to, agents)
      actor.continue(state)
    }

    SaveAgent(agent) -> {
      let new_agents = dict.insert(state.agents, agent.config.id, agent)
      actor.continue(State(..state, agents: new_agents))
    }

    DeleteAgent(agent_id) -> {
      let new_agents = dict.delete(state.agents, agent_id)
      actor.continue(State(..state, agents: new_agents))
    }

    Persist -> {
      let _ = persist_state(state)
      actor.continue(state)
    }

    Load -> {
      case load_state(state.state_dir) {
        Ok(loaded) -> actor.continue(loaded)
        Error(_) -> actor.continue(state)
      }
    }

    Shutdown -> {
      // Persist before shutdown
      let _ = persist_state(state)
      actor.stop()
    }
  }
}

/// Persist state to JSON files
fn persist_state(state: State) -> Result(Nil, simplifile.FileError) {
  // Ensure directory exists
  use _ <- result.try(simplifile.create_directory_all(state.state_dir))

  // Encode and write tasks
  let tasks_json =
    state.tasks
    |> dict.values
    |> json.array(encode_task)
    |> json.to_string

  use _ <- result.try(simplifile.write(
    state.state_dir <> "/tasks.json",
    tasks_json,
  ))

  // Encode and write agents
  let agents_json =
    state.agents
    |> dict.values
    |> json.array(encode_agent_state)
    |> json.to_string

  simplifile.write(state.state_dir <> "/agents.json", agents_json)
}

/// Load state from JSON files
fn load_state(state_dir: String) -> Result(State, simplifile.FileError) {
  // Check if directory exists
  case simplifile.is_directory(state_dir) {
    Ok(True) -> {
      // Load tasks
      let tasks = case simplifile.read(state_dir <> "/tasks.json") {
        Error(_) -> dict.new()
        Ok(content) ->
          case decode_tasks(content) {
            Error(_) -> dict.new()
            Ok(task_list) ->
              task_list
              |> list.map(fn(t) { #(t.id, t) })
              |> dict.from_list
          }
      }

      // Load agents
      let agents = case simplifile.read(state_dir <> "/agents.json") {
        Error(_) -> dict.new()
        Ok(content) ->
          case decode_agents(content) {
            Error(_) -> dict.new()
            Ok(agent_list) ->
              agent_list
              |> list.map(fn(a) { #(a.config.id, a) })
              |> dict.from_list
          }
      }

      Ok(State(tasks: tasks, agents: agents, state_dir: state_dir))
    }
    _ -> Error(simplifile.Enoent)
  }
}

// -- JSON Encoding --

fn encode_task(t: Task) -> json.Json {
  json.object([
    #("id", json.string(t.id)),
    #("title", json.string(t.title)),
    #("description", json.string(t.description)),
    #("status", encode_task_status(t.status)),
    #("priority", encode_priority(t.priority)),
    #("task_type", encode_task_type(t.task_type)),
    #("dependencies", json.array(t.dependencies, json.string)),
    #("created_at", json.int(t.created_at)),
    #("updated_at", json.int(t.updated_at)),
    #("source_id", encode_option(t.source_id, json.string)),
    #("jj_change_id", encode_option(t.jj_change_id, json.string)),
    #("parent", encode_option(t.parent, json.string)),
  ])
}

fn encode_task_status(s: TaskStatus) -> json.Json {
  case s {
    Pending -> json.object([#("type", json.string("pending"))])
    Ready -> json.object([#("type", json.string("ready"))])
    Assigned(agent_id) ->
      json.object([
        #("type", json.string("assigned")),
        #("agent_id", json.string(agent_id)),
      ])
    InProgress(agent_id, started_at) ->
      json.object([
        #("type", json.string("in_progress")),
        #("agent_id", json.string(agent_id)),
        #("started_at", json.int(started_at)),
      ])
    Completed(agent_id, completed_at) ->
      json.object([
        #("type", json.string("completed")),
        #("agent_id", json.string(agent_id)),
        #("completed_at", json.int(completed_at)),
      ])
    Failed(agent_id, reason, failed_at) ->
      json.object([
        #("type", json.string("failed")),
        #("agent_id", json.string(agent_id)),
        #("reason", json.string(reason)),
        #("failed_at", json.int(failed_at)),
      ])
    Blocked(reason) ->
      json.object([
        #("type", json.string("blocked")),
        #("reason", json.string(reason)),
      ])
  }
}

fn encode_priority(p: Priority) -> json.Json {
  case p {
    Low -> json.string("low")
    Normal -> json.string("normal")
    High -> json.string("high")
    Critical -> json.string("critical")
  }
}

fn encode_task_type(t: TaskType) -> json.Json {
  case t {
    RegularTask -> json.string("task")
    Epic -> json.string("epic")
    Bug -> json.string("bug")
    Feature -> json.string("feature")
  }
}

fn encode_agent_state(a: AgentState) -> json.Json {
  json.object([
    #("config", encode_agent_config(a.config)),
    #("status", encode_agent_status(a.status)),
  ])
}

fn encode_agent_config(c: AgentConfig) -> json.Json {
  json.object([
    #("id", json.string(c.id)),
    #("provider", encode_provider(c.provider)),
    #("working_dir", json.string(c.working_dir)),
    #("max_retries", json.int(c.max_retries)),
    #("timeout_ms", json.int(c.timeout_ms)),
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

fn encode_option(opt: Option(a), encoder: fn(a) -> json.Json) -> json.Json {
  case opt {
    None -> json.null()
    Some(value) -> encoder(value)
  }
}

// -- JSON Decoding --

fn decode_tasks(content: String) -> Result(List(Task), Nil) {
  case json.parse(content, decode.list(decode_task())) {
    Error(_) -> Error(Nil)
    Ok(tasks) -> Ok(tasks)
  }
}

fn decode_task() -> decode.Decoder(Task) {
  use id <- decode.field("id", decode.string)
  use title <- decode.field("title", decode.string)
  use description <- decode.field("description", decode.string)
  use status <- decode.field("status", decode_task_status())
  use priority <- decode.field("priority", decode_priority())
  use task_type_opt <- decode.field(
    "task_type",
    decode.optional(decode_task_type()),
  )
  let task_type = option.unwrap(task_type_opt, RegularTask)
  use dependencies <- decode.field("dependencies", decode.list(decode.string))
  use created_at <- decode.field("created_at", decode.int)
  use updated_at <- decode.field("updated_at", decode.int)
  use source_id <- decode.field("source_id", decode.optional(decode.string))
  use jj_change_id <- decode.field(
    "jj_change_id",
    decode.optional(decode.string),
  )
  use parent <- decode.field("parent", decode.optional(decode.string))
  decode.success(Task(
    id: id,
    title: title,
    description: description,
    status: status,
    priority: priority,
    task_type: task_type,
    dependencies: dependencies,
    created_at: created_at,
    updated_at: updated_at,
    source_id: source_id,
    jj_change_id: jj_change_id,
    parent: parent,
  ))
}

fn decode_task_status() -> decode.Decoder(TaskStatus) {
  use status_type <- decode.field("type", decode.string)
  case status_type {
    "pending" -> decode.success(Pending)
    "ready" -> decode.success(Ready)
    "assigned" -> {
      use agent_id <- decode.field("agent_id", decode.string)
      decode.success(Assigned(agent_id))
    }
    "in_progress" -> {
      use agent_id <- decode.field("agent_id", decode.string)
      use started_at <- decode.field("started_at", decode.int)
      decode.success(InProgress(agent_id, started_at))
    }
    "completed" -> {
      use agent_id <- decode.field("agent_id", decode.string)
      use completed_at <- decode.field("completed_at", decode.int)
      decode.success(Completed(agent_id, completed_at))
    }
    "failed" -> {
      use agent_id <- decode.field("agent_id", decode.string)
      use reason <- decode.field("reason", decode.string)
      use failed_at <- decode.field("failed_at", decode.int)
      decode.success(Failed(agent_id, reason, failed_at))
    }
    "blocked" -> {
      use reason <- decode.field("reason", decode.string)
      decode.success(Blocked(reason))
    }
    _ -> decode.failure(Pending, "TaskStatus")
  }
}

fn decode_priority() -> decode.Decoder(Priority) {
  use s <- decode.then(decode.string)
  case s {
    "low" -> decode.success(Low)
    "normal" -> decode.success(Normal)
    "high" -> decode.success(High)
    "critical" -> decode.success(Critical)
    _ -> decode.failure(Normal, "Priority")
  }
}

fn decode_task_type() -> decode.Decoder(TaskType) {
  use s <- decode.then(decode.string)
  case s {
    "task" -> decode.success(RegularTask)
    "epic" -> decode.success(Epic)
    "bug" -> decode.success(Bug)
    "feature" -> decode.success(Feature)
    _ -> decode.success(RegularTask)
  }
}

fn decode_agents(content: String) -> Result(List(AgentState), Nil) {
  case json.parse(content, decode.list(decode_agent_state())) {
    Error(_) -> Error(Nil)
    Ok(agents) -> Ok(agents)
  }
}

fn decode_agent_state() -> decode.Decoder(AgentState) {
  use config <- decode.field("config", decode_agent_config())
  use status <- decode.field("status", decode_agent_status())
  decode.success(AgentState(config: config, status: status))
}

fn decode_agent_config() -> decode.Decoder(AgentConfig) {
  use id <- decode.field("id", decode.string)
  use provider <- decode.field("provider", decode_provider())
  use working_dir <- decode.field("working_dir", decode.string)
  use max_retries <- decode.field("max_retries", decode.int)
  use timeout_ms <- decode.field("timeout_ms", decode.int)
  decode.success(AgentConfig(
    id: id,
    provider: provider,
    working_dir: working_dir,
    max_retries: max_retries,
    timeout_ms: timeout_ms,
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
