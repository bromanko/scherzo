/// State store actor with in-memory cache and JSON file persistence
import gleam/dict.{type Dict}
import gleam/erlang/process.{type Subject}
import gleam/option.{type Option}
import gleam/otp/actor
import gleam/result
import scherzo/core/task.{type Task}
import scherzo/core/types.{type AgentConfig, type AgentStatus, type Id}
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
  let _ = simplifile.create_directory_all(state.state_dir)

  // For now, just ensure the directory structure exists
  // Full JSON serialization will be implemented with proper encoders
  Ok(Nil)
}

/// Load state from JSON files
fn load_state(state_dir: String) -> Result(State, simplifile.FileError) {
  // Check if directory exists
  case simplifile.is_directory(state_dir) {
    Ok(True) -> {
      // For now, return empty state - full deserialization will be implemented
      Ok(State(tasks: dict.new(), agents: dict.new(), state_dir: state_dir))
    }
    _ -> Error(simplifile.Enoent)
  }
}
