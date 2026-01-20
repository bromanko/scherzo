/// Agent process actor - manages the lifecycle of a CLI agent
import gleam/erlang/process.{type Subject}
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/result
import gleam/string
import scherzo/agent/driver.{type AgentResult, type Command, type Driver}
import scherzo/agent/workspace.{type Workspace}
import scherzo/core/task.{type Task}
import scherzo/core/types.{type AgentConfig, type Id, type Timestamp}
import scherzo/vcs/jj
import shellout

/// Messages the agent process can receive
pub type Message {
  /// Start working on a task
  Start(task: Task, reply_to: Subject(StartResult))
  /// Get current status
  GetStatus(reply_to: Subject(AgentProcessStatus))
  /// Stop the agent (graceful)
  Stop
}

/// Result of starting a task
pub type StartResult {
  Started(change_id: String)
  StartFailed(reason: String)
}

/// Status of the agent process
pub type AgentProcessStatus {
  Idle
  Running(task_id: Id, change_id: String, started_at: Timestamp)
  Completed(task_id: Id, result: AgentResult)
  Failed(reason: String)
}

/// Internal state of the agent process
pub type State {
  State(
    id: Id,
    config: AgentConfig,
    driver: Driver,
    status: AgentProcessStatus,
    /// Active workspace for the current task (if any)
    workspace: Option(Workspace),
  )
}

/// Start a new agent process actor
pub fn start(
  id: Id,
  config: AgentConfig,
  driver: Driver,
) -> Result(Subject(Message), actor.StartError) {
  let initial_state =
    State(id: id, config: config, driver: driver, status: Idle, workspace: None)

  actor.new(initial_state)
  |> actor.on_message(handle_message)
  |> actor.start
  |> result.map(fn(started) { started.data })
}

/// Start working on a task
pub fn start_task(agent: Subject(Message), task: Task) -> StartResult {
  actor.call(agent, 60_000, Start(task, _))
}

/// Get current status
pub fn get_status(agent: Subject(Message)) -> AgentProcessStatus {
  actor.call(agent, 5000, GetStatus)
}

/// Stop the agent
pub fn stop(agent: Subject(Message)) -> Nil {
  actor.send(agent, Stop)
}

/// Handle incoming messages
fn handle_message(state: State, message: Message) -> actor.Next(State, Message) {
  case message {
    Start(task, reply_to) -> handle_start(state, task, reply_to)
    GetStatus(reply_to) -> {
      process.send(reply_to, state.status)
      actor.continue(state)
    }
    Stop -> actor.stop()
  }
}

/// Handle the Start message - run the agent
fn handle_start(
  state: State,
  task: Task,
  reply_to: Subject(StartResult),
) -> actor.Next(State, Message) {
  // Create workspace for this task (defaults to .scherzo/workspaces/ in project)
  let workspace_config = workspace.default_config(state.config.working_dir)

  case workspace.create(workspace_config, task) {
    Error(err) -> {
      process.send(reply_to, StartFailed("Failed to create workspace: " <> err))
      actor.continue(state)
    }

    Ok(ws) -> {
      // Get the workspace's jj change ID for tracking
      // Note: Using "unknown" on error allows task execution to continue even if jj
      // is not available. This is acceptable because change_id is informational only
      // and the task can still complete successfully without it.
      let change_id = case jj.get_current_change(ws.path) {
        Ok(id) -> id
        Error(_) -> "unknown"
      }

      // Build the command - run in the workspace directory
      let workspace_config =
        types.AgentConfig(..state.config, working_dir: ws.path)
      let command = driver.build_command(state.driver, task, workspace_config)

      // Reply that we've started
      process.send(reply_to, Started(change_id))

      // Update state to running with workspace
      let running_state =
        State(
          ..state,
          status: Running(task_id: task.id, change_id: change_id, started_at: 0),
          workspace: Some(ws),
        )

      // Run the agent (this blocks!)
      let agent_result = run_command(command, state.driver)

      // Update jj change with result in the workspace
      let _ = update_change_on_completion(ws.path, task, agent_result)

      // Cleanup workspace
      let _ = workspace.destroy(ws)

      // Update state with result
      let final_state =
        State(
          ..running_state,
          status: Completed(task.id, agent_result),
          workspace: None,
        )

      actor.continue(final_state)
    }
  }
}

/// Run a command and get the result
fn run_command(command: Command, drv: Driver) -> AgentResult {
  // Build options including environment variables if any
  let opts = case command.env {
    [] -> []
    env_vars -> [shellout.SetEnvironment(env_vars)]
  }

  case
    shellout.command(
      run: command.executable,
      with: command.args,
      in: command.working_dir,
      opt: opts,
    )
  {
    Ok(output) -> driver.detect_result(drv, output, 0)
    Error(#(exit_code, output)) -> driver.detect_result(drv, output, exit_code)
  }
}

/// Update the jj change description after completion
fn update_change_on_completion(
  working_dir: String,
  task: Task,
  result: AgentResult,
) -> Result(Nil, String) {
  let description = case result {
    driver.Success(_output) ->
      string.concat([
        task.title,
        "\n\n",
        "Completed successfully.\n\n",
        "Task: ",
        task.id,
      ])

    driver.Failure(reason, _) ->
      string.concat([
        "FAILED: ",
        task.title,
        "\n\n",
        "Error: ",
        reason,
        "\n\n",
        "Task: ",
        task.id,
      ])

    driver.ContextExhausted(_) ->
      string.concat([
        "WIP: ",
        task.title,
        " (context exhausted, needs continuation)",
        "\n\n",
        "Task: ",
        task.id,
      ])

    driver.Interrupted ->
      string.concat(["INTERRUPTED: ", task.title, "\n\n", "Task: ", task.id])
  }

  jj.describe(working_dir, description)
}
