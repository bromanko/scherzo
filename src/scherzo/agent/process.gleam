/// Agent process actor - manages the lifecycle of a CLI agent
import gleam/erlang/process.{type Subject}
import gleam/io
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/result
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
  // TODO(S-A704): Load and pass custom agent config here
  let workspace_config = workspace.default_config(state.config.working_dir)

  case workspace.create(workspace_config, task, None) {
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
      let agent_result =
        run_command(command, state.driver, state.config.timeout_ms)

      // Update jj change with result in the workspace
      case jj.describe_task_completion(ws.path, task, agent_result) {
        Ok(_) -> Nil
        Error(err) ->
          io.println(
            "Warning: Failed to update jj change for task "
            <> task.id
            <> ": "
            <> err,
          )
      }

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

/// Run a command and get the result, with optional timeout
fn run_command(command: Command, drv: Driver, timeout_ms: Int) -> AgentResult {
  // If timeout is 0 or negative, run without timeout
  case timeout_ms <= 0 {
    True -> run_command_sync(command, drv, command.env)
    False -> run_command_with_timeout(command, drv, command.env, timeout_ms)
  }
}

/// Run command synchronously without timeout
fn run_command_sync(
  command: Command,
  drv: Driver,
  env_vars: List(#(String, String)),
) -> AgentResult {
  let opts = case env_vars {
    [] -> []
    vars -> [shellout.SetEnvironment(vars)]
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

/// Run command with timeout using Erlang spawn
fn run_command_with_timeout(
  command: Command,
  drv: Driver,
  env_vars: List(#(String, String)),
  timeout_ms: Int,
) -> AgentResult {
  // Create a subject to receive the result
  let result_subject: Subject(AgentResult) = process.new_subject()

  // Spawn a process to run the command using Erlang FFI
  let pid =
    erlang_spawn(fn() {
      let opts = case env_vars {
        [] -> []
        vars -> [shellout.SetEnvironment(vars)]
      }
      let result = case
        shellout.command(
          run: command.executable,
          with: command.args,
          in: command.working_dir,
          opt: opts,
        )
      {
        Ok(output) -> driver.detect_result(drv, output, 0)
        Error(#(exit_code, output)) ->
          driver.detect_result(drv, output, exit_code)
      }
      process.send(result_subject, result)
    })

  // Wait for result with timeout
  case process.receive(result_subject, timeout_ms) {
    Ok(result) -> result
    Error(Nil) -> {
      // Timeout occurred - kill the spawned Erlang process
      // This closes the port, sending SIGHUP to the subprocess
      erlang_exit_kill(pid)
      driver.Interrupted
    }
  }
}

/// Spawn an Erlang process (not linked)
@external(erlang, "erlang", "spawn")
fn erlang_spawn(func: fn() -> a) -> process.Pid

/// Kill an Erlang process with exit reason 'kill' (cannot be trapped)
/// This will also close any ports owned by the process
@external(erlang, "scherzo_process_ffi", "exit_kill")
fn erlang_exit_kill(pid: process.Pid) -> Bool
