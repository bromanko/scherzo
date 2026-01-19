/// Agent process actor - manages the lifecycle of a CLI agent

import gleam/erlang/process.{type Subject}
import gleam/otp/actor
import gleam/result
import gleam/string
import scherzo/agent/driver.{type AgentResult, type Command, type Driver}
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
  )
}

/// Start a new agent process actor
pub fn start(
  id: Id,
  config: AgentConfig,
  driver: Driver,
) -> Result(Subject(Message), actor.StartError) {
  let initial_state =
    State(id: id, config: config, driver: driver, status: Idle)

  actor.new(initial_state)
  |> actor.on_message(handle_message)
  |> actor.start
  |> result.map(fn(started) { started.data })
}

/// Start working on a task
pub fn start_task(
  agent: Subject(Message),
  task: Task,
) -> StartResult {
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
  // Create a new jj change for this task
  let change_result =
    jj.new_change(state.config.working_dir, "WIP: " <> task.title)

  case change_result {
    Error(err) -> {
      process.send(reply_to, StartFailed("Failed to create jj change: " <> err))
      actor.continue(state)
    }

    Ok(change_id) -> {
      // Build the command
      let command = driver.build_command(state.driver, task, state.config)

      // Reply that we've started
      process.send(reply_to, Started(change_id))

      // Update state to running
      let running_state =
        State(
          ..state,
          status: Running(task_id: task.id, change_id: change_id, started_at: 0),
        )

      // Run the agent (this blocks!)
      let agent_result = run_command(command, state.driver)

      // Update jj change with result
      let _ = update_change_on_completion(state.config.working_dir, task, agent_result)

      // Update state with result
      let final_state =
        State(..running_state, status: Completed(task.id, agent_result))

      actor.continue(final_state)
    }
  }
}

/// Run a command and get the result
fn run_command(command: Command, drv: Driver) -> AgentResult {
  case
    shellout.command(
      run: command.executable,
      with: command.args,
      in: command.working_dir,
      opt: [],
    )
  {
    Ok(output) -> driver.detect_result(drv, output, 0)
    Error(#(exit_code, output)) ->
      driver.detect_result(drv, output, exit_code)
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
