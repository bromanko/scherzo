import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/agent/pi_rpc
import scherzo/agent/probe
import scherzo/config as config_module
import scherzo/domain
import scherzo/error
import scherzo/log
import scherzo/session/event as session_event
import scherzo/session/redaction
import scherzo/template
import scherzo/tracker
import scherzo/workspace

pub type FinalClassification {
  FinalActive
  FinalTerminal
  FinalNonActive
}

pub type WorkerSuccess {
  WorkerSuccess(
    final_issue: Option(domain.Issue),
    final_classification: FinalClassification,
    workspace_path: String,
    tokens: domain.TokenTotals,
    turns: Int,
  )
}

pub type WorkerFailure {
  WorkerFailure(reason: error.AgentRunnerError, workspace_path: Option(String))
}

pub type PiUpdate {
  PiUpdate(
    event: String,
    message: Option(String),
    raw_json: Option(session_event.RedactedRawJson),
    turn: Option(Int),
    request_id: Option(String),
    method: Option(String),
    pi_session_id: Option(String),
    tokens: domain.TokenTotals,
    tool_name: Option(String),
  )
}

pub fn run_attempt(
  issue: domain.Issue,
  attempt: Option(Int),
  workflow: domain.WorkflowDefinition,
  config: domain.EffectiveConfig,
  tracker_client: tracker.Client,
  emit_update: fn(String, PiUpdate) -> Nil,
) -> Result(WorkerSuccess, WorkerFailure) {
  case workspace.prepare(issue.identifier, config.workspace, config.hooks) {
    Error(workspace.WorkspaceFailure(err)) ->
      Error(WorkerFailure(error.WorkspaceFailed(err), None))
    Error(workspace.HookFailure(err)) ->
      Error(WorkerFailure(error.HookFailedError(err), None))
    Ok(prepared) ->
      run_prepared(
        issue,
        attempt,
        workflow,
        config,
        tracker_client,
        emit_update,
        prepared,
      )
  }
}

fn run_prepared(
  issue: domain.Issue,
  attempt: Option(Int),
  workflow: domain.WorkflowDefinition,
  config: domain.EffectiveConfig,
  tracker_client: tracker.Client,
  emit_update: fn(String, PiUpdate) -> Nil,
  prepared: workspace.PreparedWorkspace,
) -> Result(WorkerSuccess, WorkerFailure) {
  case template.render(workflow.prompt_template, issue, attempt) {
    Error(err) -> {
      let _ = workspace.after_run(prepared.path, config.hooks)
      Error(WorkerFailure(error.PromptFailed(err), Some(prepared.path)))
    }
    Ok(prompt) ->
      case config.pi.compatibility_probe {
        True -> {
          emit_update(issue.id, lifecycle_update("probe_started"))
          case
            probe.probe(
              config.pi.command,
              prepared.path,
              config.pi.read_timeout_ms,
            )
          {
            Error(err) -> {
              let _ = workspace.after_run(prepared.path, config.hooks)
              Error(WorkerFailure(error.ProbeFailed(err), Some(prepared.path)))
            }
            Ok(Nil) -> {
              emit_update(issue.id, lifecycle_update("probe_finished"))
              run_pi_loop(
                issue,
                prompt,
                config,
                tracker_client,
                emit_update,
                prepared.path,
              )
            }
          }
        }
        False ->
          run_pi_loop(
            issue,
            prompt,
            config,
            tracker_client,
            emit_update,
            prepared.path,
          )
      }
  }
}

fn run_pi_loop(
  issue: domain.Issue,
  first_prompt: String,
  config: domain.EffectiveConfig,
  tracker_client: tracker.Client,
  emit_update: fn(String, PiUpdate) -> Nil,
  workspace_path: String,
) -> Result(WorkerSuccess, WorkerFailure) {
  case
    pi_rpc.launch(
      config.pi.command,
      workspace_path,
      issue.identifier <> ": " <> issue.title,
      config.pi.auto_retry,
      config.pi.read_timeout_ms,
    )
  {
    Error(err) -> {
      let _ = workspace.after_run(workspace_path, config.hooks)
      Error(WorkerFailure(error.PiFailed(err), Some(workspace_path)))
    }
    Ok(session) -> {
      emit_update(issue.id, pi_session_started_update(session.session_id))
      loop_turns(
        session,
        issue,
        first_prompt,
        1,
        domain.zero_token_totals(),
        config,
        tracker_client,
        emit_update,
        workspace_path,
      )
    }
  }
}

fn loop_turns(
  session: pi_rpc.Session,
  issue: domain.Issue,
  prompt: String,
  turn: Int,
  totals: domain.TokenTotals,
  config: domain.EffectiveConfig,
  tracker_client: tracker.Client,
  emit_update: fn(String, PiUpdate) -> Nil,
  workspace_path: String,
) -> Result(WorkerSuccess, WorkerFailure) {
  case
    pi_rpc.prompt_with_ui_policy(
      session,
      prompt,
      config.pi.read_timeout_ms,
      config.pi.turn_timeout_ms,
      config.pi.stall_timeout_ms,
      config.pi.ui_request_policy,
      fn(record) {
        emit_update(
          issue.id,
          update_from_record(
            record,
            turn,
            config_module.resolved_secrets(config),
          ),
        )
      },
    )
  {
    Error(err) -> {
      let _ = pi_rpc.terminate(session)
      let _ = workspace.after_run(workspace_path, config.hooks)
      Error(WorkerFailure(error.PiFailed(err), Some(workspace_path)))
    }
    Ok(#(session, _events)) -> {
      case pi_rpc.get_session_stats(session, config.pi.read_timeout_ms) {
        Error(err) -> {
          let _ = pi_rpc.terminate(session)
          let _ = workspace.after_run(workspace_path, config.hooks)
          Error(WorkerFailure(error.PiFailed(err), Some(workspace_path)))
        }
        Ok(#(session, turn_tokens)) -> {
          let totals = add_tokens(totals, turn_tokens)
          emit_update(issue.id, token_update("turn_finished", turn, totals))
          refresh_after_turn(
            session,
            issue,
            turn,
            totals,
            config,
            tracker_client,
            emit_update,
            workspace_path,
          )
        }
      }
    }
  }
}

fn refresh_after_turn(
  session: pi_rpc.Session,
  issue: domain.Issue,
  turn: Int,
  totals: domain.TokenTotals,
  config: domain.EffectiveConfig,
  tracker_client: tracker.Client,
  emit_update: fn(String, PiUpdate) -> Nil,
  workspace_path: String,
) -> Result(WorkerSuccess, WorkerFailure) {
  case tracker_client.fetch_issue_states_by_ids([issue.id]) {
    Error(err) -> {
      let _ = pi_rpc.terminate(session)
      let _ = workspace.after_run(workspace_path, config.hooks)
      Error(WorkerFailure(error.StateRefreshFailed(err), Some(workspace_path)))
    }
    Ok([final_issue]) ->
      decide_after_refresh(
        session,
        final_issue,
        turn,
        totals,
        config,
        tracker_client,
        emit_update,
        workspace_path,
      )
    Ok(_) ->
      decide_after_refresh(
        session,
        issue,
        turn,
        totals,
        config,
        tracker_client,
        emit_update,
        workspace_path,
      )
  }
}

fn decide_after_refresh(
  session: pi_rpc.Session,
  issue: domain.Issue,
  turn: Int,
  totals: domain.TokenTotals,
  config: domain.EffectiveConfig,
  tracker_client: tracker.Client,
  emit_update: fn(String, PiUpdate) -> Nil,
  workspace_path: String,
) -> Result(WorkerSuccess, WorkerFailure) {
  let classification = classify(config, issue.state)
  case classification {
    FinalTerminal | FinalNonActive ->
      finish_success(
        session,
        issue,
        classification,
        workspace_path,
        totals,
        turn,
        config,
      )
    FinalActive ->
      case turn >= config.agent.max_turns {
        True ->
          finish_success(
            session,
            issue,
            FinalActive,
            workspace_path,
            totals,
            turn,
            config,
          )
        False ->
          loop_turns(
            session,
            issue,
            "Continue working on "
              <> issue.identifier
              <> ". Do not repeat the original task prompt; report progress or complete the remaining work.",
            turn + 1,
            totals,
            config,
            tracker_client,
            emit_update,
            workspace_path,
          )
      }
  }
}

fn finish_success(
  session: pi_rpc.Session,
  issue: domain.Issue,
  classification: FinalClassification,
  workspace_path: String,
  totals: domain.TokenTotals,
  turns: Int,
  config: domain.EffectiveConfig,
) -> Result(WorkerSuccess, WorkerFailure) {
  let _ = pi_rpc.terminate(session)
  let _ = workspace.after_run(workspace_path, config.hooks)
  Ok(WorkerSuccess(
    final_issue: Some(issue),
    final_classification: classification,
    workspace_path: workspace_path,
    tokens: totals,
    turns: turns,
  ))
}

fn lifecycle_update(name: String) -> PiUpdate {
  PiUpdate(
    event: name,
    message: None,
    raw_json: None,
    turn: None,
    request_id: None,
    method: None,
    pi_session_id: None,
    tokens: domain.zero_token_totals(),
    tool_name: None,
  )
}

fn pi_session_started_update(pi_session_id: Option(String)) -> PiUpdate {
  PiUpdate(
    event: "pi_session_started",
    message: None,
    raw_json: None,
    turn: None,
    request_id: None,
    method: None,
    pi_session_id: pi_session_id,
    tokens: domain.zero_token_totals(),
    tool_name: None,
  )
}

fn token_update(name: String, turn: Int, tokens: domain.TokenTotals) -> PiUpdate {
  PiUpdate(
    event: name,
    message: None,
    raw_json: None,
    turn: Some(turn),
    request_id: None,
    method: None,
    pi_session_id: None,
    tokens: tokens,
    tool_name: None,
  )
}

fn update_from_record(
  record: pi_rpc.RpcRecord,
  turn: Int,
  secrets: List(String),
) -> PiUpdate {
  PiUpdate(
    event: record.type_,
    message: redact_message(record.delta, secrets),
    raw_json: Some(redaction.redact_raw_json(record.raw_json, secrets)),
    turn: Some(turn),
    request_id: record.id,
    method: record.method,
    pi_session_id: record.session_id,
    tokens: record.tokens,
    tool_name: None,
  )
}

fn redact_message(
  message: Option(String),
  secrets: List(String),
) -> Option(String) {
  case message {
    Some(value) -> Some(log.redact("message", value, secrets))
    None -> None
  }
}

fn classify(
  config: domain.EffectiveConfig,
  state: String,
) -> FinalClassification {
  case contains(config.tracker.terminal_states, state) {
    True -> FinalTerminal
    False ->
      case contains(config.tracker.active_states, state) {
        True -> FinalActive
        False -> FinalNonActive
      }
  }
}

fn contains(states: List(String), state: String) -> Bool {
  list.any(states, fn(s) { string_lower(s) == string_lower(state) })
}

fn string_lower(value: String) -> String {
  value |> string.trim |> string.lowercase
}

fn add_tokens(
  a: domain.TokenTotals,
  b: domain.TokenTotals,
) -> domain.TokenTotals {
  domain.TokenTotals(
    input: a.input + b.input,
    output: a.output + b.output,
    cache_read: a.cache_read + b.cache_read,
    cache_write: a.cache_write + b.cache_write,
    total: a.total + b.total,
  )
}
