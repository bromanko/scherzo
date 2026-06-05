import gleam/dict.{type Dict}
import gleam/list
import scherzo/agent/types as agent_types
import scherzo/session/tokens as session_tokens
import scherzo/workflow_run

pub type StepTokenEntries =
  Dict(String, StepTokenEntry)

pub type StepTokenEntry {
  StepTokenEntry(
    run_id: String,
    parent_session_id: String,
    tokens: session_tokens.TokenTotals,
  )
}

pub fn new() -> StepTokenEntries {
  dict.new()
}

pub fn register_step(
  entries: StepTokenEntries,
  session_id: String,
  run_id: String,
  parent_session_id: String,
) -> StepTokenEntries {
  dict.insert(
    entries,
    session_id,
    StepTokenEntry(
      run_id: run_id,
      parent_session_id: parent_session_id,
      tokens: session_tokens.zero_token_totals(),
    ),
  )
}

pub fn update_from_runner(
  entries: StepTokenEntries,
  session_id: String,
  runner_update: agent_types.RunnerUpdate,
) -> StepTokenEntries {
  let tokens = case runner_update {
    agent_types.RunnerPiUpdate(update) -> update.tokens
    agent_types.RunnerTurnUpdate(update) -> update.tokens
  }
  update_tokens(entries, session_id, tokens)
}

pub fn update_tokens(
  entries: StepTokenEntries,
  session_id: String,
  tokens: session_tokens.TokenTotals,
) -> StepTokenEntries {
  case session_tokens.nonzero(tokens) {
    False -> entries
    True ->
      case dict.get(entries, session_id) {
        Ok(entry) ->
          dict.insert(
            entries,
            session_id,
            StepTokenEntry(..entry, tokens: tokens),
          )
        Error(Nil) -> entries
      }
  }
}

pub fn total(entries: StepTokenEntries) -> session_tokens.TokenTotals {
  entries
  |> dict.values
  |> list.fold(session_tokens.zero_token_totals(), fn(total, entry) {
    session_tokens.add(total, entry.tokens)
  })
}

pub fn worker_result_tokens(
  result: Result(agent_types.WorkerSuccess, agent_types.WorkerFailure),
) -> session_tokens.TokenTotals {
  case result {
    Ok(success) -> success.tokens
    Error(failure) -> failure.tokens
  }
}

pub fn workflow_run_result_tokens(
  result: Result(
    workflow_run.WorkflowRunSuccess,
    workflow_run.WorkflowRunFailure,
  ),
) -> session_tokens.TokenTotals {
  case result {
    Ok(success) -> success.worker_success.tokens
    Error(failure) -> {
      let _failure_report = workflow_run.failure_report(failure)
      session_tokens.zero_token_totals()
    }
  }
}

pub fn total_for_run(
  entries: StepTokenEntries,
  run_id: String,
) -> session_tokens.TokenTotals {
  entries
  |> dict.values
  |> list.filter(fn(entry) { entry.run_id == run_id })
  |> list.fold(session_tokens.zero_token_totals(), fn(total, entry) {
    session_tokens.add(total, entry.tokens)
  })
}

pub fn remove_run(
  entries: StepTokenEntries,
  run_id: String,
) -> StepTokenEntries {
  entries
  |> dict.to_list
  |> list.filter(fn(entry) {
    let #(_, token_entry) = entry
    token_entry.run_id != run_id
  })
  |> dict.from_list
}
