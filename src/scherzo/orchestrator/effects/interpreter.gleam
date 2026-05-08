import gleam/list
import scherzo/orchestrator/effects/types as effects_types
import scherzo/orchestrator/transition_types
import scherzo/state/ledger

pub type LedgerAppender =
  fn(effects_types.LedgerAppend) -> Result(Nil, ledger.LedgerError)

pub opaque type ShellState {
  ShellState(
    append_ledger: LedgerAppender,
    now_ms: fn() -> Int,
    started_workers: List(effects_types.WorkerStart),
  )
}

pub type ApplyResult {
  ApplyResult(
    shell: ShellState,
    follow_up_messages: List(transition_types.Message),
  )
}

pub fn new_shell_state(
  append_ledger append_ledger: LedgerAppender,
  now_ms now_ms: fn() -> Int,
) -> ShellState {
  ShellState(append_ledger: append_ledger, now_ms: now_ms, started_workers: [])
}

pub fn started_workers(shell: ShellState) -> List(effects_types.WorkerStart) {
  shell.started_workers
}

pub fn apply(
  shell: ShellState,
  effects: List(effects_types.Effect),
) -> ApplyResult {
  let #(shell, follow_up_messages) = apply_loop(shell, effects, [])
  ApplyResult(
    shell: shell,
    follow_up_messages: list.reverse(follow_up_messages),
  )
}

fn apply_loop(
  shell: ShellState,
  effects: List(effects_types.Effect),
  follow_up_messages: List(transition_types.Message),
) -> #(ShellState, List(transition_types.Message)) {
  case effects {
    [] -> #(shell, follow_up_messages)
    [effect, ..rest] ->
      case effect {
        effects_types.AppendLedger(request) -> {
          let result = shell.append_ledger(request)
          let follow_up_messages =
            append_follow_up(shell, request, result, follow_up_messages)
          case should_stop_after_append(request, result) {
            True -> #(shell, follow_up_messages)
            False -> apply_loop(shell, rest, follow_up_messages)
          }
        }
        effects_types.StartWorker(request) -> {
          let shell =
            ShellState(..shell, started_workers: [
              request,
              ..shell.started_workers
            ])
          apply_loop(shell, rest, follow_up_messages)
        }
        effects_types.Log(_, _, _) ->
          apply_loop(shell, rest, follow_up_messages)
      }
  }
}

fn append_follow_up(
  shell: ShellState,
  request: effects_types.LedgerAppend,
  result: Result(Nil, ledger.LedgerError),
  follow_up_messages: List(transition_types.Message),
) -> List(transition_types.Message) {
  case request.policy {
    effects_types.ContinueWith(continuation) -> [
      transition_types.LedgerAppendCompleted(
        correlation_id: request.correlation_id,
        continuation: continuation,
        result: result,
        now_ms: shell.now_ms(),
      ),
      ..follow_up_messages
    ]
    effects_types.ContinueRegardless | effects_types.StopBatchOnFailure ->
      follow_up_messages
  }
}

fn should_stop_after_append(
  request: effects_types.LedgerAppend,
  result: Result(Nil, ledger.LedgerError),
) -> Bool {
  case request.policy {
    effects_types.StopBatchOnFailure -> result != Ok(Nil)
    effects_types.ContinueRegardless | effects_types.ContinueWith(_) -> False
  }
}
