import gleam/list
import scherzo/log
import scherzo/orchestrator/effects/types as effects_types
import scherzo/orchestrator/state as orchestrator_state
import scherzo/orchestrator/transition_types
import scherzo/state/ledger

pub type LedgerAppender =
  fn(effects_types.LedgerAppend) -> Result(Nil, ledger.LedgerError)

pub opaque type ShellState(shell) {
  ShellState(
    data: shell,
    append_ledger: fn(shell, effects_types.LedgerAppend) ->
      #(shell, Result(Nil, ledger.LedgerError)),
    now_ms: fn(shell) -> Int,
    log_effect: fn(shell, String, String, List(log.Field)) -> shell,
    start_worker: fn(shell, effects_types.WorkerStart) -> shell,
    reply_snapshot: fn(shell, orchestrator_state.RuntimeState) -> shell,
  )
}

pub type ApplyResult(shell) {
  ApplyResult(
    shell: ShellState(shell),
    follow_up_messages: List(transition_types.Message),
  )
}

pub fn new_shell_state(
  append_ledger append_ledger: LedgerAppender,
  now_ms now_ms: fn() -> Int,
) -> ShellState(List(effects_types.WorkerStart)) {
  ShellState(
    data: [],
    append_ledger: fn(started_workers, request) {
      #(started_workers, append_ledger(request))
    },
    now_ms: fn(_) { now_ms() },
    log_effect: fn(started_workers, _, _, _) { started_workers },
    start_worker: fn(started_workers, request) {
      list.append(started_workers, [request])
    },
    reply_snapshot: fn(started_workers, _) { started_workers },
  )
}

pub fn new_production_shell_state(
  data data: shell,
  append_ledger append_ledger: fn(shell, effects_types.LedgerAppend) ->
    #(shell, Result(Nil, ledger.LedgerError)),
  now_ms now_ms: fn(shell) -> Int,
  log_effect log_effect: fn(shell, String, String, List(log.Field)) -> shell,
  start_worker start_worker: fn(shell, effects_types.WorkerStart) -> shell,
  reply_snapshot reply_snapshot: fn(shell, orchestrator_state.RuntimeState) ->
    shell,
) -> ShellState(shell) {
  ShellState(
    data: data,
    append_ledger: append_ledger,
    now_ms: now_ms,
    log_effect: log_effect,
    start_worker: start_worker,
    reply_snapshot: reply_snapshot,
  )
}

pub fn data(shell: ShellState(shell)) -> shell {
  shell.data
}

pub fn started_workers(
  shell: ShellState(List(effects_types.WorkerStart)),
) -> List(effects_types.WorkerStart) {
  shell.data
}

pub fn apply(
  shell: ShellState(shell),
  effects: List(effects_types.Effect),
) -> ApplyResult(shell) {
  let #(shell, follow_up_messages) = apply_loop(shell, effects, [])
  ApplyResult(
    shell: shell,
    follow_up_messages: list.reverse(follow_up_messages),
  )
}

fn apply_loop(
  shell: ShellState(shell),
  effects: List(effects_types.Effect),
  follow_up_messages: List(transition_types.Message),
) -> #(ShellState(shell), List(transition_types.Message)) {
  case effects {
    [] -> #(shell, follow_up_messages)
    [effect, ..rest] ->
      case effect {
        effects_types.AppendLedger(request) -> {
          let #(data, result) = shell.append_ledger(shell.data, request)
          let shell = ShellState(..shell, data: data)
          let follow_up_messages =
            append_follow_up(shell, request, result, follow_up_messages)
          case should_stop_after_append(request, result) {
            True -> #(shell, follow_up_messages)
            False -> apply_loop(shell, rest, follow_up_messages)
          }
        }
        effects_types.StartWorker(request) -> {
          let data = shell.start_worker(shell.data, request)
          let shell = ShellState(..shell, data: data)
          apply_loop(shell, rest, follow_up_messages)
        }
        effects_types.Log(level, event, fields) -> {
          let data = shell.log_effect(shell.data, level, event, fields)
          let shell = ShellState(..shell, data: data)
          apply_loop(shell, rest, follow_up_messages)
        }
        effects_types.ReplySnapshot(snapshot) -> {
          let data = shell.reply_snapshot(shell.data, snapshot)
          let shell = ShellState(..shell, data: data)
          apply_loop(shell, rest, follow_up_messages)
        }
      }
  }
}

fn append_follow_up(
  shell: ShellState(shell),
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
        now_ms: shell.now_ms(shell.data),
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
