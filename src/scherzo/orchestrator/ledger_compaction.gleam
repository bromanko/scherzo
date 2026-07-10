import gleam/list
import gleam/option.{type Option, None, Some}
import scherzo/config/types as config_types
import scherzo/state/ledger
import scherzo/state/record

pub type State {
  State(
    current: ledger.CurrentSegmentStats,
    last_attempted_at_ms: Option(Int),
    in_flight: Bool,
  )
}

pub fn new(current: ledger.CurrentSegmentStats) -> State {
  State(current: current, last_attempted_at_ms: None, in_flight: False)
}

pub fn current(state: State) -> ledger.CurrentSegmentStats {
  state.current
}

pub fn after_successful_append(
  state: State,
  records: List(record.LedgerRecord),
) -> State {
  let appended_records = list.length(records)
  let appended_bytes = ledger.records_jsonl_byte_size(records)
  let ledger.CurrentSegmentStats(record_count:, byte_size:, truncated_tail:) =
    state.current
  State(
    ..state,
    current: ledger.CurrentSegmentStats(
      record_count: record_count + appended_records,
      byte_size: byte_size + appended_bytes,
      truncated_tail: truncated_tail,
    ),
  )
}

pub fn should_start(
  state: State,
  config: config_types.LedgerCompactionConfig,
  now_ms: Int,
) -> Bool {
  case config.enabled, state.in_flight, threshold_met(state.current, config) {
    False, _, _ -> False
    _, True, _ -> False
    _, _, False -> False
    True, False, True ->
      min_interval_elapsed(state, config.min_interval_ms, now_ms)
  }
}

pub fn mark_started(state: State, now_ms: Int) -> State {
  State(..state, last_attempted_at_ms: Some(now_ms), in_flight: True)
}

pub fn mark_finished(
  state: State,
  current: ledger.CurrentSegmentStats,
) -> State {
  State(..state, current: current, in_flight: False)
}

pub fn refresh_current(
  state: State,
  current: ledger.CurrentSegmentStats,
) -> State {
  State(..state, current: current)
}

pub fn clear_in_flight(state: State) -> State {
  State(..state, in_flight: False)
}

fn threshold_met(
  current: ledger.CurrentSegmentStats,
  config: config_types.LedgerCompactionConfig,
) -> Bool {
  current.record_count >= config.max_current_records
  || current.byte_size >= config.max_current_bytes
}

fn min_interval_elapsed(
  state: State,
  min_interval_ms: Int,
  now_ms: Int,
) -> Bool {
  case state.last_attempted_at_ms {
    None -> True
    Some(last_attempted_at_ms) ->
      now_ms - last_attempted_at_ms >= min_interval_ms
  }
}
