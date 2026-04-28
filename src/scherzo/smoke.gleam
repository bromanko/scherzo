import gleam/list
import gleam/option.{type Option, None, Some}
import scherzo/domain
import scherzo/error
import scherzo/linear

pub type LinearSmokeReader {
  LinearSmokeReader(
    fetch_candidate_sample: fn() ->
      Result(List(domain.Issue), error.TrackerError),
    fetch_terminal_sample: fn(List(String)) ->
      Result(List(domain.Issue), error.TrackerError),
    refresh_issue_states_by_ids: fn(List(String)) ->
      Result(List(domain.Issue), error.TrackerError),
  )
}

pub type LinearSmokeResult {
  LinearSmokeResult(
    candidate_count: Int,
    terminal_count: Int,
    refreshed_count: Int,
  )
}

pub fn linear_read_smoke(
  reader: LinearSmokeReader,
  terminal_states: List(String),
) -> Result(LinearSmokeResult, error.TrackerError) {
  use candidates <- try_tracker(reader.fetch_candidate_sample())
  use terminals <- try_tracker(reader.fetch_terminal_sample(terminal_states))
  use refreshed <- try_tracker(refresh_first_sampled_issue(
    reader,
    candidates,
    terminals,
  ))
  Ok(LinearSmokeResult(
    candidate_count: list.length(candidates),
    terminal_count: list.length(terminals),
    refreshed_count: list.length(refreshed),
  ))
}

pub fn linear_reader(
  config: domain.TrackerConfig,
  transport: linear.Transport,
) -> LinearSmokeReader {
  LinearSmokeReader(
    fetch_candidate_sample: fn() {
      fetch_one_page(config, config.active_states, transport)
    },
    fetch_terminal_sample: fn(states) {
      fetch_one_page(config, states, transport)
    },
    refresh_issue_states_by_ids: fn(ids) {
      linear.fetch_issue_states_by_ids(config, ids, transport)
    },
  )
}

pub fn real_linear_reader(config: domain.TrackerConfig) -> LinearSmokeReader {
  linear_reader(config, linear.http_transport)
}

fn fetch_one_page(
  config: domain.TrackerConfig,
  states: List(String),
  transport: linear.Transport,
) -> Result(List(domain.Issue), error.TrackerError) {
  case states {
    [] -> Ok([])
    _ -> {
      use request <- try_tracker(linear.build_candidate_request(
        config,
        states,
        None,
      ))
      use response <- try_tracker(transport(request))
      use page <- try_tracker(linear.parse_page_response(response))
      Ok(page.nodes)
    }
  }
}

fn refresh_first_sampled_issue(
  reader: LinearSmokeReader,
  candidates: List(domain.Issue),
  terminals: List(domain.Issue),
) -> Result(List(domain.Issue), error.TrackerError) {
  case first_issue_id(candidates) {
    Some(id) -> reader.refresh_issue_states_by_ids([id])
    None ->
      case first_issue_id(terminals) {
        Some(id) -> reader.refresh_issue_states_by_ids([id])
        None -> Ok([])
      }
  }
}

fn first_issue_id(issues: List(domain.Issue)) -> Option(String) {
  case issues {
    [issue, ..] -> Some(issue.id)
    [] -> None
  }
}

fn try_tracker(
  result: Result(a, error.TrackerError),
  next: fn(a) -> Result(b, error.TrackerError),
) -> Result(b, error.TrackerError) {
  case result {
    Ok(value) -> next(value)
    Error(err) -> Error(err)
  }
}
