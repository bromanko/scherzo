import gleam/bit_array
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order.{Gt, Lt}
import gleam/string
import scherzo/agent/runner
import scherzo/domain
import scherzo/error
import scherzo/handoff_format
import scherzo/linear
import scherzo/linear_attachment

pub type Client {
  Client(
    claim_issue: fn(domain.Issue, String) -> Result(Nil, error.TrackerError),
    report_success: fn(domain.Issue, runner.WorkerSuccess, String) ->
      Result(Nil, error.TrackerError),
    report_failure: fn(domain.Issue, runner.WorkerFailure, String) ->
      Result(Nil, error.TrackerError),
  )
}

pub fn disabled_client() -> Client {
  Client(
    claim_issue: fn(_, _) { Ok(Nil) },
    report_success: fn(_, _, _) { Ok(Nil) },
    report_failure: fn(_, _, _) { Ok(Nil) },
  )
}

pub fn linear_client(
  tracker_config: domain.TrackerConfig,
  handoff_config: domain.HandoffConfig,
  transport: linear.Transport,
) -> Client {
  linear_client_with_attachment_dependencies(
    tracker_config,
    handoff_config,
    linear_attachment.real_dependencies(transport),
  )
}

pub fn linear_client_with_attachment_dependencies(
  tracker_config: domain.TrackerConfig,
  handoff_config: domain.HandoffConfig,
  dependencies: linear_attachment.Dependencies,
) -> Client {
  case handoff_config.enabled {
    False -> disabled_client()
    True ->
      Client(
        claim_issue: fn(issue, run_id) {
          claim_issue(
            tracker_config,
            handoff_config,
            dependencies.graphql_transport,
            issue,
            run_id,
          )
        },
        report_success: fn(issue, success, run_id) {
          report_success(
            tracker_config,
            handoff_config,
            dependencies,
            issue,
            success,
            run_id,
          )
        },
        report_failure: fn(issue, failure, run_id) {
          report_failure(
            tracker_config,
            handoff_config,
            dependencies.graphql_transport,
            issue,
            failure,
            run_id,
          )
        },
      )
  }
}

fn claim_issue(
  tracker_config: domain.TrackerConfig,
  handoff_config: domain.HandoffConfig,
  transport: linear.Transport,
  issue: domain.Issue,
  run_id: String,
) -> Result(Nil, error.TrackerError) {
  use _ <- try_tracker(run_comment(
    handoff_config.comment_on_claim,
    tracker_config,
    transport,
    issue.id,
    "Scherzo claimed " <> issue.identifier <> " for run " <> run_id <> ".",
  ))
  run_state_update(
    tracker_config,
    transport,
    issue.id,
    handoff_config.claim_state_id,
  )
}

fn report_success(
  tracker_config: domain.TrackerConfig,
  handoff_config: domain.HandoffConfig,
  dependencies: linear_attachment.Dependencies,
  issue: domain.Issue,
  success: runner.WorkerSuccess,
  run_id: String,
) -> Result(Nil, error.TrackerError) {
  case handoff_config.attach_result_on_success {
    False -> {
      use _ <- try_tracker(run_comment(
        handoff_config.comment_on_success,
        tracker_config,
        dependencies.graphql_transport,
        issue.id,
        handoff_format.success_comment(
          issue,
          success,
          run_id,
          handoff_config.include_result_on_success,
          tracker_secrets(tracker_config),
        ),
      ))
      run_state_update(
        tracker_config,
        dependencies.graphql_transport,
        issue.id,
        handoff_config.success_state_id,
      )
    }
    True -> {
      use comment <- try_tracker(create_success_comment(
        tracker_config,
        dependencies.graphql_transport,
        issue,
        success,
        run_id,
        handoff_config.include_result_on_success,
        tracker_secrets(tracker_config),
      ))
      use _ <- try_tracker(maybe_attach_success_result(
        tracker_config,
        handoff_config,
        dependencies,
        issue,
        success,
        run_id,
        comment.id,
      ))
      run_state_update(
        tracker_config,
        dependencies.graphql_transport,
        issue.id,
        handoff_config.success_state_id,
      )
    }
  }
}

fn report_failure(
  tracker_config: domain.TrackerConfig,
  handoff_config: domain.HandoffConfig,
  transport: linear.Transport,
  issue: domain.Issue,
  failure: runner.WorkerFailure,
  run_id: String,
) -> Result(Nil, error.TrackerError) {
  use _ <- try_tracker(run_comment(
    handoff_config.comment_on_failure,
    tracker_config,
    transport,
    issue.id,
    handoff_format.failure_comment(
      issue,
      failure,
      run_id,
      tracker_secrets(tracker_config),
    ),
  ))
  run_state_update(
    tracker_config,
    transport,
    issue.id,
    handoff_config.failure_state_id,
  )
}

fn create_success_comment(
  tracker_config: domain.TrackerConfig,
  transport: linear.Transport,
  issue: domain.Issue,
  success: runner.WorkerSuccess,
  run_id: String,
  include_result: Bool,
  secrets: List(String),
) -> Result(linear.LinearCommentDocument, error.TrackerError) {
  use request <- try_tracker(linear.build_comment_create_request(
    tracker_config,
    issue.id,
    handoff_format.success_comment(
      issue,
      success,
      run_id,
      include_result,
      secrets,
    ),
  ))
  use response <- try_tracker(transport(request))
  linear.parse_comment_create_response(response)
}

fn maybe_attach_success_result(
  tracker_config: domain.TrackerConfig,
  handoff_config: domain.HandoffConfig,
  dependencies: linear_attachment.Dependencies,
  issue: domain.Issue,
  success: runner.WorkerSuccess,
  run_id: String,
  comment_id: String,
) -> Result(Nil, error.TrackerError) {
  let attachment_success =
    limit_success_result_for_attachment(
      success,
      handoff_config.result_max_chars,
    )
  case
    handoff_format.success_result_attachment_markdown(
      issue,
      attachment_success,
      run_id,
      tracker_secrets(tracker_config),
    )
  {
    None -> Ok(Nil)
    Some(markdown) -> {
      let body = bit_array.from_string(markdown)
      use _ <- try_tracker(
        linear_attachment.validate_attachment_size(bit_array.byte_size(body)),
      )
      use _ <- try_tracker(linear_attachment.attach_markdown_to_comment(
        tracker_config,
        comment_id,
        handoff_result_filename(issue.identifier, run_id),
        body,
        linear_attachment.AttachOptions(
          fallback_to_markdown_link: handoff_config.attachment_fallback_to_markdown_link,
          dedupe_by_filename: True,
        ),
        dependencies,
      ))
      Ok(Nil)
    }
  }
}

fn limit_success_result_for_attachment(
  success: runner.WorkerSuccess,
  max_chars: Int,
) -> runner.WorkerSuccess {
  case success.result.final_response {
    None -> success
    Some(text) ->
      case string.length(text) > max_chars {
        False -> success
        True ->
          runner.WorkerSuccess(
            ..success,
            result: domain.ResultArtifact(
              ..success.result,
              final_response: Some(string.slice(
                from: text,
                at_index: 0,
                length: max_chars,
              )),
              truncated: True,
            ),
          )
      }
  }
}

fn handoff_result_filename(issue_identifier: String, run_id: String) -> String {
  let base =
    safe_filename_component(issue_identifier)
    <> "-"
    <> safe_filename_component(run_id)
    |> collapse_repeated_hyphens
    |> trim_hyphens
  let base = case base == "" {
    True -> "scherzo-result"
    False -> base
  }
  base <> "-result.md"
}

fn safe_filename_component(value: String) -> String {
  value
  |> string.lowercase
  |> string.to_graphemes
  |> list.map(fn(ch) {
    case is_filename_char(ch) {
      True -> ch
      False -> "-"
    }
  })
  |> string.join(with: "")
}

fn is_filename_char(ch: String) -> Bool {
  is_between(ch, "a", "z")
  || is_between(ch, "0", "9")
  || ch == "."
  || ch == "_"
  || ch == "-"
}

fn is_between(value: String, low: String, high: String) -> Bool {
  string.compare(value, low) != Lt && string.compare(value, high) != Gt
}

fn collapse_repeated_hyphens(value: String) -> String {
  value
  |> string.to_graphemes
  |> collapse_hyphen_graphemes(False, [])
  |> string.join(with: "")
}

fn collapse_hyphen_graphemes(
  graphemes: List(String),
  previous_was_hyphen: Bool,
  acc: List(String),
) -> List(String) {
  case graphemes {
    [] -> list.reverse(acc)
    ["-", ..rest] ->
      case previous_was_hyphen {
        True -> collapse_hyphen_graphemes(rest, True, acc)
        False -> collapse_hyphen_graphemes(rest, True, ["-", ..acc])
      }
    [ch, ..rest] -> collapse_hyphen_graphemes(rest, False, [ch, ..acc])
  }
}

fn trim_hyphens(value: String) -> String {
  value
  |> string.to_graphemes
  |> drop_leading_hyphens
  |> list.reverse
  |> drop_leading_hyphens
  |> list.reverse
  |> string.join(with: "")
}

fn drop_leading_hyphens(values: List(String)) -> List(String) {
  case values {
    ["-", ..rest] -> drop_leading_hyphens(rest)
    _ -> values
  }
}

fn run_comment(
  enabled: Bool,
  tracker_config: domain.TrackerConfig,
  transport: linear.Transport,
  issue_id: String,
  body: String,
) -> Result(Nil, error.TrackerError) {
  case enabled {
    False -> Ok(Nil)
    True -> {
      use request <- try_tracker(linear.build_comment_create_request(
        tracker_config,
        issue_id,
        body,
      ))
      use response <- try_tracker(transport(request))
      linear.parse_mutation_response(response, "commentCreate")
    }
  }
}

fn run_state_update(
  tracker_config: domain.TrackerConfig,
  transport: linear.Transport,
  issue_id: String,
  state_id: Option(String),
) -> Result(Nil, error.TrackerError) {
  case state_id {
    None -> Ok(Nil)
    Some(state_id) -> {
      use request <- try_tracker(linear.build_issue_update_state_request(
        tracker_config,
        issue_id,
        state_id,
      ))
      use response <- try_tracker(transport(request))
      linear.parse_mutation_response(response, "issueUpdate")
    }
  }
}

fn tracker_secrets(tracker_config: domain.TrackerConfig) -> List(String) {
  case tracker_config.api_key {
    Some(value) -> [value]
    None -> []
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
