import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import scherzo/control/remote/ui_protocol
import scherzo/task
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state
import scherzo/work_item_invalidation

pub fn invalidation_single_task_payload_contains_identity_source_and_ref_test() {
  let event =
    work_item_invalidation.new(
      work_item_invalidation.PollRefresh,
      [
        work_item_invalidation.AffectedTaskRef(
          "linear",
          "issue-1",
          Some("LIV-1"),
        ),
      ],
      has_unknown_refs: False,
    )

  let payload =
    ui_protocol.encode_work_item_invalidation(
      "daemon_abc",
      "boot_abc",
      42,
      None,
      event,
    )

  assert string.contains(payload, "\"type\":\"work_item_invalidation\"")
  assert string.contains(payload, "\"daemonId\":\"daemon_abc\"")
  assert string.contains(payload, "\"bootId\":\"boot_abc\"")
  assert string.contains(payload, "\"source\":\"poll_refresh\"")
  assert string.contains(payload, "\"provider\":\"linear\"")
  assert string.contains(payload, "\"id\":\"issue-1\"")
  assert string.contains(payload, "\"displayId\":\"LIV-1\"")
  assert string.contains(payload, "\"hasUnknownRefs\":false")
  assert string.contains(payload, "\"refsTruncated\":false")
}

pub fn invalidation_multiple_task_refs_are_deduplicated_test() {
  let event =
    work_item_invalidation.new(
      work_item_invalidation.TrackerRefresh,
      [
        work_item_invalidation.AffectedTaskRef(
          "linear",
          "issue-1",
          Some("LIV-1"),
        ),
        work_item_invalidation.AffectedTaskRef(
          "linear",
          "issue-2",
          Some("LIV-2"),
        ),
        work_item_invalidation.AffectedTaskRef(
          "linear",
          "issue-1",
          Some("LIV-1"),
        ),
      ],
      has_unknown_refs: False,
    )

  assert list.length(event.task_refs) == 2
  assert event.source == work_item_invalidation.TrackerRefresh
  assert !event.has_unknown_refs
}

pub fn invalidation_empty_refs_mark_unknown_scope_test() {
  let event =
    work_item_invalidation.unknown(work_item_invalidation.ManualRefresh)
  let payload =
    ui_protocol.encode_work_item_invalidation(
      "daemon_abc",
      "boot_abc",
      43,
      Some("dev daemon"),
      event,
    )

  assert event.task_refs == []
  assert event.has_unknown_refs
  assert string.contains(payload, "\"daemonLabel\":\"dev daemon\"")
  assert string.contains(payload, "\"source\":\"manual_refresh\"")
  assert string.contains(payload, "\"taskRefs\":[]")
  assert string.contains(payload, "\"hasUnknownRefs\":true")
}

pub fn invalidation_payload_bounds_ref_count_and_fields_test() {
  let refs = many_refs(work_item_invalidation.max_task_refs + 3)
  let event =
    work_item_invalidation.new(
      work_item_invalidation.WorkflowObserved,
      [
        work_item_invalidation.AffectedTaskRef(
          " linear ",
          " issue-trimmed ",
          Some(" LIV-TRIM "),
        ),
        work_item_invalidation.AffectedTaskRef(
          long_string(work_item_invalidation.max_ref_field_length + 1),
          "issue-dropped",
          None,
        ),
        ..refs
      ],
      has_unknown_refs: False,
    )

  assert list.length(event.task_refs) == work_item_invalidation.max_task_refs
  assert event.refs_truncated
  assert event.has_unknown_refs
  let assert [first, ..] = event.task_refs
  assert first.provider == "linear"
  assert first.id == "issue-trimmed"
  assert first.display_id == Some("LIV-TRIM")
}

pub fn invalidation_from_issues_redacts_provider_bodies_comments_and_prompts_test() {
  let secret = "dcred_secret_1"
  let issue =
    tracker_issue.Issue(
      id: "issue-1",
      identifier: "LIV-1",
      title: "raw provider title " <> secret,
      description: Some("raw comment body and prompt " <> secret),
      priority: None,
      state: issue_state.from_string_unchecked("Todo"),
      branch_name: Some("branch-with-" <> secret),
      url: Some("https://tracker.example/issue-1?token=" <> secret),
      labels: ["label-" <> secret],
      blocked_by: [
        tracker_issue.BlockerRef(
          id: Some("blocked-" <> secret),
          identifier: Some("LIV-0"),
          state: None,
        ),
      ],
      blocked_by_complete: True,
      created_at: None,
      updated_at: None,
    )
  let event =
    work_item_invalidation.from_issues(
      work_item_invalidation.TrackerRefresh,
      "linear",
      [issue],
    )
  let payload =
    ui_protocol.encode_client_message(ui_protocol.WorkItemInvalidation(
      "daemon_abc",
      "boot_abc",
      44,
      None,
      event,
    ))

  assert string.contains(payload, "\"id\":\"issue-1\"")
  assert string.contains(payload, "\"displayId\":\"LIV-1\"")
  assert !string.contains(payload, secret)
  assert !string.contains(payload, "raw comment")
  assert !string.contains(payload, "prompt")
  assert !string.contains(payload, "token=")
}

pub fn invalidation_from_task_ref_uses_remote_id_and_display_key_test() {
  let event =
    work_item_invalidation.from_task_refs(
      work_item_invalidation.WorkflowObserved,
      [
        task.TaskRef(
          backend_kind: "linear",
          remote_id: "issue-1",
          key: Some("LIV-1"),
          url: Some("https://tracker.example/LIV-1"),
        ),
      ],
    )

  let assert [ref] = event.task_refs
  assert ref.provider == "linear"
  assert ref.id == "issue-1"
  assert ref.display_id == Some("LIV-1")
}

fn many_refs(count: Int) -> List(work_item_invalidation.AffectedTaskRef) {
  many_refs_loop(1, count, [])
}

fn many_refs_loop(
  next: Int,
  count: Int,
  acc: List(work_item_invalidation.AffectedTaskRef),
) -> List(work_item_invalidation.AffectedTaskRef) {
  case next > count {
    True -> acc
    False ->
      many_refs_loop(
        next + 1,
        count,
        list.append(acc, [
          work_item_invalidation.AffectedTaskRef(
            "linear",
            "issue-" <> int.to_string(next),
            Some("LIV-" <> int.to_string(next)),
          ),
        ]),
      )
  }
}

fn long_string(length: Int) -> String {
  long_string_loop(length, "")
}

fn long_string_loop(remaining: Int, acc: String) -> String {
  case remaining <= 0 {
    True -> acc
    False -> long_string_loop(remaining - 1, acc <> "x")
  }
}
