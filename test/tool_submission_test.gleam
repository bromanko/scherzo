import gleam/json
import gleam/option.{Some}
import gleam/string
import scherzo/pi/protocol
import scherzo/tool_submission
import scherzo/workflow_dag

fn spec() -> workflow_dag.ToolSubmissionSpec {
  workflow_dag.ToolSubmissionSpec(
    tool_name: "submit_review_lane_draft",
    artifact_name: "correctness_draft",
    lane_id: "correctness",
    required: True,
    validation_retries: 1,
  )
}

fn draft_json() -> String {
  json.object([
    #("schema_version", json.int(1)),
    #("artifact_type", json.string("review_lane_draft")),
    #("generated_at_utc", json.string("2026-05-10T00:00:00Z")),
    #("producer", json.object([#("name", json.string("test"))])),
    #(
      "lane",
      json.object([
        #("id", json.string("correctness")),
        #("category", json.string("correctness")),
      ]),
    ),
    #("input_refs", json.array([], of: fn(value) { value })),
    #("draft_findings", json.array([], of: fn(value) { value })),
    #("review_notes", json.array([], of: fn(value) { value })),
    #("evidence_requests", json.array([], of: fn(value) { value })),
    #("self_check", json.object([])),
    #("remote_mutations", json.string("none")),
  ])
  |> json.to_string
}

fn tool_record(tool_name: String, input_json: String) -> protocol.RpcRecord {
  let line =
    "{\"type\":\"tool_execution_start\",\"toolName\":\""
    <> tool_name
    <> "\",\"input\":"
    <> input_json
    <> "}"
  let assert Ok(record) = protocol.decode_record(line)
  record
}

fn tool_status_record(tool_name: String, status: String) -> protocol.RpcRecord {
  let line =
    "{\"type\":\"tool_execution_end\",\"toolName\":\""
    <> tool_name
    <> "\",\"status\":\""
    <> status
    <> "\",\"message\":\"tool failed\"}"
  let assert Ok(record) = protocol.decode_record(line)
  record
}

fn final_json_record() -> protocol.RpcRecord {
  let content_json = json.string(draft_json()) |> json.to_string
  let line =
    "{\"type\":\"agent_end\",\"messages\":[{\"role\":\"assistant\",\"content\":"
    <> content_json
    <> "}]}"
  let assert Ok(record) = protocol.decode_record(line)
  record
}

pub fn valid_object_arguments_are_captured_test() {
  let assert Ok(payload) =
    tool_submission.extract_required_tool_payload(
      [tool_record("submit_review_lane_draft", draft_json())],
      spec(),
      [],
    )
  assert payload.tool_name == "submit_review_lane_draft"
  assert payload.submission_source == "pi_tool"
  assert string.contains(payload.payload_json, "review_lane_draft")
}

pub fn final_json_without_tool_is_missing_test() {
  let assert Error(error) =
    tool_submission.extract_required_tool_payload(
      [final_json_record()],
      spec(),
      [],
    )
  assert tool_submission.error_code(error) == "review_lane_draft_tool_missing"
}

pub fn wrong_tool_name_without_submission_is_missing_test() {
  let assert Error(error) =
    tool_submission.extract_required_tool_payload(
      [tool_record("other_tool", draft_json())],
      spec(),
      [],
    )
  assert tool_submission.error_code(error) == "review_lane_draft_tool_missing"
}

pub fn multiple_submissions_are_rejected_test() {
  let assert Error(error) =
    tool_submission.extract_required_tool_payload(
      [
        tool_record("submit_review_lane_draft", draft_json()),
        tool_record("submit_review_lane_draft", draft_json()),
      ],
      spec(),
      [],
    )
  assert tool_submission.error_code(error)
    == "review_lane_draft_multiple_submissions"
}

pub fn failed_tool_status_is_rejected_test() {
  let assert Error(error) =
    tool_submission.extract_required_tool_payload(
      [tool_status_record("submit_review_lane_draft", "failed")],
      spec(),
      [],
    )
  assert tool_submission.error_code(error) == "review_lane_draft_tool_failed"
}

pub fn placeholder_input_without_raw_object_is_rejected_test() {
  let line =
    "{\"type\":\"tool_execution_start\",\"toolName\":\"submit_review_lane_draft\",\"input\":\"[structured tool input; use --json for raw details]\"}"
  let assert Ok(record) = protocol.decode_record(line)
  assert record.tool_input
    == Some("[structured tool input; use --json for raw details]")
  let assert Error(error) =
    tool_submission.extract_required_tool_payload([record], spec(), [])
  assert tool_submission.error_code(error)
    == "review_lane_draft_tool_arguments_invalid"
}

pub fn malformed_raw_arguments_are_rejected_test() {
  let line =
    "{\"type\":\"tool_execution_start\",\"toolName\":\"submit_review_lane_draft\",\"input\":[] }"
  let assert Ok(record) = protocol.decode_record(line)
  let assert Error(error) =
    tool_submission.extract_required_tool_payload([record], spec(), [])
  assert tool_submission.error_code(error)
    == "review_lane_draft_tool_arguments_invalid"
}

pub fn same_message_extra_tool_call_is_rejected_test() {
  let line =
    "{\"type\":\"message\",\"message\":{\"role\":\"assistant\",\"content\":[{\"type\":\"toolCall\",\"name\":\"submit_review_lane_draft\",\"input\":"
    <> draft_json()
    <> "},{\"type\":\"toolCall\",\"name\":\"bash\",\"input\":{\"command\":\"echo no\"}}]}}"
  let assert Ok(record) = protocol.decode_record(line)
  let assert Error(error) =
    tool_submission.extract_required_tool_payload([record], spec(), [])
  assert tool_submission.error_code(error)
    == "review_lane_draft_extra_tool_call"
}
