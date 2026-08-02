import gleam/option.{type Option, None, Some}
import scherzo/result_artifact
import scherzo/review_lane_live_probe

pub fn failed_then_successful_probe_tool_call_retains_correction_test() {
  let phase =
    review_lane_live_probe.validate_phase_tool_calls(
      "lane_correctness",
      "registration",
      "submit_review_lane_draft",
      [
        tool_call(Some("{not json"), Some("failed")),
        tool_call(
          Some(
            "{\"draft_findings\":[{\"summary\":\"corrected\"}],\"review_notes\":[],\"evidence_requests\":[],\"self_check\":{\"summary\":\"ok\"}}",
          ),
          Some("success"),
        ),
      ],
    )

  let assert review_lane_live_probe.PhaseResult(
    step_id: "lane_correctness",
    phase: "registration",
    status: "passed",
    code: None,
    message: None,
  ) = phase
}

fn tool_call(
  arguments_json: Option(String),
  status: Option(String),
) -> result_artifact.ToolCallSubmission {
  result_artifact.ToolCallSubmission(
    name: "submit_review_lane_draft",
    arguments_json: arguments_json,
    status: status,
    sibling_count: 1,
    receipt_json: None,
  )
}
