import gleam/option.{Some}
import gleam/string
import scherzo/agent/context_recovery_prompt

pub fn recovery_prompt_is_bounded_and_points_to_artifacts_test() {
  let large = string.repeat("x", times: 1_000_000)
  let prompt =
    context_recovery_prompt.build(context_recovery_prompt.RecoveryPromptInput(
      workflow_id: "execplan-implementation-v2",
      run_id: "run-1",
      step_id: "implement_plan",
      step_attempt_index: 1,
      issue_identifier: Some("LIV-214"),
      issue_title: Some("Handle context exhaustion"),
      recovery_attempt: 1,
      recovery_method: context_recovery_prompt.PiRpcCompact,
      compaction_event_reasons: ["manual"],
      prompt_excerpt_ref: "runs/run-1/implement_plan/attempt-1/context-recovery/attempt-1-prompt-excerpt.md",
      error_artifact_ref: "runs/run-1/implement_plan/attempt-1/context-recovery/context-window-exhausted.json",
      prompt_excerpt_display_path: ".scherzo-state/artifacts/runs/run-1/implement_plan/attempt-1/context-recovery/attempt-1-prompt-excerpt.md",
      error_artifact_display_path: ".scherzo-state/artifacts/runs/run-1/implement_plan/attempt-1/context-recovery/context-window-exhausted.json",
      current_status: "Working copy has changes.",
      original_prompt_excerpt: large,
      max_chars: 40_000,
    ))

  assert string.length(prompt) <= 40_000
  assert string.contains(prompt, "execplan-implementation-v2")
  assert string.contains(prompt, "implement_plan")
  assert string.contains(prompt, "LIV-214")
  assert string.contains(prompt, "jj status --color=never")
  assert string.contains(prompt, "attempt-1-prompt-excerpt.md")
  assert string.contains(prompt, "context-window-exhausted.json")
  assert !string.contains(prompt, "<absolute-local-path>")
}
