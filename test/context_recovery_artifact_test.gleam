import gleam/option.{None, Some}
import gleam/string
import scherzo/agent/context_exhaustion
import scherzo/agent/context_recovery_artifact
import scherzo/agent/context_recovery_prompt
import scherzo/error
import scherzo/pi/protocol
import scherzo/state/artifact_store
import support/artifact_store_fixtures
import support/test_helpers

pub fn writes_redacted_bounded_context_recovery_artifacts_test() {
  let root = "test/tmp/context-recovery-artifacts"
  test_helpers.reset_dir(root)
  let store = artifact_store.new(root)
  let prompt =
    "api_key=SECRET_VALUE\n<absolute-local-path>/workspace/file.gleam\n"
    <> string.repeat("x", times: 210_000)
  let input =
    context_recovery_artifact.RecoveryArtifactInput(
      store: store,
      run_id: "run 1",
      workflow_id: "execplan-implementation",
      step_id: "implement plan!",
      step_attempt_index: 1,
      pi_attempt: 1,
      context: context_exhaustion.ContextExhaustion(
        provider: Some("openai"),
        provider_code: Some("context_length_exceeded"),
        message: "Your input exceeds the context window.",
      ),
      original_prompt: prompt,
      secrets: ["SECRET_VALUE"],
      workspace_path: "<absolute-local-path>/workspace",
      recovery_attempted: True,
      recovery_exhausted: False,
      recovery_method: context_recovery_prompt.PiRpcCompact,
    )
  let assert Ok(artifacts) = context_recovery_artifact.write_initial(input)

  assert string.starts_with(artifacts.prompt_excerpt_ref, "runs/")
  assert string.contains(artifacts.prompt_excerpt_ref, "/context-recovery/")
  let assert Ok(prompt_contents) =
    artifact_store.read_artifact_unverified(store, artifacts.prompt_excerpt_ref)
  assert !string.contains(prompt_contents, "SECRET_VALUE")
  assert !string.contains(prompt_contents, "<absolute-local-path>")
  assert string.contains(prompt_contents, "[REDACTED]")
  assert artifacts.prompt_truncated == True

  let assert Ok(error_contents) =
    artifact_store.read_artifact_unverified(store, artifacts.error_ref)
  assert string.contains(error_contents, "pi_context_window_exhausted")
  assert !string.contains(error_contents, "SECRET_VALUE")
}

pub fn context_recovery_artifacts_use_store_display_paths_test() {
  let root = "test/tmp/context-recovery-artifacts-display-path"
  test_helpers.reset_dir(root)
  let store = artifact_store_fixtures.hidden_local_path_store(root)
  let input =
    context_recovery_artifact.RecoveryArtifactInput(
      store: store,
      run_id: "run 1",
      workflow_id: "execplan-implementation",
      step_id: "implement plan!",
      step_attempt_index: 1,
      pi_attempt: 1,
      context: context_exhaustion.ContextExhaustion(
        provider: Some("openai"),
        provider_code: Some("context_length_exceeded"),
        message: "Your input exceeds the context window.",
      ),
      original_prompt: "prompt",
      secrets: [],
      workspace_path: "/workspace",
      recovery_attempted: True,
      recovery_exhausted: False,
      recovery_method: context_recovery_prompt.PiRpcCompact,
    )
  let assert Ok(artifacts) = context_recovery_artifact.write_initial(input)

  assert artifacts.prompt_excerpt_display_path
    == "artifacts://" <> artifacts.prompt_excerpt_ref
  assert artifacts.error_display_path == "artifacts://" <> artifacts.error_ref

  let assert Ok(result_ref) =
    context_recovery_artifact.write_result(
      store,
      "run 1",
      "workflow",
      "step!",
      2,
      "failed",
      context_recovery_prompt.FreshSession,
      False,
      ["manual"],
      context_recovery_artifact.compaction_succeeded(["manual"], 1),
      None,
    )
  assert result_ref.local_path == None
  assert result_ref.display_path == "artifacts://" <> result_ref.ref
}

pub fn writes_compact_failure_fallback_diagnostics_test() {
  let root = "test/tmp/context-recovery-compact-diagnostics"
  test_helpers.reset_dir(root)
  let store = artifact_store.new(root)
  let assert Ok(response) =
    protocol.decode_record(
      "{\"id\":\"3\",\"type\":\"response\",\"command\":\"compact\",\"success\":false,\"error\":\"compact failed SECRET_VALUE /tmp/workspace\"}",
    )
  let diagnostic =
    context_recovery_artifact.compaction_failed(
      error.PiProtocolError(
        "compact failed SECRET_VALUE /tmp/workspace "
        <> string.repeat("x", times: 5000),
      ),
      ["manual"],
      1,
      Some(response),
      ["SECRET_VALUE"],
      "/tmp/workspace",
    )
  let assert Ok(result_ref) =
    context_recovery_artifact.write_result(
      store,
      "run 1",
      "workflow",
      "step!",
      2,
      "failed",
      context_recovery_prompt.FreshSession,
      False,
      ["manual"],
      diagnostic,
      None,
    )
  let assert Ok(contents) =
    artifact_store.read_artifact_unverified(store, result_ref.ref)

  assert string.contains(
    contents,
    "\"fallback_from_method\":\"pi_rpc_compact\"",
  )
  assert string.contains(contents, "\"fallback_reason\":\"compact_rpc_failed\"")
  assert string.contains(contents, "\"error_code\":\"pi_protocol_error\"")
  assert string.contains(contents, "\"error_detail_truncated\":true")
  assert string.contains(contents, "\"raw_json_truncated\":false")
  assert string.contains(contents, "[REDACTED]")
  assert string.contains(contents, "[REDACTED_WORKSPACE]")
  assert !string.contains(contents, "SECRET_VALUE")
  assert !string.contains(contents, "/tmp/workspace")
}
