import gleam/option.{Some}
import gleam/string
import scherzo/agent/context_exhaustion
import scherzo/agent/context_recovery_artifact
import scherzo/agent/context_recovery_prompt
import scherzo/state/artifact_store
import simplifile

fn reset_dir(dir: String) -> Nil {
  let _ = simplifile.delete(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir)
  Nil
}

pub fn writes_redacted_bounded_context_recovery_artifacts_test() {
  let root = "test/tmp/context-recovery-artifacts"
  reset_dir(root)
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
