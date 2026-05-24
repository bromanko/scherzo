import support/docs_assert.{assert_contains_all, read_file}

pub fn workflow_step_recovery_runbook_documents_protected_checkpoints_test() {
  let path = "docs/runbooks/workflow-step-recovery.md"
  let runbook = read_file(path)

  assert_contains_all(path, runbook, [
    "Protected retry checkpoints",
    ".scherzo-state/artifacts/runs/<run>/<step>/attempt-<n>.json",
    ".scherzo-state/artifacts/runs/<run>/inputs.v1.json",
    ".scherzo-state/artifacts/runs/<run>/outputs.v1.json",
    "StepContext.workspace_path",
    "protected_checkpoint_restored",
    "recovery_artifact_restore_failed",
  ])
}
