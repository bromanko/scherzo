import scherzo/workflow_identity

pub fn id(run_id: String, step_id: String, attempt: Int) -> String {
  workflow_identity.step_session_id(run_id, step_id, attempt)
}
