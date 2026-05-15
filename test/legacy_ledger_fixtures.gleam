import gleam/int

pub fn workflow_run_started_v2(record_id: String, at_ms: Int) -> String {
  "{\"schema_version\":2,\"record_id\":\""
  <> record_id
  <> "\",\"at_ms\":"
  <> int.to_string(at_ms)
  <> ",\"kind\":\"workflow_run_started\",\"run_id\":\"run-1\",\"workflow_id\":\"execplan\",\"workflow_fingerprint\":\"wf-old\",\"issue_id\":\"issue-1\",\"issue_identifier\":\"LIV-266\",\"issue_fingerprint\":\"fp-old\",\"observed_updated_at_ms\":10,\"run_root\":\"test/tmp/run-root\"}"
}

pub fn workflow_run_finished_v2(record_id: String, at_ms: Int) -> String {
  "{\"schema_version\":2,\"record_id\":\""
  <> record_id
  <> "\",\"at_ms\":"
  <> int.to_string(at_ms)
  <> ",\"kind\":\"workflow_run_finished\",\"run_id\":\"run-1\",\"workflow_id\":\"execplan\",\"issue_id\":\"issue-1\",\"outcome\":\"success\",\"token_total\":10,\"turns\":2}"
}

pub fn workflow_run_started_with_task_v2(
  record_id: String,
  at_ms: Int,
) -> String {
  "{\"schema_version\":2,\"record_id\":\""
  <> record_id
  <> "\",\"at_ms\":"
  <> int.to_string(at_ms)
  <> ",\"kind\":\"workflow_run_started\",\"run_id\":\"run-1\",\"workflow_id\":\"execplan\",\"workflow_fingerprint\":\"wf-new\",\"issue_id\":\"issue-1\",\"issue_identifier\":\"LIV-266\",\"task_backend_kind\":\"linear\",\"task_remote_id\":\"issue-1\",\"task_key\":\"LIV-266\",\"task_url\":\"https://linear.app/living-systems/issue/LIV-266\",\"issue_fingerprint\":\"fp-new\",\"observed_updated_at_ms\":20,\"run_root\":\"test/tmp/run-root\"}"
}

pub fn workflow_run_finished_with_task_v2(
  record_id: String,
  at_ms: Int,
) -> String {
  "{\"schema_version\":2,\"record_id\":\""
  <> record_id
  <> "\",\"at_ms\":"
  <> int.to_string(at_ms)
  <> ",\"kind\":\"workflow_run_finished\",\"run_id\":\"run-1\",\"workflow_id\":\"execplan\",\"issue_id\":\"issue-1\",\"task_backend_kind\":\"linear\",\"task_remote_id\":\"issue-1\",\"task_key\":\"LIV-266\",\"task_url\":\"https://linear.app/living-systems/issue/LIV-266\",\"outcome\":\"success\",\"token_total\":10,\"turns\":2}"
}

pub fn step_attempt_pi_session_recorded_v2(
  record_id: String,
  at_ms: Int,
) -> String {
  "{\"schema_version\":2,\"record_id\":\""
  <> record_id
  <> "\",\"at_ms\":"
  <> int.to_string(at_ms)
  <> ",\"kind\":\"step_attempt_pi_session_recorded\",\"run_id\":\"run-1\",\"issue_id\":\"issue-1\",\"issue_identifier\":\"LIV-266\",\"workflow_id\":\"execplan\",\"workflow_fingerprint\":\"wf-old\",\"step_id\":\"step-1\",\"workspace_name\":\"main\",\"attempt_index\":1,\"workspace_path\":\"test/tmp/run-root/workspaces/main\",\"session_id\":\"pi-session-1\",\"session_file\":\"state/sessions/run-1/step-1.json\"}"
}

pub fn linear_command_seen_v2(record_id: String, at_ms: Int) -> String {
  "{\"schema_version\":2,\"record_id\":\""
  <> record_id
  <> "\",\"at_ms\":"
  <> int.to_string(at_ms)
  <> ",\"kind\":\"linear_command_seen\",\"comment_id\":\"comment-1\",\"issue_id\":\"issue-1\",\"author_id\":\"user-1\",\"command_name\":\"retry\",\"excerpt\":\"/scherzo retry\"}"
}

pub fn linear_command_started_v2(record_id: String, at_ms: Int) -> String {
  "{\"schema_version\":2,\"record_id\":\""
  <> record_id
  <> "\",\"at_ms\":"
  <> int.to_string(at_ms)
  <> ",\"kind\":\"linear_command_started\",\"comment_id\":\"comment-1\",\"issue_id\":\"issue-1\",\"command_name\":\"retry\"}"
}

pub fn linear_command_completed_v2(
  record_id: String,
  at_ms: Int,
  status: String,
  message_excerpt: String,
) -> String {
  "{\"schema_version\":2,\"record_id\":\""
  <> record_id
  <> "\",\"at_ms\":"
  <> int.to_string(at_ms)
  <> ",\"kind\":\"linear_command_completed\",\"comment_id\":\"comment-1\",\"issue_id\":\"issue-1\",\"status\":\""
  <> status
  <> "\",\"message_excerpt\":\""
  <> message_excerpt
  <> "\"}"
}

pub fn linear_command_acked_v2(record_id: String, at_ms: Int) -> String {
  "{\"schema_version\":2,\"record_id\":\""
  <> record_id
  <> "\",\"at_ms\":"
  <> int.to_string(at_ms)
  <> ",\"kind\":\"linear_command_acked\",\"comment_id\":\"comment-1\",\"issue_id\":\"issue-1\"}"
}

pub fn remote_command_seen_v2(record_id: String, at_ms: Int) -> String {
  "{\"schema_version\":2,\"record_id\":\""
  <> record_id
  <> "\",\"at_ms\":"
  <> int.to_string(at_ms)
  <> ",\"kind\":\"remote_command_seen\",\"backend_kind\":\"linear\",\"event_id\":\"comment-1\",\"task_remote_id\":\"issue-1\",\"task_key\":\"LIV-266\",\"author_id\":\"user-1\",\"command_name\":\"retry\",\"excerpt\":\"/scherzo retry\"}"
}

pub fn remote_command_started_v2(record_id: String, at_ms: Int) -> String {
  "{\"schema_version\":2,\"record_id\":\""
  <> record_id
  <> "\",\"at_ms\":"
  <> int.to_string(at_ms)
  <> ",\"kind\":\"remote_command_started\",\"backend_kind\":\"linear\",\"event_id\":\"comment-1\",\"task_remote_id\":\"issue-1\",\"command_name\":\"retry\"}"
}

pub fn remote_command_completed_v2(
  record_id: String,
  at_ms: Int,
  status: String,
  message_excerpt: String,
) -> String {
  "{\"schema_version\":2,\"record_id\":\""
  <> record_id
  <> "\",\"at_ms\":"
  <> int.to_string(at_ms)
  <> ",\"kind\":\"remote_command_completed\",\"backend_kind\":\"linear\",\"event_id\":\"comment-1\",\"task_remote_id\":\"issue-1\",\"status\":\""
  <> status
  <> "\",\"message_excerpt\":\""
  <> message_excerpt
  <> "\"}"
}

pub fn remote_command_acked_v2(record_id: String, at_ms: Int) -> String {
  "{\"schema_version\":2,\"record_id\":\""
  <> record_id
  <> "\",\"at_ms\":"
  <> int.to_string(at_ms)
  <> ",\"kind\":\"remote_command_acked\",\"backend_kind\":\"linear\",\"event_id\":\"comment-1\",\"task_remote_id\":\"issue-1\"}"
}
