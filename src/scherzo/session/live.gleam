import gleam/option.{type Option}

pub type LiveSession {
  LiveSession(
    session_id: String,
    pi_rpc_pid: String,
    last_pi_event: Option(String),
    last_pi_timestamp: Option(Int),
    last_pi_message: Option(String),
    pi_input_tokens: Int,
    pi_output_tokens: Int,
    pi_total_tokens: Int,
    last_reported_input_tokens: Int,
    last_reported_output_tokens: Int,
    last_reported_total_tokens: Int,
    turn_count: Int,
  )
}
