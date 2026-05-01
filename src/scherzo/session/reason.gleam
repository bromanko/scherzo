pub type WorkerExitReason {
  Normal
  Failed
  OperatorAbort
  OperatorStopAfterCurrentTurn
  WorkerDown
  Stopped
}

pub fn to_string(reason: WorkerExitReason) -> String {
  case reason {
    Normal -> "normal"
    Failed -> "failed"
    OperatorAbort -> "operator_abort"
    OperatorStopAfterCurrentTurn -> "operator_stop_after_current_turn"
    WorkerDown -> "worker_down"
    Stopped -> "stopped"
  }
}

pub fn from_string(reason: String) -> Result(WorkerExitReason, Nil) {
  case reason {
    "normal" -> Ok(Normal)
    "failed" -> Ok(Failed)
    "operator_abort" -> Ok(OperatorAbort)
    "operator_stop_after_current_turn" -> Ok(OperatorStopAfterCurrentTurn)
    "worker_down" -> Ok(WorkerDown)
    "stopped" -> Ok(Stopped)
    _ -> Error(Nil)
  }
}
