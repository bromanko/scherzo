import gleam/dict
import gleam/erlang/process
import gleam/list
import gleam/option.{None, Some}
import scherzo/control/query/types as query_types
import scherzo/orchestrator/read_model
import scherzo/orchestrator/transition_types
import scherzo/orchestrator/workflow_reloader
import scherzo/runtime/identity
import scherzo/runtime/state as orchestrator_state
import scherzo/state/projection

pub type Snapshot {
  Snapshot(
    read_model: read_model.Snapshot,
    projection: projection.Projection,
    outbox: List(#(String, projection.OutboxStatus)),
    workflow: workflow_reloader.State,
    dispatch_paused: Bool,
    claims: query_types.ClaimListDto,
  )
}

pub opaque type Handle {
  Handle(subject: process.Subject(Message), pid: process.Pid)
}

type Message {
  Update(Snapshot)
  GetReadModel(process.Subject(read_model.Snapshot))
  GetProjection(process.Subject(projection.Projection))
  GetOutbox(process.Subject(List(#(String, projection.OutboxStatus))))
  GetWorkflow(process.Subject(workflow_reloader.State))
  GetDispatchPaused(process.Subject(Bool))
  GetClaims(process.Subject(query_types.ClaimListDto))
  Stop(process.Subject(Nil))
}

pub fn start(snapshot: Snapshot) -> Result(Handle, Nil) {
  let ready = process.new_subject()
  let pid =
    process.spawn_unlinked(fn() {
      let subject = process.new_subject()
      process.send(ready, subject)
      loop(snapshot, subject)
    })
  case process.receive(ready, within: 1000) {
    Ok(subject) -> Ok(Handle(subject, pid))
    Error(Nil) -> {
      process.kill(pid)
      Error(Nil)
    }
  }
}

pub fn update(handle: Handle, snapshot: Snapshot) -> Nil {
  let Handle(subject, _) = handle
  process.send(subject, Update(snapshot))
}

pub fn claims_snapshot(
  pending_claims pending_claims: dict.Dict(
    identity.TaskIdentity,
    transition_types.PendingClaim,
  ),
  worker_claims worker_claims: dict.Dict(
    identity.TaskIdentity,
    transition_types.WorkerEntry,
  ),
  runtime_claims runtime_claims: dict.Dict(identity.TaskIdentity, String),
  sampled_at_ms sampled_at_ms: Int,
) -> query_types.ClaimListDto {
  let pending_items =
    pending_claims
    |> dict.to_list
    |> list.map(fn(entry) { pending_claim_to_dto(entry, sampled_at_ms) })
  let worker_items =
    worker_claims
    |> dict.to_list
    |> list.map(active_claim_to_dto)
  let covered_claims =
    list.append(dict.keys(pending_claims), dict.keys(worker_claims))
  let runtime_items =
    runtime_claims
    |> dict.to_list
    |> list.filter(fn(entry) {
      let #(task_identity, _) = entry
      !list.contains(covered_claims, task_identity)
    })
    |> list.map(runtime_claim_to_dto)

  query_types.ClaimListDto(
    sampled_at_ms: sampled_at_ms,
    items: list.append(pending_items, list.append(worker_items, runtime_items)),
  )
}

fn pending_claim_to_dto(
  entry: #(identity.TaskIdentity, transition_types.PendingClaim),
  sampled_at_ms: Int,
) -> query_types.ClaimDto {
  let #(task_identity, pending) = entry
  query_types.ClaimDto(
    task_identity: orchestrator_state.task_identity_to_string(task_identity),
    issue_id: Some(pending.issue_id),
    issue_identifier: Some(pending.issue.identifier),
    run_id: Some(pending.run_id),
    session_id: Some(pending.session_id),
    age_ms: Some(non_negative(sampled_at_ms - pending.claimed_at_ms)),
    holder: "pending_claim",
  )
}

fn active_claim_to_dto(
  entry: #(identity.TaskIdentity, transition_types.WorkerEntry),
) -> query_types.ClaimDto {
  let #(task_identity, worker) = entry
  query_types.ClaimDto(
    task_identity: orchestrator_state.task_identity_to_string(task_identity),
    issue_id: Some(worker.issue_id),
    issue_identifier: Some(worker.issue.identifier),
    run_id: Some(worker.run_id),
    session_id: Some(worker.session_id),
    age_ms: None,
    holder: active_claim_holder(worker.status),
  )
}

fn runtime_claim_to_dto(
  entry: #(identity.TaskIdentity, String),
) -> query_types.ClaimDto {
  let #(task_identity, identifier) = entry
  query_types.ClaimDto(
    task_identity: orchestrator_state.task_identity_to_string(task_identity),
    issue_id: None,
    issue_identifier: Some(identifier),
    run_id: None,
    session_id: None,
    age_ms: None,
    holder: "runtime_claimed",
  )
}

fn active_claim_holder(status: transition_types.WorkerStatus) -> String {
  case status {
    transition_types.WorkerStarting -> "worker_starting"
    transition_types.WorkerRunning -> "worker_running"
    transition_types.WorkerStopping(_) -> "worker_stopping"
    transition_types.WorkerFinishedStatus -> "worker_finished"
  }
}

fn non_negative(value: Int) -> Int {
  case value < 0 {
    True -> 0
    False -> value
  }
}

pub fn get_read_model_snapshot(
  handle: Handle,
  timeout_ms: Int,
) -> Result(read_model.Snapshot, Nil) {
  let Handle(subject, _) = handle
  let reply = process.new_subject()
  process.send(subject, GetReadModel(reply))
  process.receive(reply, within: timeout_ms)
}

pub fn get_projection_snapshot(
  handle: Handle,
  timeout_ms: Int,
) -> Result(projection.Projection, Nil) {
  let Handle(subject, _) = handle
  let reply = process.new_subject()
  process.send(subject, GetProjection(reply))
  process.receive(reply, within: timeout_ms)
}

pub fn get_outbox_snapshot(
  handle: Handle,
  timeout_ms: Int,
) -> Result(List(#(String, projection.OutboxStatus)), Nil) {
  let Handle(subject, _) = handle
  let reply = process.new_subject()
  process.send(subject, GetOutbox(reply))
  process.receive(reply, within: timeout_ms)
}

pub fn get_workflow_snapshot(
  handle: Handle,
  timeout_ms: Int,
) -> Result(workflow_reloader.State, Nil) {
  let Handle(subject, _) = handle
  let reply = process.new_subject()
  process.send(subject, GetWorkflow(reply))
  process.receive(reply, within: timeout_ms)
}

pub fn get_dispatch_paused(
  handle: Handle,
  timeout_ms: Int,
) -> Result(Bool, Nil) {
  let Handle(subject, _) = handle
  let reply = process.new_subject()
  process.send(subject, GetDispatchPaused(reply))
  process.receive(reply, within: timeout_ms)
}

pub fn get_claims(
  handle: Handle,
  timeout_ms: Int,
) -> Result(query_types.ClaimListDto, Nil) {
  let Handle(subject, _) = handle
  let reply = process.new_subject()
  process.send(subject, GetClaims(reply))
  process.receive(reply, within: timeout_ms)
}

pub fn stop(handle: Handle, timeout_ms: Int) -> Result(Nil, Nil) {
  let Handle(subject, _) = handle
  let reply = process.new_subject()
  process.send(subject, Stop(reply))
  process.receive(reply, within: timeout_ms)
}

pub fn kill(handle: Handle) -> Nil {
  let Handle(_, pid) = handle
  process.kill(pid)
}

type LoopAction {
  Continue(Snapshot)
  StopLoop
}

fn loop(snapshot: Snapshot, subject: process.Subject(Message)) -> Nil {
  case handle_message(process.receive_forever(subject), snapshot, subject) {
    Continue(next) -> loop(next, subject)
    StopLoop -> Nil
  }
}

fn handle_message(
  message: Message,
  snapshot: Snapshot,
  subject: process.Subject(Message),
) -> LoopAction {
  case message {
    Update(next) -> drain_ready_messages(next, subject)
    GetReadModel(reply) -> {
      process.send(reply, snapshot.read_model)
      Continue(snapshot)
    }
    GetProjection(reply) -> {
      process.send(reply, snapshot.projection)
      Continue(snapshot)
    }
    GetOutbox(reply) -> {
      process.send(reply, snapshot.outbox)
      Continue(snapshot)
    }
    GetWorkflow(reply) -> {
      process.send(reply, snapshot.workflow)
      Continue(snapshot)
    }
    GetDispatchPaused(reply) -> {
      process.send(reply, snapshot.dispatch_paused)
      Continue(snapshot)
    }
    GetClaims(reply) -> {
      process.send(reply, snapshot.claims)
      Continue(snapshot)
    }
    Stop(reply) -> {
      process.send(reply, Nil)
      StopLoop
    }
  }
}

fn drain_ready_messages(
  snapshot: Snapshot,
  subject: process.Subject(Message),
) -> LoopAction {
  case process.receive(subject, within: 0) {
    Ok(message) -> handle_message(message, snapshot, subject)
    Error(Nil) -> Continue(snapshot)
  }
}
