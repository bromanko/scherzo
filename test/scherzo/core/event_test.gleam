import gleeunit/should
import scherzo/core/event.{
  AgentSpawned, CheckpointCreated, EventEnvelope, TaskCompleted, TaskCreated,
}

pub fn wrap_creates_envelope_with_correct_fields_test() {
  let evt = TaskCreated("task-1", "Test Task", 1000)
  let envelope = event.wrap("evt-1", evt, 2000)

  envelope.id |> should.equal("evt-1")
  envelope.timestamp |> should.equal(2000)
}

pub fn wrap_preserves_task_created_event_test() {
  let evt = TaskCreated("task-1", "Test Task", 1000)
  let envelope = event.wrap("evt-1", evt, 2000)

  case envelope.event {
    TaskCreated(task_id, title, timestamp) -> {
      task_id |> should.equal("task-1")
      title |> should.equal("Test Task")
      timestamp |> should.equal(1000)
    }
    _ -> should.fail()
  }
}

pub fn wrap_preserves_agent_spawned_event_test() {
  let evt = AgentSpawned("agent-1", 1000)
  let envelope = event.wrap("evt-2", evt, 2000)

  case envelope.event {
    AgentSpawned(agent_id, timestamp) -> {
      agent_id |> should.equal("agent-1")
      timestamp |> should.equal(1000)
    }
    _ -> should.fail()
  }
}

pub fn wrap_preserves_task_completed_event_test() {
  let evt = TaskCompleted("task-1", "agent-1", 1000)
  let envelope = event.wrap("evt-3", evt, 2000)

  case envelope.event {
    TaskCompleted(task_id, agent_id, timestamp) -> {
      task_id |> should.equal("task-1")
      agent_id |> should.equal("agent-1")
      timestamp |> should.equal(1000)
    }
    _ -> should.fail()
  }
}

pub fn wrap_preserves_checkpoint_created_event_test() {
  let evt = CheckpointCreated("task-1", "agent-1", "cp-1", 1000)
  let envelope = event.wrap("evt-4", evt, 2000)

  case envelope.event {
    CheckpointCreated(task_id, agent_id, checkpoint_id, timestamp) -> {
      task_id |> should.equal("task-1")
      agent_id |> should.equal("agent-1")
      checkpoint_id |> should.equal("cp-1")
      timestamp |> should.equal(1000)
    }
    _ -> should.fail()
  }
}

pub fn event_envelope_can_be_constructed_directly_test() {
  let evt = TaskCreated("task-1", "Direct", 1000)
  let envelope = EventEnvelope(id: "direct-1", event: evt, timestamp: 3000)

  envelope.id |> should.equal("direct-1")
  envelope.timestamp |> should.equal(3000)
}
