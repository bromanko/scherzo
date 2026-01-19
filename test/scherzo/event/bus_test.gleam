import gleam/erlang/process
import gleeunit/should
import scherzo/core/event.{EventEnvelope, TaskCreated}
import scherzo/event/bus

pub fn start_creates_event_bus_test() {
  let result = bus.start()

  result |> should.be_ok
}

pub fn subscribe_and_publish_delivers_event_test() {
  let assert Ok(event_bus) = bus.start()

  // Create a subject to receive events
  let subscriber = process.new_subject()

  // Subscribe to the bus
  bus.subscribe(event_bus, subscriber)

  // Create and publish an event
  let event = TaskCreated("task-1", "Test Task", 1000)
  let envelope = EventEnvelope(id: "evt-1", event: event, timestamp: 1000)
  bus.publish(event_bus, envelope)

  // Receive the event with a timeout
  let received =
    process.receive(subscriber, 1000)
    |> should.be_ok

  received.id |> should.equal("evt-1")

  // Cleanup
  bus.stop(event_bus)
}

pub fn unsubscribe_stops_receiving_events_test() {
  let assert Ok(event_bus) = bus.start()

  // Create a subject to receive events
  let subscriber = process.new_subject()

  // Subscribe then unsubscribe
  bus.subscribe(event_bus, subscriber)
  bus.unsubscribe(event_bus, subscriber)

  // Give unsubscribe time to process
  process.sleep(50)

  // Publish an event
  let event = TaskCreated("task-1", "Test Task", 1000)
  let envelope = EventEnvelope(id: "evt-1", event: event, timestamp: 1000)
  bus.publish(event_bus, envelope)

  // Should timeout since we unsubscribed
  let received = process.receive(subscriber, 100)
  received |> should.be_error

  // Cleanup
  bus.stop(event_bus)
}

pub fn multiple_subscribers_receive_events_test() {
  let assert Ok(event_bus) = bus.start()

  // Create two subscribers
  let subscriber1 = process.new_subject()
  let subscriber2 = process.new_subject()

  bus.subscribe(event_bus, subscriber1)
  bus.subscribe(event_bus, subscriber2)

  // Publish an event
  let event = TaskCreated("task-1", "Test Task", 1000)
  let envelope = EventEnvelope(id: "evt-1", event: event, timestamp: 1000)
  bus.publish(event_bus, envelope)

  // Both subscribers should receive the event
  process.receive(subscriber1, 1000) |> should.be_ok
  process.receive(subscriber2, 1000) |> should.be_ok

  // Cleanup
  bus.stop(event_bus)
}
