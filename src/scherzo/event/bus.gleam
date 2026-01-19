/// Event bus actor for pub/sub messaging
import gleam/dict.{type Dict}
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/list
import gleam/otp/actor
import gleam/result
import scherzo/core/event.{type EventEnvelope}
import scherzo/core/types.{type Id}

/// Messages the event bus can receive
pub type Message {
  /// Subscribe to all events
  Subscribe(subscriber: Subject(EventEnvelope))
  /// Unsubscribe from events
  Unsubscribe(subscriber: Subject(EventEnvelope))
  /// Publish an event to all subscribers
  Publish(event: EventEnvelope)
  /// Shutdown the event bus
  Shutdown
}

/// Internal state of the event bus
pub type State {
  State(subscribers: Dict(Id, Subject(EventEnvelope)), next_id: Int)
}

/// Start a new event bus actor
pub fn start() -> Result(Subject(Message), actor.StartError) {
  actor.new(State(subscribers: dict.new(), next_id: 0))
  |> actor.on_message(handle_message)
  |> actor.start
  |> result.map(fn(started) { started.data })
}

/// Subscribe to events from the bus
pub fn subscribe(
  bus: Subject(Message),
  subscriber: Subject(EventEnvelope),
) -> Nil {
  actor.send(bus, Subscribe(subscriber))
}

/// Unsubscribe from events
pub fn unsubscribe(
  bus: Subject(Message),
  subscriber: Subject(EventEnvelope),
) -> Nil {
  actor.send(bus, Unsubscribe(subscriber))
}

/// Publish an event to all subscribers
pub fn publish(bus: Subject(Message), event: EventEnvelope) -> Nil {
  actor.send(bus, Publish(event))
}

/// Stop the event bus
pub fn stop(bus: Subject(Message)) -> Nil {
  actor.send(bus, Shutdown)
}

/// Handle incoming messages
fn handle_message(state: State, message: Message) -> actor.Next(State, Message) {
  case message {
    Subscribe(subscriber) -> {
      let id = "sub_" <> int.to_string(state.next_id)
      let new_subscribers = dict.insert(state.subscribers, id, subscriber)
      actor.continue(State(
        subscribers: new_subscribers,
        next_id: state.next_id + 1,
      ))
    }

    Unsubscribe(subscriber) -> {
      // Find and remove the subscriber
      let new_subscribers =
        dict.filter(state.subscribers, fn(_id, sub) { sub != subscriber })
      actor.continue(State(..state, subscribers: new_subscribers))
    }

    Publish(event) -> {
      // Send event to all subscribers
      dict.values(state.subscribers)
      |> list.each(fn(subscriber) { process.send(subscriber, event) })
      actor.continue(state)
    }

    Shutdown -> {
      actor.stop()
    }
  }
}
