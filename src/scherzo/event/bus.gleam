/// Event bus actor for pub/sub messaging

import gleam/dict.{type Dict}
import gleam/erlang/process.{type Subject}
import gleam/list
import gleam/otp/actor
import scherzo/core/event.{type Event, type EventEnvelope}
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
  actor.start(State(subscribers: dict.new(), next_id: 0), handle_message)
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
fn handle_message(message: Message, state: State) -> actor.Next(Message, State) {
  case message {
    Subscribe(subscriber) -> {
      let id = "sub_" <> int_to_string(state.next_id)
      let new_subscribers = dict.insert(state.subscribers, id, subscriber)
      actor.continue(State(..state, subscribers: new_subscribers, next_id: state.next_id + 1))
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
      actor.Stop(process.Normal)
    }
  }
}

/// Convert int to string (helper)
fn int_to_string(n: Int) -> String {
  case n {
    0 -> "0"
    _ -> do_int_to_string(n, "")
  }
}

fn do_int_to_string(n: Int, acc: String) -> String {
  case n {
    0 -> acc
    _ -> {
      let digit = n % 10
      let char = case digit {
        0 -> "0"
        1 -> "1"
        2 -> "2"
        3 -> "3"
        4 -> "4"
        5 -> "5"
        6 -> "6"
        7 -> "7"
        8 -> "8"
        _ -> "9"
      }
      do_int_to_string(n / 10, char <> acc)
    }
  }
}
