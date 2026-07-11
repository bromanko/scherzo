import gleam/erlang/process
import gleam/otp/actor

pub opaque type Clock {
  Clock(subject: process.Subject(Message))
}

type Message {
  Read(reply: process.Subject(Int))
  Advance(by_ms: Int, reply: process.Subject(Nil))
}

pub fn new(initial_ms: Int) -> Clock {
  let assert Ok(started) =
    actor.new(initial_ms)
    |> actor.on_message(handle_message)
    |> actor.start
  Clock(started.data)
}

pub fn now_ms(clock: Clock) -> Int {
  let Clock(subject) = clock
  let reply = process.new_subject()
  actor.send(subject, Read(reply))
  let assert Ok(value) = process.receive(reply, within: 1000)
  value
}

pub fn advance(clock: Clock, by_ms: Int) -> Nil {
  let Clock(subject) = clock
  let reply = process.new_subject()
  actor.send(subject, Advance(by_ms, reply))
  let assert Ok(Nil) = process.receive(reply, within: 1000)
  Nil
}

fn handle_message(state: Int, message: Message) -> actor.Next(Int, Message) {
  case message {
    Read(reply) -> {
      process.send(reply, state)
      actor.continue(state)
    }
    Advance(by_ms, reply) -> {
      process.send(reply, Nil)
      actor.continue(state + by_ms)
    }
  }
}
