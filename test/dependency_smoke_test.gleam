import birl
import gleam/dynamic/decode
import gleam/erlang/process
import gleam/json
import gleam/otp/actor
import simplifile
import yay

// This module intentionally acts as a compile/dependency compatibility guard.
// These tests exercise external package APIs so broad dependency bounds fail
// loudly when an upgrade breaks Scherzo's expected compile/runtime surface.

type ActorCompatibilityMessage {
  StopActor
}

fn stop_actor_on_message(
  _state: Int,
  _message: ActorCompatibilityMessage,
) -> actor.Next(Int, ActorCompatibilityMessage) {
  actor.stop()
}

fn monitor_down(monitor: process.Monitor, timeout_ms: Int) -> Bool {
  let selector =
    process.new_selector()
    |> process.select_specific_monitor(monitor, fn(_) { True })

  case process.selector_receive(selector, within: timeout_ms) {
    Ok(True) -> True
    Ok(False) -> False
    Error(_) -> False
  }
}

pub fn gleam_json_dependency_compatibility_guard_test() {
  let assert Ok(1) = json.parse("1", decode.int)
}

pub fn yay_yaml_dependency_compatibility_guard_test() {
  let assert Ok([document]) = yay.parse_string("a: 1\n")
  let assert yay.NodeMap(_) = yay.document_root(document)
}

pub fn simplifile_dependency_compatibility_guard_test() {
  let assert Ok(True) = simplifile.is_file("gleam.toml")
}

pub fn birl_dependency_compatibility_guard_test() {
  let time = birl.from_unix(0)
  assert time == birl.unix_epoch()
}

pub fn gleam_otp_actor_dependency_compatibility_guard_test() {
  let assert Ok(started) =
    actor.new(0)
    |> actor.on_message(stop_actor_on_message)
    |> actor.start
  let monitor = process.monitor(started.pid)
  actor.send(started.data, StopActor)
  assert monitor_down(monitor, 1000)
  process.demonitor_process(monitor)
}
