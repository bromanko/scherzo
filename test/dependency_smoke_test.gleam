import birl
import gleam/dynamic/decode
import gleam/json
import gleam/otp/actor
import simplifile
import yay

pub fn json_smoke_test() {
  let assert Ok(1) = json.parse("1", decode.int)
}

pub fn yaml_smoke_test() {
  let assert Ok([document]) = yay.parse_string("a: 1\n")
  let assert yay.NodeMap(_) = yay.document_root(document)
}

pub fn simplifile_smoke_test() {
  let assert Ok(True) = simplifile.is_file("gleam.toml")
}

pub fn birl_smoke_test() {
  let time = birl.from_unix(0)
  assert time == birl.unix_epoch()
}

pub fn otp_actor_smoke_test() {
  let _builder = actor.new(0)
}
