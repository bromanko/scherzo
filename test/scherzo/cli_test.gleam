import gleeunit/should
import scherzo
import scherzo/agent/checkpoint

// ---------------------------------------------------------------------------
// parse_checkpoint_type tests
// ---------------------------------------------------------------------------

pub fn parse_checkpoint_type_incremental_test() {
  scherzo.parse_checkpoint_type("incremental")
  |> should.equal(checkpoint.Incremental)
}

pub fn parse_checkpoint_type_pre_compact_test() {
  scherzo.parse_checkpoint_type("pre_compact")
  |> should.equal(checkpoint.PreCompact)
}

pub fn parse_checkpoint_type_final_test() {
  scherzo.parse_checkpoint_type("final")
  |> should.equal(checkpoint.Final)
}

pub fn parse_checkpoint_type_unknown_defaults_to_final_test() {
  // Unknown values should default to Final (and print a warning)
  scherzo.parse_checkpoint_type("unknown")
  |> should.equal(checkpoint.Final)

  scherzo.parse_checkpoint_type("finall")
  |> should.equal(checkpoint.Final)

  scherzo.parse_checkpoint_type("")
  |> should.equal(checkpoint.Final)
}
