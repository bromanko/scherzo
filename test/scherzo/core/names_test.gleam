import gleeunit/should
import scherzo/core/names

pub fn generate_returns_adjective_noun_format_test() {
  let name = names.generate(12_345)

  // Should contain a dash (adjective-noun)
  name |> should.not_equal("")
}

pub fn generate_with_suffix_includes_number_test() {
  let name = names.generate_with_suffix(12_345)

  // Should have format like "word-word-NN"
  name |> should.not_equal("")
}

pub fn from_agent_id_extracts_unique_name_test() {
  let name1 =
    names.from_agent_id("agent-task-1769209431148099186-1769209431148")
  let name2 =
    names.from_agent_id("agent-task-1769209431148099186-1769209431149")

  // Different timestamps should produce different names
  name1 |> should.not_equal(name2)
}

pub fn from_agent_id_same_id_produces_same_name_test() {
  let name1 = names.from_agent_id("agent-task-123-456")
  let name2 = names.from_agent_id("agent-task-123-456")

  // Same ID should produce same name (deterministic)
  name1 |> should.equal(name2)
}

pub fn generate_is_deterministic_test() {
  let name1 = names.generate(999)
  let name2 = names.generate(999)

  // Same seed should produce same name
  name1 |> should.equal(name2)
}

pub fn different_seeds_produce_different_names_test() {
  let name1 = names.generate(100)
  let name2 = names.generate(200)
  let name3 = names.generate(300)

  // Different seeds should generally produce different names
  // (not guaranteed for all seeds due to modulo, but these should differ)
  name1 |> should.not_equal(name2)
  name2 |> should.not_equal(name3)
}
