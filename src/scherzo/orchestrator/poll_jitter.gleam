import gleam/int
import gleam/list
import gleam/string
import scherzo/hash

const jitter_divisor = 10

pub fn jitter_bound_ms(interval_ms: Int) -> Int {
  let bound = interval_ms / jitter_divisor
  case bound <= 0 {
    True -> 1
    False -> bound
  }
}

pub fn delay_ms(interval_ms: Int, seed: String, generation: Int) -> Int {
  let bound = jitter_bound_ms(interval_ms)
  let offset = jitter_offset_ms(seed, generation, bound)
  positive_ms(interval_ms + offset)
}

fn jitter_offset_ms(seed: String, generation: Int, bound_ms: Int) -> Int {
  let bound = case bound_ms < 0 {
    True -> 0
    False -> bound_ms
  }
  let span = bound * 2 + 1
  hash_bucket(seed, generation, span) - bound
}

fn hash_bucket(seed: String, generation: Int, span: Int) -> Int {
  case span <= 0 {
    True -> 0
    False -> {
      let input = seed <> ":" <> int.to_string(generation)
      let value = hash.short_sha256_hex(input, 8) |> hex_to_int
      value % span
    }
  }
}

fn hex_to_int(hex: String) -> Int {
  hex
  |> string.to_graphemes
  |> list.fold(0, fn(total, grapheme) {
    total * 16 + hex_grapheme_value(grapheme)
  })
}

fn hex_grapheme_value(grapheme: String) -> Int {
  case grapheme {
    "0" -> 0
    "1" -> 1
    "2" -> 2
    "3" -> 3
    "4" -> 4
    "5" -> 5
    "6" -> 6
    "7" -> 7
    "8" -> 8
    "9" -> 9
    "a" | "A" -> 10
    "b" | "B" -> 11
    "c" | "C" -> 12
    "d" | "D" -> 13
    "e" | "E" -> 14
    "f" | "F" -> 15
    _ -> 0
  }
}

fn positive_ms(delay_ms: Int) -> Int {
  case delay_ms <= 0 {
    True -> 1
    False -> delay_ms
  }
}
