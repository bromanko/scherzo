import gleam/list
import gleam/order.{Gt, Lt}
import gleam/string

pub fn generate(issue_identifier: String, seed: String) -> String {
  let prefix = issue_prefix(issue_identifier)
  prefix
  <> "-"
  <> pick(adjectives(), seed <> ":adjective")
  <> "-"
  <> pick(animals(), seed <> ":animal")
  <> "-"
  <> pick(objects(), seed <> ":object")
}

pub fn issue_prefix(issue_identifier: String) -> String {
  let sanitized =
    issue_identifier
    |> string.lowercase
    |> string.to_graphemes
    |> sanitize_graphemes(False, [])
    |> string.join(with: "")
    |> trim_hyphens

  case sanitized == "" {
    True -> "session"
    False -> sanitized
  }
}

fn sanitize_graphemes(
  graphemes: List(String),
  previous_hyphen: Bool,
  acc: List(String),
) -> List(String) {
  case graphemes {
    [] -> list.reverse(acc)
    [grapheme, ..rest] -> {
      let sanitized = case is_allowed(grapheme) {
        True -> grapheme
        False -> "-"
      }
      case sanitized == "-" && previous_hyphen {
        True -> sanitize_graphemes(rest, True, acc)
        False -> sanitize_graphemes(rest, sanitized == "-", [sanitized, ..acc])
      }
    }
  }
}

fn trim_hyphens(value: String) -> String {
  value |> trim_left_hyphens |> trim_right_hyphens
}

fn trim_left_hyphens(value: String) -> String {
  case string.starts_with(value, "-") {
    True -> string.drop_start(value, 1) |> trim_left_hyphens
    False -> value
  }
}

fn trim_right_hyphens(value: String) -> String {
  case string.ends_with(value, "-") {
    True -> string.drop_end(value, 1) |> trim_right_hyphens
    False -> value
  }
}

fn is_allowed(grapheme: String) -> Bool {
  is_between(grapheme, "a", "z")
  || is_between(grapheme, "0", "9")
  || grapheme == "-"
}

fn is_between(value: String, low: String, high: String) -> Bool {
  string.compare(value, low) != Lt && string.compare(value, high) != Gt
}

fn pick(words: List(String), seed: String) -> String {
  word_at(words, stable_index(seed, list.length(words)))
}

fn word_at(words: List(String), index: Int) -> String {
  case words {
    [] -> "steady"
    [word, ..rest] ->
      case index <= 0 {
        True -> word
        False -> word_at(rest, index - 1)
      }
  }
}

fn stable_index(seed: String, count: Int) -> Int {
  case count <= 0 {
    True -> 0
    False -> phash2(seed, count)
  }
}

fn adjectives() -> List(String) {
  [
    "amber",
    "brave",
    "bright",
    "calm",
    "clever",
    "cozy",
    "crisp",
    "eager",
    "fancy",
    "gentle",
    "golden",
    "happy",
    "honest",
    "kind",
    "lively",
    "lucky",
    "merry",
    "nimble",
    "noble",
    "playful",
    "proud",
    "quiet",
    "rapid",
    "rosy",
    "silver",
    "steady",
    "sunny",
    "tidy",
    "vivid",
    "warm",
    "wise",
    "zesty",
  ]
}

fn animals() -> List(String) {
  [
    "alpaca",
    "badger",
    "beaver",
    "cricket",
    "dolphin",
    "falcon",
    "fox",
    "gecko",
    "heron",
    "ibex",
    "kestrel",
    "llama",
    "lynx",
    "moose",
    "narwhal",
    "otter",
    "panda",
    "penguin",
    "quail",
    "raven",
    "robin",
    "seal",
    "sparrow",
    "stoat",
    "swan",
    "tiger",
    "turtle",
    "walrus",
    "whale",
    "wren",
    "yak",
    "zebra",
  ]
}

fn objects() -> List(String) {
  [
    "anchor",
    "banner",
    "beacon",
    "blanket",
    "bridge",
    "button",
    "candle",
    "compass",
    "feather",
    "fiddle",
    "finger",
    "garden",
    "harbor",
    "kettle",
    "lantern",
    "marble",
    "meadow",
    "notebook",
    "pebble",
    "pocket",
    "ribbon",
    "saddle",
    "shell",
    "spindle",
    "teacup",
    "thimble",
    "toolbox",
    "umbrella",
    "velvet",
    "whistle",
    "window",
    "yarn",
  ]
}

@external(erlang, "erlang", "phash2")
fn phash2(seed: String, range: Int) -> Int
