/// Human-readable name generation for agents
/// Generates names like "swift-falcon", "calm-otter", etc.
import gleam/int
import gleam/list
import gleam/string

/// Adjectives for name generation (positive, short)
const adjectives = [
  "swift", "calm", "bold", "keen", "warm", "cool", "fair", "wise", "kind",
  "glad", "pure", "true", "free", "safe", "firm", "rare", "fine", "neat", "fast",
  "slow", "soft", "hard", "wild", "tame", "dark", "pale", "deep", "tall", "lean",
  "sure", "mild", "ripe", "keen", "slim", "rich", "poor",
]

/// Nouns for name generation (animals, nature)
const nouns = [
  "fox", "owl", "bee", "ant", "elk", "eel", "jay", "cod", "ray", "emu", "bat",
  "cat", "dog", "rat", "pig", "cow", "hen", "ram", "yak", "ape", "oak", "elm",
  "ash", "fir", "bay", "cay", "tor", "fen", "lea", "vale", "hawk", "lynx",
  "bear", "wolf", "deer", "hare", "seal", "crab", "frog",
]

/// Generate a human-readable name from a numeric seed
/// Returns names like "swift-fox", "calm-owl", etc.
pub fn generate(seed: Int) -> String {
  let adj_count = list.length(adjectives)
  let noun_count = list.length(nouns)

  // Use different parts of the seed for adjective and noun
  let adj_index = int.absolute_value(seed) % adj_count
  let noun_index = int.absolute_value(seed / adj_count) % noun_count

  let adj = get_at(adjectives, adj_index)
  let noun = get_at(nouns, noun_index)

  adj <> "-" <> noun
}

/// Generate a name with a numeric suffix for additional uniqueness
/// Returns names like "swift-fox-42"
pub fn generate_with_suffix(seed: Int) -> String {
  let base = generate(seed)
  let suffix = int.absolute_value(seed) % 100
  base <> "-" <> int.to_string(suffix)
}

/// Get element at index, with wraparound
fn get_at(items: List(String), index: Int) -> String {
  case list.drop(items, index) {
    [first, ..] -> first
    [] -> "unknown"
  }
}

/// Extract a short unique identifier from a full agent ID
/// e.g., "agent-task-1769209431148099186-1769209431148" -> "swift-fox-48"
pub fn from_agent_id(agent_id: String) -> String {
  // Extract the timestamp part (last segment after the last dash)
  let parts = string.split(agent_id, "-")
  let seed = case list.last(parts) {
    Ok(last) ->
      case int.parse(last) {
        Ok(n) -> n
        Error(_) -> string.length(agent_id)
      }
    Error(_) -> string.length(agent_id)
  }
  generate_with_suffix(seed)
}
