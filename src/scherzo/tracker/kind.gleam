import gleam/string

pub type TrackerKind {
  LinearTracker
}

pub fn to_string(kind: TrackerKind) -> String {
  case kind {
    LinearTracker -> "linear"
  }
}

pub fn from_string(kind: String) -> Result(TrackerKind, Nil) {
  case kind |> string.trim |> string.lowercase {
    "linear" -> Ok(LinearTracker)
    _ -> Error(Nil)
  }
}
