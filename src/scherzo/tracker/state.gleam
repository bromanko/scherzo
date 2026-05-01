import gleam/list
import gleam/string

pub type IssueState {
  IssueState(raw: String, key: IssueStateKey)
}

pub type IssueStateKey {
  IssueStateKey(normalized: String)
}

pub fn from_string(value: String) -> Result(IssueState, Nil) {
  Ok(from_string_unchecked(value))
}

pub fn from_string_unchecked(value: String) -> IssueState {
  let raw = string.trim(value)
  IssueState(raw: raw, key: key_from_string(value))
}

pub fn list_from_strings(values: List(String)) -> List(IssueState) {
  list.map(values, from_string_unchecked)
}

pub fn to_string(state: IssueState) -> String {
  let IssueState(raw: raw, ..) = state
  raw
}

pub fn to_strings(states: List(IssueState)) -> List(String) {
  list.map(states, to_string)
}

pub fn key(state: IssueState) -> IssueStateKey {
  let IssueState(key: key, ..) = state
  key
}

pub fn key_from_string(value: String) -> IssueStateKey {
  IssueStateKey(value |> string.trim |> string.lowercase)
}

pub fn key_to_string(key: IssueStateKey) -> String {
  let IssueStateKey(normalized) = key
  normalized
}

pub fn equals_key(state: IssueState, expected: IssueStateKey) -> Bool {
  key(state) == expected
}

pub fn equals_normalized(left: IssueState, right: IssueState) -> Bool {
  key(left) == key(right)
}

pub fn todo_state() -> IssueState {
  from_string_unchecked("Todo")
}

pub fn todo_key() -> IssueStateKey {
  key_from_string("Todo")
}
