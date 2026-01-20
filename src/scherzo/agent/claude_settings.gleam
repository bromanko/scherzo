/// Claude Code settings.json generation for agent workspaces
///
/// Generates .claude/settings.json files with hooks that inject context
/// via the scherzo prime command (similar to Gas Town's gt prime)
import gleam/json
import gleam/list
import gleam/string
import scherzo/core/types.{type Id}

/// Safe characters for task IDs in shell commands
/// Only allows alphanumeric, dash, and underscore
fn is_safe_task_id(task_id: Id) -> Bool {
  task_id
  |> string.to_graphemes
  |> list.all(fn(c) {
    case c {
      "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l"
      | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x"
      | "y" | "z" | "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J"
      | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V"
      | "W" | "X" | "Y" | "Z" | "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7"
      | "8" | "9" | "-" | "_" -> True
      _ -> False
    }
  })
}

/// Sanitize task ID for shell command use
/// Returns the task_id if safe, or "invalid-task-id" if it contains unsafe characters
fn sanitize_task_id(task_id: Id) -> String {
  case is_safe_task_id(task_id) {
    True -> task_id
    False -> "invalid-task-id"
  }
}

/// Generate settings.json for an autonomous agent
/// The hooks will inject context via scherzo prime on session start
pub fn generate_autonomous_settings(task_id: Id) -> String {
  json.object([
    #(
      "hooks",
      json.object([
        #("SessionStart", session_start_hooks(task_id)),
        #("Stop", stop_hooks(task_id)),
        #("PreCompact", pre_compact_hooks(task_id)),
      ]),
    ),
  ])
  |> json.to_string
}

/// SessionStart hooks - inject context when agent session begins
fn session_start_hooks(task_id: Id) -> json.Json {
  let safe_id = sanitize_task_id(task_id)
  json.array(
    [
      json.object([
        #("matcher", json.string("")),
        #(
          "hooks",
          json.array(
            [
              json.object([
                #("type", json.string("command")),
                #("command", json.string("scherzo prime " <> safe_id)),
              ]),
            ],
            fn(x) { x },
          ),
        ),
      ]),
    ],
    fn(x) { x },
  )
}

/// Stop hooks - checkpoint state when agent stops
fn stop_hooks(task_id: Id) -> json.Json {
  let safe_id = sanitize_task_id(task_id)
  json.array(
    [
      json.object([
        #("matcher", json.string("")),
        #(
          "hooks",
          json.array(
            [
              json.object([
                #("type", json.string("command")),
                #(
                  "command",
                  json.string("scherzo checkpoint --type=final " <> safe_id),
                ),
              ]),
            ],
            fn(x) { x },
          ),
        ),
      ]),
    ],
    fn(x) { x },
  )
}

/// PreCompact hooks - re-inject context before compaction
fn pre_compact_hooks(task_id: Id) -> json.Json {
  let safe_id = sanitize_task_id(task_id)
  json.array(
    [
      json.object([
        #("matcher", json.string("")),
        #(
          "hooks",
          json.array(
            [
              json.object([
                #("type", json.string("command")),
                #("command", json.string("scherzo prime " <> safe_id)),
              ]),
            ],
            fn(x) { x },
          ),
        ),
      ]),
    ],
    fn(x) { x },
  )
}
