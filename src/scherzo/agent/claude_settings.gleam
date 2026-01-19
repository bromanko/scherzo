/// Claude Code settings.json generation for agent workspaces
///
/// Generates .claude/settings.json files with hooks that inject context
/// via the scherzo prime command (similar to Gas Town's gt prime)
import gleam/json
import scherzo/core/types.{type Id}

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
                #("command", json.string("scherzo prime " <> task_id)),
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
                #("command", json.string("scherzo checkpoint " <> task_id)),
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
                #("command", json.string("scherzo prime " <> task_id)),
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
