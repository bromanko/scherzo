# Custom Agent Configurations

Allow users to customize agent behavior via files in `.scherzo/agents/<name>/`.

**Scope:** This feature is Claude-only. Codex and Gemini agent customization is not yet supported.

## Overview

```
.scherzo/
├── config.toml
└── agents/
    ├── task/                   # Main task agent
    │   ├── CLAUDE.md           # Custom instructions (replaces defaults)
    │   └── settings.json       # Claude settings overrides (merged)
    └── <gate-name>/            # Gate-specific (deferred to Phase 5c-5e)
        ├── CLAUDE.md
        └── settings.json
```

## Design Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| CLAUDE.md behavior | Replaces defaults | Simple, predictable - no inheritance complexity |
| CLAUDE.md location | Workspace root | Claude Code reads from working directory, not `.claude/` |
| settings.json behavior | Merged | Auto-generated hooks preserved, custom hooks added |
| Repo settings.json | Ignored | Workspace is isolated; restored on cleanup via `jj restore` |
| Folder naming | Agent type | `task` for main agent, gate names for gate agents |
| Init creates | `task/` stub | Most common customization point |
| Gate agent config | Deferred | Integrated when gate agents implemented (Phase 5c-5e) |
| Error on bad JSON | Yes | Fail loudly so users know their config isn't applied |

## File Behavior

### CLAUDE.md (Instructions)

Custom instructions **replace** any default instructions. This is intentional:

- Simple mental model - what you write is what the agent sees
- No hidden inheritance or extension mechanisms
- Full control over agent behavior

If the file doesn't exist, no custom instructions are used (agent uses its built-in behavior).

### settings.json (Settings Overrides)

Custom settings are **merged** with auto-generated settings:

1. Auto-generated hooks (scherzo prime, checkpoint) are always preserved
2. Custom hooks are **appended** to each event's hook array
3. Other settings (non-hooks) from custom file **override** auto-generated values

**Important:** The hook format must match Claude Code's nested structure (see `claude_settings.gleam`).

Example merge:

```json
// Auto-generated (preserved) - see claude_settings.gleam for actual format
{
  "hooks": {
    "SessionStart": [
      {"matcher": "", "hooks": [{"type": "command", "command": "scherzo prime <task-id>"}]}
    ],
    "Stop": [
      {"matcher": "", "hooks": [{"type": "command", "command": "scherzo checkpoint --type=final <task-id>"}]}
    ],
    "PreCompact": [
      {"matcher": "", "hooks": [{"type": "command", "command": "scherzo prime <task-id>"}]}
    ]
  }
}

// Custom settings.json - add PostToolUse hooks
{
  "hooks": {
    "PostToolUse": [
      {"matcher": "Edit", "hooks": [{"type": "command", "command": "echo 'File edited'"}]}
    ]
  },
  "model": "claude-sonnet-4-20250514"
}

// Result (merged)
{
  "hooks": {
    "SessionStart": [
      {"matcher": "", "hooks": [{"type": "command", "command": "scherzo prime <task-id>"}]}
    ],
    "Stop": [
      {"matcher": "", "hooks": [{"type": "command", "command": "scherzo checkpoint --type=final <task-id>"}]}
    ],
    "PreCompact": [
      {"matcher": "", "hooks": [{"type": "command", "command": "scherzo prime <task-id>"}]}
    ],
    "PostToolUse": [
      {"matcher": "Edit", "hooks": [{"type": "command", "command": "echo 'File edited'"}]}
    ]
  },
  "model": "claude-sonnet-4-20250514"
}
```

**Note:** If custom settings.json adds hooks for the same event (e.g., another SessionStart hook), they are appended to that event's array, not replaced.

## Agent Names

| Agent Name | Used For |
|------------|----------|
| `task` | Main task agent (works on tickets) |
| `code-review` | Code review gate agent |
| `security-review` | Security review gate agent |
| `<custom-gate>` | Any custom gate name from config |

Gate names come from the `name` field in `[[gates.list]]` entries in config.toml.

## Implementation

### Phase 1: Agent Config Loader

**Create: `src/scherzo/config/agent_config.gleam`**

```gleam
/// Custom configuration for an agent loaded from .scherzo/agents/<name>/
pub type AgentCustomConfig {
  AgentCustomConfig(
    name: String,
    instructions: Option(String),       // CLAUDE.md content
    settings_overrides: Option(String), // settings.json content (raw JSON)
  )
}

/// Returns an empty config (no customizations)
pub fn empty() -> AgentCustomConfig

/// Load custom config for any agent by name
/// Returns empty config (not error) if directory doesn't exist
pub fn load(repo_dir: String, agent_name: String) -> Result(AgentCustomConfig, String)

/// Convenience function for loading task agent config
pub fn load_task_config(repo_dir: String) -> Result(AgentCustomConfig, String)
```

Implementation notes:
- Reads from `.scherzo/agents/<agent_name>/CLAUDE.md` and `settings.json`
- Returns empty config if directory doesn't exist (not an error)
- Returns error only for I/O failures or malformed JSON

### Phase 2: Settings Merger

**Create: `src/scherzo/config/settings_merger.gleam`**

```gleam
/// Merge auto-generated settings with custom overrides
/// - Hooks array: auto-generated first, custom appended
/// - Other fields: custom overrides auto-generated
pub fn merge(auto_generated: String, custom: String) -> Result(String, String)
```

Implementation notes:
- Parse both JSON strings
- Extract and combine `hooks` arrays (auto-generated first)
- Merge other top-level keys (custom wins)
- Return error for invalid JSON

### Phase 3: Workspace Integration

**Modify: `src/scherzo/agent/workspace.gleam`**

1. Add optional custom config to `create()`:

```gleam
pub fn create(
  config: WorkspaceConfig,
  task: Task,
  custom_config: Option(AgentCustomConfig),  // NEW
) -> Result(Workspace, String)
```

2. Modify `write_claude_settings()` to accept and merge custom settings:

```gleam
fn write_claude_settings(
  workspace: Workspace,
  task_id: Id,
  custom_settings: Option(String),  // NEW: raw JSON to merge
) -> Result(Nil, String)
```

3. Add function to write CLAUDE.md:

```gleam
fn write_claude_instructions(
  workspace: Workspace,
  instructions: String,
) -> Result(Nil, String)
```

Writes to `<workspace_path>/CLAUDE.md` (workspace root, NOT `.claude/` directory).

4. Update `create()` flow:
   - Generate base settings
   - Merge with custom settings if present
   - Write merged settings
   - Write CLAUDE.md if custom instructions exist

5. Update `destroy()` to restore CLAUDE.md:
   - Add `jj.restore_file(workspace.path, "CLAUDE.md")` alongside existing settings.json restore
   - Prevents custom instructions from polluting commit history

### Phase 4: Process Integration

**Modify: `src/scherzo/agent/process.gleam`**

In `handle_start()`:

```gleam
// Load custom config for task agent - fail if config exists but is invalid
case agent_config.load_task_config(state.config.working_dir) {
  Error(err) -> {
    process.send(reply_to, StartFailed("Invalid agent config: " <> err))
    actor.continue(state)
  }
  Ok(custom_config) -> {
    case workspace.create(workspace_config, task, Some(custom_config)) {
      // ...
    }
  }
}
```

**Important:** Do NOT silently swallow errors. If a user has created a custom config with invalid JSON, they need to know it's not being applied.

**Note:** Gate agent config loading is deferred until gate agents are implemented (Phase 5c-5e). At that point, use:

```gleam
agent_config.load(state.config.working_dir, gate_name)
```

### Phase 5: Init Command

**Modify: `src/scherzo/cli/init.gleam`**

1. Add helper to create agent stubs:

```gleam
fn create_agent_stubs(config_dir: String) -> Result(List(String), InitError)
```

2. Stub content for `task/CLAUDE.md`:

```markdown
# Task Agent Instructions

<!-- These instructions REPLACE defaults. Customize how the agent works. -->

## Project Context

Describe your project here.

## Coding Standards

- Your conventions
- Framework patterns
- Testing requirements
```

3. Modify `do_initialize()` to call `create_agent_stubs()` after writing config.toml

4. Add created files to the output list

## Files Changed

| File | Change |
|------|--------|
| `src/scherzo/config/agent_config.gleam` | **NEW** - Load custom configs |
| `src/scherzo/config/settings_merger.gleam` | **NEW** - Merge settings.json |
| `src/scherzo/agent/workspace.gleam` | Add custom config parameter, write CLAUDE.md |
| `src/scherzo/agent/process.gleam` | Load and pass custom config |
| `src/scherzo/cli/init.gleam` | Create agent stubs on init |

## Testing

### Unit Tests

Create test files:
- `test/scherzo/config/agent_config_test.gleam`
- `test/scherzo/config/settings_merger_test.gleam`

Test cases for `agent_config`:
- Load config from existing directory with both files
- Load config from directory with only CLAUDE.md
- Load config from directory with only settings.json
- Return empty config when directory doesn't exist
- Return error for malformed settings.json

Test cases for `settings_merger`:
- Merge hook objects correctly (auto-generated events preserved)
- Custom hooks for same event are appended to event's array
- Custom hooks for new events are added
- Custom non-hook settings override auto-generated
- Handle missing hooks object gracefully
- Return error for invalid JSON

### Integration Test

Create `test/scherzo/agent/workspace_custom_config_test.gleam`:

```gleam
// Test end-to-end custom config flow
pub fn custom_config_appears_in_workspace_test() {
  // 1. Create temp directory with .scherzo/agents/task/CLAUDE.md
  // 2. Create workspace with custom config
  // 3. Verify <workspace>/CLAUDE.md contains custom instructions
  // 4. Verify <workspace>/.claude/settings.json has merged hooks
  // 5. Destroy workspace
  // 6. Verify CLAUDE.md restored (not in jj diff)
}
```

### Manual Testing

1. **Init creates stub**:
   ```bash
   scherzo init
   # Verify .scherzo/agents/task/CLAUDE.md exists
   ```

2. **Custom instructions appear in workspace**:
   ```bash
   echo "# My Custom Instructions" > .scherzo/agents/task/CLAUDE.md
   # Run a task
   # Check .scherzo/workspaces/<task-id>/CLAUDE.md contains custom content
   ```

3. **Custom hooks merge correctly**:
   ```bash
   # Use correct hook format (see claude_settings.gleam)
   cat > .scherzo/agents/task/settings.json << 'EOF'
   {
     "hooks": {
       "PostToolUse": [
         {"matcher": "Edit", "hooks": [{"type": "command", "command": "echo edited"}]}
       ]
     }
   }
   EOF
   # Run a task
   # Check .scherzo/workspaces/<task-id>/.claude/settings.json has merged hooks
   ```

4. **Invalid JSON fails loudly**:
   ```bash
   echo '{bad json' > .scherzo/agents/task/settings.json
   # Run a task
   # Should fail with "Invalid agent config" error, NOT start silently
   ```

5. **Workspace cleanup restores files**:
   ```bash
   # After task completes, verify:
   jj diff  # Should NOT show CLAUDE.md or .claude/settings.json changes
   ```

### Build Verification

```bash
gleam build  # No errors
gleam test   # All tests pass
```

## Notes

### Handoff Behavior

When an agent exhausts context and hands off to a fresh agent, the new agent loads custom config fresh from `.scherzo/agents/`. This means:
- Custom instructions are preserved across handoffs
- No special handling needed - config is read from repo_dir, not workspace

### Migration

Existing projects without `.scherzo/agents/` don't need migration. The directory is optional:
- If it doesn't exist, agents use default behavior
- Users can create it manually
- `scherzo init --force` will create the stub (but also overwrites config.toml)
