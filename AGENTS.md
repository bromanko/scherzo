# Agent Guidelines for Scherzo

## Maintaining This File

This file should evolve with the project. Update AGENTS.md when you:

- Discover patterns that future agents should follow
- Encounter pitfalls that should be documented
- Establish new conventions during implementation
- Learn project-specific idioms or practices

Keep instructions concrete and actionable. Remove guidance that becomes obsolete.

## Version Control Discipline

This project uses **jujutsu (jj)** for version control. Agents must commit work incrementally.

### Commit Cadence

- **Commit after each phase or milestone** - not every file change, but logical units of work
- **Commit before moving to a new phase** - ensure previous work is captured
- **Keep the working copy clean** between logical units

### Commit Workflow

```bash
# Check what's changed
jj status

# Commit with descriptive message
jj commit -m "type: short description

- Detail 1
- Detail 2"

# Verify commit succeeded
jj log -n 2
```

### Commit Message Format

```
type: short description

- Bullet points for details
- Reference phase if applicable
```

Types: `feat`, `fix`, `refactor`, `docs`, `test`, `chore`

### Never Leave Commits Undescribed

**Every commit must have a description.** Undescribed commits create confusion and make history hard to understand.

- `jj commit` without `-m` will fail if no editor is configured - always use `jj commit -m "..."`
- If you create a new change with `jj new`, describe it immediately with `jj describe -m "..."`
- Before ending a session, run `jj log -n 5` to verify no commits are "(no description set)"
- Empty commits (no file changes) should generally be abandoned with `jj abandon`

```bash
# BAD: Creates undescribed commit
jj new

# GOOD: Create and describe immediately
jj new -m "feat: add user authentication"

# Or describe after creating
jj new
jj describe -m "feat: add user authentication"
```

## Project Structure

Follow the structure defined in `PLAN.md`. Key directories:

- `src/scherzo/` - Main source code organized by module
- `test/` - Tests mirroring src structure
- `.scherzo/` - Runtime state (gitignored, jj-tracked during operation)
  - `.scherzo/workspaces/<task-id>/` - Per-agent jj workspaces with isolated `.claude` configs

## Agent Workspace Architecture

When Scherzo spawns an agent, it creates an isolated **jj workspace**:

```
.scherzo/workspaces/<task-id>/
├── .claude/
│   └── settings.json    # Agent-specific hooks (SessionStart → scherzo prime)
├── .scherzo/
│   └── task.json        # Task metadata for hooks to read
└── <project files>      # Full working copy from jj workspace
```

**Key modules:**
- `workspace.gleam` - Creates/destroys jj workspaces, writes settings.json and task.json
- `claude_settings.gleam` - Generates `.claude/settings.json` with hooks
- `scherzo.gleam` - Implements `scherzo prime` and `scherzo checkpoint` CLI commands

**Flow:**
1. `workspace.create()` creates jj workspace + writes config files
2. Agent runs in workspace directory with `SCHERZO_TASK_ID` env var
3. `SessionStart` hook calls `scherzo prime` → outputs task context
4. Agent works on task
5. `Stop` hook calls `scherzo checkpoint` → saves state
6. `workspace.destroy()` cleans up

## Task Management

This project uses a CLI ticket system for task management. Run `tk help` when you need to use it.

## Development Workflow

1. Read `PLAN.md` to understand current phase and goals
2. Use `nix develop` or `direnv allow` for environment
3. Run `gleam test` before and after changes
4. Format code before committing (hooks will enforce this)
5. Commit completed work before ending session

## Code Quality

### Compiler Warnings

**Always fix compiler warnings.** Do not leave warnings in the codebase. Run `gleam build` and verify zero warnings before committing.

Common warnings and fixes:

| Warning | Fix |
|---------|-----|
| Unused import | Remove the import line |
| Unused imported item (`{None, Some}`) | Remove from import or use module prefix instead |
| Unused imported type | Remove from import braces |
| Unused variable | Prefix with `_` or remove |
| Unreachable pattern | Remove the pattern (see below) |
| Inefficient `list.length` for empty check | Use `list != []` instead of `list.length(list) > 0` |

**Unreachable patterns in tests:** When you create a specific value and match on it, other patterns are unreachable:

```gleam
// BAD - compiler knows result is always RunSuccess
let result = RunSuccess(output: "done", change_id: "abc")
case result {
  RunSuccess(output, _) -> output |> should.equal("done")
  _ -> should.fail()  // Warning: unreachable
}

// GOOD - use let pattern instead
let result = RunSuccess(output: "done", change_id: "abc")
let RunSuccess(output, _) = result
output |> should.equal("done")
```

**Import only what you use:** Prefer importing the module and using qualified names over importing specific constructors you might not use:

```gleam
// Preferred - clear and no unused import warnings
import gleam/option
option.Some(value)

// Only import constructors if used frequently
import gleam/option.{None, Some}
```

### Adding a New Language

When adding a new language or file type to the project, update all validation layers:

1. **flake.nix** - Add formatter/linter to `buildInputs`
2. **.claude/settings.json** - Add PostToolUse auto-format hook, Stop warning hook
3. **.github/workflows/ci.yml** - Add format check step
4. **CONTRIBUTING.md** - Document formatting commands
5. **This file** - Update formatting commands in Code Quality section

### Claude Code Hooks

This project has Claude Code hooks configured in `.claude/settings.json`:

- **PostToolUse**: Auto-formats `.gleam` and `.nix` files after Edit/Write
- **Stop**: Warns about formatting issues before session ends

These hooks help maintain code quality automatically during development.

### Code Quality Enforcement

Since this project uses jj (not git), traditional git hooks don't apply. Quality enforcement relies on:

1. **Claude Code hooks** - Auto-format `.gleam` and `.nix` files after Edit/Write
2. **CI checks** - GitHub Actions validates formatting (authoritative)

```bash
# Manual formatting if needed
gleam format src test
nixfmt flake.nix
```

## Testing

**Tests are required for all new functionality.** Do not consider a phase complete without tests.

### Test Structure

Tests mirror the source structure:

```
src/scherzo/core/task.gleam    -> test/scherzo/core/task_test.gleam
src/scherzo/event/bus.gleam    -> test/scherzo/event/bus_test.gleam
src/scherzo/state/store.gleam  -> test/scherzo/state/store_test.gleam
```

### Running Tests

```bash
gleam test                    # Run all tests
gleam test -- -m task_test    # Run specific module (if supported)
```

### Test Naming

Use descriptive names that explain the behavior:

```gleam
pub fn is_terminal_returns_true_for_completed_test() { ... }
pub fn save_and_get_task_test() { ... }
pub fn unsubscribe_stops_receiving_events_test() { ... }
```

### What to Test

- **Types/functions**: Pure functions, constructors, predicates
- **Actors**: Start/stop, message handling, state changes
- **Integration**: Component interactions when relevant

### gleeunit Assertions

```gleam
import gleeunit/should

value |> should.equal(expected)
value |> should.be_true
value |> should.be_false
result |> should.be_ok
result |> should.be_error
list |> list.length |> should.equal(n)  // No have_length in gleeunit
```

## Dependencies

Add dependencies to `gleam.toml`. Key dependencies per PLAN.md:

- `gleam_otp` - Actors, supervisors
- `gleam_erlang` - Ports, processes
- `glint` - CLI framework
- `simplifile` - File operations
- `gleam_json` - JSON parsing
- `shellout` - Shell commands
- `tom` - TOML parsing
- `argv` - Command line arguments

## Gleam Patterns

### gleam_otp Actor API

Use the builder pattern for actors:

```gleam
// Create and start an actor
pub fn start() -> Result(Subject(Message), actor.StartError) {
  actor.new(initial_state)
  |> actor.on_message(handle_message)
  |> actor.start
  |> result.map(fn(started) { started.data })
}

// Handler signature: (state, message) -> Next(State, Message)
fn handle_message(state: State, message: Message) -> actor.Next(State, Message) {
  case message {
    SomeMessage -> actor.continue(new_state)
    Shutdown -> actor.stop()
  }
}

// Call with timeout: actor.call(subject, timeout_ms, message_fn)
actor.call(store, 5000, GetItem(_, item_id))
```

### glint CLI

Use `argv` for command line arguments:

```gleam
import argv

pub fn main() {
  glint.new()
  |> glint.add(at: ["cmd"], do: my_command())
  |> glint.run(argv.load().arguments)
}
```
