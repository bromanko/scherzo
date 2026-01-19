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

## Project Structure

Follow the structure defined in `PLAN.md`. Key directories:

- `src/scherzo/` - Main source code organized by module
- `test/` - Tests mirroring src structure
- `.scherzo/` - Runtime state (not committed, jj-tracked during operation)

## Development Workflow

1. Read `PLAN.md` to understand current phase and goals
2. Use `nix develop` or `direnv allow` for environment
3. Run `gleam test` before and after changes
4. Format code before committing (hooks will enforce this)
5. Commit completed work before ending session

## Code Quality

### Adding a New Language

When adding a new language or file type to the project, update all validation layers:

1. **flake.nix** - Add formatter/linter to `buildInputs`
2. **lefthook.yml** - Add pre-commit format check, pre-push lint/test if applicable
3. **.claude/settings.json** - Add PostToolUse auto-format hook, Stop warning hook
4. **.github/workflows/ci.yml** - Add format check step
5. **CONTRIBUTING.md** - Document formatting commands
6. **This file** - Update formatting commands in Code Quality section

### Claude Code Hooks

This project has Claude Code hooks configured in `.claude/settings.json`:

- **PostToolUse**: Auto-formats `.gleam` and `.nix` files after Edit/Write
- **Stop**: Warns about formatting issues before session ends

These hooks help maintain code quality automatically during development.

### Git Hooks

Git hooks (via lefthook) enforce quality on commit and push:

```bash
# Gleam
gleam format src test

# Nix
nixfmt flake.nix
```

Pre-commit hooks check formatting. Pre-push hooks run tests and build.

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
