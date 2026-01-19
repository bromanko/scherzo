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

- Write tests alongside implementation
- Run full test suite: `gleam test`
- Check formatting: `gleam format --check src test && nixfmt --check *.nix`

## Dependencies

Add dependencies to `gleam.toml`. Key dependencies per PLAN.md:

- `gleam_otp` - Actors, supervisors
- `gleam_erlang` - Ports, processes
- `glint` - CLI framework
- `simplifile` - File operations
- `gleam_json` - JSON parsing
- `shellout` - Shell commands
- `tom` - TOML parsing
