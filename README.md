# Scherzo

An AI agent orchestrator for coordinating CLI-based AI agents (Claude Code, Codex CLI, Gemini CLI) to autonomously execute software engineering tasks.

## What is this?

Scherzo manages AI agents working on tasks with support for:

- **Task orchestration**: Process tasks from ticket files or create them ad-hoc
- **Workspace isolation**: Each agent runs in its own [Jujutsu](https://martinvonz.github.io/jj/) workspace
- **Context continuity**: When an agent exhausts its context window, a fresh agent takes over with full state
- **Checkpointing**: Automatic state snapshots for crash recovery and handoff
- **Interactive console**: tmux-based interface for monitoring task execution

## Current State

This project is in early development - not even a v0. The core architecture is in place but many features are incomplete or untested. Expect breaking changes, rough edges, and missing functionality.

## Running Scherzo

There's no pre-built binary. You'll need to run from source.

### Prerequisites

- [Nix](https://nixos.org/) with flakes enabled (recommended), or:
- Gleam, Erlang/OTP, Jujutsu, and tmux installed manually

### Setup

```bash
git clone <repo-url>
cd scherzo

# Enter development shell (installs all dependencies)
nix develop
# or if using direnv:
direnv allow

# Build the project
gleam build
```

### Running

All commands use `gleam run --` as the entry point:

```bash
# Run a single task
gleam run -- run "Fix the login bug"
gleam run -- run "Add dark mode" "Implement a dark mode toggle in settings"

# Process tasks from .tickets/ directory
gleam run -- run --from-tickets

# Check status
gleam run -- status
gleam run -- tasks
gleam run -- agents
```

## Usage

### CLI Commands

**Task execution:**
```bash
gleam run -- run "Task title"                    # Run a task
gleam run -- run "Title" "Description"           # With description
gleam run -- run --from-tickets                  # Process ticket files
gleam run -- run --from-tickets --max-tasks 5    # Limit batch size
```

**Information:**
```bash
gleam run -- status    # Orchestrator status summary
gleam run -- tasks     # List all tasks
gleam run -- agents    # Show agent status
```

### Interactive Console

The console runs in a tmux session with a REPL for monitoring and control:

```bash
gleam run -- console   # Start or attach to console session
gleam run -- attach    # Attach to existing session
gleam run -- repl      # Standalone REPL (no tmux)
```

REPL commands:
```
status   - Show orchestrator status
tasks    - List all tasks
agents   - Show agent status
help     - Available commands
quit     - Exit
```

### Task Sources

Tasks can come from:

1. **Ad-hoc**: Created via the `run` command
2. **Tickets**: Markdown files in `.tickets/` directory (requires [tk](https://github.com/anthropics/tk) CLI)

## How It Works

1. **Task arrives** from a ticket file or command line
2. **Workspace created** - an isolated Jujutsu workspace for the agent
3. **Agent spawned** - Claude Code (or other provider) starts in the workspace
4. **Hooks fire** - SessionStart injects task context, PostToolUse saves checkpoints
5. **Work happens** - agent makes changes, commits to jj
6. **Agent stops** - either completes, fails, or exhausts context
7. **Handoff** (if needed) - fresh agent continues with checkpoint state
8. **Cleanup** - workspace destroyed, changes preserved in main repo

All orchestrator state lives in `.scherzo/` and is tracked by Jujutsu for durability and history.

## Project Structure

```
src/scherzo/
├── agent/           # Agent management, checkpoints, handoff
├── core/            # Types, shell execution, events
├── state/           # State persistence
├── task/            # Task sources and queue
├── ui/              # REPL, console, tmux integration
├── vcs/             # Jujutsu integration
└── orchestrator.gleam
```

## Development

```bash
gleam test           # Run tests
gleam format src     # Format code
gleam build          # Build

# Version control (this project uses jj, not git)
jj status
jj diff
jj commit -m "message"
```

## License

MIT
