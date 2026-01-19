# Contributing to Scherzo

## Development Environment Setup

### Prerequisites

- [Nix](https://nixos.org/download.html) with flakes enabled
- [direnv](https://direnv.net/) (recommended)

### Quick Start

1. Clone the repository:
   ```bash
   git clone https://github.com/your-org/scherzo.git
   cd scherzo
   ```

2. Allow direnv (if installed):
   ```bash
   direnv allow
   ```

   Or manually enter the dev shell:
   ```bash
   nix develop
   ```

3. Verify the environment:
   ```bash
   gleam --version
   jj --version
   ```

### What's Included

The Nix flake provides:

| Tool | Purpose |
|------|---------|
| gleam | Gleam compiler |
| erlang | Erlang/OTP runtime |
| rebar3 | Erlang build tool |
| jujutsu (jj) | Version control |
| tmux | Terminal multiplexer |
| just | Command runner |
| nixfmt | Nix formatter |
| lefthook | Git hooks manager |

## Git Hooks

Git hooks are managed by [lefthook](https://github.com/evilmartians/lefthook) and installed automatically when entering the dev shell.

### Pre-commit

Runs on every commit:
- **gleam-format**: Checks Gleam code formatting
- **nix-format**: Checks Nix code formatting

### Pre-push

Runs before pushing:
- **gleam-test**: Runs test suite
- **gleam-build**: Verifies build succeeds

### Manual Hook Installation

If hooks aren't installed automatically:
```bash
lefthook install
```

### Bypassing Hooks

In rare cases where you need to skip hooks:
```bash
jj commit --no-verify -m "message"  # Skip pre-commit
jj git push --no-verify             # Skip pre-push
```

## Development Workflow

### Running Tests

```bash
gleam test
```

### Formatting Code

Gleam:
```bash
gleam format src test
gleam format --check src test  # Check only
```

Nix:
```bash
nixfmt flake.nix
nixfmt --check flake.nix  # Check only
```

### Building

```bash
gleam build
```

### Version Control

This project uses **jujutsu (jj)** instead of git for version control.

Common commands:

```bash
jj status          # Show working copy status
jj log             # Show commit history
jj diff            # Show changes
jj commit -m "msg" # Create commit
jj new             # Start new change
```

### Creating a Change

1. Start a new change:
   ```bash
   jj new -m "Description of your change"
   ```

2. Make your changes

3. Describe the change when done:
   ```bash
   jj describe -m "Final description"
   ```

4. Push to remote:
   ```bash
   jj git push
   ```

## Code Style

- Follow standard Gleam formatting (`gleam format`)
- Use meaningful names for functions and types
- Add type annotations to public functions
- Keep functions small and focused

## Pull Request Process

1. Ensure all tests pass
2. Ensure code is formatted
3. Update documentation if needed
4. Create PR with clear description

## Project Structure

```
scherzo/
├── src/
│   ├── scherzo.gleam          # Entry point
│   └── scherzo/               # Modules
├── test/                      # Tests
├── flake.nix                  # Nix development environment
├── gleam.toml                 # Gleam project config
└── PLAN.md                    # Project roadmap
```
