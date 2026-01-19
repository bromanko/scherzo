# Scherzo Project Instructions

## Development Workflow

### Committing Changes
Commit frequently after logical chunks of work:
- After adding a new module
- After completing a feature
- After a batch of related changes

Use descriptive commit messages that explain the "why" not just the "what".

### Building and Testing
```bash
gleam build    # Compile the project
gleam test     # Run tests
gleam format   # Format code
```

### Running Scherzo
```bash
gleam run -- --help                           # Show help
gleam run -- run "title" "description"        # Run a task
gleam run -- run "title" --workdir /path      # Run with specific workdir
gleam run -- status                           # Show status
```

## Project Structure
See PLAN.md for architecture details and implementation phases.
