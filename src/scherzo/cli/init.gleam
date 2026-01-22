/// Init command - scaffolds new Scherzo projects
///
/// Creates .scherzo/config.toml with explicit gate definitions.
/// Templates expand to full gate specifications - no hidden abstractions.
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import glint
import scherzo/config/generator
import simplifile

/// Map simplifile errors to InitError
fn map_fs_error(err: simplifile.FileError) -> InitError {
  FilesystemError(simplifile.describe_error(err))
}

/// Error types for initialization
pub type InitError {
  /// Configuration already exists
  AlreadyInitialized(path: String)
  /// Invalid template name
  InvalidTemplate(name: String)
  /// Permission denied or filesystem error
  FilesystemError(reason: String)
}

/// Format an init error as a human-readable message
pub fn format_error(error: InitError) -> String {
  case error {
    AlreadyInitialized(path) ->
      "Already initialized: " <> path <> " exists.\nUse --force to overwrite."
    InvalidTemplate(name) ->
      "Invalid template '"
      <> name
      <> "'. Available templates: "
      <> string.join(generator.available_templates(), ", ")
    FilesystemError(reason) -> "Filesystem error: " <> reason
  }
}

/// Flag for template selection (comma-separated for multiple)
fn template_flag() {
  glint.strings_flag("template")
  |> glint.flag_help(
    "Template(s) to use (comma-separated): none, code-review, security-audit, quick-review",
  )
}

/// Flag for creating tickets directory
fn with_tickets_flag() {
  glint.bool_flag("with-tickets")
  |> glint.flag_default(False)
  |> glint.flag_help("Create .tickets/ directory for task tracking")
}

/// Flag for forcing overwrite
fn force_flag() {
  glint.bool_flag("force")
  |> glint.flag_default(False)
  |> glint.flag_help("Overwrite existing configuration")
}

/// Flag for working directory
fn workdir_flag() {
  glint.string_flag("workdir")
  |> glint.flag_help("Working directory (default: current directory)")
}

/// Build the init command
pub fn command() -> glint.Command(Nil) {
  use <- glint.command_help(
    "Initialize a new Scherzo project with gate configuration",
  )
  use template_getter <- glint.flag(template_flag())
  use with_tickets_getter <- glint.flag(with_tickets_flag())
  use force_getter <- glint.flag(force_flag())
  use workdir_getter <- glint.flag(workdir_flag())
  use _, _, flags <- glint.command()

  // Get flag values
  let working_dir = workdir_getter(flags) |> result.unwrap(".")
  let force = force_getter(flags) |> result.unwrap(False)
  let with_tickets_flag = with_tickets_getter(flags) |> result.unwrap(False)

  // Get templates - prompt if not provided
  let flag_templates = template_getter(flags) |> result.unwrap([])
  let templates = case flag_templates {
    [] -> prompt_templates()
    provided -> filter_none(provided)
  }

  // Get with_tickets if not provided via flag (and not in non-interactive mode)
  let with_tickets = case with_tickets_flag {
    True -> True
    False ->
      case flag_templates {
        // If template was provided (non-interactive), use flag value
        [_, ..] -> with_tickets_flag
        // Otherwise, prompt
        [] -> prompt_tickets()
      }
  }

  // Run initialization
  case initialize(working_dir, templates, with_tickets, force) {
    Ok(created_files) -> {
      io.println("")
      io.println("Created:")
      list.each(created_files, fn(f) { io.println("  " <> f) })
      io.println("")
      let template_display = case templates {
        [] -> "none"
        [single] -> single
        multiple -> string.join(multiple, " + ")
      }
      io.println(
        "Project initialized with '" <> template_display <> "' template.",
      )
      io.println("")
      io.println("All gates are explicitly defined in .scherzo/config.toml")
      io.println("Edit the file to customize commands, prompts, and timeouts.")
    }
    Error(err) -> {
      io.println("Error: " <> format_error(err))
    }
  }

  Nil
}

/// Filter out "none" from template list (none means no templates)
fn filter_none(templates: List(String)) -> List(String) {
  templates
  |> list.filter(fn(s) { s != "none" && s != "" })
}

/// Prompt user to select templates (interactive mode)
fn prompt_templates() -> List(String) {
  io.println("? Select gate template (comma-separated for multiple):")
  io.println("  1. none           - " <> generator.template_description("none"))
  io.println(
    "  2. code-review    - " <> generator.template_description("code-review"),
  )
  io.println(
    "  3. security-audit - " <> generator.template_description("security-audit"),
  )
  io.println(
    "  4. quick-review   - " <> generator.template_description("quick-review"),
  )
  io.println("")
  io.println(
    "Examples: '2' for code-review, '2,3' for code-review + security-audit",
  )
  io.println("")

  let input = read_line("Enter choice(s): ")
  parse_user_selection(input)
}

/// Parse user's selection input (numbers or names, comma-separated)
fn parse_user_selection(input: String) -> List(String) {
  let parts =
    input
    |> string.split(",")
    |> list.map(string.trim)
    |> list.filter(fn(s) { s != "" })

  case parts {
    [] -> []
    _ ->
      parts
      |> list.filter_map(fn(part) {
        case part {
          "1" | "none" -> Error(Nil)
          "2" | "code-review" -> Ok("code-review")
          "3" | "security-audit" -> Ok("security-audit")
          "4" | "quick-review" -> Ok("quick-review")
          other ->
            // Check if it's a valid template name (for forward compatibility)
            case list.contains(generator.available_templates(), other) {
              True -> Ok(other)
              False -> {
                io.println(
                  "Warning: Unknown template '" <> other <> "', skipping",
                )
                Error(Nil)
              }
            }
        }
      })
  }
}

/// Prompt user about creating tickets directory
fn prompt_tickets() -> Bool {
  case read_line("? Create .tickets/ directory for task tracking? [Y/n]: ") {
    "" | "y" | "Y" | "yes" | "Yes" | "YES" -> True
    _ -> False
  }
}

/// Read a line from stdin with prompt
fn read_line(prompt: String) -> String {
  io.print(prompt)
  case erlang_io_get_line("") {
    Ok(line) -> string.trim(line)
    Error(_) -> ""
  }
}

/// FFI to read a line from stdin - wrapper that handles charlist conversion
@external(erlang, "scherzo_io_ffi", "get_line")
fn erlang_io_get_line(_prompt: String) -> Result(String, Nil)

/// Check if a project is already initialized
pub fn is_initialized(working_dir: String) -> Bool {
  let config_path = working_dir <> "/.scherzo/config.toml"
  case simplifile.is_file(config_path) {
    Ok(True) -> True
    _ -> False
  }
}

/// Initialize a new project with given templates
pub fn initialize(
  working_dir: String,
  templates: List(String),
  with_tickets: Bool,
  force: Bool,
) -> Result(List(String), InitError) {
  let config_dir = working_dir <> "/.scherzo"
  let config_path = config_dir <> "/config.toml"
  let tickets_dir = working_dir <> "/.tickets"

  // Check if already initialized (unless force)
  case force {
    False ->
      case is_initialized(working_dir) {
        True -> Error(AlreadyInitialized(config_path))
        False ->
          do_initialize(
            config_dir,
            config_path,
            tickets_dir,
            templates,
            with_tickets,
          )
      }
    True ->
      do_initialize(
        config_dir,
        config_path,
        tickets_dir,
        templates,
        with_tickets,
      )
  }
}

/// Perform the actual initialization
fn do_initialize(
  config_dir: String,
  config_path: String,
  tickets_dir: String,
  templates: List(String),
  with_tickets: Bool,
) -> Result(List(String), InitError) {
  // Validate all templates
  use _ <- result.try(validate_templates(templates))

  // Create .scherzo directory
  use _ <- result.try(
    simplifile.create_directory_all(config_dir)
    |> result.map_error(map_fs_error),
  )

  // Generate and write config with explicit gates
  let content = generator.generate_config(templates)
  use _ <- result.try(
    simplifile.write(config_path, content)
    |> result.map_error(map_fs_error),
  )

  // Create agent config stubs
  use agent_files <- result.try(create_agent_stubs(config_dir))

  // Configure .gitignore (config_dir is working_dir/.scherzo)
  let working_dir = string.drop_end(config_dir, 8)
  use gitignore_modified <- result.try(configure_gitignore(working_dir))

  // Build list of created files
  let created = list.append([".scherzo/config.toml"], agent_files)
  let created = case gitignore_modified {
    True -> list.append(created, [".gitignore (updated)"])
    False -> created
  }

  // Optionally create tickets directory
  case with_tickets {
    False -> Ok(created)
    True -> {
      use _ <- result.try(
        simplifile.create_directory_all(tickets_dir)
        |> result.map_error(map_fs_error),
      )
      Ok(list.append(created, [".tickets/"]))
    }
  }
}

/// Create agent configuration stubs
fn create_agent_stubs(config_dir: String) -> Result(List(String), InitError) {
  let agents_dir = config_dir <> "/agents/task"

  // Create agents/task directory
  use _ <- result.try(
    simplifile.create_directory_all(agents_dir)
    |> result.map_error(map_fs_error),
  )

  // Create CLAUDE.md stub with helpful template
  let claude_md_content =
    "# Task Agent Instructions

<!-- These instructions REPLACE defaults. Customize how the agent works. -->

## Project Context

Describe your project here.

## Coding Standards

- Your conventions
- Framework patterns
- Testing requirements
"

  use _ <- result.try(
    simplifile.write(agents_dir <> "/CLAUDE.md", claude_md_content)
    |> result.map_error(map_fs_error),
  )

  Ok([".scherzo/agents/task/CLAUDE.md"])
}

/// Validate that all templates are known
fn validate_templates(templates: List(String)) -> Result(Nil, InitError) {
  let available = generator.available_templates()
  case list.find(templates, fn(t) { !list.contains(available, t) }) {
    Ok(invalid) -> Error(InvalidTemplate(invalid))
    Error(_) -> Ok(Nil)
  }
}

/// Configure .gitignore to ignore runtime directories but track config
/// Returns True if .gitignore was modified
fn configure_gitignore(working_dir: String) -> Result(Bool, InitError) {
  let gitignore_path = working_dir <> "/.gitignore"

  // Patterns to add (runtime directories that shouldn't be tracked)
  let patterns_to_add = [
    ".scherzo/workspaces/",
    ".scherzo/state/",
    ".scherzo/pipes/",
  ]

  // Patterns to remove (blanket ignores that would hide config)
  let patterns_to_remove = [".scherzo/", ".scherzo"]

  // Read existing content or start empty
  let existing_content = case simplifile.read(gitignore_path) {
    Ok(content) -> content
    Error(_) -> ""
  }

  // Split into lines and filter out blanket patterns
  let existing_lines =
    existing_content
    |> string.split("\n")
    |> list.filter(fn(line) { !list.contains(patterns_to_remove, line) })

  // Check which patterns need to be added
  let patterns_needed =
    patterns_to_add
    |> list.filter(fn(pattern) { !list.contains(existing_lines, pattern) })

  // If nothing to change, return False
  case patterns_needed {
    [] ->
      case
        string.contains(existing_content, ".scherzo/\n")
        || string.contains(existing_content, "\n.scherzo\n")
      {
        False -> Ok(False)
        True -> {
          // Need to remove blanket pattern even if no new patterns to add
          let new_content = string.join(existing_lines, "\n")
          use _ <- result.try(
            simplifile.write(gitignore_path, new_content)
            |> result.map_error(map_fs_error),
          )
          Ok(True)
        }
      }
    _ -> {
      // Add new patterns at the end
      let new_lines = list.append(existing_lines, patterns_needed)
      let new_content = string.join(new_lines, "\n")

      use _ <- result.try(
        simplifile.write(gitignore_path, new_content)
        |> result.map_error(map_fs_error),
      )
      Ok(True)
    }
  }
}
