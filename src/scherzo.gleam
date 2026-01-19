import gleam/io
import glint

pub fn main() {
  glint.new()
  |> glint.with_name("scherzo")
  |> glint.pretty_help(glint.default_pretty_help())
  |> glint.add(at: [], do: root_command())
  |> glint.add(at: ["run"], do: run_command())
  |> glint.add(at: ["status"], do: status_command())
  |> glint.run(glint.default_args())
}

fn root_command() -> glint.Command(Nil) {
  use <- glint.command_help("Scherzo - AI Agent Orchestrator")
  use _, _, _ <- glint.command()
  io.println("Scherzo - AI Agent Orchestrator")
  io.println("Use 'scherzo --help' for usage information")
  Nil
}

fn run_command() -> glint.Command(Nil) {
  use <- glint.command_help("Start the orchestrator")
  use _, _, _ <- glint.command()
  io.println("Starting orchestrator...")
  // TODO: Implement orchestrator startup
  Nil
}

fn status_command() -> glint.Command(Nil) {
  use <- glint.command_help("Show orchestrator status")
  use _, _, _ <- glint.command()
  io.println("Status: Not running")
  // TODO: Implement status check
  Nil
}
