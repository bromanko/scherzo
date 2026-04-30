import gleam/option.{None, Some}
import gleam/string
import scherzo/main

pub fn parse_args_default_explicit_and_help_test() {
  assert main.parse_args([]) == Ok(main.Run(main.Daemon, None))
  assert main.parse_args(["scherzo.yaml"])
    == Ok(main.Run(main.Daemon, Some("scherzo.yaml")))
  assert main.parse_args(["--once", "scherzo.yaml"])
    == Ok(main.Run(main.Once, Some("scherzo.yaml")))
  assert main.parse_args(["--linear-smoke", "scherzo.yaml"])
    == Ok(main.Run(main.LinearSmoke, Some("scherzo.yaml")))
  assert main.parse_args(["--linear-contract-check", "scherzo.yaml"])
    == Ok(main.Run(main.LinearContractCheck, Some("scherzo.yaml")))
  assert main.parse_args(["--pi-probe", "scherzo.yaml"])
    == Ok(main.Run(main.PiProbe, Some("scherzo.yaml")))
  assert main.parse_args(["--help"]) == Ok(main.Help)
  assert main.parse_args(["ctl", "ps"]) == Ok(main.Control(["ps"]))
  assert main.parse_args(["ctl", "events", "ABC-123"])
    == Ok(main.Control(["events", "ABC-123"]))
  assert main.parse_args(["ctl", "attach", "--raw", "ABC-123"])
    == Ok(main.Control(["attach", "--raw", "ABC-123"]))
}

pub fn parse_args_rejects_usage_errors_test() {
  assert main.parse_args(["one", "two"]) == Error(main.UsageError)
  assert main.parse_args(["--unknown"]) == Error(main.UsageError)
}

pub fn usage_mentions_required_operational_constraints_test() {
  let usage = main.usage()
  assert string.contains(usage, "gleam run -- [mode] [path-to-scherzo.yaml]")
  assert string.contains(usage, "LINEAR_API_KEY")
  assert string.contains(usage, "YAML orchestrator config")
  assert string.contains(usage, "workspace.hooks")
  assert string.contains(usage, "pi --mode rpc")
  assert string.contains(usage, "agent.max_concurrent_agents: 0")
  assert string.contains(usage, "--once")
  assert string.contains(usage, "--linear-smoke")
  assert string.contains(usage, "--linear-contract-check")
  assert string.contains(usage, "--pi-probe")
  assert string.contains(usage, "ctl ps")
  assert string.contains(usage, "ctl attach --raw")
  assert string.contains(usage, "daemon mode")
  assert string.contains(usage, "SIGTERM gracefully")
  assert string.contains(usage, "Ctrl-C/SIGINT")
  assert string.contains(usage, "kill -9")
  assert string.contains(usage, "only one Scherzo instance")
}
