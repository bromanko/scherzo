import gleam/option.{None, Some}
import gleam/string
import scherzo/doctor
import scherzo/local_workflow_run
import scherzo/main

pub fn parse_args_default_explicit_and_help_test() {
  assert main.parse_args([]) == Ok(main.Run(main.Daemon, None))
  assert main.parse_args(["scherzo.yaml"])
    == Ok(main.Run(main.Daemon, Some("scherzo.yaml")))
  assert main.parse_args(["--once", "scherzo.yaml"])
    == Ok(main.Run(main.Once, Some("scherzo.yaml")))
  assert main.parse_args(["--tracker-smoke", "scherzo.yaml"])
    == Ok(main.Run(main.LinearSmoke, Some("scherzo.yaml")))
  assert main.parse_args(["--tracker-contract-check", "scherzo.yaml"])
    == Ok(main.Run(main.LinearContractCheck, Some("scherzo.yaml")))
  assert main.parse_args(["--pi-probe", "scherzo.yaml"])
    == Ok(main.Run(main.PiProbe, Some("scherzo.yaml")))
  assert main.parse_args([
      "--linear-attach-comment-file",
      "comment-id",
      "result.md",
    ])
    == Ok(main.LinearAttachCommentFile("comment-id", "result.md", None))
  assert main.parse_args([
      "--linear-attach-comment-file",
      "comment-id",
      "result.md",
      "scherzo.yaml",
    ])
    == Ok(main.LinearAttachCommentFile(
      "comment-id",
      "result.md",
      Some("scherzo.yaml"),
    ))
  assert main.parse_args(["--help"]) == Ok(main.Help)
  assert main.parse_args(["--version"]) == Ok(main.Version)
  assert main.parse_args([
      "workflow",
      "run",
      ".scherzo/workflows/implementation.yaml",
    ])
    == Ok(
      main.WorkflowRun(local_workflow_run.Options(
        workflow_path: ".scherzo/workflows/implementation.yaml",
        run_root: "tmp/scherzo-workflow-run",
        run_id: "local-workflow-run",
        native_review_scenario: None,
      )),
    )
  assert main.parse_args([
      "workflow",
      "run",
      ".scherzo/workflows/implementation.yaml",
      "--run-root",
      "tmp/native",
      "--run-id",
      "native-pr-80",
      "--native-review-scenario",
      "pr-80",
    ])
    == Ok(
      main.WorkflowRun(local_workflow_run.Options(
        workflow_path: ".scherzo/workflows/implementation.yaml",
        run_root: "tmp/native",
        run_id: "native-pr-80",
        native_review_scenario: Some("pr-80"),
      )),
    )
  assert main.parse_args([
      "tracker-conformance",
      "run",
      "manifest.json",
      "--report",
      "report.json",
    ])
    == Ok(
      main.TrackerConformance([
        "run",
        "manifest.json",
        "--report",
        "report.json",
      ]),
    )
  assert main.parse_args(["ctl", "ps"]) == Ok(main.Control(["ps"]))
  assert main.parse_args(["ctl", "events", "ABC-123"])
    == Ok(main.Control(["events", "ABC-123"]))
  assert main.parse_args(["ctl", "attach", "--raw", "ABC-123"])
    == Ok(main.Control(["attach", "--raw", "ABC-123"]))
  assert main.parse_args(["doctor"])
    == Ok(main.Doctor(doctor.Options(None, [], False, doctor.Human)))
  assert main.parse_args(["doctor", "scherzo.yaml"])
    == Ok(
      main.Doctor(doctor.Options(Some("scherzo.yaml"), [], False, doctor.Human)),
    )
  assert main.parse_args([
      "doctor",
      "--check",
      "tracker-smoke",
      "--check",
      "pi-probe",
      "scherzo.yaml",
    ])
    == Ok(
      main.Doctor(doctor.Options(
        Some("scherzo.yaml"),
        ["tracker-smoke", "pi-probe"],
        False,
        doctor.Human,
      )),
    )
  assert main.parse_args(["doctor", "--list-checks"])
    == Ok(main.Doctor(doctor.Options(None, [], True, doctor.Human)))
  assert main.parse_args(["doctor", "--logfmt"])
    == Ok(main.Doctor(doctor.Options(None, [], False, doctor.Logfmt)))
}

pub fn parse_args_rejects_usage_errors_test() {
  assert main.parse_args(["one", "two"]) == Error(main.UsageError)
  assert main.parse_args(["--unknown"]) == Error(main.UsageError)
  assert main.parse_args(["--linear-smoke"]) == Error(main.UsageError)
  assert main.parse_args(["--linear-smoke", "scherzo.yaml"])
    == Error(main.UsageError)
  assert main.parse_args(["--linear-contract-check"]) == Error(main.UsageError)
  assert main.parse_args(["--linear-contract-check", "scherzo.yaml"])
    == Error(main.UsageError)
  assert main.parse_args(["doctor", "--unknown"]) == Error(main.UsageError)
  assert main.parse_args(["doctor", "--check"]) == Error(main.UsageError)
  assert main.parse_args(["doctor", "one.yaml", "two.yaml"])
    == Error(main.UsageError)
  assert main.parse_args(["workflow", "run"]) == Error(main.UsageError)
  assert main.parse_args(["workflow", "run", "workflow.yml", "--bad"])
    == Error(main.UsageError)
  assert main.parse_args(["--linear-attach-comment-file", "comment-id"])
    == Error(main.UsageError)
  assert main.parse_args([
      "--linear-attach-comment-file",
      "comment-id",
      "result.md",
      "one.yaml",
      "two.yaml",
    ])
    == Error(main.UsageError)
}

pub fn usage_error_hint_reports_retired_flag_replacements_test() {
  assert main.usage_error_hint(["--linear-smoke"])
    == Some("--linear-smoke was retired; use --tracker-smoke.")
  assert main.usage_error_hint(["--linear-smoke", "scherzo.yaml"])
    == Some("--linear-smoke was retired; use --tracker-smoke.")
  assert main.usage_error_hint(["--linear-contract-check"])
    == Some(
      "--linear-contract-check was retired; use --tracker-contract-check.",
    )
  assert main.usage_error_hint([
      "--linear-contract-check",
      "scherzo.yaml",
    ])
    == Some(
      "--linear-contract-check was retired; use --tracker-contract-check.",
    )
  assert main.usage_error_hint(["--unknown"]) == None
}

pub fn launcher_route_uses_canonical_cli_parser_test() {
  assert main.launcher_route([]) == main.LauncherDaemon
  assert main.launcher_route([".scherzo/scherzo.yaml"]) == main.LauncherDaemon
  assert main.launcher_route(["--help"]) == main.LauncherDirect
  assert main.launcher_route(["--version"]) == main.LauncherDirect
  assert main.launcher_route(["doctor", "--list-checks"]) == main.LauncherDirect
  assert main.launcher_route(["workflow", "run", "workflow.yml"])
    == main.LauncherDirect
  assert main.launcher_route(["tracker-conformance", "run", "manifest.json"])
    == main.LauncherDirect
  assert main.launcher_route(["ctl", "--help"]) == main.LauncherDirect
  assert main.launcher_route(["connect", "--help"]) == main.LauncherDirect
  assert main.launcher_route(["--once", ".scherzo/scherzo.yaml"])
    == main.LauncherDirect
  assert main.launcher_route(["--linear-smoke"]) == main.LauncherDirect
  assert main.launcher_route(["one.yaml", "two.yaml"]) == main.LauncherDirect
}

pub fn usage_mentions_required_operational_constraints_test() {
  let usage = main.usage()
  assert string.contains(usage, "scherzo [mode] [path-to-scherzo.yaml]")
  assert string.contains(usage, "LINEAR_API_KEY")
  assert string.contains(usage, "YAML orchestrator config")
  assert string.contains(usage, "workspace profiles with drivers")
  assert string.contains(usage, "agents.runtime.type: pi")
  assert string.contains(usage, "agents.concurrency: 0")
  assert string.contains(usage, "scherzo --version")
  assert string.contains(usage, "doctor [options]")
  assert string.contains(usage, "workflow run <workflow.yml>")
  assert string.contains(
    usage,
    "tracker-conformance run <manifest.json> --report <report.json>",
  )
  assert string.contains(usage, "doctor --check <name>")
  assert string.contains(usage, "doctor --list-checks")
  assert string.contains(usage, "doctor --logfmt")
  assert string.contains(
    usage,
    "tracker-conformance run Run the black-box tracker adapter conformance MVP",
  )
  assert string.contains(
    usage,
    "workflow-config, tracker-contract, tracker-smoke",
  )
  assert string.contains(usage, "--once")
  assert string.contains(usage, "--tracker-smoke")
  assert !string.contains(usage, "--linear-smoke")
  assert string.contains(usage, "--tracker-contract-check")
  assert !string.contains(usage, "--linear-contract-check")
  assert string.contains(usage, "--linear-attach-comment-file")
  assert string.contains(usage, "<comment-id> <file.md>")
  assert string.contains(usage, "mutates Linear")
  assert string.contains(usage, "--pi-probe")
  assert string.contains(usage, "--version")
  assert string.contains(usage, "source/build identity")
  assert string.contains(usage, "ctl ps")
  assert string.contains(usage, "ctl attach --raw")
  assert string.contains(usage, "daemon mode")
  assert string.contains(usage, "SIGTERM gracefully")
  assert string.contains(usage, "Ctrl-C/SIGINT")
  assert string.contains(usage, "packaged scherzo launcher")
  assert string.contains(usage, "compatibility scherzo-start helper")
  assert string.contains(
    usage,
    "Erlang signal FFI installs only the SIGTERM handler",
  )
  assert string.contains(usage, "kill -9")
  assert string.contains(usage, "only one Scherzo instance")
}
