import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/ctl/command_spec
import scherzo/ctl/outbox_command_spec
import scherzo/ctl/task_output
import scherzo/ctl/timeout_settings
import scherzo/terminal/style

pub type HandlerKey {
  PingKey
  PsKey
  QueryStatusKey
  QueryMetricsKey
  QueryOperationStatusKey
  TaskListKey
  TaskShowKey
  OutboxKey
  SessionKey
  EventsKey
  AttachKey
  PauseKey
  ResumeKey
  ReloadKey
  RetryKey
  RetryStepKey
  RecollectOutputsKey
  RunFinalizeKey
  RecoveryCleanupOrphanStepsKey
  ParkKey
  UnparkKey
  AbortKey
  StopAfterTurnKey
  PromptKey
  UiRespondKey
  CleanupKey
  SchedulesStatusKey
  SchedulesHistoryKey
  SchedulesLogsKey
  SchedulesDoctorKey
  SchedulesRunKey
  SchedulesReenableKey
  WorkstreamKey
  ArtifactPublicationListKey
  ArtifactPublicationShowKey
  PublicationRetryKey
  ArtifactPublicationRetryKey
  ArtifactPublicationAbandonKey
  StateStatusKey
  StateArchiveOldKey
  StateDiscardOldKey
  StateReinitializeKey
  StateCompactKey
  StateRepairRunProvenanceKey
}

pub fn parse(
  args: List(String),
) -> Result(command_spec.ParseOutcome(HandlerKey), command_spec.ParseError) {
  parse_control(args)
}

pub fn parse_control(
  args: List(String),
) -> Result(command_spec.ParseOutcome(HandlerKey), command_spec.ParseError) {
  command_spec.parse(args, commands())
}

pub fn parse_offline(
  args: List(String),
) -> Result(command_spec.ParseOutcome(HandlerKey), command_spec.ParseError) {
  command_spec.parse(args, offline_commands())
}

pub fn commands() -> List(command_spec.CommandSpec(HandlerKey)) {
  [
    command_spec.CommandSpec(
      handler: PingKey,
      path: ["ping"],
      usage: "ping",
      summary: "Check that the daemon control API is reachable.",
      positionals: [],
      options: [control_file_option(), json_option(), timeout_option()],
      help_lines: [
        line("ping", "Check that the daemon control API is reachable."),
      ],
    ),
    command_spec.CommandSpec(
      handler: PsKey,
      path: ["ps"],
      usage: "ps",
      summary: "List sessions (LAST EVENT is daemon-relative age; long session names are shortened).",
      positionals: [],
      options: [control_file_option(), json_option(), timeout_option()],
      help_lines: [
        line(
          "ps",
          "List sessions (LAST EVENT is daemon-relative age; long session names are shortened).",
        ),
      ],
    ),
    command_spec.CommandSpec(
      handler: QueryStatusKey,
      path: ["query", "status"],
      usage: "query status",
      summary: "Run the additive read-query status/introspection surface.",
      positionals: [],
      options: [control_file_option(), json_option(), timeout_option()],
      help_lines: [
        line(
          "query status",
          "Run the additive read-query status/introspection surface.",
        ),
      ],
    ),
    command_spec.CommandSpec(
      handler: QueryMetricsKey,
      path: ["query", "metrics"],
      usage: "query metrics",
      summary: "Show daemon operational health and runtime counters.",
      positionals: [],
      options: [control_file_option(), json_option(), timeout_option()],
      help_lines: [
        line(
          "query metrics",
          "Show daemon operational health and runtime counters.",
        ),
      ],
    ),
    command_spec.CommandSpec(
      handler: QueryOperationStatusKey,
      path: ["query", "operation-status"],
      usage: "query operation-status <operation-id>",
      summary: "Show one durable queued control operation status.",
      positionals: [command_spec.Required("operation_id")],
      options: [
        control_file_option(),
        json_option(),
        timeout_option(),
        wait_option(),
      ],
      help_lines: [
        line(
          "query operation-status <operation-id>",
          "Show one durable queued control operation status.",
        ),
      ],
    ),
    command_spec.CommandSpec(
      handler: TaskListKey,
      path: ["task", "list"],
      usage: "task list [--state <state>] [--limit <n>] [--cursor <cursor>]",
      summary: "List tracker tasks through the daemon query surface.",
      positionals: [],
      options: [
        control_file_option(),
        json_option(),
        state_option(),
        limit_option(),
        cursor_option(),
      ],
      help_lines: [
        line(
          "task list",
          "List tracker tasks through the daemon query surface.",
        ),
      ],
    ),
    command_spec.CommandSpec(
      handler: TaskShowKey,
      path: ["task", "show"],
      usage: "task show <task|id:<id>>",
      summary: "Show one tracker task through the daemon query surface.",
      positionals: [command_spec.Required("task_ref")],
      options: [control_file_option(), json_option()],
      help_lines: [
        line(
          "task show <task|id:<id>>",
          "Show one tracker task through the daemon query surface.",
        ),
      ],
    ),
    command_spec.CommandSpec(
      handler: RetryKey,
      path: ["task", "retry"],
      usage: "task retry <task|id:<id>> [--start-fresh --reason <text>]",
      summary: "Retry a tracker task now, or start a fresh run when explicitly requested.",
      positionals: [command_spec.Required("task_ref")],
      options: [
        control_file_option(),
        json_option(),
        timeout_option(),
        wait_option(),
        start_fresh_option(),
        reason_option(),
      ],
      help_lines: [
        line(
          "task retry <task|id:<id>> [--start-fresh --reason <text>]",
          "Retry a tracker task now, or start a fresh run when explicitly requested.",
        ),
      ],
    ),
    outbox_command_spec.command(
      OutboxKey,
      control_file_option(),
      json_option(),
      limit_option(),
      cursor_option(),
    ),
    command_spec.CommandSpec(
      handler: SessionKey,
      path: ["session"],
      usage: "session <session-ref>",
      summary: "Show one session summary.",
      positionals: [command_spec.Required("session_ref")],
      options: [control_file_option(), json_option(), timeout_option()],
      help_lines: [line("session <session-ref>", "Show one session summary.")],
    ),
    command_spec.CommandSpec(
      handler: EventsKey,
      path: ["events"],
      usage: "events <session-ref>",
      summary: "Replay recent compact event lines.",
      positionals: [command_spec.Required("session_ref")],
      options: [
        control_file_option(),
        raw_option(),
        pretty_option(),
        json_option(),
        color_option(),
        since_cursor_option(),
        verbose_option(),
      ],
      help_lines: [
        line("events <session-ref>", "Replay recent compact event lines."),
        line(
          "events --pretty <session-ref>",
          "Replay retained events with human-readable rendering.",
        ),
        line(
          "events --pretty --verbose <session-ref>",
          "Include pi cycle and raw diagnostic lines in pretty replay.",
        ),
      ],
    ),
    command_spec.CommandSpec(
      handler: AttachKey,
      path: ["attach"],
      usage: "attach [--raw|--json|--pretty] <session-ref>",
      summary: "Replay retained events and follow with human-readable rendering.",
      positionals: [command_spec.Required("session_ref")],
      options: [
        control_file_option(),
        raw_option(),
        pretty_option(),
        json_option(),
        color_option(),
        no_follow_option(),
        since_cursor_option(),
        verbose_option(),
      ],
      help_lines: [
        line(
          "attach <session-ref>",
          "Replay retained events and follow with human-readable rendering.",
        ),
        line(
          "attach --verbose <session-ref>",
          "Include pi cycle and raw diagnostic lines in pretty attach.",
        ),
        line(
          "attach --raw <session-ref>",
          "Replay and follow compact event lines.",
        ),
        line(
          "attach --json <session-ref>",
          "Replay and follow JSON stream event envelopes.",
        ),
      ],
    ),
    command_spec.CommandSpec(
      handler: PauseKey,
      path: ["pause"],
      usage: "pause",
      summary: "Pause new dispatch.",
      positionals: [],
      options: [
        control_file_option(),
        json_option(),
        timeout_option(),
        wait_option(),
      ],
      help_lines: [line("pause", "Pause new dispatch.")],
    ),
    command_spec.CommandSpec(
      handler: ResumeKey,
      path: ["resume"],
      usage: "resume",
      summary: "Resume new dispatch.",
      positionals: [],
      options: [
        control_file_option(),
        json_option(),
        timeout_option(),
        wait_option(),
      ],
      help_lines: [line("resume", "Resume new dispatch.")],
    ),
    command_spec.CommandSpec(
      handler: ReloadKey,
      path: ["reload"],
      usage: "reload",
      summary: "Reload the workflow now.",
      positionals: [],
      options: [
        control_file_option(),
        json_option(),
        timeout_option(),
        wait_option(),
      ],
      help_lines: [line("reload", "Reload the workflow now.")],
    ),
    command_spec.CommandSpec(
      handler: RetryKey,
      path: ["retry"],
      usage: "retry <task|id:<id>> [--start-fresh --reason <text>]",
      summary: "Retry a task now.",
      positionals: [command_spec.Required("task_ref")],
      options: [
        control_file_option(),
        json_option(),
        timeout_option(),
        wait_option(),
        start_fresh_option(),
        reason_option(),
      ],
      help_lines: [
        line(
          "retry <task|id:<id>> [--start-fresh --reason <text>]",
          "Retry a task now.",
        ),
      ],
    ),
    command_spec.CommandSpec(
      handler: RetryStepKey,
      path: ["run", "retry-step"],
      usage: "run retry-step <run-id> --step <step-id>",
      summary: "Retry one failed or interrupted workflow step without redispatching the whole task.",
      positionals: [command_spec.Required("run_id")],
      options: [
        control_file_option(),
        json_option(),
        timeout_option(),
        wait_option(),
        step_option(),
      ],
      help_lines: [
        line("run retry-step <run-id> --step <step-id>", ""),
        line(
          "",
          "Queue durable retry-step work without redispatching the whole task.",
        ),
        line(
          "",
          "Successful acknowledgement returns queued plus an operation_id; poll query operation-status for completion.",
        ),
      ],
    ),
    command_spec.CommandSpec(
      handler: RetryStepKey,
      path: ["retry-step"],
      usage: "retry-step <target> [--step <step-id>]",
      summary: "Retry a failed or interrupted workflow step without redispatching the whole task.",
      positionals: [command_spec.Required("target")],
      options: [
        control_file_option(),
        json_option(),
        timeout_option(),
        wait_option(),
        step_option(),
      ],
      help_lines: [
        line("retry-step <target> [--step <step-id>]", ""),
        line(
          "",
          "Queue durable retry-step work without redispatching the whole task.",
        ),
        line(
          "",
          "Successful acknowledgement returns queued plus an operation_id; poll query operation-status for completion.",
        ),
      ],
    ),
    command_spec.CommandSpec(
      handler: RecollectOutputsKey,
      path: ["run", "recollect-outputs"],
      usage: "run recollect-outputs <run-id>",
      summary: "Recollect workflow contract outputs without rerunning completed steps.",
      positionals: [command_spec.Required("run_id")],
      options: [
        control_file_option(),
        json_option(),
        timeout_option(),
        wait_option(),
      ],
      help_lines: [
        line("run recollect-outputs <run-id>", ""),
        line(
          "",
          "Recollect workflow contract outputs without rerunning completed steps. Successful acknowledgement returns queued plus an operation_id; poll query operation-status for completion.",
        ),
      ],
    ),
    command_spec.CommandSpec(
      handler: RecollectOutputsKey,
      path: ["recollect-outputs"],
      usage: "recollect-outputs run:<run-id>",
      summary: "Recollect workflow contract outputs without rerunning completed steps.",
      positionals: [command_spec.Required("run_ref")],
      options: [
        control_file_option(),
        json_option(),
        timeout_option(),
        wait_option(),
      ],
      help_lines: [
        line("recollect-outputs run:<run-id>", ""),
        line(
          "",
          "Recollect workflow contract outputs without rerunning completed steps. Successful acknowledgement returns queued plus an operation_id; poll query operation-status for completion.",
        ),
      ],
    ),
    command_spec.CommandSpec(
      handler: RunFinalizeKey,
      path: ["run", "finalize"],
      usage: "run finalize <run-id> --validate --outputs auto --publish --update-tracker --reason <text> (--dry-run|--yes)",
      summary: "Plan or perform manual run finalization without starting a new workflow.",
      positionals: [command_spec.Required("run_id")],
      options: [
        control_file_option(),
        json_option(),
        timeout_option(),
        wait_option(),
        validate_option(),
        outputs_option(),
        publish_option(),
        update_tracker_option(),
        reason_option(),
        dry_run_option(),
        yes_option(),
      ],
      help_lines: [
        line(
          "run finalize <run-id> --validate --outputs auto --publish --update-tracker --reason <text> (--dry-run|--yes)",
          "Plan or perform manual run finalization without starting a new workflow.",
        ),
      ],
    ),
    command_spec.CommandSpec(
      handler: RecoveryCleanupOrphanStepsKey,
      path: ["recovery", "cleanup-orphan-steps"],
      usage: "recovery cleanup-orphan-steps run:<run-id> [--dry-run|--yes]",
      summary: "Dry run orphaned YAML child-step cleanup by default; use --yes to mutate.",
      positionals: [command_spec.Required("run_ref")],
      options: [
        control_file_option(),
        json_option(),
        timeout_option(),
        wait_option(),
        dry_run_option(),
        yes_option(),
      ],
      help_lines: [
        line("recovery cleanup-orphan-steps run:<run-id> [--dry-run|--yes]", ""),
        line(
          "",
          "Dry run orphaned YAML child-step cleanup by default; use --yes to mutate.",
        ),
      ],
    ),
    command_spec.CommandSpec(
      handler: ParkKey,
      path: ["park"],
      usage: "park <task> --reason <text> --yes",
      summary: "Park a task until explicitly unparked.",
      positionals: [command_spec.Required("task")],
      options: [
        control_file_option(),
        json_option(),
        timeout_option(),
        wait_option(),
        reason_option(),
        yes_option(),
      ],
      help_lines: [
        line("park <task> --reason <text> --yes", ""),
        line("", "Park a task until explicitly unparked."),
      ],
    ),
    command_spec.CommandSpec(
      handler: UnparkKey,
      path: ["unpark"],
      usage: "unpark <task>",
      summary: "Unpark a task.",
      positionals: [command_spec.Required("task")],
      options: [
        control_file_option(),
        json_option(),
        timeout_option(),
        wait_option(),
      ],
      help_lines: [line("unpark <task>", "Unpark a task.")],
    ),
    command_spec.CommandSpec(
      handler: AbortKey,
      path: ["abort"],
      usage: "abort <session-ref> --yes",
      summary: "Abort a running session.",
      positionals: [command_spec.Required("session_ref")],
      options: [
        control_file_option(),
        json_option(),
        timeout_option(),
        wait_option(),
        yes_option(),
      ],
      help_lines: [
        line("abort <session-ref> --yes", "Abort a running session."),
      ],
    ),
    command_spec.CommandSpec(
      handler: StopAfterTurnKey,
      path: ["stop-after-turn"],
      usage: "stop-after-turn <session-ref> --yes",
      summary: "Stop after the current turn.",
      positionals: [command_spec.Required("session_ref")],
      options: [
        control_file_option(),
        json_option(),
        timeout_option(),
        wait_option(),
        yes_option(),
      ],
      help_lines: [
        line("stop-after-turn <session-ref> --yes", ""),
        line("", "Stop after the current turn."),
      ],
    ),
    command_spec.CommandSpec(
      handler: PromptKey,
      path: ["prompt"],
      usage: "prompt <session-ref> <text>",
      summary: "Queue an operator prompt for a session.",
      positionals: [
        command_spec.Required("session_ref"),
        command_spec.Required("text"),
      ],
      options: [
        control_file_option(),
        json_option(),
        timeout_option(),
        wait_option(),
      ],
      help_lines: [
        line(
          "prompt <session-ref> <text>",
          "Queue an operator prompt for a session.",
        ),
      ],
    ),
    command_spec.CommandSpec(
      handler: UiRespondKey,
      path: ["ui", "respond"],
      usage: "ui respond <session-ref> <request-id> (--cancel | --value <text>)",
      summary: "Respond to an operator-managed UI request.",
      positionals: [
        command_spec.Required("session_ref"),
        command_spec.Required("request_id"),
      ],
      options: [
        control_file_option(),
        json_option(),
        timeout_option(),
        wait_option(),
        cancel_option(),
        value_option(),
      ],
      help_lines: [
        line(
          "ui respond <session-ref> <request-id> (--cancel | --value <text>)",
          "",
        ),
        line("", "Respond to an operator-managed UI request."),
      ],
    ),
    command_spec.CommandSpec(
      handler: CleanupKey,
      path: ["cleanup"],
      usage: "cleanup [--provider <provider>] [--yes] [--limit <n>] [--cursor <cursor>] [--max-runtime-ms <ms>]",
      summary: "Dry-run owned cleanup inventory.",
      positionals: [],
      options: [
        control_file_option(),
        root_option(),
        provider_option(),
        json_option(),
        dry_run_option(),
        yes_option(),
        limit_option(),
        cursor_option(),
        cleanup_max_runtime_option(),
      ],
      help_lines: [
        line(
          "cleanup [--provider <provider>]",
          "Dry-run owned cleanup inventory.",
        ),
        line(
          "cleanup --yes",
          "Apply eligible owned cleanup after safety checks.",
        ),
        line(
          "cleanup --limit 100 --max-runtime-ms 240000",
          "Request a bounded cleanup page and report resume metadata.",
        ),
      ],
    ),
    command_spec.CommandSpec(
      handler: SchedulesStatusKey,
      path: ["schedules", "status"],
      usage: "schedules status [job]",
      summary: "Inspect local scheduled job status/history summary.",
      positionals: [command_spec.Optional("job")],
      options: [control_file_option(), root_option(), json_option()],
      help_lines: [
        line(
          "schedules status [job]",
          "Inspect local scheduled job status/history summary.",
        ),
      ],
    ),
    command_spec.CommandSpec(
      handler: SchedulesHistoryKey,
      path: ["schedules", "history"],
      usage: "schedules history <job>",
      summary: "Inspect local scheduled job history summary.",
      positionals: [command_spec.Required("job")],
      options: [control_file_option(), root_option(), json_option()],
      help_lines: [
        line(
          "schedules history <job>",
          "Inspect local scheduled job history summary.",
        ),
      ],
    ),
    command_spec.CommandSpec(
      handler: SchedulesLogsKey,
      path: ["schedules", "logs"],
      usage: "schedules logs <job> --last",
      summary: "Replay the latest retained scheduled session logs.",
      positionals: [command_spec.Required("job")],
      options: [
        control_file_option(),
        root_option(),
        json_option(),
        last_option(),
        color_option(),
        verbose_option(),
      ],
      help_lines: [
        line(
          "schedules logs <job> --last",
          "Replay the latest retained scheduled session logs.",
        ),
      ],
    ),
    command_spec.CommandSpec(
      handler: SchedulesDoctorKey,
      path: ["schedules", "doctor"],
      usage: "schedules doctor <job>",
      summary: "Show local scheduled job diagnostics.",
      positionals: [command_spec.Required("job")],
      options: [control_file_option(), root_option(), json_option()],
      help_lines: [
        line("schedules doctor <job>", "Show local scheduled job diagnostics."),
      ],
    ),
    command_spec.CommandSpec(
      handler: SchedulesRunKey,
      path: ["run-schedule"],
      usage: "run-schedule <job> --now",
      summary: "Start a scheduled job immediately.",
      positionals: [command_spec.Required("job")],
      options: [control_file_option(), json_option(), now_option()],
      help_lines: [
        line("run-schedule <job> --now", "Start a scheduled job immediately."),
      ],
    ),
    command_spec.CommandSpec(
      handler: SchedulesRunKey,
      path: ["schedules", "run"],
      usage: "schedules run <job> --now",
      summary: "Start a scheduled job immediately.",
      positionals: [command_spec.Required("job")],
      options: [
        control_file_option(),
        json_option(),
        timeout_option(),
        wait_option(),
        now_option(),
      ],
      help_lines: [
        line("schedules run <job> --now", "Start a scheduled job immediately."),
      ],
    ),
    command_spec.CommandSpec(
      handler: SchedulesReenableKey,
      path: ["schedules", "re-enable"],
      usage: "schedules re-enable <job>",
      summary: "Clear schedule quarantine and resume future fires.",
      positionals: [command_spec.Required("job")],
      options: [control_file_option(), json_option(), timeout_option()],
      help_lines: [
        line(
          "schedules re-enable <job>",
          "Clear schedule quarantine and resume future fires.",
        ),
      ],
    ),
    command_spec.CommandSpec(
      handler: WorkstreamKey,
      path: ["workstream"],
      usage: "workstream",
      summary: ctl_workstream_summary(),
      positionals: [command_spec.Rest("args")],
      options: [control_file_option(), root_option(), json_option()],
      help_lines: [
        line(
          "workstream list [task]",
          "List local workstreams, optionally for a Linear/task ref.",
        ),
        line(
          "workstream show <ref>",
          "Inspect one workstream id or Linear/task ref.",
        ),
        line(
          "workstream start-from-handoff <workflow> <action> <ref> <sha256> [decision-id...]",
          "",
        ),
        line(
          "",
          "Create an input bundle and queue a phase from a retained handoff.",
        ),
        line(
          "workstream start-from-input-bundle <workflow> <action> <ref> <sha256> [decision-id...]",
          "",
        ),
        line(
          "",
          "Queue a phase from an already retained workstream input bundle.",
        ),
        line(
          "workstream decision <kind> <workstream-id> <action-id> <gate-id> <actor> <rationale> <name>:<ref>:<sha256>...",
          "",
        ),
        line(
          "",
          "Record approve/request-changes/reject/deviate gate decisions for exact snapshots.",
        ),
      ],
    ),
    command_spec.CommandSpec(
      handler: ArtifactPublicationListKey,
      path: ["artifact", "publication", "list"],
      usage: "artifact publication list --run <run-id> [--root <workspace-root>]",
      summary: "Inspect the latest local publication status for one workflow run.",
      positionals: [],
      options: [
        control_file_option(),
        root_option(),
        json_option(),
        run_option(),
      ],
      help_lines: [
        line(
          "artifact publication list --run <run-id> [--root <workspace-root>]",
          "",
        ),
        line(
          "",
          "Inspect the latest local publication status for one workflow run.",
        ),
      ],
    ),
    command_spec.CommandSpec(
      handler: PublicationRetryKey,
      path: ["publication", "retry"],
      usage: "publication retry <run-id> [--publication <publication-id>]",
      summary: "Retry failed publication through the daemon queue using already-materialized outputs.",
      positionals: [command_spec.Required("run_id")],
      options: [
        control_file_option(),
        json_option(),
        timeout_option(),
        wait_option(),
        publication_option(),
      ],
      help_lines: [
        line(
          "publication retry <run-id> [--publication <publication-id>]",
          "Retry failed publication through the daemon queue using already-materialized outputs.",
        ),
      ],
    ),
    command_spec.CommandSpec(
      handler: ArtifactPublicationShowKey,
      path: ["artifact", "publication", "show"],
      usage: "artifact publication show --run <run-id> --publication <publication-id> [--root <workspace-root>]",
      summary: "Inspect the full local publication attempt history for one publication.",
      positionals: [],
      options: [
        control_file_option(),
        root_option(),
        json_option(),
        run_option(),
        publication_option(),
      ],
      help_lines: [
        line(
          "artifact publication show --run <run-id> --publication <publication-id> [--root <workspace-root>]",
          "",
        ),
        line(
          "",
          "Inspect the full local publication attempt history for one publication.",
        ),
      ],
    ),
    command_spec.CommandSpec(
      handler: ArtifactPublicationRetryKey,
      path: ["artifact", "publication", "retry"],
      usage: "artifact publication retry --run <run-id> [--publication <publication-id>] [--root <workspace-root>]",
      summary: "Retry failed publication; same-repo commit_stack retries use the retained workflow workspace driver.",
      positionals: [],
      options: [
        control_file_option(),
        root_option(),
        json_option(),
        run_option(),
        publication_option(),
      ],
      help_lines: [
        line(
          "artifact publication retry --run <run-id> [--publication <publication-id>] [--root <workspace-root>]",
          "",
        ),
        line(
          "",
          "Retry failed publication; same-repo commit_stack retries use the retained workflow workspace driver.",
        ),
      ],
    ),
    command_spec.CommandSpec(
      handler: ArtifactPublicationAbandonKey,
      path: ["artifact", "publication", "abandon"],
      usage: "artifact publication abandon --run <run-id> --publication <publication-id> --reason <text> --yes [--root <workspace-root>]",
      summary: "Release required commit_stack publication workspace protection without marking it published.",
      positionals: [],
      options: [
        control_file_option(),
        root_option(),
        json_option(),
        run_option(),
        publication_option(),
        reason_option(),
        yes_option(),
      ],
      help_lines: [
        line(
          "artifact publication abandon --run <run-id> --publication <publication-id> --reason <text> --yes [--root <workspace-root>]",
          "",
        ),
        line(
          "",
          "Release required commit_stack publication workspace protection without marking it published.",
        ),
      ],
    ),
    command_spec.CommandSpec(
      handler: StateStatusKey,
      path: ["state", "status"],
      usage: "state status --root <workspace-root>",
      summary: "Inspect offline local state schema.",
      positionals: [],
      options: [root_option(), json_option()],
      help_lines: [
        line("state status --root <workspace-root>", ""),
        line("", "Inspect offline local state schema."),
      ],
    ),
    command_spec.CommandSpec(
      handler: StateArchiveOldKey,
      path: ["state", "archive-old"],
      usage: "state archive-old --root <workspace-root> --yes",
      summary: "Archive unsupported old local ledger state.",
      positionals: [],
      options: [root_option(), json_option(), yes_option()],
      help_lines: [
        line("state archive-old --root <workspace-root> --yes", ""),
        line("", "Archive unsupported old local ledger state."),
      ],
    ),
    command_spec.CommandSpec(
      handler: StateDiscardOldKey,
      path: ["state", "discard-old"],
      usage: "state discard-old --root <workspace-root> --yes",
      summary: "Irreversibly discard unsupported old local ledger state.",
      positionals: [],
      options: [root_option(), json_option(), yes_option()],
      help_lines: [
        line("state discard-old --root <workspace-root> --yes", ""),
        line("", "Irreversibly discard unsupported old local ledger state."),
      ],
    ),
    command_spec.CommandSpec(
      handler: StateReinitializeKey,
      path: ["state", "reinitialize"],
      usage: "state reinitialize --root <workspace-root> --yes",
      summary: "Create an empty current ledger layout.",
      positionals: [],
      options: [root_option(), json_option(), yes_option()],
      help_lines: [
        line("state reinitialize --root <workspace-root> --yes", ""),
        line("", "Create an empty current ledger layout."),
      ],
    ),
    command_spec.CommandSpec(
      handler: StateCompactKey,
      path: ["state", "compact"],
      usage: "state compact --root <workspace-root> --dry-run|--yes",
      summary: "Inspect or compact the durable local state ledger.",
      positionals: [],
      options: [root_option(), json_option(), dry_run_option(), yes_option()],
      help_lines: [
        line("state compact --root <workspace-root> --dry-run|--yes", ""),
        line("", "Inspect or compact the durable local state ledger."),
      ],
    ),
    command_spec.CommandSpec(
      handler: StateRepairRunProvenanceKey,
      path: ["state", "repair-run-provenance"],
      usage: "state repair-run-provenance run:<run-id> --root <workspace-root> --dry-run|--yes",
      summary: "Inspect or append an auditable workflow provenance repair.",
      positionals: [command_spec.Required("run_ref")],
      options: [root_option(), json_option(), dry_run_option(), yes_option()],
      help_lines: [
        line(
          "state repair-run-provenance run:<run-id> --root <workspace-root> --dry-run|--yes",
          "",
        ),
        line("", "Inspect or append an auditable workflow provenance repair."),
      ],
    ),
  ]
}

pub fn control_commands() -> List(command_spec.CommandSpec(HandlerKey)) {
  list.filter(commands(), is_canonical_control_command)
}

pub fn offline_commands() -> List(command_spec.CommandSpec(HandlerKey)) {
  list.filter(commands(), is_offline_command)
}

pub fn deprecated_control_alias_commands() -> List(
  command_spec.CommandSpec(HandlerKey),
) {
  list.filter(commands(), is_deprecated_control_alias)
}

pub fn deprecated_alias_hint(args: List(String)) -> Option(String) {
  case parse_control(args) {
    Ok(command_spec.Parsed(parsed)) ->
      deprecated_alias_hint_from_parsed(args, parsed)
    _ -> None
  }
}

pub fn command_help_lines() -> List(command_spec.HelpLine) {
  flatten_help_lines(control_commands())
}

pub fn control_usage_lines() -> List(String) {
  command_spec.render_help_lines(flatten_help_lines(control_commands()))
}

pub fn offline_usage_lines() -> List(String) {
  command_spec.render_help_lines(flatten_help_lines(offline_commands()))
}

pub fn usage_lines() -> List(String) {
  control_usage_lines()
}

pub fn control_option_help_lines() -> List(command_spec.HelpLine) {
  list.map(control_option_specs_in_help_order(), command_spec.option_help_line)
}

pub fn control_option_usage_lines() -> List(String) {
  command_spec.render_help_lines(control_option_help_lines())
}

pub fn offline_option_help_lines() -> List(command_spec.HelpLine) {
  list.map(offline_option_specs_in_help_order(), command_spec.option_help_line)
}

pub fn offline_option_usage_lines() -> List(String) {
  command_spec.render_help_lines(offline_option_help_lines())
}

pub fn option_help_lines() -> List(command_spec.HelpLine) {
  list.map(option_specs_in_help_order(), command_spec.option_help_line)
}

pub fn option_usage_lines() -> List(String) {
  command_spec.render_help_lines(option_help_lines())
}

fn flatten_help_lines(
  specs: List(command_spec.CommandSpec(HandlerKey)),
) -> List(command_spec.HelpLine) {
  case specs {
    [] -> []
    [spec, ..rest] -> list.append(spec.help_lines, flatten_help_lines(rest))
  }
}

fn is_canonical_control_command(
  spec: command_spec.CommandSpec(HandlerKey),
) -> Bool {
  let command_spec.CommandSpec(handler: handler, path: path, ..) = spec
  case is_offline_handler(handler) {
    True -> False
    False ->
      case handler, path {
        SchedulesRunKey, ["run-schedule"] -> True
        RetryKey, ["task", "retry"] -> True
        RetryKey, _ -> False
        RetryStepKey, ["run", "retry-step"] -> True
        RetryStepKey, _ -> False
        RecollectOutputsKey, ["run", "recollect-outputs"] -> True
        RecollectOutputsKey, _ -> False
        RunFinalizeKey, ["run", "finalize"] -> True
        PublicationRetryKey, ["publication", "retry"] -> True
        SchedulesRunKey, _ -> False
        _, _ -> True
      }
  }
}

fn is_offline_command(spec: command_spec.CommandSpec(HandlerKey)) -> Bool {
  let command_spec.CommandSpec(handler: handler, path: path, ..) = spec
  case is_offline_handler(handler) {
    True -> True
    False ->
      case handler, path {
        SchedulesRunKey, ["schedules", "run"] -> False
        _, _ -> False
      }
  }
}

fn is_deprecated_control_alias(
  spec: command_spec.CommandSpec(HandlerKey),
) -> Bool {
  let command_spec.CommandSpec(handler: handler, path: path, ..) = spec
  case is_offline_handler(handler) {
    True -> True
    False ->
      case handler, path {
        SchedulesRunKey, ["schedules", "run"] -> True
        RetryKey, ["retry"] -> True
        RetryStepKey, ["retry-step"] -> True
        RecollectOutputsKey, ["recollect-outputs"] -> True
        _, _ -> False
      }
  }
}

fn is_offline_handler(handler: HandlerKey) -> Bool {
  case handler {
    CleanupKey
    | SchedulesStatusKey
    | SchedulesHistoryKey
    | SchedulesLogsKey
    | SchedulesDoctorKey
    | WorkstreamKey
    | ArtifactPublicationListKey
    | ArtifactPublicationShowKey
    | ArtifactPublicationRetryKey
    | ArtifactPublicationAbandonKey
    | StateStatusKey
    | StateArchiveOldKey
    | StateDiscardOldKey
    | StateReinitializeKey
    | StateCompactKey
    | StateRepairRunProvenanceKey -> True
    _ -> False
  }
}

fn deprecated_alias_hint_from_parsed(
  args: List(String),
  parsed: command_spec.ParsedCommand(HandlerKey),
) -> Option(String) {
  case is_deprecated_control_alias_spec(parsed.handler, parsed.path) {
    False -> None
    True ->
      Some(
        "Deprecated: scherzo ctl "
        <> string.join(args, with: " ")
        <> deprecated_alias_tail(args, parsed),
      )
  }
}

fn is_deprecated_control_alias_spec(
  handler: HandlerKey,
  path: List(String),
) -> Bool {
  case is_offline_handler(handler) {
    True -> True
    False ->
      case handler, path {
        SchedulesRunKey, ["schedules", "run"] -> True
        RetryKey, ["retry"] -> True
        RetryStepKey, ["retry-step"] -> True
        RecollectOutputsKey, ["recollect-outputs"] -> True
        _, _ -> False
      }
  }
}

fn deprecated_alias_tail(
  args: List(String),
  parsed: command_spec.ParsedCommand(HandlerKey),
) -> String {
  case parsed.handler, parsed.path {
    SchedulesRunKey, ["schedules", "run"] ->
      " will be removed after one release; use scherzo ctl run-schedule <job> --now."
    RetryKey, ["retry"] ->
      " will be removed after one release; use scherzo ctl task retry <task|id:<id>> [--start-fresh --reason <text>]."
    RetryStepKey, ["retry-step"] ->
      " will be removed after one release; use scherzo ctl run retry-step <run-id> --step <step-id>."
    RecollectOutputsKey, ["recollect-outputs"] ->
      " will be removed after one release; use scherzo ctl run recollect-outputs <run-id>."
    _, _ ->
      " will be removed after one release; use scherzo "
      <> string.join(args, with: " ")
      <> "."
  }
}

fn control_option_specs_in_help_order() -> List(command_spec.OptionSpec) {
  [
    control_file_option(),
    raw_option(),
    pretty_option(),
    json_option(),
    color_option(),
    no_follow_option(),
    since_cursor_option(),
    verbose_option(),
    timeout_option(),
    wait_option(),
    now_option(),
    state_option(),
    outbox_command_spec.status_option(),
    outbox_command_spec.kind_option(),
    limit_option(),
    cursor_option(),
    yes_option(),
    dry_run_option(),
    reason_option(),
    start_fresh_option(),
    step_option(),
    validate_option(),
    outputs_option(),
    publish_option(),
    update_tracker_option(),
    cancel_option(),
    value_option(),
    help_option(),
  ]
}

fn offline_option_specs_in_help_order() -> List(command_spec.OptionSpec) {
  [
    root_option(),
    provider_option(),
    json_option(),
    color_option(),
    verbose_option(),
    last_option(),
    run_option(),
    publication_option(),
    yes_option(),
    dry_run_option(),
    reason_option(),
    help_option(),
  ]
}

fn option_specs_in_help_order() -> List(command_spec.OptionSpec) {
  list.append(
    [control_file_option()],
    offline_option_specs_in_help_order()
      |> list.append(control_option_specs_in_help_order()),
  )
}

fn line(left: String, right: String) -> command_spec.HelpLine {
  command_spec.HelpLine(left, right)
}

fn ctl_workstream_summary() -> String {
  "List or operate on local workstreams."
}

fn control_file_option() -> command_spec.OptionSpec {
  command_spec.value_option(
    "--control-file",
    "<path>",
    "Use an explicit control.json path; relative paths resolve from the caller working directory.",
    False,
    command_spec.passthrough_value,
  )
}

fn root_option() -> command_spec.OptionSpec {
  command_spec.value_option(
    "--root",
    "<workspace-root>",
    "Workspace root for cleanup or offline state commands; relative paths resolve from the caller working directory.",
    False,
    command_spec.passthrough_value,
  )
}

fn provider_option() -> command_spec.OptionSpec {
  command_spec.passthrough_value_option(
    "--provider",
    "<provider>",
    "Cleanup provider: all, local-state, workspaces. Diagnostic-only unavailable providers: artifact-store, task-store, provider-live, remote-provider-cache, browser.",
  )
}

fn cleanup_max_runtime_option() -> command_spec.OptionSpec {
  command_spec.value_option(
    "--max-runtime-ms",
    "<ms>",
    "Maximum cleanup runtime budget in milliseconds.",
    False,
    fn(value) {
      case int.parse(value) {
        Ok(limit) if limit > 0 -> Ok(value)
        Ok(_) | Error(_) ->
          Error("--max-runtime-ms requires a positive integer")
      }
    },
  )
}

fn raw_option() -> command_spec.OptionSpec {
  command_spec.flag_option("--raw", "Compact line output for attach/events.")
}

fn pretty_option() -> command_spec.OptionSpec {
  command_spec.flag_option(
    "--pretty",
    "Human-readable output for attach/events.",
  )
}

fn json_option() -> command_spec.OptionSpec {
  command_spec.flag_option(
    "--json",
    "Protocol JSON for non-streaming commands, including target context; attach prints one JSON stream object per event.",
  )
}

fn color_option() -> command_spec.OptionSpec {
  command_spec.value_option(
    "--color",
    "auto|always|never",
    "Color policy for pretty output.",
    False,
    validate_color_mode,
  )
}

fn no_follow_option() -> command_spec.OptionSpec {
  command_spec.flag_option(
    "--no-follow",
    "For attach, replay retained events without following live events.",
  )
}

fn since_cursor_option() -> command_spec.OptionSpec {
  command_spec.value_option(
    "--since-cursor",
    "<n>",
    "Replay events after cursor n.",
    False,
    validate_since_cursor,
  )
}

fn verbose_option() -> command_spec.OptionSpec {
  command_spec.flag_option(
    "--verbose",
    "Include pi lifecycle and raw diagnostics in pretty attach/events output.",
  )
}

fn timeout_option() -> command_spec.OptionSpec {
  command_spec.value_option(
    "--timeout",
    "<duration>",
    "Primary control timeout; accepts values like 500ms, 5s, and 2m.",
    False,
    timeout_settings.timeout_option_validator,
  )
}

fn wait_option() -> command_spec.OptionSpec {
  command_spec.flag_option(
    "--wait",
    "Wait for an accepted operation or operation-status query to finish.",
  )
}

fn now_option() -> command_spec.OptionSpec {
  command_spec.flag_option("--now", "Required for schedules run <job> --now.")
}

fn last_option() -> command_spec.OptionSpec {
  command_spec.flag_option(
    "--last",
    "Required for schedules logs <job> --last.",
  )
}

fn run_option() -> command_spec.OptionSpec {
  command_spec.value_option(
    "--run",
    "<run-id>",
    "Workflow run id for artifact publication inspection.",
    False,
    command_spec.passthrough_value,
  )
}

fn publication_option() -> command_spec.OptionSpec {
  command_spec.value_option(
    "--publication",
    "<publication>",
    "Publication id for artifact publication show/retry/abandon.",
    False,
    command_spec.passthrough_value,
  )
}

fn state_option() -> command_spec.OptionSpec {
  command_spec.value_option(
    "--state",
    "<state>",
    "Filter task list by canonical state; may be repeated; required by Linear-backed trackers.",
    True,
    validate_task_state,
  )
}

fn limit_option() -> command_spec.OptionSpec {
  command_spec.value_option(
    "--limit",
    "<n>",
    "Maximum items to return or process.",
    False,
    validate_limit,
  )
}

fn cursor_option() -> command_spec.OptionSpec {
  command_spec.value_option(
    "--cursor",
    "<cursor>",
    "Opaque cursor returned by a previous paged command.",
    False,
    validate_cursor,
  )
}

fn yes_option() -> command_spec.OptionSpec {
  command_spec.flag_option("--yes", "Confirm destructive commands.")
}

fn dry_run_option() -> command_spec.OptionSpec {
  command_spec.flag_option("--dry-run", "Force read-only cleanup inventory.")
}

fn reason_option() -> command_spec.OptionSpec {
  command_spec.value_option(
    "--reason",
    "<text>",
    "Reason for park, start-fresh retry, or manual finalization.",
    False,
    command_spec.passthrough_value,
  )
}

fn start_fresh_option() -> command_spec.OptionSpec {
  command_spec.flag_option(
    "--start-fresh",
    "Start a fresh run from the current task payload and workflow definition.",
  )
}

fn step_option() -> command_spec.OptionSpec {
  command_spec.value_option(
    "--step",
    "<step-id>",
    "Select a failed or interrupted workflow step for retry-step.",
    False,
    command_spec.passthrough_value,
  )
}

fn validate_option() -> command_spec.OptionSpec {
  command_spec.flag_option(
    "--validate",
    "Required for run finalize to adopt validation evidence before mutation.",
  )
}

fn outputs_option() -> command_spec.OptionSpec {
  command_spec.value_option(
    "--outputs",
    "auto",
    "Required for run finalize; only auto is supported.",
    False,
    fn(value) {
      case value {
        "auto" -> Ok(value)
        _ -> Error("--outputs must be auto")
      }
    },
  )
}

fn publish_option() -> command_spec.OptionSpec {
  command_spec.flag_option(
    "--publish",
    "Required for run finalize to retry publication after outputs are ready.",
  )
}

fn update_tracker_option() -> command_spec.OptionSpec {
  command_spec.flag_option(
    "--update-tracker",
    "Required for run finalize to update the tracker after publication succeeds.",
  )
}

fn cancel_option() -> command_spec.OptionSpec {
  command_spec.flag_option("--cancel", "Cancel a UI request response.")
}

fn value_option() -> command_spec.OptionSpec {
  command_spec.value_option(
    "--value",
    "<text>",
    "Value for a UI request response.",
    False,
    command_spec.passthrough_value,
  )
}

fn help_option() -> command_spec.OptionSpec {
  command_spec.flag_option("--help, -h", "Show this help.")
}

fn validate_color_mode(value: String) -> Result(String, String) {
  case style.parse_color_mode(value) {
    Ok(_) -> Ok(value)
    Error(_) -> Error("--color must be auto, always, or never")
  }
}

fn validate_since_cursor(value: String) -> Result(String, String) {
  case int.parse(value) {
    Ok(cursor) ->
      case cursor < 0 {
        True -> Error("--since-cursor requires a non-negative integer")
        False -> Ok(value)
      }
    Error(_) -> Error("--since-cursor requires a non-negative integer")
  }
}

fn validate_task_state(value: String) -> Result(String, String) {
  case task_output.state_category_from_string(value) {
    Ok(_) -> Ok(value)
    Error(_) ->
      Error(
        "--state must be backlog, ready, active, done, canceled, duplicate, or unknown",
      )
  }
}

fn validate_limit(value: String) -> Result(String, String) {
  case int.parse(value) {
    Ok(limit) ->
      case limit > 0 {
        True -> Ok(value)
        False -> Error("--limit requires a positive integer")
      }
    Error(_) -> Error("--limit requires a positive integer")
  }
}

fn validate_cursor(value: String) -> Result(String, String) {
  case string.trim(value) == "" {
    True -> Error("--cursor must not be empty")
    False -> Ok(value)
  }
}
