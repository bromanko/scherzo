import gleam/option.{None}
import gleam/string
import scherzo/doctor

pub fn default_checks_are_stable_test() {
  assert doctor.default_checks()
    == [
      doctor.WorkflowConfig,
      doctor.LinearTaskScope,
      doctor.ScheduledJobs,
      doctor.LinearContract,
      doctor.LinearSmoke,
      doctor.InstanceLock,
      doctor.WorkspaceHooks,
      doctor.PiProbe,
    ]
  assert doctor.list_check_names()
    == [
      "workflow-config",
      "tracker-scope",
      "scheduled-jobs",
      "tracker-contract",
      "tracker-smoke",
      "instance-lock",
      "workspace-hooks",
      "pi-probe",
    ]
}

pub fn parse_check_name_accepts_known_names_test() {
  assert doctor.parse_check_name("workflow-config") == Ok(doctor.WorkflowConfig)
  assert doctor.parse_check_name("tracker-scope") == Ok(doctor.LinearTaskScope)
  assert doctor.parse_check_name("scheduled-jobs") == Ok(doctor.ScheduledJobs)
  assert doctor.parse_check_name("tracker-contract")
    == Ok(doctor.LinearContract)
  assert doctor.parse_check_name("tracker-smoke") == Ok(doctor.LinearSmoke)
  assert doctor.parse_check_name("instance-lock") == Ok(doctor.InstanceLock)
  assert doctor.parse_check_name("workspace-hooks") == Ok(doctor.WorkspaceHooks)
  assert doctor.parse_check_name("pi-probe") == Ok(doctor.PiProbe)
}

pub fn parse_check_name_rejects_unknown_names_test() {
  assert doctor.parse_check_name("workflow_config") == Error("workflow_config")
  assert doctor.parse_check_name("linear-contract") == Error("linear-contract")
  assert doctor.parse_check_name("linear-smoke") == Error("linear-smoke")
  assert doctor.parse_check_name("no-such-check") == Error("no-such-check")
}

pub fn selected_checks_deduplicates_in_first_seen_order_test() {
  assert doctor.selected_checks([
      "pi-probe",
      "tracker-smoke",
      "pi-probe",
      "workflow-config",
      "tracker-scope",
    ])
    == Ok([
      doctor.PiProbe,
      doctor.LinearSmoke,
      doctor.WorkflowConfig,
      doctor.LinearTaskScope,
    ])
  assert doctor.canonical_checks([
      doctor.PiProbe,
      doctor.LinearSmoke,
      doctor.WorkflowConfig,
      doctor.LinearTaskScope,
    ])
    == [
      doctor.WorkflowConfig,
      doctor.LinearTaskScope,
      doctor.LinearSmoke,
      doctor.PiProbe,
    ]
  assert doctor.selected_checks(["tracker-smoke", "tracker-smoke"])
    == Ok([doctor.LinearSmoke])
}

pub fn summary_counts_result_statuses_test() {
  let report =
    doctor.Report([
      doctor.CheckResult(
        check: doctor.WorkflowConfig,
        status: doctor.Pass,
        code: "ok",
        message: "loaded",
        fields: [],
      ),
      doctor.CheckResult(
        check: doctor.WorkspaceHooks,
        status: doctor.Warn,
        code: "workspace_cleanup_failed",
        message: "cleanup failed",
        fields: [],
      ),
      doctor.CheckResult(
        check: doctor.LinearSmoke,
        status: doctor.Fail,
        code: "linear_api_status",
        message: "Linear returned 500",
        fields: [],
      ),
      doctor.CheckResult(
        check: doctor.PiProbe,
        status: doctor.Skip,
        code: "instance_lock_failed",
        message: "lock unavailable",
        fields: [],
      ),
    ])
  assert doctor.summary(report) == doctor.Summary(1, 1, 1, 1)
  assert doctor.has_failures(report) == True
}

pub fn human_report_is_readable_test() {
  let report =
    doctor.Report([
      doctor.CheckResult(
        check: doctor.WorkflowConfig,
        status: doctor.Pass,
        code: "ok",
        message: "loaded",
        fields: [
          #("config_path", ".scherzo/scherzo.yaml"),
          #("workflow_count", "1"),
        ],
      ),
      doctor.CheckResult(
        check: doctor.LinearSmoke,
        status: doctor.Fail,
        code: "linear_api_status",
        message: "Linear returned an error",
        fields: [],
      ),
      doctor.CheckResult(
        check: doctor.PiProbe,
        status: doctor.Skip,
        code: "workflow_config_failed",
        message: "workflow config did not load",
        fields: [],
      ),
    ])
  let output = doctor.human_report(report, None)
  assert string.contains(output, "Scherzo doctor")
  assert string.contains(output, "Config: .scherzo/scherzo.yaml")
  assert string.contains(output, "✓ Workflow config")
  assert string.contains(output, "✗ Tracker smoke")
  assert string.contains(output, "Code: linear_api_status")
  assert string.contains(output, "- Pi probe")
  assert string.contains(
    output,
    "Summary: 1 passed, 0 warnings, 1 failed, 1 skipped",
  )
  assert string.contains(output, "Not ready.")
}

pub fn human_report_scheduled_jobs_remediation_uses_current_config_keys_test() {
  let report =
    doctor.Report([
      doctor.CheckResult(
        check: doctor.ScheduledJobs,
        status: doctor.Fail,
        code: "invalid_scheduled_job_interval",
        message: "bad schedule",
        fields: [],
      ),
    ])
  let output = doctor.human_report(report, None)
  assert string.contains(
    output,
    "Confirm schedules entries reference existing workflows",
  )
  assert string.contains(output, "schedules[].on_failure.task.enabled")
  assert !string.contains(output, "Confirm scheduled_jobs entries")
  assert !string.contains(output, "on_failure.linear")
}

pub fn human_report_tracker_remediation_uses_doctor_checks_test() {
  let report =
    doctor.Report([
      doctor.CheckResult(
        check: doctor.LinearContract,
        status: doctor.Fail,
        code: "linear_contract_state_missing",
        message: "missing state",
        fields: [],
      ),
      doctor.CheckResult(
        check: doctor.LinearSmoke,
        status: doctor.Fail,
        code: "linear_api_status",
        message: "tracker read failed",
        fields: [],
      ),
    ])
  let output = doctor.human_report(report, None)
  assert string.contains(
    output,
    "gleam run -- doctor --check tracker-contract <path-to-scherzo.yaml>",
  )
  assert string.contains(
    output,
    "gleam run -- doctor --check tracker-smoke <path-to-scherzo.yaml>",
  )
  assert !string.contains(output, "--tracker-contract-check")
  assert !string.contains(output, "--tracker-smoke")
  assert !string.contains(output, "tracker-contract-check")
}

pub fn result_events_and_log_fields_are_stable_test() {
  let result =
    doctor.CheckResult(
      check: doctor.LinearSmoke,
      status: doctor.Fail,
      code: "linear_api_status",
      message: "tracker error",
      fields: [#("candidate_count", "0")],
    )
  assert doctor.result_event(result) == "doctor_check_fail"
  assert doctor.result_log_fields(result)
    == [
      #("check", "tracker-smoke"),
      #("code", "linear_api_status"),
      #("message", "tracker error"),
      #("candidate_count", "0"),
    ]
  assert doctor.summary_log_fields(doctor.Summary(1, 2, 3, 4))
    == [
      #("passed", "1"),
      #("warned", "2"),
      #("failed", "3"),
      #("skipped", "4"),
    ]
}
