import scherzo/doctor

pub fn default_checks_are_stable_test() {
  assert doctor.default_checks()
    == [
      doctor.WorkflowConfig,
      doctor.LinearContract,
      doctor.LinearSmoke,
      doctor.InstanceLock,
      doctor.WorkspaceHooks,
      doctor.PiProbe,
    ]
  assert doctor.list_check_names()
    == [
      "workflow-config",
      "linear-contract",
      "linear-smoke",
      "instance-lock",
      "workspace-hooks",
      "pi-probe",
    ]
}

pub fn parse_check_name_accepts_known_names_test() {
  assert doctor.parse_check_name("workflow-config") == Ok(doctor.WorkflowConfig)
  assert doctor.parse_check_name("linear-contract") == Ok(doctor.LinearContract)
  assert doctor.parse_check_name("linear-smoke") == Ok(doctor.LinearSmoke)
  assert doctor.parse_check_name("instance-lock") == Ok(doctor.InstanceLock)
  assert doctor.parse_check_name("workspace-hooks") == Ok(doctor.WorkspaceHooks)
  assert doctor.parse_check_name("pi-probe") == Ok(doctor.PiProbe)
}

pub fn parse_check_name_rejects_unknown_names_test() {
  assert doctor.parse_check_name("workflow_config") == Error("workflow_config")
  assert doctor.parse_check_name("no-such-check") == Error("no-such-check")
}

pub fn selected_checks_deduplicates_in_first_seen_order_test() {
  assert doctor.selected_checks([
      "pi-probe",
      "linear-smoke",
      "pi-probe",
      "workflow-config",
    ])
    == Ok([
      doctor.PiProbe,
      doctor.LinearSmoke,
      doctor.WorkflowConfig,
    ])
  assert doctor.canonical_checks([
      doctor.PiProbe,
      doctor.LinearSmoke,
      doctor.WorkflowConfig,
    ])
    == [doctor.WorkflowConfig, doctor.LinearSmoke, doctor.PiProbe]
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
      #("check", "linear-smoke"),
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
