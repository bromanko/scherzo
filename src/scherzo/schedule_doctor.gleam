import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/config/types as config_types
import scherzo/runtime_bundle
import scherzo/scheduled_failure_reporter
import scherzo/template
import scherzo/workflow_dag

pub type Severity {
  Pass
  Warn
  Fail
  Skip
}

pub type Diagnostic {
  Diagnostic(
    name: String,
    severity: Severity,
    code: String,
    message: String,
    fields: List(#(String, String)),
  )
}

pub type Report {
  Report(job_id: Option(String), diagnostics: List(Diagnostic))
}

pub type IssueReference {
  IssueReference(
    job_id: String,
    workflow_id: String,
    step_id: String,
    variable: String,
  )
}

pub fn inspect_bundle(
  bundle: runtime_bundle.RuntimeBundle,
  job_id: Option(String),
) -> Report {
  let jobs = matching_jobs(bundle.orchestrator.scheduled_jobs, job_id)
  let diagnostics = case job_id, jobs {
    Some(id), [] -> [
      Diagnostic(
        name: "job_exists",
        severity: Fail,
        code: "schedule_job_missing",
        message: "scheduled job " <> id <> " is not configured",
        fields: [#("job_id", id)],
      ),
    ]
    None, [] -> [
      Diagnostic(
        name: "scheduled_jobs_configured",
        severity: Pass,
        code: "no_scheduled_jobs",
        message: "no scheduled jobs are configured",
        fields: [],
      ),
    ]
    _, _ -> list.flat_map(jobs, fn(job) { diagnostics_for_job(bundle, job) })
  }
  Report(job_id: job_id, diagnostics: diagnostics)
}

pub fn issue_references(
  bundle: runtime_bundle.RuntimeBundle,
  job: config_types.ScheduledJobConfig,
) -> List(IssueReference) {
  case dict.get(bundle.workflows, job.workflow) {
    Error(Nil) -> []
    Ok(dag) ->
      issue_references_in_steps(
        job,
        workflow_dag.id(dag),
        workflow_dag.steps(dag),
      )
  }
}

pub fn severity_to_string(severity: Severity) -> String {
  case severity {
    Pass -> "pass"
    Warn -> "warn"
    Fail -> "fail"
    Skip -> "skip"
  }
}

pub fn most_severe(diagnostics: List(Diagnostic)) -> Severity {
  case list.any(diagnostics, fn(diagnostic) { diagnostic.severity == Fail }) {
    True -> Fail
    False ->
      case
        list.any(diagnostics, fn(diagnostic) { diagnostic.severity == Warn })
      {
        True -> Warn
        False ->
          case
            list.any(diagnostics, fn(diagnostic) { diagnostic.severity == Pass })
          {
            True -> Pass
            False -> Skip
          }
      }
  }
}

pub fn failing_or_warning_diagnostics(
  diagnostics: List(Diagnostic),
) -> List(Diagnostic) {
  list.filter(diagnostics, fn(diagnostic) {
    diagnostic.severity == Fail || diagnostic.severity == Warn
  })
}

fn matching_jobs(
  jobs: List(config_types.ScheduledJobConfig),
  job_id: Option(String),
) -> List(config_types.ScheduledJobConfig) {
  case job_id {
    None -> jobs
    Some(id) -> list.filter(jobs, fn(job) { job.id == id })
  }
}

fn diagnostics_for_job(
  bundle: runtime_bundle.RuntimeBundle,
  job: config_types.ScheduledJobConfig,
) -> List(Diagnostic) {
  [
    job_exists_diagnostic(job),
    job_enabled_diagnostic(job),
    workflow_exists_diagnostic(bundle, job),
    interval_diagnostic(job),
    mvp_shape_diagnostic(job),
    failure_task_diagnostic(job),
    reserved_label_diagnostic(job),
    scheduled_template_diagnostic(bundle, job),
  ]
}

fn job_exists_diagnostic(job: config_types.ScheduledJobConfig) -> Diagnostic {
  Diagnostic(
    name: "job_exists",
    severity: Pass,
    code: "ok",
    message: "scheduled job is configured",
    fields: [#("job_id", job.id), #("workflow_id", job.workflow)],
  )
}

fn job_enabled_diagnostic(job: config_types.ScheduledJobConfig) -> Diagnostic {
  case job.enabled {
    True ->
      Diagnostic(
        name: "job_enabled",
        severity: Pass,
        code: "ok",
        message: "scheduled job is enabled",
        fields: [#("job_id", job.id)],
      )
    False ->
      Diagnostic(
        name: "job_enabled",
        severity: Warn,
        code: "schedule_job_disabled",
        message: "scheduled job is disabled and will not run automatically",
        fields: [#("job_id", job.id)],
      )
  }
}

fn workflow_exists_diagnostic(
  bundle: runtime_bundle.RuntimeBundle,
  job: config_types.ScheduledJobConfig,
) -> Diagnostic {
  case dict.has_key(bundle.workflows, job.workflow) {
    True ->
      Diagnostic(
        name: "workflow_exists",
        severity: Pass,
        code: "ok",
        message: "enabled workflow exists and loaded successfully",
        fields: [#("job_id", job.id), #("workflow_id", job.workflow)],
      )
    False ->
      Diagnostic(
        name: "workflow_exists",
        severity: Fail,
        code: "scheduled_workflow_missing",
        message: "scheduled job references a workflow that did not load",
        fields: [#("job_id", job.id), #("workflow_id", job.workflow)],
      )
  }
}

fn interval_diagnostic(job: config_types.ScheduledJobConfig) -> Diagnostic {
  case job.enabled, job.every_ms >= 1000 {
    True, True ->
      Diagnostic(
        name: "interval",
        severity: Pass,
        code: "ok",
        message: "fixed interval is valid for the MVP",
        fields: [
          #("job_id", job.id),
          #("every_ms", int.to_string(job.every_ms)),
        ],
      )
    True, False ->
      Diagnostic(
        name: "interval",
        severity: Fail,
        code: "invalid_scheduled_job_interval",
        message: "enabled scheduled jobs must have an interval of at least 1000ms",
        fields: [
          #("job_id", job.id),
          #("every_ms", int.to_string(job.every_ms)),
        ],
      )
    False, _ ->
      Diagnostic(
        name: "interval",
        severity: Skip,
        code: "schedule_job_disabled",
        message: "interval is not used while the job is disabled",
        fields: [
          #("job_id", job.id),
          #("every_ms", int.to_string(job.every_ms)),
        ],
      )
  }
}

fn mvp_shape_diagnostic(job: config_types.ScheduledJobConfig) -> Diagnostic {
  Diagnostic(
    name: "mvp_shape",
    severity: Pass,
    code: "ok",
    message: "unsupported schedule-level input/vars/payload fields, catch_up=true, and non-skip overlap modes were absent or rejected during config load",
    fields: [
      #("job_id", job.id),
      #("overlap", overlap_to_string(job.overlap)),
      #("catch_up", bool_to_string(job.catch_up)),
    ],
  )
}

fn failure_task_diagnostic(job: config_types.ScheduledJobConfig) -> Diagnostic {
  let task = job.on_failure.task
  case task.enabled, task.state {
    False, _ ->
      Diagnostic(
        name: "failure_task_config",
        severity: Skip,
        code: "failure_task_reporting_disabled",
        message: "failure task reporting is disabled; terminal failures remain local only",
        fields: [#("job_id", job.id)],
      )
    True, Some(state) ->
      Diagnostic(
        name: "failure_task_config",
        severity: Pass,
        code: "ok",
        message: "failure task reporting has a configured triage state and open_task_per_schedule dedupe",
        fields: [
          #("job_id", job.id),
          #("state", state),
          #("dedupe", dedupe_to_string(task.dedupe)),
          #("configured_labels", string.join(task.labels, with: ",")),
        ],
      )
    True, None ->
      Diagnostic(
        name: "failure_task_config",
        severity: Fail,
        code: "scheduled_task_state_missing",
        message: "failure task reporting is enabled but no triage state is configured",
        fields: [#("job_id", job.id)],
      )
  }
}

fn reserved_label_diagnostic(
  job: config_types.ScheduledJobConfig,
) -> Diagnostic {
  let task = job.on_failure.task
  case task.enabled {
    False ->
      Diagnostic(
        name: "linear_reserved_labels",
        severity: Skip,
        code: "failure_task_reporting_disabled",
        message: "reserved Linear dedupe labels are not needed while failure task reporting is disabled",
        fields: [#("job_id", job.id)],
      )
    True -> {
      let labels = scheduled_failure_reporter.reserved_labels(job.id)
      Diagnostic(
        name: "linear_reserved_labels",
        severity: Pass,
        code: "ok",
        message: "scheduled failure reporter will ensure reserved dedupe labels before creating or updating a Linear issue",
        fields: [
          #("job_id", job.id),
          #("dedupe_key", scheduled_failure_reporter.dedupe_key(job.id)),
          #("reserved_labels", string.join(labels, with: ",")),
        ],
      )
    }
  }
}

fn scheduled_template_diagnostic(
  bundle: runtime_bundle.RuntimeBundle,
  job: config_types.ScheduledJobConfig,
) -> Diagnostic {
  let refs = issue_references(bundle, job)
  case refs {
    [] ->
      Diagnostic(
        name: "scheduled_template_context",
        severity: Pass,
        code: "ok",
        message: "scheduled workflow templates do not reference issue.* variables",
        fields: [#("job_id", job.id), #("workflow_id", job.workflow)],
      )
    [first, ..] -> {
      let severity = case job.enabled {
        True -> Fail
        False -> Warn
      }
      let message = case job.enabled {
        True ->
          "enabled scheduled workflow references an issue.* variable and cannot run without a Linear issue context"
        False ->
          "disabled scheduled workflow references an issue.* variable; fix this before enabling the job"
      }
      Diagnostic(
        name: "scheduled_template_context",
        severity: severity,
        code: "scheduled_workflow_requires_issue_context",
        message: message,
        fields: [
          #("job_id", first.job_id),
          #("workflow_id", first.workflow_id),
          #("step_id", first.step_id),
          #("variable", first.variable),
          #("reference_count", int.to_string(list.length(refs))),
        ],
      )
    }
  }
}

fn issue_references_in_steps(
  job: config_types.ScheduledJobConfig,
  workflow_id: String,
  steps: List(workflow_dag.WorkflowStep),
) -> List(IssueReference) {
  list.flat_map(steps, fn(step) {
    issue_references_in_step(job, workflow_id, step)
  })
}

fn issue_references_in_step(
  job: config_types.ScheduledJobConfig,
  workflow_id: String,
  step: workflow_dag.WorkflowStep,
) -> List(IssueReference) {
  let source = case step.kind {
    workflow_dag.AgentStep(workflow_dag.PromptInline(prompt), _) -> prompt
    workflow_dag.AgentStep(workflow_dag.PromptResolvedFile(_, prompt), _) ->
      prompt
    workflow_dag.AgentStep(workflow_dag.PromptFile(path), _) -> path
    workflow_dag.CommandStep(run, _) -> run
  }
  template.referenced_variables(source)
  |> issue_variables
  |> list.map(fn(variable) {
    IssueReference(
      job_id: job.id,
      workflow_id: workflow_id,
      step_id: step.id,
      variable: variable,
    )
  })
}

fn issue_variables(variables: List(String)) -> List(String) {
  list.filter(variables, fn(variable) {
    variable == "issue" || string.starts_with(variable, "issue.")
  })
}

fn overlap_to_string(overlap: config_types.ScheduledOverlap) -> String {
  case overlap {
    config_types.SkipOverlap -> "skip"
  }
}

fn dedupe_to_string(dedupe: config_types.ScheduledFailureDedupe) -> String {
  case dedupe {
    config_types.OpenTaskPerSchedule -> "open_task_per_schedule"
  }
}

fn bool_to_string(value: Bool) -> String {
  case value {
    True -> "true"
    False -> "false"
  }
}
