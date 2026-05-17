import gleam/io
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/json_value
import scherzo/path
import scherzo/structured_output_contract_policy
import scherzo/structured_output_source
import scherzo/workflow_dag
import simplifile

pub type CliError {
  CliError(code: String, message: String)
}

type Command {
  CheckSchema(schema: String, output_dir: Option(String))
  CheckWorkflow(workflow: String, output_dir: Option(String))
  CheckWorkflows(output_dir: Option(String))
}

type WorkflowSummary {
  WorkflowSummary(
    path: String,
    status: String,
    structured_outputs: Int,
    errors: List(ContractError),
  )
}

type StructuredOutputSummary {
  StructuredOutputSummary(
    workflow: String,
    step_id: String,
    artifact_name: String,
    tool_name: String,
    provider_schema_path: String,
    validator_paths: List(String),
    prompt_status: String,
    materialization_status: String,
    status: String,
  )
}

type ContractError {
  ContractError(
    workflow: String,
    step_id: String,
    code: String,
    message: String,
    path: String,
  )
}

type Report {
  Report(
    status: String,
    workflows: List(WorkflowSummary),
    structured_outputs: List(StructuredOutputSummary),
    errors: List(ContractError),
  )
}

pub fn main() -> Nil {
  case parse_args(args()) {
    Error(error) -> {
      io.println_error(error.message)
      halt(2)
    }
    Ok(command) ->
      case run(command) {
        Error(error) -> {
          io.println_error(error.code <> ": " <> error.message)
          halt(1)
        }
        Ok(report) -> {
          io.println("STRUCTURED_OUTPUT_CONTRACT=" <> report.status)
          case report.status == "passed" {
            True -> Nil
            False -> halt(1)
          }
        }
      }
  }
}

fn run(command: Command) -> Result(Report, CliError) {
  case command {
    CheckSchema(schema_path, output_dir) -> {
      use schema <- result.try(load_json_file(schema_path))
      use Nil <- result.try(validate_provider_schema(schema, schema_path))
      let report =
        Report(
          status: "passed",
          workflows: [],
          structured_outputs: [],
          errors: [],
        )
      use Nil <- result.try(write_report_if_requested(report, output_dir))
      Ok(report)
    }
    CheckWorkflow(workflow_path, output_dir) -> {
      use report <- result.try(check_single_workflow(workflow_path))
      use Nil <- result.try(write_report_if_requested(report, output_dir))
      Ok(report)
    }
    CheckWorkflows(output_dir) -> {
      use workflow_paths <- result.try(workflow_files(".scherzo/workflows"))
      use reports <- result.try(check_workflow_paths(workflow_paths, []))
      let report = combine_reports(reports)
      use Nil <- result.try(write_report_if_requested(report, output_dir))
      Ok(report)
    }
  }
}

fn parse_args(raw_args: List(String)) -> Result(Command, CliError) {
  let args = case raw_args {
    ["--", ..rest] -> rest
    _ -> raw_args
  }
  case args {
    ["check-schema", "--schema", schema] -> Ok(CheckSchema(schema, None))
    ["check-schema", "--schema", schema, "--output-dir", output_dir] ->
      Ok(CheckSchema(schema, Some(output_dir)))
    ["check-workflow", "--workflow", workflow] ->
      Ok(CheckWorkflow(workflow, None))
    ["check-workflow", "--workflow", workflow, "--output-dir", output_dir] ->
      Ok(CheckWorkflow(workflow, Some(output_dir)))
    ["check-workflows"] -> Ok(CheckWorkflows(None))
    ["check-workflows", "--output-dir", output_dir] ->
      Ok(CheckWorkflows(Some(output_dir)))
    _ -> usage_error()
  }
}

fn usage_error() -> Result(a, CliError) {
  Error(CliError(
    code: "usage",
    message: "Usage: gleam run -m scherzo_structured_output_contract -- check-schema --schema <path> [--output-dir <dir>]\n       gleam run -m scherzo_structured_output_contract -- check-workflow --workflow <path> [--output-dir <dir>]\n       gleam run -m scherzo_structured_output_contract -- check-workflows [--output-dir <dir>]",
  ))
}

fn check_workflow_paths(
  workflow_paths: List(String),
  reports: List(Report),
) -> Result(List(Report), CliError) {
  case workflow_paths {
    [] -> Ok(list.reverse(reports))
    [workflow_path, ..rest] -> {
      use report <- result.try(check_single_workflow(workflow_path))
      check_workflow_paths(rest, [report, ..reports])
    }
  }
}

fn check_single_workflow(workflow_path: String) -> Result(Report, CliError) {
  use contents <- result.try(read_text_file(workflow_path))
  use dag <- result.try(parse_workflow(contents, workflow_path))
  use checked <- result.try(
    check_steps(workflow_path, contents, dag.steps, [], []),
  )
  let #(summaries, errors) = checked
  let workflow_summary =
    WorkflowSummary(
      path: workflow_path,
      status: status_from_errors(errors),
      structured_outputs: list.length(summaries),
      errors: errors,
    )
  Ok(Report(
    status: status_from_errors(errors),
    workflows: [workflow_summary],
    structured_outputs: summaries,
    errors: errors,
  ))
}

fn check_steps(
  workflow_path: String,
  workflow_text: String,
  steps: List(workflow_dag.WorkflowStep),
  summaries: List(StructuredOutputSummary),
  errors: List(ContractError),
) -> Result(#(List(StructuredOutputSummary), List(ContractError)), CliError) {
  case steps {
    [] -> Ok(#(list.reverse(summaries), list.reverse(errors)))
    [step, ..rest] ->
      case step.kind {
        workflow_dag.AgentStep(prompt, Some(spec)) -> {
          let #(summary, step_errors) =
            check_structured_output_step(
              workflow_path,
              workflow_text,
              step.id,
              prompt,
              spec,
            )
          check_steps(
            workflow_path,
            workflow_text,
            rest,
            [summary, ..summaries],
            list.reverse(step_errors) |> list.append(errors),
          )
        }
        _ -> check_steps(workflow_path, workflow_text, rest, summaries, errors)
      }
  }
}

fn check_structured_output_step(
  workflow_path: String,
  workflow_text: String,
  step_id: String,
  prompt: workflow_dag.PromptRef,
  spec: workflow_dag.StructuredOutputSpec,
) -> #(StructuredOutputSummary, List(ContractError)) {
  let errors = source_errors(workflow_path, step_id, spec.source)
  let #(tool_name, schema_path) = source_summary_fields(spec.source)
  let validator_paths = json_schema_validator_paths(spec.validators)
  let #(prompt_status, prompt_errors) =
    prompt_status_and_errors(workflow_path, step_id, prompt, tool_name)
  let materialization_status =
    materialization_status(workflow_text, schema_path)
  let materialization_errors =
    materialization_errors_for_status(
      workflow_path,
      step_id,
      materialization_status,
      schema_path,
    )
  let schema_errors = schema_errors(workflow_path, step_id, schema_path)
  let all_errors =
    errors
    |> list.append(prompt_errors)
    |> list.append(materialization_errors)
    |> list.append(schema_errors)
  let summary =
    StructuredOutputSummary(
      workflow: workflow_path,
      step_id: step_id,
      artifact_name: spec.artifact_name,
      tool_name: tool_name,
      provider_schema_path: schema_path,
      validator_paths: validator_paths,
      prompt_status: prompt_status,
      materialization_status: materialization_status,
      status: status_from_errors(all_errors),
    )
  #(summary, all_errors)
}

fn source_summary_fields(
  source: structured_output_source.StructuredOutputSource,
) -> #(String, String) {
  case source {
    structured_output_source.PiToolCallSource(
      tool_name,
      _,
      _,
      Some(schema_path),
    ) -> #(tool_name, schema_path)
    structured_output_source.PiToolCallSource(tool_name, _, _, None) -> #(
      tool_name,
      "",
    )
    structured_output_source.FinalResponseSource -> #("", "")
  }
}

fn source_errors(
  workflow_path: String,
  step_id: String,
  source: structured_output_source.StructuredOutputSource,
) -> List(ContractError) {
  case structured_output_contract_policy.validate_source(source) {
    Ok(_) -> []
    Error(error) -> [contract_policy_error(workflow_path, step_id, error)]
  }
}

fn schema_errors(
  workflow_path: String,
  step_id: String,
  schema_path: String,
) -> List(ContractError) {
  case schema_path == "" {
    True -> []
    False ->
      case load_json_file(schema_path) {
        Error(error) -> [
          ContractError(
            workflow: workflow_path,
            step_id: step_id,
            code: error.code,
            message: error.message,
            path: schema_path,
          ),
        ]
        Ok(schema) ->
          case validate_provider_schema(schema, schema_path) {
            Ok(_) -> []
            Error(error) -> [
              ContractError(
                workflow: workflow_path,
                step_id: step_id,
                code: error.code,
                message: error.message,
                path: schema_path,
              ),
            ]
          }
      }
  }
}

fn validate_provider_schema(
  schema: json_value.JsonValue,
  schema_path: String,
) -> Result(Nil, CliError) {
  structured_output_contract_policy.validate_provider_schema(
    schema,
    schema_path,
  )
  |> result.map_error(fn(error) {
    case error {
      structured_output_contract_policy.ContractPolicyError(code, message) ->
        CliError(code, message)
    }
  })
}

fn contract_policy_error(
  workflow_path: String,
  step_id: String,
  error: structured_output_contract_policy.ContractPolicyError,
) -> ContractError {
  case error {
    structured_output_contract_policy.ContractPolicyError(code, message) ->
      ContractError(
        workflow: workflow_path,
        step_id: step_id,
        code: code,
        message: message,
        path: workflow_path,
      )
  }
}

fn prompt_status_and_errors(
  workflow_path: String,
  step_id: String,
  prompt: workflow_dag.PromptRef,
  tool_name: String,
) -> #(String, List(ContractError)) {
  case prompt_status(workflow_path, prompt, tool_name) {
    Ok(status) -> #(
      status,
      prompt_errors_for_status(workflow_path, step_id, status, tool_name),
    )
    Error(error) -> #("missing_prompt", [
      ContractError(
        workflow: workflow_path,
        step_id: step_id,
        code: "structured_output_prompt_read_failed",
        message: error.message,
        path: workflow_path,
      ),
    ])
  }
}

fn prompt_status(
  workflow_path: String,
  prompt: workflow_dag.PromptRef,
  tool_name: String,
) -> Result(String, CliError) {
  case prompt {
    workflow_dag.PromptInline(text) -> Ok(prompt_text_status(text, tool_name))
    workflow_dag.PromptFile(prompt_path) ->
      read_text_file(resolve_workflow_relative_path(workflow_path, prompt_path))
      |> result.map(fn(text) { prompt_text_status(text, tool_name) })
  }
}

fn prompt_text_status(prompt_text: String, tool_name: String) -> String {
  case tool_name == "" {
    True -> "missing_tool_name"
    False -> {
      let normalized_prompt = string.lowercase(prompt_text)
      let normalized_tool_name = string.lowercase(tool_name)
      case string.contains(normalized_prompt, normalized_tool_name) {
        False -> "tool_name_missing"
        True ->
          case prompt_requests_final_response_json(normalized_prompt) {
            True -> "final_response_json"
            False -> "passed"
          }
      }
    }
  }
}

fn prompt_requests_final_response_json(normalized_prompt: String) -> Bool {
  case
    string.contains(normalized_prompt, "final assistant json alone is invalid")
    || string.contains(normalized_prompt, "final assistant json is invalid")
    || string.contains(normalized_prompt, "final response json is invalid")
  {
    True -> False
    False ->
      string.contains(normalized_prompt, "json directly in your final response")
      || string.contains(normalized_prompt, "return json directly")
      || string.contains(normalized_prompt, "final response json")
      || string.contains(normalized_prompt, "final assistant json")
  }
}

fn prompt_errors_for_status(
  workflow_path: String,
  step_id: String,
  status: String,
  tool_name: String,
) -> List(ContractError) {
  case status {
    "passed" -> []
    "missing_prompt" -> [
      ContractError(
        workflow: workflow_path,
        step_id: step_id,
        code: "structured_output_prompt_read_failed",
        message: "could not read structured-output prompt for step " <> step_id,
        path: workflow_path,
      ),
    ]
    "tool_name_missing" -> [
      ContractError(
        workflow: workflow_path,
        step_id: step_id,
        code: "structured_output_prompt_tool_name_mismatch",
        message: "prompt does not mention structured-output tool `"
          <> tool_name
          <> "`",
        path: workflow_path,
      ),
    ]
    "final_response_json" -> [
      ContractError(
        workflow: workflow_path,
        step_id: step_id,
        code: "structured_output_prompt_requests_final_response_json",
        message: "prompt mentions structured-output tool `"
          <> tool_name
          <> "` but instructs final assistant JSON instead of tool arguments",
        path: workflow_path,
      ),
    ]
    _ -> [
      ContractError(
        workflow: workflow_path,
        step_id: step_id,
        code: "structured_output_prompt_invalid",
        message: "invalid prompt guidance",
        path: workflow_path,
      ),
    ]
  }
}

fn materialization_status(
  workflow_text: String,
  schema_path: String,
) -> String {
  case lane_id_from_schema_path(schema_path) {
    None -> "not_required"
    Some(lane_id) ->
      case
        string.contains(
          workflow_text,
          "materialize_" <> string.replace(lane_id, "-", "_"),
        )
        && string.contains(workflow_text, "review-lane-draft.v1.json")
        && string.contains(workflow_text, "artifacts/review/lanes/" <> lane_id)
      {
        True -> "passed"
        False -> "missing_materialization"
      }
  }
}

fn lane_id_from_schema_path(schema_path: String) -> Option(String) {
  case string.contains(schema_path, "review-lane-draft.correctness") {
    True -> Some("correctness")
    False ->
      case string.contains(schema_path, "review-lane-draft.test-quality") {
        True -> Some("test-quality")
        False ->
          case
            string.contains(
              schema_path,
              "review-lane-draft.idioms-maintainability",
            )
          {
            True -> Some("idioms-maintainability")
            False ->
              case
                string.contains(
                  schema_path,
                  "review-lane-draft.security-performance",
                )
              {
                True -> Some("security-performance")
                False -> None
              }
          }
      }
  }
}

fn materialization_errors_for_status(
  workflow_path: String,
  step_id: String,
  status: String,
  schema_path: String,
) -> List(ContractError) {
  case status {
    "passed" | "not_required" -> []
    _ -> [
      ContractError(
        workflow: workflow_path,
        step_id: step_id,
        code: "structured_output_missing_materialization",
        message: "workflow is missing review-lane materialization for provider schema "
          <> schema_path,
        path: workflow_path,
      ),
    ]
  }
}

fn json_schema_validator_paths(
  validators: List(workflow_dag.StructuredOutputValidator),
) -> List(String) {
  validators
  |> list.fold([], fn(paths, validator) {
    case validator {
      workflow_dag.JsonSchemaValidator(path: path, ..) -> [path, ..paths]
      _ -> paths
    }
  })
  |> list.reverse
}

fn combine_reports(reports: List(Report)) -> Report {
  Report(
    status: status_from_errors(all_errors(reports)),
    workflows: reports |> list.flat_map(fn(report) { report.workflows }),
    structured_outputs: reports
      |> list.flat_map(fn(report) { report.structured_outputs }),
    errors: all_errors(reports),
  )
}

fn all_errors(reports: List(Report)) -> List(ContractError) {
  reports |> list.flat_map(fn(report) { report.errors })
}

fn status_from_errors(errors: List(ContractError)) -> String {
  case errors {
    [] -> "passed"
    _ -> "failed"
  }
}

fn workflow_files(root: String) -> Result(List(String), CliError) {
  use entries <- result.try(read_directory(root))
  workflow_files_loop(root, entries |> list.sort(by: string.compare), [])
}

fn workflow_files_loop(
  root: String,
  entries: List(String),
  paths: List(String),
) -> Result(List(String), CliError) {
  case entries {
    [] -> Ok(paths)
    [entry, ..rest] -> {
      use entry_paths <- result.try(workflow_paths_for_entry(
        root <> "/" <> entry,
      ))
      workflow_files_loop(root, rest, list.append(paths, entry_paths))
    }
  }
}

fn workflow_paths_for_entry(
  path_text: String,
) -> Result(List(String), CliError) {
  use directory <- result.try(is_directory(path_text))
  case directory {
    True -> workflow_files(path_text)
    False ->
      case
        string.ends_with(path_text, ".yaml")
        || string.ends_with(path_text, ".yml")
      {
        True -> Ok([path_text])
        False -> Ok([])
      }
  }
}

fn is_directory(path_text: String) -> Result(Bool, CliError) {
  simplifile.is_directory(path_text)
  |> result.map_error(fn(error) {
    CliError(
      code: "stat_failed",
      message: "could not stat `"
        <> path_text
        <> "`: "
        <> simplifile.describe_error(error),
    )
  })
}

fn resolve_workflow_relative_path(
  workflow_path: String,
  value: String,
) -> String {
  case
    string.starts_with(value, "/")
    || string.starts_with(value, "./")
    || string.starts_with(value, "../")
  {
    True -> value
    False ->
      case path.dirname(workflow_path) {
        Ok(dir) -> path.join(dir, value)
        Error(Nil) -> value
      }
  }
}

fn parse_workflow(
  contents: String,
  workflow_path: String,
) -> Result(workflow_dag.WorkflowDag, CliError) {
  workflow_dag.parse(contents)
  |> result.map_error(fn(error) {
    case error {
      workflow_dag.DagError(code, message) ->
        CliError(code, workflow_path <> ": " <> message)
    }
  })
}

fn read_text_file(path_text: String) -> Result(String, CliError) {
  simplifile.read(path_text)
  |> result.map_error(fn(error) {
    CliError(
      code: "read_failed",
      message: "could not read `"
        <> path_text
        <> "`: "
        <> simplifile.describe_error(error),
    )
  })
}

fn load_json_file(path_text: String) -> Result(json_value.JsonValue, CliError) {
  use contents <- result.try(read_text_file(path_text))
  case json_value.parse(contents) {
    Ok(value) -> Ok(value)
    Error(Nil) ->
      Error(CliError(
        code: "invalid_json",
        message: "invalid JSON: " <> path_text,
      ))
  }
}

fn read_directory(path_text: String) -> Result(List(String), CliError) {
  simplifile.read_directory(path_text)
  |> result.map_error(fn(error) {
    CliError(
      code: "read_directory_failed",
      message: "could not read directory `"
        <> path_text
        <> "`: "
        <> simplifile.describe_error(error),
    )
  })
}

fn write_report_if_requested(
  report: Report,
  output_dir: Option(String),
) -> Result(Nil, CliError) {
  case output_dir {
    None -> Ok(Nil)
    Some(dir) -> {
      let report_path =
        path.join(dir, "structured-output-contract-report.v1.json")
      use Nil <- result.try(
        simplifile.create_directory_all(dir)
        |> result.map_error(fn(error) {
          CliError(
            code: "write_failed",
            message: "could not create report directory `"
              <> dir
              <> "`: "
              <> simplifile.describe_error(error),
          )
        }),
      )
      simplifile.write(
        report_path,
        json.to_string(report_to_json(report)) <> "\n",
      )
      |> result.map_error(fn(error) {
        CliError(
          code: "write_failed",
          message: "could not write report `"
            <> report_path
            <> "`: "
            <> simplifile.describe_error(error),
        )
      })
    }
  }
}

fn report_to_json(report: Report) -> json.Json {
  json.object([
    #("schema_version", json.int(1)),
    #("artifact_type", json.string("structured_output_contract_report")),
    #("remote_mutations", json.string("none")),
    #("status", json.string(report.status)),
    #("workflows", json.array(report.workflows, of: workflow_summary_to_json)),
    #(
      "structured_outputs",
      json.array(
        report.structured_outputs,
        of: structured_output_summary_to_json,
      ),
    ),
    #("errors", json.array(report.errors, of: contract_error_to_json)),
  ])
}

fn workflow_summary_to_json(summary: WorkflowSummary) -> json.Json {
  json.object([
    #("workflow", json.string(summary.path)),
    #("status", json.string(summary.status)),
    #("structured_outputs", json.int(summary.structured_outputs)),
    #("errors", json.array(summary.errors, of: contract_error_to_json)),
  ])
}

fn structured_output_summary_to_json(
  summary: StructuredOutputSummary,
) -> json.Json {
  json.object([
    #("workflow", json.string(summary.workflow)),
    #("step_id", json.string(summary.step_id)),
    #("artifact_name", json.string(summary.artifact_name)),
    #("tool_name", json.string(summary.tool_name)),
    #("provider_schema_path", json.string(summary.provider_schema_path)),
    #("validator_paths", json.array(summary.validator_paths, of: json.string)),
    #("prompt_status", json.string(summary.prompt_status)),
    #("materialization_status", json.string(summary.materialization_status)),
    #("status", json.string(summary.status)),
  ])
}

fn contract_error_to_json(error: ContractError) -> json.Json {
  json.object([
    #("workflow", json.string(error.workflow)),
    #("step_id", json.string(error.step_id)),
    #("code", json.string(error.code)),
    #("message", json.string(error.message)),
    #("path", json.string(error.path)),
  ])
}

@external(erlang, "scherzo_main_ffi", "args")
fn args() -> List(String)

@external(erlang, "scherzo_main_ffi", "halt")
fn halt(code: Int) -> Nil
