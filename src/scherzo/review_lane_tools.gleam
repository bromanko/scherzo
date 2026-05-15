import gleam/list
import gleam/option.{Some}
import gleam/result
import gleam/string
import scherzo/structured_output_tool_spec
import scherzo/workflow_dag

pub type SchemaDigest {
  SchemaDigest(path: String, sha256: String)
}

pub type ReviewLaneTool {
  ReviewLaneTool(
    step_id: String,
    spec: workflow_dag.StructuredOutputSpec,
    tool_spec: structured_output_tool_spec.ToolSpec,
  )
}

pub fn for_workflow(
  workflow_id: String,
  dag: workflow_dag.WorkflowDag,
  repository_root: String,
) -> Result(List(ReviewLaneTool), structured_output_tool_spec.ToolSpecError) {
  review_lane_tools_loop(dag.steps, workflow_id, repository_root, [])
}

fn review_lane_tools_loop(
  steps: List(workflow_dag.WorkflowStep),
  workflow_id: String,
  repository_root: String,
  acc: List(ReviewLaneTool),
) -> Result(List(ReviewLaneTool), structured_output_tool_spec.ToolSpecError) {
  case steps {
    [] -> Ok(list.reverse(acc))
    [step, ..rest] ->
      case step.kind {
        workflow_dag.AgentStep(_, Some(spec)) ->
          case is_review_lane_structured_output(spec) {
            False ->
              review_lane_tools_loop(rest, workflow_id, repository_root, acc)
            True -> {
              use tool_spec <- result.try(
                structured_output_tool_spec.for_step(
                  structured_output_tool_spec.BuildInput(
                    workflow_id: workflow_id,
                    run_id: "review-lane-preflight",
                    step_id: step.id,
                    attempt_index: 0,
                    repository_root: repository_root,
                    spec: spec,
                  ),
                ),
              )
              review_lane_tools_loop(rest, workflow_id, repository_root, [
                ReviewLaneTool(
                  step_id: step.id,
                  spec: spec,
                  tool_spec: tool_spec,
                ),
                ..acc
              ])
            }
          }
        _ -> review_lane_tools_loop(rest, workflow_id, repository_root, acc)
      }
  }
}

fn is_review_lane_structured_output(
  spec: workflow_dag.StructuredOutputSpec,
) -> Bool {
  case structured_output_tool_spec.schema_path_for_source(spec.source) {
    Some(schema_path) ->
      string.starts_with(
        schema_path,
        "docs/schemas/provider/review-lane-draft.",
      )
    _ -> False
  }
}
