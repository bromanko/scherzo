import gleam/dict.{type Dict}
import gleam/list
import scherzo/workflow_dag

pub fn normalize(dag: workflow_dag.WorkflowDag) -> workflow_dag.WorkflowDag {
  normalize_ffi(dag)
}

pub fn normalize_map(
  workflows: Dict(String, workflow_dag.WorkflowDag),
) -> Dict(String, workflow_dag.WorkflowDag) {
  workflows
  |> dict.to_list
  |> list.map(fn(entry) {
    let #(id, dag) = entry
    #(id, normalize(dag))
  })
  |> dict.from_list
}

@external(erlang, "scherzo_workflow_dag_compat_ffi", "normalize")
fn normalize_ffi(dag: workflow_dag.WorkflowDag) -> workflow_dag.WorkflowDag
