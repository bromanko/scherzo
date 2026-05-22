import gleam/option.{None, Some}
import scherzo/workflow_dag
import scherzo/workflow_dag_compat

pub fn normalize_accepts_legacy_workflow_dag_without_recover_test() {
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: legacy\ncontract:\n  version: 1\n  outputs:\n    summary:\n      type: document.markdown\n      source:\n        step: summarize\n        field: stdout\nsteps:\n  - id: collect\n    kind: command\n    run: collect\n  - id: summarize\n    kind: command\n    depends_on: [collect]\n    run: summarize\n",
    )
  let legacy = legacy_without_recover(dag)

  let normalized = workflow_dag_compat.normalize(legacy)

  assert normalized.id == "legacy"
  assert normalized.recover == None
  let assert Some(_) = normalized.contract
  let assert [collect, summarize] = normalized.steps
  assert collect.id == "collect"
  assert collect.recover == None
  assert summarize.id == "summarize"
  assert summarize.depends_on == ["collect"]
  assert summarize.recover == None
}

@external(erlang, "scherzo_workflow_dag_compat_test_ffi", "legacy_without_recover")
fn legacy_without_recover(
  dag: workflow_dag.WorkflowDag,
) -> workflow_dag.WorkflowDag
