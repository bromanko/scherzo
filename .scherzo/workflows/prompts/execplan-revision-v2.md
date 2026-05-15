You are revising a previously published ExecPlan v2 bundle from actionable review feedback.

Read `tmp/execplan-v2-previous-bundle.json`, `tmp/execplan-v2-previous-pack.json`, and the checked-in review document path in `tmp/execplan-v2-review-doc.path`. Update the review document only when the feedback changes human-reviewable intent, scope, risk, rollout, milestones, or acceptance. If the feedback requires no change, leave the review document unchanged.

Submit a structured `exec_plan_revision_submission` through `submit_implementation_pack_submission` using `.scherzo/workflows/schemas/exec-plan-revision-submission.v2.schema.json`. Set `revision_status` to `unchanged` only when both the review doc and implementation mechanics remain unchanged; otherwise set it to `changed` and include updated mechanical sections.

Do not write canonical bundle JSON yourself. The helper will materialize the superseding bundle.
