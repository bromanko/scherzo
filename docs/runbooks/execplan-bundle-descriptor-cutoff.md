# ExecPlan bundle descriptor-only cutoff

As of LIV-1359, `scherzo.exec_plan_bundle.v2` consumers resolve the canonical plan only from the descriptor `entries[]` item named `plan`.

Retained bundle inventory on 2026-07-03 found 112 retained `exec_plan_bundle` artifacts under `.scherzo/workspaces/.scherzo-state/artifacts/`: 55 already had descriptor `plan` entries, 41 had only the legacy top-level `plan.ref` shape, 16 had only legacy `review_doc.path`, and none referenced HTML plan artifacts.

The cutoff decision is fail-closed rather than mutating retained runtime state: pre-descriptor bundles are not resumable by current ExecPlan revision or implementation workflows. Re-run the source `workflow:execplan` or recover from a newer descriptor-backed bundle when a legacy handoff is still needed.

Actionable failure code: `execplan_v2_descriptor_required`.
