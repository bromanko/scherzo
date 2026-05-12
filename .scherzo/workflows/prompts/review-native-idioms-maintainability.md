# Native Scherzo idioms and maintainability review lane

Read the retained artifacts under `$SCHERZO_RUN_ROOT/artifacts/review/prepare_review`: `review-brief.v1.json`, `diff.patch`, `source-metadata.v1.json`, `changed-files.v1.json`, `validation-status.v1.json`, and `context-manifest.v1.json`.

Inspect `diff.patch` directly. The brief is orientation only. Do not mutate files, post comments, push, update Linear, or contact remote services. Use repository-relative paths only.

Call `submit_review_lane_draft` exactly once as your final action with the complete `review_lane_draft` object as the tool's JSON arguments. Final assistant JSON alone is invalid for this workflow. The submitted object must have lane id `idioms-maintainability` and all required top-level fields: `schema_version`, `artifact_type`, `generated_at_utc`, `producer`, `lane`, `input_refs`, `draft_findings`, `review_notes`, `evidence_requests`, `self_check`, and `remote_mutations: "none"`. Focus on repository conventions, production safety, public interfaces, error handling, module organization, and maintainability. Record broad reviewability concerns as review notes unless there is a concrete fixable finding.
