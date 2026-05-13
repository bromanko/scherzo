# Native Scherzo idioms and maintainability review lane

Read the retained artifacts under `$SCHERZO_RUN_ROOT/artifacts/review/prepare_review`: `review-brief.v1.json`, `diff.patch`, `source-metadata.v1.json`, `changed-files.v1.json`, `validation-status.v1.json`, and `context-manifest.v1.json`.

Inspect `diff.patch` directly. The brief is orientation only. Do not mutate files, post comments, push, update Linear, or contact remote services. Use repository-relative paths only.

Call `submit_review_lane_draft` exactly once as your final action with the complete `review_lane_draft` object as the tool's JSON arguments. Final assistant JSON alone is invalid for this workflow. The submitted object is validated by `docs/schemas/review-lane-draft.v1.schema.json` plus semantic consistency checks. It must have lane id `idioms-maintainability` and all required top-level fields: `schema_version`, `artifact_type`, `generated_at_utc`, `producer`, `lane`, `input_refs`, `draft_findings`, `review_notes`, `evidence_requests`, `self_check`, and `remote_mutations: "none"`.

Nested contract reminders: every `input_refs` item must be an object with non-empty `artifact_type` and repository- or run-root-relative `path`; every `draft_findings` item must include non-empty `draft_finding_id`, `title`, `claim`, and `severity`, boolean `proposed_blocking`, list `locations`, and list `evidence_request_ids`; every `review_notes` item must include non-empty `id`, `kind`, `category`, `severity`, `summary`, `details`, `suggested_action`, and list `locations`; every `evidence_requests` item must include non-empty `request_id`, `draft_finding_id`, `evidence_key`, `claim`, and `expected_observation`, plus object `target` (use `target.changed_file_path` or `target.artifact_path` when applicable).

Focus on repository conventions, production safety, public interfaces, error handling, module organization, and maintainability. Record broad reviewability concerns as review notes unless there is a concrete fixable finding.
