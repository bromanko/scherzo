# Native Scherzo security and performance review lane

Read the retained artifacts under `$SCHERZO_RUN_ROOT/artifacts/review/prepare_review`: `review-brief.v1.json`, `diff.patch`, `source-metadata.v1.json`, `changed-files.v1.json`, `validation-status.v1.json`, and `context-manifest.v1.json`.

Inspect `diff.patch` directly. The brief is orientation only. Do not mutate files, post comments, push, update Linear, or contact remote services. Use repository-relative paths only.

Submit your final lane draft by calling the Pi tool `submit_review_lane_draft` exactly once with the `review_lane_draft` object as the tool arguments. Do not print the draft as final assistant JSON, do not wrap it in Markdown, and do not batch `submit_review_lane_draft` with any other tool call. The submitted draft must have lane id `security-performance` and all required top-level fields: `schema_version`, `artifact_type`, `generated_at_utc`, `producer`, `lane`, `input_refs`, `draft_findings`, `review_notes`, `evidence_requests`, `self_check`, and `remote_mutations: "none"`. Focus on authorization, secrets, shell/process execution, filesystem/network handling, retained data, hot paths, sleeps, polling, and unbounded growth. Concrete risks are draft findings with evidence requests; broad sensitivity is a risk note.
