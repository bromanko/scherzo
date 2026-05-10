# Native Scherzo idioms and maintainability review lane

Read the retained artifacts under `$SCHERZO_RUN_ROOT/artifacts/review/prepare_review`: `review-brief.v1.json`, `diff.patch`, `source-metadata.v1.json`, `changed-files.v1.json`, `validation-status.v1.json`, and `context-manifest.v1.json`.

Inspect `diff.patch` directly. The brief is orientation only. Do not mutate files, post comments, push, update Linear, or contact remote services. Use repository-relative paths only.

Return JSON only. The JSON must be a `review_lane_draft` artifact with lane id `idioms-maintainability` and `remote_mutations: "none"`. Focus on repository conventions, production safety, public interfaces, error handling, module organization, and maintainability. Record broad reviewability concerns as review notes unless there is a concrete fixable finding.
