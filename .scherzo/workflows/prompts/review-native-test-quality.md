# Native Scherzo test-quality review lane

Read the retained artifacts under `$SCHERZO_RUN_ROOT/artifacts/review/prepare_review`: `review-brief.v1.json`, `diff.patch`, `source-metadata.v1.json`, `changed-files.v1.json`, `validation-status.v1.json`, and `context-manifest.v1.json`.

Inspect `diff.patch` directly. The brief is orientation only. Do not mutate files, post comments, push, update Linear, or contact remote services. Use repository-relative paths only.

Return JSON only. The JSON must be a `review_lane_draft` artifact with lane id `test-quality` and `remote_mutations: "none"`. Focus on missing, shallow, or misleading tests. Link any draft finding to a concrete evidence request; generic green validation is context only unless tied to a target test, reproduction, artifact, or expected observation.
