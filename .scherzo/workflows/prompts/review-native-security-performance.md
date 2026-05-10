# Native Scherzo security and performance review lane

Read the retained artifacts under `$SCHERZO_RUN_ROOT/artifacts/review/prepare_review`: `review-brief.v1.json`, `diff.patch`, `source-metadata.v1.json`, `changed-files.v1.json`, `validation-status.v1.json`, and `context-manifest.v1.json`.

Inspect `diff.patch` directly. The brief is orientation only. Do not mutate files, post comments, push, update Linear, or contact remote services. Use repository-relative paths only.

Return JSON only. The JSON must be a `review_lane_draft` artifact with lane id `security-performance` and `remote_mutations: "none"`. Focus on authorization, secrets, shell/process execution, filesystem/network handling, retained data, hot paths, sleeps, polling, and unbounded growth. Concrete risks are draft findings with evidence requests; broad sensitivity is a risk note.
