You are running Scherzo's `workflow:execplan-implementation` workflow for task {{ issue.identifier }}: {{ issue.title }}.

Task URL:
{{ issue.url }}

Use these prepared files as the complete handoff:

- `tmp/execplan-review-doc.md`: human-reviewable intent, scope, risks, milestones, and acceptance.
- `tmp/execplan-implementation-pack.json`: mechanical implementation steps, verified facts, tests, interfaces, dependencies, and artifact notes.
- `tmp/execplan-bundle.json`: retained bundle provenance and hashes.

If intent, scope, acceptance, or safety in the review doc conflicts with the implementation pack, write a concise conflict report to `tmp/execplan-conflict.md` and stop without making code changes. Otherwise implement the next required milestone, update any living-document sections in the checked-in review doc when the actual state changes, and run targeted validation when useful.

Do not create commits, manage workspaces, or open a PR; the workflow publish step owns that.

Final response: summarize changed files and validation run.
