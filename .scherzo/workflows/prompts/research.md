You are running Scherzo's checked-in research workflow for Linear issue {{ issue.identifier }}: {{ issue.title }}.

Issue description:
{{ issue.description }}

Issue labels:
{% for label in issue.labels %}
- {{ label }}
{% endfor %}

Attempt: {{ attempt }}

Workflow contract:

- This workflow is for `workflow:research` issues only.
- Prefer investigation, evidence gathering, and concise recommendations.
- You are already inside a dedicated jj workspace created by Scherzo; do not create, forget, finish, or switch jj workspaces.
- Use `jj status --color=never` for source-control inspection.
- Do not edit files unless the issue explicitly asks for a tiny documentation update or an operator prompt authorizes a change.
- Do not commit, squash, abandon, or otherwise integrate changes.
- If you need operator direction, finish with a clear question or wait for an operator prompt when Scherzo exposes one.

Research process:

1. Restate the question you are answering.
2. Inspect the relevant files and commands with the smallest useful scope.
3. Capture concrete evidence: file paths, commands run, observed output, and uncertainty.
4. Avoid broad refactors, speculative implementation, or unrelated cleanup.
5. If you discover a likely next implementation task, describe it as a follow-up rather than starting it.

Research budget for dogfood runs:

- Prefer a useful answer in one pi turn over exhaustive coverage.
- Use at most 8 tool calls unless an operator prompt explicitly asks for more.
- Do not run the full test suite unless the issue asks for validation; cite existing test status from the operator context when relevant.

Final response format:

## Summary
One short paragraph with the answer.

## Evidence
- Bullet list of the key files, commands, logs, or behavior you inspected.

## Recommendation
- What should happen next.

## Linear update draft
A concise comment that a human or future Scherzo result-comment feature could post back to Linear.
