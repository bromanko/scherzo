## Work

- Linear issue: `{{ work.identifier }}` — {% if work.title %}{{ work.title }}{% else %}title unavailable{% endif %}
- Source: {% if work.url %}{{ work.url }}{% else %}source URL unavailable{% endif %}

## Summary

This PR publishes a retained ExecPlan review document for `{{ work.identifier }}`.

This is a planning/review artifact only. It adds or updates the human-reviewable ExecPlan source and does not implement the planned code changes.

## Published artifact

{{ publication.files_markdown }}

## Publication details

- Workflow: `{{ workflow.id }}`
- Publication route: `{{ publication.id }}`
- Run: `{{ run.id }}`
- Version: `{{ publication.version_id }}`
- Target repo: `{{ github.repo }}`
- Base branch: `{{ github.base }}`

## Review and validation evidence

Review the document for scope, risks, milestones, acceptance criteria, rollout, recovery, and whether the follow-up implementation can be executed safely. Retained Scherzo workflow artifacts for `{{ run.id }}` contain the source bundle, publication attempt, and any review/validation evidence produced by the workflow.
