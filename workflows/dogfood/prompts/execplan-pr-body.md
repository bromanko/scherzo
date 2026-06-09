## Summary

This PR publishes a retained ExecPlan review document for {% if work.identifier %}`{{ work.identifier }}`{% else %}the source work item{% endif %}.

This is a planning/review artifact only. It adds or updates the human-reviewable ExecPlan source and does not implement the planned code changes.

## Work

- Identifier: {% if work.identifier %}`{{ work.identifier }}`{% else %}Unavailable{% endif %}
- Title: {% if work.title %}{{ work.title }}{% else %}Unavailable{% endif %}
- Linear/source link: {% if work.url %}{% if work.identifier %}[{{ work.identifier }}]({{ work.url }}){% else %}{{ work.url }}{% endif %}{% else %}Unavailable{% endif %}

## Published artifact

{% if publication.files_markdown %}{{ publication.files_markdown }}{% else %}Unavailable from this publication route.{% endif %}

## Publication details

- Workflow: `{{ workflow.id }}`
- Publication route: `{{ publication.id }}`
- Run: `{{ run.id }}`
- Version: `{{ publication.version_id }}`
- Target repo: `{{ github.repo }}`
- Base branch: `{{ github.base }}`

## Validation and review evidence

Review the retained ExecPlan bundle, publication attempt manifest, and workflow run artifacts for `{{ run.id }}`. The Linear result attachment/comment also links to retained validation and review evidence when available.

## Review guidance

Review the document for scope, risks, milestones, acceptance criteria, rollout, recovery, and whether the follow-up implementation can be executed safely.
