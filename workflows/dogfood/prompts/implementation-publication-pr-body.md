## Work

- Linear issue: `{{ work.identifier }}` — {% if work.title %}{{ work.title }}{% else %}title unavailable{% endif %}
- Source: {% if work.url %}{{ work.url }}{% else %}source URL unavailable{% endif %}

## Summary

Scherzo published the implementation changes produced by the `{{ workflow.id }}` workflow through the `{{ publication.id }}` same-repo `commit_stack` publication route.

## Publication details

- Workflow: `{{ workflow.id }}`
- Publication route: `{{ publication.id }}`
- Run: `{{ run.id }}`
- Version: `{{ publication.version_id }}`
- Target repo: `{{ github.repo }}`
- Base branch: `{{ github.base }}`

## Changed files

Review the GitHub **Files changed** tab for the published commit stack. Scherzo validation and review artifacts also retain the change analysis produced during the workflow run.

## Validation and review evidence

Validation output, native review results, and publication attempt records are retained in the Scherzo workflow run artifacts for `{{ run.id }}`. When Linear metadata is available, the Scherzo result attachment on the Linear issue links back to this publication.
