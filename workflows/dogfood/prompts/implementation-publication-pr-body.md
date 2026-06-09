Published by Scherzo's same-repo `commit_stack` publication route for `{{ workflow.id }}`.

## Work

- Identifier: {% if work.identifier %}`{{ work.identifier }}`{% else %}Unavailable{% endif %}
- Title: {% if work.title %}{{ work.title }}{% else %}Unavailable{% endif %}
- Linear/source link: {% if work.url %}{% if work.identifier %}[{{ work.identifier }}]({{ work.url }}){% else %}{{ work.url }}{% endif %}{% else %}Unavailable{% endif %}

## Summary

This PR implements the source work item and publishes the resulting same-repository code change for human review.

## Publication details

- Workflow: `{{ workflow.id }}`
- Publication route: `{{ publication.id }}`
- Run: `{{ run.id }}`
- Version: `{{ publication.version_id }}`
- Target repo: `{{ github.repo }}`
- Base branch: `{{ github.base }}`

## Changed files

{% if publication.changed_files_markdown %}{{ publication.changed_files_markdown }}{% else %}Unavailable from this commit_stack artifact.{% endif %}

## Validation and review evidence

Validation results, native review lane outputs, repair summaries (if any), and publication attempt records are retained in the Scherzo workflow run artifacts for `{{ run.id }}`. The Linear result attachment/comment also links back to the retained evidence when available.
