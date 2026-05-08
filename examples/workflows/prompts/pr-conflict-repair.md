You are running Scherzo scheduled job {{ scheduled_job.id }}.

Workflow: {{ scheduled_job.workflow }}
Due at: {{ schedule.due_at }}
Started at: {{ schedule.started_at }}
Run ID: {{ run.id }}
Attempt: {{ run.attempt }}

Inspect the repository and repair merge conflicts reported by the inspect step.
This job may be retried for the same due interval, so make all changes idempotent.
If there is no conflict to repair, report success concisely and stop.
