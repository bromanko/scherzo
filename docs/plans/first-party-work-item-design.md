# First-party WorkItems and workflow execution

Status: Accepted target design. This document describes intended behavior that is not fully implemented. It is not yet an implementation ExecPlan.

## Purpose

Scherzo currently uses Linear tasks and Linear workflow states as both the source of work and the visible representation of task progress. That couples external tracker state such as `In Progress`, `In Review`, and `Done` to daemon execution even though the underlying facts belong to different things.

The target model makes a WorkItem a long-lived first-party container against which people can run workflows. WorkItems remain active until archived. Workflow execution state belongs to workflow runs, and review state belongs to immutable artifact revisions. The Scherzo UI presents projections of those facts rather than asking users or integrations to maintain a mutable WorkItem status.

This design also establishes stable execution identity. A workflow definition and a resolved input snapshot identify an execution opportunity. Running the same workflow against changed inputs creates a different opportunity, while intentionally repeating the same execution uses an explicit rerun generation.

## Non-goals

This design does not define automatic triggering. Initially, a human explicitly triggers every workflow run. A future automation system may use the same trigger operation after evaluating its own policy, but event subscriptions, schedules, automatic trigger rules, and automation authorization are outside this design.

This design does not choose the eventual hosted database technology or provide a final relational schema. The initial service boundary may build on the existing Go `scherzo-api` and its embedded database, but this document defines the facts and invariants that any storage implementation must preserve.

This design does not specify final workflow YAML syntax for WorkItem artifact selectors, review predicates, or repository snapshots. The existing workflow contract is the starting point, but syntax changes require a separate design and migration.

## Vocabulary

A **Project** is the durable semantic namespace for WorkItems. It owns the WorkItem display prefix and sequence. A daemon may serve a Project, but daemon replacement does not change Project or WorkItem identity.

A **Project runtime binding** associates a semantic Project with an installation-local directory and a currently assigned managed or external daemon. Runtime bindings are replaceable infrastructure and are distinct from the semantic Project.

A **WorkItem** is a durable first-party Scherzo record that groups source material, workflow runs, artifacts, reviews, and external references. It belongs to one Project. It is not a task that progresses through `Todo`, `In Progress`, `In Review`, and `Done`.

The **work service** is the Scherzo backend that owns Projects, WorkItems, archive decisions, external references, artifact metadata, review decisions, and human-facing identifiers. It also acts as the browser's backend-for-frontend when composing durable work data with daemon queries.

A **workflow definition** is a versioned Scherzo workflow DAG plus every static value that can affect its execution, including checked-in prompts, schemas, model settings, workspace profile, and other execution-affecting configuration.

A **workflow opportunity** is a read projection for a workflow evaluated against a WorkItem. It says whether the workflow inputs can currently be resolved, which immutable values would be used, and whether that resolved execution has already run. An opportunity is not a stored WorkItem status.

A **resolved invocation** is the immutable set of values selected when Core accepts a run request. It includes contract inputs, contract context, selected artifact revisions and review decisions, trigger parameters, the repository source snapshot, and the workflow fingerprint.

A **workflow run** is one requested execution of a workflow against one resolved invocation. Runs own execution lifecycle and terminal outcome.

An **attempt** is an execution or repair attempt within a workflow run. Transport retries, step retries, and repair attempts do not silently create a new rerun generation.

An **artifact revision** is an immutable output produced by a workflow run or attached to a WorkItem. Review decisions apply to a specific artifact revision.

A **source snapshot** is the immutable repository or workspace-driver revision from which a run must prepare its starting workspace. It is backend-neutral and is not assumed to be a Git commit.

## Core invariants

A Project and its WorkItems are work-service-owned durable product state. A daemon receives Project identity as runtime context but does not own or allocate Project or WorkItem identity.

A WorkItem has a Scherzo-owned identity and does not have a general mutable status field.

A WorkItem is available for new workflow run requests until it is archived. Archiving prevents new triggers and preserves all prior runs, artifacts, reviews, and source references. Archiving does not implicitly cancel or stop active runs.

Tracker state is external source metadata. Linear may still say that a source issue is `In Progress` or `In Review`, but that value is not Scherzo's WorkItem lifecycle and must not be the authority for UI activity.

Workflow execution state belongs to workflow runs. Artifact review state belongs to artifact revisions. A completed workflow run remains completed while its outputs are reviewed.

Readiness alone never creates a workflow run in the initial design. A human trigger is required.

Core revalidates workflow definitions, inputs, artifact selections, review predicates, and source snapshots when it handles a trigger. The UI's observed projection is advisory and cannot authorize a stale run.

A run executes against the exact resolved invocation recorded when the trigger is accepted. Workspace creation must use the recorded source snapshot rather than resolving a movable reference such as `HEAD`, `@`, or `main` again later.

A completed run for one execution key does not block a run for a changed workflow definition or changed input snapshot. Repeating the exact same execution key requires an explicit rerun generation.

Idempotent retries of the same trigger request return the original run and do not create duplicate runs.

Concurrency restrictions are scheduling or workspace policy, not WorkItem status. The identity model prevents duplicate creation for the same execution key and generation without imposing a permanent one-run-per-workflow rule on the WorkItem.

## WorkItem identity and source references

Each Project owns an immutable WorkItem display prefix and an incrementing sequence. Each WorkItem receives an immutable display identifier following the human-facing style of Linear identifiers:

    <PROJECT_PREFIX>-<sequence>

For example, a Project prefix `SCH` could produce `SCH-42`. A Project prefix becomes immutable before or when the first WorkItem is allocated. Sequence allocation is transactional, sequences are never reused, and concurrent creation cannot produce duplicate identifiers. The work service owns allocation.

External systems are references attached to the WorkItem rather than its identity. A WorkItem may initially have one Linear reference containing the provider kind, remote id, display key, and URL. The model should permit additional external references without changing the Scherzo identifier.

Changing, removing, or synchronizing an external reference must not rewrite historical run or artifact identity.

## WorkItem archival

A WorkItem is active when it has no effective archive fact. Archiving is an explicit user or system decision, not a consequence of workflow success.

An archive request:

- prevents creation of new workflow runs,
- preserves prior and active workflow runs,
- preserves artifacts and reviews,
- does not implicitly stop active workers, and
- does not reinterpret completed runs or reviewed artifacts.

Archiving is reversible through an explicit, audited unarchive decision. Unarchiving permits new human triggers but does not erase prior execution keys, create runs automatically, or reinterpret history.

## Workflow contracts and input resolution

The existing contract types in `src/scherzo/workflow_contract.gleam` and retained manifests in `src/scherzo/workflow_contract_manifest.gleam` are the foundation for the target model. They already describe typed required and optional inputs, context, mapped outputs, output types, and artifact descriptors.

The current contract machinery is primarily run I/O recording. The target model additionally needs a pre-trigger resolver that can evaluate a workflow contract against a WorkItem before a run exists.

The resolver produces one of two results:

    Blocked(missing_or_invalid_inputs)

or:

    Ready(resolved_invocation, input_fingerprint)

A resolved invocation may contain:

- canonical WorkItem fields,
- external source fields retained for compatibility,
- explicit trigger parameters,
- immutable artifact revisions,
- review decisions or review predicates required by the workflow,
- mapped outputs from prior workflow runs,
- literal workflow inputs,
- repository source snapshot information, and
- other declared execution context.

Required missing values produce structured diagnostics suitable for both UI display and trigger rejection. Optional absence is represented canonically and participates in fingerprinting; it is not silently omitted in a way that can create ambiguous identity.

Input resolution must be factored so the values shown by the opportunity projector and the values persisted for an accepted run use the same rules. Core must still resolve again at trigger time to detect stale UI state.

The run-specific retained input manifest should be generated from the accepted resolved invocation rather than independently re-resolving values after run creation.

## Workflow and input fingerprints

The workflow fingerprint identifies the static execution definition. Existing workflow fingerprinting already includes execution-affecting workflow configuration such as the DAG, checked-in prompts, schemas, model settings, workspace profile, and related configuration. A checked-in prompt change therefore changes the workflow fingerprint.

The input fingerprint identifies the canonical resolved invocation. It includes every dynamic value that can affect execution, including selected WorkItem values, trigger parameters, artifact revisions, required review facts, mapped outputs, and the source snapshot. A human-supplied prompt is a dynamic input and therefore changes the input fingerprint.

The base execution key is derived from:

    work_item_id
    + workflow_id
    + workflow_fingerprint
    + input_fingerprint

The canonical encoding must be versioned and deterministic. Hashing must not depend on map iteration order, local absolute paths, display-only URLs, or mutable external state that the workflow cannot observe.

Any ambient task or template value that can affect execution but is not declared in the current contract must still participate in execution identity during migration. The longer-term contract design should make execution-affecting inputs explicit rather than relying on ambient `issue.*` compatibility variables.

## Repository source snapshots

Every repository-backed workflow implicitly includes its starting source snapshot in the resolved invocation, even when the workflow YAML does not explicitly declare a repository input. This prevents workflow authors from accidentally omitting repository state from deduplication and recovery.

The snapshot should use workspace-driver vocabulary rather than Git-specific fields. At minimum it must identify:

- the repository or source namespace,
- the selected workspace driver,
- an immutable source revision suitable for workspace creation,
- the configured or requested base reference when relevant, and
- whether the source was dirty or otherwise not reproducible.

The current workspace-driver `baseline --json` contract exposes `baseline_id`, `workspace_revision_id`, `change_id`, and `dirty`, but it is workflow-facing and describes an already-prepared workspace. Source capture is orchestrator-facing and happens before workspace creation. Because Scherzo has not shipped and compatibility is not required, an implementation spike should compare redesigning or renaming the existing baseline semantics with adding a distinct pre-workspace source-snapshot operation. The selected protocol must also let lifecycle creation recreate the exact captured snapshot.

A code-changing workflow fails closed when its driver cannot identify and recreate a stable starting snapshot. A dirty Git source is rejected unless the driver can encode the exact dirty tree as an immutable, recreatable snapshot. A jj working-copy commit is acceptable when the driver can recreate that exact commit. No-op or artifact-only workflows may declare a documented non-repository mode rather than fabricate a commit identity.

## Runs, execution keys, and generations

The first accepted run for a base execution key uses generation `1`.

A normal **Run** action resolves current inputs. If the resulting execution key has never been run, Core creates generation `1`. If the exact execution key has already been consumed, Core does not silently create another run and instead requires an explicit repeat action.

Two repeat operations are required:

- **Rerun this run** uses the exact recorded resolved invocation and source snapshot from a historical run, then allocates the next generation for that base execution key.
- **Run again with current inputs** resolves the workflow against current WorkItem, artifact, review, and repository facts. Changed values produce a new base execution key and generation `1`; unchanged values allocate the next generation only after explicit confirmation.

A terminal run may have outcome `succeeded`, `failed`, or `canceled`. A run is not reopened merely because an artifact review changes. Existing retry-step and repair behavior may add attempts or repaired execution within the run where supported; an intentional fresh execution is represented by a new generation.

The persistent uniqueness boundary is the base execution key plus generation. Trigger requests also carry an idempotency key. Reusing the same idempotency key with the same request returns the same run; reusing it with a different request is an idempotency conflict.

## Human trigger contract

Initially, all workflow runs are triggered by humans through a Core-owned action.

The WorkItem query exposes an action descriptor containing safe observed state such as:

- WorkItem id,
- workflow id,
- workflow fingerprint,
- input fingerprint when ready,
- action instance or opportunity fingerprint,
- whether the action is Run, Rerun, or Run again with current inputs, and
- structured disabled reasons when inputs are missing or the WorkItem is archived.

The trigger command carries the action id, target, observed fingerprints, idempotency key, and only the explicit parameters permitted by the workflow contract. Core resolves current state again before creating the run and rejects stale, disabled, malformed, or conflicting requests without side effects.

A future automation component may submit the same command with an automation principal. Automation policy is not part of this design.

## Artifacts and reviews

Artifact revisions are immutable. A workflow run produces new artifact revisions rather than mutating prior revisions in place.

A review decision applies to one exact artifact revision. A replacement or superseding revision does not inherit approval from the replaced revision. Historical decisions remain visible for audit and for runs that consumed the older revision.

Reviews are append-only decisions. The first slice uses:

- no decision, projected as pending review,
- `Approve`,
- `RequestChanges`, and
- `RevokeApproval`.

`RevokeApproval` prevents new workflows that require approval from consuming the revision, but it does not mutate historical runs that captured an earlier approval. A distinct permanent `Reject` decision is deferred until product behavior requires a distinction from requesting changes.

Supersession is separate from review. A replacement artifact revision explicitly supersedes its predecessor, does not inherit approval, and leaves the predecessor plus its historical reviews intact. Default selectors ignore superseded revisions.

Workflow input resolution may select an artifact by descriptor, WorkItem association, revision policy, and review predicate. For example, an implementation workflow may require the latest approved artifact matching an ExecPlan descriptor. The design deliberately does not choose the YAML representation yet.

The selected artifact revision and the review fact satisfying the predicate are captured in the resolved invocation. Later review changes do not mutate a historical run. They may change whether a new workflow opportunity is ready and will change the input fingerprint when the review fact is execution-relevant.

Artifact review never changes the producing workflow run from completed to in review, and it never moves the WorkItem into an `In Review` state.

## Worked example: ExecPlan to implementation

An active WorkItem contains a problem statement and a Linear source reference. The ExecPlan workflow requires the WorkItem problem context and a reproducible repository source snapshot. Its checked-in DAG and prompts have a workflow fingerprint.

The WorkItem query resolves those inputs and shows the ExecPlan workflow as ready. A human selects Run. Core revalidates the inputs and source revision, creates generation `1`, and prepares the workspace from the recorded revision.

The run succeeds and produces an immutable ExecPlan artifact revision. The workflow run is complete. The WorkItem remains active. The artifact is initially unreviewed and is later approved.

The implementation workflow requires an approved ExecPlan artifact and a current repository source snapshot. Before artifact approval it is blocked with a structured missing-input reason. After approval it becomes ready. A human triggers it, and the run records the approved ExecPlan revision, the satisfying review fact, and the then-current repository snapshot.

If the ExecPlan artifact changes, its replacement is a new revision and requires its own review. If the repository changes, the source snapshot changes. Either change produces a new input fingerprint and therefore a new execution key. Intentionally repeating an unchanged execution requires Rerun or Run again with current inputs.

Archiving the WorkItem prevents further run creation but leaves all plan, implementation, artifact, review, and run history visible. An implementation run already active at archive time is allowed to finish unless an operator separately stops it.

## Three-part service architecture

The initial product has three logical parts:

- the browser UI,
- the work service and UI backend, initially built by evolving `services/scherzo-api` in the `scherzo-ui` repository, and
- the Project daemon.

The work service owns semantic Projects and WorkItems. The daemon owns executable workflow definitions, local input and source-snapshot resolution, live execution, workspaces, attempts, steps, recovery state, and locally retained artifact bodies. The browser owns presentation and user interaction only.

A Project runtime binding maps the work-service Project identity to an installation-local directory and current daemon. Local directories and managed-daemon launch details remain binding data rather than globally portable Project identity.

The browser calls one work-service API. It does not contact or merge daemon payloads directly. For WorkItem reads, the work service loads durable catalog and review facts, resolves the Project runtime binding, queries the daemon through the existing daemon bridge, and returns one composed response. When the daemon is unavailable, durable WorkItem, artifact metadata, and review data remain readable; runtime projection is marked unavailable or stale and run actions are disabled.

Initially, the work service uses request-time query composition rather than maintaining a second durable copy of daemon runtime state. A later event-fed read model may replace fan-out if measured latency or availability requires it.

For human triggers, the browser submits one action to the work service. The work service resolves the Project daemon binding and proxies the authenticated action to Core. The daemon revalidates workflow, WorkItem snapshot, inputs, reviews, and source snapshot, allocates the run identity and generation in its durable local execution state, and returns a result that the work service records as a command receipt/linkage. Promoting pending run requests into a central queue is deferred until disconnected-daemon or cross-daemon execution requires it.

This arrangement keeps Projects and WorkItems stable when daemons restart or are replaced, while leaving local execution and recovery with the component that owns the repository and workers.

## UI projection

The WorkItem header shows its Scherzo identifier, title, source references, metadata, and an Archived marker when applicable. It does not show a lifecycle status such as `In Progress` or `Done`.

The workflow section projects each applicable workflow independently. Useful presentation states include:

- waiting for inputs, with exact missing requirements,
- ready to run,
- trigger requested or pending dispatch,
- running, with current run and step,
- current inputs already run,
- last run failed, and
- unavailable because the WorkItem is archived.

These are workflow-opportunity and run projections, not persisted WorkItem statuses.

The run section shows immutable execution history, including generation, workflow and input fingerprints, source snapshot, trigger principal, timing, outcome, attempts, steps, and outputs.

The artifact section shows artifact revisions, producing runs, review decisions, supersession relationships, and workflows that can consume them.

External tracker state may be shown as secondary source metadata during migration, but UI code must not infer Scherzo activity or action availability from it. Core owns action derivation and revalidation.

## Relationship to the current implementation

`src/scherzo/task.gleam` currently represents a tracker-provided task and includes `Task.state`. During migration, that value remains provider source state and should be named or documented accordingly rather than treated as Scherzo lifecycle.

`src/scherzo/work_item.gleam` is the existing UI-oriented read model. `WorkItemSummary.state` currently exposes tracker state directly. The target is to add workflow opportunities, current run activity, artifacts, and archive projection, then retire or explicitly demote the compatibility state field.

`src/scherzo/control/query/backend.gleam` already joins provider-live WorkItem data with daemon and ledger information for action derivation. It is the natural composition boundary for the first read-model slice.

`src/scherzo/orchestrator/task_lifecycle.gleam` already defines operational states such as validating, claiming, starting, running, retry waiting, stopping, and parked. Those facts can contribute to run activity, but they are not a WorkItem lifecycle.

`src/scherzo/state/projection.gleam` already retains workflow runs, step attempts, retries, parks, artifacts, publications, and contract manifests. The first-party database should join those operational facts rather than turning the existing append-only operational ledger into the WorkItem catalog.

`src/scherzo/workflow_contract.gleam`, `src/scherzo/workflow_contract_manifest.gleam`, and `src/scherzo/workflow_run/contract_io.gleam` provide the contract and manifest foundation. They need a pre-trigger resolution boundary and canonical input fingerprint rather than wholesale replacement.

The existing `work_item.run_workflow` action direction assumed eventual creation of a Linear workflow-subtask. This target design supersedes that assumption: triggering creates a first-party workflow run. Linear child tasks may remain external source relationships during migration but are not required to represent execution.

The `scherzo-ui` repository already contains the browser control app and Go `services/scherzo-api`. Its existing `docs/project-model-decision.md` makes Projects durable roots, WorkItems Project-owned, and daemons replaceable runtime infrastructure. Its existing Project REST API, embedded migrations, and daemon WebSocket/query bridge are the preferred initial work-service and composition seam. A separately deployed hosted work service may be extracted later without moving semantic ownership into the daemon.

## Linear migration direction

The migration should be additive and field-by-field.

First, retain Linear as the source of WorkItem content while enriching the WorkItem query with daemon-derived workflow activity and ledger-derived run and artifact history. The UI switches away from tracker status before first-party task storage is complete.

Second, add Scherzo WorkItem identifiers and map existing Linear issues as external references. Runs and artifacts refer to the Scherzo WorkItem identity.

Third, migrate the low-risk research workflow to first-party human triggering. The human Run action creates a first-party pending run, while that workflow route is excluded from legacy Linear polling. Existing Linear polling and status-driven dispatch remain a compatibility path for other workflows until each route is explicitly migrated or isolated so duplicate runs cannot be created.

Fourth, move WorkItem content and archive decisions into first-party storage. Linear state becomes optional source metadata or a downstream compatibility projection.

Finally, remove tracker state from dispatch and UI authority. If Scherzo continues to update Linear statuses, those updates are one-way integration projections and must not become a second writable source of Scherzo lifecycle.

## Migration principles

The UI should consume one Scherzo-owned WorkItem projection throughout the migration and should not know which fields are still backed by Linear.

Every mutable field or decision has one authoritative writer at a time. Mirroring to Linear does not make Linear authoritative.

First-party and legacy dispatch paths must not both create runs for the same execution opportunity. Coexistence requires an explicit routing or feature boundary and observable duplicate-prevention evidence.

Schema changes should be additive, recoverable, and testable with old Linear-backed WorkItems and new first-party WorkItems in the same deployment where practical.

The existing operational ledger remains the source for durable run and recovery facts until a separate migration deliberately changes that boundary. The WorkItem catalog must not absorb raw prompts, raw provider payloads, or secret-bearing runtime data.

## Decisions recorded

- WorkItems remain active until archived and do not progress through workflow statuses.
- Archiving prevents new triggers, preserves history, and does not cancel active runs.
- Semantic Projects and WorkItems are work-service-owned; daemons are replaceable Project runtime bindings.
- Each Project owns an immutable prefix and transactional incrementing WorkItem sequence.
- Archiving is reversible through an audited unarchive decision.
- Workflow execution and terminal outcome belong to runs.
- Artifact review belongs to immutable artifact revisions.
- Initial review decisions are Approve, RequestChanges, and RevokeApproval; supersession is separate.
- Initial workflow triggering is human-only.
- Execution identity combines WorkItem, workflow fingerprint, and resolved input fingerprint.
- Repository source snapshot is an implicit execution input for repository-backed workflows.
- Changed workflow definitions or inputs produce a new execution key.
- Repeating an unchanged execution requires an explicit generation.
- Rerun this run reuses recorded inputs; Run again with current inputs re-resolves them.
- Trigger handling is stale-safe and idempotent.
- `work_item.run_workflow` creates a first-party run rather than a Linear child task.
- The browser calls one work-service API; the work service composes durable data with daemon queries.
- The existing Go `scherzo-api` is the preferred initial work-service/BFF seam rather than adding another deployable immediately.
- The daemon owns local run execution and recovery in the first slice; a central pending-run queue is deferred.
- Research is the first low-risk workflow migration.
- Automation policy and final YAML selector syntax are deferred.

## Open questions before implementation planning

- What exact REST resources and revision/ETag semantics connect Projects, WorkItems, reviews, and composed daemon projections?
- What authenticated command envelope carries a work-service WorkItem snapshot or revision to the daemon, and how does Core detect a concurrent WorkItem edit during trigger acceptance?
- Should the workspace-driver protocol redesign the existing `baseline` operation or add a distinct pre-workspace source-snapshot operation?
- What is the minimal workflow contract syntax for WorkItem fields, trigger parameters, artifact selectors, and review predicates?
- Which ambient compatibility values must enter the input fingerprint before workflows are fully contract-driven?
- What scheduler and workspace constraints, if any, should apply when different execution keys of the same workflow are active concurrently?
- How does the daemon publish stable artifact metadata to the work service while artifact bodies remain local?
- Which database technology replaces or complements embedded SQLite if the work service becomes separately hosted or multi-user?
- What exact workflow-route boundary disables legacy polling for the research workflow during the first-party trigger pilot?

## Next document

After this target design is reviewed and accepted, write a separate ExecPlan that inventories the exact current query, action, contract, run, ledger, workspace-driver, and Linear dispatch call sites. The ExecPlan should deliver the migration in independently verifiable slices, starting with the WorkItem projection and pre-trigger resolver rather than introducing the entire first-party database in one change.
