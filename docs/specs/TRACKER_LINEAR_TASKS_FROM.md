# `tracker.linear.tasks_from` specification

Status: design/specification for Linear task-scope predicates. This repository slice implements `project`, `projects`, `all_labels`, and `any_label` leaves plus `and`/`or` composition, plus doctor summaries and static overlap warnings for the supported shapes; team predicates remain future work.

## Purpose

`tracker.linear.tasks_from` defines which Linear issues Scherzo owns as daemon tasks. Today Scherzo effectively uses a single Linear project slug as that boundary. This spec keeps that default meaning while defining a restricted, Scherzo-owned predicate language that future parser and runtime work must apply consistently across every Linear read path.

The goal is to let operators express task scope precisely without exposing raw Linear GraphQL filters. That keeps validation, `doctor` diagnostics, overlap analysis, and future compatibility under Scherzo control.

## Current baseline

Current production behavior is project-scoped with explicit multi-project support, project-anchored label predicates, and boolean composition.

- `src/scherzo/config/tracker_config.gleam` reads `tracker.linear.tasks_from.project`, `tracker.linear.tasks_from.projects`, `tracker.linear.tasks_from.all_labels`, `tracker.linear.tasks_from.any_label`, `tracker.linear.tasks_from.and`, and `tracker.linear.tasks_from.or` into an internal task scope.
- When `tracker.linear.tasks_from` is absent, `tracker.linear.project`, `tracker.linear.project_slug`, and `tracker.project_slug` desugar to the single-project task scope.
- Linear read paths compile the task scope to one shared `IssueFilter` variable.

The `tasks_from` implementation preserves the single-project default when no explicit predicate is present.

## Scope and non-goals

This spec defines the configuration shape, semantics, validation rules, compatibility behavior, `doctor` expectations, and the Linear query paths that must share one compiled predicate.

This phase adds parser support, schema fields, runtime compilation, and doctor/operator diagnostics for `project`, `projects`, `all_labels`, and `any_label` leaves plus `and`/`or` composition over those leaves. It does not add team predicates, cache changes, or provider-live changes.

## Predicate model

`tracker.linear.tasks_from` is a restricted abstract syntax tree. Each node is a map with exactly one key. Raw Linear GraphQL fragments are not allowed.

This build supports these keys at any predicate node:

- `project`
- `projects`
- `all_labels`
- `any_label`
- `and`
- `or`

`all_labels` and `any_label` are valid predicate leaves, but this version requires every accepted complete predicate branch to be anchored by `project` or `projects`. Label-only task scopes are intentionally reserved until a later design adds a `team` leaf or an explicit workspace-wide opt-in.

### One-key rule

Every predicate object must contain exactly one supported key.

Valid:

```yaml
tracker:
  linear:
    tasks_from:
      project: product-platform
```

Invalid mixed-key shape:

```yaml
tracker:
  linear:
    tasks_from:
      project: product-platform
      any_label: customer-visible
```

That invalid example must be rejected because it hides whether the author meant an implicit `and` or wrote an accidental merge.

## Leaf semantics

### `project`

`project` matches issues in exactly one Linear project slug.

```yaml
tracker:
  linear:
    tasks_from:
      project: product-platform
```

Phase-1 Linear compilation:

    { project: { slugId: { eq: "product-platform" } } }

### `projects`

`projects` matches issues whose project slug is any member of a non-empty list.

```yaml
tracker:
  linear:
    tasks_from:
      projects: [product-platform, customer-success]
```

Phase-1 Linear compilation:

    { project: { slugId: { in: ["product-platform", "customer-success"] } } }

`projects: []` is invalid.

### `all_labels`

`all_labels` matches issues carrying every listed label name. The array must be non-empty. As a complete task scope, it must be combined with a `project` or `projects` anchor.

```yaml
tracker:
  linear:
    tasks_from:
      and:
        - project: product-platform
        - all_labels: [workflow:implementation, backend]
```

Linear compilation for the label leaf uses conjunction over per-label filters, not a single loose membership check:

    {
      and: [
        { labels: { some: { name: { eq: "workflow:implementation" } } } },
        { labels: { some: { name: { eq: "backend" } } } }
      ]
    }

`all_labels: []` is invalid. A root `tasks_from: { all_labels: [...] }` is also invalid in this version because it is an unanchored workspace-wide label scope.

### `any_label`

`any_label` matches issues carrying at least one listed label name. The array must be non-empty. As a complete task scope, it must be combined with a `project` or `projects` anchor.

```yaml
tracker:
  linear:
    tasks_from:
      and:
        - projects: [product-platform, customer-success]
        - any_label: [workflow:implementation, workflow:research]
```

Linear compilation for the label leaf uses disjunction over per-label filters:

    {
      or: [
        { labels: { some: { name: { eq: "workflow:implementation" } } } },
        { labels: { some: { name: { eq: "workflow:research" } } } }
      ]
    }

`any_label: []` is invalid. A root `tasks_from: { any_label: [...] }` is also invalid in this version because it is an unanchored workspace-wide label scope.

## Boolean composition

### `and`

`and` requires every child predicate to match. The array must be non-empty.

```yaml
tracker:
  linear:
    tasks_from:
      and:
        - project: product-platform
        - all_labels: [workflow:implementation, backend]
```

Linear compilation:

    {
      and: [
        { project: { slugId: { eq: "product-platform" } } },
        {
          and: [
            { labels: { some: { name: { eq: "workflow:implementation" } } } },
            { labels: { some: { name: { eq: "backend" } } } }
          ]
        }
      ]
    }

### `or`

`or` requires at least one child predicate to match. The array must be non-empty.

```yaml
tracker:
  linear:
    tasks_from:
      or:
        - project: product-platform
        - and:
            - project: customer-success
            - any_label: [workflow:implementation]
```

Linear compilation:

    {
      or: [
        { project: { slugId: { eq: "product-platform" } } },
        {
          and: [
            { project: { slugId: { eq: "customer-success" } } },
            {
              or: [
                { labels: { some: { name: { eq: "workflow:implementation" } } } }
              ]
            }
          ]
        }
      ]
    }

## Validation rules

The parser rejects unsupported shapes in this phase and future parser/config validation must continue to reject all of the following:

- empty arrays for `projects`, `and`, `or`, `all_labels`, or `any_label`
- mixed keys in one predicate object
- unknown keys
- scalar values where arrays are required
- nested raw GraphQL or arbitrary Linear `IssueFilter` shapes
- unanchored task scopes that could match issues without a `project` or `projects` bound
- predicate fan-out, scalar values, or compiled Linear filters that exceed the safety bounds below

A predicate is anchored when the current build can prove that every possible match is constrained by at least one supported ownership-boundary leaf. In this version, `project` and `projects` are anchoring leaves. An `and` node is anchored if any child is anchored. An `or` node is anchored only if every child is anchored. A future `team` leaf or workspace-wide scope may satisfy this rule only after a later spec revision defines explicit opt-in behavior.

Required safety bounds:

- maximum predicate depth: 4 predicate-map nodes, counted from the `tasks_from` root
- maximum total predicate nodes after parsing: 64
- maximum raw entries in any `projects`, `all_labels`, `any_label`, `and`, or `or` array: 32
- maximum `project`, `projects`, `all_labels`, or `any_label` scalar length: 128 Unicode scalar values
- scalar values must be non-empty UTF-8 strings without control characters; future project-slug parsing may apply stricter slug validation
- duplicate entries in `projects`, `all_labels`, and `any_label` must be collapsed for canonical summaries and compiled filters, but raw array length is still checked before normalization
- maximum unique project slugs referenced across the full predicate: 32
- maximum serialized Linear `IssueFilter` variable payload for the compiled predicate: 16 KiB

Rejected raw GraphQL passthrough example:

```yaml
tracker:
  linear:
    tasks_from:
      filter:
        project:
          slugId:
            eq: product-platform
```

Rejected unknown-key example:

```yaml
tracker:
  linear:
    tasks_from:
      team: CORE
```

Rejected excessive-nesting example:

```yaml
tracker:
  linear:
    tasks_from:
      and:
        - and:
            - and:
                - and:
                    - project: product-platform
```

Rejected unanchored label-only example:

```yaml
tracker:
  linear:
    tasks_from:
      any_label: [workflow:implementation]
```

Rejected partially unanchored `or` example:

```yaml
tracker:
  linear:
    tasks_from:
      or:
        - project: product-platform
        - any_label: [workflow:research]
```

A future implementation may support additional leaves later, but this version of the spec requires current builds to reject unsupported future leaves with a targeted diagnostic instead of silently ignoring them. Current builds accept `and`/`or` only when every complete branch is anchored by `project` or `projects` leaves.

## Compatibility and desugaring

Existing project-based fields remain compatibility aliases only when `tracker.linear.tasks_from` is absent.

Compatibility inputs:

- `tracker.linear.project`
- `tracker.linear.project_slug`
- `tracker.project_slug`

When `tracker.linear.tasks_from` is absent, these existing fields desugar to:

```yaml
tracker:
  linear:
    tasks_from:
      project: <configured-project-slug>
```

`tracker.linear.tasks_from.project` is the preferred public spelling for new single-project configs. `tracker.linear.project`, `tracker.linear.project_slug`, and `tracker.project_slug` remain compatibility aliases.

When `tracker.linear.tasks_from` is present, Scherzo must not silently merge it with any legacy project field. Mixed ownership-boundary config is a compatibility conflict and must be rejected with a message that tells the operator to choose one source of truth.

Example conflict:

```yaml
tracker:
  linear:
    project: product-platform
    tasks_from:
      projects: [product-platform, customer-success]
```

That config is invalid because it combines the legacy single-project field with the new predicate field.

## `doctor` expectations

`doctor`/contract validation treats `tasks_from.project` and `tasks_from.projects` anchors as the task-scope source for project validation, including when label predicates narrow those project scopes. `doctor --check tracker-scope` prints the canonical task-scope summary, reports compatibility desugaring from legacy project fields, and emits static overlap warnings when the supported predicate shape makes overlap with another daemon inferable.

Expected behavior:

- show a canonical summary of the configured task scope
- report when a legacy project field desugars into the canonical predicate
- reject invalid shapes with targeted messages
- reject compatibility conflicts when `tasks_from` and old project fields appear together
- reject unanchored label-only scopes unless a later explicit workspace-wide opt-in exists
- report safety-bound violations with the violated bound and observed count or size
- warn when a predicate uses supported boolean composition in ways likely to overlap another daemon's scope
- warn or fail clearly when a future config uses unsupported leaves not yet implemented by the current build

Illustrative canonical summaries:

    Linear task scope: project(product-platform)
    Linear task scope: and(project(product-platform), all_labels([workflow:implementation, backend]))
    Linear task scope: or(project(product-platform), project(customer-success))

Illustrative conflict diagnostic:

    tracker.linear.tasks_from cannot be combined with tracker.linear.project. Choose one task-scope configuration surface.

Illustrative unanchored-scope diagnostic:

    tracker.linear.tasks_from.any_label would select labels across all projects. Add project/projects bounds or use a future explicit workspace-wide opt-in when available.

Illustrative safety-bound diagnostic:

    tracker.linear.tasks_from exceeds max predicate depth 4. Simplify the predicate or split non-overlapping daemons.

Illustrative unsupported-leaf diagnostic:

    tracker.linear.tasks_from.team is not supported by this Scherzo build. Remove it or upgrade to a build that implements team predicates.

## Linear query-path inventory

One compiled phase-1 project predicate is applied consistently across every Linear read path that decides task ownership.

Required paths are:

1. candidate polling in `src/scherzo/linear.gleam`
2. task-source list/detail reads in `src/scherzo/linear/task_query.gleam`
3. scheduled-failure search in `src/scherzo/scheduled_failure_reporter.gleam`
4. contract validation in `src/scherzo/linear.gleam` and `src/scherzo/linear_contract.gleam`

These paths are tied together by the same compiled `tasks_from` predicate. Updating only candidate polling would be incorrect if task detail, scheduled failure search, or contract validation still used a different scope. For project-only and label-narrowed predicates, contract validation should use the project-filter equivalent of the same predicate for the metadata board while separately checking every configured project slug for existence.

## Operator safety and overlap guidance

The safety invariant is no longer "one daemon per project/root." The future rule is one daemon per non-overlapping Linear task scope/root.

Operators must avoid running two daemons whose `tasks_from` predicates can match the same issue, even if their workspace roots differ. This matters most for `or`, label, and future team-based scopes because overlap becomes harder to see by inspection.

This version intentionally rejects label-only task scopes. Label predicates should narrow an anchored project or projects scope, not silently opt a daemon into every project that happens to use a common workflow label. If a later runtime supports workspace-wide label scopes, that support must be explicit in config, prominent in `doctor`, and included in overlap diagnostics.

Examples of overlap risk:

- `project: product-platform` overlaps `projects: [product-platform, customer-success]`
- `project: product-platform` overlaps `and: [project: product-platform, any_label: [workflow:implementation]]`
- `or: [project: product-platform, project: customer-success]` may overlap either single-project daemon

Current `doctor` checks warn when overlap can be inferred statically from supported shapes and show the canonical task-scope summary so operators can compare daemons.

## Simplified YAML relationship

The simplified YAML spec documents `tracker.linear.tasks_from.project` and `tracker.linear.tasks_from.projects` as the current public task-scope fields. It also records `tracker.linear.project` as compatibility syntax for existing single-project configs.

## Manual review checklist for this spec slice

Manual review of the full predicate spec should confirm coverage for:

- `project`, `projects`, `and`, `or`, `all_labels`, and `any_label`
- compatibility and desugaring from `tracker.linear.project`, `tracker.linear.project_slug`, and `tracker.project_slug`
- invalid shapes
- empty arrays
- mixed keys
- unknown keys
- raw GraphQL passthrough rejection
- excessive nesting rejection
- compatibility conflicts
- anchored label-scope validation
- predicate fan-out, string-size, duplicate-normalization, and compiled-filter limits
- doctor summaries
- unsupported future leaves
- overlap warnings

## Deferred implementation notes

Team predicates, cache-specific behavior, provider-live behavior, and any explicit workspace-wide scope remain intentionally deferred to later specification and implementation work. Future work should keep using this document as the semantic contract for the supported predicate language and doctor/operator diagnostics.