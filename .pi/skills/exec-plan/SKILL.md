---
name: exec-plan
description: >
  Write or implement execution plans (ExecPlans) — thorough, self-contained
  design documents that guide a coding agent through complex features or
  significant refactors. Use this skill when the user asks to "write a plan",
  "create an exec plan", "make an ExecPlan", "plan this feature", "design this
  refactor", "write a design doc for implementation", or when they ask to
  "implement a plan", "execute this plan", "follow the plan", "continue the
  plan", "pick up where the plan left off". Also trigger when the user mentions
  "ExecPlan", "execution plan", or references a PLAN.md or similar planning
  document. This skill covers both authoring plans and executing them.
---

# Execution Plans (ExecPlans)

This skill describes how to author and implement execution plans — thorough, self-contained design documents that a coding agent can follow to deliver a working feature or system change. ExecPlans are living documents: they are written before implementation begins, updated continuously as work proceeds, and serve as the single source of truth throughout the process.

ExecPlans enable complex tasks that take significant time to research, design, and implement. They let the user verify the approach before a long implementation begins, and they let any agent — or human — pick up the work from the plan alone.

A good ExecPlan is not just executable. It is also worth executing. It frames the right problem, chooses a proportionate design, surfaces the main risks, and explains how success and failure will be observed in reality.

## Determining the Mode

Based on the user's request, operate in one of three modes:

- **Authoring**: The user wants to create a new plan. Follow the authoring guidance below to produce a complete ExecPlan.
- **Implementing**: The user wants to execute an existing plan. Follow the implementation guidance below to carry out the work.
- **Discussing**: The user wants to refine, debate, or amend an existing plan without implementing it yet.

---

## Mode 1: Authoring an ExecPlan

When creating an ExecPlan, follow the rules in this skill file _to the letter_. Be thorough in reading (and re-reading) source material to produce an accurate specification. Start from the skeleton and flesh it out as you do your research.

### Non-Negotiable Requirements

Every ExecPlan must satisfy all of the following:

- **Fully self-contained.** The plan contains all knowledge and instructions needed for a novice to succeed. There is no memory of prior plans and no external context. Treat the reader as a complete beginner to the repository: they have only the current working tree and the ExecPlan file.
- **Worth doing.** The plan states the real user or operator problem being solved, not merely the desired code motion or technology choice.
- **Proportionate.** The design fits the problem. Do not prescribe a platform, framework, abstraction, or refactor larger than the outcome requires.
- **Outcome-focused.** The plan must produce a demonstrably working behavior, not merely code changes that "meet a definition."
- **Falsifiable.** The plan must say how to prove itself wrong. If it claims better performance, resilience, safety, or usability, it must specify how that claim will be tested or observed.
- **Safe and reversible.** The plan must account for blast radius, rollout, failure detection, and rollback or containment where relevant.
- **A living document.** Contributors must revise it as progress is made, as discoveries occur, and as design decisions are finalized. Each revision must remain fully self-contained.
- **Novice-enabling.** A complete novice must be able to implement the feature end-to-end without prior knowledge of the repo.
- **Portable.** All file references and paths must be repository-relative, never absolute. The plan may be implemented in any workspace, worktree, or checkout location.
- **Jargon-free.** Define every term of art in plain language, or do not use it.

### Who This Plan Is For

Write every ExecPlan for a developer who is:

- **Skilled at programming** — they can write code, use a terminal, run tests, and commit. You do not need to explain what a function is.
- **Ignorant of your codebase** — they have never seen the repository. They do not know what files exist, what the architecture looks like, or where anything lives. Name every file, every directory, every module they need to touch.
- **Ignorant of your toolchain** — they may not know your build system, test runner, linter, or deployment target. State the exact command to run, the working directory, and the expected output. Not "run the tests" but "from the repo root, run `npm test` and expect all 47 tests to pass."
- **Ignorant of your domain** — they do not know your business logic, your users, or the problem you are solving. Explain the why, not just the what.
- **Mediocre at test design** — they will write tests if told exactly what to test and how, but they will not invent good test cases on their own. Spell out the scenarios, edge cases, and expected behaviors. Do not say "write tests for the parser." Say "write a test that calls `parse("")` and asserts it returns `Err(EmptyInput)`, and a test that calls `parse("hello world")` and asserts it returns `Ok(Greeting("hello world"))`."

If a plan cannot be implemented by this person without asking questions, it is not ready.

### Adversarial Authoring Mindset

Before you lock in an approach, pressure-test it.

Ask and answer these questions inside the plan:

- What concrete user or operator pain does this solve?
- Why is this problem real now, in this repository, rather than hypothetical?
- What is the simplest change that could plausibly solve most of the problem?
- If the plan does something larger than that simpler change, why is the simpler change insufficient?
- What assumptions must be true for this plan to work?
- Which assumption is most likely to be false, and how will the plan test it early?
- What existing behavior, interface, migration, workflow, or downstream dependency could break?
- If implementation stops halfway or the change must be backed out, how does the system stay safe?

If you cannot answer these, keep researching before writing the final plan.

### Writing Principles

**Start with the problem, not the implementation preference.** Begin by explaining, in a few sentences, why the work matters from a user's or operator's perspective: what is failing, painful, slow, risky, or impossible today, and what someone can do after this change that they could not do before. Do not treat "use technology X" or "refactor module Y" as the purpose; those are means, not ends.

**State why this approach is the right size.** If a simpler or lower-risk option exists, mention it and explain why it is insufficient. If the work requires a broad refactor, migration, or new abstraction, justify it in practical terms tied to the problem being solved. Plans should survive skeptical review, not just describe one plausible path.

**Make assumptions explicit.** If the plan depends on a library capability, repository invariant, external service behavior, data shape, performance characteristic, or operator workflow, state that assumption plainly. If the assumption is risky or uncertain, add an early milestone that validates it before deeper implementation begins.

**Self-containment and plain language are paramount.** If you introduce a phrase that is not ordinary English ("daemon", "middleware", "RPC gateway", "filter graph"), define it immediately and remind the reader how it manifests in this repository (for example, by naming the files or commands where it appears). Do not say "as defined previously" or "according to the architecture doc." Include the needed explanation here, even if you repeat yourself.

**Embed all required knowledge.** The agent executing the plan can list files, read files, search, run the project, and run tests. It does not know any prior context and cannot infer what you meant from earlier milestones. Repeat any assumption you rely on. Do not point to external blogs or docs; if knowledge is required, embed it in the plan itself in your own words. If a plan builds upon a prior plan that is checked in, incorporate the necessary context directly.

**Specify repository context explicitly.** Name files with full repository-relative paths, name functions and modules precisely, and describe where new files should be created. **Never use absolute paths.** The plan may be implemented in any workspace, worktree, or checkout location; absolute paths break portability. Every file reference, command example, and configuration value must use paths relative to the repository root. If touching multiple areas, include a short orientation paragraph that explains how those parts fit together so a novice can navigate confidently. When running commands, show the working directory and exact command line. When outcomes depend on environment, state the assumptions and provide alternatives when reasonable.

**Design for small, safe, falsifiable increments.** Prefer milestones that prove the hardest unknowns early, keep the system working at each checkpoint, and can be validated independently. Do not postpone all the risk to the end. If feasibility is uncertain, write a prototype or spike milestone before a broad refactor.

**Anchor the plan with observable outcomes.** State what the user or operator can do after implementation, the commands to run, and the outputs they should see. Acceptance should be phrased as behavior a human can verify ("after starting the server, navigating to http://localhost:8080/health returns HTTP 200 with body OK") rather than internal attributes ("added a HealthCheck struct"). If a change is internal, explain how its impact can still be demonstrated.

**Validation is not optional.** Include instructions to run tests, start the system if applicable, and observe it doing something useful. Describe comprehensive testing for any new features or capabilities. Include expected outputs and error messages so a novice can tell success from failure. If the plan claims better performance, resilience, safety, or usability, say exactly how to prove or disprove that claim.

**Think about rollout and failure before coding starts.** If the work touches an existing interface, stored data, protocol, deployment path, background job, operator workflow, or security boundary, explain how it will be introduced safely. Spell out fallback, rollback, coexistence, migration windows, or containment steps where relevant. Prefer additive and reversible changes when practical.

**Capture evidence.** When steps produce terminal output, short diffs, or logs, include them as indented examples. Keep them concise and focused on what proves success. If you need to include a patch, prefer file-scoped diffs or small excerpts that a reader can recreate by following the instructions rather than pasting large blobs.

**Fact-check the current tree before you freeze the plan.** Re-read every file, script, config, dependency, and export the plan names. Verify file paths, function names, package names, test commands, line numbers, and current behavior against the repository as it exists now. Do not copy stale facts from earlier drafts, prior plans, or memory. If a fact may drift while the plan is being revised, call it out explicitly and explain how the implementer should detect and normalize that drift.

**Make prerequisites first-class.** If the plan depends on prior work, package installs, config changes, generated files, environment variables, or another repository being checked out, say so explicitly near the top of the plan. Do not say "after Plan D" and leave it there. Restate the exact repository facts the implementer needs now: which files must already exist, what interfaces they expose, what commands must work, and what normalization step to take if the repo differs.

**Inventory scope for migrations and extractions.** When the plan moves code across packages, extracts shared modules, or replaces an existing path, account for every affected surface. Name the files, routes, commands, or UI behaviors that will move, stay local, split, or be deferred. If the plan claims a directory will be emptied or a legacy path removed, verify every current file in that area and say where each one ends up. If a user-visible surface survives the migration, specify exactly how parity will be preserved and tested.

**Close every design choice the implementer would otherwise have to make.** Resolve exact module locations, type names, dependency additions, config changes, lifecycle rules, ID generation strategy, conflict policy, retry behavior, and fallback semantics in the plan. Phrases such as "wire up", "plumb through", "choose one", "as needed", or "decide whether" usually mean the plan is still underspecified.

### Milestones

Milestones are narrative, not bureaucracy. If you break the work into milestones, introduce each with a brief paragraph that describes the scope, what will exist at the end of the milestone that did not exist before, the commands to run, and the acceptance you expect to observe. Keep it readable as a story: goal, work, result, proof. Progress and milestones are distinct: milestones tell the story, progress tracks granular work. Both must exist.

Each milestone must be independently verifiable and incrementally implement the overall goal of the plan. Sequence milestones to reduce risk early. If there is a critical unknown, prove or disprove it before asking the implementer to perform a large rewrite or migration.

### Task Granularity

Break every milestone into bite-sized steps. Each step is a single, unambiguous action that takes roughly 2–5 minutes. The developer should never wonder "what do I do next?" or "am I done with this step?"

Where applicable, steps follow the TDD cycle:

1. Write the failing test — one step.
2. Run the test to confirm it fails — one step.
3. Write the minimal code to make the test pass — one step.
4. Run the tests to confirm they pass — one step.
5. Commit — one step.

Each step must name the exact file(s) to touch, with full repository-relative paths. Each step must describe what to add, change, or remove concretely — not "update the handler" but "in `src/handlers/auth.ts`, add a new function `validateToken` that takes a `string` and returns a `Result<Claims, AuthError>`." Steps that produce output (running tests, starting a server, making a request) must include the expected output so the developer can tell success from failure.

As a rule of thumb, any step that touches multiple files, mixes config changes with code changes, or would take more than 5 minutes should be split. Extraction and migration work usually needs explicit substeps for dependency installation, scaffolding, wiring, targeted tests, full-suite validation, and commit. Large steps are one of the most common avoidable causes of plan-review feedback.

No step should require the developer to make a design decision. If a decision is needed, the plan makes it and explains why. No step should bundle multiple unrelated changes. "Add the type and write the test and update the config" is three steps, not one.

### Testing and Falsifiability

Assume the developer will not invent good tests on their own. The plan must specify exactly what to test and how, and it must make it possible to prove the plan wrong.

Every new behavior needs at least one test specified in the plan — not "write tests for this" but "write a test that calls `parse("")` and asserts it returns `Err(EmptyInput)`." Edge cases and negative cases must be called out explicitly. The developer should not have to think about what the edge cases are; enumerate them.

Name test file locations with full repository-relative paths. State the test runner command, including how to run a single test or a subset. Describe expected test output (pass/fail counts, specific assertion messages) so the developer knows what "working" looks like. When a test should fail before implementation (the red phase of TDD), say so and describe what the failure looks like. Include integration or end-to-end validation steps where appropriate — not just unit tests.

For every new or modified test file, specify the fixture or setup data, the exact function, route, or UI path to exercise, the concrete inputs, and the assertion values. For UI tests, name the DOM queries and expected text or attributes. For protocol or transport tests, name the exact messages, close codes, rejection reasons, or lifecycle ordering being verified. For migrations and extractions, add parity tests for every retained user-visible surface — routes, CLI commands, exports, diagnostics, and demo flows — so the plan can catch a half-migrated system that still passes lower-level unit tests.

If the plan makes non-functional claims such as performance, resilience, safety, or security improvement, include a concrete way to falsify those claims. For example, say what baseline to compare against, what command or scenario to run, what regression would count as failure, and how the result will be observed.

### Design Principles

Guide the developer toward clean, maintainable code:

- **YAGNI.** Do not introduce abstractions, interfaces, or features that are not required by the current work. No "we might need this later."
- **Minimal scope.** Each milestone delivers the smallest useful increment. No milestone tries to do everything at once.
- **Reuse before invention.** Prefer existing modules, conventions, and infrastructure when they solve the problem adequately.
- **Additive changes.** Where possible, steps add new code before modifying or removing old code, keeping the system working at every commit point.
- **Reversibility.** When the plan must take a risky path, keep the change easy to back out or isolate.

### Commit Discipline

The plan must produce a clean, readable commit history. Call out commit points explicitly so the developer never has to wonder when to commit. Each commit should represent a logical, self-contained unit of progress — a passing test, a completed refactor step, a new function with its tests. The system must be in a working state (tests pass) at every commit point. Do not instruct the developer to commit broken intermediate states.

For any plan larger than a tiny one-file change, include a short commit map in the plan itself: after which step or milestone the tree should be green, what logical unit lands in that commit, and what validation must pass first.

Where helpful, suggest commit messages or describe what the commit contains clearly enough that writing a good message is obvious.

### Prototyping and Parallel Implementations

It is acceptable — and often encouraged — to include explicit prototyping milestones when they de-risk a larger change. Examples: adding a low-level operator to a dependency to validate feasibility, or exploring two composition orders while measuring optimizer effects. Keep prototypes additive and testable. Clearly label the scope as "prototyping"; describe how to run and observe results; and state the criteria for promoting or discarding the prototype.

Prefer additive code changes followed by subtractions that keep tests passing. Parallel implementations (for example, keeping an adapter alongside an older path during migration) are fine when they reduce risk or enable tests to continue passing during a large migration. Describe how to validate both paths, how traffic or calls move from one path to the other, and how to retire one safely with tests.

When researching a design with challenging requirements or significant unknowns, use milestones to implement proof-of-concept or toy implementations that validate whether the proposal is feasible. Read the source code of libraries by finding or acquiring them, research deeply, and include prototypes to guide a fuller implementation.

### Living Document Sections

Every ExecPlan must contain and maintain all four of the following sections. These are not optional.

1. **Progress** — A checklist summarizing granular steps. Every stopping point must be documented here, even if it requires splitting a partially completed task into two ("done" vs. "remaining"). This section must always reflect the actual current state of the work. Use timestamps to measure rates of progress.

2. **Surprises & Discoveries** — Document unexpected behaviors, bugs, optimizations, or insights discovered during implementation. Provide concise evidence (test output is ideal).

3. **Decision Log** — Record every key design decision with its rationale and date. It should be unambiguously clear why any change to the specification was made.

4. **Outcomes & Retrospective** — Summarize outcomes, gaps, and lessons learned at major milestones or at completion. Compare the result against the original purpose.

### Formatting Rules

Write in plain prose. Prefer sentences over lists. Avoid checklists, tables, and long enumerations unless brevity would obscure meaning. Checklists are permitted only in the Progress section, where they are mandatory. Narrative sections must remain prose-first.

When writing an ExecPlan to a standalone Markdown file (where the file content _is_ the plan), write it as normal Markdown — no wrapping code fence needed. When embedding a plan inside a conversation or another document, wrap the entire plan in a single fenced code block labeled `md`. Do not nest additional triple-backtick code fences inside; use indented blocks instead for commands, transcripts, diffs, or code.

Use two newlines after every heading. Use `#`, `##`, etc. for headings and correct syntax for ordered and unordered lists.

### Skeleton

When creating a new ExecPlan, start from this skeleton and fill in every section:

```
# <Short, action-oriented description>

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

Explain in a few sentences what someone gains after this change and how they can see
it working. State the user-visible or operator-visible behavior you will enable.

## Problem Framing and Constraints

Describe the concrete problem in user or operator terms. State what hurts today, what
this plan will and will not solve, and any constraints that materially shape the
solution.

## Strategy Overview

Describe the chosen approach in prose. Explain why this approach is proportionate to
the problem and how it avoids unnecessary complexity.

## Alternatives Considered

Briefly describe the simplest plausible alternative and any other realistic option that
was rejected. Say why each was insufficient, too risky, or otherwise not chosen.

## Risks and Countermeasures

Name the main ways this plan could fail or cause damage. For each, state how the plan
will detect the problem early, reduce the risk, and recover safely if needed.

## Progress

- [x] (YYYY-MM-DD HH:MMZ) Example completed step.
- [ ] Example incomplete step.
- [ ] Example partially completed step (completed: X; remaining: Y).

## Surprises & Discoveries

- Observation: …
  Evidence: …

## Decision Log

- Decision: …
  Rationale: …
  Date: …

## Outcomes & Retrospective

(To be filled at major milestones and at completion.)

## Context and Orientation

Describe the current state relevant to this task as if the reader knows nothing. Name
the key files and modules by full repository-relative path. Define any non-obvious term you will use. Do
not refer to prior plans.

## Preconditions and Verified Facts

State the repository facts this plan depends on after checking the current tree:
existing files, exported symbols, scripts, package dependencies, config settings,
generated artifacts, and any other repository or environment preconditions. If the
plan depends on earlier work, restate the needed facts here instead of referring to the
earlier plan.

## Scope Boundaries

State what is in scope, what stays unchanged, what moves, what is split, and what is
explicitly deferred. For extractions or migrations, account for every currently
relevant file, route, command, or UI surface so the implementer does not have to infer
the boundary.

## Milestones

Describe each milestone in prose. For each one, say what will exist at the end, how to
observe it working, and why this milestone comes in this order.

## Plan of Work

Describe, in prose, the sequence of edits and additions. For each edit, name the file
and location (function, module) and what to insert or change. Keep it concrete and
minimal. If code is being moved or split, name the source and destination with repository-relative paths, and
what remains behind.

## Concrete Steps

State the exact commands to run and where to run them (working directory). When a
command generates output, show a short expected transcript so the reader can compare.
Include dependency-installation, config, and setup steps before code changes when they
are required. Call out commit points explicitly alongside the step sequence. This
section must be updated as work proceeds.

## Testing and Falsifiability

State exactly what tests to add or modify, where they live, how to run them, what will
fail before implementation, and what will pass after. Name the fixtures, inputs,
assertions, and expected failure or success messages. If the plan is a migration or
extraction, include parity tests for every retained user-visible surface. If the plan
makes non-functional claims, state how those claims will be disproved if they are
false.

## Validation and Acceptance

Describe how to start or exercise the system and what to observe. Phrase acceptance as
behavior, with specific inputs and outputs. If tests are involved, say "run <test
command> and expect <N> passed; the new test <name> fails before the change and passes
after." For migrations, extractions, or replacements, include validation for the
retained routes, commands, exports, diagnostics, or UI flows that prove nothing
important regressed.

## Rollout, Recovery, and Idempotence

If the change affects an existing path, say how it will be introduced safely. Describe
fallback, rollback, coexistence, or migration steps where relevant. If steps can be
repeated safely, say so. If a step is risky, provide a safe retry or rollback path.
Keep the environment clean after completion.

## Artifacts and Notes

Include the most important transcripts, diffs, or snippets as indented examples. Keep
them concise and focused on what proves success.

## Interfaces and Dependencies

Be prescriptive. Name the libraries, modules, services, package installs, config
changes, generated artifacts, and multi-repo or environment assumptions to use and
why. Specify the types, traits/interfaces, and function signatures that must exist at
the end of the milestone. Prefer stable names and paths. E.g.:

In src/planner.ts, define:

    export interface Planner {
      plan(observed: Observed): Action[];
    }
```

---

## Mode 2: Implementing an ExecPlan

When implementing an existing plan, follow these rules:

1. **Read the full plan first.** Before writing any code, read the entire ExecPlan from top to bottom. Understand the purpose, the milestones, the concrete steps, the risks, and the acceptance criteria.

2. **Proceed autonomously.** Do not prompt the user for "next steps"; simply proceed to the next milestone. Resolve ambiguities on your own only when necessary, choose the safest reasonable interpretation, and record your reasoning in the Decision Log.

3. **Keep the plan up to date.** At every stopping point, update the Progress section to affirmatively state what was accomplished and what comes next. Add or split entries as needed. If you discover something unexpected, record it in Surprises & Discoveries. If you make a design choice, record it in the Decision Log.

4. **Validate risky assumptions early.** If the plan includes a spike, prototype, or migration checkpoint, do not skip it. Use it to confirm or disprove the assumptions that justified the plan.

5. **Commit frequently.** Each logical unit of change should be its own commit. Follow the commit points called out in the plan. The system must be in a working state (tests pass) at every commit. Small, focused commits make rebasing and conflict resolution easier.

6. **Validate as you go.** After each milestone, run the validation steps described in the plan. Do not move to the next milestone until the current one passes its acceptance criteria.

7. **Maintain self-containment.** Every revision to the plan must remain fully self-contained. It should always be possible to restart from _only_ the ExecPlan and no other context.

8. **Preserve rollout and recovery guidance.** If discoveries force the rollout, fallback, or migration story to change, update the plan before proceeding further.

9. **Write the retrospective.** At completion of a major task or the full plan, write an Outcomes & Retrospective entry summarizing what was achieved, what remains, what risks were retired, and lessons learned.

---

## Mode 3: Discussing an ExecPlan

When discussing or refining an existing plan without implementing it:

1. **Record all decisions.** Every change to the plan and the thinking behind it must be captured in the Decision Log. It should be unambiguously clear why any change was made.

2. **Maintain the living document.** ExecPlans are living documents, and it should always be possible to restart from _only_ the plan and no other context.

3. **Pressure-test proposed changes.** When revising the plan, ask whether the change solves the right problem, whether a simpler alternative exists, what assumptions it introduces, and how it affects risk, rollout, and validation.

4. **When you revise a plan**, ensure your changes are comprehensively reflected across all sections — including risks, validation, rollout, and the living document sections — and write a note at the bottom of the plan describing the change and the reason why. ExecPlans must describe not just the _what_ but the _why_ for almost everything.

---

## Handling Ambiguity

If the source material is ambiguous or incomplete:

- Write the plan with your best interpretation.
- Mark the ambiguity with a `[CLARIFY]` tag and a note explaining what needs stakeholder input.
- Offer alternatives where the interpretation could go either way.
- Do not hide uncertainty. Make the boundary between facts, assumptions, and open questions explicit.

## The Bar

If you follow the guidance above, a single, stateless agent — or a human novice — can read the ExecPlan from top to bottom and produce a working, observable result. More importantly, they can do so while understanding why this plan is the right size, how to detect when it is going wrong, and how to recover safely. That is the bar: **self-contained, well-justified, risk-aware, falsifiable, and outcome-focused.**
