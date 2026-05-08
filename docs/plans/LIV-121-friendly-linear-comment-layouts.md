# Make Scherzo Linear comments friendly and scannable

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

Scherzo already tells Linear users what happened, but many of its comments read like internal daemon logs. After this change, an operator or issue owner should be able to open any Scherzo-published Linear comment and immediately answer: what happened, whether action is needed, what changed or failed, where artifacts are, what to do next, and which run or command produced the comment. The visible behavior is a consistent family of concise, friendly Linear Markdown comments with the human answer at the top and noisy run metadata at the bottom.

The implementation this plan describes does not change when Scherzo posts comments, who can issue commands, Linear state transitions, retry behavior, attachment upload behavior, or workflow dispatch policy. It only changes the Markdown bodies Scherzo writes into Linear comments and the tests that assert those bodies.

## Problem Framing and Constraints

The current comments are technically useful but force humans to parse terse internal strings. For example, a claim comment currently says only `Scherzo claimed LIV-38 for run LIV-38--576460151305-2.` A success comment starts with `Scherzo completed run ...` and then places the agent result and token metadata at the same visual priority. Some failure comments in older real issues are a single line such as `Scherzo failed run ... with error agent_pi_failed.` Newer failure formatting is more structured, but it still exposes labels such as `underlying_error`, `failure_code`, and `retained_workspace` before it explains the operator action. Command acknowledgements for `/scherzo` commands are similarly raw: they list `Command`, `Status`, `Target`, `Message`, and `Excerpt` without a friendly status headline.

The constraints are important. Comments must remain deterministic so Gleam tests can assert exact strings. Existing redaction, terminal-control-character sanitization, bounded diagnostics, and absolute-local-path hiding must continue to work. Comments must render well in Linear Markdown and remain readable if Linear strips some styling. The implementation must avoid new dependencies unless a later implementer proves one is necessary; this plan does not require any.

## Strategy Overview

Use a small shared formatting module for Linear comment house style and safe Markdown rendering, then route every Scherzo-owned comment body through that style. The style uses a strong first line, a compact summary table for the most important fields, short sections such as **Summary**, **What Scherzo did**, **Next action**, **Artifacts**, **Failure diagnostics**, **Token usage**, and **Run details**, and restrained emoji that supplements words instead of replacing them.

The right-sized implementation is not a broad workflow refactor. It is a formatting extraction plus targeted call-site updates. `src/scherzo/handoff_format.gleam` already owns most handoff formatting and should remain the domain-specific home for success, failure, park, and the new claim comment function. A new shared module, `src/scherzo/linear_comment_format.gleam`, should provide reusable low-level Markdown helpers, mandatory escaping, final-body safety helpers, and status language so handoff, workflow-label triage, and command acknowledgements do not each invent their own layout. `src/scherzo/workflow_policy.gleam` and `src/scherzo/control/linear_transport.gleam` should keep their policy and command decisions, but use the shared format helpers for the final comment body.

Success comments need one small API adjustment because attachment intent affects truthful wording. Instead of letting `success_comment` infer from only `include_result`, pass a `SuccessCommentOptions` value that contains `include_result` and `attachment_filename: Option(String)`. `attachment_filename` is `Some(filename)` only when attachment mode is enabled and a captured final response exists; it is `None` when attachment is disabled or no result body exists. The filename must come from one shared formatter function so the comment and the later attachment upload cannot drift.

## Alternatives Considered

The simplest alternative is to edit each current string in place. That would improve a few comments quickly, but it would leave separate house styles in `src/scherzo/handoff_format.gleam`, `src/scherzo/workflow_policy.gleam`, and `src/scherzo/control/linear_transport.gleam`; the next new status would likely drift back into log-like wording.

A larger alternative is to introduce a full comment-template engine or external Markdown renderer. That is disproportionate. The comments are deterministic strings assembled from existing data, and Gleam string helpers plus focused tests are enough.

A configuration-gated rollout was also considered. This plan rejects adding a new config flag because existing comment enable and disable semantics must remain unchanged, and formatting-only changes are easy to revert as a commit if they prove confusing. The implementation should be additive internally, but the user-visible switch is still the existing Scherzo version deployed by operators.

## Risks and Countermeasures

The first risk is that Linear Markdown may not render a construct the way GitHub-flavored Markdown does. A dry-run through Linear proved that Markdown tables are accepted by Linear's API and converted into rich `table` nodes. It did not prove that HTML `<details>` sections become reliable collapsible disclosures in the browser. The countermeasure is to rely on tables, headings, bullets, links, code spans, and indented code blocks; do not rely on HTML disclosure sections. Browser inspection of final candidate comments is required before rollout. If browser access is unavailable during implementation, stop before rollout and record a clarification item in this plan instead of treating API `bodyData` evidence as sufficient.

The second risk is that a success comment could claim a result file is attached even when attachment mode is disabled, no final response was captured, upload validation fails, native attachment fails, or fallback linking is used. The countermeasure is to pass explicit attachment intent into `handoff_format.success_comment` through `SuccessCommentOptions`, compute the attachment filename from one shared function, omit `## Artifacts` when there is no attachment intent, and use neutral wording such as “Scherzo will attempt to add...” rather than “attached file” because the comment is created before the upload is attempted.

The third risk is losing safety behavior while making comments prettier. The countermeasure is to keep redaction, terminal-control-character stripping, Markdown escaping, truncation, and absolute-local-path hiding as named formatter boundaries on every changed surface, not only on handoff comments. Handoff, workflow-label triage, and command acknowledgement formatters must all use the shared safe table, code-span, inline-text, indented-block, and final-body helpers.

The fourth risk is making comments too long. The countermeasure is to keep the top summary short, keep long agent results in the success-result attachment when `attach_result_on_success` is enabled, retain bounded diagnostic detail, bound command parse-error and acknowledgement text, and place token usage and run details at the bottom.

The fifth risk is behavior drift outside formatting. The countermeasure is to keep call ordering, state updates, upload flow, dedupe behavior, command parsing, authorization, and acknowledgement enablement unchanged. Tests in `test/handoff_test.gleam`, `test/linear_command_transport_test.gleam`, `test/linear_triage_test.gleam`, and `test/linear_attachment_test.gleam` should still assert the same transport actions and mutation order.

## Progress

- [x] (2026-05-07 00:00Z) Read the ExecPlan authoring skill and wrote this plan from the current ticket and repository facts.
- [x] (2026-05-07 00:00Z) Inventoried the Scherzo-owned Linear comment surfaces in the current tree.
- [x] (2026-05-07 00:00Z) Collected real comments from the `[test] scherzo` Linear project for claim, success, failure, attachment-link fallback, and `/scherzo` command acknowledgement examples.
- [x] (2026-05-07 00:00Z) Performed a temporary Linear dry-run on a `[test] scherzo` issue and deleted the dry-run comment after collecting API evidence.
- [x] (2026-05-07 00:00Z) Incorporated adversarial review findings about success attachment truthfulness, mandatory Markdown safety boundaries, artifact outcome tests, smaller TDD slices, and required browser validation.
- [x] (2026-05-08 00:00Z) Implemented `src/scherzo/linear_comment_format.gleam` with deterministic title, section, table, code-span, token table, indented-block, optional-row, and final cleanup helpers plus adversarial formatter tests.
- [x] (2026-05-08 00:00Z) Updated handoff comments for claim, success without attachment, success with attachment intent, success-result attachment Markdown, generic failure, workflow-command failure, and park while preserving redaction, truncation, token reporting, and workspace path hiding.
- [x] (2026-05-08 00:00Z) Updated workflow-label triage comments to include issue-aware friendly missing-label, multiple-label, and unknown-label layouts without changing classification or state update behavior.
- [x] (2026-05-08 00:00Z) Updated `/scherzo` command acknowledgement comments for applied, queued, rejected, not-found, not-allowed, completed durable receipt, and unknown-after-restart statuses while preserving parsing, authorization, duplicate processing, and acknowledgement enablement behavior.
- [x] (2026-05-08 00:00Z) Updated success artifact intent tests and preserved attachment upload, native attachment, fallback link append, dedupe, validation, and failure behavior.
- [x] (2026-05-08 00:00Z) Ran repository validation: `direnv exec . gleam test`, `direnv exec . gleam format --check src test`, and `direnv exec . gleam run -m glinter` all completed without production policy errors.
- [x] (2026-05-08 15:27Z) Completed required Linear browser visual validation before rollout. Four representative `[DRY RUN — LIV-121]` comments were posted to `LIV-12` in the `[test] scherzo` project, inspected in the Linear browser UI, accepted by the operator as readable/scannable, and queried for `bodyData` evidence.

## Surprises & Discoveries

- Observation: Linear's API accepted a dry-run Markdown table and stored it as a rich `table` node in `bodyData`.
  Evidence: temporary dry-run comment `049ffe91-0831-4b85-8079-704972d0fd9a` on `LIV-12` had `BODY_HAS_TABLE_MARKDOWN=True`, `BODYDATA_HAS_TABLE_NODE=True`, and URL `https://linear.app/living-systems/issue/LIV-12/create-a-plan-to-address-naming-issues#comment-049ffe91` before deletion.
- Observation: The same API dry-run did not conclusively prove browser-safe collapsible disclosure sections.
  Evidence: the dry-run body contained `<details>` and `bodyData` contained the literal string `details`, but no browser screenshot was captured. Treat HTML disclosure as unverified and use non-collapsible fallback sections until a later visual check confirms it.
- Observation: The suggested `[test] scherzo` project query returned real examples for success, claim, failure, command acknowledgement, and attachment-link fallback. It did not return a real invalid-workflow or park comment in the inspected issue set.
  Evidence: queried project id `1c441d1b-40cd-4f7a-b8cd-5f200848d2aa` and inspected returned issues including `LIV-38`, `LIV-35`, `LIV-33`, `LIV-32`, `LIV-31`, `LIV-29`, `LIV-11`, and `LIV-10`.
- Observation: Implementation did not need any change to `src/scherzo/linear_attachment.gleam`; the success comment's neutral `## Artifacts` section made the existing native and fallback attachment paths understandable without changing upload or dedupe behavior.
  Evidence: `direnv exec . gleam test` passed with existing native attachment, fallback Markdown link, dedupe, upload failure, extension validation, and size-limit tests still exercising the same transport actions.
- Observation: Browser visual validation initially remained a rollout gate because the implementation coding harness did not provide Linear browser inspection.
  Evidence: the implementation stopped before rollout and recorded the missing browser check rather than claiming acceptance from deterministic tests alone.
- Observation: Post-implementation browser validation confirmed the final candidate layouts are readable in Linear's browser UI.
  Evidence: four `[DRY RUN — LIV-121]` comments were posted to `LIV-12` in the `[test] scherzo` project for success-with-attachment-intent (`484e6531-6ca2-42c6-adce-499bf61ff864`), workflow-command failure (`ebace243-0564-473d-ab60-fed0e185c29b`), invalid-workflow (`418ea577-dfb3-4d9a-90b5-46759844efd5`), and command acknowledgement (`f1f60d29-cbaf-4297-949e-a84032f208f2`). The operator inspected them in Linear's browser UI and reported that they looked good. A Linear API query confirmed each comment's `bodyData` contains rich `table` nodes, headings, code marks, and list/code-block structures as applicable.

## Decision Log

- Decision: Add `src/scherzo/linear_comment_format.gleam` for shared Markdown primitives and status language, while keeping handoff-specific data gathering in `src/scherzo/handoff_format.gleam`.
  Rationale: The current comment surfaces span handoff, workflow policy, command transport, and attachment fallback. A small shared formatter prevents style drift without redesigning Scherzo's workflow engine.
  Date: 2026-05-07
- Decision: Use Markdown tables for compact key/value summaries, but do not depend on collapsible HTML disclosure sections for the first implementation.
  Rationale: Linear API dry-run evidence showed table support through `bodyData` table nodes. Disclosure behavior still needs browser verification, so readable headings are safer.
  Date: 2026-05-07
- Decision: Preserve existing comment configuration, transport ordering, attachment behavior, and command semantics.
  Rationale: The ticket is a presentation improvement. Changing posting rules, permissions, retries, or states would increase blast radius and violate the functional requirements.
  Date: 2026-05-07
- Decision: Change the success comment formatter from a bare `include_result` flag to `SuccessCommentOptions(include_result, attachment_filename)` and expose one shared attachment filename helper.
  Rationale: The comment is created before attachment upload, and the old signature cannot distinguish disabled attachment, no captured result, attempted attachment, fallback link, or upload failure. Explicit intent plus neutral wording keeps artifact language truthful.
  Date: 2026-05-07
- Decision: Make Markdown escaping and final-body safety mandatory shared formatter responsibilities.
  Rationale: Tables and code spans are more fragile than plain lines. Central helpers prevent pipes, newlines, backticks, terminal controls, secrets, and adversarial command excerpts from breaking out of the intended layout.
  Date: 2026-05-07
- Decision: Require browser visual validation for rollout, with API `bodyData` checks as supporting evidence only.
  Rationale: The user value is human readability in Linear's browser UI. API evidence can prove parsing, but it cannot prove the final comments are scannable to operators.
  Date: 2026-05-07
- Decision: Leave `src/scherzo/linear_attachment.gleam`'s fallback Markdown-link append behavior unchanged and make the success comment anticipate fallback linking instead.
  Rationale: The plan's UX problem was the comment body, not upload mechanics. Keeping fallback behavior unchanged preserves native attachment, dedupe, validation, and failure-order guarantees while the new neutral artifact text explains where the link will appear if fallback is used.
  Date: 2026-05-08
- Decision: Record Linear browser dry-run validation as remaining work rather than claiming rollout acceptance from unit tests or API evidence.
  Rationale: The implementation environment could run deterministic repository validation but did not provide browser inspection. The original risk still requires a human-visible Linear check before rollout.
  Date: 2026-05-08

## Outcomes & Retrospective

Implementation produced a shared Linear comment formatting module and routed Scherzo-owned handoff, workflow-label triage, and `/scherzo` command acknowledgement comments through it. Operators should now see concise status headlines, compact summary tables, human summary and next-action sections, lower-priority diagnostics and token usage, and neutral artifact wording that does not claim an upload has already succeeded.

The main safety outcomes were preserved: secrets are redacted at final body boundaries, terminal control characters are rendered visibly rather than surviving raw, table pipes and backticks are escaped deterministically, long diagnostics and command messages remain bounded, forbidden absolute local workspace paths remain hidden, command parsing and authorization behavior did not change, and attachment upload/fallback ordering stayed intact. Validation passed with `direnv exec . gleam test`, `direnv exec . gleam format --check src test`, and `direnv exec . gleam run -m glinter`; glinter reported the existing warning inventory and no production policy errors.

The previously remaining browser-validation gap is now resolved. Representative `[DRY RUN — LIV-121]` comments were posted to `LIV-12` in the `[test] scherzo` project, inspected in Linear's browser UI, queried for `bodyData`, and accepted by the operator as readable/scannable. The temporary comments were retained as review evidence for the manual workflow finish.

## Context and Orientation

Scherzo is a daemon that watches Linear issues, dispatches workflow runs, and publishes Linear comments about what it did. In this plan, a "handoff" comment means a comment written when Scherzo claims an issue, completes a worker run, fails a worker run, or parks an issue. A "park" comment means Scherzo intentionally stopped retrying an issue until an unpark or retry condition. A "workflow-label triage" comment means Scherzo refused to dispatch an issue because it did not have exactly one allowed `workflow:*` label. A "command acknowledgement" means a comment Scherzo posts in response to a Linear user writing a `/scherzo` command.

The current formatting code is concentrated in these files:

- `src/scherzo/handoff.gleam` wires handoff behavior to Linear. It currently builds the claim comment inline in `claim_issue`, calls `handoff_format.success_comment`, calls `handoff_format.failure_comment`, calls `handoff_format.park_comment`, and invokes attachment upload for success-result Markdown.
- `src/scherzo/handoff_format.gleam` builds success comments, success-result attachment Markdown, failure comments, park comments, failure diagnostics, token totals, redaction, sanitization, diagnostic truncation, and safe workspace path display.
- `src/scherzo/workflow_policy.gleam` classifies workflow labels and builds invalid-workflow messages with `violation_message`.
- `src/scherzo/linear_triage.gleam` posts the invalid-workflow comment returned by `workflow_policy.violation_message` and optionally updates issue state.
- `src/scherzo/control/linear_transport.gleam` parses Linear comments for `/scherzo` commands and builds acknowledgement bodies with `common_ack_body`, `result_ack_body`, `completed_receipt_ack_body`, `unknown_after_restart_ack_body`, `unauthorized_ack_body`, and `parse_error_ack_body`.
- `src/scherzo/orchestrator/daemon.gleam` receives `linear_transport.PostAck` actions and enqueues command acknowledgement comments. This call path should not change except for the body string produced upstream.
- `src/scherzo/linear_attachment.gleam` uploads success-result Markdown, updates Linear comment `bodyData` for native file attachment, and appends a Markdown link to the comment body as fallback when native attachment is not possible.
- `src/scherzo/linear_body_data.gleam` provides the Markdown link string used by attachment fallback.
- `src/scherzo/linear.gleam` contains generic Linear GraphQL request builders such as `build_comment_create_request`, `build_comment_update_body_data_request`, and `build_comment_update_body_request`. It is not itself a Scherzo-owned comment-body surface; callers provide the body.

The relevant tests currently include:

- `test/handoff_format_test.gleam` for success, attachment Markdown, park comments, redaction, truncation, and missing result text.
- `test/handoff_test.gleam` for handoff mutation behavior, failure diagnostics, safe workspace paths, retained workspace guidance, attachment ordering, and disabled handoff.
- `test/linear_command_transport_test.gleam` for command parsing actions and acknowledgement body variants such as queued, applied, not found, unauthorized/not allowed, malformed commands, durable receipts, and unknown-after-restart receipts.
- `test/linear_triage_test.gleam` for invalid-workflow comment and state reporting behavior.
- `test/linear_attachment_test.gleam` for native attachment, fallback Markdown link updates, dedupe, upload failures, invalid body data, extension validation, and size limits.
- `test/linear_attachment_graphql_test.gleam`, `test/linear_body_data_test.gleam`, and `test/linear_comments_test.gleam` for lower-level Linear request and body-data behavior.

## Preconditions and Verified Facts

The repository uses Gleam. From the repository root, validation should run through direnv:

    direnv exec . gleam test
    direnv exec . gleam format --check src test
    direnv exec . gleam run -m glinter

The working tree was clean before this plan was written according to:

    jj status --color=never

The current claim body in `src/scherzo/handoff.gleam` is assembled inline as:

    Scherzo claimed LIV-38 for run LIV-38--576460151305-2.

The current success body is assembled in `src/scherzo/handoff_format.gleam` with a header, an optional `Result:` section, and a `Metadata:` section. A real `[test] scherzo` example from `LIV-38` is:

    Scherzo completed run LIV-38--576460151305-2 for LIV-38.

    Result:
    Workflow step summary:
    research success agent

    Metadata:
    - classification: terminal
    - turns: 1
    - tokens: input=18139 output=834 cache_read=2560 cache_write=0 total=21533

A real attachment-link fallback example from `LIV-33` shows the comment ending with a plain Markdown link after metadata:

    [liv-33-liv-33-576460751484-1-result.md](https://uploads.linear.app/c0c4f102-eb26-4308-95a6-eab20918bc8f/4debeb09-9cbb-4b05-b7b5-bfb36ccd7341/5002e117-d6c1-4ff6-b075-223ff12d0adb)

Real older failure examples from `LIV-32` and `LIV-31` are terse:

    Scherzo failed run LIV-32--576460750994-1 for LIV-32 with error agent_pi_failed.

Current source now has richer failure diagnostics in `src/scherzo/handoff_format.gleam`, including error code, optional workflow step, optional retained workspace status, safe workspace display, suggested next action, underlying error code, bounded detail, and token totals.

A real `/scherzo` command acknowledgement from `LIV-10` is:

    Scherzo command received from comment 599ae42f-ed59-40aa-a814-8354b41fe2e8.
    Command: prompt
    Status: queued
    Target: LIV-10--576460751309-1
    Message: prompt queued for next turn
    Excerpt: hello from Linear smoke

A real applied acknowledgement from `LIV-10` is:

    Scherzo command received from comment e6f8d0f9-e2cb-424e-bd76-b9084b29f20a.
    Command: abort
    Status: applied
    Target: LIV-10--576460751309-1
    Message: abort requested

The Linear Markdown dry-run used a temporary comment on `LIV-12` in the `[test] scherzo` project, prefixed with `[DRY RUN — LIV-121]`, then deleted it. The API evidence showed table support through rich `bodyData`, but did not provide a browser screenshot for collapsible disclosure. The safe implementation fallback is therefore: use tables and regular sections, not required collapsible sections.

## Scope Boundaries

In scope:

- Handoff claim comments produced by `src/scherzo/handoff.gleam`.
- Handoff success comments and success-result attachment Markdown produced by `src/scherzo/handoff_format.gleam` and attached through `src/scherzo/handoff.gleam`.
- Handoff failure comments, including failure diagnostics, retained workspace guidance, workflow command failure details, suggested next actions, path hiding, and token usage.
- Park comments produced by `src/scherzo/handoff_format.gleam`.
- Invalid workflow-label triage comments produced by `src/scherzo/workflow_policy.gleam` and posted through `src/scherzo/linear_triage.gleam`.
- `/scherzo` Linear command acknowledgement comments produced by `src/scherzo/control/linear_transport.gleam`, including applied, queued, rejected, not found, not allowed, parse errors, unauthorized users, completed durable receipts, and unknown-after-restart receipts.
- Attachment-link fallback text and comment-update behavior in `src/scherzo/linear_attachment.gleam` insofar as it affects the final rendered success comment.

Out of scope:

- Changing the Scherzo workflow engine, run scheduling, retry policy, parking policy, or state transitions.
- Changing who can issue `/scherzo` commands or how commands are parsed.
- Changing whether comments are posted when existing configuration disables them.
- Adding a web UI, screenshot renderer, or new dependency.
- Changing Linear labels, states, projects, or production issues except for temporary dry-run comments in the `[test] scherzo` project during validation.

No other Scherzo-owned comment-body construction path was found in the inspected source beyond the generic GraphQL helpers in `src/scherzo/linear.gleam`, which do not own body text.

## House Style Specification

Every Scherzo-published Linear comment should follow these rules unless a specific surface below says otherwise.

The first line is a strong status title. It includes a tasteful icon where helpful and clear words that stand on their own without the icon. Examples are `✅ Scherzo completed the run`, `🛠️ Scherzo claimed this issue`, `⚠️ Scherzo run needs attention`, `⏸️ Scherzo parked this issue`, `✅ Scherzo command applied`, `⏳ Scherzo command queued`, and `🚫 Scherzo command rejected`.

Immediately after the title, include a compact summary table for the highest-value facts. Use two columns named `Field` and `Value`. Values that are identifiers, command names, statuses, step ids, paths, or error codes should be wrapped in code spans. Keep the top table to roughly four to six rows.

Use short sections after the table. Preferred section names are `## Summary`, `## What Scherzo did`, `## Next action`, `## Artifacts`, `## Failure diagnostics`, `## Token usage`, and `## Run details`. Raw diagnostics and token counts belong below the human summary and next action.

Do not rely on emoji alone for meaning. Do not lead with raw internal names such as `agent_pi_failed` unless the comment is in a diagnostics section. Do not include absolute local paths in rendered comments. Keep raw command output in an indented code block or an equivalent monospaced block. Keep comments readable if Linear changes table styling by writing labels and values plainly.

## Safety and Markdown Rendering Requirements

The shared formatter is a safety boundary, not just a string-concatenation helper. Every value inserted into a table cell, code span, inline sentence, bullet, heading, or indented block must pass through a helper whose behavior is deterministic and tested. Callers should not concatenate untrusted Linear text, configuration labels, command excerpts, error details, paths, issue identifiers, run ids, or filenames directly into Markdown.

`src/scherzo/linear_comment_format.gleam` must provide helpers for safe inline text, safe table text, safe table code values, safe code spans, indented blocks, optional rows, token tables, and final body cleanup. Safe table cells must collapse newlines and tabs to spaces, strip terminal control characters, escape table separators such as `|`, trim surrounding whitespace, and use a deterministic fallback such as `_not provided_` for empty values. Safe code spans must handle backticks predictably: use a single-backtick span when possible, and when the value contains backticks, choose a delimiter one backtick longer than the longest backtick run and pad inside the delimiter if needed. Values rendered inside table code cells must still escape `|` so a code span cannot split the table.

Every public Scherzo-owned comment formatter must apply a final safety boundary before returning the body. The final boundary strips terminal control characters from the whole body, normalizes line endings, redacts configured secrets when a secrets list is available, and trims accidental trailing whitespace without changing intentional blank lines between sections. Handoff comments pass tracker secrets into this boundary. Command acknowledgement comments must redact and truncate result messages, command excerpts, parse-error command names, parse-error invalid values, and durable receipt excerpts before rendering, then apply final cleanup. Workflow-label comments have no tracker secrets in their current API, but they must still sanitize and Markdown-escape label names and ready-state names and call final cleanup with an empty secrets list.

The implementation must preserve the existing absolute-local-path hiding behavior for handoff failure diagnostics. A rendered comment must never expose forbidden absolute local paths; it should show the existing replacement sentence `_not shown because Scherzo recorded an absolute path outside the repository_` when that case is detected.

## Proposed Comment Templates

The templates below use placeholders such as `{issue}`, `{run}`, and `{message}`. Implementation tests should assert exact output after substituting concrete fixture values.

### Handoff claim

Current pattern:

    Scherzo claimed {issue} for run {run}.

New body:

    🛠️ Scherzo claimed this issue

    | Field | Value |
    | --- | --- |
    | Issue | `{issue}` |
    | Run | `{run}` |
    | Status | `claimed` |

    ## Summary
    Scherzo is starting work on `{issue}`.

    ## Next action
    No action is needed right now. Scherzo will post another update when the run finishes, fails, or parks.

### Handoff success comment with inline result enabled

Current pattern:

    Scherzo completed run {run} for {issue}.

    Result:
    {result}

    Metadata:
    - classification: {classification}
    - turns: {turns}
    - tokens: input={input} output={output} cache_read={cache_read} cache_write={cache_write} total={total}

New body when `include_result_on_success` is true and no separate attachment is used:

    ✅ Scherzo completed the run

    | Field | Value |
    | --- | --- |
    | Issue | `{issue}` |
    | Run | `{run}` |
    | Status | `completed` |
    | Classification | `{classification}` |

    ## Summary
    Scherzo finished the run for `{issue}`.

    ## What Scherzo did
    {result text, or `_No assistant result text was captured._`}

    {optional `_Result truncated by Scherzo._` note when the result artifact is truncated}

    ## Run details
    - Turns: {turns}
    - Result source: `{result_source}`

    ## Token usage
    | Kind | Tokens |
    | --- | ---: |
    | Input | {input} |
    | Output | {output} |
    | Cache read | {cache_read} |
    | Cache write | {cache_write} |
    | Total | {total} |

The implementation should include `Result source` only if the current `result_artifact.ResultArtifact.source` is already available in the `WorkerSuccess`; it is available in tests and should be treated as quiet run detail.

### Handoff success comment when attachment will be attempted

When `attach_result_on_success` is true, compact comments are the priority. If `include_result_on_success` is false, do not inline the agent result in the comment. The success comment should include `## Artifacts` only when `SuccessCommentOptions.attachment_filename` is `Some(filename)`, which means attachment mode is enabled and a captured final response exists. Because the comment is created before attachment upload, the wording must be neutral and truthful whether native attachment succeeds, a fallback Markdown link is appended, or upload later fails.

New body when `attachment_filename` is `Some(safe_filename)`:

    ✅ Scherzo completed the run

    | Field | Value |
    | --- | --- |
    | Issue | `{issue}` |
    | Run | `{run}` |
    | Status | `completed` |
    | Classification | `{classification}` |

    ## Summary
    Scherzo finished the run for `{issue}`.

    ## Artifacts
    - Full result: Scherzo will attempt to add `{safe-filename}` to this comment. If fallback linking is used, a Markdown link appears below.

    ## Run details
    - Turns: {turns}
    - Result source: `{result_source}`

    ## Token usage
    | Kind | Tokens |
    | --- | ---: |
    | Input | {input} |
    | Output | {output} |
    | Cache read | {cache_read} |
    | Cache write | {cache_write} |
    | Total | {total} |

If `include_result_on_success` is true while attachment is also enabled, include the `## What Scherzo did` section before `## Artifacts`, but keep the neutral artifact line so operators know where Scherzo attempted to place the full result. If attachment is disabled or `success.result.final_response` is `None`, `attachment_filename` must be `None` and the comment must omit `## Artifacts` rather than promising an artifact.

### Success-result attachment Markdown

Current attachment Markdown starts with:

    # Scherzo result for {issue} run {run}

New attachment Markdown:

    # Scherzo result for `{issue}`

    | Field | Value |
    | --- | --- |
    | Issue | `{issue}` |
    | Run | `{run}` |
    | Classification | `{classification}` |

    ## Result
    {full result text}

    {optional `_Result truncated by Scherzo._` note}

    ## Run details
    - Turns: {turns}
    - Result source: `{result_source}`

    ## Token usage
    | Kind | Tokens |
    | --- | ---: |
    | Input | {input} |
    | Output | {output} |
    | Cache read | {cache_read} |
    | Cache write | {cache_write} |
    | Total | {total} |

### Handoff failure comment for a generic agent or hook failure

Current source-level pattern:

    Scherzo failed run {run} for {issue}.

    Failure diagnostics:
    - error: {agent_error}
    - underlying_error: {underlying_error}
    - detail: {detail}
    - tokens: input={input} output={output} cache_read={cache_read} cache_write={cache_write} total={total}

New body:

    ⚠️ Scherzo run needs attention

    | Field | Value |
    | --- | --- |
    | Issue | `{issue}` |
    | Run | `{run}` |
    | Status | `failed` |
    | Error | `{friendly_error}` |

    ## Summary
    Scherzo stopped before completing this run.

    ## Next action
    Inspect the failure diagnostics below, fix the underlying issue, then retry when safe.

    ## Failure diagnostics
    | Field | Value |
    | --- | --- |
    | Error code | `{agent_error}` |
    | Underlying error | `{underlying_error}` |
    | Workspace | `{safe_workspace_path}` |

        {bounded sanitized detail, if present}

    ## Token usage
    | Kind | Tokens |
    | --- | ---: |
    | Input | {input} |
    | Output | {output} |
    | Cache read | {cache_read} |
    | Cache write | {cache_write} |
    | Total | {total} |

Use a friendly top-level error phrase such as `Pi process failed`, `Hook failed`, `Workflow hook failed`, `Tracker refresh failed`, or `Operator stopped the run` when the reason can be mapped safely. Keep the raw `agent_*` and nested codes in `## Failure diagnostics`.

### Handoff failure comment for workflow command failures

Workflow command failures already have better next-action knowledge. Preserve that specificity and keep retained workspace context visible but below the summary.

New body:

    ⚠️ Scherzo workflow step needs attention

    | Field | Value |
    | --- | --- |
    | Issue | `{issue}` |
    | Run | `{run}` |
    | Step | `{step_id}` |
    | Error | `{failure_code}` |

    ## Summary
    Scherzo stopped during `{step_id}` because `{failure_code}` occurred.

    ## Next action
    {suggested next action from the existing `suggested_next_action` mapping}

    ## Failure diagnostics
    | Field | Value |
    | --- | --- |
    | Failure code | `{failure_code}` |
    | Retained workspace | `{yes | not_detected | unknown}` |
    | Workspace | `{safe_workspace_path}` |

        {bounded sanitized workflow command detail with absolute workspace path replaced}

    ## Token usage
    | Kind | Tokens |
    | --- | ---: |
    | Input | {input} |
    | Output | {output} |
    | Cache read | {cache_read} |
    | Cache write | {cache_write} |
    | Total | {total} |

### Park comment

Current pattern:

    Scherzo parked {issue}.

    Reason: {reason}
    Release policy: {release_policy}
    Run id: {run}
    Next action: inspect recent Scherzo/Linear failure details, then run `scherzoctl unpark {issue}` or retry when safe.

New body:

    ⏸️ Scherzo parked this issue

    | Field | Value |
    | --- | --- |
    | Issue | `{issue}` |
    | Status | `parked` |
    | Reason | {reason} |
    | Release policy | `{release_policy}` |
    | Run | `{run}` |

    ## Summary
    Scherzo paused automated work on `{issue}` so it does not keep retrying an unsafe or blocked run.

    ## Next action
    Inspect the recent Scherzo and Linear failure details. When the issue is safe to run again, use `scherzoctl unpark {issue}` or retry the workflow.

Omit the `Release policy` row or `Run` row when the current values are absent. Keep the reason sanitized, single-line, redacted, and truncated as it is today.

### Invalid workflow-label triage comments

Current missing-label pattern:

    Scherzo did not dispatch this issue because it has no workflow label.

    Expected exactly one of:
    - workflow:bugfix
    - workflow:feature

    Add exactly one workflow label, then move the issue back to Ready for Agent.

New missing-label body:

    🏷️ Scherzo needs one workflow label

    | Field | Value |
    | --- | --- |
    | Issue | `{issue}` |
    | Status | `not dispatched` |
    | Problem | `missing_workflow_label` |

    ## Summary
    Scherzo did not start this issue because it has no workflow label.

    ## Next action
    Add exactly one allowed workflow label, then move the issue back to `{ready_state}`.

    ## Allowed labels
    - `workflow:bugfix`
    - `workflow:feature`

New multiple-label body:

    🏷️ Scherzo needs one workflow label

    | Field | Value |
    | --- | --- |
    | Issue | `{issue}` |
    | Status | `not dispatched` |
    | Problem | `multiple_workflow_labels` |

    ## Summary
    Scherzo found more than one workflow label and cannot choose a workflow safely.

    ## Found labels
    - `workflow:bugfix`
    - `workflow:feature`

    ## Next action
    Keep exactly one allowed workflow label, then move the issue back to `{ready_state}`.

    ## Allowed labels
    - `workflow:bugfix`
    - `workflow:feature`

New unknown-label body:

    🏷️ Scherzo needs an allowed workflow label

    | Field | Value |
    | --- | --- |
    | Issue | `{issue}` |
    | Status | `not dispatched` |
    | Problem | `unknown_workflow_label` |

    ## Summary
    Scherzo found `{found_label}`, which is not configured as an allowed workflow label.

    ## Next action
    Replace it with exactly one allowed workflow label, then move the issue back to `{ready_state}`.

    ## Allowed labels
    - `workflow:bugfix`
    - `workflow:feature`

If the ready state name is unavailable, use the existing fallback sentence `the configured ready state` in the `## Next action` section.

### `/scherzo` command acknowledgement comments

Current pattern:

    Scherzo command received from comment {comment_id}.
    Command: {command}
    Status: {status}
    Target: {target}
    Message: {message}
    Excerpt: {excerpt}

New applied body:

    ✅ Scherzo command applied

    | Field | Value |
    | --- | --- |
    | Command | `{command}` |
    | Status | `applied` |
    | Target | `{target}` |
    | Source comment | `{comment_id}` |

    ## Summary
    {message, for example `abort requested`}

    ## Command excerpt
    `{excerpt}`

Omit `Target` when absent. Omit `## Command excerpt` when absent.

New queued body:

    ⏳ Scherzo command queued

    | Field | Value |
    | --- | --- |
    | Command | `{command}` |
    | Status | `queued` |
    | Target | `{target}` |
    | Source comment | `{comment_id}` |

    ## Summary
    {message, for example `prompt queued for next turn`}

    ## Command excerpt
    `{excerpt}`

New rejected parse-error body for unknown, missing, invalid, and multiple-command parse errors:

    🚫 Scherzo command rejected

    | Field | Value |
    | --- | --- |
    | Command | `{command_or_malformed}` |
    | Status | `rejected` |
    | Source comment | `{comment_id}` |

    ## Summary
    {parse error message}

    ## Next action
    Edit or post one `/scherzo` command with the required arguments.

New not-found body for a session command without a current session:

    🔎 Scherzo command target not found

    | Field | Value |
    | --- | --- |
    | Command | `{command}` |
    | Status | `not_found` |
    | Source comment | `{comment_id}` |

    ## Summary
    No current Scherzo session is running for this issue.

    ## Next action
    Start or retry a run before sending this session command.

New not-allowed body for an unauthorized Linear user:

    🔒 Scherzo command not allowed

    | Field | Value |
    | --- | --- |
    | Command | `unknown` |
    | Status | `not_allowed` |
    | Source comment | `{comment_id}` |

    ## Summary
    Scherzo ignored this command because the Linear user is not authorized.

    ## Run details
    - Linear user id: `{author_id}`

New unknown-after-restart body:

    ❓ Scherzo command status is unknown

    | Field | Value |
    | --- | --- |
    | Command | `{command}` |
    | Status | `unknown_after_restart` |
    | Source comment | `{comment_id}` |

    ## Summary
    Scherzo restarted while this command was in progress, so it cannot safely confirm the outcome from memory.

    ## Next action
    Inspect the current issue or session state. Post a new command only if action is still needed.

Completed durable receipts should use the same applied, queued, rejected, not-found, or not-allowed templates based on the stored `result_status` and `message_excerpt`.

### Attachment-link fallback final body

Do not change native attachment semantics. When `src/scherzo/linear_attachment.gleam` cannot update `bodyData` and `fallback_to_markdown_link` is enabled, it currently appends a Markdown link to the comment body. Preserve that behavior, but make success comments' `## Artifacts` section anticipate it so the final body remains understandable.

A fallback final body should look like this after `append_markdown_link` runs:

    ✅ Scherzo completed the run

    | Field | Value |
    | --- | --- |
    | Issue | `LIV-33` |
    | Run | `LIV-33--576460751484-1` |
    | Status | `completed` |
    | Classification | `terminal` |

    ## Summary
    Scherzo finished the run for `LIV-33`.

    ## Artifacts
    - Full result: Scherzo will attempt to add `liv-33-liv-33-576460751484-1-result.md` to this comment. If fallback linking is used, a Markdown link appears below.

    ## Run details
    - Turns: 3
    - Result source: `agent_end_messages`

    ## Token usage
    | Kind | Tokens |
    | --- | ---: |
    | Input | 261838 |
    | Output | 39904 |
    | Cache read | 2752000 |
    | Cache write | 0 |
    | Total | 3053742 |

    [liv-33-liv-33-576460751484-1-result.md](https://uploads.linear.app/example/result.md)

## Milestones

Milestone 1 creates the shared house style helpers and proves they render deterministic safe Markdown. At the end of this milestone, `src/scherzo/linear_comment_format.gleam` exists with tests that verify titles, summary tables, sections, token tables, code spans, table escaping, indented blocks, optional rows, adversarial Markdown inputs, and final safety cleanup. No production call sites need to change yet.

Milestone 2 updates handoff comments in small slices. At the end of this milestone, claim, success without attachment, success with attachment intent, success-result attachment Markdown, generic failure, workflow-command failure, and park comments use the new style while preserving redaction, sanitization, path hiding, truncation, attachment upload order, and state update order. The success attachment tests prove that artifact language matches attachment intent and never falsely claims that a file is already attached.

Milestone 3 updates invalid workflow-label triage. At the end of this milestone, missing-label, multiple-label, and unknown-label comments have friendly deterministic bodies, while label classification, allowed-label ordering, issue state reporting, and issue-description exclusion behave the same as before.

Milestone 4 updates `/scherzo` command acknowledgements one status family at a time. At the end of this milestone, applied, queued, rejected parse-error, not-found, not-allowed, unauthorized, completed durable receipt, and unknown-after-restart comments use the new style, while command parsing, authorization, duplicate processing, acknowledgement enablement, and receipt handling behave the same as before.

Milestone 5 validates Linear rendering and rollout safety. At the end of this milestone, tests pass, formatting is checked, `glinter` passes, and a fresh `[test] scherzo` dry-run confirms in the Linear browser that final candidate comments render acceptably. API `bodyData` evidence should also be recorded, but browser readability is the required rollout gate.

## Plan of Work

Create `src/scherzo/linear_comment_format.gleam`. Keep it small. It should not know about Linear GraphQL transport, issue state changes, command parsing, attachment upload, or workflow dispatch. It should only build Markdown strings from already-computed facts.

Define helpers with stable names similar to these:

    pub type SummaryRow {
      SummaryRow(label: String, value: String)
    }

    pub fn title(icon: String, text: String) -> String

    pub fn safe_inline(value: String, fallback: String) -> String

    pub fn table_text(value: String, fallback: String) -> String

    pub fn table_code(value: String, fallback: String) -> String

    pub fn code_span(value: String, fallback: String) -> String

    pub fn summary_table(rows: List(SummaryRow)) -> String

    pub fn section(title: String, body: String) -> String

    pub fn bullet_section(title: String, bullets: List(String)) -> String

    pub fn token_usage_table(tokens: session_tokens.TokenTotals) -> String

    pub fn indented_block(text: String) -> String

    pub fn optional_row(label: String, value: Option(String)) -> List(SummaryRow)

    pub fn finalize_body(context: String, body: String, secrets: List(String)) -> String

The exact type names may be adjusted to fit Gleam style, but the public purpose should remain narrow and mandatory: reusable Markdown primitives and final safety cleanup for Scherzo Linear comments. The helpers must own escaping for pipes, newlines, backticks, empty values, and terminal control characters so callers do not reimplement those rules.

In `src/scherzo/handoff_format.gleam`, import `scherzo/linear_comment_format` and update the existing public handoff functions. Add a new public `claim_comment(issue_identifier: String, run_id: String, secrets: List(String)) -> String`. Replace the old `success_comment` flag argument with a `SuccessCommentOptions` value containing `include_result: Bool` and `attachment_filename: Option(String)`. Add a public `success_result_filename(issue_identifier: String, run_id: String) -> String` helper by moving the current private filename generation logic out of `src/scherzo/handoff.gleam` or otherwise exposing one shared source of truth. Keep `success_result_attachment_markdown`, `failure_comment`, and `park_comment` as the other handoff-facing APIs.

In `src/scherzo/handoff.gleam`, replace the inline claim body in `claim_issue` with `handoff_format.claim_comment(issue.identifier, run_id, tracker_secrets(tracker_config))`. In the success path, compute `attachment_filename` before creating the success comment: use `Some(handoff_format.success_result_filename(issue.identifier, run_id))` only when `handoff_config.attach_result_on_success` is true and `success.result.final_response` is present; otherwise use `None`. Pass the same filename to `linear_attachment.attach_markdown_to_comment`. Do not change `run_comment`, `run_state_update`, `create_success_comment`, `maybe_attach_success_result`, or the order in which they are called except for threading the options and shared filename through the existing calls.

In `src/scherzo/workflow_policy.gleam`, keep classification functions unchanged. Add a new formatter that can include the issue identifier, such as `violation_comment(issue_identifier: String, violation: IssueWorkflowViolation, config: config_types.LinearContractConfig) -> String`, or change `violation_message` only if all callers and tests are updated together. Prefer adding the new function and leaving `violation_message` as a compatibility wrapper if it has other callers. The new formatter should use the shared Markdown helpers and should preserve normalized allowed-label ordering.

In `src/scherzo/linear_triage.gleam`, call the new workflow-policy formatter with `issue.identifier`. Do not change the comment/state reporting outcome enum or the order of comment and state update operations.

In `src/scherzo/control/linear_transport.gleam`, replace `common_ack_body` with a formatter that chooses the status title and sections from the status string. Keep `result_ack_body`, `completed_receipt_ack_body`, `unknown_after_restart_ack_body`, `should_ack_result`, `should_ack_receipt_status`, and process-control logic intact. Continue redacting and truncating `result.message` to 160 characters and parsed excerpts to 80 characters before rendering. Also bound and sanitize parse-error command names and invalid argument values before they enter the table or summary; use the same 160-character summary bound unless existing tests require a stricter current limit.

In `src/scherzo/linear_attachment.gleam`, avoid changing upload, dedupe, validation, and fallback decisions. If tests show the bare appended link is still too abrupt, the only allowed formatting change is to make `append_markdown_link` append the link after the existing body as it does today; do not insert new upload behavior or fetch extra Linear state. The success comment's `## Artifacts` section should carry the UX improvement.

## Concrete Steps

1. From the repository root, create `test/linear_comment_format_test.gleam`. Add exact tests for `title`, `section`, `bullet_section`, `token_usage_table`, `indented_block`, optional rows, and final body cleanup.

2. In the same test file, add adversarial Markdown tests. Assert that table values containing `a|b`, ``a`b``, a two-line string, an empty string, and a terminal control character render deterministically and cannot split a table row or create a new section.

3. Run the targeted new test and expect it to fail because `src/scherzo/linear_comment_format.gleam` does not exist yet:

    direnv exec . gleam test --target erlang test/linear_comment_format_test.gleam

   If the project test runner does not support file-target syntax, run `direnv exec . gleam test` and expect compile errors naming the missing module.

4. Add `src/scherzo/linear_comment_format.gleam` with only the helpers needed by the tests. Include safe inline text, table text, table code, code span, indented block, token table, optional row, and final-body helpers. Do not import transport or workflow modules.

5. Run the formatter tests again and expect them to pass. Commit point: shared safe Linear comment Markdown primitives.

6. In `test/handoff_format_test.gleam`, add a failing exact test for `handoff_format.claim_comment("LIV-38", "LIV-38--576460151305-2", [])`. Assert the title, summary table rows, summary section, and next-action section.

7. In `src/scherzo/handoff_format.gleam`, add `claim_comment` using the shared formatter and final safety boundary. Then update `src/scherzo/handoff.gleam` so `claim_issue` uses it instead of the inline string.

8. Run `direnv exec . gleam test` and fix only claim-comment failures. Commit point: friendly claim comment.

9. In `test/handoff_format_test.gleam`, update or add success-without-attachment tests for `SuccessCommentOptions(include_result: True, attachment_filename: None)` and `SuccessCommentOptions(include_result: False, attachment_filename: None)`. Assert no `## Artifacts` section appears in either body, and assert `_No assistant result text was captured._` appears when the result is absent and inline result display is enabled.

10. In `src/scherzo/handoff_format.gleam`, define `SuccessCommentOptions` and update `success_comment` for the no-attachment cases. Preserve `sanitize_comment_body` behavior by delegating to the new final safety boundary. Run `direnv exec . gleam test` and fix only success-comment failures.

11. In `test/handoff_format_test.gleam`, add tests for `success_result_filename`. Use issue and run values containing uppercase letters and punctuation, and assert the same safe filename currently expected by handoff attachment tests.

12. In `src/scherzo/handoff_format.gleam`, move or expose the filename-generation logic currently private to `src/scherzo/handoff.gleam` as `success_result_filename`. In `src/scherzo/handoff.gleam`, replace calls to the private filename helper with the shared function. Run `direnv exec . gleam test`.

13. In `test/handoff_format_test.gleam`, add success-with-attachment-intent tests for `SuccessCommentOptions(include_result: False, attachment_filename: Some("liv-33-liv-33-576460751484-1-result.md"))` and for `include_result: True` with the same filename. Assert the neutral artifact line appears and never says the file is already attached.

14. In `src/scherzo/handoff_format.gleam`, implement the attachment-intent branch in `success_comment`. In `src/scherzo/handoff.gleam`, compute `attachment_filename` as `Some(handoff_format.success_result_filename(issue.identifier, run_id))` only when `handoff_config.attach_result_on_success` is true and `success.result.final_response` is present; otherwise pass `None`. Run `direnv exec . gleam test`.

15. In `test/handoff_test.gleam`, add or update the success artifact matrix. Cover attachment disabled, attachment enabled with no final response, attachment enabled with final response and native upload success, attachment enabled with fallback Markdown link, and attachment enabled with upload failure. Assert the filename in the comment options matches the filename passed to `linear_attachment.attach_markdown_to_comment`, no artifact line appears for disabled/no-result cases, fallback appends the link below the neutral artifact line, and upload failure does not leave a false “attached file” claim.

16. In `src/scherzo/handoff.gleam`, thread the shared filename through `create_success_comment` and `maybe_attach_success_result` without changing comment creation, upload, or state-update ordering. Run `direnv exec . gleam test`. Commit point: truthful success comments and attachment intent.

17. In `test/handoff_format_test.gleam`, update `success_result_attachment_markdown` tests for the new attachment Markdown template. Include a no-final-response case that still returns `None`, and include redaction and terminal-control sanitization cases.

18. In `src/scherzo/handoff_format.gleam`, update `success_result_attachment_markdown` only. Run `direnv exec . gleam test`. Commit point: friendly result attachment Markdown.

19. In `test/handoff_format_test.gleam` and `test/handoff_test.gleam`, update generic failure tests for nested Pi failure, hook failure, workflow hook failure, long detail truncation, token table rendering, redaction, and terminal-control sanitization.

20. In `src/scherzo/handoff_format.gleam`, update generic failure rendering while preserving diagnostic truncation and token collection. Run `direnv exec . gleam test`.

21. In the same test files, update workflow-command failure tests for retained workspace marker, revalidation failure, safe relative workspace display, and forbidden absolute local workspace hiding.

22. In `src/scherzo/handoff_format.gleam`, update workflow-command failure rendering while preserving `display_workspace_path`, `sanitize_workspace_path_in_detail`, `retained_workspace_status`, and suggested next-action mapping. Run `direnv exec . gleam test`. Commit point: friendly failure comments.

23. In `test/handoff_format_test.gleam`, update park tests. Assert the title, summary table, redacted single-line reason, optional release policy row, optional run row, truncation, and next-action wording.

24. In `src/scherzo/handoff_format.gleam`, update `park_comment`. Run `direnv exec . gleam test`. Commit point: friendly park comments.

25. In `test/linear_triage_test.gleam`, update the missing-workflow-label assertion. Assert title, issue identifier, `not dispatched` status, `missing_workflow_label` problem code, configured ready-state guidance, and deterministic allowed-label list.

26. In `src/scherzo/workflow_policy.gleam`, add `violation_comment` for the missing-label case, keeping classification behavior unchanged. In `src/scherzo/linear_triage.gleam`, call it with `issue.identifier`. Run `direnv exec . gleam test`.

27. Repeat the previous test-code cycle for multiple workflow labels, then for unknown workflow labels. Include a ready-state-unavailable test that uses `the configured ready state`, and assert issue descriptions and unrelated issue fields still do not appear. Commit point: friendly workflow-label triage comments.

28. In `test/linear_command_transport_test.gleam`, update applied and queued acknowledgement tests first. Assert title, table rows, summary message, target omission when absent, source comment id, command excerpt omission when absent, and redaction/truncation of messages and excerpts.

29. In `src/scherzo/control/linear_transport.gleam`, update the applied and queued branches of the shared acknowledgement formatter. Run `direnv exec . gleam test`.

30. In `test/linear_command_transport_test.gleam`, update rejected parse-error tests for unknown command, missing argument, invalid argument, and multiple commands. Add adversarial invalid argument input containing a pipe, backtick, newline, and terminal control character; assert it stays inside the summary/table and is bounded.

31. In `src/scherzo/control/linear_transport.gleam`, update rejected parse-error rendering and bound parse-error text before formatting. Run `direnv exec . gleam test`.

32. In `test/linear_command_transport_test.gleam`, update not-found, not-allowed, unauthorized, completed durable receipt, and unknown-after-restart tests. Preserve duplicate-processing and acknowledgement-enable/disable assertions.

33. In `src/scherzo/control/linear_transport.gleam`, update those remaining acknowledgement status families. Run `direnv exec . gleam test`. Commit point: friendly Linear command acknowledgements.

34. In `test/linear_attachment_test.gleam`, keep all native attachment, fallback, dedupe, invalid body data, extension validation, size limit, and failure tests. Update only expected success-comment snippets affected by the neutral `## Artifacts` wording. Do not weaken upload safety assertions.

35. Perform a visual dry-run in the `[test] scherzo` project using a disposable or clearly safe issue. Prefix each temporary comment with `[DRY RUN — LIV-121]`. Post one representative success-with-attachment-intent body, one workflow-command failure body, one invalid-workflow body, and one command acknowledgement body. Query their `bodyData`, inspect each comment in the Linear browser, record whether tables and sections are readable, and delete the temporary comments unless a reviewer asks to keep them. If browser inspection is not possible, stop before rollout and add a clarification entry to this plan.

36. Run final validation:

    direnv exec . gleam test
    direnv exec . gleam format --check src test
    direnv exec . gleam run -m glinter

   Expected result: all tests pass, formatting check exits successfully, and `glinter` reports no production policy errors.

## Testing and Falsifiability

Add `test/linear_comment_format_test.gleam` with exact tests for the shared primitives and adversarial Markdown behavior. A table test should call `summary_table([SummaryRow("Issue", table_code("ABC-1", "unknown")), SummaryRow("Run", table_code("run-1", "unknown"))])` and assert:

    | Field | Value |
    | --- | --- |
    | Issue | `ABC-1` |
    | Run | `run-1` |

Adversarial formatter tests are mandatory. Assert that a table code value `a|b` renders as a single table cell, that a value containing a backtick is wrapped with a safe longer delimiter, that multi-line values are collapsed before entering a table, that empty values use the configured fallback, and that terminal control characters are stripped. Add an indented block test where a multi-line diagnostic remains inside the block and cannot introduce a new top-level heading.

Update `test/handoff_format_test.gleam` with exact success-comment tests. The red phase should fail because the old body starts with `Scherzo completed run`. The green phase should start with `✅ Scherzo completed the run`, include a summary table, include `## What Scherzo did` only when inline results are enabled, omit `## Artifacts` when `attachment_filename` is `None`, include neutral `## Artifacts` wording when `attachment_filename` is `Some(_)`, and include token usage as a table. Keep the existing secret redaction test but assert the secret is absent from every section after Markdown escaping and final cleanup.

Add a success artifact outcome matrix in `test/handoff_test.gleam` and, where lower-level exact strings are easier, `test/handoff_format_test.gleam`. Cover at least these cases: `attach_result_on_success` false with `include_result_on_success` false; `attach_result_on_success` false with `include_result_on_success` true; `attach_result_on_success` true with no captured final response; `attach_result_on_success` true with captured response and native upload success; `attach_result_on_success` true with fallback Markdown link; and `attach_result_on_success` true with upload failure. Assert no artifact line appears when attachment is disabled or no result body exists, the filename in the comment matches the filename passed to `linear_attachment.attach_markdown_to_comment`, fallback appends the link below the neutral artifact line, and upload failure does not leave text that claims a file is already attached.

Update `success_result_attachment_markdown` tests in `test/handoff_format_test.gleam`. Assert the new heading, summary table, `## Result`, run details, token table, redaction, truncation marker, and `None` when no final response exists.

Update failure tests in `test/handoff_test.gleam` and `test/handoff_format_test.gleam`. Include these exact scenarios: nested Pi failure, hook failure, workflow hook failure, workflow command failure with retained workspace marker, workflow command revalidation failure, long detail truncation, terminal control character sanitization, relative workspace display, and absolute local workspace hiding. A test for an absolute local path should construct the path without placing a real machine-specific prefix in source comments, and should assert the rendered comment contains `_not shown because Scherzo recorded an absolute path outside the repository_` rather than the raw path. Add at least one failure-detail test containing a pipe, backtick, and newline to prove diagnostics cannot break the table before the indented block.

Update park tests in `test/handoff_format_test.gleam`. Assert the title is `⏸️ Scherzo parked this issue`, the reason is single-line and redacted, optional release policy and run rows appear only when present, long reasons are truncated, and Markdown table separators in the reason cannot split the table.

Update `test/linear_triage_test.gleam`. Add missing-label, multiple-label, and unknown-label cases. Assert issue description and other unrelated issue fields still do not appear. Assert allowed labels are normalized and deterministic. Assert ready-state guidance uses the configured ready state when present and `the configured ready state` when absent. Include a label or ready-state fixture containing a pipe or newline and assert it is safely rendered.

Update `test/linear_command_transport_test.gleam`. For each important status variant, assert the friendly first line and the raw status in the table. Cover `applied`, `queued`, `rejected`, `not_found`, `not_allowed`, and `unknown_after_restart`. Include parse errors for unknown command, missing argument, invalid argument, and multiple commands. Include unauthorized user rejection. Include durable completed receipts with and without `acked_at_ms`. Preserve redaction and truncation tests for command messages and excerpts. Add an end-to-end acknowledgement body test where the command excerpt or invalid argument contains a pipe, backtick, newline, terminal control character, and a configured secret; assert it remains inside the intended summary or excerpt section and the secret is absent.

Update attachment tests in `test/linear_attachment_test.gleam`. Keep `fallback_updates_body_when_body_data_is_invalid_test` asserting that fallback uses `ScherzoCommentUpdateBody`, not `ScherzoCommentUpdateBodyData`, and that the Markdown link is appended deterministically. Keep dedupe tests for both native file nodes and fallback links. Keep upload failure tests strong: they should prove upload failure does not change state ordering and does not make the comment falsely claim a completed attachment.

The plan is falsified if any of these are true after implementation: a secret appears in a generated comment, a terminal control character survives sanitization, an adversarial Markdown value breaks a table or section boundary, a forbidden absolute local path appears in a generated comment, a long diagnostic or parse-error value is unbounded, a success comment promises an artifact for a disabled/no-result/upload-failure case, command authorization or parsing behavior changes, attachment upload order changes, existing comment enablement config is ignored, browser validation is skipped, or Linear browser dry-runs show the chosen Markdown is unreadable.

## Validation and Acceptance

From the repository root, run:

    direnv exec . gleam test

Expected result: the full Gleam test suite passes. The new and updated tests prove exact comment bodies, redaction, path hiding, truncation, command acknowledgement variants, workflow-label triage variants, and attachment fallback behavior.

Then run:

    direnv exec . gleam format --check src test

Expected result: the formatter exits successfully with no required changes.

Then run:

    direnv exec . gleam run -m glinter

Expected result: `glinter` exits successfully with no production policy errors. Do not add production `let assert`, `panic`, or `todo` while implementing this plan.

Perform a Linear dry-run against the `[test] scherzo` project only. Use a clearly disposable or dedicated test issue, prefix all temporary comments with `[DRY RUN — LIV-121]`, and delete them after inspection unless they are intentionally kept as review evidence. Acceptance requires recorded notes from browser inspection that tables, headings, code spans, links, and indented blocks render acceptably in Linear's browser UI. Also query `bodyData` and record whether table nodes are present, but do not treat API evidence as a substitute for browser readability. If browser access is unavailable, stop before rollout, add a clarification item to this plan, and do not mark validation accepted.

## Rollout, Recovery, and Idempotence

The rollout is a normal Scherzo code deploy with formatting-only behavior changes. There is no data migration. Existing comments remain unchanged; only newly posted comments use the new layout. Existing configuration flags such as `comment_on_claim`, `comment_on_success`, `comment_on_failure`, `comment_on_park`, `comment_on_invalid_workflow`, `acknowledge_success`, and `acknowledge_rejection` keep their current meaning.

Recovery is straightforward. If operators find the new comments confusing or noisy, revert the formatting commit or temporarily restore the old body builders in `src/scherzo/handoff_format.gleam`, `src/scherzo/workflow_policy.gleam`, and `src/scherzo/control/linear_transport.gleam`. Because the change does not alter state transitions, retries, command parsing, permissions, or attachment upload, rollback does not require data cleanup.

Implementation steps are idempotent at the repository level: rerunning tests and dry-run comments is safe. Dry-run comments in Linear should always be prefixed `[DRY RUN — LIV-121]` and deleted after use. Never dry-run against the production `Scherzo` project without explicit human approval.

## Artifacts and Notes

Real Linear examples collected from `[test] scherzo`:

- `LIV-38` claim comment: `Scherzo claimed LIV-38 for run LIV-38--576460151305-2.`
- `LIV-38` success comment with result and metadata: `Scherzo completed run LIV-38--576460151305-2 for LIV-38.` followed by `Result:` and `Metadata:`.
- `LIV-33` success comment with fallback attachment link at the bottom: `liv-33-liv-33-576460751484-1-result.md` linked to a Linear upload URL.
- `LIV-32` failure comment: `Scherzo failed run LIV-32--576460750994-1 for LIV-32 with error agent_pi_failed.`
- `LIV-11` failure comment with attachment link: `Scherzo failed run LIV-11--576460159570-3 for LIV-11 with error agent_hook_failed.` followed by a Linear upload link.
- `LIV-10` queued command acknowledgement for `/scherzo prompt hello from Linear smoke`.
- `LIV-10` applied command acknowledgement for `/scherzo abort`.

Linear Markdown dry-run notes:

- Temporary issue used: `LIV-12` in `[test] scherzo`.
- Temporary prefix used: `[DRY RUN — LIV-121]`.
- Temporary comment id: `049ffe91-0831-4b85-8079-704972d0fd9a`.
- Temporary comment URL before deletion: `https://linear.app/living-systems/issue/LIV-12/create-a-plan-to-address-naming-issues#comment-049ffe91`.
- API evidence: Markdown table syntax remained in `body` and became `table` nodes in `bodyData`.
- API limitation: HTML `<details>` was not proven as a safe browser-rendered collapsible disclosure. Use regular sections as fallback.

Final Linear browser validation notes:

- Temporary issue used: `LIV-12` in `[test] scherzo`.
- Temporary prefix used: `[DRY RUN — LIV-121]`.
- Representative success-with-attachment-intent comment: `484e6531-6ca2-42c6-adce-499bf61ff864`, `https://linear.app/living-systems/issue/LIV-12/create-a-plan-to-address-naming-issues#comment-484e6531`.
- Representative workflow-command failure comment: `ebace243-0564-473d-ab60-fed0e185c29b`, `https://linear.app/living-systems/issue/LIV-12/create-a-plan-to-address-naming-issues#comment-ebace243`.
- Representative invalid-workflow comment: `418ea577-dfb3-4d9a-90b5-46759844efd5`, `https://linear.app/living-systems/issue/LIV-12/create-a-plan-to-address-naming-issues#comment-418ea577`.
- Representative command acknowledgement comment: `f1f60d29-cbaf-4297-949e-a84032f208f2`, `https://linear.app/living-systems/issue/LIV-12/create-a-plan-to-address-naming-issues#comment-f1f60d29`.
- Browser evidence: the operator inspected these comments in Linear's browser UI and accepted the layouts as readable/scannable.
- API evidence: a Linear GraphQL query confirmed the comments' `bodyData` contains rich `table` nodes for summary/token tables, `heading` nodes for sections, `code` marks for identifiers, and a `code_block` node for the workflow-command diagnostic block.

## Interfaces and Dependencies

No new package dependency is required.

The new module `src/scherzo/linear_comment_format.gleam` should expose Markdown helpers and final safety cleanup only. It may import `gleam/list`, `gleam/option`, `gleam/string`, `gleam/int`, `scherzo/log`, `scherzo/terminal/sanitize`, and `scherzo/session/tokens` if token table rendering lives there. It should not import `scherzo/linear`, `scherzo/handoff`, `scherzo/control/command`, or transport modules.

`src/scherzo/handoff_format.gleam` remains the handoff API and should expose:

    pub fn claim_comment(issue_identifier: String, run_id: String, secrets: List(String)) -> String

    pub type SuccessCommentOptions {
      SuccessCommentOptions(include_result: Bool, attachment_filename: Option(String))
    }

    pub fn success_comment(issue: tracker_issue.Issue, success: agent_types.WorkerSuccess, run_id: String, options: SuccessCommentOptions, secrets: List(String)) -> String

    pub fn success_result_filename(issue_identifier: String, run_id: String) -> String

    pub fn success_result_attachment_markdown(issue: tracker_issue.Issue, success: agent_types.WorkerSuccess, run_id: String, secrets: List(String)) -> Option(String)

    pub fn failure_comment(issue: tracker_issue.Issue, failure: agent_types.WorkerFailure, run_id: String, secrets: List(String)) -> String

    pub fn park_comment(issue_identifier: String, reason: String, release_policy: Option(String), run_id: Option(String), secrets: List(String)) -> String

`src/scherzo/workflow_policy.gleam` should expose an issue-aware invalid-workflow comment builder, preferably without removing the existing `violation_message` until all callers are known:

    pub fn violation_comment(issue_identifier: String, violation: IssueWorkflowViolation, config: config_types.LinearContractConfig) -> String

`src/scherzo/control/linear_transport.gleam` should keep these public functions and their signatures unless tests reveal a current signature is unused and private:

    pub fn completed_receipt_ack_body(comment_id: String, command_name: String, status: String, message_excerpt: String) -> String

    pub fn unknown_after_restart_ack_body(comment_id: String, command_name: String) -> String

    pub fn result_ack_body(source_comment_id: String, parsed: linear_parser.ParsedLinearCommand, result: command.CommandResult, secrets: List(String)) -> String

Do not change `linear_transport.TransportAction`, `linear_transport.process_comments`, `handoff.Client`, `linear_attachment.AttachOptions`, or the Linear GraphQL request builders as part of this plan.

## Open Questions and Clarifications Needed

- Resolved 2026-05-08: Browser validation was completed after the implementation run stopped at the plan-completion gate. Four representative `[DRY RUN — LIV-121]` comments on `LIV-12` were inspected in Linear's browser UI, accepted by the operator as readable/scannable, and queried for `bodyData` evidence.
