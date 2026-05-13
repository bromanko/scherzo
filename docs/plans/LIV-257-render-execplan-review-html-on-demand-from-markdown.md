# Render ExecPlan Review HTML from Markdown on Demand

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

ExecPlan authors now keep Markdown as the canonical, reviewable plan artifact. Reviewers still need the richer browser experience that shows a rendered plan and lets them click a block, draft feedback, and submit inline GitHub review comments. After this change, a reviewer can run `scripts/scherzo-execplan-review 123 --no-open` for a read-only preview or `scripts/scherzo-execplan-review review 123 --no-open` for interactive review of a pull request that changes one `docs/plans/*.md` file. The tool downloads the Markdown from the PR head, renders a temporary HTML viewer under `tmp/scherzo-execplan-review/`, serves that viewer locally for interactive review, and submits comments against the Markdown path and Markdown source lines rather than against any generated HTML file.

The observable outcome is that a Markdown-only ExecPlan PR is reviewable in the same browser UI previously used for HTML plans, without committing or requiring `docs/plans/*.html`. The default preview command remains read-only. Remote GitHub mutation remains available only after the reviewer explicitly uses the `review` subcommand and then clicks Submit in the browser UI.

## Problem Framing and Constraints

The current review helper is in the middle of a source-of-truth transition. Markdown plans are canonical, but the interactive review path still treats committed HTML as the source file that contains review targets. In the current tree, Markdown preview mode can render a temporary HTML preview, but Markdown interactive review mode remains preview-only and does not start the local draft server. That means a reviewer of a Markdown ExecPlan PR must either review raw Markdown manually or lose the interactive block-level browser workflow.

This hurts operators and reviewers because the generated HTML viewer is the easiest way to read long plans, but GitHub inline comments must still land on the diff for `docs/plans/*.md`. A generated viewer line is not the same artifact as the PR source file. The implementation must therefore keep two ideas separate: the PR artifact is the Markdown source, and the HTML viewer is a local derived convenience artifact.

The main constraints are safety, portability, and compatibility. The implementation must not create a tracked `docs/plans/*.html` artifact. Temporary output must stay under `tmp/scherzo-execplan-review/`, which is ignored by the repository. Existing legacy PRs that still change one `docs/plans/*.html` file should remain previewable and reviewable where practical. The browser must not receive a GitHub token; the localhost Python server remains responsible for all GitHub API calls through `gh`. Paths in code, tests, docs, and retained artifacts must be repository-relative or local temporary paths, not hard-coded machine-specific absolute paths.

## Strategy Overview

The right-sized approach is to extend the existing Python helper and renderer rather than add a new service or redesign the review UI. `scripts/scherzo_execplan_review.py` already knows how to find the single changed plan artifact in a PR, download it under `tmp/scherzo-execplan-review/`, render Markdown to HTML for preview with `scripts/scherzo-execplan-html`, serve an interactive HTML review UI for legacy HTML plans, persist draft comments, compute diff-eligible lines, and submit GitHub review payloads. The missing piece is to let the interactive path use a rendered Markdown viewer as its HTML input, while keeping the stored PR path and submitted comment coordinates pointed at the Markdown source.

The implementation should make Markdown an explicit first-class source kind in the review module. In preview mode, Markdown continues to render to a static temporary HTML file and prints `PLAN_PR_PATH=docs/plans/example.md`. In interactive review mode, the module downloads the Markdown source, renders it to a temporary HTML viewer, injects the existing draft UI into that viewer, starts the same loopback-only server, and records drafts with the Markdown `plan_path`. When building review submissions for Markdown, the source line should come from the rendered viewer element's `data-source-line`, because `scripts/scherzo-execplan-html` emits that attribute from the original Markdown line number. For legacy HTML, keep the existing `data-comment-id` lookup against the HTML source file so old HTML PRs continue to target their actual HTML diff lines.

This is proportionate because it reuses the current renderer, draft persistence, local server, security headers, and GitHub submission code. The plan avoids a broader viewer redesign and avoids a Markdown AST dependency. The hardest assumption is that every commentable block in the generated viewer has a reliable `data-source-line` that corresponds to an eligible Markdown diff line. The earliest implementation milestone tests that assumption directly before changing the submission flow.

## Alternatives Considered

The simplest alternative is to keep Markdown PRs preview-only and require reviewers to copy feedback manually into GitHub. That preserves safety but fails the desired outcome: reviewers lose the interactive HTML review UI exactly when Markdown becomes the canonical artifact.

Another option is to generate and commit HTML beside every Markdown plan. That would make the existing HTML review mode work with little code change, but it reintroduces the stale derived artifact problem and directly conflicts with the source-of-truth direction. It also creates review noise whenever renderer output changes.

A third option is to parse Markdown in the browser and post comments from browser JavaScript directly to GitHub. That would duplicate an existing renderer, expand the security boundary, and risk exposing credentials or remote mutation capability to PR-supplied content. The existing server-side `gh` submission path is safer and already tested.

A fourth option is to submit all Markdown review feedback as a single PR summary comment. That is useful as a fallback when GitHub cannot anchor an inline comment, but it should not be the primary path because the acceptance criteria require eligible comments to target Markdown diff lines.

## Risks and Countermeasures

The main correctness risk is wrong line mapping. If the generated HTML target's `data-source-line` is missing, stale, or from the generated file rather than the Markdown source, GitHub comments could land on the wrong Markdown line or fail submission. Countermeasure: add focused unit tests that render Markdown through `scripts/scherzo-execplan-html`, inspect the generated `data-source-line` values, create drafts from those values, and assert that review payloads target `docs/plans/example.md` at the Markdown source line. Keep fallback summary behavior when a line is missing or not diff-eligible.

A related browser-flow risk is losing review metadata during sanitization or review UI injection. If the server renders correct Markdown HTML but then strips `data-comment-id` or `data-source-line` before the browser sees it, browser-created drafts will lack usable Markdown coordinates and will all fall back to summaries. Countermeasure: add an interactive server test that requests the served, sanitized viewer and asserts the response still contains commentable blocks with the expected `data-comment-id` and `data-source-line` attributes before creating a draft through the server draft API.

A compatibility risk is breaking legacy HTML review. HTML PRs are still possible in old branches. Countermeasure: keep `PLAN_RE` accepting both `.md` and `.html`, keep the legacy HTML interactive path, and add a regression test proving HTML mapping still uses the HTML source file's line for `data-comment-id` rather than a Markdown-style `dom_source_line` fallback.

A safety risk is accidentally serving raw PR HTML without the existing sanitizer or content security policy. Countermeasure: make the interactive preparation step always produce a sanitized, injected preview, whether the source is Markdown-rendered HTML or legacy HTML. Keep the loopback host, per-session token, `Content-Security-Policy`, `Referrer-Policy`, `Cache-Control`, and `X-Content-Type-Options` tests.

A workflow risk is creating tracked generated files. Countermeasure: keep every downloaded source, rendered viewer, draft file, submit payload, and result under the configured output directory, defaulting to `tmp/scherzo-execplan-review/`. Add tests that assert Markdown preview paths are under the session root and that no implementation step writes to `docs/plans/*.html`.

A UX risk is confusing reviewers about which file is being reviewed. Countermeasure: update CLI help, printed metadata, and operator-facing docs text to say that `PLAN_PR_PATH` is the source artifact, Markdown is primary, and `PLAN_PREVIEW_PATH` is a temporary derived viewer.

## Progress

- [x] (2026-05-13 00:00Z) Read the repository-local ExecPlan authoring guidance and inspected the current review helper, renderer, review tests, validation helper, ignored `tmp/` behavior, and relevant operator docs context.
- [x] (2026-05-13 00:00Z) Drafted this Markdown ExecPlan source artifact for `LIV-257` without implementing code changes.
- [x] (2026-05-13 00:00Z) Incorporated adversarial review feedback into this plan by requiring served-viewer source-line metadata tests, making deterministic fake-`gh` tests the required validation path, and assigning interactive preview regeneration ownership to `start_review_server`.
- [x] (2026-05-13 20:27Z) Confirmed the Markdown renderer emits usable `data-source-line` values for headings and paragraphs in `test_markdown_renderer_emits_source_line_metadata`.
- [x] (2026-05-13 20:27Z) Made Markdown interactive review mode render, sanitize, inject, and serve a temporary HTML viewer through the existing loopback server.
- [x] (2026-05-13 20:27Z) Mapped Markdown draft comments from browser-observed `dom_source_line` to Markdown source lines while preserving legacy HTML `data-comment-id` source lookup.
- [x] (2026-05-13 20:27Z) Added Python tests for Markdown preview, Markdown interactive serving, Markdown source-line submission, Markdown fallback behavior, help text, and legacy HTML compatibility.
- [x] (2026-05-13 20:27Z) Updated CLI/operator docs text to explain Markdown source-of-truth and derived temporary HTML preview behavior.
- [x] (2026-05-13 20:27Z) Ran targeted Python validation and practical repo validation; targeted Python checks passed, `glinter` and `scherzo_lint` completed with existing warnings and no errors, and the first `direnv exec . gleam test` failed in an existing path-relativization test unrelated to this Python-only change.
- [x] (2026-05-13 22:05Z) Rebased the retained implementation workspace onto current `main`, reran validation, and confirmed `python3 -m unittest test/scherzo_execplan_review_test.py -v`, Python compilation, `direnv exec . gleam format --check src test`, `direnv exec . gleam test`, `glinter`, `scherzo_lint`, and ExecPlan validation pass.

## Surprises & Discoveries

- Observation: The wrapper `scripts/scherzo-execplan-review` is already a thin Python entry point that imports `main` from `scripts/scherzo_execplan_review.py`.
  Evidence: The wrapper only imports `main`, imports `sys`, and exits through `main(sys.argv[1:])`.

- Observation: Markdown preview support already exists, but Markdown interactive review is intentionally preview-only in the current tests.
  Evidence: `scripts/scherzo_execplan_review.py` renders Markdown in `prepare_preview`, while `review_pr` starts the loopback review server only when the session's review mode is `interactive-html`; `test/scherzo_execplan_review_test.py` contains `test_markdown_review_mode_remains_preview_only`.

- Observation: The renderer already emits comment metadata that can bridge generated HTML back to Markdown source lines.
  Evidence: `scripts/scherzo-execplan-html` defines `commentable_attrs` and includes `data-comment-id` plus optional `data-source-line` attributes for rendered headings, paragraphs, lists, code blocks, and tables.

- Observation: Renderer metadata tests alone do not prove browser-created drafts will carry Markdown source coordinates.
  Evidence: The served review page passes through sanitizer and review UI injection after rendering; those steps must preserve `data-comment-id` and `data-source-line` for the browser draft flow to work.

- Observation: Temporary review artifacts are already placed under an ignored directory.
  Evidence: `scripts/scherzo_execplan_review.py` defines `DEFAULT_OUTPUT_DIR` as `tmp/scherzo-execplan-review`, and `.gitignore` ignores `tmp/`.

- Observation: Practical `direnv exec . gleam test` validation initially failed in an unrelated review-artifact path relativization test when run from the retained Scherzo workspace before rebasing to current `main`.
  Evidence: The command reported `1265 passed, 1 failures`; the failing test was `review_artifacts_test.verify_evidence_relativizes_absolute_draft_path_test`, expecting `"path": "test/tmp/native-absolute-draft-path/draft.v1.json"` while the retained ledger path contained `.scherzo/workspaces/execplan-implementation/.../workspaces/main/test/tmp/native-absolute-draft-path/draft.v1.json`. The targeted Python review tests still passed with `Ran 23 tests ... OK`. After rebasing onto current `main`, `direnv exec . gleam test` passed with `1291 passed, no failures`.

## Decision Log

- Decision: Treat Markdown and legacy HTML as source kinds, but keep the browser review UI as an HTML viewer in both cases.
  Rationale: Reviewers need one browser interaction model, while GitHub comments must target the actual PR artifact. This avoids committing derived HTML and minimizes UI churn.
  Date: 2026-05-13

- Decision: Use rendered viewer `data-source-line` as the authoritative Markdown mapping and keep `data-comment-id` source lookup as the authoritative legacy HTML mapping.
  Rationale: Generated HTML IDs do not exist in the Markdown file, so searching the Markdown source for `data-comment-id` cannot work. For legacy HTML, the checked-in HTML file is the source, so the existing `data-comment-id` lookup correctly finds HTML diff lines.
  Date: 2026-05-13

- Decision: Prove source-line metadata at the served viewer boundary, not only the renderer boundary.
  Rationale: The browser saves drafts from the sanitized and injected review page. A renderer-only test could pass while the served page strips the attributes needed to populate `DraftComment.dom_source_line`.
  Date: 2026-05-13

- Decision: Make `start_review_server` the single owner of interactive preview regeneration.
  Rationale: Letting both `review_pr` and `start_review_server` call `prepare_review_preview` would risk redundant render/inject work and brittle tests. The command handler should create token and nonce, delegate preparation to server startup, then print metadata and serve.
  Date: 2026-05-13

- Decision: Treat deterministic fake-`gh` Python tests as the required end-to-end validation and keep real or disposable PR exercise optional.
  Rationale: A self-contained implementation plan cannot assume an external PR number, repository, or network state. The fake-`gh` tests can prove preview, serving, draft persistence, payload construction, and legacy compatibility without mutating GitHub.
  Date: 2026-05-13

- Decision: Keep default `preview` behavior read-only and make remote-capable behavior require the existing explicit `review` subcommand plus browser Submit confirmation.
  Rationale: This preserves the current safety boundary and keeps accidental preview runs from mutating GitHub.
  Date: 2026-05-13

- Decision: Preserve legacy HTML support rather than removing it during the Markdown-first change.
  Rationale: Old PRs and branches may still contain `docs/plans/*.html`; maintaining the existing path is cheaper and safer than forcing all in-flight review work to migrate at once.
  Date: 2026-05-13

- Decision: Keep the existing `interactive-html` review mode string while deriving source behavior from `plan_path` helpers.
  Rationale: The server still serves HTML in both source modes, so preserving the existing operator metadata avoids an unnecessary contract break. New helpers `is_markdown_plan`, `is_html_plan`, and `plan_source_kind` make Markdown-specific mapping explicit without renaming the mode.
  Date: 2026-05-13

## Outcomes & Retrospective

Implemented the Markdown-on-demand review flow. Markdown-only ExecPlan PRs can still be previewed read-only, and the explicit `review` subcommand now serves a sanitized, injected temporary HTML viewer for Markdown sources. Browser-created drafts for Markdown use the rendered element's `data-source-line` to submit inline comments against the changed `docs/plans/*.md` source file when the line is present in the PR diff; invalid or non-diff Markdown coordinates fall back to the existing summary-confirmation path. Legacy `docs/plans/*.html` review remains compatible and continues to map by `data-comment-id` in the HTML source rather than by the browser DOM line.

Targeted validation passed: `python3 -m unittest test/scherzo_execplan_review_test.py -v` ran 23 tests successfully, and `python3 -m py_compile scripts/scherzo-execplan-review scripts/scherzo_execplan_review.py test/scherzo_execplan_review_test.py` produced no output. After rebasing the retained workspace onto current `main`, practical repository validation also passed: `direnv exec . gleam format --check src test`, `direnv exec . gleam test` (`1291 passed, no failures`), `direnv exec . gleam run -m glinter` (`0 errors`, existing warnings), `direnv exec . gleam run -m scherzo_lint` (`0 errors`, existing warnings), and `scripts/scherzo-execplan validate docs/plans/LIV-257-render-execplan-review-html-on-demand-from-markdown.md`.

## Context and Orientation

The repository has a local ExecPlan review helper in `scripts/scherzo-execplan-review` and `scripts/scherzo_execplan_review.py`. The wrapper script is the executable command operators run. The module contains the actual implementation: GitHub CLI calls, PR file discovery, temporary session paths, preview rendering, review-server code, draft persistence, line mapping, and submission payload building.

The helper uses the GitHub CLI command `gh` to inspect a pull request. It expects exactly one changed plan file matching `docs/plans/[^/]+.(md|html)`. It downloads that file from the PR head repository and SHA into a session directory below `tmp/scherzo-execplan-review/<repo-key>/pr-<number>/`, where `<repo-key>` is the owner and repository name with the slash replaced by a safe separator. It also writes session metadata, PR file records, drafts, submit payloads, and submit results below the same session root.

`PlanSession` is the dataclass that carries the PR source path and local paths. Its `plan_path` is the repository path of the changed PR artifact, such as `docs/plans/example.md`. Its `source_path` is the downloaded copy of that artifact under `tmp/scherzo-execplan-review/`. Its `preview_path` is the local HTML file the browser opens or the local server serves. For Markdown, `preview_path` should be a derived `.html` file under the session root. For legacy HTML interactive review, `preview_path` remains a sanitized and injected copy under `preview/index.html`, leaving the downloaded source HTML untouched.

`scripts/scherzo-execplan-html` is the renderer used by ExecPlan tooling. Its `render PLAN OUTPUT [DISPLAY_PATH]` command reads Markdown and writes a self-contained HTML artifact. The generated HTML marks reviewable blocks with the `commentable` class, a stable `data-comment-id`, and a `data-source-line` that points back to the Markdown source line. The implementation should rely on this existing renderer rather than introducing a second Markdown renderer.

The interactive review server is implemented with Python standard-library HTTP server classes. It binds to `127.0.0.1`, requires a per-session token for preview and API requests, injects a drawer and JavaScript into sanitized HTML, stores drafts in `drafts.json`, and submits comments through server-side `gh api`. The browser JavaScript never receives GitHub credentials and only talks to the loopback server.

A GitHub inline review comment needs a repository path, a side, and a line that is eligible in the PR diff. The existing code parses the PR file patch to find right-side commentable lines. For this plan, the path must be `docs/plans/example.md` when the PR artifact is Markdown. The source line must be a Markdown source line, not a generated HTML line.

## Preconditions and Verified Facts

The implementation starts from the repository root. `scripts/scherzo-execplan-review` exists and delegates to `scripts/scherzo_execplan_review.py`. `scripts/scherzo_execplan_review.py` defines `PLAN_RE` for both `docs/plans/*.html` and `docs/plans/*.md`, `DEFAULT_OUTPUT_DIR` as `tmp/scherzo-execplan-review`, and helpers including `prepare_plan_session`, `prepare_preview`, `render_markdown`, `prepare_review_preview`, `start_review_server`, `build_review_submission`, `line_for_comment_id`, and `right_side_commentable_lines`.

`prepare_preview(session)` already renders Markdown by calling `render_markdown(session.source_path, session.preview_path, session.plan_path)` and returns `rendered-markdown`; for HTML it returns `html`. The preview command prints metadata including `PLAN_PR_PATH`, `PLAN_SOURCE_PATH`, `PLAN_PREVIEW_KIND`, `PLAN_PREVIEW_PATH`, `PLAN_PREVIEW_URL`, and `PLAN_REMOTE_MUTATIONS`.

`prepare_plan_session(args, review_mode=...)` currently computes an effective interactive mode only for `review` plus a `.html` plan path. When the plan path ends in `.md`, `review_pr` does not start the review server; it renders a preview and exits with `PLAN_REMOTE_MUTATIONS=none`.

`prepare_review_preview(session, token, nonce)` currently reads `session.source_path` as HTML, sanitizes it, injects the review UI, and writes `session.preview_path`. This works for legacy HTML sources but must be adjusted for Markdown sources so it first renders Markdown to HTML and then injects the UI into that rendered HTML.

`build_review_submission(session, drafts, pr_file)` currently reads `session.source_path`, tries to map each draft by searching that source for a matching `data-comment-id`, checks diff eligibility from the PR patch, and builds GitHub review comments whose `path` is `session.plan_path`. This works for legacy HTML because the source contains `data-comment-id`. It cannot map generated Markdown viewer IDs by searching the Markdown source, so Markdown needs a separate source-line rule using `DraftComment.dom_source_line`.

`test/scherzo_execplan_review_test.py` exists and uses `unittest`, a fake `gh` helper, and `ServerHarness` for loopback server tests. It already covers argument parsing, session paths, preview read-only behavior, sanitizer behavior, token/security headers, draft create/update/delete, line mapping, diff parsing, review submission, summary fallback, failed inline submit fallback, and current Markdown preview-only behavior.

`docs/review-artifacts.md` documents local review artifact behavior for Scherzo review workflows. The CLI help text in `scripts/scherzo_execplan_review.py` is also operator-facing documentation because it is what reviewers see when they run the helper with invalid arguments or `--help`. `scripts/scherzo-execplan` already describes Markdown as the primary retained ExecPlan artifact and HTML as derived for the authoring workflow.

## Scope Boundaries

In scope: update `scripts/scherzo_execplan_review.py` so Markdown PR artifacts are first-class in both preview and interactive review mode; render Markdown to temporary HTML under `tmp/scherzo-execplan-review/`; inject and serve the existing review UI for Markdown-derived HTML; persist drafts against the Markdown `plan_path`; submit inline comments to eligible Markdown diff lines; keep summary fallback behavior; preserve legacy HTML preview and interactive review behavior; update Python tests in `test/scherzo_execplan_review_test.py`; and update operator-facing text in the CLI help plus a concise non-plan docs note.

Out of scope: changing ExecPlan authoring back to HTML; bulk converting old plans; changing the visual design of the viewer beyond any small labels needed to clarify source and preview paths; replacing the existing renderer; adding third-party Python dependencies; changing the GitHub review API shape except as required to target Markdown paths; and editing existing `docs/plans/*` artifacts.

The implementation must not create a checked-in `docs/plans/*.html` file. A local generated file named like `tmp/scherzo-execplan-review/owner__repo/pr-123/docs/plans/example.html` is acceptable because it is under `tmp/` and is derived from the downloaded Markdown. If a PR changes both a Markdown plan and a generated HTML plan, keep the current fail-fast behavior for multiple plan artifacts and make the error text tell the operator to review the Markdown-only PR or remove the generated HTML artifact.

## Milestones

Milestone 1 proves the Markdown renderer provides usable source-line metadata. At the end of this milestone, a unit test renders a small Markdown plan to temporary HTML, verifies that a heading or paragraph has `data-source-line` equal to the original Markdown line, and records a draft whose `dom_source_line` can be used as a Markdown line. This comes first because the whole design depends on generated viewer elements carrying source-line coordinates.

Milestone 2 makes interactive review preparation source-kind aware. At the end of this milestone, `review` mode for `docs/plans/*.md` produces a rendered and injected HTML viewer under the session root, starts the same loopback server path as legacy HTML, and prints metadata that still identifies `PLAN_PR_PATH` as the Markdown file. `start_review_server` is the single function that regenerates and injects the interactive preview; `review_pr` does not call `prepare_review_preview` directly. The served Markdown-derived HTML still exposes each commentable block's `data-comment-id` and `data-source-line` attributes after sanitization and injection. Legacy HTML still uses a sanitized injected copy of the downloaded HTML source.

Milestone 3 changes submission mapping. At the end of this milestone, Markdown drafts submit inline comments with `path: docs/plans/example.md` and `line` equal to a Markdown source line when that line is present in the PR patch. Drafts without a valid or diff-eligible Markdown line still go to the existing summary fallback. Legacy HTML mapping remains unchanged and continues to anchor against the HTML source file's diff lines.

Milestone 4 hardens tests and docs. At the end of this milestone, the Python test suite covers Markdown preview, Markdown interactive serving, Markdown source-line mapping, fallback behavior, legacy HTML compatibility, and operator-facing messaging. The docs and CLI help make clear that HTML is a temporary derived viewer and Markdown is the source of truth.

Milestone 5 performs validation and records outcomes. At the end of this milestone, targeted Python tests, Python compilation, and practical repo validation pass. This ExecPlan's living sections are updated with what actually happened, including any compatibility limitations found during implementation.

## Plan of Work

In `scripts/scherzo_execplan_review.py`, add small source-kind helpers near the constants. Define `is_markdown_plan(plan_path: str) -> bool`, `is_html_plan(plan_path: str) -> bool`, and `plan_source_kind(plan_path: str) -> str` returning `markdown` or `legacy-html`. These helpers keep conditionals readable and make tests assert the intended source-of-truth behavior.

Update `prepare_plan_session`. Keep `PLAN_RE` accepting `.md` and `.html`, and keep the exactly-one-plan-file rule. Change the effective review mode so the explicit `review` subcommand creates an interactive review session for both Markdown and HTML plan paths. The session may continue to use the existing `review_mode` value `interactive-html`, because the browser viewer is HTML in both cases, but the implementation should derive source behavior from `plan_path` rather than from `review_mode` alone. If a new value is clearer, use `interactive-viewer` and update the tests consistently.

Update `make_plan_session` only as needed to ensure Markdown interactive sessions write their preview to a derived `.html` path under the session root and legacy HTML interactive sessions write to `preview/index.html`. Do not make Markdown interactive previews point at `docs/plans/*.html` in the repository. `source_path` must remain the downloaded Markdown file for Markdown sessions.

Refactor preview preparation into an explicit helper such as `render_preview_html(session: PlanSession) -> str`. For Markdown, this helper calls the existing `render_markdown` with the downloaded Markdown source, the temporary HTML preview path, and the repository display path. It returns `rendered-markdown`. For HTML, it returns `html` and does not modify the downloaded source file in preview mode. Then update `prepare_preview(session)` to call this helper and keep existing read-only preview behavior.

Refactor `prepare_review_preview(session, token, nonce)` so it chooses the HTML input by source kind. For Markdown, call `render_markdown` first, read the generated HTML from `session.preview_path`, sanitize that HTML, inject the review UI, and write the injected result back to `session.preview_path`. For legacy HTML, read `session.source_path`, sanitize it, inject the review UI, and write to the separate interactive `session.preview_path`. In both cases, include `planPath` as the original PR artifact path in the injected config.

Make `start_review_server` the single owner of interactive preview preparation. It always calls `prepare_review_preview(session, token, nonce)` for an interactive session instead of skipping preparation when `session.preview_path` already exists. This prevents a stale non-injected Markdown preview from being served if the same output directory was previously used for preview mode. The preparation step is idempotent because it regenerates the viewer from `session.source_path` each time. Do not also call `prepare_review_preview` from `review_pr`.

Update `review_pr(args)`. After `prepare_plan_session(args, review_mode="review")`, both Markdown and legacy HTML sessions should take the server path. Generate token and nonce, call `start_review_server` so it prepares and serves the preview, print metadata, and block in `serve_forever` until interrupted. The printed metadata should make the relationship explicit: `PLAN_PR_PATH` is the source artifact; `PLAN_PREVIEW_KIND` can be `interactive-rendered-markdown` for Markdown or `interactive-html` for legacy HTML; `PLAN_REMOTE_MUTATIONS` remains `available-after-browser-submit`; and `PLAN_REVIEW_MODE=interactive-html` may remain if that is the existing operator contract.

Add a helper such as `source_line_for_draft(session: PlanSession, draft: DraftComment, source_text: str) -> int | None`. If `session.plan_path` is Markdown, return `draft.dom_source_line` only when it is a positive integer within the downloaded Markdown source's line count. Do not search Markdown text for `data-comment-id`. If `session.plan_path` is legacy HTML, keep the existing `line_for_comment_id(source_text, draft.data_comment_id)` behavior and return `None` when the ID is missing or duplicated. Update `build_review_submission` to call this helper, store the result in `draft.source_line`, and keep the existing diff-eligibility and fallback behavior.

Update `empty_drafts_document` and `write_session_json` only if helpful for clarity. If adding fields, use additive names such as `source_kind` and `preview_is_derived`. Existing tests and retained artifacts should continue to work when old JSON files lack those fields. Do not change the meaning of `plan_path`; it must continue to mean the PR artifact path that comments target.

Update operator-facing text. In the module docstring and `argparse` description, say that Markdown plans are the primary PR artifact, HTML is rendered temporarily under `tmp/scherzo-execplan-review/`, and legacy HTML plan PRs are still supported. Add a short note to `docs/review-artifacts.md` explaining that ExecPlan PR review previews are local derived viewers and that submitted inline comments target the changed `docs/plans/*.md` source when the PR artifact is Markdown. Do not edit existing files under `docs/plans/` as part of the implementation.

## Concrete Steps

1. From the repository root, run the targeted existing Python test command to establish the starting point:

        python3 -m unittest test/scherzo_execplan_review_test.py -v

   Expect the current suite to pass before changes. If it fails for unrelated local environment reasons, record the failure in this plan before continuing.

2. In `test/scherzo_execplan_review_test.py`, add a Markdown fixture constant near `FIXTURE_HTML`, for example `FIXTURE_MARKDOWN` with `# Example`, a blank line, and a paragraph. Add a helper on `FakeGh` or reuse its existing constructor parameters so tests can serve `docs/plans/example.md` content and a patch containing Markdown lines.

3. In `test/scherzo_execplan_review_test.py`, add `test_markdown_preview_mode_downloads_renders_and_reports_source`. Use `FakeGh` with `plan_path="docs/plans/example.md"`, Markdown content, and a patch. Parse args for `preview 123 --repo owner/repo --no-open --output-dir <test-output>`, run `review.preview_pr(args)` with fake `gh` on `PATH`, and assert the output contains `PLAN_PR_PATH=docs/plans/example.md`, `PLAN_PREVIEW_KIND=rendered-markdown`, and `PLAN_REMOTE_MUTATIONS=none`. Assert the preview path printed or stored by the session points under the temporary output directory and ends in `.html`. Assert fake `gh` recorded read-only calls only.

4. In `test/scherzo_execplan_review_test.py`, add a renderer-focused test that creates a Markdown session with `plan_path="docs/plans/example.md"`, calls the review module's Markdown render helper, reads the generated preview HTML, and asserts it contains a commentable heading with `data-source-line="1"` and a commentable paragraph with the expected Markdown source line. Run the new test now and expect it to fail until the helper exists or is exposed.

5. In `scripts/scherzo_execplan_review.py`, add `is_markdown_plan`, `is_html_plan`, and `plan_source_kind` near `PLAN_RE`. Keep them pure string helpers with no filesystem access.

6. In `scripts/scherzo_execplan_review.py`, update `prepare_plan_session` so `review_mode="review"` creates an interactive viewer session for Markdown as well as HTML. Keep `preview` and bare invocation read-only.

7. In `scripts/scherzo_execplan_review.py`, add `render_preview_html(session)` or an equivalently named helper. Implement Markdown by delegating to `render_markdown`; implement HTML by returning the existing `html` kind. Update `prepare_preview(session)` to call it.

8. Run:

        python3 -m unittest test/scherzo_execplan_review_test.py -v

   Expect the new Markdown preview test and renderer metadata test to pass if milestones 1 and 2 foundations are correct, while interactive server and submission tests may still be missing.

9. In `scripts/scherzo_execplan_review.py`, update `prepare_review_preview` so Markdown sessions render Markdown first, then sanitize and inject the generated HTML. Keep the legacy HTML branch reading from `session.source_path` and writing to the interactive `session.preview_path`.

10. In `scripts/scherzo_execplan_review.py`, update `start_review_server` to always regenerate the injected review preview from `session.source_path` when a server starts. Remove or bypass the existing preview-path-exists shortcut for interactive sessions. If `review_pr` currently calls `prepare_review_preview`, remove that direct call so server startup is the single preparation owner.

11. In `scripts/scherzo_execplan_review.py`, update `review_pr` so Markdown review mode creates token and nonce, starts the server, and prints the same remote mutation safety marker as HTML interactive review without directly preparing the preview. Preserve the Ctrl-C shutdown behavior.

12. In `test/scherzo_execplan_review_test.py`, replace `test_markdown_review_mode_remains_preview_only` with a test for Markdown interactive review preparation. Build or download a Markdown session, assert the session is interactive, start `ServerHarness`, request `/?token=test-token`, and assert HTTP 200, the review drawer markup, rendered plan text, and the expected security headers. Also assert the served, sanitized response contains a `.commentable` block with a stable `data-comment-id` and the expected `data-source-line="1"` for the heading and `data-source-line="3"` for the paragraph. Assert tokenless requests still return HTTP 403. Close the harness in `finally`.

13. In `test/scherzo_execplan_review_test.py`, add `test_markdown_review_drafts_submit_against_markdown_line`. Use a Markdown session whose PR patch includes line 1 and line 3. Start `ServerHarness`, fetch the served viewer with the token, parse the heading's `data-comment-id` and `data-source-line` from the served HTML, and create a draft through the existing `/api/drafts` endpoint using those parsed values, `dom_tag="h1"`, and a body. Then call `build_review_submission` with the saved PR file record. Assert the first inline comment is exactly for `path: docs/plans/example.md`, `side: RIGHT`, and `line: 1`, and that the body contains the draft text plus the selected block footer.

14. In `test/scherzo_execplan_review_test.py`, add `test_markdown_missing_or_non_diff_source_line_uses_summary_fallback`. Create one draft with no `dom_source_line` and one draft with a Markdown source line outside the patch. Assert `build_review_submission` does not create inline comments for those drafts and returns a summary-only or mixed fallback submission according to whether any other inline-eligible draft exists.

15. In `scripts/scherzo_execplan_review.py`, add `source_line_for_draft(session, draft, source_text)` and update `build_review_submission` to use it. For Markdown, validate `draft.dom_source_line` against the Markdown source line count. For legacy HTML, keep `line_for_comment_id` and do not use `dom_source_line` as an HTML fallback.

16. In `test/scherzo_execplan_review_test.py`, add `test_legacy_html_mapping_ignores_markdown_dom_line_fallback`. Use `FIXTURE_HTML` where `data-comment-id="heading-purpose"` is on the known HTML source line, create a draft with the same comment id but an intentionally different `dom_source_line`, and assert the submitted inline comment uses the HTML source line found by `line_for_comment_id`. This prevents Markdown mapping from breaking legacy HTML.

17. In `test/scherzo_execplan_review_test.py`, update any expectations that currently say Markdown review has `PLAN_REMOTE_MUTATIONS=none` in `review` mode. The default preview command should still assert `none`; the explicit `review` subcommand should now assert `available-after-browser-submit` when tested through nonblocking preparation helpers or server harness.

18. In `scripts/scherzo_execplan_review.py`, update the top-level docstring, `argparse` description, examples, and any compatibility error text so operators understand that Markdown is primary, HTML is a temporary derived viewer, and legacy HTML PRs are supported. Do not remove existing preview examples.

19. In `docs/review-artifacts.md`, add a concise section or paragraph outside `docs/plans/` explaining the ExecPlan PR review preview relationship: source artifacts are `docs/plans/*.md`, temporary viewers live under `tmp/scherzo-execplan-review/`, and inline submissions target the source artifact and source line when possible. Keep the wording local-only and avoid promising that review artifacts are durable remote state.

20. Run the targeted Python tests:

        python3 -m unittest test/scherzo_execplan_review_test.py -v

   Expect all tests in `test/scherzo_execplan_review_test.py` to pass. The final output should end with `OK`.

21. Run Python compilation:

        python3 -m py_compile scripts/scherzo-execplan-review scripts/scherzo_execplan_review.py test/scherzo_execplan_review_test.py

   Expect no output and exit code 0.

22. If `direnv` is available, run practical repository validation from the repository root:

        direnv exec . gleam test
        direnv exec . gleam run -m glinter
        direnv exec . gleam run -m scherzo_lint

   If `direnv exec .` reports that `.envrc` is blocked, inspect `.envrc`, run `direnv allow .`, and retry. If these checks are not practical for this Python-only change, record why in this plan and at minimum run the targeted Python checks above.

23. Treat steps 20 and 21 as the required end-to-end validation for this Python-only workflow. They use deterministic fake-`gh` data and do not depend on an external PR, network state, or GitHub mutation capability. If a safe real or disposable PR already exists, optionally exercise Markdown preview mode by substituting its values here:

        scripts/scherzo-execplan-review preview <pr-number> --repo <owner>/<repo> --no-open

   Expect output containing `PLAN_PR_PATH=docs/plans/example.md` or the actual Markdown plan path, `PLAN_PREVIEW_KIND=rendered-markdown`, `PLAN_REMOTE_MUTATIONS=none`, and a preview path under `tmp/scherzo-execplan-review/`. This optional check is not required for acceptance when the fake-`gh` tests pass.

24. Optionally exercise interactive Markdown mode only with a safe real or disposable PR that changes exactly one Markdown plan, substituting its values here:

        scripts/scherzo-execplan-review review <pr-number> --repo <owner>/<repo> --no-open

   Expect output containing the Markdown `PLAN_PR_PATH`, a local `PLAN_PREVIEW_URL` on `127.0.0.1`, `PLAN_REVIEW_SERVER`, and `PLAN_REMOTE_MUTATIONS=available-after-browser-submit`. Open the URL, create a draft on a rendered block, submit only against a safe target, and confirm the retained or fake GitHub payload comments target the Markdown source path at the Markdown source line.

25. Optionally exercise legacy HTML compatibility only with a safe real or disposable PR that changes exactly one HTML plan. Run preview and review mode, confirm the preview is still served, drafts can be stored, and inline payloads target the `docs/plans/*.html` source path when the HTML diff line is eligible. If a legacy feature cannot be supported, change the tool to fail with an explicit compatibility message and update this plan's Outcomes & Retrospective.

26. Update this ExecPlan's Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective as implementation proceeds. Commit only after the relevant tests pass. A good final commit message is `Render ExecPlan review UI from Markdown source`.

## Testing and Falsifiability

The Python test suite is the primary falsification mechanism. A passing implementation must add or update tests in `test/scherzo_execplan_review_test.py` so the old behavior, where Markdown review mode remains preview-only, would fail. The specific scenarios are as follows.

The Markdown preview test proves the default command remains read-only. It uses fake `gh` data for one changed `docs/plans/example.md`, runs `preview` with `--no-open`, and asserts `PLAN_PR_PATH=docs/plans/example.md`, `PLAN_PREVIEW_KIND=rendered-markdown`, `PLAN_REMOTE_MUTATIONS=none`, and no fake `gh` mutation calls.

The renderer metadata test proves the source-line assumption. It renders Markdown containing a heading on line 1 and a paragraph on line 3, then asserts the generated HTML includes commentable elements with `data-source-line="1"` and `data-source-line="3"`. If this fails, do not build source-line submission on top of the renderer until the renderer or plan is revised.

The Markdown interactive server test proves `review` mode is no longer preview-only and that the browser receives usable mapping metadata. It starts the loopback server for a Markdown session, requests the preview with the token, and asserts the response includes the rendered plan, `scherzo-review-drawer`, the same security headers tested for legacy HTML, and commentable elements whose `data-comment-id` and `data-source-line="1"` or `data-source-line="3"` attributes survived sanitization and injection. It also asserts tokenless requests still return HTTP 403.

The Markdown submission test proves comments target the source artifact from browser-observed metadata. It fetches the served Markdown-derived viewer, parses a generated viewer element's `data-comment-id` and `data-source-line`, creates a draft through `/api/drafts` with that parsed `dom_source_line`, uses a PR patch where line 1 is commentable, and asserts `build_review_submission` creates an inline comment with `path: docs/plans/example.md`, `side: RIGHT`, and `line: 1`. This test must fail if the served page strips source-line metadata or if the code searches the Markdown source for `data-comment-id` instead of using `dom_source_line`.

The Markdown fallback tests prove bad coordinates do not create wrong inline comments. Drafts with missing `dom_source_line`, a non-positive line, a line past the end of the Markdown source, or a line not present in the right-side diff must become fallback entries and require the existing summary confirmation when no inline-safe comments remain.

The legacy HTML compatibility test proves old PRs still behave as before. It creates a legacy HTML session with a `data-comment-id` on a known HTML source line and an intentionally different `dom_source_line`. The expected inline payload line is the HTML source line found by `line_for_comment_id`, not the DOM line. This catches accidental use of Markdown mapping for HTML sources.

The docs/help test can be lightweight. If testing help output is simple, call `parse_args(["--help"])` under `assertRaises(SystemExit)` and capture stdout to assert it mentions Markdown source and temporary HTML preview. If this makes the test brittle, rely on direct review of the docstring and `argparse` description rather than adding a brittle assertion.

Run the targeted tests with:

        python3 -m unittest test/scherzo_execplan_review_test.py -v

Before implementation, at least the updated Markdown interactive tests should fail because Markdown review mode is preview-only. After implementation, all tests in that file should pass. Run compilation with:

        python3 -m py_compile scripts/scherzo-execplan-review scripts/scherzo_execplan_review.py test/scherzo_execplan_review_test.py

No output and exit code 0 means the Python files compile.

The plan is false if any of these happen after implementation: `review` mode for Markdown exits without starting the server, submitted inline payloads target `docs/plans/example.html` for a Markdown PR, submitted inline payloads use generated HTML line numbers rather than Markdown source lines, legacy HTML drafts stop mapping to HTML diff lines, or the default preview command performs a GitHub mutation.

## Validation and Acceptance

The required acceptance path is deterministic and local. The fake-`gh` tests in `test/scherzo_execplan_review_test.py` must prove preview rendering, interactive serving, draft persistence, submission payload construction, fallback behavior, and legacy HTML compatibility without depending on an external PR or mutating GitHub. Manual exercise against a real or disposable PR is useful when a safe target already exists, but it is optional and must not be the only proof that the implementation works.

Acceptance for the default preview path is the test `test_markdown_preview_mode_downloads_renders_and_reports_source`. It supplies fake data for one changed `docs/plans/example.md`, runs preview mode with `--no-open`, and expects `PLAN_PR_PATH=docs/plans/example.md`, `PLAN_PREVIEW_KIND=rendered-markdown`, `PLAN_REMOTE_MUTATIONS=none`, and `OPENED=false`. The test must also assert that `PLAN_SOURCE_PATH` is the downloaded Markdown copy under the local output directory, `PLAN_PREVIEW_PATH` is a derived temporary `.html` file under the same `tmp/scherzo-execplan-review/` session root, and the fake GitHub activity is read-only.

Acceptance for the interactive Markdown path is the replacement for `test_markdown_review_mode_remains_preview_only`. It supplies fake data for one changed `docs/plans/example.md`, starts the loopback server, requests the tokenized preview, and expects HTTP 200, rendered plan text, `scherzo-review-drawer`, preserved `data-comment-id`, preserved `data-source-line` attributes for the heading and paragraph, and the existing security headers. Tokenless requests must still return HTTP 403. The command-level metadata path must print the Markdown `PLAN_PR_PATH`, a local `PLAN_PREVIEW_URL` on `127.0.0.1`, `PLAN_REVIEW_SERVER`, `PLAN_REVIEW_DRAFTS_PATH`, and `PLAN_REMOTE_MUTATIONS=available-after-browser-submit` when exercised through nonblocking helpers or an optional manual run.

Acceptance for inline submission is `test_markdown_review_drafts_submit_against_markdown_line`. It creates a draft through the server draft API using `data-comment-id` and `data-source-line` parsed from the served viewer. The retained or constructed `submit-payload.json` must contain a `comments` entry with `path` equal to `docs/plans/example.md`, `side` equal to `RIGHT`, and `line` equal to the Markdown source line from the selected block's `data-source-line`. The payload must not target a generated `.html` file. If no selected block maps to a diff-eligible Markdown line, the UI and submission builder must require explicit summary-only confirmation and post an issue comment rather than creating a wrong inline review.

Acceptance for legacy HTML compatibility is `test_legacy_html_mapping_ignores_markdown_dom_line_fallback`. It uses fake data for one changed `docs/plans/example.html` and proves preview mode remains previewable, interactive review mode remains available, and inline payloads continue to target `docs/plans/example.html` and HTML source lines when the HTML diff line is eligible. If a specific legacy HTML artifact lacks comment metadata and cannot support inline review, the tool must fail or fall back with an explicit message that names the compatibility limitation.

Acceptance for optional manual validation, when a safe real or disposable PR exists, uses placeholders that must be substituted by the operator:

        scripts/scherzo-execplan-review preview <pr-number> --repo <owner>/<repo> --no-open
        scripts/scherzo-execplan-review review <pr-number> --repo <owner>/<repo> --no-open

The PR must change exactly one `docs/plans/*.md` file for Markdown validation or exactly one `docs/plans/*.html` file for legacy validation. The preview command should be read-only. The review command should keep running until Ctrl-C and expose the local review drawer. Submit only against a safe target and confirm retained payloads target the source artifact and source line. Skip this optional validation when no safe PR exists.

Acceptance for validation: `python3 -m unittest test/scherzo_execplan_review_test.py -v` passes, `python3 -m py_compile scripts/scherzo-execplan-review scripts/scherzo_execplan_review.py test/scherzo_execplan_review_test.py` passes, and practical repo validation through `direnv exec . gleam test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint` passes or is documented as not practical for a Python-only change.

## Rollout, Recovery, and Idempotence

Rollout is additive. Keep the existing default preview command read-only. The only remote-capable path remains the explicit `review` subcommand plus browser Submit confirmation. If Markdown interactive review has a defect, reviewers can immediately fall back to `scripts/scherzo-execplan-review preview PR_NUMBER --no-open` and manually review the Markdown or temporary preview without remote mutation.

The local generated viewer is safe to recreate. Starting review mode can overwrite the temporary `PLAN_PREVIEW_PATH` from the downloaded source each time. Drafts are already tied to repository, PR number, plan path, head SHA, and source hash; stale drafts should continue to be backed up or ignored by the existing draft-loading logic when the source changes.

Recovery from bad local state is to stop the server with Ctrl-C and remove the relevant session directory under `tmp/scherzo-execplan-review/`. Recovery from an unwanted GitHub comment remains manual through GitHub's PR UI or API, which is why the implementation must keep explicit browser confirmation and retained submit payload artifacts.

If implementation stops after Markdown preview changes but before interactive submission mapping, the tool should still be safe because default preview remains read-only and Markdown review mode should either still be preview-only or be blocked by failing tests. Do not ship a state where Markdown review mode serves a submit-capable UI but maps all drafts to fallback or generated HTML paths without an explicit compatibility message.

## Artifacts and Notes

The implementation should retain the existing artifact layout under `tmp/scherzo-execplan-review/<repo-key>/pr-<number>/`. For a Markdown plan, important files are the downloaded Markdown source at a path mirroring `docs/plans/example.md`, the derived preview HTML at the matching `.html` path under the same session root, `session.json`, `pr-files.json`, `drafts.json`, `submit-payload.json`, `submit-result.json`, and `submit-error.json` when applicable.

The printed metadata should make the distinction easy to inspect. A representative Markdown preview run should include values like these, with local absolute paths represented here as placeholders:

        PR_NUMBER=123
        REPO=owner/repo
        PLAN_PR_PATH=docs/plans/example.md
        PLAN_SOURCE_PATH=<absolute-local-path-to-session>/docs/plans/example.md
        PLAN_PREVIEW_KIND=rendered-markdown
        PLAN_PREVIEW_PATH=<absolute-local-path-to-session>/docs/plans/example.html
        PLAN_REMOTE_MUTATIONS=none

A representative Markdown interactive run should include the Markdown source path and a loopback URL:

        PLAN_PR_PATH=docs/plans/example.md
        PLAN_PREVIEW_KIND=interactive-rendered-markdown
        PLAN_REVIEW_MODE=interactive-html
        PLAN_REVIEW_SERVER=http://127.0.0.1:<port>/
        PLAN_PREVIEW_URL=http://127.0.0.1:<port>/?token=<session-token>
        PLAN_REMOTE_MUTATIONS=available-after-browser-submit

The exact `PLAN_PREVIEW_KIND` string can differ if the implementation keeps an existing value for compatibility, but tests and docs must agree. Do not print or store GitHub tokens in the preview HTML other than the per-session loopback token needed for local API requests.

## Interfaces and Dependencies

Use only Python standard-library modules already used by `scripts/scherzo_execplan_review.py`, plus any existing imports needed for small helper functions. Do not add a package dependency for Markdown parsing; use `scripts/scherzo-execplan-html` through the existing `render_markdown` helper.

The review module should expose or contain these stable helpers by the end of implementation:

        def is_markdown_plan(plan_path: str) -> bool: ...
        def is_html_plan(plan_path: str) -> bool: ...
        def plan_source_kind(plan_path: str) -> str: ...
        def render_preview_html(session: PlanSession) -> str: ...
        def source_line_for_draft(session: PlanSession, draft: DraftComment, source_text: str) -> int | None: ...

The exact helper names may vary if the existing module has a better naming convention, but the responsibilities must exist and be tested. `PlanSession.plan_path` must remain the repository path used for GitHub review comments. `DraftComment.dom_source_line` must remain the line captured from the viewer element's `data-source-line`. `ReviewSubmission.inline_comments` must continue to hold dictionaries with `path`, `body`, `side`, and `line` fields accepted by the GitHub create-review API.

External tools are `gh` for GitHub API reads and submissions, `python3` for tests and compilation, `direnv` for repository validation when available, and the existing Gleam toolchain provided by the repository environment. The implementation should not require network access for unit tests because the fake `gh` harness supplies deterministic responses.

## Commit Map

Commit 1 should contain source-kind helpers, Markdown preview tests, and any refactor that keeps preview behavior passing. Validate with `python3 -m unittest test/scherzo_execplan_review_test.py -v` before committing.

Commit 2 should contain Markdown interactive review serving and server-preparation changes. Validate with the targeted Python tests and Python compilation before committing.

Commit 3 should contain Markdown submission mapping, fallback behavior, and legacy HTML compatibility tests. Validate with the targeted Python tests and Python compilation before committing.

Commit 4 should contain CLI/operator docs text and final validation updates. Validate with the targeted Python tests and practical repo validation before committing.

## Open Questions and Clarifications Needed

None.
