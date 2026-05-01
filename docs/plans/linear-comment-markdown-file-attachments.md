# Support Linear UI-style Markdown file attachments on comments

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

After this change, Scherzo can upload a Markdown file to Linear and attach it to an existing issue comment so Linear renders it like a file attached through the Linear web UI. Operators will be able to attach a local `.md` file to a known comment for verification, and Scherzo handoff can optionally attach the generated success result as a Markdown file to the success comment it posts.

The visible result is not an issue-level attachment card. The target Linear comment's rich-text document gains a top-level `file` node with the uploaded asset URL, and the Linear UI projects that node into the comment as a native file attachment / Markdown link. Existing comment text and existing file nodes remain present.

## Problem Framing and Constraints

Linear has two different attachment surfaces. Issue-level `Attachment` entities appear as separate cards on the issue. The Linear UI's comment file uploads are different: Linear stores them inside the comment rich-text document as ProseMirror `file` nodes in `Comment.bodyData`. ProseMirror is a JSON document model used by rich-text editors. Linear exposes `Comment.bodyData` in its GraphQL API as a JSON string, but Linear marks it internal, meaning it may change without a public stability guarantee.

Scherzo currently posts Linear comments through GraphQL mutations in `src/scherzo/linear.gleam` and `src/scherzo/handoff.gleam`, but it only sends Markdown text in `body`. It cannot upload a generated artifact and attach it to a specific existing comment in the same style the Linear UI uses. This hurts review and operator handoff because longer Markdown results either have to be pasted inline, truncated, or manually uploaded through the Linear UI.

This plan supports Markdown artifacts only. It intentionally does not add a general binary attachment product, does not add a Linear SDK dependency, and does not replace existing issue comments. The implementation must be additive, default-off for automatic handoff attachments, and must keep the ordinary Markdown-link fallback available because `bodyData` is internal.

## Strategy Overview

Implement a small Linear comment attachment client in Gleam using the existing GraphQL transport style. The client will fetch the target comment, parse its `bodyData` JSON string into a small repository-owned JSON value type, preserve the existing document, upload Markdown bytes through Linear's `fileUpload` mutation and the returned pre-signed upload URL, then update the comment's `bodyData` with one added top-level `file` node.

Keep the body-data manipulation separate from HTTP. A new pure module will parse, validate, append file nodes, detect existing file nodes for safe handoff retries, and encode the modified JSON. A separate attachment module will orchestrate Linear GraphQL requests, upload PUT requests, fallback link updates, and file reading. This is proportionate because the risky part is a small document transformation and a three-request upload flow; it does not require changing Scherzo's workflow runner, pi integration, or local control API.

Expose the feature in two places. First, add a narrow top-level diagnostic mode that attaches a local `.md` file to an existing comment by id. This gives a human-observable acceptance path without running a full daemon. Second, add opt-in handoff configuration so a success result can be uploaded as `<issue>-<run-id>-result.md` and attached to the success comment Scherzo creates. Existing handoff behavior remains unchanged unless the new option is enabled.

## Alternatives Considered

The simplest alternative is to append `[filename.md](asset-url)` to `Comment.body` only. That is useful as a fallback, but it does not meet the goal because it is not the same storage shape the Linear UI uses for comment file attachments and it does not exercise `bodyData` preservation.

Using issue-level attachments was rejected because LIV-26 explicitly states that Linear UI comment uploads are not represented as issue-level `Attachment` entities. Issue-level attachments would render differently and would not attach to the comment being used for Scherzo handoff.

Adding the official Linear SDK or a Node helper was rejected as disproportionate. Scherzo is currently a Gleam/Erlang project with direct GraphQL request builders and `gleam_httpc`. The needed operations are a few GraphQL queries/mutations plus one HTTP PUT, so a second runtime would add packaging and failure modes without reducing the main `bodyData` risk.

Trying to fully model ProseMirror was rejected. The implementation only needs to preserve arbitrary JSON and append one top-level `file` node to a document whose top-level shape is `{ "type": "doc", "content": [...] }`. A repository-owned generic JSON value plus small helpers is enough.

## Risks and Countermeasures

The main product risk is Linear changing or removing `Comment.bodyData`. The countermeasure is to isolate all native attachment logic in the new body-data and attachment modules, return a clear `linear_attachment_error` if native mode cannot proceed, and support a fallback mode that updates `Comment.body` with a normal Markdown link after upload. Documentation must say this fallback exists because `bodyData` is internal.

The main data-loss risk is overwriting a comment rich-text document and losing existing text, existing file nodes, or unknown rich-text node types. The countermeasure is a generic JSON representation that preserves objects, arrays, strings, numbers, booleans, and nulls; tests must prove an existing file node and paragraph remain after appending a new file node. The update must modify only the top-level `content` array by appending one node.

The main upload risk is creating an uploaded asset and then failing to update the comment, leaving an unattached upload in Linear's storage. The countermeasure is to parse and validate the comment's `bodyData` before requesting an upload, and in native mode to keep the parsed document or append plan so final bodyData construction cannot newly fail after the PUT succeeds. When fallback is enabled, decide the update mode before upload. If dedupe is enabled and fallback mode is needed, also check the existing comment `body` for the deterministic Markdown-link filename before upload so retries do not append duplicate fallback links. If the final `commentUpdate` fails after a successful upload, report a clear error and do not attempt a second unsafe update automatically. Retrying may upload another asset; handoff retries reduce duplicates by using a deterministic filename and checking for an existing native file node or fallback Markdown link with that filename before uploading.

The main ordering risk is surprising users by moving comment text. The plan appends new file nodes at the end of the top-level `content` array rather than prepending them. This preserves the visible reading order of existing comments. Linear UI examples may place file nodes before paragraphs depending on when the user attached the file, but native rendering is tied to the `file` node type, not to a specific position.

The main concurrent-edit risk is overwriting a manual comment edit made between Scherzo's fetch and update. Linear's verified schema did not expose a compare-and-swap token for `commentUpdate`, so this plan does not attempt full optimistic concurrency control. The mitigation is to keep the fetch-to-update window short, use the feature automatically only on the success comment Scherzo just created, document that the diagnostic command should be run against a quiescent comment, and keep fallback body updates explicit because they may normalize rich text to Markdown when native bodyData is unavailable.

The main secret-handling risk is leaking the Linear API key to the upload URL. The countermeasure is to use the existing GraphQL `Authorization` header only for GraphQL requests and to send only the headers returned by `fileUpload` plus `Content-Type` to the pre-signed upload URL. Tests must assert that upload requests do not include `Authorization`.

The main size risk is sending an incorrect `size` to `fileUpload` for Unicode Markdown. The countermeasure is to read files as `BitArray` with `simplifile.read_bits` and compute size with `gleam/bit_array.byte_size`, not `string.length`.

The main state-handoff risk is changing existing success comments or state transitions. The countermeasure is an opt-in config field, defaulting to `False`, and tests proving existing handoff still posts a single structured result comment when attachment is disabled.

## Progress

- [x] (2026-05-01 03:21Z) Loaded Linear issue LIV-26 and captured its background, proposed flow, acceptance criteria, and internal-API warning.
- [x] (2026-05-01 03:21Z) Queried Linear schema facts for `Comment.bodyData`, `fileUpload`, `UploadPayload`, `UploadFileHeader`, `commentUpdate`, and `CommentUpdateInput.bodyData` without printing secrets.
- [x] (2026-05-01 03:21Z) Verified the LIV-20 example comment contains top-level `file` nodes in `bodyData` and Linear projects them into `Comment.body` as Markdown links.
- [x] (2026-05-01 03:21Z) Re-read current Scherzo files relevant to Linear comments and handoff: `src/scherzo/linear.gleam`, `src/scherzo/handoff.gleam`, `src/scherzo/handoff_format.gleam`, `src/scherzo/domain.gleam`, `src/scherzo/error.gleam`, `src/scherzo/main.gleam`, `src/scherzo/orchestrator/service.gleam`, and current Linear/handoff tests.
- [x] (2026-05-01 03:21Z) Approved the workspace `.envrc` after inspection and ran `direnv exec . gleam test`; the baseline suite reported `464 passed, no failures`.
- [x] (2026-05-01 03:21Z) Confirmed `jj status --ignore-working-copy` reported no working-copy changes before this plan was written.
- [x] (2026-05-01 03:21Z) Authored this ExecPlan as `docs/plans/linear-comment-markdown-file-attachments.md`.
- [x] (2026-05-01 03:50Z) Reviewed the ExecPlan and tightened the upload-order, fallback idempotence, diagnostic error, and handoff transport-injection guidance.
- [x] (2026-05-01 04:10Z) Implemented pure bodyData parsing, file-node appending, fallback-link helpers, and unit tests in `src/scherzo/linear_body_data.gleam` and `test/linear_body_data_test.gleam`.
- [x] (2026-05-01 04:10Z) Implemented Linear GraphQL request builders and response parsers for comment fetch, file upload, comment update, and comment-create-with-comment-id, with tests in `test/linear_attachment_graphql_test.gleam`.
- [x] (2026-05-01 04:10Z) Implemented the attachment orchestration module and upload PUT transport in `src/scherzo/linear_attachment.gleam`, with mocked transport tests in `test/linear_attachment_test.gleam`.
- [x] (2026-05-01 04:10Z) Added the local `--linear-attach-comment-file` diagnostic mode, startup error mapping, CLI parsing tests, and a validation-path service test.
- [x] (2026-05-01 04:10Z) Added opt-in handoff result attachments, config parsing, reusable result Markdown formatting, deterministic filenames, and handoff transport-injection tests.
- [x] (2026-05-01 04:10Z) Updated README documentation and ran final validation: `direnv exec . gleam format --check src test` and `direnv exec . gleam test`.
- [x] (2026-05-01 14:21Z) Ran the optional live Linear smoke against comment `1c42970b-021c-4f79-a010-ef2d7a13e051` on issue `LIV-11` using `test/tmp/linear-attachment-live-smoke-20260501T142035Z.md`; the diagnostic logged `linear_comment_attachment_ok` with `mode=native`, and a follow-up GraphQL check confirmed the comment bodyData has the smoke filename as a `text/markdown` file node.

## Surprises & Discoveries

- Observation: Linear exposes `Comment.bodyData` as a non-null GraphQL `String`, while `CommentUpdateInput.bodyData` accepts the `JSON` scalar.
  Evidence: Schema introspection showed `Comment.bodyData: String!` and `CommentUpdateInput.bodyData: JSON`.

- Observation: The file upload mutation is named `fileUpload` and returns `UploadPayload`, not a type named `FileUploadPayload`.
  Evidence: Schema introspection showed `Mutation.fileUpload(contentType: String!, filename: String!, size: Int!, metaData: JSON, makePublic: Boolean): UploadPayload!`, and `UploadPayload` has `success`, `lastSyncId`, and nullable `uploadFile`.

- Observation: Upload headers are structured objects.
  Evidence: Schema introspection showed `UploadFile.headers: [UploadFileHeader!]!`, and `UploadFileHeader` has non-null `key` and `value` string fields.

- Observation: The verified LIV-20 example has exactly the shape needed for this plan: top-level `file` nodes followed by a paragraph.
  Evidence: Querying LIV-20 returned a comment whose `bodyData` string decodes to `{"type":"doc","content":[{"type":"file",...},{"type":"file",...},{"type":"paragraph",...}]}` and whose `body` contains Markdown links to the uploaded files.

- Observation: Existing Scherzo Linear transport only supports GraphQL POST requests with UTF-8 string bodies.
  Evidence: `src/scherzo/linear.gleam` defines `Request(endpoint, headers, body, timeout_ms)`, `Response(status, body)`, and `http_transport` always sets method `http.Post` and dispatches `Request(String)` through `httpc.dispatch`.

- Observation: `gleam_httpc` already supports binary request bodies.
  Evidence: `build/packages/gleam_httpc/src/gleam/httpc.gleam` exposes `dispatch_bits(config, Request(BitArray))`, which is suitable for the upload PUT.

- Observation: The diagnostic attach mode can surface invalid local-file input without any Linear network call.
  Evidence: `test/orchestrator_service_test.gleam` calls `start_linear_attach_comment_file` with `test/tmp/result.txt` and asserts the startup error code is `linear_attachment_error` with the readable `.md or .markdown` detail rather than `tracker error`.

## Decision Log

- Decision: Append a top-level ProseMirror `file` node to `bodyData.content` rather than using issue-level attachments.
  Rationale: LIV-26 is about matching Linear UI comment file attachments. Issue-level attachments render as a different issue surface and do not attach to a specific comment.
  Date: 2026-05-01

- Decision: Keep automatic handoff attachments opt-in with `handoff.attach_result_on_success: false` by default.
  Rationale: Uploading files mutates Linear more than today's text comment, depends on an internal field, and can create unattached uploads if the final update fails. Existing deployments should not change behavior without explicit configuration.
  Date: 2026-05-01

- Decision: Add a normal Markdown-link fallback mode and enable it by default for attachment operations.
  Rationale: Linear marks `bodyData` internal. If native rich-text update becomes unavailable, operators should still get a useful link to the uploaded artifact instead of a hard failure whenever fallback is acceptable.
  Date: 2026-05-01

- Decision: Preserve arbitrary `bodyData` JSON through a small Scherzo JSON value type instead of modeling every ProseMirror node.
  Rationale: The implementation needs to append one node and preserve everything else. A full ProseMirror schema would be larger and more brittle than the current problem requires.
  Date: 2026-05-01

- Decision: Append the new file node at the end of the top-level content array.
  Rationale: Appending preserves existing visible order and avoids moving a success comment's summary below its attachment. Native rendering depends on the node type and attrs, not a proven requirement to prepend.
  Date: 2026-05-01

- Decision: Use deterministic filenames for handoff result attachments and skip upload when a matching file node is already present.
  Rationale: Linear does not provide a public idempotency key for this composed operation. A filename containing the run id makes duplicate detection safe for retries of the same handoff side effect.
  Date: 2026-05-01

- Decision: Validate native bodyData shape and fallback duplicate state before requesting `fileUpload`.
  Rationale: An uploaded asset cannot be cleanly deleted with the verified API. Parsing the native document and detecting already-present native nodes or fallback links before upload avoids avoidable orphaned uploads and duplicate retry links.
  Date: 2026-05-01

- Decision: Preserve injected GraphQL transports in the handoff attachment path.
  Rationale: Existing tests and future callers rely on `handoff.linear_client(..., transport)` using the supplied transport. Hard-coding `linear.http_transport` inside that wrapper would break mocked tests and could accidentally make real network calls.
  Date: 2026-05-01

## Outcomes & Retrospective

Implementation completed the planned native comment-file path, fallback Markdown-link path, local diagnostic command, and opt-in handoff attachment path. The mocked suite now proves bodyData preservation, no `Authorization` header on upload PUT requests, fallback behavior when bodyData is invalid, byte-size handling for Unicode Markdown files, default-off handoff behavior, and fail-fast handoff state-update ordering when attachment fails. Final validation passed with `direnv exec . gleam format --check src test` and `direnv exec . gleam test`, ending with `487 passed, no failures`. A live Linear smoke also succeeded against issue `LIV-11`: the diagnostic selected comment `1c42970b-021c-4f79-a010-ef2d7a13e051`, uploaded `linear-attachment-live-smoke-20260501T142035Z.md`, logged `mode=native`, and a follow-up GraphQL query verified the bodyData `file` node exists with `mimetype` `text/markdown`.

## Context and Orientation

Scherzo is a Gleam project targeting Erlang. Production source code lives under `src/scherzo/`, Erlang FFI files live directly under `src/`, tests live under `test/`, and execution plans live under `docs/plans/`. The normal validation command from the repository root is `direnv exec . gleam test`. This repository uses Jujutsu; use `jj status`, `jj describe`, and `jj new` for commit discipline rather than mutating `git` commands.

The Linear GraphQL client is currently implemented in `src/scherzo/linear.gleam`. It defines `Request`, `Response`, and `Transport`, where `Transport` is a function from `linear.Request` to `Result(linear.Response, error.TrackerError)`. The real transport is `linear.http_transport`, which posts GraphQL JSON to the configured Linear endpoint. The module also owns issue queries, comment fetching for command comments, comment creation, issue state updates, response parsing, and helper decoders.

The current command-comment support in `src/scherzo/linear.gleam` fetches comments with `body`, `createdAt`, `updatedAt`, and `user`; it does not fetch `bodyData`. The `LinearComment` type is used by Linear command transport and should not be changed to require `bodyData` for command polling.

The Scherzo handoff client is implemented in `src/scherzo/handoff.gleam`. It posts claim, success, and failure comments and optionally updates issue state. Success comment text is produced by `src/scherzo/handoff_format.gleam`. The current `handoff.Client` functions return `Result(Nil, error.TrackerError)`. The daemon creates a handoff client through `src/scherzo/orchestrator/daemon.gleam` default dependencies, and side effects are executed in `src/scherzo/orchestrator/effect_runner.gleam`.

Runtime configuration types are in `src/scherzo/domain.gleam`. `domain.HandoffConfig` currently contains `enabled`, comment toggles, optional state ids, `include_result_on_success`, and `result_max_chars`. Config parsing is in `src/scherzo/config.gleam`; tests in `test/config_test.gleam` assert handoff defaults and validation.

The top-level CLI parser is in `src/scherzo/main.gleam`. Existing diagnostic modes include `--linear-smoke`, `--linear-contract-check`, and `--pi-probe`. The local daemon control CLI is separate and lives in `src/scherzo/ctl.gleam`; it should not grow Linear API mutation commands because `ctl` talks to a running local daemon control file, not directly to Linear.

## Preconditions and Verified Facts

Before implementing this plan in a fresh checkout, run these commands from the repository root:

    jj status --ignore-working-copy
    direnv exec . gleam test

If `direnv exec . gleam test` reports that `.envrc` is blocked, inspect `.envrc`, run `direnv allow .`, and retry. At plan authoring, `.envrc` sources `devenv` and optionally loads `.env` and `.env.local`. The baseline test run reported:

    464 passed, no failures

The working copy was clean before this plan file was written:

    The working copy has no changes.

Verified repository facts at authoring:

- `gleam.toml` targets Erlang and depends on `gleam_stdlib`, `gleam_erlang`, `gleam_otp`, `gleam_json`, `gleam_http`, `gleam_httpc`, `simplifile`, `yay`, and `birl`.
- `src/scherzo/linear.gleam` imports `gleam/json`, `gleam/http`, `gleam/http/request`, and `gleam/httpc`, and its `http_transport` only sends POST requests with string bodies.
- `src/scherzo/error.gleam` currently has `TrackerError` variants `LinearApiRequest`, `LinearApiStatus`, `LinearGraphqlErrors`, `LinearUnknownPayload`, and `LinearMissingEndCursor`.
- `src/scherzo/handoff.gleam` currently posts success comments through `linear.build_comment_create_request`, parses them with `linear.parse_mutation_response(response, "commentCreate")`, and then runs an optional issue state update.
- `src/scherzo/handoff_format.gleam` currently keeps `result_section` and `metadata` private, so attachment content generation needs either a new public helper or a small refactor.
- `src/scherzo/main.gleam` currently accepts `--linear-smoke [path]`, `--linear-contract-check [path]`, and `--pi-probe [path]` but has no mode for mutating a specific Linear comment.
- `simplifile.read_bits` returns `BitArray`, and `gleam/bit_array.byte_size` returns the byte count needed for Linear `fileUpload(size:)`.
- `gleam_httpc.dispatch_bits` can send `Request(BitArray)` and should be used for the pre-signed upload URL.

Verified Linear schema facts at authoring:

- `Query.comment` accepts `id: String` or `hash: String` and returns `Comment!`.
- `Comment.body` is `String!` and `Comment.bodyData` is `String!`.
- `Mutation.fileUpload` accepts `filename: String!`, `contentType: String!`, `size: Int!`, optional `metaData: JSON`, and optional `makePublic: Boolean`, and returns `UploadPayload!`.
- `UploadPayload` has `success: Boolean!`, `lastSyncId: Float!`, and nullable `uploadFile: UploadFile`.
- `UploadFile` has `filename`, `contentType`, `size`, `uploadUrl`, `assetUrl`, nullable `metaData`, and `headers`.
- `UploadFileHeader` has `key: String!` and `value: String!`.
- `Mutation.commentUpdate` accepts `id: String!`, `input: CommentUpdateInput!`, and optional `skipEditedAt: Boolean`, and returns `CommentPayload!`.
- `CommentUpdateInput.bodyData` is a `JSON` scalar, and `CommentUpdateInput.body` is a `String` scalar.
- `CommentPayload` has `success: Boolean!` and `comment: Comment!`.

The LIV-20 verification comment had id `1866ea7d-3865-47a8-a3f5-2313d689300f` and contained `bodyData` with top-level `file` nodes. Do not hard-code this id in tests; use it only as evidence for the shape.

## Scope Boundaries

In scope: parsing and encoding comment `bodyData`; appending a Linear UI-style `file` node; file upload GraphQL request/response parsing; pre-signed upload URL PUT support; fallback Markdown-link comment updates; a diagnostic local-file attach mode; opt-in success-result attachment in handoff; tests with mocked GraphQL and upload transports; and README documentation.

Out of scope: issue-level Linear attachments; general non-Markdown file attachment UI; changing Linear command polling to include `bodyData`; using Linear's SDK; adding a Node helper; changing pi output capture; persisting uploaded artifacts in Scherzo's local ledger; deleting uploaded Linear assets on failure; full optimistic concurrency control for comment edits beyond preserving the fetched document and keeping the update window short; and automatic attachment of arbitrary workflow workspace files after cleanup.

The implementation may support `BitArray` content internally so tests and generated strings do not have to round-trip through the filesystem. The user-facing diagnostic mode should restrict the file path to `.md` or `.markdown` and use `text/markdown` as the MIME type.

## Milestones

Milestone 1 builds the pure document transformation. At the end, `src/scherzo/linear_body_data.gleam` can parse a `Comment.bodyData` string into a reusable document value, verify it is a ProseMirror document with a top-level `content` array, append a native file node while preserving unknown existing JSON, detect an already-attached native filename for idempotent handoff retries, detect an existing fallback Markdown link in plain comment text, and build a fallback Markdown link. This milestone has no network behavior and is proven by exact unit tests.

Milestone 2 adds Linear GraphQL builders and parsers. At the end, `src/scherzo/linear.gleam` can build and parse comment fetch, file upload, comment update with `bodyData`, comment update with `body`, and comment create responses that include the created comment id. Existing issue, command-comment, contract, and mutation tests still pass.

Milestone 3 adds upload PUT and the attachment orchestration client. At the end, `src/scherzo/linear_attachment.gleam` can attach in-memory Markdown bytes or a local Markdown file to a comment using injected GraphQL and upload transports. Tests prove the order of operations, byte-size calculation, upload headers, native bodyData update, fallback link update, and clear failure behavior.

Milestone 4 adds a local diagnostic mode. At the end, an operator can run one command from the repository root to attach a local Markdown file to a known Linear comment id using a Scherzo config for the Linear endpoint and API key. This mode is intentionally separate from `scherzoctl` because it talks directly to Linear and mutates a remote comment.

Milestone 5 wires opt-in handoff result attachments. At the end, `handoff.attach_result_on_success: true` causes Scherzo to create its usual success comment, upload the redacted success result as a Markdown file, attach it to that comment, and then continue the existing success state update. With the option absent or false, current handoff behavior is unchanged.

Milestone 6 documents and validates. At the end, README explains the diagnostic command, handoff options, internal `bodyData` risk, fallback behavior, and validation commands. Format checks and the full test suite pass.

## Plan of Work

Create `src/scherzo/linear_body_data.gleam`. Define a small JSON value type with variants for objects, arrays, strings, integers, floats, booleans, and null. Provide a recursive decoder from `gleam/dynamic/decode` and an encoder to `gleam/json.Json`. The object representation may use `Dict` or a list of key-value pairs; preserving key order is not required, but preserving all fields and values is required.

In the same module, define `FileNodeAttrs` with `upload_id`, `href`, `name`, `size`, and `mimetype`. Define `AppendResult` with `Appended(body_data: JsonValue)` and `AlreadyPresent`. Implement `parse_document(body_data_json)` so it parses the bodyData string, requires top-level object field `type == "doc"`, requires top-level `content` to be an array, and returns the parsed `JsonValue` for reuse by the upload orchestration. Implement `append_file_node_to_document(parsed_document, attrs, dedupe_by_filename)` so it optionally returns `AlreadyPresent` if a top-level file node already has matching `attrs.name` and `attrs.mimetype`, and otherwise appends a file node shaped exactly as:

    {
      "type": "file",
      "attrs": {
        "uploadState": "finished",
        "uploadId": "upload-...",
        "href": "https://uploads.linear.app/...",
        "name": "result.md",
        "size": 1234,
        "mimetype": "text/markdown"
      }
    }

Also keep a convenience `append_file_node(body_data_json, attrs, dedupe_by_filename)` wrapper for direct unit tests, implemented by calling `parse_document` and `append_file_node_to_document`. Implement `markdown_link(filename, href)` for fallback body updates and `body_has_markdown_link_for_filename(body, filename)` for retry dedupe when native bodyData is unavailable. `markdown_link` should escape `[` and `]` in the label at minimum. The asset URL comes from Linear's `UploadFile.assetUrl`; do not use the pre-signed `uploadUrl` as the comment link.

Extend `src/scherzo/error.gleam` by adding `LinearUploadStatus(Int)` and `LinearAttachmentError(String)` to `TrackerError`. Update `tracker_code` so these map to `linear_upload_status` and `linear_attachment_error`. Use `LinearAttachmentError` for invalid bodyData, invalid file extension, file read errors, missing uploadFile in a successful fileUpload payload, and other attachment-specific validation. Use `LinearUploadStatus` when the pre-signed PUT returns a non-2xx status. Continue using `LinearApiRequest` for invalid endpoints, invalid upload URLs, missing API keys, and HTTP client failures.

Extend `src/scherzo/linear.gleam` with small types for this feature: `LinearCommentDocument(id, body, body_data)`, `UploadHeader(key, value)`, and `UploadFile(filename, content_type, size, upload_url, asset_url, headers)`. Add request builders and response parsers named predictably:

    pub fn build_comment_fetch_request(config: domain.TrackerConfig, comment_id: String) -> Result(Request, error.TrackerError)
    pub fn parse_comment_fetch_response(response: Response) -> Result(LinearCommentDocument, error.TrackerError)
    pub fn build_file_upload_request(config: domain.TrackerConfig, filename: String, content_type: String, size: Int, meta_data: json.Json) -> Result(Request, error.TrackerError)
    pub fn parse_file_upload_response(response: Response) -> Result(UploadFile, error.TrackerError)
    pub fn build_comment_update_body_data_request(config: domain.TrackerConfig, comment_id: String, body_data: json.Json) -> Result(Request, error.TrackerError)
    pub fn build_comment_update_body_request(config: domain.TrackerConfig, comment_id: String, body: String) -> Result(Request, error.TrackerError)
    pub fn parse_comment_update_response(response: Response) -> Result(LinearCommentDocument, error.TrackerError)
    pub fn parse_comment_create_response(response: Response) -> Result(LinearCommentDocument, error.TrackerError)

Change `comment_create_mutation()` to request `comment { id body bodyData }` in addition to `success`. Keep `parse_mutation_response(response, "commentCreate")` working with existing mock responses that only include `success`; extra fields from the real API are ignored by that parser.

Create `src/scherzo/linear_attachment.gleam`. Define `UploadRequest(url, headers, body, timeout_ms)`, `UploadResponse(status, body)`, `UploadTransport`, `Dependencies(graphql_transport, upload_transport, now_ms, nonce)`, `AttachOptions(fallback_to_markdown_link, dedupe_by_filename)`, and `AttachmentOutcome` variants `AttachedNative(comment_id, filename, asset_url)`, `AttachedMarkdownLink(comment_id, filename, asset_url)`, and `AlreadyAttached(comment_id, filename)`.

In `linear_attachment.gleam`, implement `real_upload_transport` using `gleam/http/request`, `gleam/http`, `gleam/httpc.dispatch_bits`, and `Request(BitArray)`. It must require the upload URL to start with `https://`, set method `http.Put`, set the body bits, apply exactly the upload headers supplied in `UploadRequest`, and map `httpc.HttpError` through `linear.http_error_to_string` into `error.LinearApiRequest`.

Implement `attach_markdown_to_comment(config, comment_id, filename, body_bits, options, dependencies)`. It should validate a non-empty filename ending in `.md` or `.markdown`, set `content_type = "text/markdown"`, compute `size = bit_array.byte_size(body_bits)`, fetch the comment document, and decide one of three modes before upload: already attached, native append, or fallback link. Native mode must call `linear_body_data.parse_document` before `fileUpload` and keep the parsed document in memory; after upload succeeds, append the file node to that already-validated document rather than reparsing the original string. If a native file node is already attached, return `AlreadyAttached` without uploading. If bodyData is invalid and fallback is disabled, return `LinearAttachmentError` without uploading. If bodyData is invalid, fallback is enabled, and `dedupe_by_filename` is true, check `body_has_markdown_link_for_filename` before upload and return `AlreadyAttached` if the link is already present. Otherwise call `fileUpload`, then PUT the bytes to `uploadFile.uploadUrl` using all returned headers plus `Content-Type: text/markdown`, then update the comment. Native mode calls `commentUpdate` with `bodyData`; fallback mode calls `commentUpdate` with `body` equal to the previous body plus a blank line plus the Markdown link. Treat upload PUT statuses from 200 through 299 as success.

Implement `attach_markdown_file_to_comment(config, comment_id, path, options, dependencies)`. It should use `simplifile.file_info` to require a regular file, use `simplifile.read_bits`, derive the filename from the final path segment, validate the Markdown extension, and call `attach_markdown_to_comment`. Map filesystem errors through `LinearAttachmentError` with readable `simplifile.describe_error` text.

Generate upload ids as `"upload-" <> int.to_string(now_ms()) <> "-" <> nonce()`. For real dependencies, use the existing `scherzo_time_ffi.monotonic_ms` pattern for `now_ms` or inject it from callers, and add a tiny Erlang FFI only if needed for a monotonic unique nonce. Tests must inject `now_ms = fn() { 123 }` and `nonce = fn() { "abc" }` so request bodies are deterministic. Do not add custom attrs to the ProseMirror file node; unknown attrs might be rejected or normalized by Linear.

Extend `src/scherzo/main.gleam` with a new CLI result for direct attachment, for example `LinearAttachCommentFile(comment_id: String, file_path: String, config_path: Option(String))`. Parse these forms:

    gleam run -- --linear-attach-comment-file <comment-id> <file.md>
    gleam run -- --linear-attach-comment-file <comment-id> <file.md> <path-to-scherzo.yaml>

Extend `src/scherzo/orchestrator/service.gleam` with `start_linear_attach_comment_file(workflow_path, comment_id, file_path)`. It should load the runtime bundle to get `effective.tracker` and `secrets`, call `linear_attachment.attach_markdown_file_to_comment` with real dependencies and fallback enabled, and log `linear_comment_attachment_ok` with `comment_id`, `filename`, `mode`, and `asset_url` redacted through the existing logger. Do not use the existing generic `map_tracker_error` unchanged for this mode, because it would hide useful attachment validation details behind `tracker error`. Add a small tracker-error-to-message helper or mode-local mapping so `StartupError.code` remains `error.tracker_code(err)` and `StartupError.message` includes the readable `LinearAttachmentError` or upload status detail. This command mutates Linear and should not be part of doctor checks.

Extend `src/scherzo/domain.gleam` by adding two fields to `HandoffConfig`: `attach_result_on_success: Bool` and `attachment_fallback_to_markdown_link: Bool`. Update `src/scherzo/config.gleam` defaults and parser so `attach_result_on_success` defaults to `False` and `attachment_fallback_to_markdown_link` defaults to `True`. Update every `domain.HandoffConfig(` constructor found by `grep -R "HandoffConfig(" src test`.

Refactor `src/scherzo/handoff_format.gleam` so attachment content can reuse the existing success result formatting without duplicating redaction rules. Add a public helper such as:

    pub fn success_result_attachment_markdown(
      issue: domain.Issue,
      success: runner.WorkerSuccess,
      run_id: String,
      secrets: List(String),
    ) -> Option(String)

It should return `Some(markdown)` only when `success.result.final_response` is `Some(text)`. The Markdown should include a short heading with the issue identifier and run id, the redacted result text, the truncation note when applicable, and the same metadata block used in the comment. If there is no captured result text, return `None` and skip upload.

Update `src/scherzo/handoff.gleam`. Keep `linear_client(tracker_config, handoff_config, transport)` available for existing tests and have it call a new `linear_client_with_attachment_dependencies` using the supplied `transport` for every GraphQL request, `linear_attachment.real_upload_transport` for the upload PUT, and real clock/nonce dependencies. Do not hard-code `linear.http_transport` inside this wrapper; the daemon already passes that real transport, while tests pass fakes and must not make network calls. In `report_success`, when `attach_result_on_success` is false, keep the existing sequence exactly: optional comment, optional state update. When it is true and success comments are enabled, create the success comment and parse the returned `LinearCommentDocument`, generate the attachment Markdown, attach it to that comment with `dedupe_by_filename = True` and the configured fallback, then run the optional success state update. If `comment_on_success` is false, skip attachment because there is no Scherzo success comment to attach to.

For handoff filenames, use a deterministic safe pattern: lowercase issue identifier and run id, replace any character outside ASCII letters, digits, dot, underscore, and hyphen with `-`, collapse empty output to `scherzo-result`, and append `.md`. A typical filename is `abc-1-ABC-1-123-1-result.md` or a cleaner equivalent as long as it includes both issue identifier and run id. Tests should assert the chosen exact format. The filename must not include path separators.

Update README. Add the new diagnostic command near the existing Linear diagnostic modes. Add a handoff snippet showing:

    handoff:
      enabled: true
      attach_result_on_success: true
      include_result_on_success: false
      attachment_fallback_to_markdown_link: true

Explain that `bodyData` is internal to Linear, Scherzo first tries the native `file` node path, and when fallback is enabled it updates the comment body with a normal Markdown link instead.

## Concrete Steps

1. Run `jj status --ignore-working-copy` from the repository root and confirm only this plan file is modified, or record any unrelated changes before proceeding.

2. Run `direnv exec . gleam test` from the repository root. Expect the baseline to pass. At plan authoring the output ended with `464 passed, no failures`.

3. Create `src/scherzo/linear_body_data.gleam` with the JSON value type, recursive decoder, JSON encoder, `FileNodeAttrs`, `AppendResult`, `parse_document`, `append_file_node_to_document`, the convenience `append_file_node`, `has_file_named`, `markdown_link`, and `body_has_markdown_link_for_filename` helpers.

4. Create `test/linear_body_data_test.gleam`. Add a test that passes a document with one existing file node and one paragraph to `append_file_node`, asserts the result is `Appended`, encodes it back to JSON, parses it again, and asserts the original file node name, original paragraph text, and new file node attrs are all present.

5. In the same test file, add a test that `append_file_node` returns `AlreadyPresent` when `dedupe_by_filename` is true and the document already has a top-level file node with the same `name` and `mimetype`.

6. Add a test that malformed JSON returns an error whose message includes `invalid bodyData JSON`.

7. Add a test that a JSON object without top-level `type: "doc"` or without an array `content` returns an error whose message names the missing shape.

8. Add a test for `markdown_link("a[b].md", "https://uploads.linear.app/file")` and assert the output is `[a\[b\].md](https://uploads.linear.app/file)`. Add a retry-dedupe test that `body_has_markdown_link_for_filename` returns true for a body containing that escaped link label and false for a different filename.

9. Run `direnv exec . gleam test`. The new body-data tests should pass, and no existing tests should fail.

10. Update `src/scherzo/error.gleam` with `LinearUploadStatus` and `LinearAttachmentError`, and update `tracker_code`.

11. Update any tests that pattern match exhaustively on `TrackerError`, if the compiler reports them. Run `direnv exec . gleam test` and expect either all tests to pass or only failures from tests that intentionally assert the old variant set.

12. In `src/scherzo/linear.gleam`, add `LinearCommentDocument`, `UploadHeader`, and `UploadFile` public types near the existing Linear comment types.

13. Add `comment_fetch_query`, `file_upload_mutation`, `comment_update_body_data_mutation`, and `comment_update_body_mutation` string functions. Keep query names prefixed with `Scherzo`, for example `ScherzoCommentFetch`, `ScherzoFileUpload`, and `ScherzoCommentUpdateBodyData`.

14. Add `build_comment_fetch_request`, `build_file_upload_request`, `build_comment_update_body_data_request`, and `build_comment_update_body_request`, using the same HTTPS endpoint and API key validation style as existing builders.

15. Change `comment_create_mutation()` so it requests `success comment { id body bodyData }`.

16. Add parsers for comment fetch, file upload, comment update, and comment create. Reuse the existing GraphQL error decoder pattern: non-200 maps to `LinearApiStatus`, `errors` maps to `LinearGraphqlErrors`, invalid JSON or missing expected data maps to `LinearUnknownPayload` or `LinearAttachmentError` as appropriate.

17. Extend `test/linear_test.gleam` or create `test/linear_attachment_graphql_test.gleam`. Add request-builder tests asserting the new request bodies contain the expected GraphQL operation names, variables, `bodyData`, `contentType`, `filename`, `size`, and existing `Authorization` / `Content-Type` GraphQL headers.

18. Add parser tests for a successful `fileUpload` response containing two headers, asset URL, upload URL, filename, content type, and size.

19. Add parser tests for `fileUpload` returning `success: false`, `uploadFile: null`, GraphQL errors, non-200 status, and invalid JSON.

20. Add parser tests for comment fetch and comment update returning `comment { id body bodyData }`.

21. Run `direnv exec . gleam test`. Fix any existing mutation tests affected by the richer commentCreate selection; do not weaken existing assertions that protect current behavior.

22. Create `src/scherzo/linear_attachment.gleam` with `UploadRequest`, `UploadResponse`, `UploadTransport`, `Dependencies`, `AttachOptions`, `AttachmentOutcome`, and `real_upload_transport`.

23. Implement helper functions in `linear_attachment.gleam`: Markdown filename validation, basename extraction, upload-id generation, upload-header construction, 2xx status checking, and outcome-to-mode string for logging.

24. Implement `attach_markdown_to_comment` using injected transports. The exact operation order for native success is: fetch comment, parse and validate bodyData into a reusable document value, detect an already-present file, request `fileUpload`, PUT file bytes, append the file node to the already-parsed document, call `commentUpdate` with bodyData, return `AttachedNative`.

25. Implement fallback behavior. If bodyData parsing fails and `fallback_to_markdown_link` is true, first check `body_has_markdown_link_for_filename` when `dedupe_by_filename` is true. If the link already exists, return `AlreadyAttached` without upload. Otherwise the operation order is: fetch comment, request `fileUpload`, PUT file bytes, call `commentUpdate` with body equal to old body plus two newlines plus `markdown_link(filename, assetUrl)`, return `AttachedMarkdownLink`.

26. Implement `attach_markdown_file_to_comment` using `simplifile.file_info`, `simplifile.file_info_type`, `simplifile.read_bits`, and `simplifile.describe_error`.

27. Create `test/linear_attachment_test.gleam`. Add a test for native attachment with fake GraphQL and upload transports. The fake GraphQL transport should return, in order, a comment fetch response, a fileUpload response, and a commentUpdate response. Assert the fileUpload request contains byte size from `bit_array.byte_size`, the upload request URL is the pre-signed URL, the upload request includes returned headers and `Content-Type: text/markdown`, the upload request does not include `Authorization`, and the final commentUpdate body contains the file node with `uploadState`, deterministic upload id, asset URL, filename, byte size, and `text/markdown`.

28. Add a test where the fetched bodyData already contains the deterministic handoff filename and `dedupe_by_filename` is true. Assert the result is `AlreadyAttached` and neither fileUpload nor upload transport is called. Add a separate fallback-dedupe test where bodyData is invalid, the existing `body` already contains `markdown_link(filename, assetUrl-or-any-url)`, and `dedupe_by_filename` is true; assert `AlreadyAttached` and no upload.

29. Add a test where fetched bodyData is invalid and fallback is enabled. Assert fileUpload and PUT still happen, the final commentUpdate request updates `body`, the body includes the Markdown link to `assetUrl`, and the request does not contain `bodyData`.

30. Add a test where fetched bodyData is invalid and fallback is disabled. Assert the result is `Error(error.LinearAttachmentError(_))` and no fileUpload or PUT happens.

31. Add a test where upload PUT returns status 403. Assert the result is `Error(error.LinearUploadStatus(403))` and no commentUpdate happens.

32. Add a test that `attach_markdown_file_to_comment` rejects a non-Markdown extension with `LinearAttachmentError` before any GraphQL call.

33. Add a test using a small temp `.md` file with Unicode content such as `hello 🌍`; assert the fileUpload `size` equals the UTF-8 byte size, not the character count.

34. Run `direnv exec . gleam test`. The new attachment tests should pass without real Linear network calls.

35. Update `src/scherzo/main.gleam` to add the `LinearAttachCommentFile` CLI result and parse `--linear-attach-comment-file <comment-id> <file.md> [config]`. Update `usage()` to document it under modes, and make invalid arity return `UsageError`.

36. Update `src/scherzo/orchestrator/service.gleam` to add `start_linear_attach_comment_file`. Use `runtime_bundle.load`, `linear_attachment.attach_markdown_file_to_comment`, real dependencies, existing log redaction, and the new detailed tracker-error message mapping for this diagnostic mode. Add any small dependency record if tests need injection.

37. Update `test/main_test.gleam` to assert the new CLI parse forms and invalid arity.

38. Update or add service tests in `test/orchestrator_service_test.gleam` using injected dependencies if you added a testable service wrapper. Assert success logs include `linear_comment_attachment_ok`, comment id, filename, and mode without exposing the API key. Add an error-path assertion that an attachment validation failure surfaces `linear_attachment_error` with the readable detail instead of the generic message `tracker error`.

39. Run `direnv exec . gleam test`.

40. Update `src/scherzo/domain.gleam` to add `attach_result_on_success` and `attachment_fallback_to_markdown_link` to `HandoffConfig`.

41. Update `src/scherzo/config.gleam` defaults and `resolve_handoff` parsing for the two new fields.

42. Update every `domain.HandoffConfig(` constructor in `src` and `test`. Use the repository search command `grep -R "HandoffConfig(" src test` or the structured grep tool to ensure none remain with the old field list.

43. Update `test/config_test.gleam`. Add assertions that defaults are `attach_result_on_success == False` and `attachment_fallback_to_markdown_link == True`. Add a parsing test that sets both fields explicitly and asserts the parsed values.

44. Refactor `src/scherzo/handoff_format.gleam` to expose `success_result_attachment_markdown`. Keep `success_comment` output byte-for-byte compatible for existing tests when no new config is enabled.

45. Update `test/handoff_format_test.gleam` to cover the attachment Markdown helper. Assert it redacts secrets, includes the run id and issue identifier, includes the result text, includes the truncation note when `success.result.truncated` is true, and returns `None` when there is no final response.

46. Update `src/scherzo/handoff.gleam` to support attachment dependencies. Add a new constructor such as `linear_client_with_attachment_dependencies` that tests can call with fake GraphQL and upload transports, deterministic clock, and nonce. Keep the existing `linear_client` function as a convenience wrapper so current call sites do not all change, and verify that it uses its supplied GraphQL `transport` argument rather than hard-coding `linear.http_transport`.

47. Change `report_success` in `handoff.gleam` for the enabled attachment path. It should create the success comment, parse the returned comment id, generate attachment Markdown, call `linear_attachment.attach_markdown_to_comment`, then run the optional success state update. If attachment Markdown is `None`, skip upload and continue to state update.

48. Update `test/handoff_test.gleam`. Keep existing tests proving disabled attachment behavior and single structured comments. Add a new test with `attach_result_on_success: true` and no success state update that captures GraphQL requests and upload requests, asserts the commentCreate happens, asserts the attachment client attaches to the created comment id, and asserts only one success comment is created.

49. Add a handoff test with `attach_result_on_success: true`, `include_result_on_success: false`, and a success state id. Assert the created comment contains metadata but not inline result text, the uploaded Markdown contains the result text, and the issue state update happens after attachment succeeds.

50. Add a handoff test where attachment fails. Assert `report_success` returns a `TrackerError`, the comment was created, and the success state update was not sent. This preserves the existing fail-fast style where a failed comment prevents the following state update.

51. Run `direnv exec . gleam test`.

52. Update `README.md` with the diagnostic command, handoff config fields, and `bodyData` internal/fallback note.

53. Run `direnv exec . gleam format --check src test`. Expect exit code 0.

54. Run `direnv exec . gleam test`. If you added exactly the tests in this plan and no others, expect the final count to be the baseline 464 plus the new `pub fn ..._test` functions you added, with `no failures`. If the repository has moved, the invariant is that the count does not drop unexpectedly and there are no failures.

55. Optional live smoke, only against a throwaway Linear comment: create a small `test/tmp/linear-attachment-smoke.md`, identify a comment id on a non-production issue, then run `LINEAR_API_KEY=... direnv exec . gleam run -- --linear-attach-comment-file <comment-id> test/tmp/linear-attachment-smoke.md .scherzo/scherzo.yaml`. Expect a log line containing `linear_comment_attachment_ok`. Open the comment in Linear and verify the file renders on that comment. Do not run this against an important comment until the mocked test suite is green.

56. Commit point: after format, tests, and documentation pass, create a logical jj commit for the implementation. A suitable message is `support Linear comment Markdown file attachments`.

## Testing and Falsifiability

The pure body-data tests falsify the claim that existing comment content is preserved. `test/linear_body_data_test.gleam` must include a fixture with an existing `file` node and a paragraph. The test must assert that after appending, both old nodes and the new file node remain. If this test cannot preserve an unknown node or existing attrs, the plan's preservation claim is false and implementation must stop until the representation is fixed.

The upload orchestration tests falsify the claim that Scherzo follows Linear's required flow. `test/linear_attachment_test.gleam` must assert the sequence fetch comment → fileUpload → upload PUT → commentUpdate for native mode, with bodyData parsed and validated before `fileUpload` is requested. It must assert upload PUT uses the returned upload URL and returned headers plus `Content-Type`, and does not include the GraphQL `Authorization` header. It must assert non-2xx upload status stops before commentUpdate.

The fallback tests falsify the resilience claim. A mocked comment with invalid `bodyData` and fallback enabled must still produce a useful `body` update with a Markdown link to `assetUrl`. The same invalid comment with fallback disabled must fail before upload so it does not create an orphan upload. A fallback retry with the deterministic filename already present in the comment body must return `AlreadyAttached` before upload so fallback mode is not the path that creates duplicates.

The handoff tests falsify the compatibility claim. Existing tests must continue to prove that with `attach_result_on_success` false, Scherzo posts the same structured success comment shape as before. New tests must prove the opt-in path creates one success comment, attaches to that comment id using the injected GraphQL transport, and does not perform the success state update if attachment fails.

The size test falsifies correct upload metadata for Unicode. A Markdown file containing `hello 🌍` must send the UTF-8 byte count from `bit_array.byte_size`, not `string.length`. A mismatch means Linear may reject uploads or store corrupt metadata.

The final validation command is:

    direnv exec . gleam format --check src test
    direnv exec . gleam test

At authoring, the baseline was `464 passed, no failures`. After implementation the exact count should increase by the number of new test functions and still end with `no failures`.

## Validation and Acceptance

Acceptance is met when a developer can demonstrate all of the following.

First, the mocked test suite passes and proves the native bodyData path. Running `direnv exec . gleam test` from the repository root must report no failures. The new native attachment test must show a `commentUpdate` GraphQL body whose variables contain a JSON `bodyData` document with a top-level `file` node, `uploadState: "finished"`, an `uploadId` beginning with `upload-`, `href` equal to `uploadFile.assetUrl`, `name` equal to the Markdown filename, `size` equal to the uploaded bytes, and `mimetype: "text/markdown"`.

Second, fallback is observable in tests. With invalid `bodyData`, fallback enabled, and mocked upload success, the final GraphQL mutation must update `body` to include a normal Markdown link to the uploaded `assetUrl`. With fallback disabled, the operation must return `LinearAttachmentError` and no upload request may be sent. With fallback enabled and a matching Markdown link already present, the operation must return `AlreadyAttached` before upload.

Third, current behavior is retained by default. With existing handoff config or `handoff.attach_result_on_success: false`, `test/handoff_test.gleam` must show only the existing success comment and no upload PUT. Existing Linear command comment polling tests must not require `bodyData`.

Fourth, the local diagnostic mode is available. `direnv exec . gleam run -- --help` must document `--linear-attach-comment-file <comment-id> <file.md> [path-to-scherzo.yaml]`. When run manually against a throwaway comment with valid `LINEAR_API_KEY`, the command must log `linear_comment_attachment_ok`, and the Linear UI must show the Markdown file attached to that comment. When invoked with an invalid non-Markdown file path in tests, the mode must surface `linear_attachment_error` with readable detail rather than the generic message `tracker error`.

Fifth, docs explain the internal API risk. README must mention that Scherzo uses Linear's internal `Comment.bodyData` field for native comment file attachments and can fall back to a normal Markdown link when configured.

## Rollout, Recovery, and Idempotence

All code paths are additive. The low-level attachment client is unused until the new diagnostic mode is invoked or the new handoff config field is enabled. Automatic handoff attachments default to disabled, so existing daemon deployments keep current comment and state behavior after upgrade.

To roll out automatic attachments, enable them on a non-critical Scherzo config first:

    handoff:
      enabled: true
      attach_result_on_success: true
      include_result_on_success: false
      attachment_fallback_to_markdown_link: true

Run one controlled issue and inspect the success comment. If the native attachment does not render as expected, set `attach_result_on_success: false` and reload/restart Scherzo. Existing text comments still work. If fallback links are acceptable but native bodyData is failing, keep `attachment_fallback_to_markdown_link: true` while investigating.

The composed upload operation is only partially idempotent because Linear does not expose a public idempotency token for `fileUpload` plus `commentUpdate`. Handoff reduces duplicate attachments by using a deterministic filename containing the run id and checking for an existing native file node or fallback Markdown link with that filename before uploading. Generic local-file attachment does not guarantee idempotence unless `dedupe_by_filename` is enabled by the caller. If a retry creates duplicate attachments, remove the duplicate manually in the Linear UI or edit the comment.

If `fileUpload` or upload PUT fails, the comment is not updated. If upload PUT succeeds and `commentUpdate` fails, an uploaded asset may exist without being referenced by the comment. The implementation reports a clear error; it does not attempt deletion because no stable delete operation for these upload assets was verified. Retrying is safe for Scherzo's internal state but may upload another asset.

If native bodyData parsing fails before upload and fallback is disabled, the operation stops before upload. If fallback is enabled, Scherzo updates the comment body with a normal Markdown link after upload. This fallback is reversible by editing the comment in Linear, but on arbitrary rich-text comments it may normalize Linear's rich text representation to Markdown; use the diagnostic command against throwaway or quiescent comments first.

## Artifacts and Notes

The Linear issue LIV-26 supplied this target file node shape:

    {
      "type": "file",
      "attrs": {
        "uploadState": "finished",
        "uploadId": "upload-<timestamp>-<random>",
        "href": "<uploadFile.assetUrl>",
        "name": "<filename.md>",
        "size": 1234,
        "mimetype": "text/markdown"
      }
    }

The verified LIV-20 example showed Linear projecting bodyData file nodes into comment body Markdown links:

    [AGENTS.md](https://uploads.linear.app/...)

    [linear-api-bodydata-test.md](https://uploads.linear.app/...)

    test

The schema fragments that matter for implementation are:

    Query.comment(id: String, hash: String): Comment!
    Comment.body: String!
    Comment.bodyData: String!
    Mutation.fileUpload(filename: String!, contentType: String!, size: Int!, metaData: JSON, makePublic: Boolean): UploadPayload!
    UploadPayload.success: Boolean!
    UploadPayload.uploadFile: UploadFile
    UploadFile.uploadUrl: String!
    UploadFile.assetUrl: String!
    UploadFile.headers: [UploadFileHeader!]!
    UploadFileHeader.key: String!
    UploadFileHeader.value: String!
    Mutation.commentUpdate(id: String!, input: CommentUpdateInput!, skipEditedAt: Boolean): CommentPayload!
    CommentUpdateInput.bodyData: JSON
    CommentUpdateInput.body: String

Do not include the Linear API key in logs, upload headers, test fixtures, or documentation examples. The only real upload URL to log is `assetUrl`, and even that should pass through the existing redaction path.

## Interfaces and Dependencies

In `src/scherzo/linear_body_data.gleam`, define these public types and functions or close equivalents:

    pub type JsonValue {
      JObject(List(#(String, JsonValue)))
      JArray(List(JsonValue))
      JString(String)
      JInt(Int)
      JFloat(Float)
      JBool(Bool)
      JNull
    }

    pub type FileNodeAttrs {
      FileNodeAttrs(
        upload_id: String,
        href: String,
        name: String,
        size: Int,
        mimetype: String,
      )
    }

    pub type AppendResult {
      Appended(JsonValue)
      AlreadyPresent
    }

    pub fn parse_document(body_data_json: String) -> Result(JsonValue, String)

    pub fn append_file_node_to_document(
      body_data: JsonValue,
      attrs: FileNodeAttrs,
      dedupe_by_filename: Bool,
    ) -> AppendResult

    pub fn append_file_node(
      body_data_json: String,
      attrs: FileNodeAttrs,
      dedupe_by_filename: Bool,
    ) -> Result(AppendResult, String)

    pub fn has_file_named(body_data: JsonValue, filename: String, mimetype: String) -> Bool
    pub fn to_json(value: JsonValue) -> json.Json
    pub fn markdown_link(filename: String, href: String) -> String
    pub fn body_has_markdown_link_for_filename(body: String, filename: String) -> Bool

In `src/scherzo/linear_attachment.gleam`, define:

    pub type UploadRequest {
      UploadRequest(
        url: String,
        headers: List(#(String, String)),
        body: BitArray,
        timeout_ms: Int,
      )
    }

    pub type UploadResponse {
      UploadResponse(status: Int, body: BitArray)
    }

    pub type Dependencies {
      Dependencies(
        graphql_transport: linear.Transport,
        upload_transport: fn(UploadRequest) -> Result(UploadResponse, error.TrackerError),
        now_ms: fn() -> Int,
        nonce: fn() -> String,
      )
    }

    pub type AttachOptions {
      AttachOptions(
        fallback_to_markdown_link: Bool,
        dedupe_by_filename: Bool,
      )
    }

    pub type AttachmentOutcome {
      AttachedNative(comment_id: String, filename: String, asset_url: String)
      AttachedMarkdownLink(comment_id: String, filename: String, asset_url: String)
      AlreadyAttached(comment_id: String, filename: String)
    }

    pub fn real_upload_transport(UploadRequest) -> Result(UploadResponse, error.TrackerError)

    pub fn attach_markdown_to_comment(
      config: domain.TrackerConfig,
      comment_id: String,
      filename: String,
      body: BitArray,
      options: AttachOptions,
      dependencies: Dependencies,
    ) -> Result(AttachmentOutcome, error.TrackerError)

    pub fn attach_markdown_file_to_comment(
      config: domain.TrackerConfig,
      comment_id: String,
      path: String,
      options: AttachOptions,
      dependencies: Dependencies,
    ) -> Result(AttachmentOutcome, error.TrackerError)

In `src/scherzo/domain.gleam`, update `HandoffConfig` to include:

    attach_result_on_success: Bool,
    attachment_fallback_to_markdown_link: Bool,

In `src/scherzo/handoff_format.gleam`, expose a helper returning optional attachment Markdown:

    pub fn success_result_attachment_markdown(
      issue: domain.Issue,
      success: runner.WorkerSuccess,
      run_id: String,
      secrets: List(String),
    ) -> Option(String)

No new third-party dependencies are required. Use existing `gleam_json`, `gleam_http`, `gleam_httpc`, `simplifile`, and `gleam/bit_array` APIs.
