# Implement Scherzo, a Gleam service that runs pi agents from Linear issues

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

After this change, an operator can run a long-lived command named `scherzo` from this repository and have it continuously pull eligible Linear issues, create one isolated filesystem workspace per issue, and run a pi coding-agent session inside that workspace. The operator-visible proof is that, given a valid `WORKFLOW.md`, `LINEAR_API_KEY`, a Linear project slug, a pi executable that passes the RPC compatibility probe, and either a workspace population hook such as `git clone "$REPO_URL" .` or an explicit `before_run` verification hook for pre-populated workspaces, Scherzo logs poll cycles, dispatch decisions, populated workspace paths, pi session lifecycle events, retries, caps, parking decisions, and cleanup decisions without requiring manual per-ticket scripts.

This plan implements the core behavior of OpenAI's Symphony service specification as stored in `docs/SYMPHONY_SPEC.md`, adapted for this repository. The adapted service is called Scherzo, is written in Gleam, uses `devenv` plus `direnv` for the development environment, uses pi RPC mode as the coding harness instead of Codex app-server mode, and uses Linear as the issue tracker.

A successful implementation does not merely prove that a scheduler can log a dispatch. It must prove that pi is launched from the per-issue workspace, that the workflow's workspace population hook can put repository files in that workspace or its verification hook can prove they are already there before pi starts, and that active issues cannot be retried forever when no Linear handoff occurs.

## Problem Framing and Constraints

Today this repository has no service code. A team that wants coding agents to work Linear issues would have to run ad hoc commands by hand, remember which issues are already in progress, create or clean workspaces manually, and inspect scattered terminal output when something fails. That is error-prone once more than one issue or more than one agent run is involved.

Scherzo solves the operational problem by centralizing scheduling in one in-memory orchestrator, making workspace naming deterministic, requiring workspace population or verification to be declared in a repository-owned `WORKFLOW.md`, and rendering issue-specific instructions into the pi session. The minimum runnable workflow must include either a population strategy, normally an `after_create` hook that clones or copies the target repository into the new workspace, or a `before_run` verification hook for intentionally pre-populated workspaces. Scherzo itself does not guess how to populate project code because repository checkout, authentication, branch policy, and dependency bootstrap are team-specific. Because the core plan defers durable tracker claiming and built-in Linear writes, this central authority exists only inside one process; operators must run only one Scherzo instance per Linear project and workspace root.

Scherzo is not a general workflow engine, a web UI, a multi-tenant control plane, or a first-class Linear ticket writer. Ticket comments, state changes, and pull request links are expected to be performed by the pi agent through normal repository tools, shell commands, installed CLIs, or future pi extensions described in the workflow prompt. Because this core plan defers built-in Linear writes, Scherzo must guard against endless reruns by capping failure retries and normal continuation sessions per issue and then parking the issue in memory until Linear reports a newer `updated_at` value or the service restarts.

The important constraints are these. The implementation must be a Gleam application targeting Erlang/BEAM. Development must be reproducible through checked-in `devenv.nix` and `.envrc`. The agent runner must start pi in RPC mode over stdin/stdout JSON Lines, not Codex app-server mode. Scherzo must never launch pi outside the per-issue workspace directory. Linear is the only production tracker required for this plan. Persistent scheduler storage is intentionally out of scope; after process restart, Scherzo recovers by polling Linear and reusing or cleaning workspaces. A reloadable pause must be possible by setting `agent.max_concurrent_agents: 0`, which keeps reconciliation active but prevents new dispatch.

## Strategy Overview

Build the service in small, testable layers, but move the subprocess/RPC proof of feasibility near the front. Start by scaffolding the Gleam project and development environment. Immediately after that, build a minimal Erlang port wrapper and fake pi RPC fixture that proves the service can launch `bash -lc <command>` with an exact cwd, keep JSONL stdout separate enough from diagnostics for protocol safety, send stdin lines, enforce read/turn timeouts, and terminate the child process. Do not build the higher-level scheduler until this spike passes or the plan is revised with a different process strategy.

After the subprocess spike, implement pure domain types, workflow loading, configuration resolution, strict prompt rendering, workspace safety, and Linear normalization before implementing the concurrent orchestrator and pi subprocess runner. This order proves the risky parsing, safety, and integration boundaries early and keeps the later orchestrator code mostly wiring around already-tested behavior.

The design mirrors the Symphony layers but renames Codex-specific pieces to pi-specific pieces. The `WORKFLOW.md` front matter uses top-level keys `tracker`, `polling`, `workspace`, `hooks`, `agent`, and `pi`. The `pi` section replaces Symphony's `codex` section. Scherzo launches `pi --mode rpc --no-session` by default, communicates with it using pi's documented JSONL RPC commands and events, and treats extension UI requests as non-blocking events that are automatically cancelled unless a future extension deliberately implements operator interaction. Dispatch-time preflight runs a pi RPC compatibility probe inside the prepared per-issue workspace when `pi.compatibility_probe` is true, so protocol mismatches fail before Scherzo sends the issue prompt. Startup validates workflow and dispatch configuration but does not launch pi, because the filesystem invariant is that pi processes only start from per-issue workspaces.

The simplest useful observability surface is structured key-value logging to stderr. The optional HTTP dashboard, optional `linear_graphql` pi tool extension, optional SSH worker execution, and durable retry queue are deferred. This is proportionate because core conformance already provides a useful daemon, deterministic populated workspaces, retries with explicit caps, dynamic workflow reload, a reloadable pause switch, and enough logs to operate the service.

## Alternatives Considered

One alternative is to write a small shell script that lists Linear issues and runs `pi -p` once per issue. That is insufficient because it has no single authority for claimed issues, no bounded concurrency, no retry backoff, no reconciliation when issue state changes, and no safe workspace lifecycle.

A second alternative is to copy Symphony's Codex app-server integration exactly and treat pi as a drop-in command. That is incorrect because pi exposes a different integration contract: RPC mode uses JSONL commands such as `prompt`, `get_state`, `get_session_stats`, and streaming events such as `agent_start`, `turn_end`, and `agent_end`. Scherzo must adapt to pi's protocol rather than invent Codex-shaped fields that pi does not provide.

A third alternative is to build the HTTP dashboard and Linear tool extension first. That would increase scope before the core scheduler is trustworthy. The plan defers these extensions until the daemon can safely poll, dispatch, run, retry, and clean up using logs alone.

## Risks and Countermeasures

The largest safety risk is accidentally running pi in the repository root, an empty unprepared directory, a partially populated workspace, or another shared directory. Countermeasure: implement workspace key sanitization, reject empty, `.` and `..` keys, normalize workspace root and workspace path to absolute paths, assert the workspace path is inside the workspace root, assert the pi subprocess cwd equals the workspace path immediately before launch, and require dispatch configuration to provide either an `after_create` population hook or a `before_run` verification hook. A workflow that intentionally uses pre-populated workspaces can satisfy this requirement with an explicit `before_run` check such as `test -d .git`; omitting both hooks is a configuration error because Scherzo cannot otherwise prove the workspace is usable before pi starts. If `after_create` fails for a workspace that Scherzo just created, Scherzo must best-effort remove that workspace and use a sidecar population marker under the workspace root to remember that the directory is partial if cleanup fails, so the next retry re-runs population or fails safely instead of reusing a half-created directory. Unit tests must prove malicious identifiers such as `../outside`, `.`, `..`, and `A/B` cannot escape the root, failed population does not leave a reusable partial workspace, and integration tests must prove the fake pi sees a populated workspace marker.

The largest feasibility risk is the Erlang/Gleam subprocess boundary. Countermeasure: perform an early spike before the domain and scheduler layers. The spike must prove cwd enforcement, JSONL stdin/stdout framing, maximum line handling, read timeout, child termination, and the chosen stderr strategy. If Erlang ports cannot provide separate stderr in the required shape, revise the plan to use a small wrapper process, a temp-file stderr capture, or another documented strategy before implementing the rest of the service.

The largest integration risk is pi RPC protocol drift or partial misunderstanding. Countermeasure: isolate all JSONL command and event handling in `src/scherzo/agent/pi_rpc.gleam`, cover it with a fake pi RPC shell fixture, use only documented RPC commands `set_session_name`, `set_auto_retry`, `prompt`, `abort`, `get_state`, and `get_session_stats`, and add a compatibility probe that launches the configured command in the prepared per-issue workspace and verifies `get_state` and `set_session_name` responses before any prompt is sent. Before implementing the full pi runner, re-check the current pi RPC documentation or run the no-prompt probe against an installed pi when available; if command names, response shapes, or `extension_ui_response` semantics differ, update this plan's Decision Log and fake fixture before higher-level code depends on the wrong contract. The client must split records only on `\n`, strip an optional trailing `\r`, keep diagnostic stderr out of JSON stdout, and treat malformed stdout as an agent failure.

The largest scheduling risk is duplicate dispatch or unbounded repeated dispatch inside one Scherzo process. Countermeasure: make the orchestrator actor the only owner of `running`, `claimed`, `retry_attempts`, `issue_counters`, and `parked`; check `running`, `claimed`, and `parked` before dispatch; cancel and replace any existing retry timer before storing a new retry for the same issue; keep claims and requeue on retry-poll tracker failures instead of releasing work prematurely; allow `agent.max_concurrent_agents: 0` to pause new dispatch; cap abnormal retries with `agent.max_retry_attempts`; cap normal continuation worker sessions with `agent.max_sessions_per_issue`; and cover dispatch eligibility, retry, parking, un-parking on issue update, retry-timer replacement, retry-fetch failure, and worker-exit state transitions with pure unit tests.

A related operational risk is running two Scherzo processes against the same Linear project before this core plan has a distributed claim or built-in Linear handoff. Countermeasure: document a hard operating constraint that only one Scherzo instance may run per Linear project and workspace root. This plan does not rely on a local lock file as a correctness guarantee because it would not prevent two hosts or two workspace roots from claiming the same issue; a durable claim backend or first-class Linear write path is deferred with the other multi-operator features.

The largest configuration risk is a broken `WORKFLOW.md` reload crashing the daemon, silently changing behavior, continuing to dispatch from stale policy after an operator has edited the file into an invalid state, or leaking a newly configured secret before the redactor knows about it. Countermeasure: load and validate the workflow before startup, poll the workflow file mtime before each dispatch tick, keep the last known good effective config only for reconciliation and already-running workers, mark the current workflow invalid on read/YAML/config errors, block all new dispatch while the current workflow is invalid, register any newly resolved secret values with the logger immediately after a valid reload and before logging reload-derived errors or summaries, and log an operator-visible error until a later valid reload resumes dispatch.

The largest external-service risk is Linear API schema or network failure. Countermeasure: keep GraphQL query construction isolated in `src/scherzo/linear.gleam`, map transport, status, GraphQL, and payload errors to typed error categories, skip dispatch on candidate-fetch failure, keep running workers when state refresh fails, and include a real-integration profile that is skipped unless credentials are deliberately supplied. Before or during Milestone 6, verify the current Linear GraphQL assumptions that are cheapest to check, especially `project.slugId`, `[ID!]` state refresh variables, pagination cursors, and relation payload shape; fake tests remain required, but any mismatch discovered from docs or a credential-gated smoke run must update this plan and the fake payloads before the orchestrator depends on them.

A subtle cleanup risk is that a worker can observe a terminal Linear state after a successful pi turn before the orchestrator's next reconciliation tick sees it. Countermeasure: the worker success result must include the final refreshed issue snapshot or final state classification, and the orchestrator must clean the stored workspace path immediately when a normal worker exit reports a terminal final state instead of scheduling a continuation retry.

The largest secrecy risk is accidental logging of API keys or authorization headers through errors, hook output, HTTP debug payloads, or subprocess diagnostics. Countermeasure: redact by field name and by known configured secret value, never log raw HTTP headers, truncate hook and pi diagnostics, and add tests for secrets appearing inside nested error strings.

If implementation stops halfway, each milestone leaves either a compile-tested library layer or an executable service that can be safely ignored. There is no data migration. Reverting the relevant commits removes the service. Workspaces live under the configured workspace root and can be deleted manually after stopping Scherzo.

## Progress

- [x] (2026-04-28 15:01Z) Verified the repository had no tracked implementation files before this plan; only VCS metadata existed.
- [x] (2026-04-28 15:01Z) Read the Symphony draft service specification and pi RPC documentation; captured the required adaptations in this plan.
- [x] (2026-04-28 15:11Z) Stored the Symphony draft service specification locally at `docs/SYMPHONY_SPEC.md` for repository-relative reference.
- [x] (2026-04-28 15:45Z) Revised the plan after adversarial review to add early pi subprocess validation, explicit workspace population, retry/session caps, issue parking, a pause control, a pi compatibility probe, and deterministic fake integration validation.
- [x] (2026-04-28 16:05Z) Revised the plan after spec-coverage review to add explicit Symphony coverage mapping, invalid-reload dispatch gating, per-workspace pi probing, retry timer replacement, retry-poll failure handling, worker-observed terminal cleanup, logger-sink resilience, and CLI negative-path validation.
- [x] (2026-04-28 16:43Z) Tightened execution readiness by filling the milestone 1 step gap, requiring an explicit population or verification hook, documenting the single-instance operating constraint, clarifying continuation loops, requiring secret registration on reload, and specifying failed-population cleanup.
- [x] (2026-04-28 17:09Z) Created the Gleam project scaffold, `devenv.nix`, a direnv-compatible `.envrc`, README, ignore rules, dependency smoke tests, and `examples/WORKFLOW.md` with an explicit `hooks.after_create` population hook; `direnv exec . gleam test` passed with 6 tests.
- [x] (2026-04-28 17:12Z) Proved the Erlang port and fake pi RPC subprocess boundary with `src/scherzo/port.gleam`, `src/scherzo_port_ffi.erl`, `test/fixtures/fake_pi_rpc.sh`, and port tests for cwd, stdin/stdout JSONL separation, stderr diagnostics capture, termination, and 10 MB line handling; `direnv exec . gleam test` passed with 10 tests.
- [x] (2026-04-28 17:14Z) Implemented domain types, typed errors, structured logging helpers, retry/session counters, parked issue state, and deterministic tests in `test/domain_test.gleam` and `test/log_test.gleam`; `direnv exec . gleam test` passed with 18 tests.
- [x] (2026-04-28 17:18Z) Implemented `WORKFLOW.md` loading, YAML front matter parsing, config defaults, env/path resolution, validation, dynamic reload state, pause semantics, limit validation, and resolved-secret reporting.
- [x] (2026-04-28 17:18Z) Implemented strict Liquid-like prompt rendering with `issue`, `attempt`, `if`, `else`, and `for` support plus unknown-variable/filter failures; `direnv exec . gleam test` passed with 40 tests.
- [x] (2026-04-28 17:22Z) Implemented workspace path safety, directory lifecycle, hook execution, population and verification hooks, sidecar population marker handling, best-effort after-run/before-remove hooks, and stored-path cleanup tests; `direnv exec . gleam test` passed with 48 tests.
- [x] (2026-04-28 17:27Z) Implemented the Linear tracker abstraction and GraphQL client with candidate and state-refresh request construction, injectable transport, pagination, payload normalization for labels/blockers/timestamps, and status/GraphQL/payload/end-cursor error mapping; `direnv exec . gleam test` passed with 55 tests.
- [x] (2026-04-28 17:30Z) Implemented pure orchestrator scheduling, eligibility, sorting, reconciliation, continuation and failure retry scheduling, retry replacement effects, parking/unparking, token accounting, and worker-exit transitions in `src/scherzo/orchestrator/core.gleam`; `direnv exec . gleam test` passed with 63 tests.
- [x] (2026-04-28 17:35Z) Implemented the pi RPC subprocess client, command/event codec helpers, workspace-scoped compatibility probe, extension UI cancellation, stats decoding, and agent runner with fake-pi integration tests for probe ordering, prompt rendering, terminal success, failure paths, and in-worker continuation; `direnv exec . gleam test` passed with 73 tests.
- [ ] Implement the runtime orchestrator actor, dependency-injected test harness, CLI startup, poll loop, retry timers, and graceful shutdown.
- [ ] Run full validation, update README and this plan with final outcomes, and commit the completed implementation.

## Surprises & Discoveries

- Observation: The repository had no implementation files at the start of plan authoring, so there are no existing modules, commands, tests, or application docs to preserve.
  Evidence: `pwd && ls -la` from the repository root showed only `.git` and `.jj` directories before `docs/plans/implement-scherzo.md` was written.

- Observation: pi has a documented non-Node integration mode that fits a Gleam service: `pi --mode rpc` communicates over LF-delimited JSON Lines on stdin/stdout.
  Evidence: The pi RPC documentation defines commands such as `prompt`, `get_state`, `get_session_stats`, and events such as `agent_start`, `turn_end`, and `agent_end`.

- Observation: The host has `devenv` installed but the default direnv stdlib did not define `use devenv` until the devenv-provided direnvrc was sourced.
  Evidence: `direnv exec . gleam new ...` first failed with `use_devenv: command not found`; updating `.envrc` to source `devenv direnvrc` before `use devenv` let `direnv exec . gleam --version` print `gleam 1.15.4`.

- Observation: Erlang ports provide stdout line messages but not a convenient independent live stderr stream for this use case.
  Evidence: The passing port tests use a wrapper that redirects child stderr into a temporary diagnostics file; stdout returned only the JSON line while `read_diagnostics` later contained `diagnostic`.

- Observation: `yay.parse_string` can return an error shape that is not safe to destructure exhaustively in Scherzo's workflow loader.
  Evidence: The initial invalid-YAML test crashed with `CaseClause(YamlError(UnexpectedParsingError))`; Scherzo now maps any YAML parse failure to a stable `WorkflowParseError("YAML parse error")` without relying on the package's internal error representation.

## Decision Log

- Decision: Implement the core Symphony conformance profile first and defer the optional HTTP dashboard, optional `linear_graphql` client-side pi tool, optional SSH worker extension, and durable retry queue.
  Rationale: The core scheduler/runner provides the useful daemon behavior and safety invariants. The deferred features are explicitly optional or future work in the source specification and would add substantial integration risk before the core is proven.
  Date: 2026-04-28

- Decision: Use `pi` as the workflow front matter key instead of `codex`, and use pi-specific runtime fields such as `pi.command`, `pi.turn_timeout_ms`, `pi.read_timeout_ms`, and `pi.stall_timeout_ms`.
  Rationale: The requested implementation uses pi, not Codex. Keeping a `codex` key would make Scherzo's public contract misleading.
  Date: 2026-04-28

- Decision: Launch pi through RPC mode with a default command of `pi --mode rpc --no-session`, executed with `bash -lc` in the per-issue workspace.
  Rationale: RPC mode is pi's documented process-integration protocol. `--no-session` avoids accidental cross-issue session reuse by default; continuation within one worker uses the same live pi process.
  Date: 2026-04-28

- Decision: Implement a small strict Liquid-like renderer in Gleam instead of adopting a non-Liquid template package.
  Rationale: Symphony requires unknown variables and unknown filters to fail. A purpose-built renderer for variables, `if`, `else`, and `for` blocks is enough for issue prompts and avoids semantic mismatch with Mustache-style packages.
  Date: 2026-04-28

- Decision: Detect workflow changes by polling file metadata before every dispatch tick rather than adding an OS file watcher.
  Rationale: The spec requires dynamic reload without restart, not a specific watcher implementation. Metadata polling is portable, simple, testable, and defensive against missed events.
  Date: 2026-04-28

- Decision: Use structured key-value logs on stderr as the required observability surface.
  Rationale: Logs are required by the source specification and sufficient for core operation. A dashboard can be added later from the same orchestrator state.
  Date: 2026-04-28

- Decision: Add an early subprocess/RPC feasibility milestone before building the domain, Linear, and orchestrator layers.
  Rationale: The Erlang port boundary and pi JSONL protocol are the riskiest technical assumptions. Proving cwd, framing, timeout, termination, and stderr handling early prevents a late rewrite after higher-level code depends on a broken process abstraction.
  Date: 2026-04-28

- Decision: Treat workspace population as an explicit workflow responsibility and make the example workflow clone from `REPO_URL` in `hooks.after_create`.
  Rationale: Scherzo cannot safely infer how to obtain a team's project code. A scheduler that launches pi in an empty directory is not useful, so the minimum runnable workflow and tests must prove the workspace is populated before pi starts.
  Date: 2026-04-28

- Decision: Add `agent.max_retry_attempts`, `agent.max_sessions_per_issue`, in-memory `parked` issues, and a reloadable pause via `agent.max_concurrent_agents: 0`.
  Rationale: This core plan defers built-in Linear writes. Without caps, an active issue can be retried forever and burn tokens. Parking prevents repeated dispatch in one process until Linear shows the issue changed or the operator restarts the service.
  Date: 2026-04-28

- Decision: Add `pi.compatibility_probe` with a default of `true`, but run it only after the per-issue workspace has been prepared.
  Rationale: Fake-pi tests prove Scherzo's client but not the operator's installed pi. The probe must still honor the strongest filesystem invariant: no pi process is launched outside a per-issue workspace. Running the no-prompt probe after workspace preparation catches missing or incompatible pi RPC commands before Scherzo sends issue instructions or consumes model tokens.
  Date: 2026-04-28

- Decision: Treat an invalid current `WORKFLOW.md` reload as a dispatch blocker even when a last known good config exists.
  Rationale: The Symphony spec requires workflow read/YAML errors to block new dispatch until fixed. The last known good config remains useful for reconciliation and already-running workers, but new work must not start from stale policy after an operator has edited the workflow into an invalid state.
  Date: 2026-04-28

- Decision: Model retry scheduling as replacement of any existing retry timer for the same issue, and requeue instead of releasing claims when retry candidate fetch fails.
  Rationale: Duplicate timers can create duplicate dispatch pressure. Linear fetch failures during retry handling are transient tracker failures, not proof that the issue is no longer eligible.
  Date: 2026-04-28

- Decision: Include final issue state in successful worker results and clean workspaces when a worker observes a terminal state.
  Rationale: Waiting only for startup cleanup or the next reconciliation tick can miss terminal cleanup when the worker itself sees the transition and exits normally before reconciliation runs.
  Date: 2026-04-28

- Decision: Add an explicit Symphony spec coverage matrix to this plan and README-facing documentation.
  Rationale: Scherzo deliberately adapts the Symphony contract from Codex to pi and defers optional extensions. A section-by-section coverage map makes implemented, adapted, deferred, and intentionally omitted behavior auditable.
  Date: 2026-04-28

- Decision: Require dispatchable workflows to include either `hooks.after_create` or `hooks.before_run`, and best-effort remove newly-created workspaces when `after_create` fails.
  Rationale: Launching pi in an empty or half-populated directory is both unsafe and operationally useless. Requiring an explicit population or verification hook makes the operator's workspace contract visible, while cleanup on failed population ensures retries do not silently reuse partial state.
  Date: 2026-04-28

- Decision: Document Scherzo as a single-instance service per Linear project and workspace root until durable claiming or Linear writes are implemented.
  Rationale: The in-memory orchestrator prevents duplicate dispatch only inside one process. A local lock file would not protect multiple hosts or multiple workspace roots, so the honest core contract is an explicit operating constraint and deferred distributed claiming.
  Date: 2026-04-28

- Decision: Treat `agent.max_turns` and `agent.max_sessions_per_issue` as two separate continuation limits.
  Rationale: A worker may keep one pi RPC process alive for several turns, but the orchestrator may later start a fresh worker session if the issue remains active. Naming the two loops prevents accidental double-dispatch or premature parking.
  Date: 2026-04-28

- Decision: Register newly resolved secrets immediately after every valid workflow reload.
  Rationale: A reload can change `tracker.api_key` or another secret indirection. The logger must know the new value before any reload-derived diagnostics, summaries, HTTP errors, or hook errors can expose it.
  Date: 2026-04-28

- Decision: Keep `.envrc` semantically aligned with `use devenv` while sourcing `devenv direnvrc` when needed.
  Rationale: This host's direnv installation did not know the devenv integration function by default. Sourcing the checked host command's generated direnvrc preserves the documented direnv workflow and avoids adding a separate non-devenv setup path.
  Date: 2026-04-28

- Decision: Capture subprocess stderr in a temporary diagnostics file rather than merging it into the Erlang port stdout stream.
  Rationale: pi RPC stdout must remain parseable JSON Lines. The file strategy keeps diagnostics available for logging while preventing stderr text from corrupting JSON framing. Live stderr streaming is deferred because the core tests only require stdout safety and diagnostics availability.
  Date: 2026-04-28

## Outcomes & Retrospective

(To be filled at major milestones and at completion.)

## Context and Orientation

This repository starts empty. The implementation will create a standard Erlang-target Gleam application in the repository root. The package and executable are named `scherzo`.

The key concepts used throughout the implementation are these. An issue is a normalized Linear ticket with stable fields such as `id`, `identifier`, `title`, `state`, labels, blockers, and timestamps. A workspace is a filesystem directory assigned to exactly one issue identifier under a configured workspace root. A population hook is workflow-owned shell code, normally `hooks.after_create`, that puts the target project files into a newly created workspace before pi is ever launched. A verification hook is workflow-owned shell code, normally `hooks.before_run`, that proves an existing or pre-populated workspace is suitable before pi starts. Dispatch configuration is valid only when at least one of these hooks is present; a workflow that intentionally uses pre-populated workspaces should set `before_run` to a real check such as `test -d .git`. The orchestrator is the single long-running scheduler process that owns in-memory state and decides which issues run, retry, stop, park, unpark, or release. This single-authority guarantee applies only inside one process; operators must not run two Scherzo instances against the same Linear project and workspace root until a future durable claim or Linear write path exists. A worker is a spawned process that prepares one issue workspace, renders the prompt, starts pi RPC mode inside that workspace, streams pi events back to the orchestrator, and exits with a normal or abnormal reason. There are two continuation loops: inside one worker, `agent.max_turns` limits how many turns one live pi RPC process may take; after a worker exits normally while the issue remains active, `agent.max_sessions_per_issue` limits how many fresh worker sessions the orchestrator may schedule for that same issue. A retry entry is a scheduled future attempt for one issue and includes enough runtime identity, such as a timer handle or generation token, for the actor to cancel replaced timers and ignore stale timer messages. An issue counter records failure retry count and normal worker-session count for one issue in this process. A parked issue is an active Linear issue that Scherzo will not dispatch again because it hit a safety cap; it becomes eligible again only when Linear returns the same issue with a newer `updated_at` value, or after the service restarts. A terminal issue state is a Linear state such as `Closed`, `Cancelled`, `Canceled`, `Duplicate`, or `Done`; terminal states trigger workspace cleanup. An active issue state is a Linear state such as `Todo` or `In Progress`; active states are candidates for dispatch unless paused, claimed, running, parked, blocked, or over a concurrency limit.

The planned repository layout is as follows. `gleam.toml` declares the package. `src/scherzo.gleam` is the package entry point. `src/scherzo/main.gleam` parses CLI arguments and starts the service. `src/scherzo/domain.gleam` defines shared records and error types. `src/scherzo/workflow.gleam` reads `WORKFLOW.md`. `src/scherzo/config.gleam` resolves defaults, environment variables, paths, and validation. `src/scherzo/template.gleam` renders strict issue prompts. `src/scherzo/workspace.gleam` manages workspace creation, reuse, safety checks, hooks, and cleanup. `src/scherzo/linear.gleam` implements the Linear GraphQL client. `src/scherzo/tracker.gleam` defines the tracker abstraction used by tests and the orchestrator. `src/scherzo/orchestrator/core.gleam` contains pure scheduling and state-transition logic. `src/scherzo/orchestrator/service.gleam` contains the actor, timers, side effects, and an injectable dependency record used by deterministic service tests. `src/scherzo/agent/pi_rpc.gleam` implements the pi RPC JSONL client. `src/scherzo/agent/probe.gleam` implements the pi compatibility probe. `src/scherzo/agent/runner.gleam` combines workspace, prompt rendering, pi RPC, and post-turn issue refresh. `src/scherzo/log.gleam` emits structured logs. `src/scherzo/port.gleam` and `src/scherzo_port_ffi.erl` wrap Erlang ports for subprocesses. Tests live in `test/*_test.gleam`; shell fixtures live in `test/fixtures/`.

## Preconditions and Verified Facts

At plan-authoring time the repository root contained no implementation files. After this plan is added, the current repository root contains `docs/plans/implement-scherzo.md`, `docs/SYMPHONY_SPEC.md`, and VCS metadata, but still no `gleam.toml`, no `src/`, no `test/`, and no existing command scripts. Because the root is no longer empty once this plan exists, scaffold Gleam files in an ignored temporary directory and copy only the generated project files into the root.

The local source specification is `docs/SYMPHONY_SPEC.md`. Use it as the repository-relative reference for the unadapted Symphony requirements when checking details, but treat this ExecPlan as the implementation authority where it deliberately differs for Scherzo, Gleam, devenv/direnv, pi, and Linear.

The host running development commands must have Nix, `devenv`, and `direnv` available before this plan starts. Those tools are outside the repository. Once `.envrc` and `devenv.nix` exist, contributors should run `direnv allow` from the repository root and let direnv load the devenv shell automatically.

The service's runtime external dependencies are Linear network access, `LINEAR_API_KEY` or an equivalent configured API key value, a `pi` executable on `PATH` wherever Scherzo is run, a workflow-provided way to populate or verify workspaces, and the operational discipline to run only one Scherzo instance per Linear project and workspace root. The example workflow uses `REPO_URL` and `git clone "$REPO_URL" .` in `hooks.after_create`; operators may replace that hook with any trusted population script that leaves project files in the workspace, or with a `before_run` verification hook such as `test -d .git` when workspaces are intentionally pre-populated. Tests must not require real Linear credentials, a real pi install, or a network-accessible repository; tests use fake HTTP transports, local fixture repositories, and a fake pi RPC script.

The pi RPC protocol facts used by this plan are restated here so implementation does not depend on external memory. Start pi with `pi --mode rpc`. Send one JSON object per line to stdin. Read one JSON object per line from stdout. Split only on `\n` and strip a trailing `\r` if present. A command may include an `id` field, and the matching response includes the same `id`. The `prompt` command accepts `{"type":"prompt","message":"..."}` and returns a response when accepted; actual work continues through streamed events. The events needed for core behavior include `agent_start`, `agent_end`, `turn_start`, `turn_end`, `message_start`, `message_update`, `message_end`, `tool_execution_start`, `tool_execution_update`, `tool_execution_end`, `queue_update`, `compaction_start`, `compaction_end`, `auto_retry_start`, `auto_retry_end`, `extension_error`, and `extension_ui_request`. The `get_state` command returns fields including `sessionId`, `sessionFile`, and `isStreaming`. The `get_session_stats` command returns token totals under `tokens.input`, `tokens.output`, `tokens.cacheRead`, `tokens.cacheWrite`, and `tokens.total`. The compatibility probe must not send a prompt and must launch pi with cwd exactly equal to the prepared per-issue workspace path; it only verifies command/response behavior and session state.

## Scope Boundaries

In scope for this plan: a Gleam Erlang application; devenv and direnv setup; CLI startup with optional positional workflow path; `WORKFLOW.md` parsing; typed config; dynamic reload by mtime polling; Linear candidate, terminal, and state-refresh reads; workflow-owned workspace population or verification hooks; validation that dispatchable workflows include either `hooks.after_create` or `hooks.before_run`; workspace creation, reuse, failed-population cleanup, hooks, and terminal cleanup; strict prompt rendering; early subprocess feasibility testing; pi RPC compatibility probing; pi RPC launch and event handling; bounded global and per-state concurrency; `agent.max_concurrent_agents: 0` pause semantics; Todo blocker filtering; exponential retry backoff; retry/session caps; in-memory parked issue handling; continuation retries after normal worker exits; active-run reconciliation; stall detection; token/runtime accounting from pi stats; deterministic service tests with fake tracker and fake agent dependencies; and structured logs.

Out of scope for this plan: a web dashboard or JSON HTTP API, a pi extension that exposes a `linear_graphql` tool, SSH worker execution, first-class Linear ticket writes from the orchestrator, built-in repository checkout logic beyond example hooks, distributed claiming across multiple Scherzo processes or hosts, persisted retry timers or live session recovery after process restart, multi-tracker support in production, and production packaging beyond `gleam run` and `gleam export erlang-shipment` readiness.

The local source specification in `docs/SYMPHONY_SPEC.md` includes optional HTTP server endpoints, an optional human-readable dashboard, an optional SSH worker appendix, an optional `linear_graphql` tool extension, and TODO persistent scheduler state. Those features are explicitly deferred for Scherzo in this plan. The implementation should still name these as deferred features in `README.md` so operators do not assume they exist. README must also state that Linear write/handoff behavior must be supplied by the workflow environment until the optional `linear_graphql` extension or another write path exists.

## Symphony Spec Coverage Matrix

This plan covers `docs/SYMPHONY_SPEC.md` as follows. Sections 1 through 4 are implemented directly in Scherzo terms: Scherzo remains a long-running tracker reader, workspace manager, orchestrator, agent runner, and logger, with Codex-specific names adapted to pi-specific names. Sections 5 and 6 are implemented with the documented schema adaptation from top-level `codex` to top-level `pi`; workflow discovery, YAML front matter parsing, defaults, environment indirection, path resolution, dynamic reload, startup validation, and per-tick dispatch validation remain required, and an invalid current workflow file blocks new dispatch until a valid reload succeeds.

Sections 7 and 8 are implemented with the same single-authority in-memory orchestrator model, dispatch sorting, blocker rules, bounded concurrency, active-run reconciliation, continuation retries, exponential failure retries, startup terminal cleanup, and tracker/filesystem-driven restart recovery. Scherzo adds retry/session caps and in-memory parking as a safety extension because built-in Linear writes are deferred. Section 9 is implemented directly: deterministic sanitized workspaces, root containment, lifecycle hooks, hook timeouts, and agent cwd enforcement are core requirements. The pi compatibility probe is treated as an agent subprocess and therefore also runs only inside a prepared per-issue workspace.

Section 10 is implemented as a pi RPC adaptation rather than a Codex app-server implementation. Scherzo uses pi's JSON Lines RPC commands and events as the protocol source of truth, auto-cancels blocking extension UI dialogs, logs fire-and-forget UI requests, fails unsupported blocking behavior, and documents that sandboxing and approvals are controlled by the operator's pi command and host environment. Sections 11 and 12 are implemented directly for Linear reads and strict prompt rendering. Section 13 is implemented through structured stderr logs; the optional snapshot API, HTTP dashboard, and human-readable status surface are deferred. Sections 14 and 15 are implemented through typed failure handling, retry/recovery behavior, workspace safety, secret redaction, trusted-hook documentation, and explicit high-trust harness posture documentation.

Section 16's reference algorithms are implemented as adapted algorithms in `src/scherzo/orchestrator/core.gleam`, `src/scherzo/orchestrator/service.gleam`, `src/scherzo/workspace.gleam`, and `src/scherzo/agent/runner.gleam`. Sections 17 and 18 are covered by deterministic tests named in this plan, except extension conformance items for deferred optional features and real-integration checks that are explicitly optional and must be recorded as completed or skipped in Outcomes & Retrospective. Appendix A's SSH worker extension is deferred.

## Milestones

Milestone 1 creates the project, development shell, dependency declarations, dependency smoke tests, README, and example workflow. At the end, `direnv exec . gleam test` runs the generated test and dependency smoke tests successfully, and `examples/WORKFLOW.md` includes an explicit `hooks.after_create` population hook using `REPO_URL`. This comes first because every later step depends on a reproducible Gleam environment, verified package APIs, and a realistic minimum workflow.

Milestone 2 proves the subprocess and pi-RPC-shaped boundary before higher-level code depends on it. At the end, a fake pi script can be launched through the Erlang port wrapper with an exact cwd, exchange LF-delimited JSONL over stdin/stdout, expose a documented stderr strategy, time out, and be terminated. This milestone is a feasibility gate: if it fails, revise the process strategy before continuing.

Milestone 3 defines the domain model, errors, logs, issue counters, and parked issue records. At the end, tests can construct normalized issues, runtime state, retry entries, parked entries, pi session metadata, and structured log lines without touching the network or filesystem.

Milestone 4 implements workflow loading, configuration, reload state, pause semantics, safety caps, pi compatibility probe configuration, and strict prompt rendering. At the end, `WORKFLOW.md` examples with YAML front matter produce a typed effective config, invalid reloads preserve the last known good config for reconciliation while blocking new dispatch until fixed, `agent.max_concurrent_agents: 0` pauses dispatch, and prompt rendering fails on unknown variables or filters.

Milestone 5 implements workspace safety and hooks. At the end, Scherzo can create and reuse deterministic workspaces, prove that unsafe identifiers cannot escape the root, run population and lifecycle hooks with timeouts, prove hook cwd is the workspace, clean up newly-created workspaces after failed population, and clean terminal workspaces. This milestone directly addresses the highest filesystem safety risk.

Milestone 6 implements the Linear tracker client. At the end, fake HTTP tests prove query construction, pagination, normalization, blockers, labels, state refresh, terminal fetch, and error mapping. Real Linear smoke tests remain optional and credential-gated.

Milestone 7 implements pure orchestrator scheduling. At the end, tests prove dispatch eligibility, sorting, pause behavior, concurrency, retries, retry caps, session caps, parking, un-parking on issue update, reconciliation decisions, stall decisions, token accounting, and snapshot rows without starting timers or subprocesses. This keeps the mutable actor small.

Milestone 8 implements the full pi RPC client, per-workspace compatibility probe, and agent runner. At the end, a fake pi script can be launched in a populated workspace, pass a no-prompt probe from that same cwd, receive prompts, emit JSONL events, return stats, handle extension UI cancellation, keep one pi process alive for in-worker continuation up to `agent.max_turns`, and cause the runner to report normal, terminal, failed, timed-out, or stalled outcomes without consuming real model tokens.

Milestone 9 wires the runtime actor, dependency-injected deterministic service harness, CLI, graceful shutdown, and fake end-to-end service validation. At the end, tests prove startup validation, terminal cleanup, immediate ticks, dispatch, retry timer replacement, retry-poll failure handling, invalid-reload dispatch blocking, worker-observed terminal cleanup, logger-sink resilience, worker termination, CLI negative paths, and shutdown child cleanup using fake dependencies.

Milestone 10 performs full validation, documentation, real-integration notes, and retrospective. At the end, all tests pass, the README documents setup, configuration, safety posture, workspace population, deferred Linear writes, production checks, and skipped or completed real Linear validation, and this plan records what was completed and what remains deferred.

## Plan of Work

Create the Gleam project at the repository root with an Erlang target. Add dependencies for JSON, HTTP, YAML, filesystem, time, OTP actors, and Erlang interop. Use Erlang-target Gleam because Scherzo needs BEAM processes, timers, and OS subprocess ports. Before adding orchestration code, validate that every chosen dependency can be imported and used for its minimum purpose in `test/dependency_smoke_test.gleam`; this smoke test is the early check that package names, module names, and basic APIs match the assumptions in this plan.

Implement the subprocess foundation early in `src/scherzo/port.gleam` and `src/scherzo_port_ffi.erl`. The public API must launch `bash -lc <command>` with a specified cwd, send stdin lines, receive LF-delimited stdout lines with a maximum line length of 10 MB, capture diagnostics without feeding them into the JSON parser, observe exit status, enforce read timeouts in the Gleam layer, and terminate the process. If native Erlang ports cannot provide live separated stderr, implement a documented wrapper strategy such as redirecting stderr to a temp file that the port wrapper tails or reads after exit. Do not parse JSON in the FFI layer.

Implement domain types in `src/scherzo/domain.gleam`. The `Issue` record must include `id`, `identifier`, `title`, `description`, `priority`, `state`, `branch_name`, `url`, `labels`, `blocked_by`, `created_at`, and `updated_at`. `BlockerRef` must include `id`, `identifier`, and `state`, each optional except where normalization provides values. `WorkflowDefinition` must include `config` as a raw YAML-derived dynamic map and `prompt_template` as a trimmed string. `EffectiveConfig` must include `tracker`, `polling`, `workspace`, `hooks`, `agent`, and `pi` nested records. `AgentConfig` must include `max_concurrent_agents`, `max_turns`, `max_retry_backoff_ms`, `max_retry_attempts`, `max_sessions_per_issue`, and `max_concurrent_agents_by_state`. `PiConfig` must include `command`, timeouts, `auto_retry`, `ui_request_policy`, `compatibility_probe`, and the optional rate-limit payload. `RuntimeState` must include `poll_interval_ms`, `max_concurrent_agents`, `running`, `claimed`, `retry_attempts` whose entries include timer handle or generation identity, `issue_counters`, `parked`, `completed`, aggregate pi totals, and latest rate-limit payload as `Nil` or an opaque JSON value. `IssueCounter` must track `failure_attempts`, `worker_sessions`, and the last `updated_at` value observed when the counter was updated. `ParkedEntry` must track `issue_id`, `identifier`, `reason`, `observed_updated_at`, and `parked_at_ms`. `LiveSession` must use pi names: `session_id`, `pi_rpc_pid`, `last_pi_event`, `last_pi_timestamp`, `last_pi_message`, `pi_input_tokens`, `pi_output_tokens`, `pi_total_tokens`, `last_reported_input_tokens`, `last_reported_output_tokens`, `last_reported_total_tokens`, and `turn_count`.

Implement `src/scherzo/log.gleam` so structured logs are stable and safe. Logs use `key=value` fields on one line, escape spaces, quotes, equals signs, and newlines with JSON-string-style quoting, redact field names containing `token`, `api_key`, `authorization`, or `secret`, and also redact any exact configured secret values registered with the logger. Hook output, pi stderr, and HTTP errors are truncated before logging.

Implement `src/scherzo/workflow.gleam` so it chooses an explicit path if the CLI provided one, otherwise `WORKFLOW.md` in the current working directory. If the file starts with `---`, split front matter at the next line containing exactly `---`, parse that YAML as a map, and trim the remaining Markdown body. If no front matter exists, use an empty map and trim the full file as the prompt body. Return typed errors `MissingWorkflowFile`, `WorkflowParseError`, and `WorkflowFrontMatterNotMap`.

Implement `src/scherzo/config.gleam` so defaults are applied exactly. Linear tracker defaults are endpoint `https://api.linear.app/graphql`, active states `Todo` and `In Progress`, terminal states `Closed`, `Cancelled`, `Canceled`, `Duplicate`, and `Done`, and missing `api_key` for `tracker.kind = linear` is treated as `$LINEAR_API_KEY`. Polling default is `30000` ms. Workspace root default is the host temp directory plus `scherzo_workspaces`; relative roots resolve relative to the directory containing the selected workflow file; `~` expands to the user home; `$VAR_NAME` is resolved only where the config value explicitly contains a single env indirection. Hook timeout default is `60000` ms. Dispatch validation requires either `hooks.after_create` or `hooks.before_run` to be a non-empty script; a workflow with pre-populated workspaces must use `before_run` as an explicit verification or opt-out hook, such as `test -d .git`, instead of omitting both hooks. Agent defaults are `max_concurrent_agents = 10`, `max_turns = 20`, `max_retry_backoff_ms = 300000`, `max_retry_attempts = 5`, `max_sessions_per_issue = 3`, and empty per-state limits. `max_concurrent_agents = 0` is valid and means pause new dispatch; negative values are invalid. Per-state limits must be positive integers; invalid entries are ignored. Pi defaults are `command = "pi --mode rpc --no-session"`, `turn_timeout_ms = 3600000`, `read_timeout_ms = 5000`, `stall_timeout_ms = 300000`, `auto_retry = True`, `ui_request_policy = Cancel`, `compatibility_probe = True`, and no rate-limit payload. Unknown top-level keys must be ignored. Reload state must separately track `last_known_good` and `current_status`; invalid current workflow reads or config validation errors preserve `last_known_good` for reconciliation and existing workers but set `current_status` to invalid so new dispatch is blocked until a later valid reload replaces it. Valid reloads must return the resolved secret values that the service should register with the logger before logging reload-derived details.

Implement `src/scherzo/template.gleam` with a strict Liquid-like subset. Support interpolation `{{ issue.identifier }}`, `{{ attempt }}`, `{{ label }}`, and nested paths. Support `{% if path %}`, `{% else %}`, `{% endif %}`, `{% for label in issue.labels %}`, and `{% endfor %}`. Treat non-empty strings, non-empty lists, non-zero ints, and booleans as truthy; `Nil`, empty strings, empty lists, zero, and `False` are falsey. On the first run, `attempt` is `Nil`, `{{ attempt }}` renders as an empty string, and `{% if attempt %}` is false. On retry or continuation, `attempt` renders as a base-10 integer. Reject unknown variables, unknown tags, malformed blocks, and any expression containing a pipe character `|` with `TemplateRenderError("unknown filter ...")`. If the workflow prompt body is empty, use `You are working on an issue from Linear.`.

Implement `src/scherzo/workspace.gleam`. Sanitize an issue identifier by replacing any character outside `[A-Za-z0-9._-]` with `_`. Reject sanitized keys that are empty, `.`, or `..`. Join the key under the normalized absolute workspace root and prove containment. Create the workspace directory if missing, reuse it if it is already a directory, and fail safely if a non-directory exists at that path. Run `after_create` only when the directory was created in that call; this is where the example workflow clones from `REPO_URL`. Before running `after_create`, create a sidecar population marker under the workspace root, such as `.scherzo-state/<sanitized-key>.populating`, not inside the workspace directory, so clone hooks can still clone into an empty directory. Remove the marker only after `after_create` succeeds. If `after_create` fails for a directory created by this call, run no further hooks, best-effort remove that directory with the same containment checks used for cleanup, leave the sidecar marker if cleanup fails, and return the original hook error. On a later prepare, the sidecar marker means the workspace is partial; Scherzo must clean it and re-run `after_create`, or fail safely without launching pi if cleanup still fails. Run `before_run` before each pi attempt. Run `after_run` after success, failure, timeout, or cancellation and ignore its failure after logging. Run `before_remove` before cleanup and ignore its failure after logging. Use `bash -lc <script>` with the workspace path as cwd and the configured hook timeout. Store the exact workspace path in each running entry; cleanup for an in-flight worker must use the stored path even if a later workflow reload changes `workspace.root`.

Implement `src/scherzo/linear.gleam` with an injectable HTTP transport. Candidate fetch must query Linear issues for the configured project slug and active states, using the project filter field `slugId`, page size 50, and pagination until `hasNextPage` is false. State refresh must query issue IDs with GraphQL variable type `[ID!]`. Terminal fetch must return issues in configured terminal states. Normalize labels to lowercase, priority to optional integer, ISO timestamp strings to optional time values, and blockers from inverse relations where `type` is `blocks`. Map failures to `UnsupportedTrackerKind`, `MissingTrackerApiKey`, `MissingTrackerProjectSlug`, `LinearApiRequest`, `LinearApiStatus`, `LinearGraphqlErrors`, `LinearUnknownPayload`, and `LinearMissingEndCursor`. Never log raw authorization headers or full request bodies containing secrets.

Implement `src/scherzo/orchestrator/core.gleam` as pure functions. Candidate eligibility requires id, identifier, title, and state to be present and non-empty; state lowercased must be active and not terminal; issue must not be running or claimed; issue must not be parked with the same `updated_at`; global and per-state slots must be available; and Todo issues must not have any blocker whose state is missing or non-terminal. If `max_concurrent_agents` is zero, dispatch eligibility is false for every candidate but reconciliation and retry timer bookkeeping still run. Sort candidates by priority ascending with `Nil` last, then oldest `created_at`, then identifier lexicographically. Normal worker exit removes running state, records runtime totals, marks completed for bookkeeping only, and inspects the final worker-observed issue state. If the final state is terminal, request cleanup of the stored workspace path and release the claim without scheduling continuation. If the final state is active, increment `worker_sessions` and schedule a continuation retry after 1000 ms only if the issue has not reached `max_sessions_per_issue`; otherwise park the issue with reason `max_sessions_per_issue`. Abnormal worker exit increments `failure_attempts` and schedules retry with `min(10000 * 2^(attempt - 1), max_retry_backoff_ms)` only if the issue has not reached `max_retry_attempts`; otherwise park the issue with reason `max_retry_attempts`. Scheduling any retry must emit an effect to cancel and replace an existing retry timer for the same issue before storing the new entry. Retry timer handling fetches active candidates; if that fetch fails, it keeps the claim and requeues with error `retry poll failed`; if the fetch succeeds, it un-parks an issue when its `updated_at` differs from the parked value, finds the issue by id, releases the claim if absent, dispatches if slots are available, or requeues with error `no available orchestrator slots`.

Implement `src/scherzo/agent/pi_rpc.gleam`, `src/scherzo/agent/probe.gleam`, and `src/scherzo/agent/runner.gleam`. The RPC client must launch `bash -lc <pi.command>` with cwd exactly equal to the workspace path. It must send JSON commands with unique ids, correlate responses, process events until `agent_end`, and map pi events to Scherzo updates. On startup, send `set_session_name` with `<issue.identifier>: <issue.title>`, send `set_auto_retry` from config, and read `get_state` to learn the pi `sessionId`. Use `session_id = <pi_session_id>-turn-<turn_count>` when a pi session id exists; otherwise use `pi-<os_pid>-turn-<turn_count>`. When `extension_ui_request` is a dialog method (`select`, `confirm`, `input`, or `editor`), immediately send an `extension_ui_response` with `cancelled: true`, log the cancellation, and keep processing unless pi fails. Fire-and-forget UI requests are logged and ignored. On `agent_end`, call `get_session_stats` and emit token totals. If the turn times out, abort and kill pi. If pi exits, emits malformed JSON, or requests unsupported blocking behavior, fail the attempt. The compatibility probe must run only after workspace preparation and `before_run` success, must launch the configured command with cwd exactly equal to that issue's workspace path, must send `set_session_name` and `get_state`, must require successful responses within `pi.read_timeout_ms`, and must terminate the process without sending a prompt. The runner owns only in-worker continuation: it may reuse the same live pi process for concise follow-up prompts until `agent.max_turns` is reached or the refreshed issue becomes terminal or non-active. The orchestrator owns cross-worker continuation: after a normal worker exit with an active final issue, it may schedule a fresh worker session subject to `agent.max_sessions_per_issue`. `WorkerSuccess` must include the final refreshed issue snapshot or an explicit final state classification so the orchestrator can distinguish terminal cleanup from active continuation.

Implement `src/scherzo/orchestrator/service.gleam` as the only process that mutates runtime state. At startup, configure logging, load and validate workflow config, register known secrets with the logger, perform startup terminal workspace cleanup, then schedule an immediate tick; startup must not launch pi because no per-issue workspace exists yet. On every tick, reconcile running issues first, reload workflow if mtime changed, register any newly resolved secrets immediately after a valid reload, validate dispatch config, skip new dispatch if the current workflow status is invalid or paused, fetch candidates, sort and dispatch until slots are exhausted, emit logs, and schedule the next tick with the current effective `polling.interval_ms`. Dispatch invokes the runner, which prepares the workspace, optionally probes pi in that workspace, and only then sends the issue prompt. Reconciliation first checks stalls using `pi.stall_timeout_ms` and last pi event time, then fetches current issue states for running ids. Terminal states terminate the worker and clean the stored workspace path. Non-active non-terminal states terminate without cleanup. Active states update the running issue snapshot and reset counters/parking if `updated_at` changed. State-refresh failure logs and leaves workers running. Logger sink failures are best-effort observability failures and must not crash the orchestrator. The service and README must be explicit that this in-memory authority is not a distributed lock; production operators run one Scherzo instance per Linear project and workspace root.

Implement `src/scherzo/main.gleam`. `gleam run -- --help` prints usage. `gleam run --` uses `WORKFLOW.md` in the current working directory. `gleam run -- path/to/WORKFLOW.md` uses that explicit path and fails startup if it cannot be read. Startup failure exits nonzero and prints a redacted structured error. Normal shutdown exits zero. Keep CLI parsing pure and testable. Keep runtime startup injectable so tests can run one or more deterministic ticks with fake tracker and fake agent dependencies without real Linear or real pi.

## Concrete Steps

1. From the repository root, create `devenv.nix` with packages `pkgs.gleam`, `pkgs.erlang`, `pkgs.rebar3`, `pkgs.nodejs_22`, `pkgs.git`, and `pkgs.jq`. Add a `check` script that runs `gleam format --check src test` and `gleam test` once those directories exist.

2. Create `.envrc` containing `use devenv`.

3. Create `.gitignore` with `_build/`, `.devenv/`, `.direnv/`, `erl_crash.dump`, `.scherzo/`, `node_modules/`, temporary log files, and `test/tmp/`.

4. Run from the repository root: `direnv allow`. Expect direnv to load devenv or tell you which host-level tool is missing. If host-level Nix/devenv/direnv is missing, install it outside the repository and repeat; do not work around this by adding non-devenv setup scripts.

5. Run from the repository root: `mkdir -p .scherzo && direnv exec . gleam new --name scherzo --skip-git .scherzo/scaffold && cp .scherzo/scaffold/gleam.toml ./gleam.toml && cp -R .scherzo/scaffold/src ./src && cp -R .scherzo/scaffold/test ./test && rm -rf .scherzo/scaffold`. Expect `gleam.toml`, `src/scherzo.gleam`, and `test/scherzo_test.gleam` to exist in the repository root while this plan remains in `docs/plans/`.

6. Edit `gleam.toml` to keep the package name `scherzo`, set or confirm Erlang target settings, and keep generated package metadata minimal.

7. Run from the repository root: `direnv exec . gleam add gleam_erlang gleam_otp gleam_json gleam_httpc simplifile yay birl`. Expect `manifest.toml` to be updated.

8. Create `test/dependency_smoke_test.gleam`. Add one tiny test per non-test dependency: decode a small JSON value with `gleam_json`, parse `a: 1` with `yay`, reference a `simplifile` file API without touching real project files, construct a `birl` time value, and import the OTP module that will back the actor. Run `direnv exec . gleam test` and expect the generated test plus smoke tests to pass. If any dependency API is incompatible, replace the dependency now and record the decision in the Decision Log before proceeding.

9. Create `README.md` with a short description of Scherzo, the required host tools, the `direnv allow` workflow, `gleam test`, `gleam run -- [WORKFLOW.md]`, runtime dependencies `LINEAR_API_KEY`, `REPO_URL` or another population or verification hook input, and `pi`, and clear statements that HTTP dashboard, SSH workers, built-in Linear writes, distributed claiming, and `linear_graphql` are deferred and that operators must run only one Scherzo instance per Linear project and workspace root.

10. Create `examples/WORKFLOW.md` with placeholder Linear config, workspace root `.scherzo/workspaces`, `hooks.after_create` that runs `git clone "$REPO_URL" .` and then `test -d .git`, `hooks.before_run` that runs `test -d .git && git status --short`, `pi.command: "pi --mode rpc --no-session"`, `pi.compatibility_probe: true`, `agent.max_concurrent_agents: 1`, `agent.max_retry_attempts: 5`, `agent.max_sessions_per_issue: 3`, and a prompt that references `{{ issue.identifier }}`, `{{ issue.title }}`, `{{ issue.description }}`, labels in a `{% for label in issue.labels %}` loop, and `{{ attempt }}`. The prompt must explicitly tell the agent that Scherzo does not include built-in Linear writes and that the workflow environment must provide whatever CLI/tool is needed for comments or state changes.

11. Run from the repository root: `direnv exec . gleam format` and `direnv exec . gleam test`. Expect exit code 0.

12. Commit milestone 1. If `jj status` works, run `jj commit -m "Scaffold Scherzo Gleam project"`; otherwise run the equivalent git commit.

13. Create `test/fixtures/fake_pi_rpc.sh`. The script must read JSONL commands from stdin, respond to known ids, emit `agent_start`, `turn_start`, a `message_update` text delta, `turn_end`, and `agent_end` for `prompt`, respond to `get_state` with `sessionId: "fake-session"`, respond to `get_session_stats` with token totals, print diagnostics to stderr when `FAKE_PI_STDERR` is set, and support environment variables that force malformed JSON, delayed response, nonzero exit, or never-ending output.

14. Mark `test/fixtures/fake_pi_rpc.sh` executable and document in a comment that it is a test fixture, not a production pi substitute.

15. Create `src/scherzo/port.gleam` and `src/scherzo_port_ffi.erl` with only the types and functions needed by the first port tests: `start(command, cwd)`, `send_line(process, line)`, `read_stdout_line(process, timeout_ms)`, `read_diagnostics(process)`, `terminate(process)`, and `await_exit(process, timeout_ms)`.

16. Create `test/port_test.gleam`. Add a test that starts `bash -lc 'pwd; while read line; do echo "$line"; done'` in a temp directory, sends `hello`, and asserts the first stdout line is the temp directory and the second line is `hello`.

17. Add a port test that starts a command writing JSON to stdout and text to stderr. Assert stdout lines do not contain the stderr text. If the chosen Erlang strategy captures stderr only after exit rather than live, assert and document that behavior in `src/scherzo/port.gleam` and this plan's Decision Log before continuing.

18. Add a port test that starts a command that sleeps forever, calls `terminate`, and asserts `await_exit` returns before the test timeout. Run `direnv exec . gleam test` and expect the port tests to pass.

19. Add a port test for maximum line handling: emit a line just under the 10 MB limit and assert it succeeds, then emit a line over the limit and assert the wrapper returns a line-too-long error without crashing the BEAM process.

20. Run `direnv exec . gleam format` and `direnv exec . gleam test`. If the port wrapper cannot satisfy cwd, JSONL, diagnostics, timeout, and termination requirements, stop and revise this plan before implementing higher layers.

21. Commit milestone 2 with message `Prove Scherzo subprocess boundary` after formatting and tests pass.

22. Create `src/scherzo/domain.gleam`. Add `Issue`, `BlockerRef`, `WorkflowDefinition`, config records, workspace records, run attempt records, live pi session metadata, retry entries with timer handle or generation identity, token totals, rate-limit placeholder, running entries, `IssueCounter`, `ParkedEntry`, and runtime state types exactly as described in the Plan of Work.

23. Create `src/scherzo/error.gleam`. Add error union types for workflow, config, template, workspace, hook, tracker, pi RPC, agent runner, orchestrator, and subprocess errors. Include a function that converts each error to a stable lowercase code string.

24. Create `src/scherzo/log.gleam`. Add `info`, `warn`, `error`, and `debug` functions that produce one line of escaped `key=value` fields. Required fields for issue logs are `issue_id` and `issue_identifier`; required field for pi session logs is `session_id`. Add redaction by field name and by exact registered secret value.

25. Create `test/domain_test.gleam` and `test/log_test.gleam`. Test construction of an issue with labels and blockers, default token totals of zero, default issue counters of zero, a parked issue with an observed `updated_at`, log escaping for spaces/newlines/equals signs, and redaction of both `LINEAR_API_KEY`-like fields and a known secret embedded inside an error string.

26. Run `direnv exec . gleam test`. Expect new domain and log tests to pass.

27. Commit milestone 3 with message `Add Scherzo domain and logging types` after formatting and tests pass.

28. Create `src/scherzo/workflow.gleam`. Implement path selection, file reading, front matter splitting, YAML parsing through `yay`, non-map detection, and prompt trimming.

29. Create `test/workflow_test.gleam`. Add tests for missing file, no front matter, valid map front matter, invalid YAML, non-map YAML, missing closing `---`, prompt trimming, and explicit path selection.

30. Run `direnv exec . gleam test`. Before code completion, the new workflow tests should fail on missing functions. After implementation, expect exit code 0.

31. Create `src/scherzo/config.gleam`. First implement default records for tracker, polling, workspace, hooks, agent, and pi without reading YAML. Add tests in `test/config_test.gleam` asserting every default value, including `agent.max_retry_attempts = 5`, `agent.max_sessions_per_issue = 3`, and `pi.compatibility_probe = True`.

32. Extend `src/scherzo/config.gleam` to read tracker fields from the raw YAML map. Add tests for missing tracker kind validation, unsupported tracker kind validation, missing project slug validation, `LINEAR_API_KEY` fallback, explicit `$OTHER_VAR` resolution, and empty env var treated as missing.

33. Extend `src/scherzo/config.gleam` to resolve paths. Add tests for relative workspace root resolution against the workflow directory, `~` expansion, temp directory default, and `$WORKSPACE_ROOT` env indirection only when the whole value is a single env reference.

34. Extend `src/scherzo/config.gleam` to validate hooks and agent limits. Add tests for invalid hook timeout, both `hooks.after_create` and `hooks.before_run` missing or blank causing dispatch validation failure, `before_run: "test -d .git"` being accepted as an explicit pre-populated-workspace verification hook, invalid max turns, negative `max_concurrent_agents`, valid `max_concurrent_agents = 0` pause, invalid max retry attempts, invalid max sessions per issue, per-state concurrency normalization, and invalid per-state entries ignored.

35. Extend `src/scherzo/config.gleam` to validate pi config. Add tests for invalid pi command, invalid timeouts, `compatibility_probe: false`, and unknown top-level YAML keys being ignored.

36. Add reload tests to `test/config_test.gleam`: a valid first load becomes `last_known_good` and sets `current_status` to valid; a later invalid reload returns an error, preserves the previous effective config for reconciliation, and sets `current_status` to invalid so dispatch validation fails; a later valid reload replaces `last_known_good`, returns any newly resolved secret values for logger registration, and resumes dispatch; a reload that changes `agent.max_concurrent_agents` to `0` pauses future dispatch without crashing.

37. Run `direnv exec . gleam test`. Expect workflow and config tests to pass.

38. Create `src/scherzo/template.gleam` with the strict renderer. Implement parsing for text and variable nodes first, then `if/else`, then `for` loops.

39. Create `test/template_test.gleam`. Add tests that render `{{ issue.identifier }}`, nested optional fields, labels through a loop, first-run `{{ attempt }}` as an empty string, retry `{{ attempt }}` as an integer, `{% if attempt %}` false on first run and true on retry, empty prompt fallback, unknown variable failure, unknown filter failure for `{{ issue.title | upcase }}`, unknown tag failure, and malformed block failure.

40. Run `direnv exec . gleam test`. Expect template tests to pass.

41. Commit milestone 4 with message `Load workflows and render strict prompts` after formatting and tests pass.

42. Create `src/scherzo/path.gleam` and any needed Erlang FFI in `src/scherzo_path_ffi.erl` for absolute path normalization, dirname, temp directory, home directory, environment lookup, and path containment. Keep all public functions returning `Result` instead of crashing.

43. Create `src/scherzo/hooks.gleam`. Implement `run_hook` with cwd, timeout, output truncation, fatal/best-effort mode, structured logging, and use of the already-proven port wrapper.

44. Create `src/scherzo/workspace.gleam`. Implement sanitizer and key rejection only. Add tests in `test/workspace_test.gleam` for `ABC-123`, identifiers containing spaces and slashes, `../outside`, `.`, `..`, an identifier that sanitizes to empty, and `A/B` becoming `A_B` without path traversal.

45. Extend `src/scherzo/workspace.gleam` to create and reuse directories. Add tests for existing directory reuse, existing file collision, workspace root containment, and a sentinel directory next to the root remaining untouched.

46. Extend `src/scherzo/workspace.gleam` to run hooks. Add tests that `after_create` runs only on a new directory, a population hook can create a marker file in the workspace, a failing `after_create` on a newly-created workspace best-effort removes that directory and returns the original hook error, a sidecar `.scherzo-state/<key>.populating` marker prevents reuse of a partial workspace if cleanup fails, `before_run` failure aborts an attempt, `after_run` failure is ignored after logging, `before_remove` failure is ignored, and hook timeout returns a typed error.

47. Add a workspace cleanup test where `workspace.root` is changed after a running entry stores its workspace path. Assert cleanup uses the stored old path and does not delete anything under the new root.

48. Run `direnv exec . gleam test`. Expect workspace and hook tests to pass. Also manually inspect the test temp directory to ensure no file was created outside the intended workspace root.

49. Commit milestone 5 with message `Add safe populated workspace lifecycle` after formatting and tests pass.

50. Create `src/scherzo/tracker.gleam`. Define a `Client` record with functions `fetch_candidate_issues`, `fetch_issues_by_states`, and `fetch_issue_states_by_ids`, each returning typed results. This abstraction is used by tests and the orchestrator.

51. Create `src/scherzo/linear.gleam`. Before locking the fake payload shape, re-check current Linear GraphQL assumptions from available docs or a credential-gated schema/query smoke run when credentials are available: candidate project filter uses `slugId`, state refresh variables use `[ID!]`, pagination uses `endCursor`, and blockers can be normalized from inverse relations where `type` is `blocks`. Implement GraphQL request JSON construction separately from HTTP execution. Add tests in `test/linear_test.gleam` for candidate query variables, project slug filter text using `slugId`, state refresh query containing `[ID!]`, and terminal fetch state variables. If the real schema disagrees, update this plan's Decision Log and tests before continuing.

52. Extend `src/scherzo/linear.gleam` with injectable HTTP POST, `Authorization` header, 30 second network timeout, and non-200 handling. Add tests that a fake HTTP transport sees the expected endpoint and redacted logs never include the authorization value.

53. Extend `src/scherzo/linear.gleam` with candidate pagination. Add tests for pagination across two pages preserving order, empty `fetch_issues_by_states([])` returning empty without HTTP call, and missing end cursor becoming `LinearMissingEndCursor`.

54. Extend `src/scherzo/linear.gleam` with payload normalization. Add tests for labels lowercased, blockers from inverse relations, non-integer priority becoming `Nil`, ISO timestamps parsed, malformed payload becoming `LinearUnknownPayload`, GraphQL errors becoming `LinearGraphqlErrors`, and non-200 becoming `LinearApiStatus`.

55. Run `direnv exec . gleam test`. Expect Linear tests to pass without network access.

56. Commit milestone 6 with message `Implement Linear tracker reads` after formatting and tests pass.

57. Create `src/scherzo/orchestrator/core.gleam`. Implement state constructors, normalized state comparison, candidate sorting, and basic `should_dispatch`. Add tests in `test/orchestrator_core_test.gleam` for priority/null sorting, oldest creation tie-break, identifier tie-break, missing required issue fields rejected, active state accepted, terminal state rejected, running issue rejected, claimed issue rejected, and parked issue rejected when `updated_at` matches the parked value.

58. Extend orchestrator core with slot handling. Add tests for global slot exhaustion, `max_concurrent_agents = 0` pausing all new dispatch, per-state slot exhaustion, Todo with non-terminal blocker rejected, and Todo with terminal blocker accepted.

59. Extend orchestrator core with worker start and normal worker exit. Add tests that worker start records the exact workspace path, normal exit with active final state increments worker session count, active final state schedules continuation retry after 1000 ms below `max_sessions_per_issue`, active final state parks with reason `max_sessions_per_issue` at the cap, and terminal final state requests cleanup without incrementing continuation sessions.

60. Extend orchestrator core with abnormal exit and backoff. Add tests for abnormal retry delays 10000/20000/40000 and cap, failure count incrementing, parking with reason `max_retry_attempts` when the cap is reached, and retry scheduling returning an effect to cancel and replace any existing retry timer for the same issue.

61. Extend orchestrator core with retry timer handling. Add tests that retry candidate-fetch failure keeps the claim and requeues with explicit error `retry poll failed`, absent candidate releases claim, no slots requeues with explicit error `no available orchestrator slots`, parked issue with newer `updated_at` is unparked and counters reset, and parked issue with unchanged `updated_at` is not dispatched.

62. Extend orchestrator core with reconciliation, worker-observed terminal cleanup, and accounting. Add tests for terminal reconciliation requesting cleanup of the stored workspace path, normal worker success with a terminal final issue requesting cleanup and releasing the claim without scheduling continuation, non-active reconciliation stopping without cleanup, active refresh updating running state, active refresh with newer `updated_at` resetting counters, stall disabled when timeout <= 0, stall timeout scheduling retry, and token absolute totals counted as deltas.

63. Run `direnv exec . gleam test`. Expect orchestrator pure tests to pass.

64. Commit milestone 7 with message `Add pure orchestrator scheduling logic` after formatting and tests pass.

65. Create `src/scherzo/agent/pi_rpc.gleam`. Before locking the codec helpers, re-check current pi RPC command and event names from available pi documentation, and if an installed pi is available, run only an equivalent no-prompt `set_session_name` plus `get_state` handshake manually in a temporary prepared workspace after the port wrapper exists. Start with pure JSON encoding/decoding helpers for `set_session_name`, `set_auto_retry`, `get_state`, `prompt`, `abort`, `get_session_stats`, responses, `agent_end`, `message_update`, and `extension_ui_request`. Add tests in `test/pi_rpc_test.gleam` for each codec helper. If the real protocol disagrees, update this plan's Decision Log and fake fixture before continuing.

66. Extend `src/scherzo/agent/pi_rpc.gleam` to launch fake pi through `src/scherzo/port.gleam`. Add tests that the fake pi is launched with cwd equal to a prepared temp issue workspace, startup commands receive responses, prompt events stream in order, `agent_end` completes the prompt, and stats are decoded.

67. Add pi RPC tests for failure paths: malformed JSON fails, delayed response triggers read timeout, turn timeout sends abort then terminates, stderr is logged but not parsed as JSON, nonzero process exit maps to a pi error, and command responses are correlated by id rather than arrival order.

68. Add pi RPC tests for extension UI. The fake pi emits a dialog `extension_ui_request`; assert Scherzo sends `extension_ui_response` with the matching id and `cancelled: true`. The fake pi emits a `notify` request; assert Scherzo logs it and does not send a response.

69. Create `src/scherzo/agent/probe.gleam`. Implement `probe(command, cwd, read_timeout_ms)` by launching the command with cwd equal to a prepared issue workspace, sending `set_session_name` and `get_state`, requiring successful responses, and terminating without sending `prompt`. Add tests that the probe succeeds against fake pi from the workspace cwd, fails on missing command, fails on malformed JSON, fails on timeout, and records no `prompt` input in the fake pi transcript.

70. Create `src/scherzo/agent/runner.gleam`. First implement prompt rendering, per-workspace compatibility probing, and pre-launch failure paths. Add tests in `test/agent_runner_test.gleam` for first prompt rendering, prompt render failure aborting before pi launch, `before_run` failure aborting before probe, `pi.compatibility_probe = true` running the no-prompt probe after `before_run` and before the first prompt, probe failure aborting before the prompt is sent, and `after_run` attempted on prompt, probe, and pi failures once the workspace exists.

71. Extend `src/scherzo/agent/runner.gleam` with one successful pi turn and state refresh. Add tests for fake pi receiving the rendered first prompt, terminal state stopping normally with `WorkerSuccess` carrying the final terminal issue snapshot or classification, state refresh error causing abnormal failure, and pi turn failure causing abnormal failure.

72. Extend `src/scherzo/agent/runner.gleam` with in-worker continuation. Add tests that an active issue after a successful turn receives concise continuation guidance, does not resend the original full task prompt, stops at `agent.max_turns`, preserves the same live fake pi process across continuation turns, and returns control to the orchestrator after `max_turns` so any further fresh-session continuation is governed only by `agent.max_sessions_per_issue`.

73. Run `direnv exec . gleam test`. Expect pi RPC, probe, and agent runner tests to pass without a real pi install.

74. Commit milestone 8 with message `Run pi RPC workers in populated workspaces` after formatting and tests pass.

75. Create `src/scherzo/orchestrator/service.gleam`. Define actor messages `Tick`, `RetryTimerFired(issue_id)`, `WorkerUpdate(issue_id, update)`, `WorkerExited(issue_id, result)`, `WorkflowMaybeChanged`, and `Shutdown`. Define a `Dependencies` record for test injection: tracker factory, agent runner function, workspace cleanup function, clock, timer scheduler with cancel support, and logger sink that may report failure without crashing the actor.

76. Implement startup validation in `src/scherzo/orchestrator/service.gleam`. Add tests in `test/orchestrator_service_test.gleam` that startup fails on invalid config, missing Linear API key is redacted, startup does not launch pi or run the compatibility probe before any per-issue workspace exists, and startup fails before scheduling ticks on workflow/config validation errors.

77. Implement startup terminal workspace cleanup. Add tests that terminal cleanup runs, uses workspace identifiers from terminal issues, continues startup on tracker cleanup fetch failure, and logs cleanup failures without crashing.

78. Implement one tick with reconciliation before dispatch. Add tests that first tick reconciles before fetching candidates, candidate fetch failure skips dispatch, paused config with `max_concurrent_agents = 0` skips dispatch but still reconciles, invalid current workflow status skips dispatch but still reconciles, logger sink failure during tick does not crash the actor, and valid candidates spawn up to slots.

79. Implement worker update and worker exit handling. Add tests that worker updates refresh live session state, worker normal exit with active final state schedules continuation retry below cap, worker normal exit with active final state parks at session cap, worker normal exit with terminal final state cleans the stored workspace path and releases the claim without continuation, worker abnormal exit schedules exponential retry below cap, and worker abnormal exit parks at retry cap.

80. Implement retry timer handling in the actor. Add tests that retry timer dispatches when candidate is present, releases claim when absent, requeues when slots are unavailable, requeues and keeps the claim when candidate fetch fails with error `retry poll failed`, un-parks when candidate `updated_at` changed, cancels an old timer when a replacement retry is scheduled for the same issue, and ignores stale timer firings that do not match the currently stored retry entry.

81. Implement workflow mtime reload. Add tests that a valid reload changes future poll interval and pause behavior, invalid reload logs an error, keeps the last known good config for reconciliation, blocks new dispatch while invalid, a later valid reload resumes dispatch, newly resolved secret values from that valid reload are registered before reload-derived logs are emitted, and a reload that changes workspace root does not alter stored workspace paths for already running workers.

82. Implement graceful shutdown. Add tests that shutdown terminates running workers, terminates or cancels live pi subprocesses through the runner handle, cancels retry timers, does not leave fake pi processes alive, and returns a normal shutdown result.

83. Create `src/scherzo/main.gleam`. Implement pure CLI parsing for no args, one workflow path arg, `--help`, and too many args. Add `test/main_test.gleam` for default path, explicit path, nonexistent explicit workflow path, missing default `WORKFLOW.md`, too many args, help, and redacted startup failure output.

84. Update generated `src/scherzo.gleam` to call `scherzo/main.main` or expose the application entry point expected by `gleam run`.

85. Add a deterministic end-to-end service test in `test/service_integration_test.gleam` using the dependency-injected service entry point, fake tracker, fake workspace population hook that writes `POPULATED`, and fake pi. Run one startup and one tick. Assert logs include workflow loaded, candidates fetched, workspace path, populated marker created, pi probe succeeded from that same workspace path before any prompt transcript entry, dispatch started, populated marker observed by fake pi, pi session started, worker exited, and no real Linear or real pi command was invoked.

86. Run `direnv exec . gleam test`. Expect all tests to pass.

87. Run `direnv exec . gleam run -- --help`. Expect usage text that includes `gleam run -- [path-to-WORKFLOW.md]`, `LINEAR_API_KEY`, `REPO_URL` or workspace population/verification hooks, `pi --mode rpc`, `agent.max_concurrent_agents: 0` as the pause mechanism, and the one-instance-per-project/root operating constraint.

88. Commit milestone 9 with message `Wire Scherzo CLI and orchestrator runtime` after formatting and tests pass.

89. Update `README.md` with the final workflow schema and a compact Symphony coverage/deviation matrix. Include all core keys: `tracker.kind`, `tracker.endpoint`, `tracker.api_key`, `tracker.project_slug`, `tracker.active_states`, `tracker.terminal_states`, `polling.interval_ms`, `workspace.root`, all four hooks, `hooks.timeout_ms`, `agent.max_concurrent_agents`, `agent.max_turns`, `agent.max_retry_backoff_ms`, `agent.max_retry_attempts`, `agent.max_sessions_per_issue`, `agent.max_concurrent_agents_by_state`, `pi.command`, `pi.turn_timeout_ms`, `pi.read_timeout_ms`, `pi.stall_timeout_ms`, `pi.auto_retry`, `pi.ui_request_policy`, and `pi.compatibility_probe`.

90. Update `README.md` with the workspace population contract. Explain that Scherzo creates directories but does not know how to check out project code; show the `REPO_URL` + `git clone` hook from `examples/WORKFLOW.md`; state that dispatchable workflows must include either `hooks.after_create` or an explicit `hooks.before_run` verification hook, that failed `after_create` on a newly-created workspace is cleaned up before retry or marked with a sidecar `.scherzo-state/<key>.populating` file if cleanup fails, that hooks are trusted shell, and show how to pause dispatch with `agent.max_concurrent_agents: 0` while preserving reconciliation.

91. Update `README.md` with the safety posture: Scherzo is intended for trusted repositories and trusted workflow files, hooks are arbitrary shell, pi tool execution follows the operator's pi command and OS environment, Scherzo enforces workspace cwd and root containment but does not provide a VM/container sandbox, pi extension UI dialogs are cancelled automatically, fire-and-forget UI requests are logged, unsupported blocking behavior fails the attempt, built-in Linear writes and distributed claiming are deferred, operators must run only one Scherzo instance per Linear project and workspace root, and retry/session caps park issues rather than spending tokens forever.

92. Update `README.md` with operational examples: run tests, start with default `WORKFLOW.md`, start with explicit workflow path, configure Linear credentials with `LINEAR_API_KEY`, configure `REPO_URL`, pause by reloading `agent.max_concurrent_agents: 0`, and stop the service with Ctrl-C or process manager termination.

93. Update `docs/plans/implement-scherzo.md` Progress, Surprises & Discoveries, Decision Log if implementation forced any changes, and Outcomes & Retrospective.

94. Run `direnv exec . gleam format --check src test`. Expect exit code 0.

95. Run `direnv exec . gleam test`. Expect exit code 0 and no failing tests.

96. Run `direnv exec . gleam run -- --help`. Expect exit code 0 and useful usage text.

97. Run a deterministic fake integration through the test suite if not already run in Step 85. Expect evidence in test output or captured logs that fake pi observed the populated workspace marker and no real Linear or real pi process was used.

98. Optional real pi compatibility check: run the probe through an exposed test helper or through a harmless test issue whose workspace has already been populated, with `pi.compatibility_probe: true`. Expect the probe cwd to equal the issue workspace path, to send no prompt, and to consume no model tokens. If no real pi executable or safe test issue/workspace is available, record this check as skipped in Outcomes & Retrospective.

99. Optional real Linear integration check: create a private Linear test project, export `LINEAR_API_KEY`, copy `examples/WORKFLOW.md` to `.scherzo/WORKFLOW.real.md`, set `tracker.project_slug` to the test project slug, set `agent.max_concurrent_agents: 0` first to validate startup/polling without dispatch, and set `REPO_URL` to a safe test repository. Run `direnv exec . gleam run -- .scherzo/WORKFLOW.real.md`. Expect structured logs for startup validation, terminal cleanup attempt, immediate tick, candidate fetch, paused dispatch, and next tick scheduling; no pi probe should run while dispatch is paused or no issue workspace is prepared. If credentials or a test project are unavailable, record this check as skipped and state that Linear compatibility was validated only against fake responses.

100. If the optional real Linear check passes in paused mode and a harmless test issue is available, set `agent.max_concurrent_agents: 1` and `pi.command` to the fake pi fixture copied into the workspace or to a harmless local RPC-compatible fake command. Expect one dispatched test issue, populated workspace logs, fake pi session logs, and no real model tokens. Record sanitized evidence in Outcomes & Retrospective.

101. Commit milestone 10 with message `Document and validate Scherzo implementation` after all required validation passes.

## Testing and Falsifiability

Every new behavior in this plan must have deterministic tests. The primary test command is run from the repository root:

    direnv exec . gleam test

The formatting check is:

    direnv exec . gleam format --check src test

Dependency smoke tests in `test/dependency_smoke_test.gleam` must fail early if a chosen package cannot provide the API this plan expects. They must exercise the actual imported modules for JSON decoding, YAML parsing, HTTP request construction or client import, file operations, time values, and OTP actor/timer support. If these tests fail because a package is incompatible, the implementation must change the dependency or design before adding higher-level code and record the decision before moving to scheduler or protocol work.

Port tests in `test/port_test.gleam` are a feasibility gate. They must fail if Scherzo cannot launch `bash -lc <command>` with the exact requested cwd, cannot send stdin lines, cannot read LF-delimited stdout without consuming stderr as JSON, cannot handle overlong lines safely, or cannot terminate a child process. The rest of the implementation should not proceed until these tests pass or the plan is revised.

Workflow tests in `test/workflow_test.gleam` must prove missing files, invalid YAML, non-map front matter, no front matter, missing closing delimiters, explicit path selection, and prompt trimming. Config tests in `test/config_test.gleam` must prove every default and every validation rule, including `max_concurrent_agents = 0` as a valid pause, negative concurrency as invalid, `max_retry_attempts`, `max_sessions_per_issue`, `pi.compatibility_probe`, rejecting dispatch configuration with both `hooks.after_create` and `hooks.before_run` absent or blank, accepting an explicit `before_run` verification hook for pre-populated workspaces, and the critical behavior that invalid reloads preserve the last known good config for reconciliation while marking current dispatch validation invalid. Reload tests must also prove newly resolved secrets from a valid reload are returned for logger registration before reload-derived logs can be emitted. These tests falsify the claim that Scherzo can be safely reconfigured without restart.

Template tests in `test/template_test.gleam` must prove strict rendering. A test that renders `Hello {{ issue.unknown }}` must return `TemplateRenderError`; if it silently renders an empty string, the implementation fails the plan. A test that renders `{{ issue.title | upcase }}` must fail as an unknown filter until filters are deliberately implemented. First-run `{{ attempt }}` must render as an empty string and become truthy only when an integer retry or continuation attempt is provided.

Workspace tests in `test/workspace_test.gleam` must prove path safety and population behavior. Inputs `../outside`, `.`, and `..` must not create directories outside the workspace root. A test must create a sentinel directory next to the root and assert it remains untouched. A hook test must prove `after_create` can create a marker file in the workspace and that pi is not launched until `before_run` has succeeded. Another hook test must make `after_create` fail after writing a partial file and assert the newly-created workspace directory is best-effort removed or, if removal itself fails, that the sidecar `.scherzo-state/<key>.populating` marker remains and the partial directory is not treated as prepared on the next attempt. If any malicious identifier can make Scherzo create, remove, or launch pi outside the workspace root, the safety claim is false.

Linear tests in `test/linear_test.gleam` must not call the network. They must inspect the outgoing GraphQL body and fake responses. The test named for state refresh must assert the query contains `[ID!]`. The candidate query test must assert the project filter uses `slugId`. Pagination tests must fail if page order changes or if missing cursors are ignored. Redaction tests must fail if authorization values appear in logs or errors.

Orchestrator tests in `test/orchestrator_core_test.gleam` and `test/orchestrator_service_test.gleam` falsify scheduling claims. They must fail if duplicate claimed issues dispatch, if global or per-state limits are exceeded, if paused dispatch still launches a worker, if invalid current workflow status still launches a worker, if Todo blockers are ignored, if normal worker exits with active final state do not schedule a 1000 ms continuation retry below cap, if normal active exits do not park at `max_sessions_per_issue`, if normal worker exits with terminal final state do not clean the stored workspace path and release the claim, if abnormal retry backoff does not cap, if retry scheduling fails to cancel and replace an old timer, if retry candidate-fetch failure releases the claim instead of requeueing, if stale retry timer firings can dispatch, if retry attempts do not park at `max_retry_attempts`, if parked issues dispatch before `updated_at` changes, if terminal states do not request cleanup of the stored workspace path, if invalid workflow reloads crash the service, if newly resolved secrets from valid reloads are not registered before later logs, if logger sink failures crash the service, or if shutdown leaves fake workers or fake pi processes alive.

Pi RPC tests in `test/pi_rpc_test.gleam` use `test/fixtures/fake_pi_rpc.sh`. They must fail if stdout and stderr are mixed, if command responses are not correlated by id, if `agent_end` is not recognized, if `get_session_stats` token totals are not decoded, if extension UI dialogs can stall indefinitely, if malformed JSON is ignored, if the compatibility probe sends a prompt, if the probe is launched before a workspace exists, or if pi launches with the wrong cwd.

Agent runner tests in `test/agent_runner_test.gleam` falsify prompt, probe, and continuation behavior. The first turn must receive the rendered issue prompt only after workspace preparation, `before_run`, and any enabled no-prompt probe have succeeded in that workspace. A later in-worker continuation turn must receive concise continuation guidance and must not resend the full original task prompt. The runner must stop at `agent.max_turns`, preserve the same fake pi process only for those in-worker turns, return final issue state in `WorkerSuccess`, run `after_run` in success and failure paths once the workspace exists, treat prompt render errors as attempt failures, treat probe failures as pre-prompt attempt failures, and surface state-refresh errors as abnormal failures. Tests must distinguish this in-worker limit from orchestrator-level fresh-session continuation controlled by `agent.max_sessions_per_issue`.

The deterministic service integration test in `test/service_integration_test.gleam` must run without real Linear, real pi, or network access. It must prove startup validation without launching pi, candidate dispatch, workspace population marker creation, fake pi compatibility probing from the prepared workspace before any prompt, fake pi execution, worker exit handling, and shutdown through the same service wiring used by the CLI except for injected dependencies.

CLI tests in `test/main_test.gleam` must fail if the default `WORKFLOW.md` is missing, if an explicit workflow path does not exist, if too many arguments are accepted, if `--help` starts the service, or if startup failures leak configured secret values. These tests cover the host lifecycle requirements in `docs/SYMPHONY_SPEC.md` Section 17.7.

The real integration profile is recommended but not required for CI. It requires valid Linear credentials, a test project, a safe workspace root, and either a real pi install for a workspace-scoped compatibility probe or a fake RPC-compatible pi command for dispatch. A skipped real integration must be recorded as skipped. If a release process explicitly enables real integration, failure must fail that release validation.

## Validation and Acceptance

Core acceptance is met when these commands, run from the repository root, all succeed:

    direnv exec . gleam format --check src test
    direnv exec . gleam test
    direnv exec . gleam run -- --help

The expected result is exit code 0 for all three commands. The help output must mention the default workflow path `WORKFLOW.md`, the explicit workflow path argument, the need for Linear credentials, the need for a workspace population or verification hook such as `REPO_URL` plus `git clone` in `hooks.after_create` or `test -d .git` in `hooks.before_run`, the pause mechanism `agent.max_concurrent_agents: 0`, that pi is launched through RPC mode, and that operators must run only one Scherzo instance per Linear project and workspace root.

Deterministic behavioral acceptance is met by `test/service_integration_test.gleam`. That test must prove the service can load a workflow without launching pi at startup, fetch fake Linear candidates, create a per-issue workspace, run a population hook that writes a marker file, run the fake pi compatibility probe from that same workspace cwd without sending a prompt, launch fake pi with cwd equal to that workspace, observe fake pi reading the marker, process `agent_end`, update orchestrator state, and shut down without leaked child processes.

Operator behavioral acceptance is met when an operator can create a `WORKFLOW.md` like `examples/WORKFLOW.md`, export `LINEAR_API_KEY`, export `REPO_URL` or provide an equivalent population or verification hook, run `direnv exec . gleam run -- ./WORKFLOW.md`, and observe structured logs similar to these lines, with exact timestamps and ids differing:

    level=info service=scherzo event=startup workflow_path=WORKFLOW.md
    level=info service=scherzo event=workflow_loaded polling_interval_ms=30000 max_concurrent_agents=1
    level=info service=scherzo event=tick_started
    level=info service=scherzo event=candidates_fetched count=1
    level=info service=scherzo event=workspace_prepared issue_id=... issue_identifier=ABC-123 workspace_path=... populated=true
    level=info service=scherzo event=pi_probe_succeeded issue_id=... issue_identifier=ABC-123 workspace_path=...
    level=info service=scherzo event=dispatch_started issue_id=... issue_identifier=ABC-123 workspace_path=...
    level=info service=scherzo event=pi_session_started issue_id=... issue_identifier=ABC-123 session_id=...
    level=info service=scherzo event=worker_exited issue_id=... issue_identifier=ABC-123 reason=normal

If no eligible Linear issues exist, acceptance is still met when the service logs startup, validation, a poll tick, `candidates_fetched count=0`, schedules the next tick, and stays alive without launching pi. If Linear credentials are missing, acceptance is met when startup fails nonzero with a redacted error code such as `missing_tracker_api_key` and does not print the secret value. If `agent.max_concurrent_agents: 0`, acceptance is met when reconciliation still runs but no new workers or pi probes are dispatched and logs include a paused-dispatch event. If a reload makes the current workflow unreadable or invalid, acceptance is met when reconciliation continues using the last known good config, new dispatch is blocked, an operator-visible reload error is logged, and a later valid reload resumes dispatch.

Safety acceptance is met when tests prove pi cwd is the per-issue workspace for both probes and prompted sessions and never the repository root, workspace paths are under the normalized root, dispatch is rejected unless a population or verification hook is configured, failed `after_create` on a newly-created workspace either removes the partial workspace or leaves a sidecar marker that blocks reuse, hooks run with workspace cwd, terminal cleanup deletes only the intended workspace, worker-observed terminal final states trigger cleanup of the stored workspace path, and changing `workspace.root` during an in-flight run does not make Scherzo clean the wrong path. Recovery acceptance is met when tests prove failed workers retry with exponential backoff until `max_retry_attempts`, replacement retries cancel old timers, retry-poll tracker failures requeue without releasing claims, normal active exits retry until `max_sessions_per_issue`, capped issues are parked, updated issues are unparked, graceful shutdown kills workers and timers, and process restart does not require any persistent database to resume polling active issues.

Real integration acceptance is optional for development but must be explicit in the retrospective. If skipped, Outcomes & Retrospective must say that Linear API compatibility was validated only against fake responses. If run, record sanitized evidence such as candidate count, successful startup terminal cleanup attempt, paused dispatch behavior, and, when dispatch is enabled for a safe test issue, fake or real workspace-scoped pi probe result without storing tokens, issue contents, or private repository URLs.

## Rollout, Recovery, and Idempotence

This is a new service in an empty repository, so rollout is additive. There is no migration of existing code or stored data. Each milestone can be reverted by reverting its commit. If the service is started accidentally, stop it with Ctrl-C or process manager termination, or reload `WORKFLOW.md` with `agent.max_concurrent_agents: 0` to pause new dispatch while keeping reconciliation active. Because scheduler state is in memory, stopping the process cancels active retry timers, parked issue state, counters, and live workers. On the next start, Scherzo performs terminal workspace cleanup and polls Linear again. Until a future durable claim backend or built-in Linear write path exists, rollout instructions must tell operators to run only one Scherzo instance per Linear project and workspace root.

Workspace operations are designed to be idempotent. Creating a workspace for the same issue reuses the existing directory. `after_create` runs only when the directory is newly created. If that initial `after_create` fails, Scherzo best-effort removes the newly-created directory before returning the hook error; if removal fails, the sidecar `.scherzo-state/<key>.populating` marker remains so the next attempt cleans and repopulates or fails safely rather than reusing partial files. Successful runs do not auto-delete workspaces. Terminal cleanup can be repeated; if the workspace is already absent, cleanup is a no-op after logging. In-flight workers keep their exact workspace path in runtime state, so cleanup for those workers uses the stored path even if a valid workflow reload changes `workspace.root`.

Hooks are trusted workflow configuration and may have arbitrary side effects inside the workspace. A dispatchable workflow must include either `after_create` to populate a new workspace or `before_run` to verify an intentionally pre-populated workspace; if neither hook is present, fix `WORKFLOW.md` before expecting dispatch. If a hook is unsafe, fix `WORKFLOW.md`; Scherzo reloads valid changes without restart. If an edit makes `WORKFLOW.md` unreadable or invalid, Scherzo keeps reconciliation for existing workers on the last known good config but blocks all new dispatch until the workflow is valid again. If a valid reload changes `workspace.root`, future dispatches and startup cleanup use the new root, but in-flight workers are not moved. If a valid reload changes a secret, Scherzo registers the newly resolved value with the logger before emitting reload-derived summaries or errors.

If pi integration fails in production, set `agent.max_concurrent_agents: 0` in `WORKFLOW.md` and let Scherzo reload, or stop the service. Then fix `pi.command`, `pi.compatibility_probe`, credentials, or the pi installation before resuming dispatch. If Linear is down, Scherzo logs tracker errors, skips dispatch for that tick, keeps running workers during state-refresh failures, and tries again on the next tick.

If an issue repeatedly fails or remains active after normal sessions, Scherzo parks it in memory after the configured cap. Parking is a safety valve, not a tracker write. Operators should inspect logs, fix the issue, update the Linear ticket, or restart the service if they intentionally want to clear in-memory parked state. When Linear reports the issue with a newer `updated_at`, Scherzo un-parks it and resets the relevant counters.

## Artifacts and Notes

The adapted `WORKFLOW.md` front matter schema is summarized here for implementers. Unknown top-level keys are ignored.

    tracker:
      kind: linear
      endpoint: https://api.linear.app/graphql
      api_key: "$LINEAR_API_KEY"
      project_slug: YOUR_PROJECT_SLUG
      active_states: [Todo, In Progress]
      terminal_states: [Closed, Cancelled, Canceled, Duplicate, Done]
    polling:
      interval_ms: 30000
    workspace:
      root: .scherzo/workspaces
    hooks:
      after_create: |
        test -n "$REPO_URL"
        git clone "$REPO_URL" .
        test -d .git
      before_run: |
        test -d .git
        git status --short
      after_run: |
        echo done
      before_remove: |
        echo removing
      timeout_ms: 60000
    agent:
      max_concurrent_agents: 10
      max_turns: 20
      max_retry_backoff_ms: 300000
      max_retry_attempts: 5
      max_sessions_per_issue: 3
      max_concurrent_agents_by_state:
        todo: 2
    pi:
      command: "pi --mode rpc --no-session"
      turn_timeout_ms: 3600000
      read_timeout_ms: 5000
      stall_timeout_ms: 300000
      auto_retry: true
      ui_request_policy: cancel
      compatibility_probe: true

Dispatch validation requires either a non-empty `hooks.after_create` or a non-empty `hooks.before_run`. Use `after_create` to populate new workspaces, as shown above. Use `before_run` alone only when workspaces are intentionally pre-populated, and make it a real verification command such as `test -d .git`, not a silent no-op. If initial population fails and cleanup cannot remove the partial directory, Scherzo records a sidecar `.scherzo-state/<sanitized-key>.populating` marker under the workspace root and must not launch pi from that workspace until cleanup and population have succeeded. Set `agent.max_concurrent_agents: 0` to pause new dispatch while leaving startup validation, reload, and reconciliation active. Paused dispatch also means no pi compatibility probe runs, because probes are tied to prepared issue workspaces. Set `pi.compatibility_probe: false` only for tests or emergencies where launching pi for a no-prompt probe is known to be impossible but dispatch is still intentionally allowed. Run only one Scherzo process per Linear project and workspace root until durable claiming or built-in Linear writes are added.

The adapted prompt template receives this input shape:

    issue.id
    issue.identifier
    issue.title
    issue.description
    issue.priority
    issue.state
    issue.branch_name
    issue.url
    issue.labels
    issue.blocked_by
    issue.created_at
    issue.updated_at
    attempt

The first run receives `attempt = Nil`; interpolation renders it as an empty string and conditionals treat it as false. Retry and continuation attempts receive an integer.

The pi RPC compatibility probe flow runs after workspace preparation and `before_run` success, with pi launched using cwd equal to the issue workspace path, and before Scherzo sends any issue prompt:

    Scherzo -> pi: {"id":"probe-1","type":"set_session_name","name":"scherzo compatibility probe"}
    pi -> Scherzo: {"id":"probe-1","type":"response","command":"set_session_name","success":true}
    Scherzo -> pi: {"id":"probe-2","type":"get_state"}
    pi -> Scherzo: {"id":"probe-2","type":"response","command":"get_state","success":true,"data":{"sessionId":"...","isStreaming":false}}
    Scherzo terminates the probe process without sending `prompt`, then starts a fresh pi RPC process for the actual prompted worker turn in the same workspace.

The pi RPC minimum command/event flow for one worker turn is:

    Scherzo -> pi: {"id":"1","type":"set_session_name","name":"ABC-123: Fix tests"}
    pi -> Scherzo: {"id":"1","type":"response","command":"set_session_name","success":true}
    Scherzo -> pi: {"id":"2","type":"set_auto_retry","enabled":true}
    pi -> Scherzo: {"id":"2","type":"response","command":"set_auto_retry","success":true}
    Scherzo -> pi: {"id":"3","type":"get_state"}
    pi -> Scherzo: {"id":"3","type":"response","command":"get_state","success":true,"data":{"sessionId":"..."}}
    Scherzo -> pi: {"id":"4","type":"prompt","message":"...rendered prompt..."}
    pi -> Scherzo: {"id":"4","type":"response","command":"prompt","success":true}
    pi -> Scherzo: {"type":"agent_start"}
    pi -> Scherzo: {"type":"turn_start"}
    pi -> Scherzo: {"type":"message_update", ...}
    pi -> Scherzo: {"type":"turn_end", ...}
    pi -> Scherzo: {"type":"agent_end", "messages":[...]}
    Scherzo -> pi: {"id":"5","type":"get_session_stats"}
    pi -> Scherzo: {"id":"5","type":"response","command":"get_session_stats","success":true,"data":{"tokens":{"input":1,"output":2,"total":3}}}

The Linear candidate query must include these semantic pieces, though field order may differ:

    query CandidateIssues($projectSlug: String!, $activeStates: [String!], $after: String) {
      issues(
        first: 50,
        after: $after,
        filter: {
          project: { slugId: { eq: $projectSlug } },
          state: { name: { in: $activeStates } }
        }
      ) { ... }
    }

The state refresh query must use GraphQL ID typing:

    query IssueStates($ids: [ID!]!) { ... }

## Interfaces and Dependencies

Use these Gleam dependencies unless implementation discovers an incompatibility and records the change in the Decision Log: `gleam_erlang` for Erlang interop and process utilities, `gleam_otp` for actors and timers, `gleam_json` for RPC and GraphQL JSON, `gleam_httpc` for Linear HTTP calls, `simplifile` for file operations where it fits, `yay` for YAML parsing, `birl` for current time, durations, and ISO timestamp handling, and `gleeunit` for tests. The dependency smoke tests are the authoritative early check that these packages are usable as expected; they must be run and passing before implementing workflow/config, Linear, or pi RPC modules.

In `src/scherzo/domain.gleam`, define the normalized issue and config types. The exact Gleam syntax can differ, but these public concepts must exist:

    pub type Issue {
      Issue(
        id: String,
        identifier: String,
        title: String,
        description: Option(String),
        priority: Option(Int),
        state: String,
        branch_name: Option(String),
        url: Option(String),
        labels: List(String),
        blocked_by: List(BlockerRef),
        created_at: Option(Time),
        updated_at: Option(Time),
      )
    }

    pub type AgentConfig {
      AgentConfig(
        max_concurrent_agents: Int,
        max_turns: Int,
        max_retry_backoff_ms: Int,
        max_retry_attempts: Int,
        max_sessions_per_issue: Int,
        max_concurrent_agents_by_state: Dict(String, Int),
      )
    }

    pub type PiConfig {
      PiConfig(
        command: String,
        turn_timeout_ms: Int,
        read_timeout_ms: Int,
        stall_timeout_ms: Int,
        auto_retry: Bool,
        ui_request_policy: UiRequestPolicy,
        compatibility_probe: Bool,
      )
    }

    pub type EffectiveConfig {
      EffectiveConfig(
        tracker: TrackerConfig,
        polling: PollingConfig,
        workspace: WorkspaceConfig,
        hooks: HooksConfig,
        agent: AgentConfig,
        pi: PiConfig,
      )
    }

    pub type IssueCounter {
      IssueCounter(
        failure_attempts: Int,
        worker_sessions: Int,
        observed_updated_at: Option(Time),
      )
    }

    pub type ParkedEntry {
      ParkedEntry(
        issue_id: String,
        identifier: String,
        reason: String,
        observed_updated_at: Option(Time),
        parked_at_ms: Int,
      )
    }

In `src/scherzo/tracker.gleam`, define a testable tracker client shape:

    pub type Client {
      Client(
        fetch_candidate_issues: fn() -> Result(List(Issue), TrackerError),
        fetch_issues_by_states: fn(List(String)) -> Result(List(Issue), TrackerError),
        fetch_issue_states_by_ids: fn(List(String)) -> Result(List(Issue), TrackerError),
      )
    }

In `src/scherzo/port.gleam`, define the subprocess wrapper used by hooks and pi:

    pub fn start(command: String, cwd: String) -> Result(Process, PortError)
    pub fn send_line(process: Process, line: String) -> Result(Nil, PortError)
    pub fn read_stdout_line(process: Process, timeout_ms: Int) -> Result(String, PortError)
    pub fn read_diagnostics(process: Process) -> Result(String, PortError)
    pub fn terminate(process: Process) -> Result(Nil, PortError)
    pub fn await_exit(process: Process, timeout_ms: Int) -> Result(Int, PortError)

In `src/scherzo/agent/probe.gleam`, define:

    pub fn probe(
      command: String,
      cwd: String,
      read_timeout_ms: Int,
    ) -> Result(Nil, PiProbeError)

The probe must not send a prompt, and every caller must pass a cwd equal to a prepared per-issue workspace path under the normalized workspace root. Startup code must not call this function before an issue workspace exists.

In `src/scherzo/agent/runner.gleam`, define the runner entry point used by workers:

    pub fn run_attempt(
      issue: Issue,
      attempt: Option(Int),
      workflow: WorkflowDefinition,
      config: EffectiveConfig,
      tracker: tracker.Client,
      emit_update: fn(String, PiUpdate) -> Nil,
    ) -> Result(WorkerSuccess, WorkerFailure)

`WorkerSuccess` must include the final refreshed issue snapshot when available, or an explicit final state classification of active, terminal, or non-active, plus the stored workspace path used for the run. The orchestrator uses this data to decide between continuation retry, release without cleanup, and terminal workspace cleanup without waiting for a later reconciliation tick.

In `src/scherzo/orchestrator/core.gleam`, define pure transition functions. They should accept the current state and explicit inputs such as `now_ms`, refreshed issues, and worker results rather than reading clocks or performing I/O internally. Public functions must cover candidate eligibility, applying worker start, applying worker exit, retry timer handling, parking, un-parking, reconciliation, stall detection, and token accounting.

In `src/scherzo/orchestrator/service.gleam`, define the runtime start functions:

    pub fn start(workflow_path: Option(String)) -> Result(Nil, StartupError)

    pub fn start_with_dependencies(
      workflow_path: Option(String),
      dependencies: Dependencies,
    ) -> Result(Nil, StartupError)

The default `start` uses real Linear, real filesystem, real port/pi, and real timers. `start_with_dependencies` is for deterministic tests and must not require real Linear credentials, a real pi install, or network access when fake dependencies are supplied. The function may block until shutdown because this is a long-running service. If a future implementation wants supervised application packaging, add it after the CLI behavior and tests in this plan are already passing.

## Revision Note

This revision closes the spec-coverage gaps found during review of `docs/plans/implement-scherzo.md` against `docs/SYMPHONY_SPEC.md`. It adds an explicit Symphony coverage matrix, changes invalid reloads to block new dispatch while preserving last-known-good reconciliation, confines pi compatibility probes to prepared issue workspaces, specifies retry timer replacement and retry-poll failure handling, requires cleanup when workers observe terminal issue states, documents logger-sink resilience and CLI negative-path tests, and records the rationale in the Decision Log.

The follow-up readiness revision fills the milestone 1 concrete-step gap with dependency declaration, dependency smoke tests, initial README, and example workflow work; requires dispatchable workflows to provide either `after_create` population or `before_run` verification; cleans newly-created workspaces after failed population; clarifies `max_turns` versus `max_sessions_per_issue`; registers new secrets on valid reloads; documents the one-instance-per-project/root operating constraint; and adds early checks for actual Linear and pi protocol assumptions.
