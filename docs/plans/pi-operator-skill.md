# Add a pi operator skill for Scherzo

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

After this change, an operator can open a normal pi session in the Scherzo repository and use a project skill to inspect and operate a running Scherzo daemon conversationally. The visible proof is that `/skill:scherzo-operator` loads a skill that teaches pi to use `scherzoctl` read-only commands first, summarize active sessions, inspect one session's recent events, and ask for confirmation before invoking mutating commands such as abort, prompt, pause, retry, park, unpark, or UI response.

This phase does not require a custom pi extension. It uses the programmatic interface from the previous control phases through `scherzoctl --json`, which is simpler, reviewable, and works in any pi session that can run shell commands in the repository.

## Problem Framing and Constraints

A human-friendly `scherzoctl attach` is useful, but the user also wants an agent UI: a pi session that can reason over running Scherzo workers, summarize what is happening, and perform operator actions when asked. A skill is the smallest way to provide that experience because pi already discovers project skills and can run repository commands. A custom pi extension with typed tools may be useful later, but it adds another integration surface before the CLI protocol has proven itself.

The skill must be safe. It can instruct an agent to run powerful commands. It must bias toward read-only inspection, require user confirmation before destructive actions, use JSON output for machine parsing, avoid exposing control tokens or API keys in summaries, and remind the agent that Scherzo's control API is local to the daemon host.

This plan assumes the local control API, pretty attach, and mutating-controls phases are complete. If mutating controls are not complete, the skill can still ship read-only instructions, but the plan should record that limitation and omit mutating command examples until they exist.

## Strategy Overview

Add a project-level pi skill under `.pi/skills/scherzo-operator/SKILL.md`. Pi's skill documentation says project skills are discovered from `.pi/skills/`, a skill directory contains a required `SKILL.md`, and skill frontmatter must include a lowercase hyphenated `name` matching the parent directory plus a `description`. The skill will instruct the model to use `scripts/scherzoctl` or `direnv exec . gleam run -- ctl` with `--json` for inspection and to ask before any mutation.

Add reference documentation under `.pi/skills/scherzo-operator/references/` that lists supported commands and response shapes. Keep the main `SKILL.md` concise so it loads cheaply, and put detailed command examples in the reference file.

Add a lightweight validation test that checks the skill's frontmatter, required safety phrases, command references, and that the skill name matches its directory. This does not prove agent behavior, but it prevents accidental breakage of the skill package.

## Alternatives Considered

One alternative is to build a custom pi extension that exposes typed Scherzo tools directly. That would be a richer agent UI, but it requires extension packaging and protocol work. The CLI/control API already provides a programmatic interface, so a skill using `scherzoctl --json` is the smaller first step.

Another alternative is to tell operators to prompt pi manually with ad hoc instructions. That is error-prone because the agent may use human table output instead of JSON, skip confirmation before destructive commands, or forget where to find the control file.

A third alternative is to embed the operator instructions only in README. That helps humans but does not make pi load the workflow on demand. A project skill is discoverable by pi and can be invoked explicitly with `/skill:scherzo-operator`.

## Risks and Countermeasures

The main safety risk is the operator agent taking destructive actions without explicit user intent. Countermeasure: the skill must require confirmation before `abort`, `prompt`, `pause`, `resume`, `retry`, `park`, `unpark`, `ui respond`, or any command that uses `--yes`. The validation test checks for those confirmation rules in the skill text.

The main privacy risk is the agent exposing control tokens, Linear API keys, repository URLs, or sensitive issue content in summaries. Countermeasure: instruct the skill to never print control token values, to prefer redacted summaries, and to avoid quoting large raw payloads unless the user asks.

The main reliability risk is the agent parsing pretty terminal output. Countermeasure: the skill must use `scherzoctl ... --json` for data gathering and reserve pretty attach for human display only.

The main environment risk is the pi session not knowing where the daemon control file is. Countermeasure: the skill instructs the agent to check `SCHERZO_CONTROL_FILE`, accept a user-supplied path, or ask the user for the `control_server_started` log line. It must not guess or scan broad filesystem locations.

The main scope risk is trying to implement a typed pi extension in this phase. Countermeasure: explicitly defer the extension until the CLI-based skill has been used and gaps are known.

## Progress

- [x] (2026-04-28 18:40Z) Read pi's `docs/skills.md` and confirmed project skills can live under `.pi/skills/<name>/SKILL.md` with required `name` and `description` frontmatter.
- [x] (2026-04-28 18:40Z) Confirmed the current repository has no existing `.pi/skills/` directory.
- [ ] Normalize the tree after control and mutating-control phases are complete.
- [ ] Add the `scherzo-operator` project skill and reference command guide.
- [ ] Add validation tests for skill frontmatter and safety instructions.
- [ ] Update README with instructions for using a pi session as the Scherzo operator UI.
- [ ] Run a manual dry-run with a fake daemon and record the transcript shape.

## Surprises & Discoveries

- Observation: Pi discovers project skills from `.pi/skills/` and directories containing `SKILL.md`; root `.md` files in `.pi/skills/` are also discovered, but a directory skill is better for bundled references.
  Evidence: Pi's `docs/skills.md` says `.pi/skills/` is a project skill location and shows the `my-skill/SKILL.md` structure.

## Decision Log

- Decision: Ship a project skill that uses `scherzoctl --json` instead of a custom pi extension.
  Rationale: The control API and CLI already provide a stable programmatic interface. A skill is smaller, reviewable, and avoids another protocol layer.
  Date: 2026-04-28

- Decision: Put detailed command examples in a reference file and keep `SKILL.md` focused on behavior and safety.
  Rationale: Pi skills use progressive disclosure. The main skill should fit in context, while details can be loaded only when needed.
  Date: 2026-04-28

- Decision: Require confirmation before every mutating command.
  Rationale: A pi operator session can take real actions. Confirmation reduces the chance of accidental aborts, prompts, or scheduler changes.
  Date: 2026-04-28

## Outcomes & Retrospective

(To be filled at completion. Include whether the skill was tested against a fake daemon, what commands worked, and any limitations that suggest a future custom pi extension.)

## Context and Orientation

Pi skills are self-contained capability packages. A project skill can live under `.pi/skills/<skill-name>/SKILL.md`. The `SKILL.md` frontmatter must include `name` and `description`. The name must be lowercase letters, numbers, and hyphens, and should match the parent directory. Pi scans skills at startup and exposes them as `/skill:<name>` commands when skill commands are enabled.

Scherzo's operator interface after prior phases is `scherzoctl`, available during development through `scripts/scherzoctl` or `direnv exec . gleam run -- ctl`. It can list sessions, fetch session details, fetch events, attach, pause/resume, abort, send prompts, respond to UI requests, retry, park, and unpark. The skill should prefer JSON commands such as `scripts/scherzoctl ps --json` and `scripts/scherzoctl events <session-id> --json` so the model receives structured data.

The daemon writes a control file containing host, port, and token. The skill should not reveal the token. It should use `SCHERZO_CONTROL_FILE` if set, or ask the user for the control file path if needed.

## Preconditions and Verified Facts

Before implementing this plan:

- `docs/plans/local-control-api-and-scherzoctl.md` is complete.
- `docs/plans/mutating-operator-controls.md` is complete, or this plan is narrowed to read-only commands and the Decision Log records that mutation examples are deferred.
- `scripts/scherzoctl ps --json` works against a running daemon when `SCHERZO_CONTROL_FILE` is set.
- `scripts/scherzoctl events <session-id> --json` returns structured event data.
- Mutating commands require authentication and destructive CLI commands require `--yes`.
- `direnv exec . gleam test` passes.

Current repository fact: there is no `.pi/skills/` directory at plan-authoring time.

## Scope Boundaries

In scope: project skill under `.pi/skills/scherzo-operator/`; reference guide with command examples; README instructions for starting pi and loading the skill; tests that validate skill frontmatter and safety requirements; manual dry-run instructions with a fake daemon.

Out of scope: custom pi extension; typed pi tool definitions; web dashboard; remote control; broad filesystem discovery of control files; automatic execution of destructive commands without confirmation; changing Scherzo control protocol.

## Milestones

Milestone 1 adds the skill skeleton and validates discovery metadata. At the end, `.pi/skills/scherzo-operator/SKILL.md` has valid frontmatter, clear trigger description, and concise operator behavior instructions.

Milestone 2 adds the command reference. At the end, the skill can point to `references/commands.md` for exact `scherzoctl` commands, JSON usage, and mutating safety rules.

Milestone 3 adds tests and documentation. At the end, `direnv exec . gleam test` validates the skill package and README shows how to use a pi session as an operator UI.

Milestone 4 performs a manual dry-run. At the end, a fake daemon session can be summarized by pi using the skill, and the plan records any gaps.

## Plan of Work

Create `.pi/skills/scherzo-operator/SKILL.md`. The frontmatter name is `scherzo-operator`. The description says the skill is for operating a running Scherzo daemon, inspecting sessions, summarizing worker progress, and using `scherzoctl` controls safely. The body should instruct the agent to use read-only commands first, prefer JSON output, summarize concisely, ask before mutations, never reveal tokens, and explain when it cannot proceed because no control file is available.

Create `.pi/skills/scherzo-operator/references/commands.md`. Include command examples for `ps`, `session`, `events`, `attach --json`, `pause`, `resume`, `reload`, `prompt`, `abort --yes`, `stop-after-turn`, `retry`, `park`, `unpark`, and `ui respond`. Include expected JSON response status names and guidance on interpreting `applied`, `queued`, `rejected`, `not_found`, and `not_allowed`.

Add `test/skill_docs_test.gleam`. The test reads `.pi/skills/scherzo-operator/SKILL.md`, verifies the frontmatter contains `name: scherzo-operator`, verifies the description is present and less than 1024 characters if practical, verifies the text mentions `scherzoctl`, `--json`, `SCHERZO_CONTROL_FILE`, and confirmation before mutating commands, and verifies it does not include placeholder text.

Update `README.md` with a section `Using pi as an operator UI`. Explain starting the daemon, setting `SCHERZO_CONTROL_FILE`, starting a separate pi session in the repository, and invoking `/skill:scherzo-operator` or asking pi to operate Scherzo.

## Concrete Steps

1. From the repository root, run `direnv exec . gleam test` and record the current pass count. Stop if control CLI tests fail.

2. Create directories `.pi/skills/scherzo-operator/` and `.pi/skills/scherzo-operator/references/`.

3. Write `.pi/skills/scherzo-operator/SKILL.md` with valid frontmatter:

       ---
       name: scherzo-operator
       description: Operate a running Scherzo daemon from pi by using scherzoctl to inspect sessions, summarize worker progress, and perform confirmed operator controls. Use when the user asks to inspect Scherzo, attach to workers, summarize sessions, pause/resume dispatch, abort workers, send follow-up prompts, retry, park/unpark, or answer Scherzo UI requests.
       ---

   Then add concise instructions for discovery, read-only-first behavior, JSON commands, confirmation, and secrecy.

4. Write `.pi/skills/scherzo-operator/references/commands.md` with command examples. Use repository-relative command examples such as `scripts/scherzoctl ps --json` and mention `direnv exec . gleam run -- ctl ...` as the fallback.

5. Create `test/skill_docs_test.gleam`. Add `scherzo_operator_skill_frontmatter_is_valid_test`, reading the skill file and asserting the name and description exist.

6. In the same test file, add `scherzo_operator_skill_requires_safe_operating_rules_test`, asserting the skill contains `--json`, `SCHERZO_CONTROL_FILE`, `ask`, `confirm`, `abort`, `pause`, `prompt`, and `never reveal` or equivalent token secrecy wording.

7. Add `scherzo_operator_reference_lists_supported_commands_test`, reading `references/commands.md` and asserting it contains examples for `ps`, `events`, `prompt`, `abort`, `ui respond`, `park`, and `unpark`.

8. Run `direnv exec . gleam test`. The new tests should pass after the files are created.

9. Update `README.md` with `Using pi as an operator UI`. Include these steps: start Scherzo daemon, copy or export the control file path, start pi in another terminal from the repository root, use `/skill:scherzo-operator`, ask for a summary, and confirm before any mutation.

10. If pi is available in the environment, run a manual validation with a fake daemon: start Scherzo with fake pi, start a separate pi session, invoke `/skill:scherzo-operator`, ask `summarize current Scherzo sessions`, and verify pi uses `scherzoctl ps --json` or `events --json` rather than parsing pretty output. If pi is not available, record the skipped validation in Outcomes.

11. Run `direnv exec . gleam format --check src test` and `direnv exec . gleam test`. Record final pass count in Progress.

12. Commit the phase with a message such as `Add Scherzo operator pi skill`.

## Testing and Falsifiability

The skill is falsified if pi cannot discover it because frontmatter is invalid, if the skill fails to mention `scherzoctl`, if it tells the agent to parse pretty output instead of JSON, if it omits confirmation for destructive commands, if it encourages revealing control tokens, or if the reference commands do not match the implemented `scherzoctl` interface.

Add deterministic tests in `test/skill_docs_test.gleam` as described above. These tests do not prove model behavior, but they make the skill package reviewable and prevent accidental removal of safety instructions.

Run from the repository root:

    direnv exec . gleam format --check src test
    direnv exec . gleam test

Manual validation with a real pi session is recommended but optional if pi is not available. If skipped, record that in Outcomes & Retrospective.

## Validation and Acceptance

Accept this phase when:

- `.pi/skills/scherzo-operator/SKILL.md` has valid skill frontmatter.
- `direnv exec . gleam test` passes including `test/skill_docs_test.gleam`.
- README explains how to use a separate pi session as the operator UI.
- A manual or documented dry-run shows the intended workflow:

      export SCHERZO_CONTROL_FILE=<path-from-daemon-log>
      pi
      /skill:scherzo-operator
      summarize current Scherzo sessions

- The skill uses read-only JSON commands before suggesting any mutating command.

## Rollout, Recovery, and Idempotence

The skill is additive. If it behaves poorly, operators can ignore it and continue using `scherzoctl` directly. Removing `.pi/skills/scherzo-operator/` disables the project skill without affecting Scherzo daemon behavior.

The skill should not store state. Each pi session reads the current control file and calls `scherzoctl` as needed. Re-running commands is safe for read-only operations. Mutating commands are not idempotent in general, so the skill must confirm intent and report command responses.

## Artifacts and Notes

Example skill workflow:

    User: /skill:scherzo-operator
    User: summarize current workers
    Agent: runs scripts/scherzoctl ps --json
    Agent: runs scripts/scherzoctl events <session> --json for active sessions
    Agent: summarizes progress and asks before any intervention

Future extension point: if CLI-based operation is too clumsy, a later plan can add a custom pi extension with typed tools equivalent to `scherzo_list_sessions`, `scherzo_get_events`, `scherzo_send_prompt`, and `scherzo_abort_session`. That is deliberately deferred here.

## Interfaces and Dependencies

The skill file path is:

    .pi/skills/scherzo-operator/SKILL.md

The reference file path is:

    .pi/skills/scherzo-operator/references/commands.md

The skill depends on the existing `scherzoctl` interface. It should prefer these command shapes:

    scripts/scherzoctl ps --json
    scripts/scherzoctl session <session-id> --json
    scripts/scherzoctl events <session-id> --json
    scripts/scherzoctl prompt <session-id> "message"
    scripts/scherzoctl abort <session-id> --yes
    scripts/scherzoctl ui respond <session-id> <request-id> --cancel

No new runtime package dependency is required.
