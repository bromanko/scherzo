/// Config generator - generates TOML configuration with explicit gate definitions
///
/// Creates configuration file content with all gates explicitly defined.
/// Templates expand to full gate specifications - no hidden abstractions.
import gleam/int
import gleam/list
import gleam/string
import scherzo/config/types.{
  type GateConfig, type ReviewDimension, type ReviewPass, CommandGate, HumanGate,
  MultiPassReviewGate, ParallelReviewGate,
}

/// Available template names
pub fn available_templates() -> List(String) {
  ["none", "code-review", "security-audit", "quick-review"]
}

/// Get description for a template
pub fn template_description(name: String) -> String {
  case name {
    "none" -> "No gates configured"
    "code-review" -> "Tests + type check + 7-dimension parallel code review"
    "security-audit" -> "Tests + multi-pass security review"
    "quick-review" -> "Fast feedback with just tests and type check"
    _ -> "Unknown template"
  }
}

/// Generate a complete TOML configuration for the given templates
pub fn generate_config(templates: List(String)) -> String {
  let gates = templates_to_gates(templates)
  let retry = default_retry_section(templates)

  "# Scherzo Configuration
# https://github.com/anthropics/scherzo

" <> gates_section(gates) <> "\n" <> retry
}

/// Convert template names to explicit gate list
fn templates_to_gates(templates: List(String)) -> List(GateConfig) {
  templates
  |> list.flat_map(template_gates)
}

/// Get the gates for a specific template
fn template_gates(name: String) -> List(GateConfig) {
  case name {
    "none" -> []
    "code-review" -> code_review_gates()
    "security-audit" -> security_audit_gates()
    "quick-review" -> quick_review_gates()
    _ -> []
  }
}

/// Generate the [gates] section with all gate definitions
fn gates_section(gates: List(GateConfig)) -> String {
  case gates {
    [] ->
      "[gates]
# No gates configured - agent output goes directly to completion.
# Add gates to validate agent work before marking tasks complete.
#
# Example command gate:
# [[gates.gates]]
# type = \"command\"
# name = \"tests\"
# command = \"gleam test\"
# timeout_ms = 300000
# serial = true
"
    _ ->
      "[gates]\n"
      <> {
        gates
        |> list.map(gate_to_toml)
        |> string.join("\n")
      }
  }
}

/// Convert a single gate to TOML format
fn gate_to_toml(gate: GateConfig) -> String {
  case gate {
    CommandGate(name, command, timeout_ms, serial, fail_fast) ->
      command_gate_toml(name, command, timeout_ms, serial, fail_fast)
    ParallelReviewGate(name, dimensions, synthesis_prompt) ->
      parallel_review_toml(name, dimensions, synthesis_prompt)
    MultiPassReviewGate(name, passes, require_convergence, max_passes) ->
      multipass_review_toml(name, passes, require_convergence, max_passes)
    HumanGate(name, prompt) -> human_gate_toml(name, prompt)
  }
}

/// Generate TOML for a command gate
fn command_gate_toml(
  name: String,
  command: String,
  timeout_ms: Int,
  serial: Bool,
  fail_fast: Bool,
) -> String {
  "
[[gates.gates]]
type = \"command\"
name = \"" <> name <> "\"
command = \"" <> escape_toml_string(command) <> "\"
timeout_ms = " <> int.to_string(timeout_ms) <> "
serial = " <> bool_to_toml(serial) <> "
fail_fast = " <> bool_to_toml(fail_fast)
}

/// Generate TOML for a parallel review gate
fn parallel_review_toml(
  name: String,
  dimensions: List(ReviewDimension),
  synthesis_prompt: String,
) -> String {
  let dims_toml =
    dimensions
    |> list.map(dimension_to_toml)
    |> string.join("\n")

  "
[[gates.gates]]
type = \"parallel-review\"
name = \"" <> name <> "\"
synthesis_prompt = \"\"\"
" <> synthesis_prompt <> "
\"\"\"

" <> dims_toml
}

/// Generate TOML for a review dimension
fn dimension_to_toml(dim: ReviewDimension) -> String {
  "[[gates.gates.dimensions]]
id = \"" <> dim.id <> "\"
focus = \"" <> escape_toml_string(dim.focus) <> "\"
prompt = \"\"\"
" <> dim.prompt <> "
\"\"\""
}

/// Generate TOML for a multi-pass review gate
fn multipass_review_toml(
  name: String,
  passes: List(ReviewPass),
  require_convergence: Bool,
  max_passes: Int,
) -> String {
  let passes_toml =
    passes
    |> list.map(pass_to_toml)
    |> string.join("\n")

  "
[[gates.gates]]
type = \"multi-pass-review\"
name = \"" <> name <> "\"
require_convergence = " <> bool_to_toml(require_convergence) <> "
max_passes = " <> int.to_string(max_passes) <> "

" <> passes_toml
}

/// Generate TOML for a review pass
fn pass_to_toml(pass: ReviewPass) -> String {
  "[[gates.gates.passes]]
focus = \"" <> escape_toml_string(pass.focus) <> "\"
prompt = \"\"\"
" <> pass.prompt <> "
\"\"\""
}

/// Generate TOML for a human gate
fn human_gate_toml(name: String, prompt: String) -> String {
  "
[[gates.gates]]
type = \"human\"
name = \"" <> name <> "\"
prompt = \"\"\"
" <> prompt <> "
\"\"\""
}

/// Generate default retry section
fn default_retry_section(templates: List(String)) -> String {
  // quick-review uses different retry settings
  case list.contains(templates, "quick-review") {
    True ->
      "[retry]
strategy = \"same\"
fresh_after_failures = 3
max_iterations = 2
"
    False ->
      "[retry]
strategy = \"auto\"
fresh_after_failures = 2
max_iterations = 3
"
  }
}

/// Escape special characters for TOML basic strings
fn escape_toml_string(s: String) -> String {
  s
  |> string.replace("\\", "\\\\")
  |> string.replace("\"", "\\\"")
  |> string.replace("\n", "\\n")
  |> string.replace("\t", "\\t")
  |> string.replace("\r", "\\r")
}

/// Convert bool to TOML
fn bool_to_toml(b: Bool) -> String {
  case b {
    True -> "true"
    False -> "false"
  }
}

// =============================================================================
// Template Gate Definitions
// =============================================================================

/// Gates for code-review template
fn code_review_gates() -> List(GateConfig) {
  [
    CommandGate(
      name: "tests",
      command: "gleam test",
      timeout_ms: 300_000,
      serial: True,
      fail_fast: False,
    ),
    CommandGate(
      name: "check",
      command: "gleam check",
      timeout_ms: 60_000,
      serial: True,
      fail_fast: False,
    ),
    ParallelReviewGate(
      name: "code-review",
      dimensions: [
        types.ReviewDimension(
          id: "correctness",
          focus: "Functional correctness and bug detection",
          prompt: "Review the code changes for correctness. Look for:
- Logic errors and edge cases
- Off-by-one errors
- Null/None handling
- Race conditions
- Resource leaks",
        ),
        types.ReviewDimension(
          id: "security",
          focus: "Security vulnerabilities",
          prompt: "Review the code changes for security issues. Look for:
- Input validation gaps
- Injection vulnerabilities
- Authentication/authorization issues
- Sensitive data exposure
- Cryptographic weaknesses",
        ),
        types.ReviewDimension(
          id: "performance",
          focus: "Performance and efficiency",
          prompt: "Review the code changes for performance. Look for:
- Algorithmic complexity issues
- Unnecessary allocations
- N+1 query patterns
- Missing caching opportunities
- Blocking operations",
        ),
        types.ReviewDimension(
          id: "maintainability",
          focus: "Code clarity and maintainability",
          prompt: "Review the code changes for maintainability. Look for:
- Complex or unclear logic
- Poor naming
- Missing or misleading comments
- Code duplication
- Overly long functions",
        ),
        types.ReviewDimension(
          id: "testing",
          focus: "Test coverage and quality",
          prompt: "Review the test changes (if any). Look for:
- Missing test cases for new functionality
- Edge cases not covered
- Brittle tests
- Unclear test names
- Missing assertions",
        ),
        types.ReviewDimension(
          id: "api-design",
          focus: "API design and contracts",
          prompt: "Review any API changes. Look for:
- Breaking changes
- Unclear contracts
- Poor error handling
- Missing documentation
- Inconsistent naming",
        ),
        types.ReviewDimension(
          id: "architecture",
          focus: "Architectural concerns",
          prompt: "Review for architectural issues. Look for:
- Inappropriate coupling
- Layer violations
- Missing abstractions
- Scalability concerns
- Configuration management",
        ),
      ],
      synthesis_prompt: "Synthesize the dimension reviews into a unified assessment. Prioritize findings by severity (P0 critical, P1 major, P2 minor, P3 suggestion). Focus on actionable feedback.",
    ),
  ]
}

/// Gates for security-audit template
fn security_audit_gates() -> List(GateConfig) {
  [
    CommandGate(
      name: "tests",
      command: "gleam test",
      timeout_ms: 300_000,
      serial: True,
      fail_fast: False,
    ),
    MultiPassReviewGate(
      name: "security-audit",
      passes: [
        types.ReviewPass(
          focus: "Input Validation",
          prompt: "Review all external inputs (user data, API calls, file reads) for proper validation. Check for injection vulnerabilities, buffer overflows, and type confusion.",
        ),
        types.ReviewPass(
          focus: "Authentication & Authorization",
          prompt: "Review authentication and authorization logic. Check for broken auth, privilege escalation, and missing access controls.",
        ),
        types.ReviewPass(
          focus: "Data Protection",
          prompt: "Review data handling for sensitive information. Check for exposure in logs, insecure storage, weak encryption, and data leakage.",
        ),
        types.ReviewPass(
          focus: "Dependencies & Configuration",
          prompt: "Review dependencies and configuration for security. Check for known vulnerabilities, insecure defaults, and exposed secrets.",
        ),
        types.ReviewPass(
          focus: "Attack Surface",
          prompt: "Final pass to identify any remaining attack surface. Consider the full threat model and potential abuse cases.",
        ),
      ],
      require_convergence: True,
      max_passes: 5,
    ),
  ]
}

/// Gates for quick-review template
fn quick_review_gates() -> List(GateConfig) {
  [
    CommandGate(
      name: "tests",
      command: "gleam test",
      timeout_ms: 120_000,
      serial: True,
      fail_fast: True,
    ),
    CommandGate(
      name: "check",
      command: "gleam check",
      timeout_ms: 30_000,
      serial: True,
      fail_fast: True,
    ),
  ]
}
