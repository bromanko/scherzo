/// Config loader - loads and merges gate configurations
///
/// Loads formula defaults and merges with user configuration.
/// User settings override formula defaults where they conflict.
import gleam/option.{type Option, None, Some}
import gleam/result
import scherzo/config/parser
import scherzo/config/types.{
  type GatesConfig, type RetryConfig, type ScherzoConfig, CommandGate,
  GatesConfig, MultiPassReviewGate, ParallelReviewGate, RetryConfig,
  ReviewDimension, ReviewPass, ScherzoConfig,
}
import simplifile

/// Error types for config loading
pub type LoadError {
  /// Error parsing config file
  ParseError(parser.ParseError)
  /// Unknown formula name
  UnknownFormula(name: String)
}

/// Format a load error as a human-readable message
pub fn format_error(error: LoadError) -> String {
  case error {
    ParseError(e) -> parser.format_error(e)
    UnknownFormula(name) ->
      "Unknown formula '"
      <> name
      <> "'. Available formulas: code-review, security-audit, quick-review, none"
  }
}

/// Load configuration from .scherzo/config.toml in the given directory
/// Merges with formula defaults if specified
pub fn load(working_dir: String) -> Result(ScherzoConfig, LoadError) {
  let config_path = working_dir <> "/.scherzo/config.toml"

  // Try to load user config
  case simplifile.is_file(config_path) {
    Ok(True) -> load_with_user_config(config_path)
    _ -> load_formula("none")
  }
}

/// Load config when user config file exists
fn load_with_user_config(config_path: String) -> Result(ScherzoConfig, LoadError) {
  case parser.parse_file(config_path) {
    Error(e) -> Error(ParseError(e))
    Ok(user_config) -> {
      // Get formula name from user config or use default
      let formula_name = case user_config.gates.formula {
        Some(name) -> name
        None -> "none"
      }

      // Load formula defaults
      use formula_config <- result.try(load_formula(formula_name))

      // Merge configs (user wins)
      Ok(merge_configs(formula_config, Some(user_config)))
    }
  }
}

/// Load a named formula
pub fn load_formula(name: String) -> Result(ScherzoConfig, LoadError) {
  case name {
    "none" | "" -> Ok(types.default_config())
    "code-review" -> Ok(code_review_formula())
    "security-audit" -> Ok(security_audit_formula())
    "quick-review" -> Ok(quick_review_formula())
    _ -> Error(UnknownFormula(name))
  }
}

/// Merge two configs, with user config taking precedence
pub fn merge_configs(
  base: ScherzoConfig,
  override: Option(ScherzoConfig),
) -> ScherzoConfig {
  case override {
    None -> base
    Some(user) -> {
      ScherzoConfig(
        gates: merge_gates_config(base.gates, user.gates),
        retry: merge_retry_config(base.retry, user.retry),
      )
    }
  }
}

/// Merge gates configs
fn merge_gates_config(base: GatesConfig, user: GatesConfig) -> GatesConfig {
  // User gates completely replace base gates if any are specified
  let gates = case user.gates {
    [] -> base.gates
    user_gates -> user_gates
  }

  GatesConfig(formula: user.formula, gates: gates)
}

/// Merge retry configs - user values override base values
fn merge_retry_config(_base: RetryConfig, user: RetryConfig) -> RetryConfig {
  // For retry config, we take user values if they differ from defaults
  // This is a simple approach - in practice, we just use user config
  // if any retry section was specified
  user
}

// ============================================================================
// Built-in Formulas
// ============================================================================

/// Code review formula - comprehensive 7-dimension parallel review
fn code_review_formula() -> ScherzoConfig {
  ScherzoConfig(
    gates: GatesConfig(
      formula: Some("code-review"),
      gates: [
        // First run tests and type checking
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
        // Then parallel code review
        ParallelReviewGate(
          name: "code-review",
          dimensions: [
            ReviewDimension(
              id: "correctness",
              focus: "Functional correctness and bug detection",
              prompt: "Review the code changes for correctness. Look for:\n- Logic errors and edge cases\n- Off-by-one errors\n- Null/None handling\n- Race conditions\n- Resource leaks",
            ),
            ReviewDimension(
              id: "security",
              focus: "Security vulnerabilities",
              prompt: "Review the code changes for security issues. Look for:\n- Input validation gaps\n- Injection vulnerabilities\n- Authentication/authorization issues\n- Sensitive data exposure\n- Cryptographic weaknesses",
            ),
            ReviewDimension(
              id: "performance",
              focus: "Performance and efficiency",
              prompt: "Review the code changes for performance. Look for:\n- Algorithmic complexity issues\n- Unnecessary allocations\n- N+1 query patterns\n- Missing caching opportunities\n- Blocking operations",
            ),
            ReviewDimension(
              id: "maintainability",
              focus: "Code clarity and maintainability",
              prompt: "Review the code changes for maintainability. Look for:\n- Complex or unclear logic\n- Poor naming\n- Missing or misleading comments\n- Code duplication\n- Overly long functions",
            ),
            ReviewDimension(
              id: "testing",
              focus: "Test coverage and quality",
              prompt: "Review the test changes (if any). Look for:\n- Missing test cases for new functionality\n- Edge cases not covered\n- Brittle tests\n- Unclear test names\n- Missing assertions",
            ),
            ReviewDimension(
              id: "api-design",
              focus: "API design and contracts",
              prompt: "Review any API changes. Look for:\n- Breaking changes\n- Unclear contracts\n- Poor error handling\n- Missing documentation\n- Inconsistent naming",
            ),
            ReviewDimension(
              id: "architecture",
              focus: "Architectural concerns",
              prompt: "Review for architectural issues. Look for:\n- Inappropriate coupling\n- Layer violations\n- Missing abstractions\n- Scalability concerns\n- Configuration management",
            ),
          ],
          synthesis_prompt: "Synthesize the dimension reviews into a unified assessment. Prioritize findings by severity (P0 critical, P1 major, P2 minor, P3 suggestion). Focus on actionable feedback.",
        ),
      ],
    ),
    retry: types.default_retry_config(),
  )
}

/// Security audit formula - focused security review
fn security_audit_formula() -> ScherzoConfig {
  ScherzoConfig(
    gates: GatesConfig(
      formula: Some("security-audit"),
      gates: [
        // Run tests first
        CommandGate(
          name: "tests",
          command: "gleam test",
          timeout_ms: 300_000,
          serial: True,
          fail_fast: False,
        ),
        // Security-focused multi-pass review
        MultiPassReviewGate(
          name: "security-audit",
          passes: [
            ReviewPass(
              focus: "Input Validation",
              prompt: "Review all external inputs (user data, API calls, file reads) for proper validation. Check for injection vulnerabilities, buffer overflows, and type confusion.",
            ),
            ReviewPass(
              focus: "Authentication & Authorization",
              prompt: "Review authentication and authorization logic. Check for broken auth, privilege escalation, and missing access controls.",
            ),
            ReviewPass(
              focus: "Data Protection",
              prompt: "Review data handling for sensitive information. Check for exposure in logs, insecure storage, weak encryption, and data leakage.",
            ),
            ReviewPass(
              focus: "Dependencies & Configuration",
              prompt: "Review dependencies and configuration for security. Check for known vulnerabilities, insecure defaults, and exposed secrets.",
            ),
            ReviewPass(
              focus: "Attack Surface",
              prompt: "Final pass to identify any remaining attack surface. Consider the full threat model and potential abuse cases.",
            ),
          ],
          require_convergence: True,
          max_passes: 5,
        ),
      ],
    ),
    retry: types.default_retry_config(),
  )
}

/// Quick review formula - fast feedback for small changes
fn quick_review_formula() -> ScherzoConfig {
  ScherzoConfig(
    gates: GatesConfig(
      formula: Some("quick-review"),
      gates: [
        // Just tests and type check
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
      ],
    ),
    retry: RetryConfig(
      strategy: types.SameAgent,
      fresh_after_failures: 3,
      max_iterations: 2,
    ),
  )
}

/// Get list of available formula names
pub fn available_formulas() -> List(String) {
  ["none", "code-review", "security-audit", "quick-review"]
}

/// Get description for a formula
pub fn formula_description(name: String) -> String {
  case name {
    "none" -> "No gates configured"
    "code-review" ->
      "Comprehensive 7-dimension parallel code review with tests"
    "security-audit" -> "Security-focused multi-pass review"
    "quick-review" -> "Fast feedback with just tests and type check"
    _ -> "Unknown formula"
  }
}
