/// Config validation for Scherzo configuration
///
/// Validates configuration structures to catch invalid values before
/// gates are executed. All validation rules return helpful error messages.
import gleam/int
import gleam/list
import gleam/string
import scherzo/config/types.{
  type GateConfig, type GatesConfig, type RetryConfig, type ReviewDimension,
  type ReviewPass, type ScherzoConfig, CommandGate, HumanGate,
  MultiPassReviewGate, ParallelReviewGate,
}

/// Validation error with path and message
pub type ValidationError {
  ValidationError(path: String, message: String)
}

/// Result of validation - list of all errors found
pub type ValidationResult =
  List(ValidationError)

/// Validate a ScherzoConfig, returning all validation errors
pub fn validate(config: ScherzoConfig) -> ValidationResult {
  let gates_errors = validate_gates_config(config.gates)
  let retry_errors = validate_retry_config(config.retry)
  list.append(gates_errors, retry_errors)
}

/// Check if config is valid (no errors)
pub fn is_valid(config: ScherzoConfig) -> Bool {
  validate(config) == []
}

/// Format validation errors as a human-readable message
pub fn format_errors(errors: ValidationResult) -> String {
  case errors {
    [] -> "Configuration is valid"
    _ -> {
      let formatted =
        errors
        |> list.map(fn(e) { "  - " <> e.path <> ": " <> e.message })
        |> string.join("\n")
      "Configuration validation failed:\n" <> formatted
    }
  }
}

/// Validate gates configuration
fn validate_gates_config(config: GatesConfig) -> ValidationResult {
  config.gates
  |> list.index_map(fn(gate, index) { validate_gate(gate, index) })
  |> list.flatten
}

/// Validate a single gate
fn validate_gate(gate: GateConfig, index: Int) -> ValidationResult {
  let path = "gates.gates[" <> int.to_string(index) <> "]"

  case gate {
    CommandGate(name, command, timeout_ms, _, _) ->
      validate_command_gate(path, name, command, timeout_ms)

    ParallelReviewGate(name, dimensions, synthesis_prompt) ->
      validate_parallel_review_gate(path, name, dimensions, synthesis_prompt)

    MultiPassReviewGate(name, passes, _, max_passes) ->
      validate_multipass_review_gate(path, name, passes, max_passes)

    HumanGate(name, prompt) -> validate_human_gate(path, name, prompt)
  }
}

/// Validate command gate
fn validate_command_gate(
  path: String,
  name: String,
  command: String,
  timeout_ms: Int,
) -> ValidationResult {
  let errors = []

  // Name must not be empty
  let errors = case string.is_empty(string.trim(name)) {
    True -> [
      ValidationError(path <> ".name", "Gate name cannot be empty"),
      ..errors
    ]
    False -> errors
  }

  // Command must not be empty
  let errors = case string.is_empty(string.trim(command)) {
    True -> [
      ValidationError(path <> ".command", "Command cannot be empty"),
      ..errors
    ]
    False -> errors
  }

  // Timeout must be non-negative
  let errors = case timeout_ms < 0 {
    True -> [
      ValidationError(
        path <> ".timeout_ms",
        "Timeout must be non-negative, got " <> int.to_string(timeout_ms),
      ),
      ..errors
    ]
    False -> errors
  }

  errors
}

/// Validate parallel review gate
fn validate_parallel_review_gate(
  path: String,
  name: String,
  dimensions: List(ReviewDimension),
  synthesis_prompt: String,
) -> ValidationResult {
  let errors = []

  // Name must not be empty
  let errors = case string.is_empty(string.trim(name)) {
    True -> [
      ValidationError(path <> ".name", "Gate name cannot be empty"),
      ..errors
    ]
    False -> errors
  }

  // Must have at least one dimension
  let errors = case dimensions {
    [] -> [
      ValidationError(
        path <> ".dimensions",
        "Parallel review must have at least one dimension",
      ),
      ..errors
    ]
    _ -> errors
  }

  // Validate each dimension
  let dimension_errors =
    dimensions
    |> list.index_map(fn(dim, idx) { validate_dimension(path, dim, idx) })
    |> list.flatten

  // Synthesis prompt must not be empty
  let errors = case string.is_empty(string.trim(synthesis_prompt)) {
    True -> [
      ValidationError(
        path <> ".synthesis_prompt",
        "Synthesis prompt cannot be empty",
      ),
      ..errors
    ]
    False -> errors
  }

  list.append(errors, dimension_errors)
}

/// Validate a review dimension
fn validate_dimension(
  parent_path: String,
  dim: ReviewDimension,
  index: Int,
) -> ValidationResult {
  let path = parent_path <> ".dimensions[" <> int.to_string(index) <> "]"
  let errors = []

  // ID must not be empty
  let errors = case string.is_empty(string.trim(dim.id)) {
    True -> [
      ValidationError(path <> ".id", "Dimension ID cannot be empty"),
      ..errors
    ]
    False -> errors
  }

  // Focus must not be empty
  let errors = case string.is_empty(string.trim(dim.focus)) {
    True -> [
      ValidationError(path <> ".focus", "Dimension focus cannot be empty"),
      ..errors
    ]
    False -> errors
  }

  // Prompt must not be empty
  let errors = case string.is_empty(string.trim(dim.prompt)) {
    True -> [
      ValidationError(path <> ".prompt", "Dimension prompt cannot be empty"),
      ..errors
    ]
    False -> errors
  }

  errors
}

/// Validate multi-pass review gate
fn validate_multipass_review_gate(
  path: String,
  name: String,
  passes: List(ReviewPass),
  max_passes: Int,
) -> ValidationResult {
  let errors = []

  // Name must not be empty
  let errors = case string.is_empty(string.trim(name)) {
    True -> [
      ValidationError(path <> ".name", "Gate name cannot be empty"),
      ..errors
    ]
    False -> errors
  }

  // Must have at least one pass
  let errors = case passes {
    [] -> [
      ValidationError(
        path <> ".passes",
        "Multi-pass review must have at least one pass",
      ),
      ..errors
    ]
    _ -> errors
  }

  // Validate each pass
  let pass_errors =
    passes
    |> list.index_map(fn(pass, idx) { validate_pass(path, pass, idx) })
    |> list.flatten

  // Max passes must be positive
  let errors = case max_passes < 1 {
    True -> [
      ValidationError(
        path <> ".max_passes",
        "Max passes must be at least 1, got " <> int.to_string(max_passes),
      ),
      ..errors
    ]
    False -> errors
  }

  list.append(errors, pass_errors)
}

/// Validate a review pass
fn validate_pass(
  parent_path: String,
  pass: ReviewPass,
  index: Int,
) -> ValidationResult {
  let path = parent_path <> ".passes[" <> int.to_string(index) <> "]"
  let errors = []

  // Focus must not be empty
  let errors = case string.is_empty(string.trim(pass.focus)) {
    True -> [
      ValidationError(path <> ".focus", "Pass focus cannot be empty"),
      ..errors
    ]
    False -> errors
  }

  // Prompt must not be empty
  let errors = case string.is_empty(string.trim(pass.prompt)) {
    True -> [
      ValidationError(path <> ".prompt", "Pass prompt cannot be empty"),
      ..errors
    ]
    False -> errors
  }

  errors
}

/// Validate human gate
fn validate_human_gate(
  path: String,
  name: String,
  prompt: String,
) -> ValidationResult {
  let errors = []

  // Name must not be empty
  let errors = case string.is_empty(string.trim(name)) {
    True -> [
      ValidationError(path <> ".name", "Gate name cannot be empty"),
      ..errors
    ]
    False -> errors
  }

  // Prompt must not be empty
  let errors = case string.is_empty(string.trim(prompt)) {
    True -> [
      ValidationError(path <> ".prompt", "Prompt cannot be empty"),
      ..errors
    ]
    False -> errors
  }

  errors
}

/// Validate retry configuration
fn validate_retry_config(config: RetryConfig) -> ValidationResult {
  let errors = []

  // max_iterations must be positive
  let errors = case config.max_iterations < 1 {
    True -> [
      ValidationError(
        "retry.max_iterations",
        "Max iterations must be at least 1, got "
          <> int.to_string(config.max_iterations),
      ),
      ..errors
    ]
    False -> errors
  }

  // fresh_after_failures must be non-negative
  let errors = case config.fresh_after_failures < 0 {
    True -> [
      ValidationError(
        "retry.fresh_after_failures",
        "Fresh after failures must be non-negative, got "
          <> int.to_string(config.fresh_after_failures),
      ),
      ..errors
    ]
    False -> errors
  }

  errors
}

/// Validate and return Result
pub fn validate_strict(
  config: ScherzoConfig,
) -> Result(ScherzoConfig, ValidationResult) {
  let errors = validate(config)
  case errors {
    [] -> Ok(config)
    _ -> Error(errors)
  }
}
