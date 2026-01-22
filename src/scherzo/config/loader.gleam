/// Config loader - loads gate configurations from config files
///
/// The config file is the source of truth - all gates are explicitly defined.
/// No formula expansion or hidden abstractions.
import scherzo/config/parser
import scherzo/config/types.{type ScherzoConfig}
import simplifile

/// Error types for config loading
pub type LoadError {
  /// Error parsing config file
  ParseError(parser.ParseError)
}

/// Format a load error as a human-readable message
pub fn format_error(error: LoadError) -> String {
  case error {
    ParseError(e) -> parser.format_error(e)
  }
}

/// Load configuration from .scherzo/config.toml in the given directory
pub fn load(working_dir: String) -> Result(ScherzoConfig, LoadError) {
  let config_path = working_dir <> "/.scherzo/config.toml"

  case simplifile.is_file(config_path) {
    Ok(True) ->
      case parser.parse_file(config_path) {
        Ok(config) -> Ok(config)
        Error(e) -> Error(ParseError(e))
      }
    _ ->
      // No config file - use defaults (no gates)
      Ok(types.default_config())
  }
}
