import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

pub type ParseError {
  ParseError(message: String)
}

pub type ParseOutcome(handler) {
  Parsed(ParsedCommand(handler))
  HelpRequested
}

pub type ParsedCommand(handler) {
  ParsedCommand(
    handler: handler,
    path: List(String),
    positionals: List(String),
    options: List(ParsedOption),
    usage: String,
  )
}

pub type ParsedOption {
  ParsedFlag(name: String)
  ParsedValue(name: String, value: String)
}

pub type PositionalSpec {
  Required(name: String)
  Optional(name: String)
  Rest(name: String)
}

pub type OptionSpec {
  FlagOption(name: String, help: String)
  ValueOption(
    name: String,
    value_name: String,
    help: String,
    repeatable: Bool,
    validate: fn(String) -> Result(String, String),
  )
}

pub type HelpLine {
  HelpLine(left: String, right: String)
}

pub type CommandSpec(handler) {
  CommandSpec(
    handler: handler,
    path: List(String),
    usage: String,
    summary: String,
    positionals: List(PositionalSpec),
    options: List(OptionSpec),
    help_lines: List(HelpLine),
  )
}

pub fn flag_option(name: String, help: String) -> OptionSpec {
  FlagOption(name, help)
}

pub fn value_option(
  name: String,
  value_name: String,
  help: String,
  repeatable: Bool,
  validate: fn(String) -> Result(String, String),
) -> OptionSpec {
  ValueOption(name, value_name, help, repeatable, validate)
}

pub fn passthrough_value(value: String) -> Result(String, String) {
  Ok(value)
}

pub fn parse(
  args: List(String),
  specs: List(CommandSpec(handler)),
) -> Result(ParseOutcome(handler), ParseError) {
  use #(words, options) <- try_parse(tokenize(args, all_options(specs), [], []))
  case words {
    ["--help"] -> Ok(HelpRequested)
    _ -> {
      use spec <- try_parse(select_spec(words, specs))
      use positionals <- try_parse(validate_positionals(
        list.drop(words, list.length(spec.path)),
        spec.positionals,
        spec.usage,
      ))
      use validated_options <- try_parse(validate_options(options, spec))
      Ok(
        Parsed(ParsedCommand(
          handler: spec.handler,
          path: spec.path,
          positionals: positionals,
          options: validated_options,
          usage: spec.usage,
        )),
      )
    }
  }
}

pub fn error_message(error: ParseError) -> String {
  let ParseError(message) = error
  message
}

pub fn has_flag(parsed: ParsedCommand(handler), name: String) -> Bool {
  parsed.options
  |> list.any(fn(option) {
    case option {
      ParsedFlag(option_name) -> option_name == name
      ParsedValue(_, _) -> False
    }
  })
}

pub fn option_value(
  parsed: ParsedCommand(handler),
  name: String,
) -> Option(String) {
  option_value_loop(list.reverse(parsed.options), name)
}

pub fn option_values(
  parsed: ParsedCommand(handler),
  name: String,
) -> List(String) {
  option_values_loop(parsed.options, name, [])
}

pub fn render_help_lines(lines: List(HelpLine)) -> List(String) {
  let width = int_min(max_left_width(lines, 0), 27)
  list.map(lines, fn(line) { render_help_line(line, width) })
}

pub fn option_name(option: OptionSpec) -> String {
  case option {
    FlagOption(name, _) -> name
    ValueOption(name, _, _, _, _) -> name
  }
}

pub fn option_help_line(option: OptionSpec) -> HelpLine {
  case option {
    FlagOption(name, help) -> HelpLine(name, help)
    ValueOption(name, value_name, help, _, _) ->
      HelpLine(name <> " " <> value_name, help)
  }
}

fn option_value_loop(
  options: List(ParsedOption),
  name: String,
) -> Option(String) {
  case options {
    [] -> None
    [option, ..rest] ->
      case option {
        ParsedValue(option_name, value) if option_name == name -> Some(value)
        _ -> option_value_loop(rest, name)
      }
  }
}

fn option_values_loop(
  options: List(ParsedOption),
  name: String,
  acc: List(String),
) -> List(String) {
  case options {
    [] -> list.reverse(acc)
    [option, ..rest] ->
      case option {
        ParsedValue(option_name, value) if option_name == name ->
          option_values_loop(rest, name, [value, ..acc])
        _ -> option_values_loop(rest, name, acc)
      }
  }
}

fn render_help_line(line: HelpLine, width: Int) -> String {
  let HelpLine(left, right) = line
  let prefix = "  "
  let gap = " "
  case left == "" {
    True -> prefix <> string.repeat(" ", width) <> gap <> right
    False -> prefix <> pad_right(left, width) <> gap <> right
  }
}

fn pad_right(value: String, width: Int) -> String {
  let padding = width - string.length(value)
  case padding > 0 {
    True -> value <> string.repeat(" ", padding)
    False -> value
  }
}

fn max_left_width(lines: List(HelpLine), current: Int) -> Int {
  case lines {
    [] -> current
    [HelpLine(left, _), ..rest] -> {
      let next = int_max(current, string.length(left))
      max_left_width(rest, next)
    }
  }
}

fn tokenize(
  args: List(String),
  known_options: List(OptionSpec),
  words_acc: List(String),
  options_acc: List(ParsedOption),
) -> Result(#(List(String), List(ParsedOption)), ParseError) {
  case args {
    [] -> Ok(#(list.reverse(words_acc), list.reverse(options_acc)))
    ["--help", ..] | ["-h", ..] -> Ok(#(["--help"], list.reverse(options_acc)))
    [arg, ..rest] ->
      case string.starts_with(arg, "--") {
        True ->
          tokenize_option(arg, rest, known_options, words_acc, options_acc)
        False -> tokenize(rest, known_options, [arg, ..words_acc], options_acc)
      }
  }
}

fn tokenize_option(
  arg: String,
  rest: List(String),
  known_options: List(OptionSpec),
  words_acc: List(String),
  options_acc: List(ParsedOption),
) -> Result(#(List(String), List(ParsedOption)), ParseError) {
  case string.split_once(arg, on: "=") {
    Ok(#(name, value)) ->
      case find_option(known_options, name) {
        Some(FlagOption(_, _)) ->
          Error(ParseError("option does not take a value: " <> name))
        Some(ValueOption(option_name, _, _, _, validate)) -> {
          use normalized <- try_parse(validate_value(
            option_name,
            value,
            validate,
          ))
          tokenize(rest, known_options, words_acc, [
            ParsedValue(option_name, normalized),
            ..options_acc
          ])
        }
        None -> Error(ParseError("unknown option: " <> name))
      }
    Error(_) ->
      case find_option(known_options, arg) {
        Some(FlagOption(name, _)) ->
          tokenize(rest, known_options, words_acc, [
            ParsedFlag(name),
            ..options_acc
          ])
        Some(ValueOption(name, value_name, _, _, validate)) ->
          case rest {
            [value, ..remaining] -> {
              use normalized <- try_parse(validate_value(name, value, validate))
              tokenize(remaining, known_options, words_acc, [
                ParsedValue(name, normalized),
                ..options_acc
              ])
            }
            [] -> Error(ParseError(name <> " requires " <> value_name))
          }
        None -> Error(ParseError("unknown option: " <> arg))
      }
  }
}

fn validate_value(
  _name: String,
  value: String,
  validate: fn(String) -> Result(String, String),
) -> Result(String, ParseError) {
  case validate(value) {
    Ok(normalized) -> Ok(normalized)
    Error(message) -> Error(ParseError(message))
  }
}

fn all_options(specs: List(CommandSpec(handler))) -> List(OptionSpec) {
  case specs {
    [] -> []
    [spec, ..rest] -> list.append(spec.options, all_options(rest))
  }
}

fn select_spec(
  words: List(String),
  specs: List(CommandSpec(handler)),
) -> Result(CommandSpec(handler), ParseError) {
  case longest_matching_spec(words, specs, None) {
    Some(spec) -> Ok(spec)
    None ->
      case words {
        [name, ..] ->
          Error(ParseError("unknown or invalid ctl command: " <> name))
        [] -> Error(ParseError("unknown or invalid ctl command"))
      }
  }
}

fn longest_matching_spec(
  words: List(String),
  specs: List(CommandSpec(handler)),
  best: Option(CommandSpec(handler)),
) -> Option(CommandSpec(handler)) {
  case specs {
    [] -> best
    [spec, ..rest] -> {
      let next_best = case path_matches(words, spec.path) {
        True -> pick_longer(best, spec)
        False -> best
      }
      longest_matching_spec(words, rest, next_best)
    }
  }
}

fn pick_longer(
  best: Option(CommandSpec(handler)),
  candidate: CommandSpec(handler),
) -> Option(CommandSpec(handler)) {
  case best {
    None -> Some(candidate)
    Some(current) ->
      case list.length(candidate.path) > list.length(current.path) {
        True -> Some(candidate)
        False -> best
      }
  }
}

fn path_matches(words: List(String), path: List(String)) -> Bool {
  case path, words {
    [], _ -> True
    [segment, ..path_rest], [word, ..word_rest] if segment == word ->
      path_matches(word_rest, path_rest)
    _, _ -> False
  }
}

fn validate_positionals(
  values: List(String),
  specs: List(PositionalSpec),
  usage: String,
) -> Result(List(String), ParseError) {
  case validate_positionals_loop(values, specs, usage) {
    Ok(Nil) -> Ok(values)
    Error(error) -> Error(error)
  }
}

fn validate_positionals_loop(
  values: List(String),
  specs: List(PositionalSpec),
  usage: String,
) -> Result(Nil, ParseError) {
  case specs, values {
    [], [] -> Ok(Nil)
    [], _ -> Error(ParseError(usage_error(usage)))
    [Required(_), ..], [] -> Error(ParseError(usage_error(usage)))
    [Required(_), ..rest], [_, ..value_rest] ->
      validate_positionals_loop(value_rest, rest, usage)
    [Optional(_), ..rest], [] -> validate_positionals_loop([], rest, usage)
    [Optional(_), ..rest], [_, ..value_rest] ->
      validate_positionals_loop(value_rest, rest, usage)
    [Rest(_)], _ -> Ok(Nil)
    [Rest(_), ..], _ -> Ok(Nil)
  }
}

fn validate_options(
  options: List(ParsedOption),
  spec: CommandSpec(handler),
) -> Result(List(ParsedOption), ParseError) {
  validate_options_loop(options, spec, [])
}

fn validate_options_loop(
  options: List(ParsedOption),
  spec: CommandSpec(handler),
  seen_single_values: List(String),
) -> Result(List(ParsedOption), ParseError) {
  case options {
    [] -> Ok([])
    [option, ..rest] -> {
      let name = case option {
        ParsedFlag(option_name) -> option_name
        ParsedValue(option_name, _) -> option_name
      }
      case find_option(spec.options, name) {
        Some(option_spec) ->
          case option {
            ParsedFlag(_) ->
              case option_spec {
                FlagOption(_, _) ->
                  validate_options_loop(rest, spec, seen_single_values)
                  |> result.map(fn(validated_rest) {
                    [option, ..validated_rest]
                  })
                ValueOption(_, _, _, _, _) ->
                  Error(ParseError(name <> " requires a value"))
              }
            ParsedValue(_, _) ->
              case option_spec {
                FlagOption(_, _) ->
                  Error(ParseError("option does not take a value: " <> name))
                ValueOption(_, _, _, repeatable, _) ->
                  case repeatable, list.contains(seen_single_values, name) {
                    False, True ->
                      Error(ParseError(
                        "option may only be supplied once: " <> name,
                      ))
                    True, _ ->
                      validate_options_loop(rest, spec, seen_single_values)
                      |> result.map(fn(validated_rest) {
                        [option, ..validated_rest]
                      })
                    False, False ->
                      validate_options_loop(rest, spec, [
                        name,
                        ..seen_single_values
                      ])
                      |> result.map(fn(validated_rest) {
                        [option, ..validated_rest]
                      })
                  }
              }
          }
        None ->
          Error(ParseError(
            "unsupported option for "
            <> string.join(spec.path, with: " ")
            <> ": "
            <> name,
          ))
      }
    }
  }
}

fn find_option(options: List(OptionSpec), name: String) -> Option(OptionSpec) {
  case options {
    [] -> None
    [option, ..rest] ->
      case option_name(option) == name {
        True -> Some(option)
        False -> find_option(rest, name)
      }
  }
}

fn usage_error(usage: String) -> String {
  case string.starts_with(usage, "Usage:") {
    True -> usage
    False -> usage <> " usage: " <> usage
  }
}

fn int_max(left: Int, right: Int) -> Int {
  case left >= right {
    True -> left
    False -> right
  }
}

fn int_min(left: Int, right: Int) -> Int {
  case left <= right {
    True -> left
    False -> right
  }
}

fn try_parse(
  value: Result(a, ParseError),
  next: fn(a) -> Result(b, ParseError),
) -> Result(b, ParseError) {
  case value {
    Ok(inner) -> next(inner)
    Error(error) -> Error(error)
  }
}
