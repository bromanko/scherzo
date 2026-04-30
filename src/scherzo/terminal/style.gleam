import gleam/string

pub type ColorMode {
  ColorAuto
  ColorAlways
  ColorNever
}

const reset = "\u{1b}[0m"

const dim_gray = "\u{1b}[38;2;102;102;102m"

const thinking_gray = "\u{1b}[38;2;128;128;128m"

const tool_pending_bg = "\u{1b}[48;2;40;40;50m"

pub fn heading(mode: ColorMode, text: String) -> String {
  color(mode, "\u{1b}[1m", text)
}

pub fn dim(mode: ColorMode, text: String) -> String {
  color(mode, dim_gray, text)
}

pub fn success(mode: ColorMode, text: String) -> String {
  color(mode, "\u{1b}[38;2;181;189;104m", text)
}

pub fn warning(mode: ColorMode, text: String) -> String {
  color(mode, "\u{1b}[38;2;240;198;116m", text)
}

pub fn error(mode: ColorMode, text: String) -> String {
  color(mode, "\u{1b}[38;2;204;102;102m", text)
}

pub fn meta_label(mode: ColorMode, text: String) -> String {
  color(mode, dim_gray, text)
}

pub fn thinking_label(mode: ColorMode, text: String) -> String {
  color(mode, "\u{1b}[3m" <> thinking_gray, text)
}

pub fn tool_label(mode: ColorMode, text: String) -> String {
  background(mode, "\u{1b}[1m" <> tool_pending_bg, text)
}

pub fn input_label(mode: ColorMode, text: String) -> String {
  background(mode, tool_pending_bg, text)
}

pub fn output_label(mode: ColorMode, text: String) -> String {
  background(mode, tool_pending_bg, text)
}

pub fn tool_body_line(mode: ColorMode, text: String) -> String {
  background(mode, tool_pending_bg, text)
}

pub fn tool_gap_line(mode: ColorMode) -> String {
  background(mode, tool_pending_bg, "")
}

pub fn status_label(mode: ColorMode, text: String) -> String {
  background(mode, tool_pending_bg, text)
}

pub fn success_status_label(mode: ColorMode, text: String) -> String {
  background(mode, tool_pending_bg, text)
}

pub fn error_status_label(mode: ColorMode, text: String) -> String {
  background(mode, tool_pending_bg, text)
}

pub fn raw_label(mode: ColorMode, text: String) -> String {
  color(mode, dim_gray, text)
}

fn color(mode: ColorMode, code: String, text: String) -> String {
  case color_enabled(mode) {
    True -> code <> text <> reset
    False -> text
  }
}

fn background(mode: ColorMode, code: String, text: String) -> String {
  case color_enabled(mode) {
    True -> code <> pad_to_terminal_width(text) <> reset
    False -> text
  }
}

fn pad_to_terminal_width(text: String) -> String {
  let width = terminal_columns()
  let visible_width = string.length(text)
  case width > visible_width {
    True -> text <> string.repeat(" ", times: width - visible_width)
    False -> text
  }
}

pub fn color_enabled(mode: ColorMode) -> Bool {
  case mode {
    ColorAlways -> True
    ColorNever -> False
    ColorAuto -> stdout_supports_color()
  }
}

pub fn parse_color_mode(value: String) -> Result(ColorMode, Nil) {
  case string.lowercase(value) {
    "auto" -> Ok(ColorAuto)
    "always" -> Ok(ColorAlways)
    "never" -> Ok(ColorNever)
    _ -> Error(Nil)
  }
}

@external(erlang, "scherzo_terminal_ffi", "stdout_supports_color")
fn stdout_supports_color() -> Bool

@external(erlang, "scherzo_terminal_ffi", "terminal_columns")
fn terminal_columns() -> Int
