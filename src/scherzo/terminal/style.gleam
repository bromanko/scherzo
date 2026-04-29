import gleam/string

pub type ColorMode {
  ColorAuto
  ColorAlways
  ColorNever
}

pub fn heading(mode: ColorMode, text: String) -> String {
  color(mode, "\u{1b}[1m", text)
}

pub fn dim(mode: ColorMode, text: String) -> String {
  color(mode, "\u{1b}[2m", text)
}

pub fn success(mode: ColorMode, text: String) -> String {
  color(mode, "\u{1b}[32m", text)
}

pub fn warning(mode: ColorMode, text: String) -> String {
  color(mode, "\u{1b}[33m", text)
}

pub fn error(mode: ColorMode, text: String) -> String {
  color(mode, "\u{1b}[31m", text)
}

pub fn assistant_label(mode: ColorMode, text: String) -> String {
  color(mode, "\u{1b}[36m", text)
}

pub fn tool_label(mode: ColorMode, text: String) -> String {
  color(mode, "\u{1b}[35m", text)
}

fn color(mode: ColorMode, code: String, text: String) -> String {
  case mode {
    ColorAlways -> code <> text <> "\u{1b}[0m"
    ColorAuto | ColorNever -> text
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
