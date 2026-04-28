import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/domain
import scherzo/error

pub type Value {
  VString(String)
  VInt(Int)
  VBool(Bool)
  VList(List(Value))
  VNil
}

pub type Context {
  Context(
    issue: domain.Issue,
    attempt: Option(Int),
    locals: List(#(String, Value)),
  )
}

pub fn render(
  template: String,
  issue: domain.Issue,
  attempt: Option(Int),
) -> Result(String, error.TemplateError) {
  let template = case string.trim(template) {
    "" -> "You are working on an issue from Linear."
    _ -> template
  }
  render_section(template, Context(issue: issue, attempt: attempt, locals: []))
}

fn render_section(
  source: String,
  context: Context,
) -> Result(String, error.TemplateError) {
  case next_token(source) {
    None -> Ok(source)
    Some(#(before, token, after)) ->
      case token.kind {
        Variable(expr) -> {
          use value <- try_template(eval(expr, context))
          use rest <- try_template(render_section(after, context))
          Ok(before <> value_to_string(value) <> rest)
        }
        Tag(tag) -> render_tag(before, tag, after, context)
      }
  }
}

fn render_tag(
  before: String,
  tag: String,
  after: String,
  context: Context,
) -> Result(String, error.TemplateError) {
  let tag = string.trim(tag)
  case string.starts_with(tag, "if ") {
    True ->
      render_if(before, string.trim(string.drop_start(tag, 3)), after, context)
    False ->
      case string.starts_with(tag, "for ") {
        True ->
          render_for(
            before,
            string.trim(string.drop_start(tag, 4)),
            after,
            context,
          )
        False -> Error(error.TemplateRenderError("unknown tag " <> tag))
      }
  }
}

fn render_if(
  before: String,
  expr: String,
  after: String,
  context: Context,
) -> Result(String, error.TemplateError) {
  use block <- try_template(scan_if_block(after))
  use value <- try_template(eval(expr, context))
  let selected = case truthy(value) {
    True -> block.then_body
    False -> block.else_body
  }
  use rendered <- try_template(render_section(selected, context))
  use rest <- try_template(render_section(block.rest, context))
  Ok(before <> rendered <> rest)
}

fn render_for(
  before: String,
  spec: String,
  after: String,
  context: Context,
) -> Result(String, error.TemplateError) {
  case string.split_once(spec, on: " in ") {
    Error(_) -> Error(error.TemplateRenderError("malformed for block"))
    Ok(#(name, expr)) -> {
      use block <- try_template(scan_for_block(after))
      use value <- try_template(eval(string.trim(expr), context))
      case value {
        VList(values) -> {
          use rendered_items <- try_template(
            render_loop(values, string.trim(name), block.body, context, []),
          )
          use rendered_rest <- try_template(render_section(block.rest, context))
          Ok(before <> string.join(rendered_items, with: "") <> rendered_rest)
        }
        _ -> Error(error.TemplateRenderError("for expression is not a list"))
      }
    }
  }
}

type IfBlock {
  IfBlock(then_body: String, else_body: String, rest: String)
}

type ForBlock {
  ForBlock(body: String, rest: String)
}

fn scan_if_block(source: String) -> Result(IfBlock, error.TemplateError) {
  scan_if_loop(source, 0, False, "", "")
}

fn scan_if_loop(
  source: String,
  depth: Int,
  in_else: Bool,
  then_acc: String,
  else_acc: String,
) -> Result(IfBlock, error.TemplateError) {
  case next_token(source) {
    None -> Error(error.TemplateRenderError("malformed if block"))
    Some(#(before, token, after)) ->
      case token.kind {
        Tag(tag) ->
          case is_if_open(tag) {
            True -> {
              let #(then_acc, else_acc) =
                append_current(
                  in_else,
                  then_acc,
                  else_acc,
                  before <> token_source(token),
                )
              scan_if_loop(after, depth + 1, in_else, then_acc, else_acc)
            }
            False ->
              case tag == "endif" {
                True ->
                  case depth == 0 {
                    True ->
                      case in_else {
                        True -> Ok(IfBlock(then_acc, else_acc <> before, after))
                        False -> Ok(IfBlock(then_acc <> before, "", after))
                      }
                    False -> {
                      let #(then_acc, else_acc) =
                        append_current(
                          in_else,
                          then_acc,
                          else_acc,
                          before <> token_source(token),
                        )
                      scan_if_loop(
                        after,
                        depth - 1,
                        in_else,
                        then_acc,
                        else_acc,
                      )
                    }
                  }
                False ->
                  case tag == "else" && depth == 0 {
                    True ->
                      scan_if_loop(
                        after,
                        depth,
                        True,
                        then_acc <> before,
                        else_acc,
                      )
                    False -> {
                      let #(then_acc, else_acc) =
                        append_current(
                          in_else,
                          then_acc,
                          else_acc,
                          before <> token_source(token),
                        )
                      scan_if_loop(after, depth, in_else, then_acc, else_acc)
                    }
                  }
              }
          }
        Variable(_) -> {
          let #(then_acc, else_acc) =
            append_current(
              in_else,
              then_acc,
              else_acc,
              before <> token_source(token),
            )
          scan_if_loop(after, depth, in_else, then_acc, else_acc)
        }
      }
  }
}

fn scan_for_block(source: String) -> Result(ForBlock, error.TemplateError) {
  scan_for_loop(source, 0, "")
}

fn scan_for_loop(
  source: String,
  depth: Int,
  acc: String,
) -> Result(ForBlock, error.TemplateError) {
  case next_token(source) {
    None -> Error(error.TemplateRenderError("malformed for block"))
    Some(#(before, token, after)) ->
      case token.kind {
        Tag(tag) ->
          case is_for_open(tag) {
            True ->
              scan_for_loop(
                after,
                depth + 1,
                acc <> before <> token_source(token),
              )
            False ->
              case tag == "endfor" {
                True ->
                  case depth == 0 {
                    True -> Ok(ForBlock(acc <> before, after))
                    False ->
                      scan_for_loop(
                        after,
                        depth - 1,
                        acc <> before <> token_source(token),
                      )
                  }
                False ->
                  scan_for_loop(
                    after,
                    depth,
                    acc <> before <> token_source(token),
                  )
              }
          }
        Variable(_) ->
          scan_for_loop(after, depth, acc <> before <> token_source(token))
      }
  }
}

fn append_current(
  in_else: Bool,
  then_acc: String,
  else_acc: String,
  piece: String,
) -> #(String, String) {
  case in_else {
    True -> #(then_acc, else_acc <> piece)
    False -> #(then_acc <> piece, else_acc)
  }
}

fn token_source(token: Token) -> String {
  case token.kind {
    Variable(expr) -> "{{ " <> expr <> " }}"
    Tag(tag) -> "{% " <> tag <> " %}"
  }
}

fn is_if_open(tag: String) -> Bool {
  string.starts_with(string.trim(tag), "if ")
}

fn is_for_open(tag: String) -> Bool {
  string.starts_with(string.trim(tag), "for ")
}

fn render_loop(
  values: List(Value),
  name: String,
  body: String,
  context: Context,
  acc: List(String),
) -> Result(List(String), error.TemplateError) {
  case values {
    [] -> Ok(list.reverse(acc))
    [value, ..rest] -> {
      let context =
        Context(..context, locals: [#(name, value), ..context.locals])
      use rendered <- try_template(render_section(body, context))
      render_loop(rest, name, body, context, [rendered, ..acc])
    }
  }
}

pub type TokenKind {
  Variable(String)
  Tag(String)
}

pub type Token {
  Token(kind: TokenKind)
}

fn next_token(source: String) -> Option(#(String, Token, String)) {
  let var = string.split_once(source, on: "{{")
  let tag = string.split_once(source, on: "{%")
  case var, tag {
    Error(_), Error(_) -> None
    Ok(#(before, rest)), Error(_) -> variable_token(before, rest)
    Error(_), Ok(#(before, rest)) -> tag_token(before, rest)
    Ok(#(var_before, var_rest)), Ok(#(tag_before, tag_rest)) ->
      case string.length(var_before) <= string.length(tag_before) {
        True -> variable_token(var_before, var_rest)
        False -> tag_token(tag_before, tag_rest)
      }
  }
}

fn variable_token(
  before: String,
  rest: String,
) -> Option(#(String, Token, String)) {
  case string.split_once(rest, on: "}}") {
    Ok(#(expr, after)) ->
      Some(#(before, Token(Variable(string.trim(expr))), after))
    Error(_) -> Some(#(before, Token(Tag("malformed variable")), ""))
  }
}

fn tag_token(before: String, rest: String) -> Option(#(String, Token, String)) {
  case string.split_once(rest, on: "%}") {
    Ok(#(tag, after)) -> Some(#(before, Token(Tag(string.trim(tag))), after))
    Error(_) -> Some(#(before, Token(Tag("malformed tag")), ""))
  }
}

fn eval(expr: String, context: Context) -> Result(Value, error.TemplateError) {
  case string.contains(expr, "|") {
    True -> Error(error.TemplateRenderError("unknown filter in " <> expr))
    False -> eval_no_filter(string.trim(expr), context)
  }
}

fn eval_no_filter(
  expr: String,
  context: Context,
) -> Result(Value, error.TemplateError) {
  case list.key_find(context.locals, expr) {
    Ok(value) -> Ok(value)
    Error(_) ->
      case expr {
        "attempt" ->
          case context.attempt {
            Some(value) -> Ok(VInt(value))
            None -> Ok(VNil)
          }
        "issue.id" -> Ok(VString(context.issue.id))
        "issue.identifier" -> Ok(VString(context.issue.identifier))
        "issue.title" -> Ok(VString(context.issue.title))
        "issue.description" -> Ok(option_to_value(context.issue.description))
        "issue.priority" -> Ok(option_int_to_value(context.issue.priority))
        "issue.state" -> Ok(VString(context.issue.state))
        "issue.branch_name" -> Ok(option_to_value(context.issue.branch_name))
        "issue.url" -> Ok(option_to_value(context.issue.url))
        "issue.labels" -> Ok(VList(list.map(context.issue.labels, VString)))
        "issue.created_at" -> Ok(VNil)
        "issue.updated_at" -> Ok(VNil)
        _ -> Error(error.TemplateRenderError("unknown variable " <> expr))
      }
  }
}

fn option_to_value(value: Option(String)) -> Value {
  case value {
    Some(value) -> VString(value)
    None -> VNil
  }
}

fn option_int_to_value(value: Option(Int)) -> Value {
  case value {
    Some(value) -> VInt(value)
    None -> VNil
  }
}

fn value_to_string(value: Value) -> String {
  case value {
    VString(value) -> value
    VInt(value) -> int_to_string(value)
    VBool(True) -> "true"
    VBool(False) -> "false"
    VList(_) -> ""
    VNil -> ""
  }
}

fn truthy(value: Value) -> Bool {
  case value {
    VString(value) -> value != ""
    VInt(value) -> value != 0
    VBool(value) -> value
    VList(values) -> values != []
    VNil -> False
  }
}

fn try_template(
  result: Result(a, error.TemplateError),
  next: fn(a) -> Result(b, error.TemplateError),
) -> Result(b, error.TemplateError) {
  case result {
    Ok(value) -> next(value)
    Error(err) -> Error(err)
  }
}

@external(erlang, "erlang", "integer_to_binary")
fn int_to_string(value: Int) -> String
