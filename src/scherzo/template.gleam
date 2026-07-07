import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/error
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state

pub type Value {
  VString(String)
  VInt(Int)
  VBool(Bool)
  VList(List(Value))
  VNil
}

pub type InvocationContext {
  IssueInvocation(issue: tracker_issue.Issue)
  ScheduledInvocation(run: ScheduledTemplateContext)
}

pub type ScheduledTemplateContext {
  ScheduledTemplateContext(
    job_id: String,
    workflow_id: String,
    due_at: String,
    started_at: String,
    run_id: String,
    attempt: Int,
  )
}

pub type Context {
  Context(
    invocation: InvocationContext,
    attempt: Option(Int),
    locals: List(#(String, Value)),
  )
}

pub const include_depth_limit = 3

pub type IncludeDependency {
  IncludeDependency(path: String, contents: String)
}

pub type IncludeExpansion {
  IncludeExpansion(contents: String, dependencies: List(IncludeDependency))
}

pub fn expand_includes(
  source: String,
  source_path: String,
  resolver: fn(String, String) -> Result(IncludeDependency, error.TemplateError),
) -> Result(IncludeExpansion, error.TemplateError) {
  expand_includes_with_limit(source, source_path, include_depth_limit, resolver)
}

pub fn expand_includes_with_limit(
  source: String,
  source_path: String,
  max_depth: Int,
  resolver: fn(String, String) -> Result(IncludeDependency, error.TemplateError),
) -> Result(IncludeExpansion, error.TemplateError) {
  use contents_and_dependencies <- try_template(expand_include_source(
    source,
    source_path,
    max_depth,
    0,
    [source_path],
    resolver,
  ))
  let #(contents, dependencies) = contents_and_dependencies
  Ok(IncludeExpansion(
    contents: contents,
    dependencies: normalize_include_dependencies(dependencies),
  ))
}

fn expand_include_source(
  source: String,
  source_path: String,
  max_depth: Int,
  depth: Int,
  stack: List(String),
  resolver: fn(String, String) -> Result(IncludeDependency, error.TemplateError),
) -> Result(#(String, List(IncludeDependency)), error.TemplateError) {
  case next_token(source) {
    None -> Ok(#(source, []))
    Some(#(before, token, after)) ->
      case token.kind {
        Variable(_) -> {
          use #(expanded_after, dependencies) <- try_template(
            expand_include_source(
              after,
              source_path,
              max_depth,
              depth,
              stack,
              resolver,
            ),
          )
          Ok(#(before <> token_source(token) <> expanded_after, dependencies))
        }
        Tag(tag) ->
          case parse_include_tag(tag) {
            Ok(Some(include_path)) -> {
              use dependency <- try_template(resolve_include_dependency(
                include_path,
                source_path,
                max_depth,
                depth,
                stack,
                resolver,
              ))
              use #(expanded_after, after_dependencies) <- try_template(
                expand_include_source(
                  after,
                  source_path,
                  max_depth,
                  depth,
                  stack,
                  resolver,
                ),
              )
              let IncludeDependency(path:, contents:) = dependency
              use #(expanded_fragment, fragment_dependencies) <- try_template(
                expand_include_source(
                  contents,
                  path,
                  max_depth,
                  depth + 1,
                  [path, ..stack],
                  resolver,
                ),
              )
              Ok(
                #(before <> expanded_fragment <> expanded_after, [
                  dependency,
                  ..list.append(fragment_dependencies, after_dependencies)
                ]),
              )
            }
            Ok(None) -> {
              use #(expanded_after, dependencies) <- try_template(
                expand_include_source(
                  after,
                  source_path,
                  max_depth,
                  depth,
                  stack,
                  resolver,
                ),
              )
              Ok(#(
                before <> token_source(token) <> expanded_after,
                dependencies,
              ))
            }
            Error(err) -> Error(err)
          }
      }
  }
}

fn resolve_include_dependency(
  include_path: String,
  source_path: String,
  max_depth: Int,
  depth: Int,
  stack: List(String),
  resolver: fn(String, String) -> Result(IncludeDependency, error.TemplateError),
) -> Result(IncludeDependency, error.TemplateError) {
  case depth >= max_depth {
    True ->
      Error(error.TemplateRenderError(
        "include depth limit exceeded while expanding "
        <> include_path
        <> " from "
        <> source_path,
      ))
    False -> {
      use dependency <- try_template(resolver(include_path, source_path))
      case list.contains(stack, dependency.path) {
        True ->
          Error(error.TemplateRenderError(
            "include cycle detected: "
            <> string.join(
              list.reverse([dependency.path, ..stack]),
              with: " -> ",
            ),
          ))
        False -> Ok(dependency)
      }
    }
  }
}

fn parse_include_tag(
  tag: String,
) -> Result(Option(String), error.TemplateError) {
  let tag = string.trim(tag)
  case tag == "include" || string.starts_with(tag, "include ") {
    False -> Ok(None)
    True -> {
      let literal = string.trim(string.drop_start(tag, 7))
      case is_quoted_include_literal(literal) {
        True -> {
          let include_path =
            literal |> string.drop_start(1) |> string.drop_end(1) |> string.trim
          case include_path == "" {
            True -> Error(error.TemplateRenderError("malformed include tag"))
            False -> Ok(Some(include_path))
          }
        }
        False -> Error(error.TemplateRenderError("malformed include tag"))
      }
    }
  }
}

fn is_quoted_include_literal(value: String) -> Bool {
  string.length(value) >= 2
  && string.starts_with(value, "\"")
  && string.ends_with(value, "\"")
}

fn normalize_include_dependencies(
  dependencies: List(IncludeDependency),
) -> List(IncludeDependency) {
  dependencies
  |> list.reverse
  |> normalize_include_dependencies_loop([], [])
}

fn normalize_include_dependencies_loop(
  dependencies: List(IncludeDependency),
  seen: List(String),
  acc: List(IncludeDependency),
) -> List(IncludeDependency) {
  case dependencies {
    [] -> list.reverse(acc)
    [dependency, ..rest] ->
      case list.contains(seen, dependency.path) {
        True -> normalize_include_dependencies_loop(rest, seen, acc)
        False ->
          normalize_include_dependencies_loop(rest, [dependency.path, ..seen], [
            dependency,
            ..acc
          ])
      }
  }
}

pub fn render(
  template: String,
  issue: tracker_issue.Issue,
  attempt: Option(Int),
) -> Result(String, error.TemplateError) {
  render_with_locals(template, issue, attempt, [])
}

pub fn render_with_locals(
  template: String,
  issue: tracker_issue.Issue,
  attempt: Option(Int),
  locals: List(#(String, Value)),
) -> Result(String, error.TemplateError) {
  let template = case string.trim(template) {
    "" -> "You are working on an issue from Linear."
    _ -> template
  }
  render_section(
    template,
    Context(
      invocation: IssueInvocation(issue),
      attempt: attempt,
      locals: locals,
    ),
  )
}

pub fn render_scheduled(
  template: String,
  scheduled: ScheduledTemplateContext,
) -> Result(String, error.TemplateError) {
  render_scheduled_with_locals(template, scheduled, [])
}

pub fn render_scheduled_with_locals(
  template: String,
  scheduled: ScheduledTemplateContext,
  locals: List(#(String, Value)),
) -> Result(String, error.TemplateError) {
  let template = case string.trim(template) {
    "" -> "You are running a scheduled Scherzo job."
    _ -> template
  }
  render_section(
    template,
    Context(
      invocation: ScheduledInvocation(scheduled),
      attempt: Some(scheduled.attempt),
      locals: locals,
    ),
  )
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
        _ -> eval_invocation(expr, context.invocation)
      }
  }
}

fn eval_invocation(
  expr: String,
  invocation: InvocationContext,
) -> Result(Value, error.TemplateError) {
  case invocation {
    IssueInvocation(issue) -> eval_issue(expr, issue)
    ScheduledInvocation(scheduled) -> eval_scheduled(expr, scheduled)
  }
}

fn eval_issue(
  expr: String,
  issue: tracker_issue.Issue,
) -> Result(Value, error.TemplateError) {
  case expr {
    "issue.id" -> Ok(VString(issue.id))
    "issue.identifier" -> Ok(VString(issue.identifier))
    "issue.title" -> Ok(VString(issue.title))
    "issue.description" -> Ok(option_to_value(issue.description))
    "issue.priority" -> Ok(option_int_to_value(issue.priority))
    "issue.state" -> Ok(VString(issue_state.to_string(issue.state)))
    "issue.branch_name" -> Ok(option_to_value(issue.branch_name))
    "issue.url" -> Ok(option_to_value(issue.url))
    "issue.labels" -> Ok(VList(list.map(issue.labels, VString)))
    "issue.created_at" -> Ok(VNil)
    "issue.updated_at" -> Ok(VNil)
    _ -> Error(error.TemplateRenderError("unknown variable " <> expr))
  }
}

fn eval_scheduled(
  expr: String,
  scheduled: ScheduledTemplateContext,
) -> Result(Value, error.TemplateError) {
  case expr {
    "scheduled_job.id" -> Ok(VString(scheduled.job_id))
    "scheduled_job.workflow" -> Ok(VString(scheduled.workflow_id))
    "schedule.due_at" -> Ok(VString(scheduled.due_at))
    "schedule.started_at" -> Ok(VString(scheduled.started_at))
    "run.id" -> Ok(VString(scheduled.run_id))
    "run.attempt" -> Ok(VInt(scheduled.attempt))
    _ -> Error(error.TemplateRenderError("unknown variable " <> expr))
  }
}

pub fn referenced_variables(template: String) -> List(String) {
  referenced_variables_loop(template, [])
  |> list.reverse
  |> dedupe_preserving_first
}

pub fn referenced_variables_with_includes(
  source: String,
  source_path: String,
  resolver: fn(String, String) -> Result(IncludeDependency, error.TemplateError),
) -> Result(List(String), error.TemplateError) {
  use expansion <- try_template(expand_includes(source, source_path, resolver))
  Ok(referenced_variables(expansion.contents))
}

fn referenced_variables_loop(
  source: String,
  acc: List(String),
) -> List(String) {
  case next_token(source) {
    None -> acc
    Some(#(_, token, after)) -> {
      let acc = case token.kind {
        Variable(expr) -> [reference_expr(expr), ..acc]
        Tag(tag) -> tag_references(tag, acc)
      }
      referenced_variables_loop(after, acc)
    }
  }
}

fn tag_references(tag: String, acc: List(String)) -> List(String) {
  let tag = string.trim(tag)
  case string.starts_with(tag, "if ") {
    True -> [reference_expr(string.drop_start(tag, 3)), ..acc]
    False ->
      case string.starts_with(tag, "for ") {
        True ->
          case string.split_once(string.drop_start(tag, 4), on: " in ") {
            Ok(#(_, expr)) -> [reference_expr(expr), ..acc]
            Error(_) -> acc
          }
        False -> acc
      }
  }
}

fn reference_expr(expr: String) -> String {
  case string.split_once(expr, on: "|") {
    Ok(#(left, _)) -> string.trim(left)
    Error(_) -> string.trim(expr)
  }
}

fn dedupe_preserving_first(values: List(String)) -> List(String) {
  dedupe_loop(values, []) |> list.reverse
}

fn dedupe_loop(values: List(String), acc: List(String)) -> List(String) {
  case values {
    [] -> acc
    [value, ..rest] ->
      case list.contains(acc, value) || value == "" {
        True -> dedupe_loop(rest, acc)
        False -> dedupe_loop(rest, [value, ..acc])
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
