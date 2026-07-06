import gleam/list
import gleam/result
import gleam/string
import scherzo/error
import scherzo/path
import scherzo/template
import simplifile

pub type PromptLoadError {
  PromptLoadError(code: String, message: String)
}

pub type ResolvedPrompt {
  ResolvedPrompt(contents: String, dependencies: List(#(String, String)))
}

pub fn read_relative_prompt(
  prompt_path: String,
  workflow_path: String,
) -> Result(ResolvedPrompt, PromptLoadError) {
  case validate_relative_path(prompt_path, "invalid_prompt_path") {
    Error(PromptLoadError(code, message)) ->
      Error(PromptLoadError(code, message <> " in workflow " <> workflow_path))
    Ok(Nil) -> {
      let prompt_path = string.trim(prompt_path)
      use workflow_dir <- result.try(workflow_directory(workflow_path))
      use workflow_root <- result.try(workflow_root_path(
        workflow_dir,
        workflow_path,
      ))
      use full_path <- result.try(resolve_child_path(
        workflow_dir,
        workflow_root,
        prompt_path,
        workflow_path,
        "invalid_prompt_path",
        "prompt path",
      ))
      use contents <- result.try(read_file(full_path, "missing_prompt_file"))
      use expansion <- result.try(
        template.expand_includes(
          contents,
          full_path,
          fn(include_path, including_path) {
            resolve_prompt_include(
              include_path,
              including_path,
              workflow_path,
              prompt_path,
              workflow_root,
            )
          },
        )
        |> result.map_error(fn(err) {
          prompt_include_error(
            workflow_path,
            prompt_path,
            template_error_message(err),
          )
        }),
      )
      Ok(
        ResolvedPrompt(contents: expansion.contents, dependencies: [
          #(full_path, contents),
          ..list.map(expansion.dependencies, fn(dependency) {
            #(dependency.path, dependency.contents)
          })
        ]),
      )
    }
  }
}

fn resolve_prompt_include(
  include_path: String,
  including_path: String,
  workflow_path: String,
  root_prompt_path: String,
  workflow_root: String,
) -> Result(template.IncludeDependency, error.TemplateError) {
  case validate_relative_path(include_path, "prompt_include_error") {
    Error(PromptLoadError(_, message)) ->
      Error(
        error.TemplateRenderError(prompt_include_context(
          workflow_path,
          root_prompt_path,
          including_path,
          include_path,
          message,
        )),
      )
    Ok(Nil) -> {
      let include_path = string.trim(include_path)
      use including_dir <- result.try(
        path.dirname(including_path)
        |> result.replace_error(
          error.TemplateRenderError(prompt_include_context(
            workflow_path,
            root_prompt_path,
            including_path,
            include_path,
            "could not resolve including file directory",
          )),
        ),
      )
      use full_path <- result.try(
        resolve_child_path(
          including_dir,
          workflow_root,
          include_path,
          workflow_path,
          "prompt_include_error",
          "include path",
        )
        |> result.map_error(fn(load_error) {
          case load_error {
            PromptLoadError(_, message) ->
              error.TemplateRenderError(prompt_include_context(
                workflow_path,
                root_prompt_path,
                including_path,
                include_path,
                message,
              ))
          }
        }),
      )
      use contents <- result.try(
        simplifile.read(full_path)
        |> result.replace_error(
          error.TemplateRenderError(prompt_include_context(
            workflow_path,
            root_prompt_path,
            including_path,
            include_path,
            "could not read included prompt fragment " <> full_path,
          )),
        ),
      )
      Ok(template.IncludeDependency(path: full_path, contents: contents))
    }
  }
}

fn workflow_directory(
  workflow_path: String,
) -> Result(String, PromptLoadError) {
  path.dirname(workflow_path)
  |> result.replace_error(PromptLoadError(
    "invalid_prompt_path",
    "could not resolve workflow directory for " <> workflow_path,
  ))
}

fn workflow_root_path(
  workflow_dir: String,
  workflow_path: String,
) -> Result(String, PromptLoadError) {
  path.absolute(workflow_dir)
  |> result.replace_error(PromptLoadError(
    "invalid_prompt_path",
    "could not resolve workflow directory for " <> workflow_path,
  ))
  |> result.map(canonicalize_existing_path)
}

fn resolve_child_path(
  root_dir: String,
  workflow_root: String,
  child_path: String,
  workflow_path: String,
  code: String,
  label: String,
) -> Result(String, PromptLoadError) {
  let joined_path = path.join(root_dir, string.trim(child_path))
  use full_path <- result.try(
    path.absolute(joined_path)
    |> result.replace_error(PromptLoadError(
      code,
      "could not resolve "
        <> label
        <> " "
        <> child_path
        <> " in workflow "
        <> workflow_path,
    )),
  )
  let full_path = canonicalize_existing_path(full_path)
  case path.contains(workflow_root, full_path) {
    False ->
      Error(PromptLoadError(
        code,
        label <> " escapes workflow directory: " <> child_path,
      ))
    True -> Ok(full_path)
  }
}

fn prompt_include_error(
  workflow_path: String,
  prompt_path: String,
  message: String,
) -> PromptLoadError {
  PromptLoadError(
    "prompt_include_error",
    "workflow " <> workflow_path <> " prompt " <> prompt_path <> ": " <> message,
  )
}

fn prompt_include_context(
  workflow_path: String,
  root_prompt_path: String,
  including_path: String,
  include_path: String,
  message: String,
) -> String {
  "workflow "
  <> workflow_path
  <> " prompt "
  <> root_prompt_path
  <> " including "
  <> including_path
  <> " include "
  <> include_path
  <> ": "
  <> message
}

fn template_error_message(err: error.TemplateError) -> String {
  case err {
    error.TemplateRenderError(message) -> message
  }
}

fn canonicalize_existing_path(path_value: String) -> String {
  case path.realpath(path_value) {
    Ok(canonical) -> canonical
    Error(Nil) -> path_value
  }
}

fn validate_relative_path(
  value: String,
  code: String,
) -> Result(Nil, PromptLoadError) {
  let trimmed = string.trim(value)
  case trimmed == "" {
    True -> Error(PromptLoadError(code, "path must be non-empty"))
    False ->
      case
        path.is_absolute(trimmed)
        || path.has_parent_segment(trimmed)
        || path.contains_control_character(trimmed)
      {
        True ->
          Error(PromptLoadError(
            code,
            "path must be relative, must not contain .., and must not contain control characters",
          ))
        False -> Ok(Nil)
      }
  }
}

fn read_file(path: String, code: String) -> Result(String, PromptLoadError) {
  case simplifile.read(path) {
    Ok(content) -> Ok(content)
    Error(_) -> Error(PromptLoadError(code, "could not read " <> path))
  }
}
