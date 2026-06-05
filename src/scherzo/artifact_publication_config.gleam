import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/template
import scherzo/workflow_contract
import yay

pub type PublicationConfigError {
  PublicationConfigError(code: String, message: String)
}

pub fn error_code(error: PublicationConfigError) -> String {
  let PublicationConfigError(code: code, ..) = error
  code
}

pub fn error_message(error: PublicationConfigError) -> String {
  let PublicationConfigError(message: message, ..) = error
  message
}

pub type ArtifactRepositories {
  ArtifactRepositories(github: Dict(String, GithubRepositoryTarget))
}

pub fn empty_repositories() -> ArtifactRepositories {
  ArtifactRepositories(github: dict.new())
}

pub type GithubRepositoryTarget {
  GithubRepositoryTarget(
    name: String,
    repo: String,
    base: String,
    checkout: GithubCheckoutConfig,
    branch: GithubBranchConfig,
    pull_request: GithubPullRequestConfig,
  )
}

pub type GithubCheckoutConfig {
  GithubCheckoutConfig(strategy: GithubCheckoutStrategy)
}

pub type GithubCheckoutStrategy {
  ManagedGit
}

pub type GithubBranchConfig {
  GithubBranchConfig(strategy: GithubBranchStrategy, template: String)
}

pub type GithubBranchStrategy {
  StablePerWork
}

pub type GithubPullRequestConfig {
  GithubPullRequestConfig(
    enabled: Bool,
    strategy: GithubPullRequestStrategy,
    draft: Bool,
    title: Option(String),
    body_template: Option(String),
  )
}

pub type GithubPullRequestStrategy {
  UpdateExisting
}

pub type PublicationMode {
  FilesPublication
  CommitStackPublication
}

pub type PublicationRoute {
  PublicationRoute(
    id: String,
    repository: String,
    required: Bool,
    mode: PublicationMode,
    pull_request: Option(PublicationPullRequestOverride),
    files: List(PublicationFileRoute),
    commit_stack: Option(PublicationCommitStackRoute),
  )
}

pub type PublicationCommitStackRoute {
  PublicationCommitStackRoute(selector: PublicationFileSelector)
}

pub type PublicationPullRequestOverride {
  PublicationPullRequestOverride(
    title: Option(String),
    body_template: Option(String),
  )
}

pub type PublicationFileRoute {
  PublicationFileRoute(selector: PublicationFileSelector, path: String)
}

pub type PublicationFileSelector {
  PublicationFileSelector(output: String, entry: Option(String))
}

const default_branch_template = "scherzo/{{ workflow.id }}/{{ work.identifier }}/{{ publication.id }}"

pub fn parse_root_repositories(
  root: yay.Node,
) -> Result(ArtifactRepositories, PublicationConfigError) {
  use artifacts <- result.try(get_map_or_empty(root, "artifacts", "artifacts"))
  use repositories <- result.try(get_map_or_empty(
    artifacts,
    "repositories",
    "artifacts.repositories",
  ))
  case repositories {
    yay.NodeMap([]) -> Ok(empty_repositories())
    yay.NodeMap(_) -> {
      use entries <- result.try(read_map_entries(
        repositories,
        "artifacts.repositories",
      ))
      parse_repository_backends(entries, empty_repositories())
    }
    _ ->
      error(
        "artifact_repositories_not_map",
        "artifacts.repositories must be a map",
      )
  }
}

fn parse_repository_backends(
  entries: List(#(String, yay.Node)),
  acc: ArtifactRepositories,
) -> Result(ArtifactRepositories, PublicationConfigError) {
  case entries {
    [] -> Ok(acc)
    [#("github", node), ..rest] -> {
      use github <- result.try(parse_github_repositories(node))
      parse_repository_backends(rest, ArtifactRepositories(github: github))
    }
    [#(backend, _), ..] ->
      error(
        "unsupported_artifact_repository_backend",
        "artifacts.repositories." <> backend <> " is not supported",
      )
  }
}

fn parse_github_repositories(
  node: yay.Node,
) -> Result(Dict(String, GithubRepositoryTarget), PublicationConfigError) {
  use entries <- result.try(read_map_entries(
    node,
    "artifacts.repositories.github",
  ))
  parse_github_repository_entries(entries, dict.new())
}

fn parse_github_repository_entries(
  entries: List(#(String, yay.Node)),
  acc: Dict(String, GithubRepositoryTarget),
) -> Result(Dict(String, GithubRepositoryTarget), PublicationConfigError) {
  case entries {
    [] -> Ok(acc)
    [#(name, node), ..rest] -> {
      use _ <- result.try(validate_repository_name(
        name,
        "artifacts.repositories.github",
      ))
      use target <- result.try(parse_github_repository_target(name, node))
      parse_github_repository_entries(rest, dict.insert(acc, name, target))
    }
  }
}

fn parse_github_repository_target(
  name: String,
  node: yay.Node,
) -> Result(GithubRepositoryTarget, PublicationConfigError) {
  let path = "artifacts.repositories.github." <> name
  use entries <- result.try(read_map_entries(node, path))
  use _ <- result.try(require_only_keys(
    entries,
    ["repo", "base", "checkout", "branch", "pull_request", "draft_pr"],
    path,
  ))
  use _ <- result.try(reject_legacy_draft_pr(entries, path))
  use repo <- result.try(required_string_entry(entries, "repo", path <> ".repo"))
  use _ <- result.try(validate_github_repo(repo, path <> ".repo"))
  use base <- result.try(required_string_entry(entries, "base", path <> ".base"))
  use _ <- result.try(validate_non_empty(base, path <> ".base"))
  let checkout_node =
    unwrap_node(get_entry(entries, "checkout"), yay.NodeMap([]))
  let branch_node = unwrap_node(get_entry(entries, "branch"), yay.NodeMap([]))
  let pull_request_node =
    unwrap_node(get_entry(entries, "pull_request"), yay.NodeMap([]))
  use checkout <- result.try(parse_checkout_config(
    checkout_node,
    path <> ".checkout",
  ))
  use branch <- result.try(parse_branch_config(branch_node, path <> ".branch"))
  use pull_request <- result.try(parse_pull_request_defaults(
    pull_request_node,
    path <> ".pull_request",
  ))
  Ok(GithubRepositoryTarget(
    name:,
    repo:,
    base:,
    checkout:,
    branch:,
    pull_request:,
  ))
}

fn reject_legacy_draft_pr(
  entries: List(#(String, yay.Node)),
  path: String,
) -> Result(Nil, PublicationConfigError) {
  case get_entry(entries, "draft_pr") {
    Some(_) ->
      error(
        "artifact_publication_legacy_draft_pr",
        path <> ".draft_pr was removed; use " <> path <> ".pull_request.draft",
      )
    None -> Ok(Nil)
  }
}

fn parse_checkout_config(
  node: yay.Node,
  path: String,
) -> Result(GithubCheckoutConfig, PublicationConfigError) {
  use entries <- result.try(read_map_entries(node, path))
  use _ <- result.try(require_only_keys(entries, ["strategy"], path))
  let strategy = case get_entry(entries, "strategy") {
    None -> Ok(ManagedGit)
    Some(yay.NodeStr("managed_git")) -> Ok(ManagedGit)
    Some(yay.NodeStr(other)) ->
      error(
        "invalid_artifact_repository_checkout_strategy",
        path <> ".strategy must be managed_git, got " <> other,
      )
    Some(_) ->
      error(
        "artifact_repository_checkout_strategy_not_string",
        path <> ".strategy must be a string",
      )
  }
  result.map(strategy, fn(strategy) { GithubCheckoutConfig(strategy:) })
}

fn parse_branch_config(
  node: yay.Node,
  path: String,
) -> Result(GithubBranchConfig, PublicationConfigError) {
  use entries <- result.try(read_map_entries(node, path))
  use _ <- result.try(require_only_keys(entries, ["strategy", "template"], path))
  use strategy <- result.try(parse_branch_strategy(
    get_entry(entries, "strategy"),
    path,
  ))
  use template <- result.try(case get_entry(entries, "template") {
    None -> Ok(default_branch_template)
    Some(yay.NodeStr(value)) -> Ok(value)
    Some(_) ->
      error(
        "artifact_repository_branch_template_not_string",
        path <> ".template must be a string",
      )
  })
  use _ <- result.try(validate_template_variables(
    template,
    branch_template_variables(),
    path <> ".template",
  ))
  use _ <- result.try(validate_relative_template_path(
    template,
    path <> ".template",
  ))
  Ok(GithubBranchConfig(strategy:, template:))
}

fn parse_branch_strategy(
  node: Option(yay.Node),
  path: String,
) -> Result(GithubBranchStrategy, PublicationConfigError) {
  case node {
    None -> Ok(StablePerWork)
    Some(yay.NodeStr("stable_per_work")) -> Ok(StablePerWork)
    Some(yay.NodeStr(other)) ->
      error(
        "invalid_artifact_repository_branch_strategy",
        path <> ".strategy must be stable_per_work, got " <> other,
      )
    Some(_) ->
      error(
        "artifact_repository_branch_strategy_not_string",
        path <> ".strategy must be a string",
      )
  }
}

fn parse_pull_request_defaults(
  node: yay.Node,
  path: String,
) -> Result(GithubPullRequestConfig, PublicationConfigError) {
  use entries <- result.try(read_map_entries(node, path))
  use _ <- result.try(require_only_keys(
    entries,
    ["enabled", "strategy", "draft", "title", "body_template"],
    path,
  ))
  use enabled <- result.try(optional_bool_entry(
    entries,
    "enabled",
    path <> ".enabled",
  ))
  use strategy <- result.try(parse_pull_request_strategy(
    get_entry(entries, "strategy"),
    path,
  ))
  use draft <- result.try(optional_bool_entry(
    entries,
    "draft",
    path <> ".draft",
  ))
  let title = case get_entry(entries, "title") {
    None -> Ok(None)
    Some(yay.NodeStr(value)) -> Ok(Some(value))
    Some(_) ->
      error(
        "artifact_repository_pull_request_title_not_string",
        path <> ".title must be a string",
      )
  }
  use title <- result.try(title)
  use _ <- result.try(validate_optional_template_variables(
    title,
    publication_template_variables(),
    path <> ".title",
  ))
  let body_template = case get_entry(entries, "body_template") {
    None -> Ok(None)
    Some(yay.NodeStr(value)) -> Ok(Some(value))
    Some(_) ->
      error(
        "artifact_repository_pull_request_body_template_not_string",
        path <> ".body_template must be a string",
      )
  }
  use body_template <- result.try(body_template)
  use _ <- result.try(validate_optional_relative_path(
    body_template,
    path <> ".body_template",
  ))
  Ok(GithubPullRequestConfig(
    enabled: unwrap_bool(enabled, True),
    strategy: strategy,
    draft: unwrap_bool(draft, False),
    title: title,
    body_template: body_template,
  ))
}

fn parse_pull_request_strategy(
  node: Option(yay.Node),
  path: String,
) -> Result(GithubPullRequestStrategy, PublicationConfigError) {
  case node {
    None -> Ok(UpdateExisting)
    Some(yay.NodeStr("update_existing")) -> Ok(UpdateExisting)
    Some(yay.NodeStr(other)) ->
      error(
        "invalid_artifact_repository_pull_request_strategy",
        path <> ".strategy must be update_existing, got " <> other,
      )
    Some(_) ->
      error(
        "artifact_repository_pull_request_strategy_not_string",
        path <> ".strategy must be a string",
      )
  }
}

pub fn parse_workflow_publications(
  root: yay.Node,
  contract: Option(workflow_contract.Contract),
) -> Result(List(PublicationRoute), PublicationConfigError) {
  use artifacts <- result.try(get_map_or_empty(root, "artifacts", "artifacts"))
  case get_node(artifacts, "publications") {
    None -> Ok([])
    Some(yay.NodeSeq([])) -> Ok([])
    Some(yay.NodeSeq(values)) ->
      case contract {
        None ->
          error(
            "missing_publication_contract",
            "artifacts.publications requires a workflow contract when routes are declared",
          )
        Some(_) -> parse_publication_routes(values, contract, [], [])
      }
    Some(_) ->
      error(
        "workflow_publications_not_list",
        "artifacts.publications must be a list",
      )
  }
}

fn parse_publication_routes(
  values: List(yay.Node),
  contract: Option(workflow_contract.Contract),
  seen_ids: List(String),
  acc: List(PublicationRoute),
) -> Result(List(PublicationRoute), PublicationConfigError) {
  case values {
    [] -> Ok(list.reverse(acc))
    [node, ..rest] -> {
      use route <- result.try(parse_publication_route(node, contract))
      case list.contains(seen_ids, route.id) {
        True ->
          error(
            "duplicate_publication_id",
            "duplicate publication id: " <> route.id,
          )
        False ->
          parse_publication_routes(rest, contract, [route.id, ..seen_ids], [
            route,
            ..acc
          ])
      }
    }
  }
}

fn parse_publication_route(
  node: yay.Node,
  contract: Option(workflow_contract.Contract),
) -> Result(PublicationRoute, PublicationConfigError) {
  use entries <- result.try(read_map_entries(node, "artifacts.publications[]"))
  use _ <- result.try(require_only_keys(
    entries,
    [
      "id",
      "repository",
      "required",
      "mode",
      "pull_request",
      "files",
      "commit_stack",
    ],
    "artifacts.publications[]",
  ))
  use id <- result.try(required_string_entry(
    entries,
    "id",
    "artifacts.publications[].id",
  ))
  use _ <- result.try(validate_publication_id(id))
  let repository_result = case get_entry(entries, "repository") {
    Some(yay.NodeStr(value)) -> Ok(value)
    Some(_) ->
      error(
        "publication_repository_not_string",
        "artifacts.publications[].repository must be a string",
      )
    None ->
      error(
        "missing_publication_repository",
        "artifacts.publications[].repository is required",
      )
  }
  use repository <- result.try(repository_result)
  use _ <- result.try(validate_repository_ref(
    repository,
    "artifacts.publications[].repository",
  ))
  use required <- result.try(optional_bool_entry(
    entries,
    "required",
    "artifacts.publications[].required",
  ))
  use mode <- result.try(parse_publication_mode(
    get_entry(entries, "mode"),
    "artifacts.publications[].mode",
  ))
  let pull_request_node = get_entry(entries, "pull_request")
  use pull_request <- result.try(parse_route_pull_request_override(
    pull_request_node,
    "artifacts.publications[].pull_request",
  ))
  let files_node = get_entry(entries, "files")
  let commit_stack_node = get_entry(entries, "commit_stack")
  use #(files, commit_stack) <- result.try(parse_publication_payload(
    mode,
    files_node,
    commit_stack_node,
    contract,
    id,
  ))
  Ok(PublicationRoute(
    id: id,
    repository: repository,
    required: unwrap_bool(required, True),
    mode: mode,
    pull_request: pull_request,
    files: files,
    commit_stack: commit_stack,
  ))
}

fn parse_publication_mode(
  node: Option(yay.Node),
  path: String,
) -> Result(PublicationMode, PublicationConfigError) {
  case node {
    None -> Ok(FilesPublication)
    Some(yay.NodeStr("files")) -> Ok(FilesPublication)
    Some(yay.NodeStr("commit_stack")) -> Ok(CommitStackPublication)
    Some(yay.NodeStr(other)) ->
      error(
        "invalid_publication_mode",
        path <> " must be files or commit_stack, got " <> other,
      )
    Some(_) -> error("publication_mode_not_string", path <> " must be a string")
  }
}

fn parse_publication_payload(
  mode: PublicationMode,
  files_node: Option(yay.Node),
  commit_stack_node: Option(yay.Node),
  contract: Option(workflow_contract.Contract),
  publication_id: String,
) -> Result(
  #(List(PublicationFileRoute), Option(PublicationCommitStackRoute)),
  PublicationConfigError,
) {
  case mode {
    FilesPublication -> {
      case commit_stack_node {
        Some(_) ->
          error(
            "publication_commit_stack_unexpected",
            "artifacts.publications[].commit_stack is only valid when mode is commit_stack",
          )
        None -> {
          use files <- result.try(parse_publication_files(
            files_node,
            contract,
            publication_id,
          ))
          Ok(#(files, None))
        }
      }
    }
    CommitStackPublication -> {
      case files_node {
        Some(_) ->
          error(
            "publication_files_unexpected",
            "artifacts.publications[].files is not valid when mode is commit_stack",
          )
        None -> {
          use commit_stack <- result.try(parse_publication_commit_stack(
            commit_stack_node,
            contract,
            publication_id,
          ))
          Ok(#([], Some(commit_stack)))
        }
      }
    }
  }
}

fn parse_publication_commit_stack(
  node: Option(yay.Node),
  contract: Option(workflow_contract.Contract),
  publication_id: String,
) -> Result(PublicationCommitStackRoute, PublicationConfigError) {
  case node {
    None ->
      error(
        "missing_publication_commit_stack",
        "artifacts.publications[].commit_stack is required when mode is commit_stack",
      )
    Some(node) -> {
      use entries <- result.try(read_map_entries(
        node,
        "artifacts.publications[].commit_stack",
      ))
      use _ <- result.try(require_only_keys(
        entries,
        ["select"],
        "artifacts.publications[].commit_stack",
      ))
      use selector <- result.try(parse_file_selector(
        get_entry(entries, "select"),
        contract,
        publication_id,
      ))
      use _ <- result.try(validate_commit_stack_selector(
        contract,
        selector.output,
        selector.entry,
        publication_id,
      ))
      Ok(PublicationCommitStackRoute(selector: selector))
    }
  }
}

fn parse_route_pull_request_override(
  node: Option(yay.Node),
  path: String,
) -> Result(Option(PublicationPullRequestOverride), PublicationConfigError) {
  case node {
    None -> Ok(None)
    Some(node) -> {
      use entries <- result.try(read_map_entries(node, path))
      use _ <- result.try(require_only_keys(
        entries,
        ["title", "body_template"],
        path,
      ))
      let title = case get_entry(entries, "title") {
        None -> Ok(None)
        Some(yay.NodeStr(value)) -> Ok(Some(value))
        Some(_) ->
          error(
            "publication_pull_request_title_not_string",
            path <> ".title must be a string",
          )
      }
      use title <- result.try(title)
      use _ <- result.try(validate_optional_template_variables(
        title,
        publication_template_variables(),
        path <> ".title",
      ))
      let body_template = case get_entry(entries, "body_template") {
        None -> Ok(None)
        Some(yay.NodeStr(value)) -> Ok(Some(value))
        Some(_) ->
          error(
            "publication_pull_request_body_template_not_string",
            path <> ".body_template must be a string",
          )
      }
      use body_template <- result.try(body_template)
      use _ <- result.try(validate_optional_relative_path(
        body_template,
        path <> ".body_template",
      ))
      Ok(Some(PublicationPullRequestOverride(title:, body_template:)))
    }
  }
}

fn parse_publication_files(
  node: Option(yay.Node),
  contract: Option(workflow_contract.Contract),
  publication_id: String,
) -> Result(List(PublicationFileRoute), PublicationConfigError) {
  case node {
    None ->
      error(
        "missing_publication_files",
        "artifacts.publications[].files is required",
      )
    Some(yay.NodeSeq([])) ->
      error(
        "publication_files_empty",
        "artifacts.publications[].files must not be empty",
      )
    Some(yay.NodeSeq(values)) ->
      parse_publication_file_routes(values, contract, publication_id, [])
    Some(_) ->
      error(
        "publication_files_not_list",
        "artifacts.publications[].files must be a list",
      )
  }
}

fn parse_publication_file_routes(
  values: List(yay.Node),
  contract: Option(workflow_contract.Contract),
  publication_id: String,
  acc: List(PublicationFileRoute),
) -> Result(List(PublicationFileRoute), PublicationConfigError) {
  case values {
    [] -> Ok(list.reverse(acc))
    [node, ..rest] -> {
      use file_route <- result.try(parse_publication_file_route(
        node,
        contract,
        publication_id,
      ))
      parse_publication_file_routes(rest, contract, publication_id, [
        file_route,
        ..acc
      ])
    }
  }
}

fn parse_publication_file_route(
  node: yay.Node,
  contract: Option(workflow_contract.Contract),
  publication_id: String,
) -> Result(PublicationFileRoute, PublicationConfigError) {
  use entries <- result.try(read_map_entries(
    node,
    "artifacts.publications[].files[]",
  ))
  use _ <- result.try(require_only_keys(
    entries,
    ["select", "path"],
    "artifacts.publications[].files[]",
  ))
  use selector <- result.try(parse_file_selector(
    get_entry(entries, "select"),
    contract,
    publication_id,
  ))
  use path <- result.try(required_string_entry(
    entries,
    "path",
    "artifacts.publications[].files[].path",
  ))
  use _ <- result.try(validate_template_variables(
    path,
    file_path_template_variables(),
    "artifacts.publications[].files[].path",
  ))
  use _ <- result.try(validate_relative_template_path(
    path,
    "artifacts.publications[].files[].path",
  ))
  Ok(PublicationFileRoute(selector:, path:))
}

fn parse_file_selector(
  node: Option(yay.Node),
  contract: Option(workflow_contract.Contract),
  publication_id: String,
) -> Result(PublicationFileSelector, PublicationConfigError) {
  case node {
    None ->
      error(
        "missing_publication_file_selector",
        "artifacts.publications[].files[].select is required",
      )
    Some(node) -> {
      use entries <- result.try(read_map_entries(
        node,
        "artifacts.publications[].files[].select",
      ))
      use _ <- result.try(require_only_keys(
        entries,
        ["output", "entry"],
        "artifacts.publications[].files[].select",
      ))
      use output <- result.try(required_string_entry(
        entries,
        "output",
        "artifacts.publications[].files[].select.output",
      ))
      use _ <- result.try(validate_contract_name(
        output,
        "artifacts.publications[].files[].select.output",
      ))
      let entry = case get_entry(entries, "entry") {
        None -> Ok(None)
        Some(yay.NodeStr(value)) -> Ok(Some(value))
        Some(_) ->
          error(
            "publication_selector_entry_not_string",
            "artifacts.publications[].files[].select.entry must be a string",
          )
      }
      use entry <- result.try(entry)
      use _ <- result.try(validate_optional_contract_name(
        entry,
        "artifacts.publications[].files[].select.entry",
      ))
      use _ <- result.try(validate_selector_against_contract(
        contract,
        output,
        entry,
        publication_id,
      ))
      Ok(PublicationFileSelector(output:, entry:))
    }
  }
}

fn validate_selector_against_contract(
  contract: Option(workflow_contract.Contract),
  output: String,
  entry: Option(String),
  publication_id: String,
) -> Result(Nil, PublicationConfigError) {
  case contract {
    None ->
      error(
        "missing_publication_contract",
        "publication "
          <> publication_id
          <> " cannot select output "
          <> output
          <> " because the workflow has no contract",
      )
    Some(contract) ->
      case find_output(contract.outputs, output) {
        None ->
          error(
            "unknown_publication_output",
            "publication "
              <> publication_id
              <> " references unknown contract output "
              <> output,
          )
        Some(spec) ->
          case entry {
            None -> Ok(Nil)
            Some(entry_name) ->
              case aggregate_capable_output(spec.type_) {
                True -> Ok(Nil)
                False ->
                  error(
                    "publication_selector_entry_not_supported",
                    "publication "
                      <> publication_id
                      <> " output "
                      <> output
                      <> " does not support select.entry "
                      <> entry_name,
                  )
              }
          }
      }
  }
}

fn validate_commit_stack_selector(
  contract: Option(workflow_contract.Contract),
  output: String,
  entry: Option(String),
  publication_id: String,
) -> Result(Nil, PublicationConfigError) {
  case entry {
    Some(_) ->
      error(
        "publication_commit_stack_entry_not_supported",
        "publication "
          <> publication_id
          <> " commit_stack selector must not include select.entry",
      )
    None ->
      case contract {
        None ->
          error(
            "missing_publication_contract",
            "publication "
              <> publication_id
              <> " cannot select commit_stack output "
              <> output
              <> " because the workflow has no contract",
          )
        Some(contract) ->
          case find_output(contract.outputs, output) {
            Some(workflow_contract.OutputSpec(
              type_: workflow_contract.CommitStack,
              ..,
            )) -> Ok(Nil)
            Some(_) ->
              error(
                "publication_commit_stack_output_type_mismatch",
                "publication "
                  <> publication_id
                  <> " commit_stack selector output "
                  <> output
                  <> " must have contract type commit_stack",
              )
            None ->
              error(
                "unknown_publication_output",
                "publication "
                  <> publication_id
                  <> " references unknown contract output "
                  <> output,
              )
          }
      }
  }
}

fn aggregate_capable_output(type_: workflow_contract.ContractType) -> Bool {
  case type_ {
    workflow_contract.ArtifactList
    | workflow_contract.ExecPlanBundle
    | workflow_contract.CodeChangeBundle -> True
    _ -> False
  }
}

fn find_output(
  outputs: List(workflow_contract.OutputSpec),
  name: String,
) -> Option(workflow_contract.OutputSpec) {
  case outputs {
    [] -> None
    [output, ..rest] ->
      case output.name == name {
        True -> Some(output)
        False -> find_output(rest, name)
      }
  }
}

pub fn validate_repository_ref(
  value: String,
  path: String,
) -> Result(Nil, PublicationConfigError) {
  use #(backend, name) <- result.try(repository_ref_parts(value, path))
  use _ <- result.try(validate_repository_name(backend, path))
  validate_repository_name(name, path)
}

pub fn repository_ref_parts(
  value: String,
  path: String,
) -> Result(#(String, String), PublicationConfigError) {
  let value = string.trim(value)
  case string.split(value, on: ".") {
    [backend, name] if backend != "" && name != "" -> Ok(#(backend, name))
    _ ->
      error(
        "invalid_publication_repository_ref",
        path <> " must be shaped as <backend>.<name>",
      )
  }
}

fn validate_repository_name(
  value: String,
  path: String,
) -> Result(Nil, PublicationConfigError) {
  case workflow_contract.valid_contract_name(value) {
    True -> Ok(Nil)
    False ->
      error(
        "invalid_artifact_repository_name",
        path <> " has invalid name: " <> value,
      )
  }
}

fn validate_publication_id(
  value: String,
) -> Result(Nil, PublicationConfigError) {
  case workflow_contract.valid_contract_name(value) {
    True -> Ok(Nil)
    False ->
      error("invalid_publication_id", "invalid publication id: " <> value)
  }
}

fn validate_contract_name(
  value: String,
  path: String,
) -> Result(Nil, PublicationConfigError) {
  case workflow_contract.valid_contract_name(value) {
    True -> Ok(Nil)
    False ->
      error(
        "invalid_publication_contract_name",
        path <> " has invalid name: " <> value,
      )
  }
}

fn validate_optional_contract_name(
  value: Option(String),
  path: String,
) -> Result(Nil, PublicationConfigError) {
  case value {
    None -> Ok(Nil)
    Some(value) -> validate_contract_name(value, path)
  }
}

fn validate_github_repo(
  value: String,
  path: String,
) -> Result(Nil, PublicationConfigError) {
  let parts = string.split(string.trim(value), on: "/")
  case parts {
    [owner, repo] ->
      case string.trim(owner) != "" && string.trim(repo) != "" {
        True -> Ok(Nil)
        False ->
          error("invalid_github_repository", path <> " must be owner/repo")
      }
    _ -> error("invalid_github_repository", path <> " must be owner/repo")
  }
}

fn validate_non_empty(
  value: String,
  path: String,
) -> Result(Nil, PublicationConfigError) {
  case string.trim(value) == "" {
    True ->
      error("empty_publication_config_value", path <> " must be non-empty")
    False -> Ok(Nil)
  }
}

fn validate_template_variables(
  value: String,
  allowed: List(String),
  path: String,
) -> Result(Nil, PublicationConfigError) {
  use _ <- result.try(validate_interpolation_only_template(value, path))
  validate_template_variable_list(
    template.referenced_variables(value),
    allowed,
    path,
  )
}

fn validate_optional_template_variables(
  value: Option(String),
  allowed: List(String),
  path: String,
) -> Result(Nil, PublicationConfigError) {
  case value {
    None -> Ok(Nil)
    Some(value) -> validate_template_variables(value, allowed, path)
  }
}

fn validate_interpolation_only_template(
  value: String,
  path: String,
) -> Result(Nil, PublicationConfigError) {
  case string.contains(value, "{%") {
    True ->
      error(
        "unsupported_publication_template_syntax",
        path
          <> " must use interpolation variables only; control tags are not supported",
      )
    False -> Ok(Nil)
  }
}

fn validate_template_variable_list(
  variables: List(String),
  allowed: List(String),
  path: String,
) -> Result(Nil, PublicationConfigError) {
  case variables {
    [] -> Ok(Nil)
    [variable, ..rest] ->
      case list.contains(allowed, variable) {
        True -> validate_template_variable_list(rest, allowed, path)
        False ->
          error(
            "unknown_publication_template_variable",
            path <> " references unsupported template variable " <> variable,
          )
      }
  }
}

fn validate_relative_template_path(
  value: String,
  path: String,
) -> Result(Nil, PublicationConfigError) {
  use _ <- result.try(validate_non_empty(value, path))
  use _ <- result.try(validate_interpolation_only_template(value, path))
  case invalid_relative_path_reason(value) {
    Some(reason) -> error("unsafe_publication_path", path <> " " <> reason)
    None -> Ok(Nil)
  }
}

fn validate_optional_relative_path(
  value: Option(String),
  path: String,
) -> Result(Nil, PublicationConfigError) {
  case value {
    None -> Ok(Nil)
    Some(value) -> validate_relative_template_path(value, path)
  }
}

fn invalid_relative_path_reason(value: String) -> Option(String) {
  let trimmed = string.trim(value)
  case trimmed == "" {
    True -> Some("must be non-empty")
    False ->
      case string.starts_with(trimmed, "/") {
        True -> Some("must be repository-relative")
        False ->
          case has_parent_segment(trimmed) {
            True -> Some("must not contain ..")
            False ->
              case has_control_character(trimmed) {
                True -> Some("must not contain control characters")
                False -> None
              }
          }
      }
  }
}

fn has_parent_segment(value: String) -> Bool {
  value == ".."
  || string.starts_with(value, "../")
  || string.ends_with(value, "/..")
  || string.contains(value, "/../")
}

fn has_control_character(value: String) -> Bool {
  value
  |> string.to_graphemes
  |> list.any(fn(ch) { ch == "\n" || ch == "\r" || ch == "\t" })
}

fn branch_template_variables() -> List(String) {
  publication_template_variables()
}

fn publication_template_variables() -> List(String) {
  [
    "work.kind",
    "work.id",
    "work.identifier",
    "work.slug",
    "work.title",
    "issue.title",
    "workflow.id",
    "run.id",
    "publication.id",
    "publication.series_id",
    "publication.version_id",
    "repository.kind",
    "repository.id",
    "github.repo",
    "github.base",
  ]
}

fn file_path_template_variables() -> List(String) {
  list.append(publication_template_variables(), [
    "artifact.output",
    "artifact.entry",
    "artifact.name",
    "artifact.ref",
    "artifact.media_type",
    "artifact.artifact_type",
    "artifact.sha256",
    "artifact.sha256_short",
    "artifact.default_extension",
    "artifact.metadata.publication.destination_path",
  ])
}

fn get_map_or_empty(
  node: yay.Node,
  key: String,
  path: String,
) -> Result(yay.Node, PublicationConfigError) {
  case get_node(node, key) {
    None -> Ok(yay.NodeMap([]))
    Some(yay.NodeMap(_) as value) -> Ok(value)
    Some(_) -> error(path <> "_not_map", path <> " must be a map")
  }
}

fn read_map_entries(
  node: yay.Node,
  context: String,
) -> Result(List(#(String, yay.Node)), PublicationConfigError) {
  case node {
    yay.NodeMap(pairs) -> read_map_entry_list(pairs, context, [], [])
    _ -> error(context <> "_not_map", context <> " must be a map")
  }
}

fn read_map_entry_list(
  pairs: List(#(yay.Node, yay.Node)),
  context: String,
  seen: List(String),
  acc: List(#(String, yay.Node)),
) -> Result(List(#(String, yay.Node)), PublicationConfigError) {
  case pairs {
    [] -> Ok(list.reverse(acc))
    [#(yay.NodeStr(key), value), ..rest] ->
      case list.contains(seen, key) {
        True ->
          error(
            "duplicate_publication_config_key",
            context <> " contains duplicate key: " <> key,
          )
        False ->
          read_map_entry_list(rest, context, [key, ..seen], [
            #(key, value),
            ..acc
          ])
      }
    [#(_, _), ..] ->
      error(context <> "_key_not_string", context <> " keys must be strings")
  }
}

fn require_only_keys(
  entries: List(#(String, yay.Node)),
  allowed: List(String),
  path: String,
) -> Result(Nil, PublicationConfigError) {
  case entries {
    [] -> Ok(Nil)
    [#(key, _), ..rest] ->
      case list.contains(allowed, key) {
        True -> require_only_keys(rest, allowed, path)
        False ->
          error(
            path <> "_unsupported_key",
            path <> " contains unsupported key: " <> key,
          )
      }
  }
}

fn required_string_entry(
  entries: List(#(String, yay.Node)),
  key: String,
  path: String,
) -> Result(String, PublicationConfigError) {
  case get_entry(entries, key) {
    Some(yay.NodeStr(value)) -> Ok(value)
    Some(_) -> error(path <> "_not_string", path <> " must be a string")
    None -> error("missing_" <> key, path <> " is required")
  }
}

fn optional_bool_entry(
  entries: List(#(String, yay.Node)),
  key: String,
  path: String,
) -> Result(Option(Bool), PublicationConfigError) {
  case get_entry(entries, key) {
    None -> Ok(None)
    Some(yay.NodeBool(value)) -> Ok(Some(value))
    Some(_) -> error(path <> "_not_bool", path <> " must be a boolean")
  }
}

fn unwrap_node(value: Option(yay.Node), default: yay.Node) -> yay.Node {
  case value {
    Some(value) -> value
    None -> default
  }
}

fn unwrap_bool(value: Option(Bool), default: Bool) -> Bool {
  case value {
    Some(value) -> value
    None -> default
  }
}

fn get_entry(
  entries: List(#(String, yay.Node)),
  key: String,
) -> Option(yay.Node) {
  case entries {
    [] -> None
    [#(current, value), ..rest] ->
      case current == key {
        True -> Some(value)
        False -> get_entry(rest, key)
      }
  }
}

fn get_node(node: yay.Node, key: String) -> Option(yay.Node) {
  case node {
    yay.NodeMap(pairs) ->
      case list.key_find(pairs, yay.NodeStr(key)) {
        Ok(value) -> Some(value)
        Error(Nil) -> None
      }
    _ -> None
  }
}

fn error(code: String, message: String) -> Result(a, PublicationConfigError) {
  Error(PublicationConfigError(code:, message:))
}
