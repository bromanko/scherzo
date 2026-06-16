import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/config/types as config_types
import scherzo/error
import scherzo/linear_contract

pub fn query(scope: config_types.LinearTaskScope) -> String {
  "query ScherzoLinearContract("
  <> linear_contract.task_scope_project_filter_declaration("projectFilter")
  <> ", $configuredProjectSlugs: [String!]!) { projects(first: "
  <> linear_contract.task_scope_contract_project_first(scope)
  <> ", filter: $projectFilter) { nodes { id name slugId teams(first: 10) { nodes { id key name states(first: 50) { nodes { id name type } pageInfo { hasNextPage endCursor } } labels(first: 140) { nodes { id name } pageInfo { hasNextPage endCursor } } } pageInfo { hasNextPage endCursor } } } } configuredProjects: projects(first: "
  <> linear_contract.task_scope_contract_configured_project_first(scope)
  <> ", filter: { slugId: { in: $configuredProjectSlugs } }) { nodes { slugId } } issueLabels(first: 100, filter: { team: { null: true } }) { nodes { id name } pageInfo { hasNextPage endCursor } } }"
}

pub fn parse_response(
  status: Int,
  body: String,
) -> Result(linear_contract.RemoteBoard, error.TrackerError) {
  case status == 200 {
    False -> Error(error.LinearApiStatus(status))
    True ->
      case json.parse(body, contract_graphql_decoder()) {
        Ok(Ok(raw_data)) -> raw_contract_to_board(raw_data)
        Ok(Error(message)) -> Error(error.LinearGraphqlErrors(message))
        Error(_) -> Error(error.LinearUnknownPayload("invalid JSON payload"))
      }
  }
}

pub fn parse_response_for_scope(
  status: Int,
  body: String,
  scope: config_types.LinearTaskScope,
) -> Result(linear_contract.RemoteBoard, error.TrackerError) {
  case status == 200 {
    False -> Error(error.LinearApiStatus(status))
    True ->
      case json.parse(body, contract_graphql_decoder()) {
        Ok(Ok(raw_data)) -> raw_contract_to_board_for_scope(raw_data, scope)
        Ok(Error(message)) -> Error(error.LinearGraphqlErrors(message))
        Error(_) -> Error(error.LinearUnknownPayload("invalid JSON payload"))
      }
  }
}

type RawContractData {
  RawContractData(
    projects: List(RawProject),
    configured_project_slugs: Option(List(String)),
    workspace_labels: RawConnection(RawLabel),
  )
}

type RawProject {
  RawProject(
    id: String,
    name: String,
    slug_id: String,
    teams: RawConnection(RawTeam),
  )
}

type RawTeam {
  RawTeam(
    id: String,
    key: String,
    name: String,
    states: RawConnection(RawState),
    labels: RawConnection(RawLabel),
  )
}

type RawState {
  RawState(id: String, name: String, type_: String)
}

type RawLabel {
  RawLabel(id: String, name: String)
}

type RawConnection(a) {
  RawConnection(nodes: List(a), page_info: PageInfo)
}

type PageInfo {
  PageInfo(has_next_page: Bool, end_cursor: Option(String))
}

fn contract_graphql_decoder() -> decode.Decoder(Result(RawContractData, String)) {
  use errors <- decode.optional_field(
    "errors",
    [],
    decode.list(error_message_decoder()),
  )
  case errors {
    [] -> {
      use data <- decode.field("data", contract_data_decoder())
      decode.success(Ok(data))
    }
    errors -> decode.success(Error(string.join(errors, with: "; ")))
  }
}

fn contract_data_decoder() -> decode.Decoder(RawContractData) {
  use projects <- decode.field("projects", raw_project_nodes_decoder())
  use configured_project_slugs <- decode.optional_field(
    "configuredProjects",
    None,
    decode.optional(configured_project_slugs_decoder()),
  )
  use workspace_labels <- decode.field(
    "issueLabels",
    raw_label_connection_decoder(),
  )
  decode.success(RawContractData(
    projects: projects,
    configured_project_slugs: configured_project_slugs,
    workspace_labels: workspace_labels,
  ))
}

fn configured_project_slugs_decoder() -> decode.Decoder(List(String)) {
  use nodes <- decode.field(
    "nodes",
    decode.list(configured_project_slug_decoder()),
  )
  decode.success(nodes)
}

fn configured_project_slug_decoder() -> decode.Decoder(String) {
  use slug <- decode.field("slugId", decode.string)
  decode.success(slug)
}

fn raw_project_nodes_decoder() -> decode.Decoder(List(RawProject)) {
  use nodes <- decode.field("nodes", decode.list(raw_project_decoder()))
  decode.success(nodes)
}

fn raw_project_decoder() -> decode.Decoder(RawProject) {
  use id <- decode.field("id", decode.string)
  use name <- decode.field("name", decode.string)
  use slug_id <- decode.field("slugId", decode.string)
  use teams <- decode.field("teams", raw_team_connection_decoder())
  decode.success(RawProject(id: id, name: name, slug_id: slug_id, teams: teams))
}

fn raw_team_connection_decoder() -> decode.Decoder(RawConnection(RawTeam)) {
  use nodes <- decode.field("nodes", decode.list(raw_team_decoder()))
  use page_info <- decode.field("pageInfo", page_info_decoder())
  decode.success(RawConnection(nodes: nodes, page_info: page_info))
}

fn raw_team_decoder() -> decode.Decoder(RawTeam) {
  use id <- decode.field("id", decode.string)
  use key <- decode.field("key", decode.string)
  use name <- decode.field("name", decode.string)
  use states <- decode.field("states", raw_state_connection_decoder())
  use labels <- decode.field("labels", raw_label_connection_decoder())
  decode.success(RawTeam(
    id: id,
    key: key,
    name: name,
    states: states,
    labels: labels,
  ))
}

fn raw_state_connection_decoder() -> decode.Decoder(RawConnection(RawState)) {
  use nodes <- decode.field("nodes", decode.list(raw_state_decoder()))
  use page_info <- decode.field("pageInfo", page_info_decoder())
  decode.success(RawConnection(nodes: nodes, page_info: page_info))
}

fn raw_state_decoder() -> decode.Decoder(RawState) {
  use id <- decode.field("id", decode.string)
  use name <- decode.field("name", decode.string)
  use type_ <- decode.field("type", decode.string)
  decode.success(RawState(id: id, name: name, type_: type_))
}

fn raw_label_connection_decoder() -> decode.Decoder(RawConnection(RawLabel)) {
  use nodes <- decode.field("nodes", decode.list(raw_label_decoder()))
  use page_info <- decode.field("pageInfo", page_info_decoder())
  decode.success(RawConnection(nodes: nodes, page_info: page_info))
}

fn raw_label_decoder() -> decode.Decoder(RawLabel) {
  use id <- decode.field("id", decode.string)
  use name <- decode.field("name", decode.string)
  decode.success(RawLabel(id: id, name: name))
}

fn raw_contract_to_board(
  data: RawContractData,
) -> Result(linear_contract.RemoteBoard, error.TrackerError) {
  case data.projects {
    [] -> Error(error.LinearUnknownPayload("project slug not found"))
    [project] -> raw_project_to_board(project, data.workspace_labels)
    [_, ..] -> Error(error.LinearUnknownPayload("project slug is not unique"))
  }
}

fn raw_contract_to_board_for_scope(
  data: RawContractData,
  scope: config_types.LinearTaskScope,
) -> Result(linear_contract.RemoteBoard, error.TrackerError) {
  use _ <- try_tracker(validate_configured_project_slugs(data, scope))
  case scope {
    config_types.LinearTaskProject(_) -> raw_contract_to_board(data)
    config_types.LinearTaskProjects(_) -> {
      let expected_slugs = linear_contract.task_scope_project_slugs(scope)
      contract_projects_to_board(data, expected_slugs)
    }
    config_types.LinearTaskAllLabels(_)
    | config_types.LinearTaskAnyLabel(_)
    | config_types.LinearTaskAnd(_)
    | config_types.LinearTaskOr(_) -> {
      let expected_slugs =
        linear_contract.task_scope_matching_project_slugs(scope)
      contract_projects_to_board(data, expected_slugs)
    }
  }
}

fn validate_configured_project_slugs(
  data: RawContractData,
  scope: config_types.LinearTaskScope,
) -> Result(Nil, error.TrackerError) {
  let expected_slugs = linear_contract.task_scope_project_slugs(scope)
  let returned_slugs = configured_project_slugs_from_contract(data)
  case missing_project_slug_strings(expected_slugs, returned_slugs) {
    [] -> Ok(Nil)
    missing ->
      Error(error.LinearUnknownPayload(
        "project slug(s) not found: " <> string.join(missing, with: ", "),
      ))
  }
}

fn configured_project_slugs_from_contract(
  data: RawContractData,
) -> List(String) {
  case data.configured_project_slugs {
    Some(slugs) -> slugs
    None -> list.map(data.projects, fn(project) { project.slug_id })
  }
}

fn contract_projects_to_board(
  data: RawContractData,
  expected_slugs: List(String),
) -> Result(linear_contract.RemoteBoard, error.TrackerError) {
  case expected_slugs {
    [] ->
      Error(error.LinearUnknownPayload(
        "task scope does not match any configured project slug",
      ))
    _ -> {
      let matching_projects =
        list.filter_map(expected_slugs, fn(expected) {
          list.find(data.projects, fn(project) {
            linear_contract.task_scope_matches_project_slug(
              config_types.LinearTaskProject(expected),
              project.slug_id,
            )
          })
        })
      case missing_project_slugs(expected_slugs, matching_projects) {
        [] -> raw_projects_to_board(matching_projects, data.workspace_labels)
        missing ->
          Error(error.LinearUnknownPayload(
            "project slug(s) not found: " <> string.join(missing, with: ", "),
          ))
      }
    }
  }
}

fn missing_project_slugs(
  expected_slugs: List(String),
  projects: List(RawProject),
) -> List(String) {
  list.filter(expected_slugs, fn(expected) {
    !list.any(projects, fn(project) {
      linear_contract.task_scope_matches_project_slug(
        config_types.LinearTaskProject(expected),
        project.slug_id,
      )
    })
  })
}

fn missing_project_slug_strings(
  expected_slugs: List(String),
  returned_slugs: List(String),
) -> List(String) {
  list.filter(expected_slugs, fn(expected) {
    !list.any(returned_slugs, fn(returned) {
      linear_contract.task_scope_matches_project_slug(
        config_types.LinearTaskProject(expected),
        returned,
      )
    })
  })
}

fn raw_projects_to_board(
  projects: List(RawProject),
  workspace_labels: RawConnection(RawLabel),
) -> Result(linear_contract.RemoteBoard, error.TrackerError) {
  case projects {
    [] -> Error(error.LinearUnknownPayload("project slug not found"))
    [project] -> raw_project_to_board(project, workspace_labels)
    [_, ..] -> {
      use boards <- try_tracker(
        raw_projects_to_boards(projects, workspace_labels, []),
      )
      case boards {
        [] -> Error(error.LinearUnknownPayload("project slug not found"))
        [primary_board, ..] ->
          Ok(linear_contract.RemoteBoard(
            project_id: primary_board.project_id,
            project_slug: string.join(
              list.map(boards, fn(board) { board.project_slug }),
              with: ",",
            ),
            project_name: string.join(
              list.map(boards, fn(board) { board.project_name }),
              with: ",",
            ),
            teams: collect_board_teams(boards),
            workspace_labels: primary_board.workspace_labels,
          ))
      }
    }
  }
}

fn raw_projects_to_boards(
  projects: List(RawProject),
  workspace_labels: RawConnection(RawLabel),
  acc: List(linear_contract.RemoteBoard),
) -> Result(List(linear_contract.RemoteBoard), error.TrackerError) {
  case projects {
    [] -> Ok(list.reverse(acc))
    [project, ..rest] -> {
      use board <- try_tracker(raw_project_to_board(project, workspace_labels))
      raw_projects_to_boards(rest, workspace_labels, [board, ..acc])
    }
  }
}

fn collect_board_teams(
  boards: List(linear_contract.RemoteBoard),
) -> List(linear_contract.RemoteTeam) {
  boards
  |> list.map(fn(board) { board.teams })
  |> list.flatten
  |> collect_unique_teams([])
}

fn collect_unique_teams(
  teams: List(linear_contract.RemoteTeam),
  acc: List(linear_contract.RemoteTeam),
) -> List(linear_contract.RemoteTeam) {
  case teams {
    [] -> list.reverse(acc)
    [team, ..rest] ->
      case list.any(acc, fn(existing) { existing.id == team.id }) {
        True -> collect_unique_teams(rest, acc)
        False -> collect_unique_teams(rest, [team, ..acc])
      }
  }
}

fn raw_project_to_board(
  project: RawProject,
  workspace_labels: RawConnection(RawLabel),
) -> Result(linear_contract.RemoteBoard, error.TrackerError) {
  case project.teams.page_info.has_next_page {
    True ->
      Error(error.LinearUnknownPayload("project teams metadata truncated"))
    False ->
      case list.is_empty(project.teams.nodes) {
        True -> Error(error.LinearUnknownPayload("project has no teams"))
        False ->
          case workspace_labels.page_info.has_next_page {
            True ->
              Error(error.LinearUnknownPayload(
                "workspace issue labels metadata truncated",
              ))
            False -> {
              use teams <- try_tracker(
                raw_teams_to_remote(project.teams.nodes, []),
              )
              Ok(linear_contract.RemoteBoard(
                project_id: project.id,
                project_slug: project.slug_id,
                project_name: project.name,
                teams: teams,
                workspace_labels: raw_labels_to_remote(workspace_labels.nodes),
              ))
            }
          }
      }
  }
}

fn raw_teams_to_remote(
  teams: List(RawTeam),
  acc: List(linear_contract.RemoteTeam),
) -> Result(List(linear_contract.RemoteTeam), error.TrackerError) {
  case teams {
    [] -> Ok(list.reverse(acc))
    [team, ..rest] -> {
      use remote <- try_tracker(raw_team_to_remote(team))
      raw_teams_to_remote(rest, [remote, ..acc])
    }
  }
}

fn raw_team_to_remote(
  team: RawTeam,
) -> Result(linear_contract.RemoteTeam, error.TrackerError) {
  case team.states.page_info.has_next_page {
    True ->
      Error(error.LinearUnknownPayload(
        "team " <> team.key <> " states metadata truncated",
      ))
    False ->
      case team.labels.page_info.has_next_page {
        True ->
          Error(error.LinearUnknownPayload(
            "team " <> team.key <> " labels metadata truncated",
          ))
        False ->
          Ok(linear_contract.RemoteTeam(
            id: team.id,
            key: team.key,
            name: team.name,
            states: raw_states_to_remote(team.states.nodes),
            labels: raw_labels_to_remote(team.labels.nodes),
          ))
      }
  }
}

fn raw_states_to_remote(
  states: List(RawState),
) -> List(linear_contract.RemoteState) {
  states
  |> list.map(fn(state) {
    linear_contract.RemoteState(
      id: state.id,
      name: state.name,
      type_: state.type_,
    )
  })
}

fn raw_labels_to_remote(
  labels: List(RawLabel),
) -> List(linear_contract.RemoteLabel) {
  labels
  |> list.map(fn(label) {
    linear_contract.RemoteLabel(id: label.id, name: label.name)
  })
}

fn error_message_decoder() -> decode.Decoder(String) {
  use message <- decode.field("message", decode.string)
  decode.success(message)
}

fn page_info_decoder() -> decode.Decoder(PageInfo) {
  use has_next_page <- decode.field("hasNextPage", decode.bool)
  use end_cursor <- decode.field("endCursor", decode.optional(decode.string))
  decode.success(PageInfo(has_next_page: has_next_page, end_cursor: end_cursor))
}

fn try_tracker(
  result: Result(a, error.TrackerError),
  next: fn(a) -> Result(b, error.TrackerError),
) -> Result(b, error.TrackerError) {
  case result {
    Ok(value) -> next(value)
    Error(err) -> Error(err)
  }
}
