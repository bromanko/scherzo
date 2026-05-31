import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/json
import gleam/list
import scherzo/control/query/types

pub fn status_from_source(source: types.StatusSource) -> types.StatusDto {
  let types.StatusSource(
    daemon_id: daemon_id,
    boot_id: boot_id,
    dispatch_paused: dispatch_paused,
    ui_server_enabled: ui_server_enabled,
    supported_queries: supported_queries,
    ..,
  ) = source

  types.StatusDto(
    daemon_id: daemon_id,
    boot_id: boot_id,
    dispatch_paused: dispatch_paused,
    ui_server_enabled: ui_server_enabled,
    supported_queries: case list.is_empty(supported_queries) {
      True -> types.supported_queries()
      False -> supported_queries
    },
  )
}

pub fn status_to_json(status: types.StatusDto) -> json.Json {
  let types.StatusDto(
    daemon_id: daemon_id,
    boot_id: boot_id,
    dispatch_paused: dispatch_paused,
    ui_server_enabled: ui_server_enabled,
    supported_queries: supported_queries,
  ) = status

  json.object([
    #("daemon_id", json.string(daemon_id)),
    #("boot_id", json.string(boot_id)),
    #("dispatch_paused", json.bool(dispatch_paused)),
    #("ui_server_enabled", json.bool(ui_server_enabled)),
    #("supported_queries", json.array(supported_queries, of: json.string)),
  ])
}

pub fn decode_status_dynamic(
  value: Dynamic,
) -> Result(types.StatusDto, types.QueryError) {
  case decode.run(value, status_decoder()) {
    Ok(status) -> Ok(status)
    Error(_) ->
      Error(types.QueryError(
        types.QueryBackendFailed,
        "invalid status query payload",
      ))
  }
}

fn status_decoder() -> decode.Decoder(types.StatusDto) {
  use daemon_id <- decode.field("daemon_id", decode.string)
  use boot_id <- decode.field("boot_id", decode.string)
  use dispatch_paused <- decode.field("dispatch_paused", decode.bool)
  use ui_server_enabled <- decode.field("ui_server_enabled", decode.bool)
  use supported_queries <- decode.field(
    "supported_queries",
    decode.list(decode.string),
  )
  decode.success(types.StatusDto(
    daemon_id: daemon_id,
    boot_id: boot_id,
    dispatch_paused: dispatch_paused,
    ui_server_enabled: ui_server_enabled,
    supported_queries: supported_queries,
  ))
}
