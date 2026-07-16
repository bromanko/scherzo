import gleam/dynamic/decode
import gleam/http
import gleam/http/request as http_request
import gleam/httpc
import gleam/int
import gleam/json
import gleam/option.{type Option, None, Some}
import gleam/result
import scherzo/control/remote/credential_store
import scherzo/control/remote/url
import scherzo/http_client_proxy

pub type PairingSuccess {
  PairingSuccess(
    server_url: String,
    daemon_id: String,
    credential: credential_store.DaemonCredential,
  )
}

pub type PairingError {
  InvalidUrl(url.ValidationError)
  InvalidPairingToken
  ExpiredPairingToken
  PairingTokenAlreadyConsumed
  UnreachableServer
  UnexpectedStatus(Int)
  InvalidResponse
}

pub type HttpRequest {
  HttpRequest(url: String, json_body: String)
}

pub type HttpResponse {
  HttpResponse(status: Int, body: String)
}

pub type Dependencies {
  Dependencies(post_json: fn(HttpRequest) -> Result(HttpResponse, Nil))
}

pub fn default_dependencies() -> Dependencies {
  Dependencies(post_json: post_json)
}

pub fn exchange_pairing_token(
  server_url_value: String,
  pairing_token: String,
  daemon_id: String,
  allow_loopback: Bool,
  deps: Dependencies,
) -> Result(PairingSuccess, PairingError) {
  exchange_pairing_token_with_label(
    server_url_value,
    pairing_token,
    daemon_id,
    None,
    allow_loopback,
    deps,
  )
}

pub fn exchange_pairing_token_with_label(
  server_url_value: String,
  pairing_token: String,
  daemon_id: String,
  daemon_label: Option(String),
  allow_loopback: Bool,
  deps: Dependencies,
) -> Result(PairingSuccess, PairingError) {
  use validated <- result.try(
    url.validate_server_url(server_url_value, allow_loopback: allow_loopback)
    |> result.map_error(InvalidUrl),
  )
  let body = pairing_request_body(pairing_token, daemon_id, daemon_label)
  let request =
    HttpRequest(
      url: validated.base_url <> "/api/daemons/pairing-exchanges",
      json_body: body,
    )
  use response <- result.try(
    deps.post_json(request)
    |> result.map_error(fn(_) { UnreachableServer }),
  )
  case response.status {
    200 | 201 -> decode_success(validated.base_url, daemon_id, response.body)
    400 -> Error(InvalidPairingToken)
    401 | 403 -> Error(ExpiredPairingToken)
    409 -> Error(PairingTokenAlreadyConsumed)
    status -> Error(UnexpectedStatus(status))
  }
}

fn pairing_request_body(
  pairing_token: String,
  daemon_id: String,
  daemon_label: Option(String),
) -> String {
  let base_fields = [
    #("pairingToken", json.string(pairing_token)),
    #("daemonId", json.string(daemon_id)),
  ]
  let fields = case daemon_label {
    Some(label) -> [#("daemonLabel", json.string(label)), ..base_fields]
    None -> base_fields
  }
  fields |> json.object |> json.to_string
}

pub fn error_code(error: PairingError) -> String {
  case error {
    InvalidUrl(validation_error) -> url.error_code(validation_error)
    InvalidPairingToken -> "invalid_pairing_token"
    ExpiredPairingToken -> "expired_pairing_token"
    PairingTokenAlreadyConsumed -> "pairing_token_already_consumed"
    UnreachableServer -> "unreachable_server"
    UnexpectedStatus(_) -> "pairing_exchange_unexpected_status"
    InvalidResponse -> "pairing_exchange_invalid_response"
  }
}

pub fn error_message(error: PairingError) -> String {
  case error {
    InvalidUrl(validation_error) -> url.error_message(validation_error)
    InvalidPairingToken -> "the pairing token is invalid"
    ExpiredPairingToken -> "the pairing token has expired"
    PairingTokenAlreadyConsumed -> "the pairing token was already consumed"
    UnreachableServer -> "the UI server could not be reached"
    UnexpectedStatus(status) ->
      "the UI server returned unexpected status " <> int.to_string(status)
    InvalidResponse -> "the UI server pairing response was invalid"
  }
}

fn decode_success(
  server_url: String,
  daemon_id: String,
  body: String,
) -> Result(PairingSuccess, PairingError) {
  case json.parse(body, pairing_response_decoder()) {
    Ok(credential) ->
      Ok(PairingSuccess(
        server_url: server_url,
        daemon_id: daemon_id,
        credential: credential,
      ))
    Error(_) -> Error(InvalidResponse)
  }
}

fn pairing_response_decoder() -> decode.Decoder(
  credential_store.DaemonCredential,
) {
  use credential_id <- decode.optional_field(
    "credentialId",
    None,
    decode.optional(decode.string),
  )
  use secret <- decode.field("credential", decode.string)
  decode.success(credential_store.DaemonCredential(
    credential_id: credential_id,
    secret: secret,
  ))
}

fn post_json(request: HttpRequest) -> Result(HttpResponse, Nil) {
  use Nil <- result.try(
    http_client_proxy.configure_from_environment()
    |> result.replace_error(Nil),
  )
  use http_request <- result.try(http_request.to(request.url))
  let http_request =
    http_request
    |> http_request.set_method(http.Post)
    |> http_request.set_body(request.json_body)
    |> http_request.set_header("content-type", "application/json")
    |> http_request.set_header("accept", "application/json")
  case
    httpc.configure() |> httpc.timeout(5000) |> httpc.dispatch(http_request)
  {
    Ok(response) ->
      Ok(HttpResponse(status: response.status, body: response.body))
    Error(_) -> Error(Nil)
  }
}
