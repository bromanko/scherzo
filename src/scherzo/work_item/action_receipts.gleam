import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/control/command
import scherzo/hash

pub const max_receipts = 256

pub type Receipt {
  Receipt(payload_hash: String, result: command.CommandResult)
}

pub fn empty() -> Dict(String, Receipt) {
  dict.new()
}

pub fn store(
  receipts: Dict(String, Receipt),
  request: command.WorkItemActionRequest,
  result: command.CommandResult,
) -> Dict(String, Receipt) {
  let receipts = case dict.size(receipts) >= max_receipts {
    True -> dict.new()
    False -> receipts
  }

  dict.insert(
    receipts,
    receipt_key(request),
    Receipt(payload_hash: payload_hash(request), result: result),
  )
}

pub fn receipt_key(request: command.WorkItemActionRequest) -> String {
  string.join(
    [
      request.action_id,
      request.target_kind,
      option_string(request.target_provider),
      request.target_id,
      request.idempotency_key,
    ],
    with: ":",
  )
}

pub fn payload_hash(request: command.WorkItemActionRequest) -> String {
  [
    request.action_id,
    request.action_instance_id,
    request.target_kind,
    option_string(request.target_provider),
    request.target_id,
    request.observed_fingerprint,
    request.idempotency_key,
    params_key(request.params),
  ]
  |> string.join(with: "|")
  |> hash.sha256_hex
}

fn params_key(params: List(#(String, String))) -> String {
  params
  |> list.map(fn(param) {
    let #(name, value) = param
    name <> "=" <> value
  })
  |> string.join(with: "&")
}

fn option_string(value: Option(String)) -> String {
  case value {
    Some(value) -> value
    None -> ""
  }
}
