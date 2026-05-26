import gleam/dict.{type Dict}

pub const context_name = "commands"

pub fn insert_status(
  statuses: Dict(String, status),
  id: String,
  status: status,
) -> Dict(String, status) {
  dict.insert(statuses, id, status)
}

pub fn command_receipt(
  receipts: Dict(String, receipt),
  comment_id: String,
  unseen: receipt,
) -> receipt {
  case dict.get(receipts, comment_id) {
    Ok(receipt) -> receipt
    Error(Nil) -> unseen
  }
}
