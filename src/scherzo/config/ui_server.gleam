import gleam/option.{type Option, Some}
import scherzo/config/types

pub fn endpoint(config: types.UiServerConfig) -> Option(String) {
  case config {
    types.UiServerDisabled(endpoint: endpoint, ..) -> endpoint
    types.UiServerEnabled(endpoint: endpoint, ..) -> Some(endpoint)
  }
}

pub fn credential_ref(config: types.UiServerConfig) -> Option(String) {
  case config {
    types.UiServerDisabled(credential_ref: credential_ref, ..) -> credential_ref
    types.UiServerEnabled(credential_ref: credential_ref, ..) ->
      Some(credential_ref)
  }
}

pub fn daemon_label(config: types.UiServerConfig) -> Option(String) {
  case config {
    types.UiServerDisabled(daemon_label: daemon_label, ..) -> daemon_label
    types.UiServerEnabled(daemon_label: daemon_label, ..) -> daemon_label
  }
}

pub fn command_bridge_enabled(config: types.UiServerConfig) -> Bool {
  case config {
    types.UiServerDisabled(..) -> False
    types.UiServerEnabled(command_bridge_enabled: command_bridge_enabled, ..) ->
      command_bridge_enabled
  }
}
