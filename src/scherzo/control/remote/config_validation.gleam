import scherzo/control/remote/credential_store
import scherzo/control/remote/daemon_label

pub type CredentialRefValidationError {
  InvalidCredentialRef(String)
}

pub type DaemonLabelValidationError {
  InvalidDaemonLabel(daemon_label.ValidationError)
}

pub fn normalize_credential_ref(
  value: String,
) -> Result(String, CredentialRefValidationError) {
  case credential_store.normalize_credential_ref(value) {
    Ok(credential_store.CredentialRef(profile: profile)) -> Ok(profile)
    Error(message) -> Error(InvalidCredentialRef(message))
  }
}

pub fn credential_ref_error_message(
  error: CredentialRefValidationError,
) -> String {
  let InvalidCredentialRef(message) = error
  message
}

pub fn normalize_daemon_label(
  value: String,
) -> Result(String, DaemonLabelValidationError) {
  case daemon_label.normalize(value) {
    Ok(label) -> Ok(label)
    Error(validation_error) -> Error(InvalidDaemonLabel(validation_error))
  }
}

pub fn daemon_label_error_message(error: DaemonLabelValidationError) -> String {
  let InvalidDaemonLabel(validation_error) = error
  daemon_label.error_message(validation_error)
}
