import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/tracker/conformance/profile
import scherzo/tracker/conformance/types

pub fn validate_profile(
  name: profile.ProfileName,
  capabilities: List(profile.Capability),
  requested_packs: List(profile.PackName),
  operations: List(profile.AdapterOperation),
  retry_behavior: Option(types.RetryBehaviorConfig),
) -> Result(Nil, types.ManifestError) {
  use Nil <- result.try(validate_profile_name(name))
  use Nil <- result.try(validate_capabilities(capabilities))
  use Nil <- result.try(validate_requested_packs(
    requested_packs,
    capabilities,
    retry_behavior,
  ))
  validate_operations(name, requested_packs, operations)
}

fn validate_profile_name(
  name: profile.ProfileName,
) -> Result(Nil, types.ManifestError) {
  case name == profile.TaskSourceProfile {
    True -> Ok(Nil)
    False ->
      Error(types.ManifestError(
        "unknown_profile",
        "profile.name must be task_source",
      ))
  }
}

fn validate_capabilities(
  capabilities: List(profile.Capability),
) -> Result(Nil, types.ManifestError) {
  use Nil <- result.try(validate_known_capabilities(capabilities))
  use Nil <- result.try(validate_unique_capabilities(capabilities))
  case capability_in_list(capabilities, profile.TaskSourceCapability) {
    True -> Ok(Nil)
    False ->
      Error(types.ManifestError(
        "missing_capability",
        "profile.capabilities must include task_source",
      ))
  }
}

fn validate_known_capabilities(
  capabilities: List(profile.Capability),
) -> Result(Nil, types.ManifestError) {
  case capabilities {
    [] -> Ok(Nil)
    [profile.UnknownCapability(name), ..] ->
      Error(types.ManifestError(
        "unknown_capability",
        "profile.capabilities contains an unknown capability: " <> name,
      ))
    [_, ..rest] -> validate_known_capabilities(rest)
  }
}

fn validate_unique_capabilities(
  capabilities: List(profile.Capability),
) -> Result(Nil, types.ManifestError) {
  case capabilities {
    [] -> Ok(Nil)
    [capability, ..rest] ->
      case capability_in_list(rest, capability) {
        True ->
          Error(types.ManifestError(
            "duplicate_capability",
            "profile.capabilities must not contain duplicate capability: "
              <> profile.capability_to_string(capability),
          ))
        False -> validate_unique_capabilities(rest)
      }
  }
}

fn validate_requested_packs(
  requested_packs: List(profile.PackName),
  capabilities: List(profile.Capability),
  retry_behavior: Option(types.RetryBehaviorConfig),
) -> Result(Nil, types.ManifestError) {
  use Nil <- result.try(validate_known_requested_packs(requested_packs))
  use Nil <- result.try(validate_unique_requested_packs(requested_packs))
  use Nil <- result.try(
    case pack_in_list(requested_packs, profile.TaskSourcePack) {
      True -> Ok(Nil)
      False ->
        Error(types.ManifestError(
          "missing_requested_pack",
          "profile.requested_packs must include task_source",
        ))
    },
  )
  use Nil <- result.try(validate_requested_pack_capabilities(
    requested_packs,
    capabilities,
  ))
  validate_requested_pack_retry_behavior(requested_packs, retry_behavior)
}

fn validate_known_requested_packs(
  requested_packs: List(profile.PackName),
) -> Result(Nil, types.ManifestError) {
  case requested_packs {
    [] -> Ok(Nil)
    [profile.UnknownPack(name), ..] ->
      Error(types.ManifestError(
        "unknown_requested_pack",
        "profile.requested_packs contains an unknown pack: " <> name,
      ))
    [_, ..rest] -> validate_known_requested_packs(rest)
  }
}

fn validate_unique_requested_packs(
  requested_packs: List(profile.PackName),
) -> Result(Nil, types.ManifestError) {
  case requested_packs {
    [] -> Ok(Nil)
    [requested_pack, ..rest] ->
      case pack_in_list(rest, requested_pack) {
        True ->
          Error(types.ManifestError(
            "duplicate_requested_pack",
            "profile.requested_packs must not contain duplicate pack: "
              <> profile.pack_name_to_string(requested_pack),
          ))
        False -> validate_unique_requested_packs(rest)
      }
  }
}

fn validate_requested_pack_capabilities(
  requested_packs: List(profile.PackName),
  capabilities: List(profile.Capability),
) -> Result(Nil, types.ManifestError) {
  case requested_packs {
    [] -> Ok(Nil)
    [requested_pack, ..rest] -> {
      use Nil <- result.try(validate_single_requested_pack_capabilities(
        requested_pack,
        capabilities,
      ))
      validate_requested_pack_capabilities(rest, capabilities)
    }
  }
}

fn validate_single_requested_pack_capabilities(
  requested_pack: profile.PackName,
  capabilities: List(profile.Capability),
) -> Result(Nil, types.ManifestError) {
  case
    missing_required_capability(
      profile.required_capabilities_for_pack(requested_pack),
      capabilities,
    )
  {
    None -> Ok(Nil)
    Some(capability) ->
      Error(types.ManifestError(
        "missing_requested_pack_capability",
        "profile.requested_packs includes "
          <> profile.pack_name_to_string(requested_pack)
          <> " but profile.capabilities is missing "
          <> profile.capability_to_string(capability),
      ))
  }
}

fn missing_required_capability(
  required: List(profile.Capability),
  capabilities: List(profile.Capability),
) -> Option(profile.Capability) {
  case required {
    [] -> None
    [capability, ..rest] ->
      case capability_in_list(capabilities, capability) {
        True -> missing_required_capability(rest, capabilities)
        False -> Some(capability)
      }
  }
}

fn validate_requested_pack_retry_behavior(
  requested_packs: List(profile.PackName),
  retry_behavior: Option(types.RetryBehaviorConfig),
) -> Result(Nil, types.ManifestError) {
  use Nil <- result.try(require_retry_behavior_for_requested_pack(
    requested_packs,
    retry_behavior,
    profile.RemoteCommandsPack,
    "remote_command_ack",
  ))
  require_retry_behavior_for_requested_pack(
    requested_packs,
    retry_behavior,
    profile.HandoffPack,
    "handoff_report",
  )
}

pub fn validate_probe_requirements(
  requested_packs: List(profile.PackName),
  probes: List(types.ProbeConfig),
) -> Result(Nil, types.ManifestError) {
  use Nil <- result.try(require_handoff_probe(requested_packs, probes))
  require_scheduled_failures_probe(requested_packs, probes)
}

fn require_handoff_probe(
  requested_packs: List(profile.PackName),
  probes: List(types.ProbeConfig),
) -> Result(Nil, types.ManifestError) {
  case pack_in_list(requested_packs, profile.HandoffPack) && probes == [] {
    True ->
      Error(types.ManifestError(
        "missing_probe",
        "profile.requested_packs includes handoff but probes must include at least one backend-visibility check",
      ))
    False -> Ok(Nil)
  }
}

fn require_scheduled_failures_probe(
  requested_packs: List(profile.PackName),
  probes: List(types.ProbeConfig),
) -> Result(Nil, types.ManifestError) {
  case pack_in_list(requested_packs, profile.ScheduledFailuresPack) {
    False -> Ok(Nil)
    True ->
      case has_scheduled_failures_probe(probes) {
        True -> Ok(Nil)
        False ->
          Error(types.ManifestError(
            "missing_probe",
            "profile.requested_packs includes scheduled_failures but probes must include at least one scheduled-failures backend-visibility check",
          ))
      }
  }
}

fn has_scheduled_failures_probe(probes: List(types.ProbeConfig)) -> Bool {
  case probes {
    [] -> False
    [types.ProbeConfig(name: name, ..), ..rest] ->
      string.starts_with(name, "scheduled-failures")
      || has_scheduled_failures_probe(rest)
  }
}

fn require_retry_behavior_for_requested_pack(
  requested_packs: List(profile.PackName),
  retry_behavior: Option(types.RetryBehaviorConfig),
  requested_pack: profile.PackName,
  field_name: String,
) -> Result(Nil, types.ManifestError) {
  case pack_in_list(requested_packs, requested_pack) {
    False -> Ok(Nil)
    True ->
      case retry_behavior_present(retry_behavior, field_name) {
        True -> Ok(Nil)
        False ->
          Error(types.ManifestError(
            "missing_retry_behavior",
            "profile.requested_packs includes "
              <> profile.pack_name_to_string(requested_pack)
              <> " but profile.retry_behavior."
              <> field_name
              <> " is missing",
          ))
      }
  }
}

fn retry_behavior_present(
  retry_behavior: Option(types.RetryBehaviorConfig),
  field_name: String,
) -> Bool {
  case retry_behavior {
    None -> False
    Some(types.RetryBehaviorConfig(
      remote_command_ack: remote_command_ack,
      handoff_report: handoff_report,
    )) ->
      case field_name {
        "remote_command_ack" -> option_is_some(remote_command_ack)
        "handoff_report" -> option_is_some(handoff_report)
        _ -> False
      }
  }
}

fn option_is_some(value: Option(a)) -> Bool {
  case value {
    Some(_) -> True
    None -> False
  }
}

fn validate_operations(
  name: profile.ProfileName,
  requested_packs: List(profile.PackName),
  operations: List(profile.AdapterOperation),
) -> Result(Nil, types.ManifestError) {
  case operations {
    [] ->
      Error(types.ManifestError(
        "missing_operation",
        "profile.adapter_operations must list the required adapter operations",
      ))
    _ -> {
      use Nil <- result.try(validate_operation_list(name, operations))
      use Nil <- result.try(validate_unique_operations(operations))
      use Nil <- result.try(validate_required_operations(name, operations))
      validate_required_requested_pack_operations(requested_packs, operations)
    }
  }
}

fn validate_required_requested_pack_operations(
  requested_packs: List(profile.PackName),
  operations: List(profile.AdapterOperation),
) -> Result(Nil, types.ManifestError) {
  use Nil <- result.try(require_operation_for_requested_pack(
    requested_packs,
    operations,
    profile.CommentsPack,
    profile.CommentsPostOrUpdate,
  ))
  use Nil <- result.try(require_operation_for_requested_pack(
    requested_packs,
    operations,
    profile.RemoteCommandsPack,
    profile.RemoteCommandsFetchEvents,
  ))
  use Nil <- result.try(require_operation_for_requested_pack(
    requested_packs,
    operations,
    profile.RemoteCommandsPack,
    profile.RemoteCommandsPostAck,
  ))
  use Nil <- result.try(require_operation_for_requested_pack(
    requested_packs,
    operations,
    profile.StateTransitionsPack,
    profile.StateTransitionsTransition,
  ))
  use Nil <- result.try(require_operation_for_requested_pack(
    requested_packs,
    operations,
    profile.HandoffPack,
    profile.HandoffReport,
  ))
  require_operation_for_requested_pack(
    requested_packs,
    operations,
    profile.ScheduledFailuresPack,
    profile.ScheduledFailuresPublish,
  )
}

fn require_operation_for_requested_pack(
  requested_packs: List(profile.PackName),
  operations: List(profile.AdapterOperation),
  requested_pack: profile.PackName,
  operation: profile.AdapterOperation,
) -> Result(Nil, types.ManifestError) {
  case pack_in_list(requested_packs, requested_pack) {
    False -> Ok(Nil)
    True ->
      case operation_in_list(operations, operation) {
        True -> Ok(Nil)
        False ->
          Error(types.ManifestError(
            "missing_operation",
            "profile.adapter_operations must include "
              <> profile.operation_to_string(operation),
          ))
      }
  }
}

fn validate_operation_list(
  name: profile.ProfileName,
  operations: List(profile.AdapterOperation),
) -> Result(Nil, types.ManifestError) {
  case operations {
    [] -> Ok(Nil)
    [operation, ..rest] ->
      case operation {
        profile.FixtureNamespaceOperation(value) ->
          Error(types.ManifestError(
            "fixture_operation_disallowed",
            "profile.adapter_operations must not include fixture/probe/hook operations: "
              <> value,
          ))
        profile.UnknownAdapterOperation(value) ->
          Error(types.ManifestError(
            "unknown_operation",
            "profile.adapter_operations contains an unknown operation: "
              <> value,
          ))
        _ ->
          case profile.operation_is_allowed_for_profile(name, operation) {
            True -> validate_operation_list(name, rest)
            False ->
              Error(types.ManifestError(
                "unknown_operation",
                "profile.adapter_operations contains an unsupported adapter operation",
              ))
          }
      }
  }
}

fn validate_unique_operations(
  operations: List(profile.AdapterOperation),
) -> Result(Nil, types.ManifestError) {
  case operations {
    [] -> Ok(Nil)
    [operation, ..rest] ->
      case operation_in_list(rest, operation) {
        True ->
          Error(types.ManifestError(
            "duplicate_operation",
            "profile.adapter_operations must not contain duplicate operation: "
              <> profile.operation_to_string(operation),
          ))
        False -> validate_unique_operations(rest)
      }
  }
}

fn validate_required_operations(
  name: profile.ProfileName,
  operations: List(profile.AdapterOperation),
) -> Result(Nil, types.ManifestError) {
  case
    missing_required_operation(
      profile.profile_default_operations(name),
      operations,
    )
  {
    None -> Ok(Nil)
    Some(operation) ->
      Error(types.ManifestError(
        "missing_operation",
        "profile.adapter_operations must include "
          <> profile.operation_to_string(operation),
      ))
  }
}

fn missing_required_operation(
  required: List(profile.AdapterOperation),
  operations: List(profile.AdapterOperation),
) -> Option(profile.AdapterOperation) {
  case required {
    [] -> None
    [operation, ..rest] ->
      case operation_in_list(operations, operation) {
        True -> missing_required_operation(rest, operations)
        False -> Some(operation)
      }
  }
}

fn operation_in_list(
  operations: List(profile.AdapterOperation),
  target: profile.AdapterOperation,
) -> Bool {
  case operations {
    [] -> False
    [operation, ..rest] ->
      operation == target || operation_in_list(rest, target)
  }
}

fn capability_in_list(
  capabilities: List(profile.Capability),
  target: profile.Capability,
) -> Bool {
  case capabilities {
    [] -> False
    [capability, ..rest] ->
      capability == target || capability_in_list(rest, target)
  }
}

fn pack_in_list(
  requested_packs: List(profile.PackName),
  target: profile.PackName,
) -> Bool {
  case requested_packs {
    [] -> False
    [requested_pack, ..rest] ->
      requested_pack == target || pack_in_list(rest, target)
  }
}
