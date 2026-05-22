import gleam/option.{type Option, None, Some}
import gleam/result
import scherzo/tracker/conformance/profile
import scherzo/tracker/conformance/types

pub fn validate_profile(
  name: profile.ProfileName,
  capabilities: List(profile.Capability),
  requested_packs: List(profile.PackName),
  operations: List(profile.AdapterOperation),
) -> Result(Nil, types.ManifestError) {
  use Nil <- result.try(validate_profile_name(name))
  use Nil <- result.try(validate_capabilities(capabilities))
  use Nil <- result.try(validate_requested_packs(requested_packs, capabilities))
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
  validate_requested_pack_capabilities(requested_packs, capabilities)
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
  require_operation_for_requested_pack(
    requested_packs,
    operations,
    profile.StateTransitionsPack,
    profile.StateTransitionsTransition,
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
