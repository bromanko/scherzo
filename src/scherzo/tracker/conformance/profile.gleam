import gleam/string

pub type ProfileName {
  TaskSourceProfile
}

pub type Capability {
  TaskSourceCapability
  UnknownCapability(name: String)
}

pub type AdapterOperation {
  TaskSourceFetchCandidates
  TaskSourceRefreshByRefs
  TaskSourceLookupByOperatorRef
  FixtureNamespaceOperation(name: String)
  UnknownAdapterOperation(name: String)
}

pub fn profile_name_to_string(name: ProfileName) -> String {
  case name {
    TaskSourceProfile -> "task_source"
  }
}

pub fn profile_name_from_string(value: String) -> Result(ProfileName, Nil) {
  case string.trim(value) {
    "task_source" -> Ok(TaskSourceProfile)
    _ -> Error(Nil)
  }
}

pub fn capability_to_string(capability: Capability) -> String {
  case capability {
    TaskSourceCapability -> "task_source"
    UnknownCapability(name) -> name
  }
}

pub fn capability_from_string(value: String) -> Capability {
  case string.trim(value) {
    "task_source" -> TaskSourceCapability
    other -> UnknownCapability(other)
  }
}

pub fn operation_to_string(operation: AdapterOperation) -> String {
  case operation {
    TaskSourceFetchCandidates -> "task_source.fetch_candidates"
    TaskSourceRefreshByRefs -> "task_source.refresh_by_refs"
    TaskSourceLookupByOperatorRef -> "task_source.lookup_by_operator_ref"
    FixtureNamespaceOperation(name) -> name
    UnknownAdapterOperation(name) -> name
  }
}

pub fn operation_from_string(value: String) -> AdapterOperation {
  let trimmed = string.trim(value)
  case trimmed {
    "task_source.fetch_candidates" -> TaskSourceFetchCandidates
    "task_source.refresh_by_refs" -> TaskSourceRefreshByRefs
    "task_source.lookup_by_operator_ref" -> TaskSourceLookupByOperatorRef
    _ ->
      case operation_name_has_fixture_namespace(trimmed) {
        True -> FixtureNamespaceOperation(trimmed)
        False -> UnknownAdapterOperation(trimmed)
      }
  }
}

pub fn profile_default_operations(name: ProfileName) -> List(AdapterOperation) {
  case name {
    TaskSourceProfile -> [
      TaskSourceFetchCandidates,
      TaskSourceRefreshByRefs,
      TaskSourceLookupByOperatorRef,
    ]
  }
}

pub fn operation_is_allowed_for_profile(
  name: ProfileName,
  operation: AdapterOperation,
) -> Bool {
  case operation {
    FixtureNamespaceOperation(_) | UnknownAdapterOperation(_) -> False
    _ -> list_contains(profile_default_operations(name), operation)
  }
}

pub fn operation_name_has_fixture_namespace(value: String) -> Bool {
  let trimmed = string.trim(value)
  string.starts_with(trimmed, "fixture.")
  || string.starts_with(trimmed, "probe.")
  || string.starts_with(trimmed, "hook.")
}

fn list_contains(
  values: List(AdapterOperation),
  target: AdapterOperation,
) -> Bool {
  case values {
    [] -> False
    [value, ..rest] -> value == target || list_contains(rest, target)
  }
}
