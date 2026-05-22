import gleam/string

pub type ProfileName {
  TaskSourceProfile
}

pub type PackName {
  TaskSourcePack
  CommentsPack
  StateTransitionsPack
  RoutingMetadataPack
  UnknownPack(name: String)
}

pub type Capability {
  TaskSourceCapability
  CommentsCreateCapability
  CommentsUpdateCapability
  CommentsAllowCreateFallbackCapability
  StateTransitionsTransitionCapability
  StateTransitionsReasonCapability
  RoutingMetadataWorkflowLabelsCapability
  RoutingMetadataBlockerRefsCapability
  UnknownCapability(name: String)
}

pub type AdapterOperation {
  TaskSourceFetchCandidates
  TaskSourceRefreshByRefs
  TaskSourceLookupByOperatorRef
  CommentsPostOrUpdate
  StateTransitionsTransition
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

pub fn pack_name_to_string(name: PackName) -> String {
  case name {
    TaskSourcePack -> "task_source"
    CommentsPack -> "comments"
    StateTransitionsPack -> "state_transitions"
    RoutingMetadataPack -> "routing_metadata"
    UnknownPack(name) -> name
  }
}

pub fn pack_name_from_string(value: String) -> PackName {
  case string.trim(value) {
    "task_source" -> TaskSourcePack
    "comments" -> CommentsPack
    "state_transitions" -> StateTransitionsPack
    "routing_metadata" -> RoutingMetadataPack
    other -> UnknownPack(other)
  }
}

pub fn default_requested_packs() -> List(PackName) {
  [TaskSourcePack]
}

pub fn required_capabilities_for_pack(pack: PackName) -> List(Capability) {
  case pack {
    TaskSourcePack -> [TaskSourceCapability]
    CommentsPack -> [
      CommentsCreateCapability,
      CommentsUpdateCapability,
      CommentsAllowCreateFallbackCapability,
    ]
    StateTransitionsPack -> [StateTransitionsTransitionCapability]
    RoutingMetadataPack -> [
      RoutingMetadataWorkflowLabelsCapability,
      RoutingMetadataBlockerRefsCapability,
    ]
    UnknownPack(_) -> []
  }
}

pub fn capability_to_string(capability: Capability) -> String {
  case capability {
    TaskSourceCapability -> "task_source"
    CommentsCreateCapability -> "comments.create"
    CommentsUpdateCapability -> "comments.update"
    CommentsAllowCreateFallbackCapability -> "comments.allow_create_fallback"
    StateTransitionsTransitionCapability -> "state_transitions.transition"
    StateTransitionsReasonCapability -> "state_transitions.reason"
    RoutingMetadataWorkflowLabelsCapability ->
      "routing_metadata.workflow_labels"
    RoutingMetadataBlockerRefsCapability -> "routing_metadata.blocker_refs"
    UnknownCapability(name) -> name
  }
}

pub fn capability_from_string(value: String) -> Capability {
  case string.trim(value) {
    "task_source" -> TaskSourceCapability
    "comments.create" -> CommentsCreateCapability
    "comments.update" -> CommentsUpdateCapability
    "comments.allow_create_fallback" -> CommentsAllowCreateFallbackCapability
    "state_transitions.transition" -> StateTransitionsTransitionCapability
    "state_transitions.reason" -> StateTransitionsReasonCapability
    "routing_metadata.workflow_labels" ->
      RoutingMetadataWorkflowLabelsCapability
    "routing_metadata.blocker_refs" -> RoutingMetadataBlockerRefsCapability
    other -> UnknownCapability(other)
  }
}

pub fn operation_to_string(operation: AdapterOperation) -> String {
  case operation {
    TaskSourceFetchCandidates -> "task_source.fetch_candidates"
    TaskSourceRefreshByRefs -> "task_source.refresh_by_refs"
    TaskSourceLookupByOperatorRef -> "task_source.lookup_by_operator_ref"
    CommentsPostOrUpdate -> "comments.post_or_update"
    StateTransitionsTransition -> "state_transitions.transition"
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
    "comments.post_or_update" -> CommentsPostOrUpdate
    "state_transitions.transition" -> StateTransitionsTransition
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
  case name {
    TaskSourceProfile ->
      case operation {
        FixtureNamespaceOperation(_) | UnknownAdapterOperation(_) -> False
        TaskSourceFetchCandidates
        | TaskSourceRefreshByRefs
        | TaskSourceLookupByOperatorRef
        | CommentsPostOrUpdate
        | StateTransitionsTransition -> True
      }
  }
}

pub fn operation_name_has_fixture_namespace(value: String) -> Bool {
  let trimmed = string.trim(value)
  string.starts_with(trimmed, "fixture.")
  || string.starts_with(trimmed, "probe.")
  || string.starts_with(trimmed, "hook.")
}
