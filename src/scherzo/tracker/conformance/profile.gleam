import gleam/string

pub type ProfileName {
  TaskSourceProfile
}

pub type PackName {
  TaskSourcePack
  CommentsPack
  RemoteCommandsPack
  StateTransitionsPack
  RoutingMetadataPack
  HandoffPack
  ScheduledFailuresPack
  UnknownPack(name: String)
}

pub type Capability {
  TaskSourceCapability
  CommentsCreateCapability
  CommentsUpdateCapability
  CommentsAllowCreateFallbackCapability
  RemoteCommandsCapability
  StateTransitionsTransitionCapability
  StateTransitionsReasonCapability
  RoutingMetadataWorkflowLabelsCapability
  RoutingMetadataBlockerRefsCapability
  HandoffCapability
  ScheduledFailuresCapability
  UnknownCapability(name: String)
}

pub type AdapterOperation {
  TaskSourceFetchCandidates
  TaskSourceRefreshByRefs
  TaskSourceLookupByOperatorRef
  CommentsPostOrUpdate
  RemoteCommandsFetchEvents
  RemoteCommandsPostAck
  StateTransitionsTransition
  HandoffReport
  ScheduledFailuresPublish
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
    RemoteCommandsPack -> "remote_commands"
    StateTransitionsPack -> "state_transitions"
    RoutingMetadataPack -> "routing_metadata"
    HandoffPack -> "handoff"
    ScheduledFailuresPack -> "scheduled_failures"
    UnknownPack(name) -> name
  }
}

pub fn pack_name_from_string(value: String) -> PackName {
  case string.trim(value) {
    "task_source" -> TaskSourcePack
    "comments" -> CommentsPack
    "remote_commands" -> RemoteCommandsPack
    "state_transitions" -> StateTransitionsPack
    "routing_metadata" -> RoutingMetadataPack
    "handoff" -> HandoffPack
    "scheduled_failures" -> ScheduledFailuresPack
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
    RemoteCommandsPack -> [
      RemoteCommandsCapability,
      CommentsCreateCapability,
    ]
    StateTransitionsPack -> [StateTransitionsTransitionCapability]
    RoutingMetadataPack -> [
      RoutingMetadataWorkflowLabelsCapability,
      RoutingMetadataBlockerRefsCapability,
    ]
    HandoffPack -> [HandoffCapability]
    ScheduledFailuresPack -> [ScheduledFailuresCapability]
    UnknownPack(_) -> []
  }
}

pub fn capability_to_string(capability: Capability) -> String {
  case capability {
    TaskSourceCapability -> "task_source"
    CommentsCreateCapability -> "comments.create"
    CommentsUpdateCapability -> "comments.update"
    CommentsAllowCreateFallbackCapability -> "comments.allow_create_fallback"
    RemoteCommandsCapability -> "remote_commands"
    StateTransitionsTransitionCapability -> "state_transitions.transition"
    StateTransitionsReasonCapability -> "state_transitions.reason"
    RoutingMetadataWorkflowLabelsCapability ->
      "routing_metadata.workflow_labels"
    RoutingMetadataBlockerRefsCapability -> "routing_metadata.blocker_refs"
    HandoffCapability -> "handoff"
    ScheduledFailuresCapability -> "scheduled_failures"
    UnknownCapability(name) -> name
  }
}

pub fn capability_from_string(value: String) -> Capability {
  case string.trim(value) {
    "task_source" -> TaskSourceCapability
    "comments.create" -> CommentsCreateCapability
    "comments.update" -> CommentsUpdateCapability
    "comments.allow_create_fallback" -> CommentsAllowCreateFallbackCapability
    "remote_commands" -> RemoteCommandsCapability
    "state_transitions.transition" -> StateTransitionsTransitionCapability
    "state_transitions.reason" -> StateTransitionsReasonCapability
    "routing_metadata.workflow_labels" ->
      RoutingMetadataWorkflowLabelsCapability
    "routing_metadata.blocker_refs" -> RoutingMetadataBlockerRefsCapability
    "handoff" -> HandoffCapability
    "scheduled_failures" -> ScheduledFailuresCapability
    other -> UnknownCapability(other)
  }
}

pub fn operation_to_string(operation: AdapterOperation) -> String {
  case operation {
    TaskSourceFetchCandidates -> "task_source.fetch_candidates"
    TaskSourceRefreshByRefs -> "task_source.refresh_by_refs"
    TaskSourceLookupByOperatorRef -> "task_source.lookup_by_operator_ref"
    CommentsPostOrUpdate -> "comments.post_or_update"
    RemoteCommandsFetchEvents -> "remote_commands.fetch_events"
    RemoteCommandsPostAck -> "remote_commands.post_ack"
    StateTransitionsTransition -> "state_transitions.transition"
    HandoffReport -> "handoff.report"
    ScheduledFailuresPublish -> "scheduled_failures.publish"
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
    "remote_commands.fetch_events" -> RemoteCommandsFetchEvents
    "remote_commands.post_ack" -> RemoteCommandsPostAck
    "state_transitions.transition" -> StateTransitionsTransition
    "handoff.report" -> HandoffReport
    "scheduled_failures.publish" -> ScheduledFailuresPublish
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
        | RemoteCommandsFetchEvents
        | RemoteCommandsPostAck
        | StateTransitionsTransition
        | HandoffReport
        | ScheduledFailuresPublish -> True
      }
  }
}

pub fn operation_name_has_fixture_namespace(value: String) -> Bool {
  let trimmed = string.trim(value)
  string.starts_with(trimmed, "fixture.")
  || string.starts_with(trimmed, "probe.")
  || string.starts_with(trimmed, "hook.")
}
