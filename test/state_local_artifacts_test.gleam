import gleam/json
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/session/event
import scherzo/state/ledger
import scherzo/state/local_artifacts
import scherzo/state/record
import simplifile

fn metadata(
  artifact_type: local_artifacts.ArtifactType,
  terminal_at_ms: Option(Int),
  recovery_status: Option(event.RecoveryStatus),
) -> local_artifacts.LocalArtifactMetadata {
  local_artifacts.LocalArtifactMetadata(
    artifact_type: artifact_type,
    id: "artifact-1",
    path: "test/tmp/artifact-1",
    owner_id: Some("owner-1"),
    terminal_at_ms: terminal_at_ms,
    recovery_status: recovery_status,
    schema_status: local_artifacts.SchemaCurrent,
    malformed: False,
  )
}

pub fn retention_classifier_keeps_ambiguous_and_recovery_hold_artifacts_test() {
  let now = local_artifacts.workflow_artifact_retention_ms + 10

  let terminal =
    local_artifacts.classify(
      metadata(local_artifacts.WorkflowArtifact, Some(0), Some(event.Cleanup)),
      now,
    )
  assert terminal.cleanup_phase == event.Eligible

  let active =
    local_artifacts.classify(
      metadata(local_artifacts.WorkflowArtifact, None, Some(event.Cleanup)),
      now,
    )
  assert active.cleanup_phase == event.Retained
  assert string.contains(active.reason, "missing terminal time")

  let interrupted =
    local_artifacts.classify(
      metadata(
        local_artifacts.WorkflowArtifact,
        Some(0),
        Some(event.Interrupted),
      ),
      now,
    )
  assert interrupted.cleanup_phase == event.Retained
  assert string.contains(interrupted.reason, "interrupted")

  let parked =
    local_artifacts.classify(
      metadata(local_artifacts.WorkflowArtifact, Some(0), Some(event.Parked)),
      now,
    )
  assert parked.cleanup_phase == event.Retained

  let old_state =
    local_artifacts.classify(
      metadata(
        local_artifacts.WorkflowArtifact,
        Some(0),
        Some(event.OldStateResetRequired),
      ),
      now,
    )
  assert old_state.cleanup_phase == event.Retained

  let missing_owner =
    local_artifacts.classify(
      local_artifacts.LocalArtifactMetadata(
        ..metadata(
          local_artifacts.WorkflowArtifact,
          Some(0),
          Some(event.Cleanup),
        ),
        owner_id: None,
      ),
      now,
    )
  assert missing_owner.cleanup_phase == event.Retained

  let malformed =
    local_artifacts.classify(
      local_artifacts.LocalArtifactMetadata(
        ..metadata(
          local_artifacts.WorkflowArtifact,
          Some(0),
          Some(event.Cleanup),
        ),
        malformed: True,
      ),
      now,
    )
  assert malformed.cleanup_phase == event.Retained

  let unsupported =
    local_artifacts.classify(
      local_artifacts.LocalArtifactMetadata(
        ..metadata(
          local_artifacts.WorkflowArtifact,
          Some(0),
          Some(event.Cleanup),
        ),
        schema_status: local_artifacts.SchemaUnsupported(0),
      ),
      now,
    )
  assert unsupported.cleanup_phase == event.Retained
}

pub fn retention_classifier_uses_transcript_and_tombstone_durations_test() {
  let transcript_before =
    local_artifacts.classify(
      metadata(local_artifacts.PiTranscript, Some(0), Some(event.Cleanup)),
      local_artifacts.pi_transcript_retention_ms - 1,
    )
  assert transcript_before.cleanup_phase == event.Retained

  let transcript_after =
    local_artifacts.classify(
      metadata(local_artifacts.PiTranscript, Some(0), Some(event.Cleanup)),
      local_artifacts.pi_transcript_retention_ms,
    )
  assert transcript_after.cleanup_phase == event.Eligible

  let tombstone_after =
    local_artifacts.classify(
      metadata(local_artifacts.CleanupTombstone, Some(0), Some(event.Cleanup)),
      local_artifacts.cleanup_tombstone_retention_ms,
    )
  assert tombstone_after.cleanup_phase == event.Eligible
}

pub fn path_safety_rejects_escapes_and_symlinks_test() {
  let root = "test/tmp/local-artifacts/path-safety"
  let _ = simplifile.delete(root)
  let assert Ok(Nil) =
    simplifile.create_directory_all(root <> "/.scherzo-state")
  let assert Ok(Nil) = simplifile.create_directory_all(root <> "/outside")
  let assert Ok(Nil) =
    simplifile.create_directory_all(root <> "/.scherzo-state/real")
  let assert Ok(Nil) =
    simplifile.write(root <> "/.scherzo-state/real/file", "ok")
  let _ =
    simplifile.create_symlink("../outside", root <> "/.scherzo-state/link")

  let assert local_artifacts.PathSafe(_) =
    local_artifacts.check_path_safety(root, root <> "/.scherzo-state/real/file")
  let assert local_artifacts.PathUnsafe(_) =
    local_artifacts.check_path_safety(root, root <> "/outside/file")
  let assert local_artifacts.PathUnsafe(_) =
    local_artifacts.check_path_safety(
      root,
      root <> "/.scherzo-state/../outside",
    )
  let assert local_artifacts.PathUnsafe(_) =
    local_artifacts.check_path_safety(root, root <> "/.scherzo-state/link/file")
  let assert local_artifacts.PathUnsafe(_) =
    local_artifacts.check_path_safety("/", root <> "/.scherzo-state/real/file")
}

pub fn cleanup_dry_run_and_apply_delete_only_verified_eligible_artifacts_test() {
  let root = "test/tmp/local-artifacts/cleanup"
  let _ = simplifile.delete(root)
  let archive_dir = root <> "/.scherzo-state/ledger/archive"
  let assert Ok(Nil) = simplifile.create_directory_all(archive_dir)
  let eligible = archive_dir <> "/segment-1.jsonl"
  let retained = root <> "/.scherzo-state/ledger/current.jsonl"
  let assert Ok(Nil) = simplifile.write(eligible, "old")
  let assert Ok(Nil) = simplifile.write(retained, "current")

  let now =
    local_artifacts.now_ms()
    + local_artifacts.workflow_artifact_retention_ms
    + 1000
  let dry = local_artifacts.inventory(root, now, True)
  assert dry.dry_run == True
  assert dry.deleted == []
  assert dry.would_delete != []
  let assert Ok(True) = simplifile.is_file(eligible)

  let applied = local_artifacts.apply_cleanup(root, now)
  assert applied.dry_run == False
  assert applied.deleted != []
  let assert Ok(False) = simplifile.is_file(eligible)
  let assert Ok(True) = simplifile.is_file(retained)
}

pub fn cleanup_apply_retains_artifact_when_tombstone_write_fails_test() {
  let root = "test/tmp/local-artifacts/tombstone-failure"
  let _ = simplifile.delete(root)
  let archive_dir = root <> "/.scherzo-state/ledger/archive"
  let assert Ok(Nil) = simplifile.create_directory_all(archive_dir)
  let eligible = archive_dir <> "/segment-1.jsonl"
  let assert Ok(Nil) = simplifile.write(eligible, "old")
  let assert Ok(Nil) =
    simplifile.write(root <> "/.scherzo-state/cleanup", "not a dir")

  let now =
    local_artifacts.now_ms()
    + local_artifacts.workflow_artifact_retention_ms
    + 1000
  let applied = local_artifacts.apply_cleanup(root, now)

  assert applied.deleted == []
  assert applied.warnings != []
  let assert Ok(True) = simplifile.is_file(eligible)
}

pub fn offline_state_status_archive_discard_and_reinitialize_test() {
  let root = "test/tmp/local-artifacts/old-state-archive"
  let _ = simplifile.delete(root)
  let ledger_dir = root <> "/.scherzo-state/ledger"
  let assert Ok(Nil) = simplifile.create_directory_all(ledger_dir <> "/archive")
  let unsupported_line =
    "{\"schema_version\":0,\"record_id\":\"old\",\"at_ms\":1,\"kind\":\"run_started\",\"run_id\":\"run\",\"issue_id\":\"issue\",\"issue_identifier\":\"ABC-1\",\"workspace_path\":\"work\"}"
  let assert Ok(Nil) =
    simplifile.write(ledger_dir <> "/current.jsonl", unsupported_line <> "\n")

  let status = local_artifacts.inspect_state(root)
  let assert local_artifacts.StateUnsupported(0, _) = status.status
  let encoded = status |> local_artifacts.state_status_to_json |> json.to_string
  assert string.contains(encoded, "old_state_reset_required")

  let rejected = local_artifacts.archive_old_state(root, False, 123)
  assert rejected.status == "rejected"
  let archived = local_artifacts.archive_old_state(root, True, 123)
  assert archived.status == "applied"
  let assert Ok(False) = simplifile.is_directory(ledger_dir)

  let reinitialized = local_artifacts.reinitialize_state(root, yes: True)
  assert reinitialized.status == "applied"
  let assert Ok(True) = simplifile.is_file(ledger_dir <> "/current.jsonl")

  let discard_root = "test/tmp/local-artifacts/old-state-discard"
  let _ = simplifile.delete(discard_root)
  let discard_ledger = discard_root <> "/.scherzo-state/ledger"
  let assert Ok(Nil) =
    simplifile.create_directory_all(discard_ledger <> "/archive")
  let assert Ok(Nil) =
    simplifile.write(
      discard_ledger <> "/current.jsonl",
      unsupported_line <> "\n",
    )
  let discarded = local_artifacts.discard_old_state(discard_root, True, 456)
  assert discarded.status == "applied"
  let assert Ok(False) = simplifile.is_directory(discard_ledger)
}

pub fn offline_state_status_warns_for_scheduled_records_test() {
  let root = "test/tmp/local-artifacts/scheduled-state-warning"
  let _ = simplifile.delete(root)
  let assert Ok(paths) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) =
    ledger.append_many(
      paths,
      [
        record.with_id(
          "scheduled-due",
          1,
          record.ScheduledJobDue(
            "nightly",
            "maintenance",
            1000,
            "schedule-nightly-19700101T000001Z",
            "automatic",
          ),
        ),
      ],
      True,
    )

  let status = local_artifacts.inspect_state(root)
  assert status.status == local_artifacts.StateCurrent
  assert status.warnings != []
  let encoded = status |> local_artifacts.state_status_to_json |> json.to_string
  assert string.contains(encoded, "scheduled ledger records are present")
}
