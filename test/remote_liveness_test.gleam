import scherzo/control/remote_liveness

pub fn remote_liveness_tracks_online_stale_and_offline_without_cached_online_test() {
  let daemon_id = "daemon_aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
  let boot_id = "boot_bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
  let assert Ok(registry) = remote_liveness.new(5, 10)
  let assert Ok(registered) =
    remote_liveness.register_hello(registry, daemon_id, boot_id, 100)

  let assert Ok(online) = remote_liveness.view(registered, daemon_id, 100)
  assert online.status == remote_liveness.Online

  let assert Ok(stale) = remote_liveness.view(registered, daemon_id, 105)
  assert stale.status == remote_liveness.Stale

  let assert Ok(offline) = remote_liveness.view(registered, daemon_id, 110)
  assert offline.status == remote_liveness.Offline
}

pub fn remote_liveness_replaces_old_boot_and_rejects_stale_heartbeat_test() {
  let daemon_id = "daemon_aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
  let first_boot_id = "boot_bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
  let second_boot_id = "boot_cccccccccccccccccccccccccccccccc"
  let assert Ok(registry) = remote_liveness.new(5, 10)
  let assert Ok(first) =
    remote_liveness.register_hello(registry, daemon_id, first_boot_id, 100)
  let assert Ok(second) =
    remote_liveness.register_hello(first, daemon_id, second_boot_id, 120)

  let assert Error(remote_liveness.StaleBoot(expected_boot_id:, actual_boot_id:)) =
    remote_liveness.heartbeat(second, daemon_id, first_boot_id, 130)
  assert expected_boot_id == second_boot_id
  assert actual_boot_id == first_boot_id

  let assert Ok(view) = remote_liveness.view(second, daemon_id, 120)
  assert view.boot_id == second_boot_id
  assert view.last_seen_at_ms == 120
}

pub fn remote_liveness_refreshes_active_heartbeat_and_sorts_snapshot_test() {
  let daemon_id = "daemon_aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
  let boot_id = "boot_bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
  let daemon_id_two = "daemon_dddddddddddddddddddddddddddddddd"
  let boot_id_two = "boot_eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"
  let assert Ok(registry) = remote_liveness.new(5, 10)
  let assert Ok(first) =
    remote_liveness.register_hello(registry, daemon_id_two, boot_id_two, 90)
  let assert Ok(second) =
    remote_liveness.register_hello(first, daemon_id, boot_id, 100)
  let assert Ok(third) =
    remote_liveness.heartbeat(second, daemon_id, boot_id, 101)

  let assert Ok(view) = remote_liveness.view(third, daemon_id, 101)
  assert view.status == remote_liveness.Online
  assert view.last_seen_at_ms == 101

  let snapshot = remote_liveness.snapshot(third, 101)
  assert snapshot
    == [
      remote_liveness.View(
        daemon_id: daemon_id,
        boot_id: boot_id,
        status: remote_liveness.Online,
        last_seen_at_ms: 101,
        observed_at_ms: 101,
      ),
      remote_liveness.View(
        daemon_id: daemon_id_two,
        boot_id: boot_id_two,
        status: remote_liveness.Offline,
        last_seen_at_ms: 90,
        observed_at_ms: 101,
      ),
    ]
}

pub fn remote_liveness_rejects_invalid_ids_thresholds_and_unknown_daemon_test() {
  let assert Error(remote_liveness.InvalidThresholds(
    stale_after_ms: 5,
    offline_after_ms: 5,
  )) = remote_liveness.new(5, 5)
  let assert Ok(registry) = remote_liveness.new(5, 10)
  let assert Error(remote_liveness.InvalidDaemonId(_)) =
    remote_liveness.register_hello(
      registry,
      "daemon_bad",
      "boot_bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
      0,
    )
  let assert Error(remote_liveness.InvalidBootId(_)) =
    remote_liveness.register_hello(
      registry,
      "daemon_aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
      "boot_bad",
      0,
    )
  let assert Error(remote_liveness.UnknownDaemon(_)) =
    remote_liveness.heartbeat(
      registry,
      "daemon_aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
      "boot_bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
      0,
    )
}
