import gleam/option.{None, Some}
import scherzo/review_lane_preflight
import simplifile

fn reset_dir(path: String) -> Nil {
  let _ = simplifile.delete(path)
  let assert Ok(Nil) = simplifile.create_directory_all(path)
  Nil
}

pub fn review_lane_preflight_cache_reads_unexpired_blocking_failure_test() {
  let root = "test/tmp/review-lane-preflight/cache-blocking"
  reset_dir(root)
  let state_root = root <> "/.scherzo-state"
  let assert Ok(Nil) = simplifile.create_directory_all(state_root)
  let cache_path = review_lane_preflight.cache_path(state_root)
  let assert Ok(Nil) =
    simplifile.write(
      cache_path,
      "{\"schema_version\":1,\"entries\":[{\"cache_key\":\"cache-key\",\"status\":\"failed\",\"blocking\":true,\"code\":\"provider_tool_registration_failed\",\"message\":\"cached provider rejection\",\"expires_at_ms\":2000,\"checked_at_utc\":\"1970-01-01T00:00:01Z\",\"expires_at_utc\":\"1970-01-01T00:00:02Z\"}]}\n",
    )

  let assert Some(review_lane_preflight.PreflightFailed(
    cache_key: "cache-key",
    code: "provider_tool_registration_failed",
    message: "cached provider rejection",
    blocking: True,
  )) = review_lane_preflight.read_cached_result(state_root, "cache-key", 1500)
}

pub fn review_lane_preflight_cache_ignores_expired_entry_test() {
  let root = "test/tmp/review-lane-preflight/cache-expired"
  reset_dir(root)
  let state_root = root <> "/.scherzo-state"
  let assert Ok(Nil) = simplifile.create_directory_all(state_root)
  let cache_path = review_lane_preflight.cache_path(state_root)
  let assert Ok(Nil) =
    simplifile.write(
      cache_path,
      "{\"schema_version\":1,\"entries\":[{\"cache_key\":\"cache-key\",\"status\":\"failed\",\"blocking\":true,\"code\":\"provider_tool_registration_failed\",\"message\":\"cached provider rejection\",\"expires_at_ms\":2000}]}\n",
    )

  assert review_lane_preflight.read_cached_result(state_root, "cache-key", 2000)
    == None
}
