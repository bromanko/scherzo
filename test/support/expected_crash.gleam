//// Helpers for tests that intentionally exercise crash paths.
////
//// The suppression is marker-based: only logger crash reports whose raw event
//// contains every provided marker are dropped. A mismatched/unexpected crash
//// report still reaches the normal Erlang logger output so CI remains noisy for
//// surprises.

pub fn suppressing(markers: List(String), run: fn() -> a) -> a {
  with_suppressed_crash_reports(markers, run)
}

@external(erlang, "scherzo_expected_crash_ffi", "with_suppressed_crash_reports")
fn with_suppressed_crash_reports(markers: List(String), run: fn() -> a) -> a
