import scherzo/review_lane_preflight
import scherzo/review_lane_preflight_policy

pub type ClaimGateResult {
  ClaimAllowed
  ClaimBlocked(code: String, message: String, park_on_failure: Bool)
}

pub fn before_claim(
  policy: review_lane_preflight_policy.Policy,
  result: review_lane_preflight.PreflightResult,
) -> ClaimGateResult {
  case policy.mode {
    review_lane_preflight_policy.Off -> ClaimAllowed
    review_lane_preflight_policy.OfflineRequired
    | review_lane_preflight_policy.RequiredLive ->
      case result {
        review_lane_preflight.PreflightPassed(..) -> ClaimAllowed
        review_lane_preflight.PreflightFailed(
          code: code,
          message: message,
          blocking: blocking,
          ..,
        ) ->
          case blocking {
            False -> ClaimAllowed
            True ->
              ClaimBlocked(
                code: code,
                message: message,
                park_on_failure: policy.park_on_failure,
              )
          }
      }
  }
}

pub fn report_code(result: ClaimGateResult) -> String {
  case result {
    ClaimAllowed -> "ok"
    ClaimBlocked(code, ..) -> code
  }
}
