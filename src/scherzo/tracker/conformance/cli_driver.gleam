import scherzo/tracker/conformance/driver
import scherzo/tracker/conformance/types

pub type DriverFailure =
  driver.DriverFailure

pub type DriverFailureKind =
  driver.DriverFailureKind

pub type DriverInvocation =
  driver.DriverInvocation

pub fn invoke(
  manifest: types.Manifest,
  request: types.DriverRequest,
) -> Result(DriverInvocation, DriverFailure) {
  driver.invoke(manifest, request)
}
