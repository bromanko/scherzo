import scherzo_linear_conformance_live_driver as live_driver

pub fn main() -> Nil {
  live_driver.run_cli(args())
  |> halt
}

@external(erlang, "scherzo_main_ffi", "args")
fn args() -> List(String)

@external(erlang, "scherzo_main_ffi", "halt")
fn halt(code: Int) -> Nil
