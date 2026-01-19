import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

pub fn placeholder_test() {
  // Placeholder test to verify test infrastructure works
  1 + 1
  |> should.equal(2)
}
