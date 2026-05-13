import glinter
import scherzo_lint/rules/public_function_labels

pub fn main() {
  glinter.run(extra_rules: [
    public_function_labels.rule(),
  ])
}
