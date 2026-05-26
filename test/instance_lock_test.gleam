import scherzo/instance_lock
import scherzo/path
import support/test_helpers

pub fn instance_lock_excludes_same_root_until_release_test() {
  let root = "test/tmp/instance-lock/same"
  test_helpers.reset_dir(root)

  let assert Ok(lock) = instance_lock.acquire(root)
  let assert Error(instance_lock.LockAlreadyHeld(_)) =
    instance_lock.acquire(root)
  instance_lock.release(lock)
  let assert Ok(lock) = instance_lock.acquire(root)
  instance_lock.release(lock)
}

pub fn instance_lock_treats_absolute_and_relative_root_as_same_test() {
  let root = "test/tmp/instance-lock/equivalent"
  test_helpers.reset_dir(root)
  let assert Ok(abs) = path.absolute(root)

  let assert Ok(lock) = instance_lock.acquire(root)
  let assert Error(instance_lock.LockAlreadyHeld(_)) =
    instance_lock.acquire(abs)
  instance_lock.release(lock)
}

pub fn instance_lock_allows_different_roots_test() {
  let root_a = "test/tmp/instance-lock/a"
  let root_b = "test/tmp/instance-lock/b"
  test_helpers.reset_dir(root_a)
  test_helpers.reset_dir(root_b)

  let assert Ok(lock_a) = instance_lock.acquire(root_a)
  let assert Ok(lock_b) = instance_lock.acquire(root_b)
  instance_lock.release(lock_a)
  instance_lock.release(lock_b)
}

pub fn instance_lock_release_is_idempotent_best_effort_test() {
  let root = "test/tmp/instance-lock/idempotent-release"
  test_helpers.reset_dir(root)

  let assert Ok(lock) = instance_lock.acquire(root)
  instance_lock.release(lock)
  instance_lock.release(lock)
  let assert Ok(lock) = instance_lock.acquire(root)
  instance_lock.release(lock)
}
