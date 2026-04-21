program test_watch_errors
  use fgof_watch, only : init_watch, poll_watch, reset_watch, set_ignore_prefixes
  use fgof_watch_types, only : FGOF_WATCH_ERR_NONE, FGOF_WATCH_ERR_SNAPSHOT_FAILED, watch_event, watch_options, watch_session
  use watch_test_support, only : chmod_mode, ensure_clean_dir, expect_no_events, make_dir, remove_tree, write_text
  implicit none

  call test_runtime_snapshot_failure()
  call test_hidden_pruning_avoids_failure()
  call test_prefix_pruning_avoids_failure()

contains

  subroutine test_runtime_snapshot_failure()
    character(len=*), parameter :: root = "build/watch-tests-errors"
    character(len=*), parameter :: locked_dir = "build/watch-tests-errors/locked"
    character(len=*), parameter :: locked_file = "build/watch-tests-errors/locked/file.txt"
    type(watch_event), allocatable :: events(:)
    type(watch_session) :: session

    call ensure_clean_dir(root)
    call make_dir(locked_dir)
    call write_text(locked_file, "alpha")

    call init_watch(session, root)
    if (session%last_error_code /= FGOF_WATCH_ERR_NONE) error stop "initial snapshot should succeed"

    call chmod_mode(locked_dir, "000")
    events = poll_watch(session)
    call expect_no_events(events, "snapshot failure should not emit false remove events")
    if (session%last_error_code /= FGOF_WATCH_ERR_SNAPSHOT_FAILED) error stop "snapshot failure should set the session error code"
    if (.not. allocated(session%last_error_message)) error stop "snapshot failure should preserve an error message"
    if (len(session%last_error_message) == 0) error stop "snapshot failure message should not be empty"

    call chmod_mode(locked_dir, "755")
    events = poll_watch(session)
    call expect_no_events(events, "state should recover cleanly after access is restored")
    if (session%last_error_code /= FGOF_WATCH_ERR_NONE) error stop "successful poll should clear the session error"

    call reset_watch(session)
    call remove_tree(root)
  end subroutine test_runtime_snapshot_failure

  subroutine test_hidden_pruning_avoids_failure()
    character(len=*), parameter :: root = "build/watch-tests-hidden-prune"
    character(len=*), parameter :: hidden_dir = "build/watch-tests-hidden-prune/.cache"
    character(len=*), parameter :: hidden_file = "build/watch-tests-hidden-prune/.cache/file.txt"
    type(watch_options) :: options
    type(watch_session) :: session

    call ensure_clean_dir(root)
    call make_dir(hidden_dir)
    call write_text(hidden_file, "alpha")
    call chmod_mode(hidden_dir, "000")

    options%ignore_hidden = .true.
    call init_watch(session, root, options)
    if (session%last_error_code /= FGOF_WATCH_ERR_NONE) error stop "ignored hidden subtree should be pruned before snapshot failure"

    call chmod_mode(hidden_dir, "755")
    call reset_watch(session)
    call remove_tree(root)
  end subroutine test_hidden_pruning_avoids_failure

  subroutine test_prefix_pruning_avoids_failure()
    character(len=*), parameter :: root = "build/watch-tests-prefix-prune"
    character(len=*), parameter :: vendor_dir = "build/watch-tests-prefix-prune/vendor"
    character(len=*), parameter :: vendor_file = "build/watch-tests-prefix-prune/vendor/file.txt"
    type(watch_options) :: options
    type(watch_session) :: session

    call ensure_clean_dir(root)
    call make_dir(vendor_dir)
    call write_text(vendor_file, "alpha")
    call chmod_mode(vendor_dir, "000")

    call set_ignore_prefixes(options, [character(len=len(vendor_dir)) :: vendor_dir])
    call init_watch(session, root, options)
    if (session%last_error_code /= FGOF_WATCH_ERR_NONE) error stop "ignored prefix subtree should be pruned before snapshot failure"

    call chmod_mode(vendor_dir, "755")
    call reset_watch(session)
    call remove_tree(root)
  end subroutine test_prefix_pruning_avoids_failure
end program test_watch_errors
