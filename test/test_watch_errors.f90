program test_watch_errors
  use fgof_watch, only : init_watch, poll_watch, reset_watch
  use fgof_watch_types, only : FGOF_WATCH_ERR_NONE, FGOF_WATCH_ERR_SNAPSHOT_FAILED, watch_event, watch_session
  use watch_test_support, only : chmod_mode, ensure_clean_dir, expect_no_events, make_dir, remove_tree, write_text
  implicit none

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
end program test_watch_errors
