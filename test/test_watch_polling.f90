program test_watch_polling
  use fgof_watch, only : init_watch, poll_watch, reset_watch
  use fgof_watch_types, only : &
    FGOF_WATCH_EVT_CREATED, &
    FGOF_WATCH_EVT_MODIFIED, &
    FGOF_WATCH_EVT_MOVED, &
    FGOF_WATCH_EVT_REMOVED, &
    watch_event, &
    watch_session
  use watch_test_support, only : &
    append_text, &
    ensure_clean_dir, &
    expect_no_events, &
    expect_single_event, &
    move_path, &
    remove_path, &
    remove_tree, &
    write_text
  implicit none

  character(len=*), parameter :: root = "build/watch-tests-polling"
  character(len=*), parameter :: alpha_path = "build/watch-tests-polling/alpha.txt"
  character(len=*), parameter :: beta_path = "build/watch-tests-polling/beta.txt"
  type(watch_event), allocatable :: events(:)
  type(watch_session) :: session

  call ensure_clean_dir(root)
  call init_watch(session, root)

  events = poll_watch(session)
  call expect_no_events(events, "initial poll should be empty")

  call write_text(alpha_path, "alpha")
  events = poll_watch(session)
  call expect_single_event(events, FGOF_WATCH_EVT_CREATED, alpha_path, "", .false., "file creation should be detected")

  call append_text(alpha_path, "beta")
  events = poll_watch(session)
  call expect_single_event(events, FGOF_WATCH_EVT_MODIFIED, alpha_path, "", .false., "file modification should be detected")

  call move_path(alpha_path, beta_path)
  events = poll_watch(session)
  call expect_single_event(events, FGOF_WATCH_EVT_MOVED, beta_path, alpha_path, .false., "file move should be detected")

  call remove_path(beta_path)
  events = poll_watch(session)
  call expect_single_event(events, FGOF_WATCH_EVT_REMOVED, beta_path, "", .false., "file removal should be detected")

  call reset_watch(session)
  call remove_tree(root)
end program test_watch_polling
