program test_watch_recursive
  use fgof_watch, only : init_watch, poll_watch, reset_watch
  use fgof_watch_types, only : FGOF_WATCH_EVT_CREATED, watch_event, watch_options, watch_session
  use watch_test_support, only : &
    ensure_clean_dir, &
    expect_no_events, &
    expect_single_event, &
    make_dir, &
    remove_tree, &
    write_text
  implicit none

  character(len=*), parameter :: nonrecursive_root = "build/watch-tests-nonrecursive"
  character(len=*), parameter :: recursive_root = "build/watch-tests-recursive"
  character(len=*), parameter :: nested_path_nonrecursive = "build/watch-tests-nonrecursive/child/nested.txt"
  character(len=*), parameter :: nested_path_recursive = "build/watch-tests-recursive/child/nested.txt"
  type(watch_event), allocatable :: events(:)
  type(watch_options) :: options
  type(watch_session) :: session

  call ensure_clean_dir(nonrecursive_root)
  call make_dir(nonrecursive_root // "/child")

  options = watch_options(recursive=.false.)
  call init_watch(session, nonrecursive_root, options)
  call write_text(nested_path_nonrecursive, "alpha")
  events = poll_watch(session)
  call expect_no_events(events, "nonrecursive watch should ignore nested file creation")
  call reset_watch(session)
  call remove_tree(nonrecursive_root)

  call ensure_clean_dir(recursive_root)
  call make_dir(recursive_root // "/child")

  options = watch_options(recursive=.true.)
  call init_watch(session, recursive_root, options)
  call write_text(nested_path_recursive, "alpha")
  events = poll_watch(session)
  call expect_single_event(events, FGOF_WATCH_EVT_CREATED, nested_path_recursive, "", .false., "recursive watch should detect nested file creation")
  call reset_watch(session)
  call remove_tree(recursive_root)
end program test_watch_recursive
