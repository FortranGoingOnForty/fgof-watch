program test_watch_filters
  use fgof_watch, only : clear_ignore_prefixes, init_watch, poll_watch, reset_watch, set_ignore_prefixes
  use fgof_watch_types, only : FGOF_WATCH_EVT_CREATED, watch_event, watch_options, watch_session
  use watch_test_support, only : &
    ensure_clean_dir, &
    expect_no_events, &
    expect_single_event, &
    make_dir, &
    remove_tree, &
    write_text
  implicit none

  call test_ignore_hidden()
  call test_ignore_prefixes()

contains

  subroutine test_ignore_hidden()
    character(len=*), parameter :: root = "build/watch-tests-hidden"
    character(len=*), parameter :: hidden_path = "build/watch-tests-hidden/.cache.txt"
    character(len=*), parameter :: visible_path = "build/watch-tests-hidden/visible.txt"
    type(watch_event), allocatable :: events(:)
    type(watch_options) :: options
    type(watch_session) :: session

    call ensure_clean_dir(root)

    options = watch_options(ignore_hidden=.true.)
    call init_watch(session, root, options)

    call write_text(hidden_path, "alpha")
    events = poll_watch(session)
    call expect_no_events(events, "hidden file creation should be ignored")

    call write_text(visible_path, "beta")
    events = poll_watch(session)
    call expect_single_event(events, FGOF_WATCH_EVT_CREATED, visible_path, "", .false., "visible file creation should still be reported")

    call reset_watch(session)
    call remove_tree(root)
  end subroutine test_ignore_hidden

  subroutine test_ignore_prefixes()
    character(len=*), parameter :: root = "build/watch-tests-prefixes"
    character(len=*), parameter :: ignored_dir = "build/watch-tests-prefixes/vendor"
    character(len=*), parameter :: ignored_path = "build/watch-tests-prefixes/vendor/skip.txt"
    character(len=*), parameter :: visible_path = "build/watch-tests-prefixes/main.txt"
    type(watch_event), allocatable :: events(:)
    type(watch_options) :: options
    type(watch_session) :: session

    call ensure_clean_dir(root)
    call make_dir(ignored_dir)

    call set_ignore_prefixes(options, [character(len=len(ignored_dir)) :: ignored_dir])
    call init_watch(session, root, options)

    call write_text(ignored_path, "alpha")
    events = poll_watch(session)
    call expect_no_events(events, "ignored prefix should suppress nested file creation")

    call write_text(visible_path, "beta")
    events = poll_watch(session)
    call expect_single_event(events, FGOF_WATCH_EVT_CREATED, visible_path, "", .false., "visible file outside ignored prefixes should be reported")

    call clear_ignore_prefixes(options)
    if (allocated(options%ignore_prefixes)) error stop "ignore prefixes should clear cleanly"

    call reset_watch(session)
    call remove_tree(root)
  end subroutine test_ignore_prefixes

end program test_watch_filters
