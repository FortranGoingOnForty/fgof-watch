program test_watch_debounce
  use fgof_watch, only : init_watch, poll_watch, reset_watch
  use fgof_watch_types, only : &
    FGOF_WATCH_EVT_CREATED, &
    FGOF_WATCH_EVT_MODIFIED, &
    watch_event, &
    watch_options, &
    watch_session
  use watch_test_support, only : &
    append_text, &
    ensure_clean_dir, &
    expect_no_events, &
    expect_single_event, &
    remove_path, &
    remove_tree, &
    write_text
  implicit none

  call test_created_event_debounces()
  call test_modify_burst_coalesces()
  call test_create_remove_cancels()

contains

  subroutine test_created_event_debounces()
    character(len=*), parameter :: root = "build/watch-tests-debounce-created"
    character(len=*), parameter :: file_path = "build/watch-tests-debounce-created/file.txt"
    type(watch_event), allocatable :: events(:)
    type(watch_options) :: options
    type(watch_session) :: session

    call ensure_clean_dir(root)

    options = watch_options(debounce_polls=1)
    call init_watch(session, root, options)

    call write_text(file_path, "alpha")
    events = poll_watch(session)
    call expect_no_events(events, "created event should wait for a quiet poll")

    events = poll_watch(session)
    call expect_single_event(events, FGOF_WATCH_EVT_CREATED, file_path, "", .false., "created event should emit after debounce settles")

    call reset_watch(session)
    call remove_tree(root)
  end subroutine test_created_event_debounces

  subroutine test_modify_burst_coalesces()
    character(len=*), parameter :: root = "build/watch-tests-debounce-modify"
    character(len=*), parameter :: file_path = "build/watch-tests-debounce-modify/file.txt"
    type(watch_event), allocatable :: events(:)
    type(watch_options) :: options
    type(watch_session) :: session

    call ensure_clean_dir(root)
    call write_text(file_path, "seed")

    options = watch_options(debounce_polls=1)
    call init_watch(session, root, options)

    call append_text(file_path, "-a")
    events = poll_watch(session)
    call expect_no_events(events, "first modify should debounce")

    call append_text(file_path, "-b")
    events = poll_watch(session)
    call expect_no_events(events, "modify burst should coalesce while debounce is active")

    events = poll_watch(session)
    call expect_single_event(events, FGOF_WATCH_EVT_MODIFIED, file_path, "", .false., "modify burst should collapse into one event")

    call reset_watch(session)
    call remove_tree(root)
  end subroutine test_modify_burst_coalesces

  subroutine test_create_remove_cancels()
    character(len=*), parameter :: root = "build/watch-tests-debounce-cancel"
    character(len=*), parameter :: file_path = "build/watch-tests-debounce-cancel/file.txt"
    type(watch_event), allocatable :: events(:)
    type(watch_options) :: options
    type(watch_session) :: session

    call ensure_clean_dir(root)

    options = watch_options(debounce_polls=1)
    call init_watch(session, root, options)

    call write_text(file_path, "alpha")
    events = poll_watch(session)
    call expect_no_events(events, "created event should enter debounce queue")

    call remove_path(file_path)
    events = poll_watch(session)
    call expect_no_events(events, "create then remove inside debounce window should cancel")

    events = poll_watch(session)
    call expect_no_events(events, "cancelled debounce sequence should stay quiet")

    call reset_watch(session)
    call remove_tree(root)
  end subroutine test_create_remove_cancels

end program test_watch_debounce
