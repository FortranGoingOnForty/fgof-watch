program test_watch_literal_paths
  use fgof_watch, only : init_watch, poll_watch, reset_watch
  use fgof_watch_types, only : FGOF_WATCH_EVT_CREATED, watch_event, watch_session
  use watch_test_support, only : ensure_clean_dir, expect_single_event, remove_tree, write_text
  implicit none

  call test_tab_name()
  call test_newline_name()

contains

  subroutine test_tab_name()
    character(len=*), parameter :: root = "build/watch-tests-tab-name"
    character(len=:), allocatable :: tab_path
    type(watch_event), allocatable :: events(:)
    type(watch_session) :: session

    call ensure_clean_dir(root)
    call init_watch(session, root)

    tab_path = root // "/tab" // achar(9) // "name.txt"
    call write_text(tab_path, "alpha")
    events = poll_watch(session)
    call expect_single_event(events, FGOF_WATCH_EVT_CREATED, tab_path, "", .false., "tab characters in paths should round-trip through watch events")

    call reset_watch(session)
    call remove_tree(root)
  end subroutine test_tab_name

  subroutine test_newline_name()
    character(len=*), parameter :: root = "build/watch-tests-newline-name"
    character(len=:), allocatable :: newline_path
    type(watch_event), allocatable :: events(:)
    type(watch_session) :: session

    call ensure_clean_dir(root)
    call init_watch(session, root)

    newline_path = root // "/line" // new_line("a") // "break.txt"
    call write_text(newline_path, "alpha")
    events = poll_watch(session)
    call expect_single_event(events, FGOF_WATCH_EVT_CREATED, newline_path, "", .false., "newline characters in paths should round-trip through watch events")

    call reset_watch(session)
    call remove_tree(root)
  end subroutine test_newline_name
end program test_watch_literal_paths
