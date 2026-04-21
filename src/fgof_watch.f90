module fgof_watch
  use fgof_watch_types, only : watch_event, watch_options, watch_session
  implicit none
  private

  public :: init_watch
  public :: poll_watch
  public :: reset_watch

contains

  subroutine init_watch(session, root, options)
    type(watch_session), intent(out) :: session
    character(len=*), intent(in) :: root
    type(watch_options), intent(in), optional :: options

    if (present(options)) then
      session%options = options
    end if

    session%root = root
    session%active = len(root) > 0
    allocate(session%entries(0))
  end subroutine init_watch

  function poll_watch(session) result(events)
    type(watch_session), intent(inout) :: session
    type(watch_event), allocatable :: events(:)

    if (.not. session%active) then
      allocate(events(0))
      return
    end if

    allocate(events(0))
  end function poll_watch

  subroutine reset_watch(session)
    type(watch_session), intent(inout) :: session

    if (allocated(session%root)) then
      deallocate(session%root)
    end if

    if (allocated(session%entries)) then
      deallocate(session%entries)
    end if

    session%options = watch_options()
    session%active = .false.
  end subroutine reset_watch

end module fgof_watch
