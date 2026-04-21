module fgof_watch
  use fgof_watch_types, only : FGOF_WATCH_EVT_NONE, watch_event, watch_options, watch_session
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
    session%active = len_trim(root) > 0
  end subroutine init_watch

  function poll_watch(session) result(event)
    type(watch_session), intent(in) :: session
    type(watch_event) :: event

    event%kind = FGOF_WATCH_EVT_NONE
    if (allocated(session%root)) then
      event%path = session%root
    else
      event%path = ""
    end if
  end function poll_watch

  subroutine reset_watch(session)
    type(watch_session), intent(inout) :: session

    if (allocated(session%root)) then
      deallocate(session%root)
    end if

    session%options = watch_options()
    session%active = .false.
  end subroutine reset_watch

end module fgof_watch
