module fgof_watch_types
  implicit none
  private

  integer, parameter, public :: FGOF_WATCH_EVT_NONE = 0

  public :: watch_event
  public :: watch_options
  public :: watch_session

  type :: watch_event
    integer :: kind = FGOF_WATCH_EVT_NONE
    character(len=:), allocatable :: path
  end type watch_event

  type :: watch_options
    integer :: poll_interval_ms = 250
    logical :: recursive = .true.
  end type watch_options

  type :: watch_session
    character(len=:), allocatable :: root
    type(watch_options) :: options
    logical :: active = .false.
  end type watch_session
end module fgof_watch_types
