module fgof_watch_types
  use, intrinsic :: iso_fortran_env, only : int64
  implicit none
  private

  integer, parameter, public :: FGOF_WATCH_EVT_NONE = 0
  integer, parameter, public :: FGOF_WATCH_EVT_CREATED = 1
  integer, parameter, public :: FGOF_WATCH_EVT_MODIFIED = 2
  integer, parameter, public :: FGOF_WATCH_EVT_REMOVED = 3
  integer, parameter, public :: FGOF_WATCH_EVT_MOVED = 4

  public :: watch_event
  public :: watch_entry
  public :: watch_options
  public :: watch_session

  type :: watch_event
    integer :: kind = FGOF_WATCH_EVT_NONE
    logical :: is_directory = .false.
    character(len=:), allocatable :: path
    character(len=:), allocatable :: previous_path
  end type watch_event

  type :: watch_options
    integer :: poll_interval_ms = 250
    logical :: recursive = .true.
  end type watch_options

  type :: watch_entry
    character(len=:), allocatable :: path
    integer(int64) :: inode = 0_int64
    integer(int64) :: size = 0_int64
    integer(int64) :: mtime_sec = 0_int64
    integer(int64) :: mtime_nsec = 0_int64
    logical :: is_directory = .false.
  end type watch_entry

  type :: watch_session
    character(len=:), allocatable :: root
    type(watch_options) :: options
    logical :: active = .false.
    type(watch_entry), allocatable :: entries(:)
  end type watch_session
end module fgof_watch_types
