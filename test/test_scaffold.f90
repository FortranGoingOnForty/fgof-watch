program test_scaffold
  use fgof_watch, only : init_watch, poll_watch, reset_watch
  use fgof_watch_types, only : FGOF_WATCH_EVT_NONE, watch_event, watch_options, watch_session
  implicit none

  type(watch_event) :: event
  type(watch_options) :: options
  type(watch_session) :: session

  options = watch_options(poll_interval_ms=100, recursive=.false.)
  call init_watch(session, "src", options)

  if (.not. session%active) error stop "watch session should be active"
  if (.not. allocated(session%root)) error stop "watch root should be allocated"
  if (session%root /= "src") error stop "watch root should match init input"
  if (session%options%poll_interval_ms /= 100) error stop "poll interval should be stored"
  if (session%options%recursive) error stop "recursive flag should follow options"

  event = poll_watch(session)
  if (event%kind /= FGOF_WATCH_EVT_NONE) error stop "scaffold poll should report none"
  if (.not. allocated(event%path)) error stop "scaffold poll should return a path"
  if (event%path /= "src") error stop "scaffold poll should echo root path"

  call reset_watch(session)
  if (session%active) error stop "reset should deactivate session"
  if (allocated(session%root)) error stop "reset should clear root path"
end program test_scaffold
