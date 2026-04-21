program test_event_model
  use fgof_watch_types, only : &
    FGOF_WATCH_EVT_CREATED, &
    FGOF_WATCH_EVT_MODIFIED, &
    FGOF_WATCH_EVT_MOVED, &
    FGOF_WATCH_EVT_NONE, &
    FGOF_WATCH_EVT_REMOVED, &
    watch_event
  implicit none

  type(watch_event) :: event

  if (FGOF_WATCH_EVT_NONE /= 0) error stop "none event constant should be zero"
  if (FGOF_WATCH_EVT_CREATED <= FGOF_WATCH_EVT_NONE) error stop "created event should be positive"
  if (FGOF_WATCH_EVT_MODIFIED <= FGOF_WATCH_EVT_CREATED) error stop "modified event ordering should be stable"
  if (FGOF_WATCH_EVT_REMOVED <= FGOF_WATCH_EVT_MODIFIED) error stop "removed event ordering should be stable"
  if (FGOF_WATCH_EVT_MOVED <= FGOF_WATCH_EVT_REMOVED) error stop "moved event ordering should be stable"

  event%kind = FGOF_WATCH_EVT_MOVED
  event%is_directory = .true.
  event%path = "new-path"
  event%previous_path = "old-path"

  if (event%kind /= FGOF_WATCH_EVT_MOVED) error stop "event kind should store move values"
  if (.not. event%is_directory) error stop "event directory flag should be stored"
  if (event%path /= "new-path") error stop "event path should be stored"
  if (event%previous_path /= "old-path") error stop "event previous path should be stored"
end program test_event_model
