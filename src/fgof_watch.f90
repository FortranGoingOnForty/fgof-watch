module fgof_watch
  use, intrinsic :: iso_c_binding, only : c_associated, c_char, c_f_pointer, c_int, c_null_char, c_null_ptr, c_ptr, c_size_t
  use fgof_watch_types, only : &
    FGOF_WATCH_ERR_NONE, &
    FGOF_WATCH_ERR_SNAPSHOT_FAILED, &
    FGOF_WATCH_EVT_CREATED, &
    FGOF_WATCH_EVT_MODIFIED, &
    FGOF_WATCH_EVT_MOVED, &
    FGOF_WATCH_EVT_NONE, &
    FGOF_WATCH_EVT_REMOVED, &
    watch_entry, &
    watch_event, &
    watch_options, &
    watch_session
  implicit none
  private

  public :: init_watch
  public :: poll_watch
  public :: reset_watch
  public :: clear_ignore_prefixes
  public :: set_ignore_prefixes

  interface
    integer(c_int) function fgof_watch_collect_snapshot_c(root, recursive, buffer, buffer_len) bind(C, name="fgof_watch_collect_snapshot")
      import :: c_char, c_int, c_ptr, c_size_t
      character(kind=c_char), intent(in) :: root(*)
      integer(c_int), value :: recursive
      type(c_ptr), intent(out) :: buffer
      integer(c_size_t), intent(out) :: buffer_len
    end function fgof_watch_collect_snapshot_c

    subroutine fgof_watch_free_buffer_c(buffer) bind(C, name="fgof_watch_free_buffer")
      import :: c_ptr
      type(c_ptr), value :: buffer
    end subroutine fgof_watch_free_buffer_c
  end interface

contains

  subroutine init_watch(session, root, options)
    type(watch_session), intent(out) :: session
    character(len=*), intent(in) :: root
    type(watch_options), intent(in), optional :: options
    integer :: snapshot_status
    character(len=:), allocatable :: snapshot_message

    if (present(options)) then
      session%options = options
    end if

    session%root = root
    session%active = (len(root) > 0)

    if (.not. session%active) then
      allocate(session%entries(0))
      allocate(session%pending_events(0))
      allocate(session%pending_remaining(0))
      call clear_watch_error(session)
      return
    end if

    call collect_snapshot(root, session%options, session%entries, snapshot_status, snapshot_message)
    allocate(session%pending_events(0))
    allocate(session%pending_remaining(0))
    if (snapshot_status /= 0) then
      call set_watch_error(session, FGOF_WATCH_ERR_SNAPSHOT_FAILED, snapshot_message)
      deallocate(session%entries)
      allocate(session%entries(0))
    else
      call clear_watch_error(session)
    end if
  end subroutine init_watch

  function poll_watch(session) result(events)
    type(watch_session), intent(inout) :: session
    type(watch_event), allocatable :: events(:)
    type(watch_entry), allocatable :: current_entries(:)
    integer :: snapshot_status
    character(len=:), allocatable :: snapshot_message

    if (.not. session%active) then
      allocate(events(0))
      return
    end if

    call collect_snapshot(session%root, session%options, current_entries, snapshot_status, snapshot_message)
    if (snapshot_status /= 0) then
      call set_watch_error(session, FGOF_WATCH_ERR_SNAPSHOT_FAILED, snapshot_message)
      allocate(events(0))
      return
    end if

    call clear_watch_error(session)
    events = diff_snapshots(session%entries, current_entries, session%options)
    if (session%options%debounce_polls > 0) then
      events = debounce_event_batch(session, events)
    end if
    call move_alloc(current_entries, session%entries)
  end function poll_watch

  subroutine reset_watch(session)
    type(watch_session), intent(inout) :: session

    if (allocated(session%root)) then
      deallocate(session%root)
    end if

    if (allocated(session%entries)) then
      deallocate(session%entries)
    end if

    if (allocated(session%pending_events)) then
      deallocate(session%pending_events)
    end if

    if (allocated(session%pending_remaining)) then
      deallocate(session%pending_remaining)
    end if

    session%options = watch_options()
    session%active = .false.
    call clear_watch_error(session)
  end subroutine reset_watch

  subroutine clear_watch_error(session)
    type(watch_session), intent(inout) :: session

    session%last_error_code = FGOF_WATCH_ERR_NONE
    if (allocated(session%last_error_message)) then
      deallocate(session%last_error_message)
    end if
    session%last_error_message = ""
  end subroutine clear_watch_error

  subroutine set_watch_error(session, code, message)
    type(watch_session), intent(inout) :: session
    integer, intent(in) :: code
    character(len=*), intent(in) :: message

    session%last_error_code = code
    if (allocated(session%last_error_message)) then
      deallocate(session%last_error_message)
    end if
    session%last_error_message = trim(message)
  end subroutine set_watch_error

  subroutine set_ignore_prefixes(options, prefixes)
    type(watch_options), intent(inout) :: options
    character(len=*), intent(in) :: prefixes(:)
    integer :: i
    integer :: width

    call clear_ignore_prefixes(options)
    if (size(prefixes) == 0) return

    width = max(1, max_string_length(prefixes))
    allocate(character(len=width) :: options%ignore_prefixes(size(prefixes)))
    do i = 1, size(prefixes)
      options%ignore_prefixes(i) = prefixes(i)
    end do
  end subroutine set_ignore_prefixes

  subroutine clear_ignore_prefixes(options)
    type(watch_options), intent(inout) :: options

    if (allocated(options%ignore_prefixes)) then
      deallocate(options%ignore_prefixes)
    end if
  end subroutine clear_ignore_prefixes

  subroutine collect_snapshot(root, options, entries, status_code, status_message)
    character(len=*), intent(in) :: root
    type(watch_options), intent(in) :: options
    type(watch_entry), allocatable, intent(out) :: entries(:)
    integer, intent(out) :: status_code
    character(len=:), allocatable, intent(out) :: status_message
    type(c_ptr) :: raw_ptr
    integer(c_int) :: status
    integer(c_size_t) :: raw_len
    character(kind=c_char), allocatable :: c_root(:)
    character(kind=c_char), pointer :: raw_chars(:)
    character(len=:), allocatable :: text

    c_root = to_c_string(root)
    raw_ptr = c_null_ptr
    raw_len = 0_c_size_t
    status_code = 0
    status_message = ""

    status = fgof_watch_collect_snapshot_c(c_root, merge(1_c_int, 0_c_int, options%recursive), raw_ptr, raw_len)
    if (status /= 0_c_int) then
      allocate(entries(0))
      if (c_associated(raw_ptr)) call fgof_watch_free_buffer_c(raw_ptr)
      status_code = int(status)
      status_message = errno_message("snapshot collection failed", int(status))
      return
    end if

    if (.not. c_associated(raw_ptr) .or. raw_len == 0_c_size_t) then
      allocate(entries(0))
      if (c_associated(raw_ptr)) call fgof_watch_free_buffer_c(raw_ptr)
      return
    end if

    call c_f_pointer(raw_ptr, raw_chars, [int(raw_len)])
    text = buffer_to_text(raw_chars, int(raw_len))
    call fgof_watch_free_buffer_c(raw_ptr)

    call parse_snapshot_text(text, entries)
    call filter_entries(root, options, entries)
    call sort_entries(entries)
  end subroutine collect_snapshot

  subroutine filter_entries(root, options, entries)
    character(len=*), intent(in) :: root
    type(watch_options), intent(in) :: options
    type(watch_entry), allocatable, intent(inout) :: entries(:)
    type(watch_entry), allocatable :: filtered(:)
    integer :: i

    allocate(filtered(0))
    do i = 1, size(entries)
      if (entry_is_ignored(root, options, entries(i))) cycle
      call append_entry(filtered, entries(i))
    end do
    call move_alloc(filtered, entries)
  end subroutine filter_entries

  logical function entry_is_ignored(root, options, entry) result(ignored)
    character(len=*), intent(in) :: root
    type(watch_options), intent(in) :: options
    type(watch_entry), intent(in) :: entry

    ignored = .false.

    if (options%ignore_hidden) then
      if (contains_hidden_segment(path_after_root(root, entry%path))) then
        ignored = .true.
        return
      end if
    end if

    if (path_matches_ignore_prefix(options, entry%path)) then
      ignored = .true.
    end if
  end function entry_is_ignored

  logical function path_matches_ignore_prefix(options, path) result(matches)
    type(watch_options), intent(in) :: options
    character(len=*), intent(in) :: path
    integer :: i
    character(len=:), allocatable :: prefix

    matches = .false.
    if (.not. allocated(options%ignore_prefixes)) return

    do i = 1, size(options%ignore_prefixes)
      prefix = trim(options%ignore_prefixes(i))
      if (len(prefix) == 0) cycle
      if (path == prefix) then
        matches = .true.
        return
      end if
      if (len(path) > len(prefix)) then
        if (path(1:len(prefix)) == prefix .and. path(len(prefix) + 1:len(prefix) + 1) == "/") then
          matches = .true.
          return
        end if
      end if
    end do
  end function path_matches_ignore_prefix

  function path_after_root(root, path) result(relative)
    character(len=*), intent(in) :: root
    character(len=*), intent(in) :: path
    character(len=:), allocatable :: relative

    if (path == root) then
      relative = basename_text(root)
      return
    end if

    if (len(path) > len(root)) then
      if (path(1:len(root)) == root .and. path(len(root) + 1:len(root) + 1) == "/") then
        relative = path(len(root) + 2:)
        return
      end if
    end if

    relative = path
  end function path_after_root

  logical function contains_hidden_segment(path) result(has_hidden)
    character(len=*), intent(in) :: path
    integer :: i
    integer :: start
    integer :: n

    has_hidden = .false.
    n = len(path)
    if (n == 0) return

    start = 1
    do i = 1, n + 1
      if (i <= n .and. path(i:i) /= "/") cycle
      if (i > start) then
        if (path(start:start) == ".") then
          has_hidden = .true.
          return
        end if
      end if
      start = i + 1
    end do
  end function contains_hidden_segment

  function basename_text(path) result(name)
    character(len=*), intent(in) :: path
    character(len=:), allocatable :: name
    integer :: i

    do i = len(path), 1, -1
      if (path(i:i) == "/") then
        name = path(i + 1:)
        return
      end if
    end do

    name = path
  end function basename_text

  function diff_snapshots(previous_entries, current_entries, options) result(events)
    type(watch_entry), intent(in) :: previous_entries(:)
    type(watch_entry), intent(in) :: current_entries(:)
    type(watch_options), intent(in) :: options
    type(watch_event), allocatable :: events(:)
    type(watch_entry), allocatable :: created(:)
    type(watch_entry), allocatable :: modified(:)
    type(watch_entry), allocatable :: removed(:)
    integer :: i
    integer :: j

    allocate(created(0))
    allocate(modified(0))
    allocate(removed(0))

    i = 1
    j = 1
    do while (i <= size(previous_entries) .or. j <= size(current_entries))
      if (i > size(previous_entries)) then
        call append_entry(created, current_entries(j))
        j = j + 1
      else if (j > size(current_entries)) then
        call append_entry(removed, previous_entries(i))
        i = i + 1
      else if (previous_entries(i)%path == current_entries(j)%path) then
        if (entry_changed(previous_entries(i), current_entries(j))) then
          if (.not. current_entries(j)%is_directory) then
            call append_entry(modified, current_entries(j))
          end if
        end if
        i = i + 1
        j = j + 1
      else if (entry_less(previous_entries(i)%path, current_entries(j)%path)) then
        call append_entry(removed, previous_entries(i))
        i = i + 1
      else
        call append_entry(created, current_entries(j))
        j = j + 1
      end if
    end do

    events = build_event_batch(created, modified, removed, options)
    call sort_events(events)
  end function diff_snapshots

  function build_event_batch(created, modified, removed, options) result(events)
    type(watch_entry), intent(in) :: created(:)
    type(watch_entry), intent(in) :: modified(:)
    type(watch_entry), intent(in) :: removed(:)
    type(watch_options), intent(in) :: options
    type(watch_event), allocatable :: events(:)
    logical, allocatable :: created_used(:)
    logical, allocatable :: removed_used(:)
    integer :: i
    integer :: j

    allocate(events(0))
    allocate(created_used(size(created)))
    allocate(removed_used(size(removed)))
    created_used = .false.
    removed_used = .false.

    do i = 1, size(removed)
      do j = 1, size(created)
        if (created_used(j)) cycle
        if (removed(i)%inode <= 0) cycle
        if (removed(i)%inode /= created(j)%inode) cycle
        if (removed(i)%is_directory .neqv. created(j)%is_directory) cycle
        if (created(j)%is_directory .and. .not. options%emit_directory_events) then
          created_used(j) = .true.
          removed_used(i) = .true.
          exit
        end if
        call append_event(events, FGOF_WATCH_EVT_MOVED, created(j)%path, removed(i)%path, created(j)%is_directory)
        created_used(j) = .true.
        removed_used(i) = .true.
        exit
      end do
    end do

    do i = 1, size(created)
      if (created_used(i)) cycle
      if (created(i)%is_directory .and. .not. options%emit_directory_events) cycle
      call append_event(events, FGOF_WATCH_EVT_CREATED, created(i)%path, "", created(i)%is_directory)
    end do

    do i = 1, size(modified)
      if (modified(i)%is_directory .and. .not. options%emit_directory_events) cycle
      call append_event(events, FGOF_WATCH_EVT_MODIFIED, modified(i)%path, "", modified(i)%is_directory)
    end do

    do i = 1, size(removed)
      if (removed_used(i)) cycle
      if (removed(i)%is_directory .and. .not. options%emit_directory_events) cycle
      call append_event(events, FGOF_WATCH_EVT_REMOVED, removed(i)%path, "", removed(i)%is_directory)
    end do
  end function build_event_batch

  function debounce_event_batch(session, raw_events) result(events)
    type(watch_session), intent(inout) :: session
    type(watch_event), intent(in) :: raw_events(:)
    type(watch_event), allocatable :: events(:)
    logical, allocatable :: touched(:)
    integer :: i
    integer :: index

    if (.not. allocated(session%pending_events)) allocate(session%pending_events(0))
    if (.not. allocated(session%pending_remaining)) allocate(session%pending_remaining(0))

    allocate(touched(size(session%pending_events)))
    touched = .false.

    do i = 1, size(raw_events)
      index = find_related_pending_event(session%pending_events, raw_events(i))
      if (index > 0) then
        call merge_pending_event(session, index, raw_events(i))
        if (index <= size(session%pending_events)) then
          touched = resize_logical_flags(touched, size(session%pending_events))
          touched(index) = .true.
        end if
      else
        call append_pending_event(session, raw_events(i), session%options%debounce_polls)
        touched = resize_logical_flags(touched, size(session%pending_events))
        touched(size(touched)) = .true.
      end if
    end do

    do i = 1, size(session%pending_remaining)
      if (touched(i)) cycle
      session%pending_remaining(i) = session%pending_remaining(i) - 1
    end do

    call emit_ready_events(session, events)
  end function debounce_event_batch

  subroutine merge_pending_event(session, index, incoming)
    type(watch_session), intent(inout) :: session
    integer, intent(in) :: index
    type(watch_event), intent(in) :: incoming
    type(watch_event) :: merged
    logical :: drop_pending

    call merge_event_pair(session%pending_events(index), incoming, merged, drop_pending)
    if (drop_pending) then
      call remove_pending_event(session, index)
      return
    end if

    session%pending_events(index) = merged
    session%pending_remaining(index) = session%options%debounce_polls
  end subroutine merge_pending_event

  subroutine merge_event_pair(existing, incoming, merged, drop_pending)
    type(watch_event), intent(in) :: existing
    type(watch_event), intent(in) :: incoming
    type(watch_event), intent(out) :: merged
    logical, intent(out) :: drop_pending

    drop_pending = .false.
    merged = incoming

    select case (existing%kind)
    case (FGOF_WATCH_EVT_CREATED)
      select case (incoming%kind)
      case (FGOF_WATCH_EVT_CREATED)
        merged = incoming
      case (FGOF_WATCH_EVT_MODIFIED)
        merged = existing
      case (FGOF_WATCH_EVT_REMOVED)
        if (incoming%path == existing%path) then
          drop_pending = .true.
        else
          merged = incoming
        end if
      case (FGOF_WATCH_EVT_MOVED)
        if (incoming%previous_path == existing%path) then
          merged = existing
          merged%path = incoming%path
        else
          merged = incoming
        end if
      end select

    case (FGOF_WATCH_EVT_MODIFIED)
      select case (incoming%kind)
      case (FGOF_WATCH_EVT_CREATED)
        merged = incoming
      case (FGOF_WATCH_EVT_MODIFIED)
        merged = incoming
      case (FGOF_WATCH_EVT_REMOVED)
        merged = incoming
      case (FGOF_WATCH_EVT_MOVED)
        merged = incoming
      end select

    case (FGOF_WATCH_EVT_REMOVED)
      select case (incoming%kind)
      case (FGOF_WATCH_EVT_CREATED)
        if (incoming%path == existing%path) then
          merged%kind = FGOF_WATCH_EVT_MODIFIED
          merged%path = incoming%path
          merged%previous_path = ""
          merged%is_directory = incoming%is_directory
        else
          merged = incoming
        end if
      case default
        merged = incoming
      end select

    case (FGOF_WATCH_EVT_MOVED)
      select case (incoming%kind)
      case (FGOF_WATCH_EVT_MODIFIED)
        if (incoming%path == existing%path) then
          merged = existing
        else
          merged = incoming
        end if
      case (FGOF_WATCH_EVT_REMOVED)
        if (incoming%path == existing%path) then
          merged = incoming
        else
          merged = incoming
        end if
      case (FGOF_WATCH_EVT_MOVED)
        if (incoming%previous_path == existing%path) then
          merged = existing
          merged%path = incoming%path
        else
          merged = incoming
        end if
      case (FGOF_WATCH_EVT_CREATED)
        merged = incoming
      end select
    end select
  end subroutine merge_event_pair

  subroutine emit_ready_events(session, events)
    type(watch_session), intent(inout) :: session
    type(watch_event), allocatable, intent(out) :: events(:)
    type(watch_event), allocatable :: ready(:)
    integer :: i

    allocate(ready(0))
    i = 1
    do while (i <= size(session%pending_events))
      if (session%pending_remaining(i) > 0) then
        i = i + 1
        cycle
      end if

      call append_event_object(ready, session%pending_events(i))
      call remove_pending_event(session, i)
    end do

    call move_alloc(ready, events)
  end subroutine emit_ready_events

  subroutine append_pending_event(session, event, remaining)
    type(watch_session), intent(inout) :: session
    type(watch_event), intent(in) :: event
    integer, intent(in) :: remaining
    type(watch_event), allocatable :: grown_events(:)
    integer, allocatable :: grown_remaining(:)
    integer :: n

    n = size(session%pending_events)
    allocate(grown_events(n + 1))
    allocate(grown_remaining(n + 1))

    if (n > 0) then
      grown_events(1:n) = session%pending_events
      grown_remaining(1:n) = session%pending_remaining
    end if

    grown_events(n + 1) = event
    grown_remaining(n + 1) = remaining

    call move_alloc(grown_events, session%pending_events)
    call move_alloc(grown_remaining, session%pending_remaining)
  end subroutine append_pending_event

  subroutine remove_pending_event(session, index)
    type(watch_session), intent(inout) :: session
    integer, intent(in) :: index
    type(watch_event), allocatable :: kept_events(:)
    integer, allocatable :: kept_remaining(:)
    integer :: n

    n = size(session%pending_events)
    if (index < 1 .or. index > n) return

    allocate(kept_events(n - 1))
    allocate(kept_remaining(n - 1))

    if (index > 1) then
      kept_events(1:index - 1) = session%pending_events(1:index - 1)
      kept_remaining(1:index - 1) = session%pending_remaining(1:index - 1)
    end if

    if (index < n) then
      kept_events(index:n - 1) = session%pending_events(index + 1:n)
      kept_remaining(index:n - 1) = session%pending_remaining(index + 1:n)
    end if

    call move_alloc(kept_events, session%pending_events)
    call move_alloc(kept_remaining, session%pending_remaining)
  end subroutine remove_pending_event

  integer function find_related_pending_event(pending_events, incoming) result(index_found)
    type(watch_event), intent(in) :: pending_events(:)
    type(watch_event), intent(in) :: incoming
    integer :: i

    index_found = 0
    do i = 1, size(pending_events)
      if (events_related(pending_events(i), incoming)) then
        index_found = i
        return
      end if
    end do
  end function find_related_pending_event

  logical function events_related(left, right) result(related)
    type(watch_event), intent(in) :: left
    type(watch_event), intent(in) :: right

    related = .false.
    if (same_nonempty_text(left%path, right%path)) related = .true.
    if (same_nonempty_text(left%path, right%previous_path)) related = .true.
    if (same_nonempty_text(left%previous_path, right%path)) related = .true.
    if (same_nonempty_text(left%previous_path, right%previous_path)) related = .true.
  end function events_related

  logical function same_nonempty_text(left, right) result(matches)
    character(len=*), intent(in) :: left
    character(len=*), intent(in) :: right

    matches = .false.
    if (len(left) == 0 .or. len(right) == 0) return
    matches = (left == right)
  end function same_nonempty_text

  function resize_logical_flags(flags, new_size) result(resized)
    logical, intent(in) :: flags(:)
    integer, intent(in) :: new_size
    logical, allocatable :: resized(:)
    integer :: copy_count

    allocate(resized(new_size))
    resized = .false.
    copy_count = min(size(flags), new_size)
    if (copy_count > 0) resized(1:copy_count) = flags(1:copy_count)
  end function resize_logical_flags

  logical function entry_changed(previous_entry, current_entry) result(changed)
    type(watch_entry), intent(in) :: previous_entry
    type(watch_entry), intent(in) :: current_entry

    changed = .false.
    if (previous_entry%inode /= current_entry%inode) changed = .true.
    if (previous_entry%size /= current_entry%size) changed = .true.
    if (previous_entry%mtime_sec /= current_entry%mtime_sec) changed = .true.
    if (previous_entry%mtime_nsec /= current_entry%mtime_nsec) changed = .true.
    if (previous_entry%is_directory .neqv. current_entry%is_directory) changed = .true.
  end function entry_changed

  subroutine parse_snapshot_text(text, entries)
    character(len=*), intent(in) :: text
    type(watch_entry), allocatable, intent(out) :: entries(:)
    integer :: count
    integer :: i
    integer :: start
    integer :: n

    n = len(text)
    if (n == 0) then
      allocate(entries(0))
      return
    end if

    count = 0
    do i = 1, n
      if (text(i:i) == new_line("a")) count = count + 1
    end do
    if (text(n:n) /= new_line("a")) count = count + 1

    allocate(entries(count))
    count = 0
    start = 1
    do i = 1, n
      if (text(i:i) /= new_line("a")) cycle
      count = count + 1
      call parse_snapshot_line(text(start:i - 1), entries(count))
      start = i + 1
    end do

    if (start <= n) then
      count = count + 1
      call parse_snapshot_line(text(start:n), entries(count))
    end if
  end subroutine parse_snapshot_text

  subroutine parse_snapshot_line(line, entry)
    character(len=*), intent(in) :: line
    type(watch_entry), intent(out) :: entry
    integer :: tab1
    integer :: tab2
    integer :: tab3
    integer :: tab4
    integer :: tab5

    tab1 = index(line, achar(9))
    tab2 = next_tab(line, tab1 + 1)
    tab3 = next_tab(line, tab2 + 1)
    tab4 = next_tab(line, tab3 + 1)
    tab5 = next_tab(line, tab4 + 1)

    if (tab1 <= 0 .or. tab2 <= 0 .or. tab3 <= 0 .or. tab4 <= 0 .or. tab5 <= 0) then
      entry = watch_entry()
      entry%path = ""
      return
    end if

    entry%is_directory = (line(1:1) == "D")
    read(line(tab1 + 1:tab2 - 1), *) entry%inode
    read(line(tab2 + 1:tab3 - 1), *) entry%size
    read(line(tab3 + 1:tab4 - 1), *) entry%mtime_sec
    read(line(tab4 + 1:tab5 - 1), *) entry%mtime_nsec
    entry%path = line(tab5 + 1:)
  end subroutine parse_snapshot_line

  integer function next_tab(line, start_index) result(position)
    character(len=*), intent(in) :: line
    integer, intent(in) :: start_index
    integer :: offset

    if (start_index > len(line)) then
      position = 0
      return
    end if

    offset = index(line(start_index:), achar(9))
    if (offset == 0) then
      position = 0
    else
      position = start_index + offset - 1
    end if
  end function next_tab

  function buffer_to_text(buffer, count) result(text)
    character(kind=c_char), intent(in) :: buffer(:)
    integer, intent(in) :: count
    character(len=:), allocatable :: text
    integer :: i

    if (count <= 0) then
      text = ""
      return
    end if

    allocate(character(len=count) :: text)
    do i = 1, count
      text(i:i) = char(iachar(buffer(i)))
    end do
  end function buffer_to_text

  function errno_message(prefix, errnum) result(message)
    character(len=*), intent(in) :: prefix
    integer, intent(in) :: errnum
    character(len=:), allocatable :: message
    character(len=32) :: code_text

    write(code_text, '(I0)') errnum
    message = trim(prefix) // " (errno=" // trim(code_text) // ")"
  end function errno_message

  function to_c_string(str) result(buf)
    character(len=*), intent(in) :: str
    character(kind=c_char), allocatable :: buf(:)
    integer :: i
    integer :: n

    n = len(str)
    allocate(buf(n + 1))
    do i = 1, n
      buf(i) = str(i:i)
    end do
    buf(n + 1) = c_null_char
  end function to_c_string

  subroutine append_entry(entries, entry)
    type(watch_entry), allocatable, intent(inout) :: entries(:)
    type(watch_entry), intent(in) :: entry
    type(watch_entry), allocatable :: grown(:)
    integer :: n

    n = size(entries)
    allocate(grown(n + 1))
    if (n > 0) grown(1:n) = entries
    grown(n + 1) = entry
    call move_alloc(grown, entries)
  end subroutine append_entry

  subroutine append_event(events, kind, path, previous_path, is_directory)
    type(watch_event), allocatable, intent(inout) :: events(:)
    integer, intent(in) :: kind
    character(len=*), intent(in) :: path
    character(len=*), intent(in) :: previous_path
    logical, intent(in) :: is_directory
    type(watch_event), allocatable :: grown(:)
    integer :: n

    n = size(events)
    allocate(grown(n + 1))
    if (n > 0) grown(1:n) = events
    grown(n + 1)%kind = kind
    grown(n + 1)%is_directory = is_directory
    grown(n + 1)%path = path
    if (len(previous_path) > 0) then
      grown(n + 1)%previous_path = previous_path
    else
      grown(n + 1)%previous_path = ""
    end if
    call move_alloc(grown, events)
  end subroutine append_event

  subroutine append_event_object(events, event)
    type(watch_event), allocatable, intent(inout) :: events(:)
    type(watch_event), intent(in) :: event
    type(watch_event), allocatable :: grown(:)
    integer :: n

    n = size(events)
    allocate(grown(n + 1))
    if (n > 0) grown(1:n) = events
    grown(n + 1) = event
    call move_alloc(grown, events)
  end subroutine append_event_object

  subroutine sort_entries(entries)
    type(watch_entry), intent(inout) :: entries(:)
    type(watch_entry) :: temp
    integer :: i
    integer :: j

    do i = 1, size(entries) - 1
      do j = i + 1, size(entries)
        if (entry_less(entries(j)%path, entries(i)%path)) then
          temp = entries(i)
          entries(i) = entries(j)
          entries(j) = temp
        end if
      end do
    end do
  end subroutine sort_entries

  subroutine sort_events(events)
    type(watch_event), intent(inout) :: events(:)
    type(watch_event) :: temp
    integer :: i
    integer :: j

    do i = 1, size(events) - 1
      do j = i + 1, size(events)
        if (event_less(events(j), events(i))) then
          temp = events(i)
          events(i) = events(j)
          events(j) = temp
        end if
      end do
    end do
  end subroutine sort_events

  logical function entry_less(left, right) result(is_less)
    character(len=*), intent(in) :: left
    character(len=*), intent(in) :: right
    integer :: i
    integer :: limit

    limit = min(len(left), len(right))
    do i = 1, limit
      if (left(i:i) < right(i:i)) then
        is_less = .true.
        return
      end if
      if (left(i:i) > right(i:i)) then
        is_less = .false.
        return
      end if
    end do

    is_less = (len(left) < len(right))
  end function entry_less

  integer function max_string_length(values) result(max_len)
    character(len=*), intent(in) :: values(:)
    integer :: i

    max_len = 1
    do i = 1, size(values)
      max_len = max(max_len, len(values(i)))
    end do
  end function max_string_length

  logical function event_less(left, right) result(is_less)
    type(watch_event), intent(in) :: left
    type(watch_event), intent(in) :: right

    if (left%path /= right%path) then
      is_less = entry_less(left%path, right%path)
      return
    end if

    if (left%kind /= right%kind) then
      is_less = (left%kind < right%kind)
      return
    end if

    is_less = entry_less(left%previous_path, right%previous_path)
  end function event_less

end module fgof_watch
