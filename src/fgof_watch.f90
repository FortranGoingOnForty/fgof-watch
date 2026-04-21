module fgof_watch
  use, intrinsic :: iso_c_binding, only : c_associated, c_char, c_f_pointer, c_int, c_null_char, c_null_ptr, c_ptr, c_size_t
  use fgof_watch_types, only : &
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

    if (present(options)) then
      session%options = options
    end if

    session%root = root
    session%active = (len(root) > 0)

    if (.not. session%active) then
      allocate(session%entries(0))
      return
    end if

    call collect_snapshot(root, session%options%recursive, session%entries)
  end subroutine init_watch

  function poll_watch(session) result(events)
    type(watch_session), intent(inout) :: session
    type(watch_event), allocatable :: events(:)
    type(watch_entry), allocatable :: current_entries(:)

    if (.not. session%active) then
      allocate(events(0))
      return
    end if

    call collect_snapshot(session%root, session%options%recursive, current_entries)
    events = diff_snapshots(session%entries, current_entries)
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

    session%options = watch_options()
    session%active = .false.
  end subroutine reset_watch

  subroutine collect_snapshot(root, recursive, entries)
    character(len=*), intent(in) :: root
    logical, intent(in) :: recursive
    type(watch_entry), allocatable, intent(out) :: entries(:)
    type(c_ptr) :: raw_ptr
    integer(c_int) :: status
    integer(c_size_t) :: raw_len
    character(kind=c_char), allocatable :: c_root(:)
    character(kind=c_char), pointer :: raw_chars(:)
    character(len=:), allocatable :: text

    c_root = to_c_string(root)
    raw_ptr = c_null_ptr
    raw_len = 0_c_size_t

    status = fgof_watch_collect_snapshot_c(c_root, merge(1_c_int, 0_c_int, recursive), raw_ptr, raw_len)
    if (status /= 0_c_int) then
      allocate(entries(0))
      if (c_associated(raw_ptr)) call fgof_watch_free_buffer_c(raw_ptr)
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
    call sort_entries(entries)
  end subroutine collect_snapshot

  function diff_snapshots(previous_entries, current_entries) result(events)
    type(watch_entry), intent(in) :: previous_entries(:)
    type(watch_entry), intent(in) :: current_entries(:)
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

    events = build_event_batch(created, modified, removed)
    call sort_events(events)
  end function diff_snapshots

  function build_event_batch(created, modified, removed) result(events)
    type(watch_entry), intent(in) :: created(:)
    type(watch_entry), intent(in) :: modified(:)
    type(watch_entry), intent(in) :: removed(:)
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
        call append_event(events, FGOF_WATCH_EVT_MOVED, created(j)%path, removed(i)%path, created(j)%is_directory)
        created_used(j) = .true.
        removed_used(i) = .true.
        exit
      end do
    end do

    do i = 1, size(created)
      if (created_used(i)) cycle
      call append_event(events, FGOF_WATCH_EVT_CREATED, created(i)%path, "", created(i)%is_directory)
    end do

    do i = 1, size(modified)
      call append_event(events, FGOF_WATCH_EVT_MODIFIED, modified(i)%path, "", modified(i)%is_directory)
    end do

    do i = 1, size(removed)
      if (removed_used(i)) cycle
      call append_event(events, FGOF_WATCH_EVT_REMOVED, removed(i)%path, "", removed(i)%is_directory)
    end do
  end function build_event_batch

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
