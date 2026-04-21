module watch_test_support
  use fgof_watch_types, only : watch_event
  implicit none
  private

  public :: append_text
  public :: chmod_mode
  public :: ensure_clean_dir
  public :: expect_no_events
  public :: expect_single_event
  public :: make_dir
  public :: move_path
  public :: remove_path
  public :: remove_tree
  public :: write_text

contains

  subroutine ensure_clean_dir(path)
    character(len=*), intent(in) :: path

    call remove_tree(path)
    call make_dir(path)
  end subroutine ensure_clean_dir

  subroutine chmod_mode(path, mode)
    character(len=*), intent(in) :: path
    character(len=*), intent(in) :: mode

    call run_command("chmod " // mode // " " // path)
  end subroutine chmod_mode

  subroutine make_dir(path)
    character(len=*), intent(in) :: path

    call run_command("mkdir -p " // path)
  end subroutine make_dir

  subroutine remove_tree(path)
    character(len=*), intent(in) :: path

    call run_command("rm -rf " // path)
  end subroutine remove_tree

  subroutine remove_path(path)
    character(len=*), intent(in) :: path

    call run_command("rm -f " // path)
  end subroutine remove_path

  subroutine move_path(source, destination)
    character(len=*), intent(in) :: source
    character(len=*), intent(in) :: destination

    call run_command("mv " // source // " " // destination)
  end subroutine move_path

  subroutine write_text(path, text)
    character(len=*), intent(in) :: path
    character(len=*), intent(in) :: text
    integer :: unit

    open(newunit=unit, file=path, status="replace", action="write")
    write(unit, "(A)", advance="no") text
    close(unit)
  end subroutine write_text

  subroutine append_text(path, text)
    character(len=*), intent(in) :: path
    character(len=*), intent(in) :: text
    integer :: unit

    open(newunit=unit, file=path, status="old", position="append", action="write")
    write(unit, "(A)", advance="no") text
    close(unit)
  end subroutine append_text

  subroutine expect_no_events(events, message)
    type(watch_event), intent(in) :: events(:)
    character(len=*), intent(in) :: message

    if (size(events) /= 0) error stop trim(message)
  end subroutine expect_no_events

  subroutine expect_single_event(events, kind, path, previous_path, is_directory, message)
    type(watch_event), intent(in) :: events(:)
    integer, intent(in) :: kind
    character(len=*), intent(in) :: path
    character(len=*), intent(in) :: previous_path
    logical, intent(in) :: is_directory
    character(len=*), intent(in) :: message

    if (size(events) /= 1) error stop trim(message) // ": expected one event"
    if (events(1)%kind /= kind) error stop trim(message) // ": wrong event kind"
    if (events(1)%path /= path) error stop trim(message) // ": wrong event path"
    if (events(1)%previous_path /= previous_path) error stop trim(message) // ": wrong event previous path"
    if (events(1)%is_directory .neqv. is_directory) error stop trim(message) // ": wrong directory flag"
  end subroutine expect_single_event

  subroutine run_command(command)
    character(len=*), intent(in) :: command
    integer :: exitstat

    call execute_command_line(command, exitstat=exitstat)
    if (exitstat /= 0) error stop "command failed: " // trim(command)
  end subroutine run_command

end module watch_test_support
