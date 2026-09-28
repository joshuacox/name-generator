program name_generator
  implicit none

  character(len=512) :: separator, noun_folder, adj_folder
  character(len=512) :: noun_file, adj_file, counto_str, debug_str, tmp_path
  integer :: counto, stat, i, num_nouns, num_adjs, idx_noun, idx_adj
  real :: r1, r2
  logical :: is_debug

  character(len=128), allocatable :: nouns(:)
  character(len=128), allocatable :: adjectives(:)

  ! Random seed
  call init_random_seed()

  ! Read environment variables
  call get_environment_variable("SEPARATOR", separator, status=stat)
  if (stat /= 0 .or. len_trim(separator) == 0) separator = "-"

  call get_environment_variable("NOUN_FOLDER", noun_folder, status=stat)
  if (stat /= 0 .or. len_trim(noun_folder) == 0) noun_folder = "nouns"

  call get_environment_variable("ADJ_FOLDER", adj_folder, status=stat)
  if (stat /= 0 .or. len_trim(adj_folder) == 0) adj_folder = "adjectives"

  call get_environment_variable("NOUN_FILE", noun_file, status=stat)
  if (stat /= 0 .or. len_trim(noun_file) == 0) then
    noun_file = pick_random_file(noun_folder)
  end if

  call get_environment_variable("ADJ_FILE", adj_file, status=stat)
  if (stat /= 0 .or. len_trim(adj_file) == 0) then
    adj_file = pick_random_file(adj_folder)
  end if

  ! counto
  call get_environment_variable("counto", counto_str, status=stat)
  counto = 24
  if (stat == 0 .and. len_trim(counto_str) > 0) then
    read(counto_str, *, iostat=stat) counto
    if (stat /= 0) counto = 24
  else
    tmp_path = get_tput_lines()
    if (len_trim(tmp_path) > 0) then
      read(tmp_path, *, iostat=stat) counto
      if (stat /= 0) counto = 24
    end if
  end if

  call get_environment_variable("DEBUG", debug_str, status=stat)
  is_debug = (stat == 0 .and. trim(debug_str) == "true")

  ! Read words into memory
  call read_wordlist(noun_file, nouns, num_nouns)
  call read_wordlist(adj_file, adjectives, num_adjs)

  ! Lowercase all nouns once
  do i = 1, num_nouns
    call to_lower(nouns(i))
  end do

  ! Main loop
  do i = 1, counto
    call random_number(r1)
    call random_number(r2)
    idx_noun = 1 + int(r1 * real(num_nouns))
    if (idx_noun > num_nouns) idx_noun = num_nouns
    idx_adj = 1 + int(r2 * real(num_adjs))
    if (idx_adj > num_adjs) idx_adj = num_adjs

    if (is_debug) then
      write(0, '(A)') trim(adjectives(idx_adj))
      write(0, '(A)') trim(nouns(idx_noun))
      write(0, '(A)') trim(adj_file)
      write(0, '(A)') trim(adj_folder)
      write(0, '(A)') trim(noun_file)
      write(0, '(A)') trim(noun_folder)
      write(0, '(I0, A, I0)') (i - 1), " > ", counto
    end if

    write(*, '(A)') trim(adjectives(idx_adj)) // trim(separator) // trim(nouns(idx_noun))
  end do

contains

  subroutine init_random_seed()
    integer :: n, clock, i
    integer, allocatable :: seed(:)
    call random_seed(size = n)
    allocate(seed(n))
    call system_clock(count=clock)
    seed = clock + 37 * (/ (i - 1, i = 1, n) /)
    call random_seed(put = seed)
    deallocate(seed)
  end subroutine init_random_seed

  subroutine to_lower(str)
    character(len=*), intent(inout) :: str
    integer :: k, ic
    do k = 1, len_trim(str)
      ic = iachar(str(k:k))
      if (ic >= iachar('A') .and. ic <= iachar('Z')) then
        str(k:k) = achar(ic + 32)
      end if
    end do
  end subroutine to_lower

  function pick_random_file(folder) result(res)
    character(len=*), intent(in) :: folder
    character(len=512) :: res
    character(len=1024) :: cmd
    integer :: pid, stat, u
    character(len=32) :: pid_str

    pid = 0
    call get_environment_variable("PPID", pid_str, status=stat)
    if (stat /= 0) pid_str = "0"

    write(cmd, '(A, A, A, A, A)') "find """, trim(folder), &
         """ -type f 2>/dev/null | shuf -n 1 > /tmp/namgen_file_", trim(adjustl(pid_str)), ".tmp"
    call execute_command_line(trim(cmd), wait=.true., exitstat=stat)

    open(newunit=u, file="/tmp/namgen_file_" // trim(adjustl(pid_str)) // ".tmp", &
         status="old", action="read", iostat=stat)
    if (stat == 0) then
      read(u, '(A)', iostat=stat) res
      close(u, status="delete")
    else
      res = ""
    end if
  end function pick_random_file

  function get_tput_lines() result(res)
    character(len=32) :: res
    integer :: stat, u
    call execute_command_line("tput lines 2>/dev/null > /tmp/namgen_tput.tmp", wait=.true., exitstat=stat)
    open(newunit=u, file="/tmp/namgen_tput.tmp", status="old", action="read", iostat=stat)
    if (stat == 0) then
      read(u, '(A)', iostat=stat) res
      close(u, status="delete")
    else
      res = ""
    end if
  end function get_tput_lines

  subroutine read_wordlist(filepath, words, total)
    character(len=*), intent(in) :: filepath
    character(len=128), allocatable, intent(out) :: words(:)
    integer, intent(out) :: total
    character(len=128) :: line
    integer :: u, stat, count

    count = 0
    open(newunit=u, file=trim(filepath), status="old", action="read", iostat=stat)
    if (stat /= 0) then
      write(0, '(A, A)') "Cannot open file: ", trim(filepath)
      stop 1
    end if

    do
      read(u, '(A)', iostat=stat) line
      if (stat /= 0) exit
      if (len_trim(adjustl(line)) > 0) count = count + 1
    end do

    if (count == 0) then
      write(0, '(A, A)') "Empty wordlist file: ", trim(filepath)
      close(u)
      stop 1
    end if

    allocate(words(count))
    rewind(u)
    total = 0
    do
      read(u, '(A)', iostat=stat) line
      if (stat /= 0) exit
      line = adjustl(line)
      if (len_trim(line) > 0) then
        total = total + 1
        words(total) = trim(line)
      end if
    end do
    close(u)
  end subroutine read_wordlist

end program name_generator
