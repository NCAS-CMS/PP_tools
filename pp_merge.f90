program pp_merge

  ! Author: W. McGinty
  ! NCAS-CMS
  ! 25 Nov 2008
  !
  ! An NCAS Computational Modelling Support Tool.
  !
  !  Merges (interleaves) PP files to produce a PP file in time order.
  ! This is required for the NDdiag program.
  !
  ! Usage:
  !  pp_merge <output PP file> <list of input PP files, space separated>
  !
  ! Process each input file in turn
  ! The following assumes that the input files themselves are in
  ! time order.
  !Find the earliest time across all the inputs
  !This is the current time
  !While inputs remaining do 
  !  For each input file,
  !     output each data field so long as time = current time
  !     when time changes, next_current time = min( next_currenttime, time)
  !     if ( no more fields ) close the file and remove it from the list
  !  end for
  !  current_time = next current time
  !end while
  !
  ! The process is a single pass through each of the files.  At any point
  ! we hold a list of the headers as these have the time information and also
  ! the position in the file is recorded in the logical unit number.
  !

  implicit none

  !PP header 
  type PP_header
     integer                           :: IPP(45)
     real                              :: RPP(19)
  end type PP_header

  integer                           :: io_status

  integer                           :: i, j


  integer, parameter                :: max_files = 100
  integer                           :: lun_out
  integer                           :: lun(max_files)  !for the inputs

  character(len=200)                :: pp_file_o
  character(len=200)                :: pp_file_i(max_files)

  type (PP_header)                     :: hdr(max_files)

  logical                           :: file_remaining(max_files)

  integer                           :: num_args
  integer                           :: iargc          ! standard function

  integer                           :: num_ips    ! number of input files

  integer                           :: current_time, next_current_time
  logical                           :: time_changed
  logical                           :: end_of_file
  integer                           :: new_time

  integer                           :: len , cmd_status

  num_args = command_argument_count()

  if ( (num_args < 2) ) then
     print *, " pp_merge, version 2.1"
     print *, " An NCAS Computational Modelling Support Tool."
     print *, " Merges (interleaves) PP files to produce a PP file in time order."
     print *, " Usage:"
     print *, " pp_merge <output PP file> <list of input PP files, space separated>"
     stop
  end if


  ! get the output file name and remove the trailing spaces
  call get_command_argument(1,pp_file_o, len, cmd_status)
  if ( cmd_status /= 0 ) then
     write(*,*) "Incorrect argument list."
     stop
  end if
  pp_file_o = trim(pp_file_o)

  lun_out = get_lun()
  open(lun_out, file=pp_file_o, status = "replace", form="unformatted", action="write", iostat=io_status)
  if ( io_status /= 0) then
     write(*,*)"Error opening file ", pp_file_o(1:len_trim(pp_file_o))
     write(*,*) "Io stat = ", io_status
     stop
  end if
  write(*,*)"Lun = ", lun_out,", file ", pp_file_o(1:len_trim(pp_file_o))


  num_ips = num_args - 1

  if ( num_ips <= 0 ) then
     write(*,*)" No input files have been specified."
     stop
  end if

  if (num_ips > max_files ) then
     write(*,*) "Can't handle more than ", max_files, " files."
     stop
  end if

  !get list of input file names from command line

  do i = 1, num_ips
     call get_command_argument(i+1, pp_file_i(i), len, cmd_status)
     if ( cmd_status /= 0 ) then
        write(*,*) "Incorrect argument list."
        stop
     end if
  end do

  ! CHECK THAT OP DOES NOT CLASH WITH ANY IP
  do i = 1, num_ips
     if ( pp_file_o == pp_file_i(i) ) then
        write(*,*)" Input and output files must differ."
        stop
     end if
  end do

  ! Now open the input files
  do i = 1, num_ips
     lun(i) = get_lun()
     if ( lun(i) < 0 ) then
        write(*,*)" Too many files.  Will do a partial merge with ", i-1, "files."
        num_ips = i-1
        ! WATCH IT!
     end if

     open(lun(i), file=pp_file_i(i), status = "old", form="unformatted", action="read", iostat=io_status)
     if ( io_status /= 0) then
        write(*,*)"Error opening file ", pp_file_i(i)(1:len_trim(pp_file_i(i)))
        write(*,*) "Io stat = ", io_status
        stop
     end if
     write(*,*)"Lun = ", lun(i),", file ", pp_file_i(i)(1:len_trim(pp_file_i(i)))
  end do

  do i = 1, num_ips
     file_remaining(i) = .true.
  end do

  !
  ! initialise the current time
  !
  do i = 1, num_ips
     if ( read_PP_header(lun(i), hdr(i)) ) then
        if ( i == 1) current_time = time(hdr(i))
        current_time = min(current_time, time(hdr(i)))
     end if
  end do

  ! rewind all files to the start - this simplifies the following algorithm
  do i = 1, num_ips
     rewind lun(i)
  end do


  ! read_PP_header and readdataandoutput both return false if
  ! they hit the end of file.

  ! Process the input files till none remain
  write(*,*)" Processing.  Please wait ..."
  call print_date(current_time)         ! Progress starting point

  do while (files_remaining(num_ips, file_remaining))

     next_current_time = huge(current_time)  ! infinity
     do i = 1, num_ips
        ! Output file i until time changes or it is the end of file
        time_changed = .false.
        do while (file_remaining(i) .and. .not. time_changed )

           if( read_PP_header(lun(i), hdr(i)) ) then
              new_time = time(hdr(i))
              if(new_time /= current_time ) then
                 next_current_time = min(next_current_time, new_time)
                 time_changed = .true.
                 ! Here we have read a PP_header but not output it
                 backspace lun(i)
              else
                 ! No change in time
                 ! side effect of the function is to output the PP_header and data
                 end_of_file = .not. readdataandoutput(lun(i), hdr(i), lun_out)
                 if( end_of_file )then
                    write(*,*)" Error - reached end of file while reading data."
                    write(*,*)" File ",i," on logical unit ",lun(i)
                    stop
                 end if
              end if
           else
              ! end of file reached
              file_remaining(i) = .false.    ! remove file from list
           end if

        end do
     end do ! num_ips

     ! progress indicator
     if ( next_current_time > current_time .and. (next_current_time /= huge(current_time))) then
        call print_date(next_current_time)
     end if

     ! All files examined - can update current_time
     current_time = next_current_time

  end do ! while files remaining



contains

  logical function files_remaining(num_ips, file_remaining)
    integer, intent(in)          :: num_ips
    logical, intent(in)          :: file_remaining(num_ips)
    ! Just OR all together to find out whether anything's remaining
    files_remaining = .false.
    do i = 1, num_ips
       files_remaining = files_remaining .or. file_remaining(i)
       if (files_remaining )  exit
    end do

  end function files_remaining

  ! return a free logical unit number
  integer function get_lun()
    logical exists, opened, got_one
    integer lun
    lun = 1
    got_one = .false.
    do while( .not. got_one )
       inquire(unit = lun, exist = exists, opened=opened)
       if ( exists .and. .not. opened ) then
          got_one = .true.
       else
          lun = lun + 1
       end if
    end do
    get_lun = lun
  end function get_lun

  logical function read_PP_header(lun, hdr)
    integer, intent(in)          :: lun   ! logical unit no.
    type(PP_header), intent(out)    :: hdr
    integer                      :: io_status

    read_PP_header = .true.   ! not the end of file
    read(lun, iostat=io_status) hdr%IPP, hdr%RPP
    if ( io_status > 0 ) then
       write(*,*) "Error reading file on logical unit", lun
    end if
    if ( io_status < 0 ) then
       read_PP_header = .false.   !end of file
    end if
  end function read_PP_header

  subroutine get_hdr_size(hdr, Nrows, Nptsperrow)
    type (PP_header), intent(in)    :: hdr
    integer, intent(out)         :: Nrows
    integer, intent(out)         :: Nptsperrow

    Nrows = hdr%IPP(18)
    if ( Nrows <= 0 ) then
       write(*,*) "Invalid PP header. LBROW = ", Nrows
       stop
    end if

    Nptsperrow = hdr%IPP(19)
    if ( Nptsperrow <= 0 ) then
       write(*,*) "Invalid PP header. LBNPTS = ", Nptsperrow
       stop
    end if

  end subroutine get_hdr_size

  ! calculate time parameter
  ! This stretches the date and maintains order
  integer function time(hdr)
    type (PP_header), intent(in)           :: hdr

    integer                 ::  Yr
    integer                 ::  Mon
    integer                 ::  Day
    integer                 ::  Hr
    integer                 ::  Min

    Yr = hdr%IPP(1)
    Mon= hdr%IPP(2)
    Day= hdr%IPP(3)
    Hr = hdr%IPP(4)
    Min= hdr%IPP(5)

    time = Yr*366*24*60 + &
         Mon*31*24*60 + &
         Day*24*60    + &
         Hr*60        + &
         Min

  end function time

  subroutine print_date(time)
    integer, intent(in)    :: time

    integer tim
    integer Yr, Mon, Day, Hr, Min

    tim = time
    Yr = tim/(366*24*60)
    tim = tim - Yr*366*24*60
    Mon = tim/(31*24*60)
    tim = tim - Mon*31*24*60
    Day = tim/(24*60)
    tim = tim - Day*24*60
    Hr  = tim/60
    tim = tim - Hr*60
    Min = tim

    write(*,'(i4,"/",i2.2,"/",i2.2," ",i2.2,":", i2.2 )') Yr, Mon, Day, Hr, Min
  end subroutine  print_date


  ! Read the data associated with the PP_header, then output
  ! both the PP_header and the data.
  logical function readdataandOutput(lun_in, hdr, lun_out)
    integer, intent(in)         :: lun_in
    type(PP_header), intent(in) :: hdr
    integer, intent(in)         :: lun_out

    integer                     :: Nrows, Nptsperrow

    real, dimension(:,:), allocatable ::  data

    readdataandOutput = .true.    ! not the end of the input file
    call get_hdr_size(hdr, Nrows, Nptsperrow)

    ! Read the data 
    allocate(data(Nptsperrow,Nrows), stat = io_status)
    if ( io_status /= 0 ) stop "Allocation error"
    read(lun_in, iostat = io_status) data
    if ( io_status > 0 ) then
       write(*,*)" readdataandOutput, error = ", io_status
       stop
    end if
    if ( io_status < 0 ) then
       readdataandOutput = .false.  ! end of input file
    end if

    ! write the output
    write(lun_out, iostat = io_status) hdr
    write(lun_out, iostat = io_status) data

    deallocate(data, stat = io_status)
    if ( io_status /= 0 ) then
       write(*,*) "De-allocation error" 
       stop
    end if
  end function readdataandOutput


end program pp_merge


