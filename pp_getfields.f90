program pp_getfields

  ! Author: W. McGinty
  ! NCAS-CMS
  ! 6/Jun/2008
  !

  use PP_type
  use stash_list          ! defines storage for list, and the test function

  implicit none

  type(PP)                         :: PPobj

  logical                           ::  end_of_file

  integer                           ::  stash_no

  integer                           ::  io_status

  integer                           :: i, j

  integer                           ::  lun1, lun2

  character(len=200)                ::  pp_file1
  character(len=200)                ::  pp_file2

  character(len=200)                :: stash_str      ! for STASH number translation

  integer                           ::  num_args
  integer                           ::  len, cmd_status

  num_args = command_argument_count()

  if ( (num_args < 2) ) then
     print *, " pp_getfields, version 2.1"
     print *, " An NCAS Computational Modelling Support Tool."
     print *, " Extracts fields specified by STASH number."
     print *, " Usage:"
     print *, " pp_getfields <input PP file> <output PP file> <STASH list separated by spaces>"
     stop
  end if

  ! get the file names and remove the trailing spaces
  call get_command_argument(1,pp_file1, len, cmd_status)
  if ( cmd_status /= 0 ) then
     write(*,*)" Incorrect argument list."
     stop
  end if

  pp_file1 = trim(pp_file1)

  call get_command_argument(2,pp_file2, len, cmd_status)
  if ( cmd_status /= 0 ) then
     write(*,*)" Incorrect argument list."
     stop
  end if
  pp_file2 = trim(pp_file2)

  num_stash = num_args - 2

  if ( num_stash <= 0 ) then
     write(*,*)" No STASH numbers have been specified."
     stop
  end if

  if (num_stash > max_num_stash ) then
     write(*,*) "Can't handle more than ", max_num_stash, " STASH numbers."
     stop
  end if

  !get list of STASH numbers from command line

  do i = 1, num_stash
     call get_command_argument(i+2, stash_str, len, cmd_status)
     if ( cmd_status /= 0 ) then
        write(*,*)" Incorrect argument list."
        stop
     end if
     read(stash_str,iostat = io_status, fmt=*)STASH(i)
     if ( io_status > 0 ) then
        write(*,*)" Error reading STASH ", stash_str
        write(*,*)' IO status = ', io_status
        stop
     end if
     if (io_status < 0 ) then
        write(*,*)" Unexpected end of file while reading STASH list.", stash_str
        write(*,*)' IO status = ', io_status
        stop
     end if

     if ( STASH(i) <=0 ) then
        write(*,*)"Error reading STASH list."
        stop
     end if
  end do

  ! Now open the files
  lun1 = 13
  open(lun1, file=pp_file1, status = "old", form="unformatted", action="read", iostat=io_status)
  if ( io_status /= 0) then
     write(*,*)"Error opening file ", pp_file1(1:len_trim(pp_file1))
     write(*,*) "Io stat = ", io_status
     stop
  end if


  lun2 = 14
  open(lun2, file=pp_file2, status = "replace", form="unformatted", action="write", iostat=io_status)
  if ( io_status /= 0) then
     write(*,*)"Error opening file ", pp_file2(1:len_trim(pp_file2))
     write(*,*) "Io stat = ", io_status
     stop
  end if


  ! Now filter the input file for the required STASH numbers
  end_of_file = .false.
  do while ( .not. end_of_file )

     ! Read the header
     call PP_read(lun1, io_status, PPobj)
     if ( io_status > 0 ) stop "Error reading file 1"
     if ( io_status < 0 ) then
        end_of_file = .true.
        exit
     end if

     if ( .not. valid_PP(PPobj) ) then
        write(*,*)" Invlaid PP file."
        stop
     end if


     stash_no = PPobj%LBUSER(4)  ! Not very clear, but that's PP format!

     ! Read the data 
     allocate(PPobj%data(PPobj%LBNPT, PPobj%LBROW), stat = io_status)
     if ( io_status /= 0 ) stop "Allocation error"

     read(lun1, iostat = io_status) PPobj%data
     if ( io_status > 0 ) then
        write(*,*)" Error reading PP file data, io_status = ", io_status
        stop
     end if
     if ( io_status < 0 ) then
        end_of_file = .true.
        exit
     end if

     if ( wanted_STASH( stash_no ) ) then
        ! Write the header
        call PP_write(lun2, io_status, PPobj)
        if ( io_status > 0 ) stop "Error writing output file"
        ! Write the data
        write(lun2, iostat = io_status) PPobj%data
        if ( io_status > 0 ) then
           write(*,*)" io_status = ", io_status
           stop "Read 3 error"
        end if

     end if

     deallocate(PPobj%data, stat = io_status)
     if ( io_status /= 0 ) stop "De-allocation error" 

  end do

  close(lun1)
  close(lun2)


end program pp_getfields


