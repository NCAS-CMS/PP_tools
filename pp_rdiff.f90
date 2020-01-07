program pp_rdiff

  ! Author: W. McGinty
  ! NCAS-CMS
  ! 2/Jun/2008
  !
  ! Checks the relative difference in the floating point part of
  ! a PP file is less than a specified percentage
  !

  Use PP_type

  implicit none


  type(PP)   ::  PP1, PP2


  logical                           ::  end_of_file

  integer                           ::  io_status

  integer                           :: i, j

  integer                           ::  lun1, lun2

  character(len=200)                ::  pp_file1
  character(len=200)                ::  pp_file2

  character(len=200)                :: thresh_str      ! for threshold

  integer                           ::  num_args

  real                              :: threshold

  integer                           :: size1, size2

  real                              :: max_rel_diff, r_diff


  num_args = command_argument_count()

  if ( (num_args /= 2) .and. (num_args /= 3)) then
     print *, " pp_rdiff, version 2.1"
     print *, " An NCAS Computational Modelling Support Tool."
     print *, " Checks whether two PP files differ significantly."
     print *, " Usage:"
     print *, " pp_rdiff <PP file1> <PP file2> [threshold]"
     print *, " A threshold of 1.0e-7 is used if it is not specified." 
     stop
  end if

  ! get the file names and remove the trailing spaces
  call get_command_argument(1,pp_file1)
  pp_file1 = trim(pp_file1)

  call get_command_argument(2,pp_file2)
  pp_file2 = trim(pp_file2)

  ! Threshold is 1.0 e-7 if not specified
  if ( num_args == 3 ) then   
     call get_command_argument(3, thresh_str)
     read(thresh_str,iostat = io_status, fmt=*)threshold
     if ( io_status > 0 ) then
        write(*,*)" Error reading threshold."
        write(*,*)' IO status = ', io_status
        stop
     end if

     if ( threshold <= 0 ) then
        write(*,*)" Threshold must be positive."
        stop
     end if
  else
     threshold = 1.0e-7
  end if

  write(*,*) "File 1 ", pp_file1(1:len_trim(pp_file1))
  write(*,*) "File 2 ", trim(pp_file2)
  write(*,*) "Threshold ", threshold

  write(*,*) "Checking sizes - this may take some time ..."

  ! Check that the files are the same size

  size1 = pp_size(pp_file1)
  size2 = pp_size(pp_file2)

  if ( size1 /= size2 ) then
     write(*,*) 'PP Files have different sizes.'
     write(*,*)' File 1 has size ', size1
     write(*,*)' File 2 has size ', size2
     stop
  end if

  write(*,*) "Comparing differences ..."
  lun1 = 13
  open(lun1, file=pp_file1, status = "old", form="unformatted", action="read",iostat=io_status)
  if ( io_status > 0 ) then
     write(*,*)" Error reading file 1:", pp_file1
     write(*,*)' IO status = ', io_status
     stop
  end if


  lun2 = 14
  open(lun2, file=pp_file2, status = "old", form="unformatted", action="read", iostat=io_status)
  if ( io_status > 0 ) then
     write(*,*)" Error reading file 2:", pp_file2
     write(*,*)' IO status = ', io_status
     stop
  end if

  max_rel_diff = 0

  ! Now compare the files header by header and data record by record 
  end_of_file = .false.
  do while ( .not. end_of_file )

     ! Do file 1
     call PP_read(lun1, io_status, PP1)
     if ( io_status > 0 ) stop "Error reading file 1"
     if ( io_status < 0 ) then
        end_of_file = .true.
        exit
     end if

     if( .not. valid_pp(PP1) )stop "Invalid PP file 1"


     allocate(PP1%data(PP1%LBNPT, PP1%LBROW), stat = io_status)
     if ( io_status /= 0 ) stop "Allocation error"
     read(lun1, iostat = io_status) PP1%data
     if ( io_status > 0 ) then
        write(*,*)" io_status = ", io_status
        stop "Read 3 error"
     end if
     if ( io_status < 0 ) then
        end_of_file = .true.
        exit
     end if

     ! Repeat for file 2
     call PP_read(lun2, io_status, PP2)
     if ( io_status > 0 ) stop "Error reading file 2"
     if ( io_status < 0 ) then
        end_of_file = .true.
        exit
     end if

     if ( .not. compare_PP_hdr(PP1, PP2) ) then
        write(*,*)'Headers are different.'
        stop
     end if


     allocate(PP2%data(PP1%LBNPT, PP1%LBROW), stat = io_status)
     if ( io_status /= 0 ) stop "Allocation error"
     read(lun2, iostat = io_status) PP2%data
     if ( io_status > 0 ) then
        write(*,*)" io_status = ", io_status
        stop "Read 3 error"
     end if
     if ( io_status < 0 ) then
        end_of_file = .true.
        exit
     end if

     do i = 1, PP1%LBNPT
        do j = 1, PP1%LBROW
           r_diff = rdiff(PP1%data(i,j), pp2%data(i,j))
           max_rel_diff = max(max_rel_diff, abs(r_diff) )
           if ( abs(r_diff) > threshold ) then
              write(*,*) ' Files are different'
              write(*,*)' The relative difference exceeds ', threshold
              write(*,*)" The maximum was ", max_rel_diff
              stop
           end if
        end do
     end do

     deallocate(PP1%data, stat = io_status)
     if ( io_status /= 0 ) stop "De-allocation error 1"     
     deallocate(PP2%data, stat = io_status)
     if ( io_status /= 0 ) stop "De-allocation error 2"     

  end do

  ! If we get to here then the files must be pretty similar
  write(*,*)" The absolute relative difference between the files is less than ", threshold
  write(*,*)" The maximum was ", max_rel_diff

  close(lun1)
  close(lun2)

contains


  ! Compute the relative difference in x, y
  real function rdiff(x, y)
    implicit none
    real, intent(in)        :: x, y
    if ( x == 0 .and. y == 0) then
       rdiff = 0
    else
       rdiff = (x-y)/max(abs(x),abs(y))
    end if
  end function rdiff

  ! compute the size of a PP file
  ! This is defined to be the number of headers, plus the Sum of Nx X Ny
  !
  integer function pp_size(fn)
    implicit none
    character(len=200), intent(in)   :: fn        ! the filename

    integer lun

    type(PP)                          :: PPobj

    logical                           ::  end_of_file

    integer                           ::  io_status


    lun = 21
    open(lun, file=fn, status = "old", form="unformatted", action="read", iostat=io_status)
    if ( io_status > 0) then
       write(*,*)"Error opening file ", trim(fn)
       write(*,*) "Io stat = ", io_status
       stop
    end if

    pp_size = 0

    end_of_file = .false.
    do while ( .not. end_of_file )
       call PP_read(lun, io_status, PPobj)
       if ( io_status > 0 ) then
          write(*,*)"Error reading file ", trim(fn)
          stop
       end if
       if ( io_status < 0 ) then
          end_of_file = .true.
          exit
       end if

       if ( .not. valid_pp(PPobj) ) then
          write(*,*)"Invalid PP file ", trim(fn)
          stop 
       end if


       pp_size = pp_size + 1 + PPobj%LBROW*PPobj%LBNPT

       call skip_pp_data(lun)

    end do

    close(unit=lun, iostat = io_status)
    if ( io_status > 0 ) then
       write(*,*) "Error closing file ", trim(fn)
       write(*,*) " Iostat = ", io_status
       stop
    end if
  end function pp_size



end program pp_rdiff


