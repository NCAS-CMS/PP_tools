program pp_qdiff
  ! Author: W. McGinty
  ! NCAS-CMS
  ! 5/Jun/2008
  !
  ! subtract two PP files of the same size

  Use PP_type

  implicit none

  type(PP)                          ::  PP1, PP2

  logical                           ::  end_of_file

  integer                           ::  Nx, Ny
  integer                           ::  io_status

  integer                           ::  lun1, lun2

  character(len=200)                ::  pp_file1
  character(len=200)                ::  pp_file2
  character(len=200)                ::  pp_outfile
  integer                           ::  num_args

  integer lun_out

  integer                           :: size1, size2  ! of the PP input files

  integer                           :: len, cmd_status

  num_args = command_argument_count()

  if ( num_args /= 3) then
     print *, " pp_qdiff, version 2.1"
     print *, " An NCAS Computational Modelling Support Tool."
     print *, " Computes the difference between two PP files of the same size."
     print *, " Usage:"
     print *, " pp_qdiff <PP file1> <PP file2> <PP outfile>" 
     stop
  end if

  ! get the file names and remove the trailing spaces
  call get_command_argument(1,pp_file1, len, cmd_status)
  if ( cmd_status /= 0 ) then
     write(*,*)"Incorrect argument list."
     stop
  end if
  pp_file1 = trim(pp_file1)

  call get_command_argument(2,pp_file2, len, cmd_status)
  if ( cmd_status /= 0 ) then
     write(*,*)"Incorrect argument list."
     stop
  end if
  pp_file2 = trim(pp_file2)

  call get_command_argument(3,pp_outfile, len, cmd_status)
  if ( cmd_status /= 0 ) then
     write(*,*)"Incorrect argument list."
     stop
  end if
  pp_outfile = trim(pp_outfile)

  ! check that the output file is not the same as either of the input
  ! files
  if ( pp_file1 == pp_outfile ) then
     write(*,*)" Output file name must be different from file 1"
     stop
  end if

  if ( pp_file2 == pp_outfile ) then
     write(*,*)" Output file name must be different from file 2"
     stop
  end if

  ! Check that the files are the same size

  size1 = pp_size(pp_file1)
  size2 = pp_size(pp_file2)

  if ( size1 /= size2 ) then
     write(*,*) 'PP input files have different sizes.'
     write(*,*)' File 1 has size ', size1
     write(*,*)' File 2 has size ', size2
     stop
  end if

  lun1 = 13
  open(lun1, file=pp_file1, status = "old", form="unformatted", action="read", iostat=io_status)
  if ( io_status /= 0) then
     write(*,*)"Error opening file ", pp_file1(1:len_trim(pp_file1))
     write(*,*) "Io stat = ", io_status
     stop
  end if


  lun2 = 14
  open(lun2, file=pp_file2, status = "old", form="unformatted", action="read", iostat=io_status)
  if ( io_status /= 0) then
     write(*,*)"Error opening file ", pp_file2(1:len_trim(pp_file2))
     write(*,*) "Io stat = ", io_status
     stop
  end if

  lun_out = 23
  open(lun_out, file=pp_outfile, status="replace", form="unformatted")

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

     !Compute the difference and output it with a file1 header
     call PP_write(lun_out, io_status, PP1)
     if( io_status > 0 ) then
        write(*,*)" Error writing output file."
        stop
     end if
     write(lun_out, iostat = io_status)PP1%data - PP2%data
     if( io_status > 0 ) then
        write(*,*)" Error writing output file."
        stop
     end if


     deallocate(PP1%data, stat = io_status)
     if ( io_status /= 0 ) stop "De-allocation error"     
     deallocate(PP2%data, stat = io_status)
     if ( io_status /= 0 ) stop "De-allocation error"     
  end do


  close(lun1)
  close(lun2)
  close(lun_out)


contains

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


end program pp_qdiff
