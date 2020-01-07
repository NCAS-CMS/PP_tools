Module PP_type

! defines Met office PP format

  type PP
     integer  :: LBYR
     integer  :: LBMON
     integer  :: LBDAT
     integer  :: LBHR
     integer  :: LBMIN
     integer  :: LBDAY
     integer  :: LBYRD
     integer  :: LBMOND
     integer  :: LBDATD
     integer  :: LBHRD

     integer  :: LBMIND
     integer  :: LBDAYD
     integer  :: LBTIM
     integer  :: LBFT
     integer  :: LBLREC
     integer  :: LBCODE
     integer  :: LBHEM
     integer  :: LBROW
     integer  :: LBNPT
     integer  :: LBEXT

     integer  :: LBPACK
     integer  :: LBREL
     integer  :: LBFC
     integer  :: LBCFC
     integer  :: LBPROC
     integer  :: LBVC
     integer  :: LBRVC
     integer  :: LBEXP
     integer  :: LBEGIN
     integer  :: LBNREC

     integer  :: LBPROJ
     integer  :: LBTYP
     integer  :: LBLEV
     integer, dimension(4)  :: LBRSVD ! reserved for future PP use
     integer  :: LBSRCE
!
! LBUSER(1)  data type 1 = real
! LBUSER(2)  Start addr in DATA
! LBUSER(3)  unused >= 4.0
! LBUSER(4)  STASH code = section number x 1000 + STASHmaster item number
! LBUSER(5)  pseudo level number
! LBUSER(6)  unused
! LBUSER(7)  internal model identifier
!
     integer, dimension(7)  :: LBUSER
!
! BRSVD(1)  vertical coordinate
! BRSVD(2)  "A" value of vertical coordinate
! BRSVD(3)  reserved
! BRSVD(4)  reserved
!
     real    :: BRSVD(4)
     real    :: BDATUM
     real    :: BACC
     real    :: BLEV
     real    :: BRLEV
     real    :: BHLEV
     real    :: BHRLEV
     real    :: BPLAT
     real    :: BPLON
     real    :: BGOR
     real    :: BZY
     real    :: BDY
     real    :: BZX
     real    :: BDX
     real    :: BMDI
     real    :: BKS

     real, dimension(:,:), allocatable :: data

  end type PP

contains

  subroutine PP_read(lun, iostatus, PPobj)
    implicit none
    integer, intent(in) :: lun
    integer, intent(out) :: iostatus
    type(PP), intent(out) :: PPobj

! Ah, Fortran!  It uses "records", so the header must be read in one lump

    integer         :: IPP(45)
    real            :: RPP(19)

   read(lun, iostat=iostatus) IPP, RPP
    if( iostatus > 0 ) then
      write(*,*)" Error reading PP file on unit ", lun
      stop
    end if
    if (iostatus < 0) return

! Now set up the PP header

    PPobj%LBYR      = IPP( 1)
    PPobj%LBMON     = IPP( 2)
    PPobj%LBDAT     = IPP( 3)
    PPobj%LBHR      = IPP( 4)
    PPobj%LBMIN     = IPP( 5)
    PPobj%LBDAY     = IPP( 6)
    PPobj%LBYRD     = IPP( 7)
    PPobj%LBMOND    = IPP( 8)
    PPobj%LBDATD    = IPP( 8)
    PPobj%LBHRD     = IPP(10)
    PPobj%LBMIND    = IPP(11)
    PPobj%LBDAYD    = IPP(12)
    PPobj%LBTIM     = IPP(13)
    PPobj%LBFT      = IPP(14)
    PPobj%LBLREC    = IPP(15)
    PPobj%LBCODE    = IPP(16)
    PPobj%LBHEM     = IPP(17)
    PPobj%LBROW     = IPP(18)
    PPobj%LBNPT     = IPP(19)
    PPobj%LBEXT     = IPP(20)
    PPobj%LBPACK    = IPP(21)
    PPobj%LBREL     = IPP(22)
    PPobj%LBFC      = IPP(23)
    PPobj%LBCFC     = IPP(24)
    PPobj%LBPROC    = IPP(25)
    PPobj%LBVC      = IPP(26)
    PPobj%LBRVC     = IPP(27)
    PPobj%LBEXP     = IPP(28)
    PPobj%LBEGIN    = IPP(29)
    PPobj%LBNREC    = IPP(30)
    PPobj%LBPROJ    = IPP(31)
    PPobj%LBTYP     = IPP(32)
    PPobj%LBLEV     = IPP(33)
    PPobj%LBRSVD(1) = IPP(34)
    PPobj%LBRSVD(2) = IPP(35)
    PPobj%LBRSVD(3) = IPP(36)
    PPobj%LBRSVD(4) = IPP(37)
    PPobj%LBSRCE    = IPP(38)
    PPobj%LBUSER(1) = IPP(39)
    PPobj%LBUSER(2) = IPP(40)
    PPobj%LBUSER(3) = IPP(41)
    PPobj%LBUSER(4) = IPP(42)
    PPobj%LBUSER(5) = IPP(43)
    PPobj%LBUSER(6) = IPP(44)
    PPobj%LBUSER(7) = IPP(45)

!  Reals ....
    PPobj%BRSVD(1) = RPP( 1)
    PPobj%BRSVD(2) = RPP( 2)
    PPobj%BRSVD(3) = RPP( 3)
    PPobj%BRSVD(4) = RPP( 4)
    PPobj%BDATUM   = RPP( 5)
    PPobj%BACC     = RPP( 6)
    PPobj%BLEV     = RPP( 7)
    PPobj%BRLEV    = RPP( 8)
    PPobj%BHLEV    = RPP( 9)
    PPobj%BHRLEV   = RPP(10)
    PPobj%BPLAT    = RPP(11)
    PPobj%BPLON    = RPP(12)
    PPobj%BGOR     = RPP(13)
    PPobj%BZY      = RPP(14)
    PPobj%BDY      = RPP(15)
    PPobj%BZX      = RPP(16)
    PPobj%BDX      = RPP(17)
    PPobj%BMDI     = RPP(18)
    PPobj%BKS      = RPP(19)


  end subroutine PP_read


  subroutine PP_write(lun, iostatus, PPobj)
    implicit none
    integer, intent(in) :: lun
    integer, intent(out) :: iostatus
    type(PP), intent(out) :: PPobj

    ! Ah, Fortran!  It uses "records", so the header must be written in one lump

    integer         :: IPP(45)
    real            :: RPP(19)


    ! Now set up the PP header

    IPP( 1) = PPobj%LBYR 
    IPP( 2) = PPobj%LBMON
    IPP( 3) = PPobj%LBDAT
    IPP( 4) = PPobj%LBHR
    IPP( 5) = PPobj%LBMIN
    IPP( 6) = PPobj%LBDAY
    IPP( 7) = PPobj%LBYRD
    IPP( 8) = PPobj%LBMOND
    IPP( 9) = PPobj%LBDATD
    IPP(10) = PPobj%LBHRD
    IPP(11) = PPobj%LBMIND
    IPP(12) = PPobj%LBDAYD
    IPP(13) = PPobj%LBTIM
    IPP(14) = PPobj%LBFT
    IPP(15) = PPobj%LBLREC
    IPP(16) = PPobj%LBCODE
    IPP(17) = PPobj%LBHEM
    IPP(18) = PPobj%LBROW
    IPP(19) = PPobj%LBNPT
    IPP(20) = PPobj%LBEXT
    IPP(21) = PPobj%LBPACK
    IPP(22) = PPobj%LBREL
    IPP(23) = PPobj%LBFC
    IPP(24) = PPobj%LBCFC
    IPP(25) = PPobj%LBPROC
    IPP(26) = PPobj%LBVC
    IPP(27) = PPobj%LBRVC
    IPP(28) = PPobj%LBEXP
    IPP(29) = PPobj%LBEGIN
    IPP(30) = PPobj%LBNREC
    IPP(31) = PPobj%LBPROJ
    IPP(32) = PPobj%LBTYP
    IPP(33) = PPobj%LBLEV
    IPP(34) = PPobj%LBRSVD(1)
    IPP(35) = PPobj%LBRSVD(2)
    IPP(36) = PPobj%LBRSVD(3)
    IPP(37) = PPobj%LBRSVD(4)
    IPP(38) = PPobj%LBSRCE
    IPP(39) = PPobj%LBUSER(1)
    IPP(40) = PPobj%LBUSER(2)
    IPP(41) = PPobj%LBUSER(3)
    IPP(42) = PPobj%LBUSER(4)
    IPP(43) = PPobj%LBUSER(5)
    IPP(44) = PPobj%LBUSER(6)
    IPP(45) = PPobj%LBUSER(7)

    !  Reals ....
    RPP( 1) = PPobj%BRSVD(1)
    RPP( 2) = PPobj%BRSVD(2)
    RPP( 3) = PPobj%BRSVD(3)
    RPP( 4) = PPobj%BRSVD(4)
    RPP( 5) = PPobj%BDATUM
    RPP( 6) = PPobj%BACC
    RPP( 7) = PPobj%BLEV
    RPP( 8) = PPobj%BRLEV
    RPP( 9) = PPobj%BHLEV
    RPP(10) = PPobj%BHRLEV
    RPP(11) = PPobj%BPLAT
    RPP(12) = PPobj%BPLON
    RPP(13) = PPobj%BGOR
    RPP(14) = PPobj%BZY
    RPP(15) = PPobj%BDY
    RPP(16) = PPobj%BZX
    RPP(17) = PPobj%BDX
    RPP(18) = PPobj%BMDI
    RPP(19) = PPobj%BKS

    write(lun, iostat=iostatus) IPP, RPP
    if( iostatus > 0 ) then
       write(*,*)" Error writing PP file on unit ", lun
       stop
    end if
    if (iostatus < 0) return

  end subroutine PP_write


  subroutine PP_print_hdr(lun, PPobj)
    integer, intent(in)   :: lun
    type(PP), intent(in)  :: PPobj

    write(lun,*)"PP Header:"

    write(lun,*)PPobj%LBYR
    write(lun,*)PPobj%LBMON
    write(lun,*)PPobj%LBDAT
    write(lun,*)PPobj%LBHR
    write(lun,*)PPobj%LBMIN
    write(lun,*)PPobj%LBDAY
    write(lun,*)PPobj%LBYRD
    write(lun,*)PPobj%LBMOND
    write(lun,*)PPobj%LBDATD
    write(lun,*)PPobj%LBHRD

    write(lun,*)PPobj%LBMIND
    write(lun,*)PPobj%LBDAYD
    write(lun,*)PPobj%LBTIM
    write(lun,*)PPobj%LBFT
    write(lun,*)PPobj%LBLREC
    write(lun,*)PPobj%LBCODE
    write(lun,*)PPobj%LBHEM
    write(lun,*)"Rows           ", PPobj%LBROW
    write(lun,*)"Points per row ", PPobj%LBNPT
    write(lun,*)PPobj%LBEXT

    write(lun,*)PPobj%LBPACK
    write(lun,*)PPobj%LBREL
    write(lun,*)" Field code    ", PPobj%LBFC
    write(lun,*)PPobj%LBCFC
    write(lun,*)PPobj%LBPROC
    write(lun,*)PPobj%LBVC
    write(lun,*)PPobj%LBRVC
    write(lun,*)PPobj%LBEXP
    write(lun,*)PPobj%LBEGIN
    write(lun,*)PPobj%LBNREC

    write(lun,*)PPobj%LBPROJ
    write(lun,*)PPobj%LBTYP
    write(lun,*)PPobj%LBLEV
    write(lun,*)PPobj%LBRSVD
    write(lun,*)PPobj%LBSRCE
    write(lun,*)PPobj%LBUSER(1)
    write(lun,*)PPobj%LBUSER(2)
    write(lun,*)PPobj%LBUSER(3)
    write(lun,*)" STASH         ",PPobj%LBUSER(4)
    write(lun,*)PPobj%LBUSER(5)
    write(lun,*)PPobj%LBUSER(6)
    write(lun,*)PPobj%LBUSER(7)


    write(lun,*)


  end subroutine PP_print_hdr


  ! skip past the data in the unformatted sequential file open on lun
  subroutine skip_pp_data(lun)
    implicit none
    integer, intent(in) :: lun

    real                :: d                ! this needs to be 4 bytes
    integer             :: io_status

    io_status = 0
    read(lun,iostat = io_status)d
    if ( io_status > 0 ) then
       write(*,*)" io_status = ", io_status
       stop " skip_pp_data - Read error."
    end if

  end subroutine skip_pp_data


  logical function valid_pp(PPobj)
    type(PP) , intent(in)  :: PPobj


    valid_pp = .true.

    if ( (PPobj%LBHEM >= 0 .and. PPobj%LBHEM <= 5)  .or. (PPobj%LBHEM == 99)   ) then
       ! it's valid
    else
       write(*,*)" Invalid hemisphere indicator ", PPobj%LBHEM
       valid_pp = .false.
    end if

    if ( PPobj%LBROW <= 0 ) then
       write(*,*) "Invalid  number of rows ",  PPobj%LBROW
       valid_pp = .false.
    end if
    if ( PPobj%LBNPT <= 0 ) then
       write(*,*) "Invalid  number of points per row ", PPobj%LBNPT
       valid_pp = .false.
    end if

  end function valid_pp


  logical function compare_PP_hdr( PP1, PP2)
    type(PP), intent(in) :: PP1, PP2

    if ( PP1%LBROW == PP2%LBROW .and. &
         PP1%LBNPT == PP2%LBNPT ) then
       compare_PP_hdr = .true.
    else
       compare_PP_hdr = .false.
    end if

  end function compare_PP_hdr


end module PP_type
