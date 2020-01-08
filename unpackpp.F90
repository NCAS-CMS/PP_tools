!*==UNPACKPP.spg  processed by SPAG 6.72Dc at 11:38 on  8 Jan 2020
      PROGRAM UNPACKPP
!
      IMPLICIT NONE
!*--UNPACKPP5
!
      INTEGER N1 , N2
      PARAMETER (N1=45,N2=19)
!
      INTEGER NXMAX , NYMAX , NDATAMAX
!      PARAMETER (NXMAX=700,NYMAX=500,NDATAMAX=NXMAX*NYMAX)
      
!
      INTEGER ilkup(N1)
      REAL rlkup(N2)
      real, dimension(:), allocatable ::  data, idata
!
      CHARACTER carg*256
      INTEGER narg , IARGC , jarg , arglen , LENB
!
      INTEGER ichan , ochan , nx , ny , nin , nout , j , jd , ierr
      INTEGER ipack
      REAL amdi
!
      ichan = 11
      ochan = 12
      ierr = 0
      j = 0
!
      narg = IARGC()
      DO jarg = 1 , narg
         CALL GETARG(jarg,carg)
         arglen = LENB(carg)
         WRITE (*,*) 'OPENING INPUT FILE ' , carg(1:arglen)
         OPEN (ichan,FILE=carg(1:arglen),FORM='UNFORMATTED',            &
              &ACCESS='SEQUENTIAL')
         WRITE (*,*) 'OPENING OUTPUT FILE ' , carg(1:arglen)            &
                    &//'.unpacked'
         OPEN (ochan,FILE=carg(1:arglen)//'.unpacked',                  &
             & FORM='UNFORMATTED',ACCESS='SEQUENTIAL')
!
!        Loop over all fields in PP file
!
 50      j = j + 1
!
!           Read headers
!
         READ (ichan,END=100) ilkup , rlkup
!
         ipack = MOD(ilkup(21),10)
         nx = ilkup(19)
         ny = ilkup(18)
         nin = ilkup(15)
         nout = nx*ny
         allocate(data(nout))
         allocate(idata(nout))
         amdi = rlkup(18)
!
!           Read data section
!
         IF ( ipack.EQ.0 ) THEN
            READ (ichan,END=100) (data(jd),jd=1,nin)
         ELSEIF ( ipack.EQ.1 ) THEN
            READ (ichan,END=100) (idata(jd),jd=1,nin)
!
!              Unpack data
!
            CALL UNWGDOS(idata,nin,data,nout,amdi,nx,ierr)
         ELSE
            WRITE (*,*) 'Unpacking option ' , ipack , ' not supported.'
            ierr = 1
         ENDIF
         IF ( ierr.NE.0 ) THEN
            WRITE (*,*) 'Error unpacking data for field ' , j
            STOP
         ENDIF
!
!           Change header values
!
         ilkup(15) = nout
         ilkup(21) = 0
!
!           Write headers
!
         WRITE (ochan) ilkup , rlkup
!
!           Write data section
!
         WRITE (ochan) (data(jd),jd=1,nout)
!
!           End of loop over all fields in PP file
!
         deallocate(data)
         deallocate(idata)
         GOTO 50
!
 100     CLOSE (ichan)
      ENDDO
!
      END
!*==LENB.spg  processed by SPAG 6.72Dc at 11:38 on  8 Jan 2020
!
!-----------------------------------------------------------------------
!
      FUNCTION LENB(Dummy)
!
      IMPLICIT NONE
!*--LENB108
!
      INTEGER LEN , LENB , i
      CHARACTER*(*) Dummy
!
!     Calculate length of string without trailing blanks
!
      i = LEN(Dummy)
      DO WHILE ( Dummy(i:i).EQ.' ' )
         i = i - 1
         IF ( i.EQ.0 ) GOTO 100
      ENDDO
 100  LENB = i
!
      END
!*==UNWGDOS.spg  processed by SPAG 6.72Dc at 11:38 on  8 Jan 2020
!
!-----------------------------------------------------------------------
!
      SUBROUTINE UNWGDOS(Ain32,Nin,Aout,Nout,Amdi,nx,Kerr)
!
      IMPLICIT NONE
!*--UNWGDOS130
!
      INTEGER Nin , Nout , Kerr
      INTEGER nx
      REAL Amdi
      INTEGER Ain32(Nin)
      REAL Aout(Nout)
!
      INTEGER n1 , offset0 , offset1 , iswap , head(2)
      INTEGER*4 j , len , isc , ix , iy , icx , jcx , ibit , nop
      REAL prec , base
!
      n1 = 1
      offset0 = 0
      offset1 = 1
!
!     Determine if data needs byte swapping
!
      iswap = -1
      head(1) = Ain32(3)
      CALL IBMI2TOI4(head,ix,n1,offset0,Kerr)
      CALL IBMI2TOI4(head,iy,n1,offset1,Kerr)
      IF ( ix*iy.EQ.Nout ) iswap = 0
!
      IF ( iswap.EQ.-1 ) THEN
!
!        See if data is byte swapped with 4 byte words
!
         head(1) = Ain32(3)
         CALL SWAPBYTES(head,4,n1)
         CALL IBMI2TOI4(head,ix,n1,offset0,Kerr)
         CALL IBMI2TOI4(head,iy,n1,offset1,Kerr)
         IF ( ix*iy.EQ.Nout ) iswap = 4
      ENDIF
!
      IF ( iswap.EQ.-1 ) THEN
!
!        See if data is byte swapped with 8 byte words
!
         head(1) = Ain32(3)
         head(2) = Ain32(4)
         CALL SWAPBYTES(head,8,n1)
         CALL IBMI2TOI4(head,ix,n1,offset0,Kerr)
         CALL IBMI2TOI4(head,iy,n1,offset1,Kerr)
         IF ( ix*iy.EQ.Nout ) iswap = 8
      ENDIF
!
      IF ( iswap.EQ.-1 ) THEN
         WRITE (*,*) 'WGDOS data header record mismatch'
         Kerr = 1
         RETURN
      ELSEIF ( iswap.GT.0 ) THEN
         CALL SWAPBYTES(Ain32,iswap,Nin*4/iswap)
      ENDIF
!
!     Extract scale factor and number of columns and rows from header
!
      CALL IBMI4TOI4(Ain32(1),len,n1,Kerr)
      CALL IBMI4TOI4(Ain32(2),isc,n1,Kerr)
      CALL IBMI2TOI4(Ain32(3),ix,n1,offset0,Kerr)
      CALL IBMI2TOI4(Ain32(3),iy,n1,offset1,Kerr)
!
!     Expand compressed data
!
      prec = 2.0**REAL(isc)
      icx = 4
      jcx = 1
!
      DO j = 1 , iy
!
!        Extract base, number of bits per value,
!        number of 32 bit words used
!
         CALL IBMR4TOR4(Ain32(icx),base,n1,Kerr)
         CALL IBMI2TOI4(Ain32(icx+1),ibit,n1,offset0,Kerr)
         CALL IBMI2TOI4(Ain32(icx+1),nop,n1,offset1,Kerr)
!
!ifdef LITTLE__ENDIAN
         CALL SWAPBYTES(Ain32(icx+2),4,nop)
!endif
         CALL XPND(ix,Ain32(icx+2),Aout(jcx),prec,ibit,base,nop,Amdi,   &
                 & nx, Kerr)
!
         icx = icx + nop + 2
         jcx = jcx + ix
      ENDDO
!
      END
!*==XPND.spg  processed by SPAG 6.72Dc at 11:38 on  8 Jan 2020
!
!-----------------------------------------------------------------------
!
      SUBROUTINE XPND(Ix,Icomp,Field,Prec,Ibit,Base,Nop,Amdi,nx,Kerr)
!
      IMPLICIT NONE
!*--XPND224
!
      INTEGER nx
      INTEGER*4 Ix , Icomp(*) , Ibit , Nop, Kerr
      REAL Field(*) , Prec , Base , Amdi
!
      LOGICAL lbtmap , lbtmis , lbtmin , lbtzer
      INTEGER*4 jword , jbit , j , iscale
!
      integer*4, dimension(:), allocatable :: imap , imis , imin , izer
!
!

      allocate( imap(nx) , imis(nx) , imin(nx) , izer(nx))
      lbtmap = .FALSE.
      lbtmis = .FALSE.
      lbtmin = .FALSE.
      lbtzer = .FALSE.
!
!     Check if bitmap used for zero values
!
      IF ( Ibit.GE.128 ) THEN
         lbtzer = .TRUE.
         lbtmap = .TRUE.
         Ibit = Ibit - 128
      ENDIF
!
!     Check if bitmap used for minimum values
!
      IF ( Ibit.GE.64 ) THEN
         lbtmin = .TRUE.
         lbtmap = .TRUE.
         Ibit = Ibit - 64
      ENDIF
!
!     Check if bitmap used for missing data values
!
      IF ( Ibit.GE.32 ) THEN
         lbtmis = .TRUE.
         lbtmap = .TRUE.
         Ibit = Ibit - 32
      ENDIF
!
      IF ( Ibit.GT.32 ) THEN
         WRITE (*,*) 'Number of bits used to pack wgdos data = ' ,      &
                   & Ibit , ' must be <= 32'
         Kerr = 1
         RETURN
      ENDIF
!
      IF ( lbtmap ) THEN
         DO j = 1 , Ix
            imap(j) = 1
         ENDDO
      ENDIF
!
!     Set start position in icomp
!
      jword = 1
      jbit = 31
!
!     Extract missing data value bitmap
!
      IF ( lbtmis ) THEN
         DO j = 1 , Ix
            IF ( BTEST(Icomp(jword),jbit) ) THEN
               imis(j) = 1
               imap(j) = 0
            ELSE
               imis(j) = 0
            ENDIF
!
            IF ( jbit.GT.0 ) THEN
               jbit = jbit - 1
            ELSE
               jbit = 31
               jword = jword + 1
            ENDIF
         ENDDO
      ENDIF
!
!     Extract minimum value bitmap
!
      IF ( lbtmin ) THEN
         DO j = 1 , Ix
            IF ( BTEST(Icomp(jword),jbit) ) THEN
               imin(j) = 1
               imap(j) = 0
            ELSE
               imin(j) = 0
            ENDIF
!
            IF ( jbit.GT.0 ) THEN
               jbit = jbit - 1
            ELSE
               jbit = 31
               jword = jword + 1
            ENDIF
         ENDDO
      ENDIF
!
!     Extract zero value bitmap
!
      IF ( lbtzer ) THEN
         DO j = 1 , Ix
            IF ( BTEST(Icomp(jword),jbit) ) THEN
               izer(j) = 0
            ELSE
               izer(j) = 1
               imap(j) = 0
            ENDIF
!
            IF ( jbit.GT.0 ) THEN
               jbit = jbit - 1
            ELSE
               jbit = 31
               jword = jword + 1
            ENDIF
         ENDDO
      ENDIF
!
!     If bitmap used reset pointers to beginning of 32 bit boundary
!
      IF ( lbtmap .AND. jbit.NE.31 ) THEN
         jbit = 31
         jword = jword + 1
      ENDIF
!
      IF ( Ibit.GT.0 ) THEN
!
!        Unpack scaled values
!
         DO j = 1 , Ix
            IF ( .NOT.(lbtmap .AND. imap(j).EQ.0) ) THEN
!
               CALL EXTRIN(Icomp(jword),4,jbit,Ibit,iscale,0)
               Field(j) = Base + iscale*Prec
!
               jbit = jbit - Ibit
               IF ( jbit.LT.0 ) THEN
                  jbit = jbit + 32
                  jword = jword + 1
               ENDIF
            ENDIF
!
         ENDDO
!
!        If minimum value bitmap fill in field with base
!
         IF ( lbtmin ) THEN
            DO j = 1 , Ix
               IF ( imin(j).EQ.1 ) Field(j) = Base
            ENDDO
         ENDIF
      ELSEIF ( Ibit.EQ.0 ) THEN
!
!        All points in row have same value
!
         DO j = 1 , Ix
            Field(j) = Base
         ENDDO
      ENDIF
!
!     If missing data value bitmap fill in field with AMDI
!
      IF ( lbtmis ) THEN
         DO j = 1 , Ix
            IF ( imis(j).EQ.1 ) Field(j) = Amdi
         ENDDO
      ENDIF
!
!     If zero value bitmap fill in field with 0.0
!
      IF ( lbtzer ) THEN
         DO j = 1 , Ix
            IF ( izer(j).EQ.1 ) Field(j) = 0.0
         ENDDO
      ENDIF
      deallocate( imap , imis , imin , izer)
!
      END
!*==EXTRIN.spg  processed by SPAG 6.72Dc at 11:38 on  8 Jan 2020
!
!-----------------------------------------------------------------------
!
      SUBROUTINE EXTRIN(Icomp,Iword,Istart,Nbit,Inum,Isign)
!
      IMPLICIT NONE
!*--EXTRIN416
!
      INTEGER*4 Icomp(2) , Iword , Istart , Nbit , Inum , Isign
!
      INTEGER*4 ibit
!
      IF ( Isign.EQ.0 ) THEN
         CALL MOVEBITS(Icomp,Istart,Nbit,Inum)
      ELSEIF ( Isign.EQ.1 ) THEN
!
!        Move integer without sign bit
!
         CALL MOVEBITS(Icomp,Istart-1,Nbit-1,Inum)
!
!        Move sign bit
!
         CALL MVBITS(Icomp(1),Istart,1,Inum,31)
!
!        Set undefined if inum negative
!
         IF ( Inum.LT.0 ) THEN
            DO ibit = Nbit - 1 , 31
               Inum = IBSET(Inum,ibit)
            ENDDO
         ENDIF
      ELSEIF ( Isign.EQ.2 ) THEN
!
!        Move integer without sign bit
!
         CALL MOVEBITS(Icomp,Istart-1,Nbit-1,Inum)
         IF ( BTEST(Icomp(1),Istart) ) Inum = -Inum
      ENDIF
!
      END
!*==MOVEBITS.spg  processed by SPAG 6.72Dc at 11:38 on  8 Jan 2020
!
!-----------------------------------------------------------------------
!
!     Move NBITS from 32 bit WORD1(1) starting at START1
!     into 32 bit WORD2. 0 =< NBITS <= 32, bits can cross into WORD1(2).
!
!-----------------------------------------------------------------------
!
      SUBROUTINE MOVEBITS(Word1,Start1,Nbits,Word2)
!
      IMPLICIT NONE
!*--MOVEBITS462
!
      INTEGER*4 Word1(2) , Start1 , Nbits , Word2
!
      Word2 = 0
!
      IF ( Start1+1.GE.Nbits ) THEN
!
!        Move bits within one word
!
         CALL MVBITS(Word1(1),Start1+1-Nbits,Nbits,Word2,0)
      ELSE
!
!        Move bits within two words
!
         CALL MVBITS(Word1(1),0,Start1+1,Word2,Nbits-Start1-1)
         CALL MVBITS(Word1(2),32+Start1+1-Nbits,Nbits-Start1-1,Word2,0)
      ENDIF
!
      END
 
