module stash_list

! For user supplied STASH list
  integer                           :: num_stash
  integer, parameter                :: max_num_stash = 100
  integer                           :: STASH(max_num_stash)
  
contains

! Function to determine whether stash is in the user list
   logical function wanted_STASH(stash_no)
        implicit none
        integer, intent(in)        :: stash_no
        integer i
	
        wanted_STASH = .false.
	
        do i = 1, num_stash
          if ( stash_no == STASH(i) ) then
            wanted_STASH = .true.
            exit
           end if
        end do
  end function wanted_STASH  
  
  
  end module stash_list
