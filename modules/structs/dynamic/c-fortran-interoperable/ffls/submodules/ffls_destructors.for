!
!   source: ffls_destructors.for
!   author: misael-diaz
!   date:   2021-11-19
!
!
!   Synopsis:
!   Defines destructors for the FORTRAN forward linked-list class.
!
!
!   Copyright (C) 2021 Misael Diaz-Maldonado
!
!   This program is free software: you can redistribute it and/or modify
!   it under the terms of the GNU General Public License as published by
!   the Free Software Foundation, either version 3 of the License, or
!   (at your option) any later version.
!
!   This program is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   GNU General Public License for more details.
!
!   You should have received a copy of the GNU General Public License
!   along with this program.  If not, see <http://www.gnu.org/licenses/>.

submodule (FFLinkedListClass) ffls_destructors
implicit none
contains

  module subroutine ffls_finalizer (ffls)
      type(ffls_t), intent(inout) :: ffls
      type(c_ptr) :: ret
      integer(kind = c_size_t) :: mstat
      integer(kind = c_size_t) :: i

      if ( associated(ffls % it) ) then
          ret = flist_destroy_iter_t (ffls % it)
      end if


      if ( associated(ffls % flit) ) then

          if ( allocated(ffls % flit % p) ) then

              do i = 1, ffls % self % size
                  ffls % flit % p(i) % data => null()
              end do

              deallocate (ffls % flit % p, stat=mstat)
              if (mstat /= 0) error stop 'dealloc err'

          end if

          deallocate (ffls % flit, stat=mstat)
          if (mstat /= 0) error stop 'dealloc err'

          ffls % flit => null()

      end if


      ffls % list = flist_list_destructor (ffls % list)

      return
  end subroutine

end submodule
