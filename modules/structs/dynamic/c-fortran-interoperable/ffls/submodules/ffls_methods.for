!
!   source: ffls_methods.for
!   author: misael-diaz
!   date:   2021-11-19
!
!
!   Synopsis:
!   Defines methods for the FORTRAN forward linked-list class.
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

submodule (FFLinkedListClass) ffls_methods
implicit none
contains

  module function ffls_create_flit_t (self, type) result(iter)
      class(ffls_t), intent(inout) :: self
      type(flit_t), pointer :: iter
      type(node_t), pointer :: node => null()
      type(link_t), pointer :: head => null()
      integer(kind = c_int32_t), intent(in) :: type
      integer(kind = c_int32_t), pointer :: data => null()
      integer(kind = c_int32_t) :: mstat
      integer(kind = c_size_t) :: i, n

      mstat = type
      allocate (self % flit, stat=mstat)
      if (mstat /= 0) error stop 'allocation error'

      iter => self % flit

      if (self % self % size /= 0) then
          allocate (iter % p(self % self % size), stat=mstat)
      else
          iter => null()
      end if


      if ( associated(iter) ) then

          do i = 1, self % self % size
              iter % p(i) % data => null()
          end do

          call c_f_pointer (self % self % head, head)
          call c_f_pointer (head % node, node)

          n = 1
          do while ( associated(node) )

              call c_f_pointer (node % data, data)

              iter % p(n) % data => data

              call c_f_pointer (node % next, node)
              n = n + 1

          end do

      end if

      return
  end function


  module function ffls_create_iter_t (self) result(iter)
      class(ffls_t), intent(inout) :: self
      type(iter_t), pointer :: it => null()
      type(c_ptr) :: iter

      iter = flist_create_iter_t (self % self)
      call c_f_pointer (iter, it)
      self % it => it

      return
  end function


  module subroutine ffls_destroy_iter_t (self, iter)
      class(ffls_t), intent(inout) :: self
      type(iter_t), intent(inout), pointer :: iter
      type(c_ptr) :: ret

      if ( associated(iter) ) then
          ret = flist_destroy_iter_t (iter)
      end if

      self % it => null()

      return
  end subroutine


  module subroutine ffls_append_int32_t_method (self, value)
      ! Synopsis: Method for appending (signed) 32-bit integers.
      class(ffls_t), intent(inout) :: self
      integer(kind = c_int32_t), intent(in) :: value

      call flist_append_int32_t_method (self % self, value)

      return
  end subroutine


  module subroutine ffls_append_int64_t_method (self, value)
      ! Synopsis: Method for appending (signed) 64-bit integers.
      class(ffls_t), intent(inout) :: self
      integer(kind = c_int64_t), intent(in) :: value

      call flist_append_int64_t_method (self % self, value)

      return
  end subroutine

end submodule
