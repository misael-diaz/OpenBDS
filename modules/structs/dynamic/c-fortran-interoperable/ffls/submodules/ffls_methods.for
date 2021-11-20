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

  module subroutine ffls_append_int32_t_method (self, value)
      ! Synopsis: Method for appending (signed) 32-bit integers.
      class(ffls_t), intent(inout) :: self
      integer(kind = c_int32_t), intent(in) :: value

      call flist_append_int32_t_method (self % list, value)

      return
  end subroutine


  module subroutine ffls_append_int64_t_method (self, value)
      ! Synopsis: Method for appending (signed) 64-bit integers.
      class(ffls_t), intent(inout) :: self
      integer(kind = c_int64_t), intent(in) :: value

      call flist_append_int64_t_method (self % list, value)

      return
  end subroutine

end submodule
