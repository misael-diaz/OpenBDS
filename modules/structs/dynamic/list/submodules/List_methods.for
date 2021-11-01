!
!   source: List_methods.for
!   author: misael-diaz
!   date:   2021-10-29
!
!
!   Synopsis:
!   Defines methods of the linked-list class.
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

submodule (ListClass) list_methods
implicit none
contains

  module subroutine list_validate_iterator_method (self, iter)
      type(iter_t), intent(inout) :: iter
      class(list_t), intent(in) :: self

      call iter % clear ()
      call iterator (self % head, iter)

      return
  end subroutine


  module function list_random_access_iterator_method (self) result(iter)
      type(iter_t) :: iter
      class(list_t), intent(in) :: self

      call iterator (self % head, iter)

      return
  end function


  module subroutine list_int32_t_append_method (self, value)
      class(list_t), intent(inout) :: self
      integer(kind = int32), intent(in) :: value

      call append (self, value)

      return
  end subroutine

end submodule


! References:
! SJ Chapman, FORTRAN for Scientists and Engineers, fourth edition
! A Koenig and B Moo, Accelerated C++ Practical Programming by Example
