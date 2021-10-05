!
!   source: Vector_real64_t_methods.for
!   author: misael-diaz
!   date:   2021-06-27
!
!
!   Synopsis:
!   Defines methods for vectors of 64-bit integers.
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

submodule (VectorClass) vector_real64_t_methods
implicit none
contains


  module subroutine vector_real64_t_indexing_method (self, i, value)
      ! Synopsis: Addresses the element pointed to by index.
      class(vector_t), intent(in) :: self
      real(kind = real64), intent(out) :: value
      integer(kind = int64), intent(in) :: i

      call is_empty (self)
      call check_bounds (self, i)

      call indexer (self, i, value)

      return
  end subroutine


  module subroutine vector_real64_t_push_back_method (self, value)
      ! Synopsis: Pushes value unto back of vector.
      class(vector_t), intent(inout) :: self
      real(kind = real64), intent(in) :: value
      call back_inserter (self, value)
      return
  end subroutine


end submodule


! References:
! SJ Chapman, FORTRAN for Scientists and Engineers, fourth edition
! A Koenig and B Moo, Accelerated C++ Practical Programming by Example
