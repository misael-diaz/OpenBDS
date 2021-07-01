!
!   source: Vector_vector_t_methods.for
!   author: misael-diaz
!   date:   2021-07-01
!
!
!   Synopsis:
!   Defines methods for a vector of vectors.
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

submodule (VectorClass) vector_vector_t_methods
    implicit none
    contains


        module subroutine vector_vector_t_iterator_method (self, it)
            ! Synopsis: Returns iterator to values in range [begin, avail).
            class(vector_t), intent(in) :: self
            type(vector_t), intent(inout), pointer, contiguous :: it(:)

            if ( self % size () == 0_int64 ) then
                it => null()
            else
                call slice (self, it)
            end if

            return
        end subroutine


        module subroutine vector_vector_t_indexing_method (self, idx, value)
            ! Synopsis: Addresses the element pointed to by index.
            class(vector_t), intent(in) :: self
            type(vector_t), intent(inout) :: value
            integer(kind = int64), intent(in) :: idx

            call is_empty (self)
            call check_bounds (self, idx)

            call indexer (self, idx, value)

            return
        end subroutine


        module subroutine vector_vector_t_push_back_method (self, value)
            ! Synopsis: Pushes value unto back of vector.
            class(vector_t), intent(inout) :: self
            type(vector_t), intent(in) :: value
            call back_inserter (self, value)
            return
        end subroutine


end submodule


! References:
! SJ Chapman, FORTRAN for Scientists and Engineers, fourth edition
! A Koenig and B Moo, Accelerated C++ Practical Programming by Example
