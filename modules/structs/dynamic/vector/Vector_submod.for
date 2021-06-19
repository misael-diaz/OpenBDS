!
!   source: Vector_submod.for
!   author: misael-diaz
!   date:   2021-06-10
!
!
!   Synopsis:
!   Implements the vector class.
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

submodule (vectors) vectors_int32_t_implementation
    contains


        module function default_constructor () result(vector)
            ! Synopsis: Returns an empty vector
            type(vector_t):: vector

            vector % begin % idx  = 0_int64
            vector % avail % idx  = 0_int64
            vector % limit % idx  = 0_int64
            vector % state % init = .false.

            return
        end function


        module function findloc_method (self, value) result(idx)
            class(vector_t), intent(in) :: self
            integer(kind = int64) :: idx
            integer(kind = int32), intent(in) :: value

            if (.not. self % state % init) then
                idx = 0_int64
            else
                call findloc_wrapper (self, value, idx)
            end if

            return
        end function


        module subroutine findloc_wrapper (vector, value, idx)
            type(vector_t), intent(in) :: vector
            integer(kind = int64), intent(out) :: idx
            integer(kind = int64) :: lb
            integer(kind = int64) :: ub
            integer(kind = int32), intent(in) :: value

            lb = vector % begin % idx
            ub = vector % avail % idx
            ub = ub - 1_int64
            idx = findloc(array = vector % array % values_int32_t(lb:ub), &
                        & value = value, dim = 1, kind = int64)

            return
        end subroutine


        module function addressing_method (self, idx) result(value)
            ! Synopsis: Addresses the element pointed to by index.
            class(vector_t), intent(in) :: self
            integer(kind = int64), intent(in) :: idx
            integer(kind = int32) :: value
            value = self % array % values_int32_t(idx)
            return
        end function


        module function size_method (self) result(vector_size)
            ! Synopsis: Returns the size of the vector.
            class(vector_t), intent(in) :: self
            integer(kind = int64) :: vector_size

            associate (begin => self % begin % idx, &
                     & end   => self % avail % idx)
                vector_size = end - begin
            end associate

            return
        end function


        module subroutine clear_method (self)
            ! Synopsis: Clears the vector elements.
            class(vector_t), intent(inout) :: self
            self % avail % idx = 0_int64
            return
        end subroutine


        module subroutine push_back_method (self, value)
            ! Synopsis: Pushes value unto back of vector.
            class(vector_t), intent(inout) :: self
            integer(kind = int32), intent(in) :: value

            if (self % state % init) then
                call insert_back (self, value)
            else
                call initializer (self, value)
            end if

            return
        end subroutine


        module subroutine insert_back (vector, value)
            ! Synopsis: Inserts value unto back, vector grows as needed.
            type(vector_t), intent(inout) :: vector
            integer(kind = int32), intent(in) :: value

            if (vector % avail % idx == vector % limit % idx) then
                call grow (vector)
            end if

            associate(avail => vector % avail % idx)
                vector % array % values_int32_t(avail) = value
                avail = avail + 1_int64
            end associate

            return
        end subroutine


        module subroutine grow (vector)
            ! Synopsis: Doubles the vector size.
            type(vector_t), intent(inout) :: vector
            integer(kind = int64):: lb
            integer(kind = int64):: ub
            integer(kind = int64):: bounds(0:1)
            integer(kind = int32), allocatable :: values(:)


            lb = vector % begin % idx
            ub = vector % limit % idx
            bounds(0) = lb
            bounds(1) = ub
            call allocator (bounds, values)
            ! copies existing values into placeholder
            values(lb:ub) = vector % array % values_int32_t(lb:ub)


            vector % limit % idx = 2_int64 * vector % limit % idx


!           bounds(0) = vector % begin % idx
            bounds(1) = vector % limit % idx
            call reallocator (bounds, vector % array % values_int32_t)
            ! copies values in placeholder into (reallocated) vector
            vector % array % values_int32_t = 0
            vector % array % values_int32_t(lb:ub) = values(lb:ub)


            call deallocator (values)

            return
        end subroutine


        module subroutine initializer (vector, value)
            type(vector_t), intent(inout) :: vector
            integer(kind = int32), intent(in) :: value
            call create (vector, value)
            return
        end subroutine


        module subroutine create (vector, value)
            ! Synopsis: Creates the first element in vector.
            type(vector_t), intent(inout) :: vector
            integer(kind = int64) :: idx
            integer(kind = int64) :: bounds(0:1)
            integer(kind = int64), parameter :: lb = 0_int64
            integer(kind = int64), parameter :: ub = 8_int64
            integer(kind = int32), intent(in) :: value


            bounds(0) = lb
            bounds(1) = ub
            call allocator (bounds, vector % array % values_int32_t)


            idx = vector % avail % idx
            vector % array % values_int32_t = 0
            vector % array % values_int32_t(idx) = value


!           vector % begin % idx  = 0_int64
            vector % avail % idx  = 1_int64
            vector % limit % idx  = 8_int64
            vector % state % init = .true.


            return
        end subroutine


        module subroutine finalizer (vector)
            type(vector_t), intent(inout) :: vector

            if ( allocated(vector % array % values_int32_t) ) then
                call deallocator (vector % array % values_int32_t)
            end if

            return
        end subroutine
end submodule


! References:
! SJ Chapman, FORTRAN for Scientists and Engineers, fourth edition
! A Koenig and B Moo, Accelerated C++ Practical Programming by Example


! Comments:
! For now iterators are implemented as indexes.
!


! Comments on Procedures:
!
! subroutine create()
! Allocates one more element on purpose to compute the size of the
! vector via: (end - begin) as in c++.
!
!
! function findloc_wrapper_method (self, value) result(idx)
! lower and upper bounds (lb, ub) are chosen so that we do not include
! the element pointed to by (end). It's a valid index but it should not
! be referenced since it does not hold an actual value.
!
!
! subroutine clear_method()
! Placing the /avail/ iterator at the beginning is equivalent to
! clearing the vector without deallocating memory. I have designed
! the vector class thinking on how it will be used for keeping
! track of neighbors. In that context it's convenient to
! clear the vector without deallocating memory since it would
! be expensive to have to grow the size of the vector over and
! over again during the simulation. Why not use fixed-size arrays?
! I have use them in the past worrying that I might have to allocate
! more memory than actually needed to avoid exceding the array
! bounds. Some systems might be more dynamic having particles with
! far more neighbors than others. Vectors would come in handy for
! such cases.
