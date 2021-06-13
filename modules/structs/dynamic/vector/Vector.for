!
!   source: Vector.for
!   author: misael-diaz
!   date:   2021-06-10
!
!
!   Synopsis:
!   Defines the vector class.
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

module vectors
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use utils, only: allocator   => util_allocate_array
    use utils, only: reallocator => util_reallocate_array
    use utils, only: deallocator => util_deallocate_array
    implicit none


    type :: iter_t
        integer(kind = int64) :: idx = 0_int64
    end type


    type :: data_t
        integer(kind = int32), allocatable :: values(:)
    end type


    type :: stat_t
        logical(kind = int32) :: init = .false.
    end type


    type, public :: vector_t
        private
        type(iter_t):: begin
        type(iter_t):: avail
        type(iter_t):: limit
        type(data_t):: array
        type(stat_t):: state
        contains
            private
            procedure :: addressing_method
            generic, public :: operator (<) => addressing_method
            procedure, public :: size => size_method
            procedure, public :: clear => clear_method
            procedure, public :: push_back => push_back_method
            final :: finalizer
    end type


    interface vector_t
        module procedure default_constructor
    end interface


!   interface allocator
!       module procedure allocate_array
!   end interface


!   interface deallocator
!       module procedure deallocate_array
!   end interface


    private
    contains


        function default_constructor () result(vector)
            ! Synopsis: Returns an empty vector
            type(vector_t):: vector

            vector % begin % idx  = 0_int64
            vector % avail % idx  = 0_int64
            vector % limit % idx  = 0_int64
            vector % state % init = .false.

            return
        end function


        function addressing_method (self, idx) result(value)
            class(vector_t), intent(in) :: self
            integer(kind = int64), intent(in) :: idx
            integer(kind = int32) :: value
            value = self % array % values(idx)
            return
        end function


        function size_method (self) result(vector_size)
            class(vector_t), intent(in) :: self
            integer(kind = int64) :: vector_size

            associate (begin => self % begin % idx, &
                     & end   => self % avail % idx)
                vector_size = end - begin
            end associate

            return
        end function


        subroutine clear_method (self)
            class(vector_t), intent(inout) :: self
            self % avail % idx = 0_int64
            return
        end subroutine


        subroutine push_back_method (self, value)
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


        subroutine insert_back (vector, value)
            ! Synopsis: Inserts value unto back, vector grows as needed.
            type(vector_t), intent(inout) :: vector
            integer(kind = int32), intent(in) :: value

            if (vector % avail % idx == vector % limit % idx) then
                call grow (vector)
            end if

            associate(avail => vector % avail % idx)
                vector % array % values(avail) = value
                avail = avail + 1_int64
            end associate

            return
        end subroutine


        subroutine grow (vector)
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
            values(lb:ub) = vector % array % values(lb:ub)


            vector % limit % idx = 2_int64 * vector % limit % idx


!           bounds(0) = vector % begin % idx
            bounds(1) = vector % limit % idx
            call reallocator (bounds, vector % array % values)
            ! copies values in placeholder into (reallocated) vector
            vector % array % values = 0
            vector % array % values(lb:ub) = values(lb:ub)


            call deallocator (values)

            return
        end subroutine


        subroutine initializer (vector, value)
            type(vector_t), intent(inout) :: vector
            integer(kind = int32), intent(in) :: value
            call create (vector, value)
            return
        end subroutine


        subroutine create (vector, value)
            ! Synopsis: Creates the first element in vector.
            type(vector_t), intent(inout) :: vector
            integer(kind = int64) :: idx
            integer(kind = int64) :: bounds(0:1)
            integer(kind = int64), parameter :: lb = 0_int64
            integer(kind = int64), parameter :: ub = 8_int64
            integer(kind = int32), intent(in) :: value


            bounds(0) = lb
            bounds(1) = ub
            call allocator (bounds, vector % array % values)


            idx = vector % avail % idx
            vector % array % values = 0
            vector % array % values(idx) = value


!           vector % begin % idx  = 0_int64
            vector % avail % idx  = 1_int64
            vector % limit % idx  = 8_int64
            vector % state % init = .true.


            return
        end subroutine


        subroutine finalizer (vector)
            type(vector_t), intent(inout) :: vector

            if ( allocated(vector % array % values) ) then
                call deallocator (vector % array % values)
            end if

            return
        end subroutine
end module


! References:
! SJ Chapman, FORTRAN for Scientists and Engineers, fourth edition
! A Koenig and B Moo, Accelerated C++ Practical Programming by Example


! Comments:
! For now iterators are implemented as indexes.
!


! Comments on Procedures:
! subroutine create()
! Allocates one more element on purpose to compute the size of the
! vector via: (end - begin) as in c++.
