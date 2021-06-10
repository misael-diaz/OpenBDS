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
    use utils, only: allocate_array   => util_allocate_array
    use utils, only: deallocate_array => util_deallocate_array
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
            procedure, public :: push_back => push_back_method
            final :: finalizer
    end type


    interface vector_t
        module procedure default_constructor
    end interface


    interface allocator
        module procedure allocate_array
    end interface


    interface deallocator
        module procedure deallocate_array
    end interface


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


        subroutine push_back_method (self, value)
            class(vector_t), intent(inout) :: self
            integer(kind = int32), intent(in) :: value

            if (self % state % init) then
                call insert_back (self, value)
            else
                call initializer (self, value)
            end if

            return
        end subroutine


        subroutine initializer(vector, value)
            type(vector_t), intent(inout) :: vector
            integer(kind = int32), intent(in) :: value
            call create(vector, value)
            return
        end subroutine        


        subroutine create(vector, value)
            ! Synopsis: Creates the first element in vector.
            type(vector_t), intent(inout) :: vector
            integer(kind = int64) :: bounds(0:1)
            integer(kind = int64), parameter :: lb = 0_int64
            integer(kind = int64), parameter :: ub = 8_int64
            integer(kind = int32), intent(in) :: value

            bounds(0) = lb
            bounds(1) = ub
            call allocator(bounds, vector % array % values)

            vector % begin % idx  = 0_int64
            vector % avail % idx  = 0_int64
            vector % limit % idx  = 8_int64
            vector % state % init = .true.

            vector % array % values = 0

            return
        end subroutine        


        subroutine finalizer (vector)
            type(vector_t), intent(inout) :: vector

            if ( allocated(vector % array % values) ) then
                call deallocator(vector % array % values)
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


! subroutine create()
! Allocates an array of 9 elements on purpose to make the
! vector class similar to that defined in the c++ standard
! template library. The size is obtained by subtracting
! the limit and begin iterators.


! TODO:
! [x] Move the array (de)allocator to a module.
! [ ] Initialize array values to zero.
