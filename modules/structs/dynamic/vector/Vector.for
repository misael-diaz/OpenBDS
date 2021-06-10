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
!   use utils, only: allocate_array   => util_allocate_array
    use utils, only: deallocate_array => util_deallocate_array
    implicit none


    type :: iter_t
        integer(kind = int64) :: idx = 0_int64
    end type


    type :: data_t
        integer(kind = int32), allocatable :: values(:)
    end type


    type, public :: vector_t
        private
        type(iter_t):: begin
        type(iter_t):: avail
        type(iter_t):: limit
        type(data_t):: array
        contains
!           procedure, public :: push_back => push_back_method
            final :: finalizer
    end type


    interface vector_t
        module procedure default_constructor
    end interface


    interface deallocator
        module procedure deallocate_array
    end interface


    private
    contains


        function default_constructor() result(vector)
            ! Synopsis: Returns an empty vector
            type(vector_t):: vector

            vector % begin % idx = 0_int64
            vector % avail % idx = 0_int64
            vector % limit % idx = 0_int64

            return
        end function


        subroutine finalizer(vector)
            type(vector_t), intent(inout) :: vector

            if ( allocated(vector % array % values) ) then
                call deallocator(vector % array % values)
            end if

            return
        end subroutine
end module

! References:
! SJ Chapman, FORTRAN for Scientists and Engineers, fourth edition


! Comments:
! For now iterators are implemented as indexes.


! TODO:
! Move the array (de)allocator to a module.
