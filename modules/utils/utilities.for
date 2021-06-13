!
!   source: utilities.for
!   author: misael-diaz
!   date:   2021-06-10
!
!
!   Synopsis:
!   Defines the interfaces to utility procedures.
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

module utils
    use, intrinsic :: iso_fortran_env, only: int32, int64
    implicit none
    public


    interface util_allocate_array
        module procedure util_allocate_array_int32_by_size
        module procedure util_allocate_array_int64_by_size
        module procedure util_allocate_array_by_bounds
    end interface


    interface util_reallocate_array
        module procedure util_reallocate_array_by_size
        module procedure util_reallocate_array_by_bounds
    end interface


    interface util_deallocate_array
        module procedure util_deallocate_array_int32
        module procedure util_deallocate_array_int64
    end interface


    interface
        module subroutine util_allocate_array_by_bounds (bounds, values)
            integer(kind = int64), intent(in) :: bounds(0:1)
            integer(kind = int32), intent(inout), allocatable :: values(:)
        end subroutine


        module subroutine util_allocate_array_int64_by_size (n, values)
            integer(kind = int64), intent(in) :: n
            integer(kind = int64), intent(inout), allocatable :: values(:)
        end subroutine


        module subroutine util_allocate_array_int32_by_size (n, values)
            integer(kind = int64), intent(in) :: n
            integer(kind = int32), intent(inout), allocatable :: values(:)
        end subroutine


        module subroutine util_reallocate_array_by_bounds (bounds, values)
            integer(kind = int64), intent(in) :: bounds(0:1)
            integer(kind = int32), intent(inout), allocatable :: values(:)
        end subroutine


        module subroutine util_reallocate_array_by_size (n, values)
            integer(kind = int64), intent(in) :: n
            integer(kind = int32), intent(inout), allocatable :: values(:)
        end subroutine


        module subroutine util_deallocate_array_int32 (values)
            integer(kind = int32), intent(inout), allocatable :: values(:)
        end subroutine


        module subroutine util_deallocate_array_int64 (values)
            integer(kind = int64), intent(inout), allocatable :: values(:)
        end subroutine
    end interface

end module
