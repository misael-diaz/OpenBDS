!
!   source: utils_submod.for
!   author: misael-diaz
!   date:   2021-06-12
!
!
!   Synopsis:
!   Implements utility procedures.
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

submodule (utils) util_implementations
    use, intrinsic :: iso_fortran_env, only: int32, int64
    implicit none
    contains


        module subroutine util_allocate_array_by_bounds (bounds, values)
            integer(kind = int64), intent(in) :: bounds(0:1)
            integer(kind = int64) :: lb
            integer(kind = int64) :: ub
            integer(kind = int32), intent(inout), allocatable :: values(:)
            integer(kind = int32) :: mstat

            lb = bounds(0)
            ub = bounds(1)
            allocate (values(lb:ub), stat = mstat)
            if (mstat /= 0) then
                error stop "util_allocate_array: insufficient memory"
            end if

            return
        end subroutine


        module subroutine util_allocate_array_int64_by_size (n, values)
            integer(kind = int64), intent(in) :: n
            integer(kind = int64), intent(inout), allocatable :: values(:)
            integer(kind = int32) :: mstat

            allocate (values(n), stat = mstat)
            if (mstat /= 0) then
                error stop "util_allocate_array: insufficient memory"
            end if

            return
        end subroutine


        module subroutine util_allocate_array_int32_by_size (n, values)
            integer(kind = int64), intent(in) :: n
            integer(kind = int32), intent(inout), allocatable :: values(:)
            integer(kind = int32) :: mstat

            allocate (values(n), stat = mstat)
            if (mstat /= 0) then
                error stop "util_allocate_array: insufficient memory"
            end if

            return
        end subroutine


        module subroutine util_reallocate_array_by_bounds (bounds, values)
            integer(kind = int64), intent(in) :: bounds(0:1)
            integer(kind = int64) :: lb
            integer(kind = int64) :: ub
            integer(kind = int32), intent(inout), allocatable :: values(:)
            integer(kind = int32) :: mstat

            call util_deallocate_array (values)

            lb = bounds(0)
            ub = bounds(1)
            allocate (values(lb:ub), stat = mstat)
            if (mstat /= 0) then
                error stop "util_reallocate_array: insufficient memory"
            end if

            return
        end subroutine


        module subroutine util_reallocate_array_by_size (n, values)
            integer(kind = int64), intent(in) :: n
            integer(kind = int32), intent(inout), allocatable :: values(:)
            integer(kind = int32) :: mstat

            call util_deallocate_array (values)

            allocate (values(n), stat = mstat)
            if (mstat /= 0) then
                error stop "util_reallocate_array: insufficient memory"
            end if

            return
        end subroutine


        module subroutine util_deallocate_array_int32 (values)
            integer(kind = int32), intent(inout), allocatable :: values(:)
            integer(kind = int32) :: mstat = 0

            if ( allocated(values) ) then
                deallocate(values, stat = mstat)
            end if

            if (mstat /= 0) then
                error stop "util_deallocate_array: unexpected error"
            end if

            return
        end subroutine


        module subroutine util_deallocate_array_int64 (values)
            integer(kind = int64), intent(inout), allocatable :: values(:)
            integer(kind = int32) :: mstat = 0

            if ( allocated(values) ) then
                deallocate(values, stat = mstat)
            end if

            if (mstat /= 0) then
                error stop "util_deallocate_array: unexpected error"
            end if

            return
        end subroutine


end submodule
