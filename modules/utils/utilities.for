!
!   source: utilities.for
!   author: misael-diaz
!   date:   2021-06-10
!
!
!   Synopsis:
!   Defines utility procedures.
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
    contains


        subroutine util_allocate_array (n, values)
            integer(kind = int64), intent(in) :: n
            integer(kind = int32), intent(inout), allocatable :: values(:)
            integer(kind = int32) :: mstat

            allocate (values(n), stat = mstat)
            if (mstat /= 0) then
                error stop "util_allocate_array: insufficient memory"
            end if

            return
        end subroutine


        subroutine util_reallocate_array (n, values)
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


        subroutine util_deallocate_array (values)
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
end module
