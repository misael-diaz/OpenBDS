!
!   source: Vector_utils.for
!   author: misael-diaz
!   date:   2021-06-27
!
!
!   Synopsis:
!   Implements (particular) utilities for the vector class.
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

submodule (vectors) vector_utils
    contains


        module subroutine allocate_vector_t (v)
            ! Synopsis: Allocates memory for the vector components.
            type(vector_t), intent(inout) :: v

!           print *, "allocating vector components ... "

            call allocator (v % begin)
            call allocator (v % avail)
            call allocator (v % limit)
            call allocator (v % array)
            call allocator (v % state)

            return
        end subroutine


        module elemental subroutine array_int32_t_copy (dst, src)
            integer(kind = int32), intent(inout) :: dst
            integer(kind = int32), intent(in) :: src
            dst = src
            return
        end subroutine


        module subroutine allocate_iter_t (i)
            type(iter_t), intent(inout), allocatable :: i
            integer(kind = int32) :: mstat

            mstat = 0
            if ( .not. allocated(i) ) then
                allocate (i, stat = mstat)
!               print *, "vector.iter has been allocated"
            end if

            if (mstat /= 0) then
                error stop "vector.allocate_iter_t: allocation error"
            end if

            return
        end subroutine


        module subroutine allocate_data_t (d)
            type(data_t), intent(inout), allocatable :: d
            integer(kind = int32) :: mstat

            mstat = 0
            if ( .not. allocated(d) ) then
                allocate (d, stat = mstat)
!               print *, "vector.data has been allocated"
            end if

            if (mstat /= 0) then
                error stop "vector.allocate_data_t: allocation error"
            end if

            return
        end subroutine


        module subroutine allocate_stat_t (s)
            type(stat_t), intent(inout), allocatable :: s
            integer(kind = int32) :: mstat

            mstat = 0
            if ( .not. allocated(s) ) then
                allocate (s, stat = mstat)
!               print *, "vector.stat has been allocated"
            end if

            if (mstat /= 0) then
                error stop "vector.allocate_stat_t: allocation error"
            end if

            return
        end subroutine


        module subroutine vector_int32_t_allocate_dynamic (b, array, value)
            integer(kind = int64), intent(in) :: b(0:1)       ! b[ounds]
            class(*), intent(inout), allocatable :: array(:)
            integer(kind = int64):: lb
            integer(kind = int64):: ub
            integer(kind = int32), intent(in) :: value
            integer(kind = int32):: mstat


            if ( allocated(array) ) then

                deallocate (array, stat = mstat)

                if (mstat /= 0) then
                    error stop "dynamic::vector.allocate: unexpected error"
                end if

            end if


            lb = b(0)
            ub = b(1)
            allocate (array(lb:ub), mold = value, stat = mstat)

            if (mstat /= 0) then
                error stop "dynamic::vector.allocate: allocation error"
            end if


            return
        end subroutine


        module subroutine deallocate_iter_t (i)
            type(iter_t), intent(inout), allocatable :: i
            integer(kind = int32) :: mstat
            character(len=*), parameter :: errmsg = &
                & "dynamic::vector.deallocate_iter: deallocation error"

            mstat = 0
            if ( allocated(i) ) then
                deallocate (i, stat = mstat)
            end if

            if (mstat /= 0) then
                error stop errmsg
            end if

            return
        end subroutine


        module subroutine deallocate_data_t (d)
            type(data_t), intent(inout), allocatable :: d
            integer(kind = int32) :: mstat
            character(len=*), parameter :: errmsg = &
                & "dynamic::vector.deallocate_data: deallocation error"

            mstat = 0
            if ( allocated(d) ) then
                deallocate (d, stat = mstat)
            end if

            if (mstat /= 0) then
                error stop errmsg
            end if

            return
        end subroutine


        module subroutine deallocate_stat_t (s)
            type(stat_t), intent(inout), allocatable :: s
            integer(kind = int32) :: mstat
            character(len=*), parameter :: errmsg = &
                & "dynamic::vector.deallocate_stat: deallocation error"

            mstat = 0
            if ( allocated(s) ) then
                deallocate (s, stat = mstat)
            end if

            if (mstat /= 0) then
                error stop errmsg
            end if

            return
        end subroutine


        module subroutine vector_int32_t_deallocate_dynamic (array, value)
            ! the compiler cannot differentiate without the dummy value
            class(*), intent(inout), allocatable :: array(:)
            integer(kind = int32), intent(in) :: value
            integer(kind = int32) :: mstat
            character(len=*), parameter :: errmsg = &
                & "dynamic::vector.deallocate_polymorphic: " // &
                & "deallocation error"

            mstat = 0
            if ( allocated(array) ) then
                deallocate (array, stat = mstat)
            end if

            if (mstat /= 0) then
                print *, value  ! use input so that compiler won't complain
                error stop errmsg
            end if

            return
        end subroutine


        module subroutine is_empty (vector)
            type(vector_t), intent(in) :: vector

            if ( .not. allocated(vector % state) ) then
                error stop "dynamic::vector.is_empty: empty vector"
            else if ( .not. vector % state % init ) then
                error stop "dynamic::vector.is_empty: empty vector"
            end if

            return
        end subroutine


        module subroutine is_instantiated (vector)
            type(vector_t), intent(inout) :: vector

            if ( .not. allocated(vector % state) ) then
                call instantiate (vector)
            end if

            return
        end subroutine


        module subroutine check_bounds (vector, idx)
            type(vector_t), intent(in) :: vector
            integer(kind = int64), intent(in) :: idx

            if ( idx < vector % begin % idx ) then
                error stop "dynamic::vector.[i]: i < lbound"
            else if ( idx >= vector % avail % idx ) then
                error stop "dynamic::vector.[i]: i > ubound"
            end if

            return
        end subroutine


!       module function to_string_int32 (i) result(str)
!           integer(kind = int32), intent(in) :: i
!           character(len = 64) :: str
!
!           write (str, '(I16)') i
!           str = adjustl (str)
!
!           return
!       end function


!       module function to_string_int64 (i) result(str)
!           integer(kind = int64), intent(in) :: i
!           character(len = 64) :: str
!
!           write (str, '(I32)') i
!           str = adjustl (str)
!
!           return
!       end function


        module subroutine finalizer (vector)
            type(vector_t), intent(inout) :: vector

!           print *, "destroying dynamic vector components ... "

            call deallocator (vector % begin)
            call deallocator (vector % avail)
            call deallocator (vector % limit)
            call deallocator (vector % array)
            call deallocator (vector % state)

            return
        end subroutine


end submodule


! References:
! SJ Chapman, FORTRAN for Scientists and Engineers, fourth edition
! A Koenig and B Moo, Accelerated C++ Practical Programming by Example