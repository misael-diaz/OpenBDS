!
!   source: Vector_submod.for
!   author: misael-diaz
!   date:   2021-06-10
!
!
!   Synopsis:
!   Implements a (minimalistic) vector class.
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

!           print *, "instantiating vector ... "
            call instantiate (vector)

            return
        end function


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


        module subroutine instantiate (vector)
            type(vector_t), intent(inout) :: vector

            call allocator (vector)

!           print *, "instantiating vector components ... "

            vector % begin % idx  = 0_int64
            vector % avail % idx  = 0_int64
            vector % limit % idx  = 0_int64
            vector % state % init = .false.

            return
        end subroutine


        module function findloc_method (self, value) result(idx)
            class(vector_t), intent(in) :: self
            integer(kind = int64) :: idx
            integer(kind = int32), intent(in) :: value

            call is_empty (self)
            call findloc_wrapper (self, value, idx)

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

            call is_empty (self)
            call check_bounds (self, idx)

            value = self % array % values_int32_t (idx)

            return
        end function


        module function size_method (self) result(vector_size)
            ! Synopsis: Returns the size of the vector.
            class(vector_t), intent(in) :: self
            integer(kind = int64) :: vector_size

            if ( .not. allocated(self % state) ) then
                vector_size = 0_int64
            else

                associate (begin => self % begin % idx, &
                         & end   => self % avail % idx)
                    vector_size = end - begin
                end associate

            end if

            return
        end function


        module subroutine clear_method (self)
            ! Synopsis: Clears the vector elements.
            class(vector_t), intent(inout) :: self

            call is_instantiated (self)

            self % avail % idx = 0_int64
            return
        end subroutine


        module subroutine push_back_method (self, value)
            ! Synopsis: Pushes value unto back of vector.
            class(vector_t), intent(inout) :: self
            integer(kind = int32), intent(in) :: value


            call is_instantiated (self)


            if ( self % state % init ) then
                call check_kind  (self, value)
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
            vector % state % kind = int32


            return
        end subroutine create


        module subroutine allocate_iter_t (i)
            type(iter_t), intent(inout), allocatable :: i
            integer(kind = int32) :: mstat

            mstat = 0
            if ( .not. allocated(i) ) then
                allocate (i, stat = mstat)
!               print *, "vector.iter has been allocated"
            end if

            if (mstat /= 0) then
                error stop "vector.allocate_iter_t: allocation failure"
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
                error stop "vector.allocate_data_t: allocation failure"
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
                error stop "vector.allocate_stat_t: allocation failure"
            end if

            return
        end subroutine


        module subroutine deallocate_iter_t (i)
            type(iter_t), intent(inout), allocatable :: i
            integer(kind = int32) :: mstat

            mstat = 0
            if ( allocated(i) ) then
                deallocate (i, stat = mstat)
            end if

            if (mstat /= 0) then
                error stop "dynamic::vector.deallocate_iter: "// &
                    & "deallocation failure"
            end if

            return
        end subroutine


        module subroutine deallocate_data_t (d)
            type(data_t), intent(inout), allocatable :: d
            integer(kind = int32) :: mstat

            mstat = 0
            if ( allocated(d) ) then
                deallocate (d, stat = mstat)
            end if

            if (mstat /= 0) then
                error stop "dynamic::vector.deallocate_data: "// &
                    & "deallocation failure"
            end if

            return
        end subroutine


        module subroutine deallocate_stat_t (s)
            type(stat_t), intent(inout), allocatable :: s
            integer(kind = int32) :: mstat

            mstat = 0
            if ( allocated(s) ) then
                deallocate (s, stat = mstat)
            end if

            if (mstat /= 0) then
                error stop "dynamic::vector.deallocate_stat: "// &
                    & "deallocation failure"
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


        module subroutine check_kind_int32 (vector, value)
            type(vector_t), intent(in) :: vector
            integer(kind = int32), intent(in) :: value
            character(len = 64) :: value_str
            character(len = 64) :: values_kind_str
            character(len = 64) :: vector_kind_str


            value_str       = to_string (value)
            values_kind_str = to_string (int32)
            vector_kind_str = to_string (vector % state % kind)


            if ( vector % state % kind /= int32 ) then
                error stop "dynamic::vector.check_kind: " // &
                    & "vector is a Container of integers of Kind " // &
                    & trim(vector_kind_str) // " but given value [" // &
                    & trim(value_str) // "] is of Kind " // &
                    & trim(values_kind_str)
            end if


            return
        end subroutine check_kind_int32


        module subroutine check_kind_int64 (vector, value)
            type(vector_t), intent(in) :: vector
            integer(kind = int64), intent(in) :: value
            character(len = 64) :: value_str
            character(len = 64) :: values_kind_str
            character(len = 64) :: vector_kind_str


            value_str       = to_string (value)
            values_kind_str = to_string (int64)
            vector_kind_str = to_string (vector % state % kind)


            if ( vector % state % kind /= int64 ) then
                error stop "dynamic::vector.check_kind: " // &
                    & "vector is a Container of integers of Kind " // &
                    & trim(vector_kind_str) // " but given value [" // &
                    & trim(value_str) // "] is of Kind " // &
                    & trim(values_kind_str)
            end if


            return
        end subroutine check_kind_int64


        module function to_string_int32 (i) result(str)
            integer(kind = int32), intent(in) :: i
            character(len = 64) :: str

            write (str, '(I16)') i
            str = adjustl (str)

            return
        end function


        module function to_string_int64 (i) result(str)
            integer(kind = int64), intent(in) :: i
            character(len = 64) :: str

            write (str, '(I32)') i
            str = adjustl (str)

            return
        end function


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


! The vector class presented by Koening and Moo inspired me to write my
! own in fortran. I borrowed some of their ideas to implement it.


! TODO:
! [x] GUARD against indexing beyond the bounds of the vector
! [x] CHECK procedures that attempt to use the vector (allocatable)
!     components. Instantiating the vector might suffice to fix some.
! [x] CHECK procedures that rely on the status value to make decisions.
!     Note that the status component is now an allocatable, so that it
!     is unsafe to check for its value since it could be unallocated.
!     A possible solution is to check for the allocation status first,
!     then check for the value.
! [ ] GUARD against using the vector to store integers of different types.
!     If the user inserts a 32-bit integer it won't be able to insert
!     a 64-bit integer unless the vector contents are cleared. A possible
!     implementation is to check the allocation status of its counterpart
!     upon calls to the push-back method.


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
