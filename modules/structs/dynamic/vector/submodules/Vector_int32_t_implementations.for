!
!   source: Vector_int32_t_implementations.for
!   author: misael-diaz
!   date:   2021-06-27
!
!
!   Synopsis:
!   Implementation procedures of the vector class.
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

submodule (VectorClass) vector_int32_t_implementation
    implicit none
    contains


        module subroutine vector_int32_t_findloc_wrapper (vector, value, i)
            type(vector_t), intent(in) :: vector
            integer(kind = int64), intent(out) :: i
            integer(kind = int64) :: lb
            integer(kind = int64) :: ub
            integer(kind = int32), intent(in) :: value


            lb = vector % begin % idx
            ub = vector % avail % idx
            ub = ub - 1_int64

            associate (values => vector % array % values)

                select type (values)
                    type is ( integer(kind = int32) )
                        i = findloc(array = values(lb:ub), &
                                  & value = value, dim = 1, kind = int64)
                        i = i - 1_int64
                    class default
                        error stop vector % state % errmsg
                end select

            end associate
            return
        end subroutine


        module subroutine vector_int32_t_indexer (vector, idx, value)
            type(vector_t), intent(in) :: vector
            integer(kind = int64), intent(in) :: idx
            integer(kind = int32), intent(out) :: value

            associate (values => vector % array % values)

                select type (values)
                    type is ( integer(kind = int32) )
                        value = values (idx)
                    class default
                        error stop vector % state % errmsg
                end select

            end associate


            return
        end subroutine


        module subroutine vector_int32_t_push_back (vector, value)
            ! Synopsis: Pushes 32-bit integer unto back of vector.
            type(vector_t), intent(inout) :: vector
            integer(kind = int32), intent(in) :: value


            call is_instantiated (vector)


            if ( vector % state % init ) then
                call insert_back (vector, value)
            else
                call initializer (vector, value)
            end if

            return
        end subroutine


        module subroutine vector_int32_t_insert_back (vector, value)
            ! Synopsis: Inserts value unto back, vector grows as needed.
            type(vector_t), intent(inout), target :: vector
            integer(kind = int32), intent(in) :: value


            if (vector % avail % idx == vector % limit % idx) then
                call grow (vector, value)
            end if


            associate (begin  => vector % begin % idx,  &
                     & avail  => vector % avail % idx,  &
                     & values => vector % array % values)

                select type (values)
                    type is ( integer(kind = int32) )
                        values (avail) = value
                    class default
                        ! caters inserting mixed-types
                        error stop vector % state % errmsg
                end select

                vector % deref % it => vector % array % values(begin:avail)
                avail = avail + 1_int64

            end associate


            return
        end subroutine


        module subroutine vector_int32_t_grow (vector, value)
            ! Synopsis: Doubles the vector size.
            type(vector_t), intent(inout), target :: vector
            integer(kind = int64):: lb
            integer(kind = int64):: ub
            integer(kind = int64):: bounds(0:1)
            integer(kind = int32), intent(in) :: value
            integer(kind = int32), allocatable :: array(:)
            character(len=*), parameter :: errmsg = &
                & "dynamic::vector.grow: unexpected error"


            ! bounds for copying the data
            lb = vector % begin % idx
            ub = vector % avail % idx - 1_int64
            bounds(0) = lb
            bounds(1) = ub
            call allocator (bounds, array)


            ! copies existing values into placeholder
            associate (values => vector % array % values)

                select type (values)
                    type is ( integer(kind = int32) )
                        array(:) = values(lb:ub)
                    class default
                        error stop vector % state % errmsg
                end select

            end associate


            vector % limit % idx = 2_int64 * vector % limit % idx


            bounds(0) = vector % begin % idx
            bounds(1) = vector % limit % idx
            call reallocator (bounds, vector % array % values, value)

            ! copies values in placeholder into (reallocated) vector
            associate (values => vector % array % values)

               select type (values)
                    type is ( integer(kind = int32) )
                        values = 0
                        values(lb:ub) = array
                    class default
                        error stop errmsg
                end select

            end associate

            return
        end subroutine vector_int32_t_grow


        module subroutine vector_int32_t_initializer (vector, value)
            type(vector_t), intent(inout) :: vector
            integer(kind = int32), intent(in) :: value
            call create (vector, value)
            return
        end subroutine


        module subroutine vector_int32_t_create (vector, value)
            ! Synopsis: Creates the first element in vector.
            type(vector_t), intent(inout), target :: vector
            integer(kind = int64) :: bounds(0:1)
            integer(kind = int64), parameter :: lb = 0_int64
            integer(kind = int64), parameter :: ub = 8_int64
            integer(kind = int32), intent(in) :: value
            integer(kind = int32) :: mstat
            character(len=*), parameter :: errmsg = &
                & "dynamic::vector.error: container of 32-bit integers"


!           TODO: consider moving to utils of the vector class
            allocate(character(len=len(errmsg)):: vector % state % errmsg,&
                   & stat = mstat)
            if (mstat /= 0) then
                error stop "dynamic::vector.create: allocation error"
            end if
            vector % state % errmsg(:) = errmsg


            bounds(0) = lb
            bounds(1) = ub
            call allocator (bounds, vector % array % values, value)


            associate (begin  => vector % begin % idx,  &
                     & avail  => vector % avail % idx,  &
                     & limit  => vector % limit % idx,  &
                     & state  => vector % state % init, &
                     & values => vector % array % values)


                begin = lb
                avail = lb
                limit = ub


                select type (values)
                    type is ( integer(kind = int32) )
                        values         = 0
                        values (avail) = value
                    class default
                        error stop "dynamic::vector.create: unexpected err"
                end select

                vector % deref % it => vector % array % values(begin:avail)
                avail = avail + 1_int64
                state = .true.

            end associate


            return
        end subroutine vector_int32_t_create


        module subroutine vector_int32_t_erase_all (vector)
            type(vector_t), intent(inout) :: vector
            integer(kind = int32):: value

            call deallocator (vector % array % values, value)

            vector % begin % idx  = 0_int64
            vector % avail % idx  = 0_int64
            vector % limit % idx  = 0_int64
            vector % deref % idx  = 0_int64
            vector % deref % it   => null()
            vector % state % init = .false.

            return
        end subroutine


        module subroutine vector_int32_t_erase_byIndexShadow (vec, idx, f)
            ! delegates the task to (specialized) subroutines
            type(vector_t), intent(inout) :: vec
            integer(kind = int64), intent(in) :: idx
            logical(kind = int32), intent(in), optional :: f

            call check_bounds (vec, idx)
            call vector_int32_t_erase_by_index (vec, idx)

            return
        end subroutine


        module subroutine vector_int32_t_erase_by_index (vector, idx)
            type(vector_t), intent(inout) :: vector
            integer(kind = int64), intent(in) :: idx


            associate (avail => vector % avail % idx)
                if (idx == avail - 1_int64) then
                    call vector_int32_t_erase_final_value (vector)
                else
                    call vector_int32_t_erase_intermediate (vector, idx)
                end if
            end associate


            return
        end subroutine


        module subroutine vector_int32_t_erase_final_value (vector)
            ! Synopsis: Erases the last stored value in vector.
            type(vector_t), intent(inout), target :: vector
            integer(kind = int64) :: final

            associate (begin => vector % begin % idx, &
                     & avail => vector % avail % idx)
                avail = avail - 1_int64
                final = avail - 1_int64
                vector % deref % it => vector % array % values(begin:final)
            end associate

            return
        end subroutine


        module subroutine vector_int32_t_erase_intermediate (vector, idx)
            ! Synopsis: Erases the value pointed to by index.
            type(vector_t), intent(inout), target :: vector
            integer(kind = int64), intent(in) :: idx
            integer(kind = int64):: final
            integer(kind = int64):: lb_ary, lb_vec, lb
            integer(kind = int64):: ub_ary, ub_vec, ub
            integer(kind = int64):: ary_bounds(0:1)
            integer(kind = int32), allocatable :: array(:)
            character(len=*), parameter :: errmsg = &
                & "dynamic::vector.erase(idx): unexpected error"


            associate (begin => vector % begin % idx,   &
                     & avail => vector % avail % idx,   &
                     & values => vector % array % values)

                ! overwrites vector values in range [idx, avail - 1)
                lb     = idx
                ub     = avail - 2_int64
                ! copies vector values in range [idx + 1, avail)
                lb_vec = lb + 1_int64
                ub_vec = ub + 1_int64

                ! placeholder's range for vector values in [idx + 1, avail)
                lb_ary = 0_int64
                ub_ary = avail - (idx + 1_int64) - 1_int64
                ary_bounds(0) = lb_ary
                ary_bounds(1) = ub_ary
                call allocator (ary_bounds, array)


                select type (values)
                    type is ( integer(kind = int32 ) )
                        array(:) = values(lb_vec:ub_vec)
                        values(lb:ub) = array(:)
                    class default
                        error stop errmsg
                end select


                avail = avail - 1_int64
                final = avail - 1_int64
                vector % deref % it => vector % array % values(begin:final)

            end associate


            return
        end subroutine


        module subroutine vector_int32_t_erase_byRangeShadow (vec, b, f)
            type(vector_t), intent(inout) :: vec
            integer(kind = int64), intent(in) :: b(2)
            integer(kind = int64):: lb, ub
            logical(kind = int32), intent(in), optional :: f

            lb = lbound(b, dim = 1, kind = int64)
            ub = ubound(b, dim = 1, kind = int64)
            lb = b(lb)
            ub = b(ub)
            call check_bounds (vec, lb)
            call check_bounds (vec, ub)
            if (lb <= ub) then
                call vector_int32_t_erase_by_range (vec, b)
!           else
!               print *, "empty range ... " ! passed test
            end if

            return
        end subroutine


        module subroutine vector_int32_t_erase_by_range (vector, bounds)
            type(vector_t), intent(inout) :: vector
            integer(kind = int64), intent(in) :: bounds(2)
            integer(kind = int64) :: lb, ub
            integer(kind = int64) :: final

            lb = lbound(bounds, dim = 1, kind = int64)
            ub = ubound(bounds, dim = 1, kind = int64)

            lb = bounds(lb)
            ub = bounds(ub)

            final = vector % avail % idx - 1_int64
            if (lb == 0_int64 .and. ub == final) then
                call vector_int32_t_erase_all (vector)
            else
                call vector_int32_t_trim (vector, bounds)
            end if

            return
        end subroutine


        module subroutine vector_int32_t_trim (vector, vec_bounds)
            type(vector_t), intent(inout), target :: vector
            integer(kind = int64), intent(in) :: vec_bounds(2)
            integer(kind = int64):: final
            integer(kind = int64):: ary_bounds(0:1)
            integer(kind = int64):: lb, lb_ary
            integer(kind = int64):: ub, ub_ary
            integer(kind = int64):: i, j, idx, numel
            integer(kind = int32), allocatable :: array(:)
            integer(kind = int32), allocatable :: mask(:)
            character(len=*), parameter :: errmsg = &
                & "dynamic::vector.trimming: unexpected error"

            lb = lbound(vec_bounds, dim = 1, kind = int64)
            ub = ubound(vec_bounds, dim = 1, kind = int64)

            ! defines the (inclusive) trimming range lb:ub
            lb = vec_bounds(lb)
            ub = vec_bounds(ub)


            ! masks (numel) elements to copy from vector into array
            lb_ary = 0_int64
            ub_ary = vector % avail % idx - 1_int64
            ary_bounds(0) = lb_ary
            ary_bounds(1) = ub_ary
            call allocator (ary_bounds, mask)

            ! masks values for erasing
            mask(:) = 0
            mask(lb:ub) = 1
            numel = (ub - lb) + 1_int64

            lb_ary = 0_int64
            ub_ary = vector % avail % idx - numel - 1_int64
            ary_bounds(0) = lb_ary
            ary_bounds(1) = ub_ary
            call allocator (ary_bounds, array)

            associate (begin  => vector % begin % idx,  &
                     & avail  => vector % avail % idx,  &
                     & values => vector % array % values)


                select type (values)
                    type is ( integer(kind = int32) )

                        idx = 0_int64
                        ! copies values unselected (for removal) into array
                        do i = begin, avail - 1_int64
                            if ( mask(i) == 0 ) then
                                array(idx) = values(i)
                                idx = idx + 1_int64
                            end if
                        end do

                        j = 0_int64
                        idx = 0_int64
                        ! effectively erases values by overwriting
                        do i = begin, avail - 1_int64
                            if ( mask(i) == 1 ) then
                                values(j) = array(idx)
                                j = j + 1_int64
                                idx = idx + 1_int64
                            end if

                            ! breaks if there are no more values to copy
                            if ( idx == avail - numel ) then
                                exit
                            end if

                        end do

                    class default
                        error stop errmsg
                end select


                avail = avail - numel
                final = avail - 1_int64
                vector % deref % it => vector % array % values(begin:final)

            end associate

            return
        end subroutine


        module subroutine vector_int32_t_erase_subs_shadow (vec, vs, f)
            ! delegates the task to (specialized) subroutines
            type(vector_t), intent(inout) :: vec
            integer(kind = int64), intent(in) :: vs(:)
            integer(kind = int64):: lb, ub, numel
            logical(kind = int32), intent(in), optional :: f

            ! queries the bounds and size of the vector-subscript
            lb = minval(vs)
            ub = maxval(vs)
            numel = size(vs, kind = int64)

            call check_bounds (vec, lb)
            call check_bounds (vec, ub)

            if ( numel == vec % size() ) then
                call vector_int32_t_erase_all (vec)
            else
                call vector_int32_t_erase_by_subscript (vec, vs)
            end if

            return
        end subroutine


        module subroutine vector_int32_t_erase_by_subscript (vector, vs)
            ! erases vector elements marked by the vector-subscript `vs'
            type(vector_t), intent(inout), target :: vector
            integer(kind = int64), intent(in) :: vs(:)
            integer(kind = int64):: final
            integer(kind = int64):: ary_bounds(0:1)
            integer(kind = int64):: lb, ub
            integer(kind = int64):: i, idx, numel
            integer(kind = int32), allocatable :: array(:)
            integer(kind = int32), allocatable :: mask(:)
            character(len=*), parameter :: errmsg = &
                & "dynamic::vector.erase_by_subscript: unexpected error"

            ! queries for the bounds and size of the vector-subscript
            lb = lbound(vs, dim = 1, kind = int64)
            ub = ubound(vs, dim = 1, kind = int64)
            numel = size(vs)

            ary_bounds(0) = 0_int64
            ary_bounds(1) = vector % avail % idx - numel - 1_int64
            call allocator (ary_bounds, array)

            ary_bounds(0) = 0_int64
            ary_bounds(1) = vector % avail % idx - 1_int64
            call allocator (ary_bounds, mask)

            mask = 0
            ! marks vector elements for erasing
            do idx = lb, ub
                mask( vs(idx) ) = 1
            end do


            associate (begin  => vector % begin % idx,  &
                     & avail  => vector % avail % idx,  &
                     & values => vector % array % values)

                select type (values)
                    type is ( integer(kind = int32) )


                        i = 0_int64
                        ! copies ``unselected'' elements into placeholder
                        do idx = begin, (avail - 1_int64)
                            if ( mask(idx) == 0 ) then
                                array(i) = values(idx)
                                i = i + 1_int64
                            end if
                        end do


                        ! erases selected elements by overwriting
                        values( 0 : (avail - numel - 1_int64) ) = array(:)


                    class default
                        error stop errmsg
                end select

                avail = avail - numel
                final = avail - 1_int64
                vector % deref % it => vector % array % values(begin:final)
            end associate


            return
        end subroutine


        module subroutine vector_int32_t_erase_values (vector, elements)
            ! erases vector elements equal to ``elements''
            type(vector_t), intent(inout) :: vector
            integer(kind = int64), allocatable :: vs(:)
            integer(kind = int32), intent(in) :: elements(:)
            integer(kind = int32), allocatable :: mask(:)
            integer(kind = int64):: ary_bounds(0:1), bounds(0:1)
            integer(kind = int64):: i, lb, ub, idx, numel
            character(len=*), parameter :: errmsg = &
                & "dynamic::vector.erase_values: unexpected error"


            ! queries for the bounds and size of the array of elements
            numel = size(elements, kind = int64)
            lb = lbound(elements, dim = 1, kind = int64)
            ub = ubound(elements, dim = 1, kind = int64)

            ary_bounds(0) = 0_int64
            ary_bounds(1) = vector % avail % idx - 1_int64
            call allocator (ary_bounds, mask)


            associate (begin  => vector % begin % idx,  &
                     & avail  => vector % avail % idx,  &
                     & values => vector % array % values)

                select type (values)
                    type is ( integer(kind = int32) )

                        mask = 0
                        do idx = begin, (avail - 1_int64)

                            ! masks values equal to any of the elements
                            do i = lb, ub
                                if ( values(idx) == elements(i) ) then
                                    mask(idx) = mask(idx) + 1
                                else
                                    mask(idx) = mask(idx) + 0
                                end if
                            end do

                            mask(idx) = min(mask(idx), 1) !! mask is 0|1
                        end do

                    class default
                        error stop errmsg
                end select

            end associate


            ! finds the number of elements marked for removal and delegates
            numel = int (sum(mask), kind = int64)

            if ( numel > 0_int64 ) then

                if ( numel == vector % avail % idx ) then
                    call vector_int32_t_erase_all (vector)
                else

                    bounds(0) = 0_int64
                    bounds(1) = numel - 1_int64
                    call allocator (bounds, vs)

                    i = 0_int64
                    ! generates vector-subscript for erase method
                    do idx = ary_bounds(0), ary_bounds(1)
                        if ( mask(idx) == 1 ) then
                            vs(i) = idx
                            i = i + 1_int64
                        end if
                    end do

                    call vector_int32_t_erase_by_subscript (vector, vs)

                end if

            end if


            return
        end subroutine


        module subroutine vector_int32_t_erase_argsCheck (i, b, s, v, m)
            ! Synopsis:
            ! Checks input arg[ument]s of erase method.
            integer(kind = int64), intent(in), optional :: i
            integer(kind = int64), intent(in), optional :: b(2)
            integer(kind = int64), intent(in), optional :: s(:)
            integer(kind = int32), intent(in), optional :: v(:)
            character(len=9), intent(in),      optional :: m
            character(len=*), parameter :: wrnmsg = &
                & "dynamic::vector.erase(): ignoring mode, it's only " // &
                & "meaningful for ranges"
            character(len=*), parameter :: errmsg = &
                & "dynamic::vector.erase(): erases either by index, "  // &
                & "subscript, range, or value, exclusively"
            character(len=*), parameter :: errmsg_mode = &
                & "dynamic::vector.erase(): missing range for " // &
                & "applying the supplied mode"


            if ( present(i) ) then
                if ( present(b) ) then
                    print *, errmsg
                else if ( present(s) ) then
                    print *, errmsg
                else if ( present(v) ) then
                    print *, errmsg
                end if
            end if


            if ( present(b) ) then
                if ( present(s) ) then
                    print *, errmsg
                else if ( present(v) ) then
                    print *, errmsg
                end if
            end if


            if ( present(s) ) then
                if ( present(v) ) then
                    print *, errmsg
                end if
            end if


            if ( present(m) ) then

                if ( .not. present(b) ) then
                    print *, errmsg_mode
                end if

                if ( present(i) .or. present(s) .or. present(v) ) then
                    print *, wrnmsg
                end if

            end if


            return
        end subroutine


end submodule


! References:
! SJ Chapman, FORTRAN for Scientists and Engineers, fourth edition
! A Koenig and B Moo, Accelerated C++ Practical Programming by Example


! The vector class presented by Koening and Moo inspired me to write my
! own in fortran. I borrowed some of their ideas to implement it.


! Comments:
! A vector is a container that holds values of the same type. The
! "default class" in select type constructs sometimes is used as a
! mechanism that saves the user from catastrophy:
! An array in Fortran can only hold values of the same type and so the
! vector class which uses the Fortran array as the underlying data
! structure to store values.


! TODO:
! [x] GUARD against indexing beyond the bounds of the vector
! [x] CHECK procedures that attempt to use the vector (allocatable)
!     components. Instantiating the vector might suffice to fix some.
! [x] CHECK procedures that rely on the status value to make decisions.
!     Note that the status component is now an allocatable, so that it
!     is unsafe to check for its value since it could be unallocated.
!     A possible solution is to check for the allocation status first,
!     then check for the value.
! [ ] PARTITION further into submodules. Idea: keep methods in a submodule
!     and implementations in another. You will need another for utility
!     procedures (for memory management).
! [x] IMPLEMENT method that returns an iterator to values in the asymmetric
!     range [begin, avail). Implement the iterator as a pointer.
! [ ] IMPLEMENT a polymorphic push-back method (value is of type class(*)).
!     Use a select type construct to determine what version
!     (of "proxy" procedure) to call at runtime.
!     Note: Most methods delegates work to a "proxy" of sorts. The method
!     test_polymorphic_method() may be used as a template for other ones.
! [ ] GUARD against using the vector to store integers of different types.
!     If the user inserts a 32-bit integer it won't be able to insert
!     a 64-bit integer unless the vector contents are cleared. A possible
!     implementation is to check the allocation status of its counterpart
!     upon calls to the push-back method.
! [x] EXPERIMENT with dynamically allocated strings. Not fully supported
!     by GNU Fortran Compiler. Cannot use them without disabling some
!     compiler options I would like to retain.


! Comments:
! For now iterators are implemented as indexes.
!


! Comments on Procedures:
!
! subroutine vector_int32_t_create()
! Allocates one more element on purpose to compute the size of the
! vector via: (end - begin) as in c++.
!
!
! function findloc_wrapper_method (self, value) result(idx)
! lower and upper bounds (lb, ub) are chosen so that we do not include
! the element pointed to by (end). It's a valid index but it should not
! be referenced since it does not hold an actual value.
!
! Index returned by findloc intrinsic has been adjusted by one to
! account for the array bounds. I am unsure if this a BUG in the
! implementation of findloc or a misinterpretation on my part.
! Both the Intel and GNU Fortran Compilers yield the same result.
! I cannot conclude anything from that but at least account for it
! when using the compilers I have available.
!
! Documentation of findloc from GNU Fortran Compiler:
! Determines the *location* of the element in the array with the value
! given in the VALUE argument ... They used *location* instead of index
! so maybe the user is responsible for obtaining the index from the
! location as it has been done here.
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
