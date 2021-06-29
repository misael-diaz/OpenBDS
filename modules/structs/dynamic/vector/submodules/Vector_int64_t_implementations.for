!
!   source: Vector_int64_t_implementations.for
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

submodule (vectors) vectors_int64_t_implementation
    contains


        module subroutine vector_int64_t_findloc_wrapper (vector, value, i)
            type(vector_t), intent(in) :: vector
            integer(kind = int64), intent(out) :: i
            integer(kind = int64) :: lb
            integer(kind = int64) :: ub
            integer(kind = int64), intent(in) :: value


            lb = vector % begin % idx
            ub = vector % avail % idx
            ub = ub - 1_int64

            associate (values => vector % array % values)

                select type (values)
                    type is ( integer(kind = int64) )
                        i= findloc(array = values(lb:ub), &
                                 & value = value, dim = 1, kind = int64)
                    class default
                        error stop vector % state % errmsg
                end select

            end associate
            return
        end subroutine


        module subroutine vector_int64_t_indexer (vector, idx, value)
            type(vector_t), intent(in) :: vector
            integer(kind = int64), intent(in) :: idx
            integer(kind = int64), intent(out) :: value

            associate (values => vector % array % values)

                select type (values)
                    type is ( integer(kind = int64) )
                        value = values (idx)
                    class default
                        error stop vector % state % errmsg
                end select

            end associate


            return
        end subroutine


        module subroutine vector_int64_t_push_back (vector, value)
            ! Synopsis: Pushes 64-bit integer unto back of vector.
            type(vector_t), intent(inout) :: vector
            integer(kind = int64), intent(in) :: value


            call is_instantiated (vector)


            if ( vector % state % init ) then
                call insert_back (vector, value)
            else
                call initializer (vector, value)
            end if

            return
        end subroutine


        module subroutine vector_int64_t_insert_back (vector, value)
            ! Synopsis: Inserts value unto back, vector grows as needed.
            type(vector_t), intent(inout) :: vector
            integer(kind = int64), intent(in) :: value


            if (vector % avail % idx == vector % limit % idx) then
                call grow (vector, value)
            end if


            associate(avail  => vector % avail % idx,&
                      values => vector % array % values)

                select type (values)
                    type is ( integer(kind = int64) )
                        values (avail) = value
                        avail = avail + 1_int64
                    class default
                        ! caters inserting mixed-types
                        error stop vector % state % errmsg
                end select

            end associate


            return
        end subroutine


        module subroutine vector_int64_t_slice (vector, it)
            ! Synopsis: Binds iterator to slice of (pushed values).
            type(vector_t), intent(in), target :: vector
            integer(kind = int64), intent(inout), &
                & pointer, contiguous :: it(:)
            integer(kind = int64) :: lb
            integer(kind = int64) :: ub


            lb = vector % begin % idx
            ub = vector % avail % idx
            ub = ub - 1_int64


            associate (values => vector % array % values)

                select type (values)
                    type is ( integer(kind = int64) )
                        it => values (lb:ub)
                    class default
                        error stop vector % state % errmsg
                end select

            end associate


            return
        end subroutine


        module subroutine vector_int64_t_grow (vector, value)
            ! Synopsis: Doubles the vector size.
            type(vector_t), intent(inout), target :: vector
            integer(kind = int64):: lb
            integer(kind = int64):: ub
            integer(kind = int64):: bounds(0:1)
            integer(kind = int64), intent(in) :: value
            integer(kind = int64), allocatable :: array(:)
            integer(kind = int64), pointer, contiguous :: ptr(:)


            lb = vector % begin % idx
            ub = vector % limit % idx
            bounds(0) = lb
            bounds(1) = ub
            call allocator (bounds, array)


            ! copies existing values into placeholder
            associate (values => vector % array % values)

                select type (values)
                    type is ( integer(kind = int64) )
                        ptr => values(:)
                        call copy (array, ptr)
                    class default
                        error stop vector % state % errmsg
                end select

            end associate


            vector % limit % idx = 2_int64 * vector % limit % idx


!           bounds(0) = vector % begin % idx
            bounds(1) = vector % limit % idx
            call reallocator (bounds, vector % array % values, value)

            ! copies values in placeholder into (reallocated) vector
            associate (values => vector % array % values)

               select type (values)
                    type is ( integer(kind = int64) )
                       values = 0
                       call copy (values(lb:ub), array)
                    class default
                        error stop "dynamic::vector.grow: unexpected error"
                end select

            end associate

            ! TODO: check with valgrind if the array is automatically
            !       deallocated upon return to caller. Would the
            !       deallocation be handled by the OS rather than the
            !       program in this case? Would not deallocating
            !       explicitly result in a speed up.
            ! TEST: outcome, array is automatically deallocated on return.
            !       Did not check the runtime.
!           call deallocator (array)


            return
        end subroutine vector_int64_t_grow


        module subroutine vector_int64_t_initializer (vector, value)
            type(vector_t), intent(inout) :: vector
            integer(kind = int64), intent(in) :: value
            call create (vector, value)
            return
        end subroutine


        module subroutine vector_int64_t_create (vector, value)
            ! Synopsis: Creates the first element in vector.
            type(vector_t), intent(inout) :: vector
            integer(kind = int64) :: bounds(0:1)
            integer(kind = int64), parameter :: lb = 0_int64
            integer(kind = int64), parameter :: ub = 8_int64
            integer(kind = int64), intent(in) :: value
            integer(kind = int32) :: mstat
            character(len=*), parameter :: errmsg = &
                & "dynamic::vector.error: container of 64-bit integers"


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

                select type (values)
                    type is ( integer(kind = int64) )
                        values         = 0
                        values (avail) = value
                    class default
                        error stop "dynamic::vector.create: unexpected err"
                end select

                begin = 0_int64
                avail = avail + 1_int64
                limit = ub
                state = .true.

            end associate


            return
        end subroutine vector_int64_t_create


end submodule


! References:
! SJ Chapman, FORTRAN for Scientists and Engineers, fourth edition
! A Koenig and B Moo, Accelerated C++ Practical Programming by Example
