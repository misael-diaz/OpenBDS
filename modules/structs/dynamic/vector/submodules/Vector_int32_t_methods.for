!
!   source: Vector_methods.for
!   author: misael-diaz
!   date:   2021-06-27
!
!
!   Synopsis:
!   Defines methods for vectors of 32-bit integers.
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

submodule (VectorClass) vector_int32_t_methods
    implicit none
    contains


        module function vector_int32_t_find_method (self, value) result(i)
            ! Synopsis: Returns index of element having the sought value.
            class(vector_t), intent(in) :: self
            integer(kind = int64) :: i
            integer(kind = int32), intent(in) :: value

            call is_empty (self)
            call find (self, value, i)

            return
        end function


        module subroutine vector_int32_t_indexing_method (self, idx, value)
            ! Synopsis: Addresses the element pointed to by index.
            class(vector_t), intent(in) :: self
            integer(kind = int64), intent(in) :: idx
            integer(kind = int32), intent(out) :: value

            call is_empty (self)
            call check_bounds (self, idx)

            call indexer (self, idx, value)

            return
        end subroutine


        module subroutine vector_int32_t_push_back_method (self, value)
            ! Synopsis: Pushes value unto back of vector.
            class(vector_t), intent(inout) :: self
            integer(kind = int32), intent(in) :: value
            call back_inserter (self, value)
            return
        end subroutine


        module subroutine vector_int32_t_erase_method(vec, i, b, s, v, m, f)
            ! Synopsis:
            ! Erases values either by index, range, subscript, or value(s).
            ! mode: [in|ex]clusive
            ! flip: inverts logic, erases all but those specified.
            class(vector_t), intent(inout) :: vec
            integer(kind = int64), intent(in), optional :: i       ! index
            integer(kind = int64), intent(in), optional :: b(2)    ! bounds
            integer(kind = int64), intent(in), optional :: s(:)    ! isubs
            integer(kind = int32), intent(in), optional :: v(:)    ! values
            logical(kind = int32), intent(in), optional :: f       ! flip
            integer(kind = int64) :: lb, ub, numel
            character(len=9), intent(in),      optional :: m       ! mode


            call vector_int32_t_erase_argsCheck (i, b, s, v, m)


            if ( present(i) ) then
                print *, "erasing by index ... "
                call vector_int32_t_erase_byIndexShadow (vec, i, f)
            else if ( present(b) ) then
                print *, "erasing by range ... "
                call vector_int32_t_erase_byRangeShadow (vec, b, f)
            else if ( present(s) ) then
                print *, "erasing by subscript ... "
                call vector_int32_t_erase_subs_shadow (vec, s, f)
            else if ( present(v) ) then
                print *, "erasing by values ... "
                call vector_int32_t_erase_values (vec, v)
            else
                print *, "erasing all ... "
                call vector_int32_t_erase_all (vec)
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
! [x] PARTITION further into submodules. Idea: keep methods in a submodule
!     and implementations in another. You will need another for utility
!     procedures (for memory management).
! [x] IMPLEMENT method that returns an iterator to values in the asymmetric
!     range [begin, avail). Implement the iterator as a pointer.
! [x] IMPLEMENT a polymorphic push-back method (value is of type class(*)).
!     Use a select type construct to determine what version
!     (of "proxy" procedure) to call at runtime.
!     Note: Most methods delegates work to a "proxy" of sorts. The method
!     test_polymorphic_method() may be used as a template for other ones.
! [x] GUARD against using the vector to store integers of different types.
!     If the user inserts a 32-bit integer it won't be able to insert
!     a 64-bit integer unless the vector contents are cleared.
! [x] EXPERIMENT with dynamically allocated strings. Not fully supported
!     by GNU Fortran Compiler. Cannot use them without disabling some
!     compiler options I would like to retain.


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
! the element pointed to by (avail). It's a valid index but it should not
! be referenced since it does not hold an actual value.
!
!
! subroutine clear_method()
! Placing the avail iterator at the beginning is equivalent to
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
!
!
! subroutine vector_int32_t_erase_method(vec, i, b, s, v, m, f)
! NOTE:
! Not sure if I should really add support for deleting multiple distinct
! values.
