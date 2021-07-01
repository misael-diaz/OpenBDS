!
!   source: test_vector.for
!   author: misael-diaz
!   date:   2021-06-10
!
!
!   Synopsis:
!   Tests the vector class.
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

module vector_class_tests
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use VectorClass, only: vector_t
    use chronos, only: chronom
    implicit none
    integer(kind = int64), parameter :: max_vector_size = 1048576_int64
    private
    public :: test_vector_get
    public :: test_vector_push_back
    public :: test_vector_copy
    public :: test_vector_find
    public :: test_vector_clear
    public :: test_vector_iterator
    public :: test_vector_array_vector_t
    public :: test_vector_up_array_vector_t
    public :: test_vector_mold_vector_t
    contains


        subroutine test_vector_push_back ()
            ! Synopsis: Tests pushing values unto back of vector.
            type(vector_t), pointer :: vector => null()
            type(chronom) :: stopwatch
            integer(kind = int64):: i
            integer(kind = int64), parameter :: n = 65536_int64
            integer(kind = int32):: mstat

            allocate (vector, stat = mstat)
            if (mstat /= 0) then
                error stop "test::vector.push_back: allocation error"
            end if


            vector = vector_t ()
            stopwatch = chronom ()
    

            print *, new_line('n')//new_line('n')
            write (*, '(1X,A)', advance='no') &
                & "pushing values unto back of vector ... "

            i = 0_int64
            call stopwatch % tic ()
            do while (i /= n)
                call vector % push_back (i)
                i = i + 1_int64
            end do
            call stopwatch % toc ()


            print *, "done"
            print *, new_line('n')//new_line('n')
            print *, "size: ", vector % size()
            write (*, '(1X,A)', advance='no') "push-back test:"
            if ( n == vector % size() ) then
                print *, "pass"
            else
                print *, "FAIL"
            end if


            print *, "elapsed-time (millis): ", stopwatch % etime ()
            print *, new_line('n')//new_line('n')



            deallocate (vector, stat = mstat)
            if (mstat /= 0) then
                error stop "test::vector.push_back: deallocation error"
            end if

            return
        end subroutine


        subroutine test_vector_array_vector_t ()
            ! Synopsis: Tests array of vectors.
            type(vector_t), allocatable :: vector(:)
            integer(kind = int64), pointer, contiguous :: it(:) => null()
            integer(kind = int64), parameter :: n = max_vector_size
            integer(kind = int64):: r, t(2), address(2)
            integer(kind = int32):: i
            integer(kind = int32):: mstat

            allocate (vector(2), stat = mstat)
            if (mstat /= 0) then
                error stop "test::vector.array: allocation error"
            end if


            vector = create () ! instantiates array of vectors


            ! size test (vectors must have the same number of elements)
            write (*, '(1X,A)', advance='no') "array of vectors test 1:"
            if (n == vector(1) % size() .and. n == vector(2) % size()) then
                print *, "pass"
            else
                print *, "FAIL"
            end if


            write (*, '(1X,A)', advance='no') "array of vectors test 2:"

            call vector(1) % iter(it)
            t(1) = sum(it)
            address(1) = loc(it)
            call vector(2) % iter(it)
            t(2) = sum(it)
            address(2) = loc(it)


            r = (n - 1_int64) * n / 2_int64
            ! running total test
            if ( r == t(1) .and. r == t(2) ) then
                print *, "pass"
            else
                print *, "FAIL"
            end if


            ! checks that (indeed) the iterator points to different objects
            write (*, '(1X,A)', advance='no') "array of vectors test 3:"
            if ( address(1) /= address(2) ) then
                print *, "pass"
            else
                print *, "FAIL"
            end if


            deallocate (vector, stat = mstat)
            if (mstat /= 0) then
                error stop "test::vector.copy: deallocation error"
            end if

            return
        end subroutine


        subroutine test_vector_up_array_vector_t ()
            ! Synopsis: Tests u[nlimited] p[olymorphic] array of vectors.
            type(vector_t), allocatable :: vector(:)
            integer(kind = int32):: i
            integer(kind = int32):: mstat

            allocate (vector(2), stat = mstat)
            if (mstat /= 0) then
                error stop "test::vector.array: allocation error"
            end if


            vector = vector_t ()
            call vector(1) % push_back(0_int32)
            call vector(2) % push_back(0_int64)


            print *, "unlimited polymorphic test: pass" ! compiles/executes


            deallocate (vector, stat = mstat)
            if (mstat /= 0) then
                error stop "test::vector.copy: deallocation error"
            end if

            return
        end subroutine


        subroutine test_vector_copy ()
            ! Synopsis: Tests the assignment operator of the vector class.
            type(vector_t), allocatable :: vector
            type(chronom) :: stopwatch
            integer(kind = int64), parameter :: n = max_vector_size
            integer(kind = int32):: mstat

            allocate (vector, stat = mstat)
            if (mstat /= 0) then
                error stop "test::vector.copy: allocation error"
            end if


            stopwatch = chronom ()


            print *, new_line('n')//new_line('n')
            write (*, '(1X,A)', advance='no') "copying vector ... "


            call stopwatch % tic ()
            vector = create ()
            call stopwatch % toc ()


            write (*, '(1X,A)') "done"
            print *, new_line('n')//new_line('n')

            write (*, '(1X,A)', advance='no') "copy test: "

            if ( n == vector % size() ) then
                print *, "pass"
            else
                print *, "FAIL"
            end if

            print *, "elapsed-time (millis): ", stopwatch % etime ()
            print *, new_line('n')//new_line('n')


            deallocate (vector, stat = mstat)
            if (mstat /= 0) then
                error stop "test::vector.copy: deallocation error"
            end if

            return
        end subroutine


        subroutine test_vector_clear ()
            type(vector_t), allocatable :: vector
            integer(kind = int64), pointer, contiguous :: it(:) => null()
            integer(kind = int32):: mstat

            allocate (vector, stat = mstat)
            if (mstat /= 0) then
                error stop "test::vector.clear: allocation error"
            end if


            vector = create ()
            call vector % clear ()
            call vector % iter  (it)


            write (*, '(1X,A)', advance='no') "[0] test::vector.clear(): "
            if ( vector % size() /= 0 ) then
                print *, "FAIL"
            else
                print *, "pass"
            end if


            write (*, '(1X,A)', advance='no') "[1] test::vector.clear(): "
            if ( associated(it) ) then
                print *, "FAIL"
            else
                print *, "pass"
            end if


            deallocate (vector, stat = mstat)
            if (mstat /= 0) then
                error stop "test::vector.clear: deallocation error"
            end if

            return
        end subroutine


        subroutine test_vector_get ()
            type(vector_t), allocatable :: vector
            integer(kind = int64):: i, t, value
            integer(kind = int64), parameter :: n = max_vector_size
            integer(kind = int32):: mstat

            allocate (vector, stat = mstat)
            if (mstat /= 0) then
                error stop "test::vector.get: allocation error"
            end if


            vector = create ()


            t = 0_int64
            i = 0_int64
            do while (i /= n)
                call vector % get(i, value)
                t = t + (i - value)
                i = i + 1_int64
            end do


            ! element equality test
            write (*, '(1X,A)', advance='no') "test::vector.get(): "
            if (t /= 0_int64) then
                print *, "FAIL"
            else
                print *, "pass"
            end if


            deallocate (vector, stat = mstat)
            if (mstat /= 0) then
                error stop "test::vector.get: deallocation error"
            end if

            return
        end subroutine


        subroutine test_vector_find ()
            type(vector_t), allocatable :: vector
            integer(kind = int64):: idx
            integer(kind = int64):: value
            integer(kind = int64):: found
            integer(kind = int32):: mstat

            allocate (vector, stat = mstat)
            if (mstat /= 0) then
                error stop "test::vector.clear: allocation error"
            end if


            vector = create ()


            value = 0_int64
            idx = vector % find (value)
            call vector % get(idx, found)


            write (*, '(1X,A)', advance='no') "test::vector.find(): "
            if ( found /= value ) then
                print *, "FAIL"
            else
                print *, "pass"
            end if


            deallocate (vector, stat = mstat)
            if (mstat /= 0) then
                error stop "test::vector.clear: deallocation error"
            end if

            return
        end subroutine


        subroutine test_vector_iterator ()
            ! Synopsis: Tests the iterator of the vector class.
            type(vector_t), allocatable :: vector
            integer(kind = int64), pointer, contiguous :: it(:) => null()
            integer(kind = int64):: n, t
            integer(kind = int32):: mstat

            allocate (vector, stat = mstat)
            if (mstat /= 0) then
                error stop "test::vector.iterator: allocation error"
            end if


            vector = create ()
            call vector % iter (it)
            n = vector % size ()
            t = (n - 1_int64) * n / 2_int64


            print *, new_line('n')//new_line('n')


            write (*, '(1X,A)', advance='no') "test::vector.iter(): "

            if ( t == sum(it) ) then
                print *, "pass"
            else
                print *, "FAIL"
            end if

            print *, new_line('n')//new_line('n')


            deallocate (vector, stat = mstat)
            if (mstat /= 0) then
                error stop "test::vector.iterator: deallocation error"
            end if

            return
        end subroutine


        function create () result(vector)
            ! Synopsis: Returns vector<int64_t>.
            type(vector_t), allocatable :: vector
            integer(kind = int64):: i
            integer(kind = int64), parameter :: n = max_vector_size
            integer(kind = int32):: mstat

            allocate (vector, stat = mstat)
            if (mstat /= 0) then
                error stop "test::vector.create: allocation error"
            end if

            i = 0_int64
            do while (i /= n)
                call vector % push_back (i)
                i = i + 1_int64
            end do

            return
        end function


        subroutine test_vector_mold_vector_t ()
            ! Synopsis: Tests molding class(*) into vector.
            type(vector_t), allocatable :: vector
            class(*), allocatable :: array(:)
            integer(kind = int64), pointer, contiguous :: it(:) => null()
            integer(kind = int64), parameter :: n = max_vector_size
            integer(kind = int64):: r, t(2)
            integer(kind = int32):: mstat

            allocate (vector, stat = mstat)
            if (mstat /= 0) then
                error stop "test::vector.cast: allocation error"
            end if


            vector = create ()


            allocate (array(2), mold = vector, stat = mstat)
            if (mstat /= 0) then
                error stop "test::vector.cast: allocation error"
            end if


            write (*, '(1X,A)', advance='no') "casting test:"
            ! copy vector into array of vectors
            select type (array)
                type is (vector_t)


                    array = vector
                    call array(1) % iter (it)
                    t(1) = sum(it)
                    call array(2) % iter (it)
                    t(2) = sum(it)

                    r = (n - 1_int64) * n / 2_int64
                    ! running total test
                    if ( r == t(1) .and. r == t(2) ) then
                        print *, "pass"
                    else
                        print *, "FAIL"
                    end if


            end select


            deallocate(vector, array, stat = mstat)
            if (mstat /= 0) then
                error stop "test::vector.cast: deallocation error"
            end if


            return
        end subroutine


end module


program test_vector_class
    use vector_class_tests, only: get => test_vector_get
    use vector_class_tests, only: push_back => test_vector_push_back
    use vector_class_tests, only: copy => test_vector_copy
    use vector_class_tests, only: find => test_vector_find
    use vector_class_tests, only: iter => test_vector_iterator
    use vector_class_tests, only: cast => test_vector_mold_vector_t
    use vector_class_tests, only: clear => test_vector_clear
    use vector_class_tests, only: array_vector_t => &
                                      & test_vector_array_vector_t
    use vector_class_tests, only: up_array_vector_t => &
                                      & test_vector_up_array_vector_t
    implicit none


    call push_back ()
    call copy ()
    call get ()
    call iter ()
    call cast ()
    call find ()
    call clear ()
    call array_vector_t ()
    call up_array_vector_t ()


end program


!!  Previous Tests (some are to be conducted again):
!!  print *, vector % find (0)       ! complains that vector is empty
!!  Tests: out-of-bounds checks
!!  print *, vector < -1_int64 !>
!!  print *, vector < int(huge(0), kind = int64) !>
