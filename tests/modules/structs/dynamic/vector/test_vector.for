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
    public :: test_vector_erase
    public :: test_vector_iterator
    public :: test_vector_public_iterator
    public :: test_vector_array_vector_t
    public :: test_vector_up_array_vector_t
    public :: test_vector_mold_vector_t
    contains


        subroutine test_vector_push_back ()
            ! Synopsis: Tests pushing values unto back of vector.
            type(vector_t), allocatable :: vector !<int64_t> !
            type(vector_t), allocatable :: vector2!<vector_t>!
            type(vector_t), allocatable :: veccopy!<vector_t>!
            type(vector_t), allocatable :: vector3!<vector_t>!
            type(vector_t), allocatable :: vc3copy!<vector_t>!
            type(chronom) :: stopwatch
            integer(kind = int64):: i
            integer(kind = int64), parameter :: n = 65536_int64
            integer(kind = int32):: mstat

            allocate (vector, vector2, vector3, veccopy, vc3copy, stat = mstat)
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
            write (*, '(1X,A)', advance='no') "push-back test 1:"
            if ( n == vector % size() ) then
                print *, "pass"
            else
                print *, "FAIL"
            end if


            print *, "elapsed-time (millis): ", stopwatch % etime ()


            vector2 = vector_t ()


            i = 0_int64
            do while (i /= 64_int64)
                call vector2 % push_back (vector)
                i = i + 1_int64
            end do


            veccopy = vector2   ! copies vector of vectors vector<vector_t>


            write (*, '(1X,A)', advance='no') "push-back test 2:"
            if ( veccopy % size() == vector2 % size() ) then
                print *, "pass"
            else
                print *, "FAIL"
            end if


            ! creates a vector of (vector of vectors)
            vector3 = vector_t ()
            call vector3 % push_back (veccopy)
            vc3copy = vector3


            write (*, '(1X,A)', advance='no') "push-back test 3:"
            if ( vc3copy % size() == vector3 % size() ) then
                print *, "pass"
            else
                print *, "FAIL"
            end if


            print *, new_line('n')//new_line('n')


            deallocate (vector, vector2, vector3, veccopy, stat = mstat)
            if (mstat /= 0) then
                error stop "test::vector.push_back: deallocation error"
            end if

            return
        end subroutine


        subroutine test_vector_array_vector_t ()
            ! Synopsis: Tests array of vectors.
            type(vector_t), allocatable :: vector(:)
            class(*), pointer, contiguous :: it(:) => null()
            integer(kind = int64), parameter :: n = max_vector_size
            integer(kind = int64):: i, r, t(2), address(2)
            integer(kind = int32):: mstat

            allocate (vector(2), stat = mstat)
            if (mstat /= 0) then
                error stop "test::vector.array: allocation error"
            end if

!! BUG      vector[(:)] = create () ! Iterators point to the same object !
            do i = 1_int64, 2_int64
                ! instantiates array of vectors the right way
                vector(i) = create ()
            end do


            ! size test (vectors must have the same number of elements)
            write (*, '(1X,A)', advance='no') "array of vectors test 1:"
            if (n == vector(1) % size() .and. n == vector(2) % size()) then
                print *, "pass"
            else
                print *, "FAIL"
            end if


            write (*, '(1X,A)', advance='no') "array of vectors test 2:"

            it => vector(1) % deref % it
            select type (it)
                type is ( integer(kind = int64) )
                    t(1) = sum(it)
                class default
                    error stop "v.iter: unexpected error"
            end select
            address(1) = loc(it)


            it => vector(2) % deref % it
            select type (it)
                type is ( integer(kind = int64) )
                    t(2) = sum(it)
                class default
                    error stop "v.iter: unexpected error"
            end select            
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
            type(vector_t), allocatable :: v(:)
            integer(kind = int32):: mstat

            allocate (v(2), stat = mstat)
            if (mstat /= 0) then
                error stop "test::vector.array: allocation error"
            end if


            v = vector_t ()
            call v(1) % push_back(0_int32)
            call v(2) % push_back(0_int64)

            if ( loc(v(1) % deref % it) == loc(v(2) % deref % it) ) then
                print *, "FAIL"
            else
                print *, "pass"
            end if

            print *, "unlimited polymorphic test: pass" ! compiles/executes


            deallocate (v, stat = mstat)
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
            class(*), pointer, contiguous :: it(:) => null()
            integer(kind = int32):: mstat

            allocate (vector, stat = mstat)
            if (mstat /= 0) then
                error stop "test::vector.clear: allocation error"
            end if


            vector = create ()
            call vector % clear ()
            it => vector % deref % it

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


        subroutine test_vector_erase ()
            ! NOTE:
            ! Vector stores an index array so that searching
            ! by value or index yields the same result.
            ! TODO:
            ! It is assumed that the user knows that the range
            ! of the vector is [0, vector % size). For users
            ! who do not wish to work this way it might be
            ! in their interest to delete by iterator. Consider
            ! implementing this feature.
            type(vector_t), allocatable :: vector, vecopy
            class(*), pointer, contiguous :: it(:) => null()
            integer(kind = int32), parameter :: n = 8
            integer(kind = int64), allocatable :: vs(:)
            logical(kind = int32), allocatable :: mask(:)
            integer(kind = int64):: idx, bounds(0:1)
            integer(kind = int32):: i, j, value, delta, diff
            integer(kind = int32):: mstat


            allocate (vector, vecopy, vs(0:7), mask(0:n-1), stat = mstat)
            if (mstat /= 0) then
                error stop "test::vector.get: allocation error"
            end if


            i = 0
            vector = vector_t ()
            do while (i /= n)
                call vector % push_back (i)
                i = i + 1
            end do
            vecopy = vector


!           bounds checks
!           call vector % erase (i = -255_int64)                ! passed
!           call vector % erase (i =  255_int64)                ! passed
!           call vector % erase (i = int(n, kind = int64) )     ! passed


            ! checks arguments:
            idx = 0_int64

            do idx = 0_int64, 7_int64
                vs(idx) = idx
            end do

            bounds(0) = 0_int64
            bounds(1) = 1_int64


            ! [0] erase the first value
            value = 0
            idx = int(value, kind = int64)
            call vector % erase (i = idx)       ! erase by index
            idx = vector % find (value)


            it => vector % deref % it
            select type (it)
                type is ( integer(kind = int32) )
                    delta = 0
                    do i = 1, (n - 1)
                        delta = delta + (it(i) - i)
                    end do
                class default
                    error stop "test::vector.erase(): unexpected error"
            end select


            ! note: value shouldn't exist after the erase
            write (*, '(1X,A)', advance='no') "[0] test::vector.erase(): "
            if ( idx == int(value, kind = int64) .or. idx /= -1_int64 ) then
                print *, "FAIL"
            else if ( n == vector % size () ) then
                print *, "FAIL"
            else if ( delta /= 0 ) then
                print *, "FAIL"
            else if ( size (it, kind = int64) /= vector % size () ) then
                print *, "FAIL"
            else if ( size (it) /= (n - 1) ) then
                print *, "FAIL"
            else
                print *, "pass"
            end if


            ! [1] erase the last value (of original vector)
            vector = vecopy
            value = 7
            idx = int(value, kind = int64)
            call vector % erase (i = idx)
            idx = vector % find (value)


            it => vector % deref % it
            select type (it)
                type is ( integer(kind = int32) )
                    delta = 0
                    do i = 0, (n - 2)
                        delta = delta + (it(i + 1) - i)
                    end do
                class default
                    error stop "test::vector.erase(): unexpected error"
            end select


            write (*, '(1X,A)', advance='no') "[1] test::vector.erase(): "
            if ( idx /= -1_int64 ) then
                print *, "FAIL"
            else if ( n == vector % size () ) then
                print *, "FAIL"
            else if ( delta /= 0 ) then
                print *, "FAIL"
            else if ( size (it, kind = int64) /= vector % size () ) then
                print *, "FAIL"
            else if ( size (it) /= (n - 1) ) then
                print *, "FAIL"
            else
                print *, "pass"
            end if


            ! erase intermediate value
            vector = vecopy
            value = 4
            idx = int(value, kind = int64)
            mask = .true.
            mask(idx) = .false.
            call vector % erase (i = idx)
            idx = vector % find (value)



            it => vector % deref % it
            select type (it)
                type is ( integer(kind = int32) )
                    j = 1
                    delta = 0
                    do i = 0, (n - 1)
                        if ( mask(i) ) then
                            delta = delta + (it(j) - i)
                            j = j + 1
                        end if
                    end do
                class default
                    error stop "test::vector.erase(): unexpected error"
            end select


            write (*, '(1X,A)', advance='no') "[2] test::vector.erase(): "
            if ( idx /= -1_int64 ) then
                print *, "FAIL"
            else if ( n == vector % size () ) then
                print *, "FAIL"
            else if ( delta /= 0 ) then
                print *, "FAIL"
            else if ( size (it, kind = int64) /= vector % size () ) then
                print *, "FAIL"
            else if ( size (it) /= (n - 1) ) then
                print *, "FAIL"
            else
                print *, "pass"
            end if


!           bounds check
!           bounds(0) = -255_int64              ! passed
!           bounds(1) =  255_int64              ! passed
!           bounds(1) = int(n, kind = int64)    ! passed



            ! erases by range (all values)
            vector = vecopy
            bounds(0) = 0_int64
            bounds(1) = int(n - 1, kind = int64)
            call vector % erase (b=bounds)


            write (*, '(1X,A)', advance='no') "[3] test::vector.erase(): "
            if (vector % size() /= 0_int64) then
                print *, "FAIL"
            else if ( associated(vector % deref % it) ) then
                print *, "FAIL"
            else
                print *, "pass"
            end if


            ! erases values in lb:ub, where ub is last but lb is not first
            vector = vecopy
            bounds(0) = 1_int64
            bounds(1) = int(n - 1, kind = int64)
            call vector % erase (b=bounds)

            value = 0
            idx = vector % find (value)
            it => vector % deref % it

            write (*, '(1X,A)', advance='no') "[4] test::vector.erase(): "
            if (vector % size() /= 1_int64) then
                print *, "FAIL"
            else if ( idx /= int(value, kind = int64) ) then
                print *, "FAIL"
            else if ( size (it, kind = int64) /= vector % size () ) then
                print *, "FAIL"
            else
                print *, "pass"
            end if



            ! erases by range (empty)
            vector = vecopy
            bounds(0) = int(n - 1, kind = int64)
            bounds(1) = 0_int64
            call vector % erase (b=bounds)
            it => vector % deref % it


            write (*, '(1X,A)', advance='no') "[5] test::vector.erase(): "
            if (vector % size() /= n) then
                print *, "FAIL"
            else if ( .not. associated(vector % deref % it) ) then
                print *, "FAIL"
            else if ( size (it, kind = int64) /= vector % size () ) then
                print *, "FAIL"
            else
                print *, "pass"
            end if


            ! erases by range
            vector = vecopy
            bounds(0) = 2_int64
            bounds(1) = int(n - 3, kind = int64)
            call vector % erase (b=bounds)
            it => vector % deref % it


            ! checks for the remaining values
            select type (it)
                type is ( integer(kind = int32) )
                    
                    diff = 0
                    value = 0
                    diff = diff + ( it(1) - value )
                    value = 1
                    diff = diff + ( it(2) - value )

                    value = n - 2
                    diff = diff + ( it(3) - value )
                    value = n - 1
                    diff = diff + ( it(4) - value )

                class default
                    error stop "unexpected error"
            end select


            write (*, '(1X,A)', advance='no') "[6] test::vector.erase(): "
            if ( vector % size() /= n - (bounds(1) - bounds(0) + 1) ) then
                print *, "FAIL"
            else if ( size (it, kind = int64) /= vector % size () ) then
                print *, "FAIL"
            else if ( diff /= 0 ) then
                print *, "FAIL"
            else
                print *, "pass"
            end if



            vector = vecopy
            do idx = 0_int64, 7_int64
                vs(idx) = idx
            end do
            call vector % erase (s=vs)          ! erase by s[ubscript]
            it => vector % deref % it


            write (*, '(1X,A)', advance='no') "[7] test::vector.erase(): "
            if ( vector % size() /= 0_int64 ) then
                print *, "FAIL"
            else if ( associated(it) ) then
                print *, "FAIL"
            else
                print *, "pass"
            end if


            call vector % erase ()              ! erases all values


            write (*, '(1X,A)', advance='no') "[] test::vector.erase(): "
            if (vector % size() /= 0_int64) then
                print *, "FAIL"
            else if ( associated(vector % deref % it) ) then
                print *, "FAIL"
            else
                print *, "pass"
            end if


            write (*, '(1X,A)', advance='no') "[] test::vector.erase(): "
            if ( associated(vector % deref % it) ) then
                print *, "FAIL"
            else
                print *, "pass"
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
            type(vector_t), allocatable :: vector2
            class(*), pointer, contiguous :: it(:) => null()
            integer(kind = int64):: n, t
            integer(kind = int32):: mstat

            allocate (vector, vector2, stat = mstat)
            if (mstat /= 0) then
                error stop "test::vector.iterator: allocation error"
            end if


            vector = create ()
            it => vector % deref % it
            n = vector % size ()
            t = (n - 1_int64) * n / 2_int64


            print *, new_line('n')//new_line('n')


            write (*, '(1X,A)', advance='no') "[0] test::vector.iter(): "

            select type (it)
                type is ( integer(kind = int64) )

                    if ( t == sum(it) ) then
                        print *, "pass"
                    else
                        print *, "FAIL"
                    end if

                class default
                    error stop "v.iter: unexepcted error"
            end select


            vector2 = vector_t ()
            call vector2 % push_back (vector)
            it => vector2 % deref % it


            write (*, '(1X,A)', advance='no') "[1] test::vector.iter(): "

            select type (it)
                type is (vector_t)

                    if ( it(1) % size() /= vector % size() ) then
                        print *, "FAIL"
                    else
                        print *, "pass"
                    end if

                class default
                    error stop "v.iter: unexpected error"
            end select


            print *, new_line('n')//new_line('n')

            deallocate (vector, vector2, stat = mstat)
            if (mstat /= 0) then
                error stop "test::vector.iterator: deallocation error"
            end if

            return
        end subroutine


        subroutine test_vector_public_iterator ()
            type(vector_t), allocatable, target :: vector
            type(vector_t), allocatable :: vecopy
            class(*), pointer, contiguous :: iter(:)
            integer(kind = int32), allocatable :: values(:)
            integer(kind = int32):: i, v(1), value
            integer(kind = int32):: mstat

            allocate (vector, vecopy, values(256), stat = mstat)
            if (mstat /= 0) then
                error stop "test::vector.iterator: allocation error"
            end if


            vector = vector_t ()

            do i = 1, 256
                value = i
                values(i) = value
                call vector % push_back (value)
            end do


            ! select-type construct needed to dereference iterator
            associate (it => vector % deref % it)
                select type (it)
                    type is ( integer(kind = int32) )
                        values(:) = it - values
                end select
            end associate


            write (*, '(1X,A)', advance='no') "[0] test::v.iter: "
            if ( sum(values) /= 0 ) then
                print *, "FAIL"
            else
                print *, "pass"
            end if


            iter => vector % deref % it
            ! inquiry functions do *not* need the select-type construct
            write (*, '(1X,A)', advance='no') "[1] test::v.iter: "
            if ( size(iter, kind = int64) /= vector % size() ) then
                print *, "FAIL"
            else
                print *, "pass"
            end if


            call vector % clear ()
            write (*, '(1X,A)', advance='no') "[2] test::v.iter: "
            if ( associated(vector % deref % it) ) then
                print *, "FAIL"
            else
                print *, "pass"
            end if


            v = 256
            call vector % push_back( v(1) )


            associate (it => vector % deref % it)
                select type (it)
                    type is ( integer(kind = int32) )
                        v = it - v
                end select
            end associate


            write (*, '(1X,A)', advance='no') "[3] test::v.iter: "
            if ( v(1) /= 0 ) then
                print *, "FAIL"
            else
                print *, "pass"
            end if


            iter => vector % deref % it
            write (*, '(1X,A)', advance='no') "[4] test::v.iter: "
            if ( size(iter, kind = int64) /= vector % size() ) then
                print *, "FAIL"
            else
                print *, "pass"
            end if


            vecopy = vector


            iter => vecopy % deref % it
            write (*, '(1X,A)', advance='no') "[5] test::v.iter: "
            if ( size(iter, kind = int64) /= vecopy % size() ) then
                print *, "FAIL"
            else
                print *, "pass"
            end if


            write (*, '(1X,A)', advance='no') "[6] test::v.iter: "
            if (loc(vecopy % deref % it) == loc(vector % deref % it) ) then
                print *, "FAIL"
            else
                print *, "pass"
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
            class(*), pointer, contiguous :: it(:) => null()
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
                    it => array(1) % deref % it
                    select type (it)
                        type is ( integer(kind = int64) )
                            t(1) = sum(it)
                        class default
                            error stop "cast test: unexpected error"
                    end select


                    it => array(2) % deref % it
                    select type (it)
                        type is ( integer(kind = int64) )
                            t(2) = sum(it)
                        class default
                            error stop "cast test: unexpected error"
                    end select


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
    use vector_class_tests, only: it   => test_vector_public_iterator
    use vector_class_tests, only: cast => test_vector_mold_vector_t
    use vector_class_tests, only: clear => test_vector_clear
    use vector_class_tests, only: erase => test_vector_erase
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
    call it ()

    call erase ()

end program


!!  Previous Tests (some are to be conducted again):
!!  print *, vector % find (0)       ! complains that vector is empty
!!  Tests: out-of-bounds checks
!!  print *, vector < -1_int64 !>
!!  print *, vector < int(huge(0), kind = int64) !>
