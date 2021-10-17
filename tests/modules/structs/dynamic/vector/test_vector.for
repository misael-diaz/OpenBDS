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
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64
    use VectorClass, only: vector_t
    use chronos, only: chronom
    implicit none
    integer(kind = int64), parameter :: max_vector_size = 1048576_int64
    private
    public :: test_vector_fill_constructor
    public :: test_vector_get
    public :: test_vector_push_back
    public :: test_vector_copy
    public :: test_vector_find
    public :: test_vector_clear
    public :: test_vector_iterator
    public :: test_vector_public_iterator
    public :: test_vector_array_vector_t
    public :: test_vector_up_array_vector_t
    public :: test_vector_mold_vector_t
    contains

        subroutine test_vector_fill_constructor ()
            type(vector_t), allocatable :: v_i32   !! vector <int32_t>
            type(vector_t), allocatable :: v_i64   !! vector <int64_t>
            type(vector_t), allocatable :: v_r64   !! vector <real64_t>
            type(vector_t), allocatable :: vofvec  !! vector of vectors
            type(vector_t), allocatable :: avofvec !! another vec of vecs
            class(*), pointer, contiguous :: it(:) => null()
            class(*), pointer, contiguous :: iter(:) => null()
            integer(kind = int64), parameter :: numel = 64_int64
            integer(kind = int64):: i, j, k, diff(3), diffs(2)
            integer(kind = int32), parameter :: value = 64
            integer(kind = int32):: mstat


            allocate (v_i32, v_i64, v_r64, vofvec, avofvec, stat=mstat)
            if (mstat /= 0) error stop 'test.vector(): allocation error'

            ! constructs vectors having `numel' copies of `value'
            v_i32 = vector_t (numel, value)
            v_i64 = vector_t (numel, int (value, kind = int64) )
            v_r64 = vector_t (numel, real(value, kind = real64) )


            ! checks the stored values for consistency
            it => v_i32 % deref % it
            select type (it)
                type is ( integer(kind = int32) )
                    diff(1) = int(sum(it), kind = int64) - numel * value
                class default
                    error stop 'test.vector(): unexpected error'
            end select


            it => v_i64 % deref % it
            select type (it)
                type is ( integer(kind = int64) )
                    diff(2) = sum(it) - numel * value
                class default
                    error stop 'test.vector(): unexpected error'
            end select


            it => v_r64 % deref % it
            select type (it)
                type is ( real(kind = real64) )
                    diff(3) = nint( sum(it), kind=int64 ) - numel * value
                class default
                    error stop 'test.vector(): unexpected error'
            end select


            write (*, '(A)', advance='no') '[00] test-vector.construct(): '
            if (v_i32 % size() /= numel .or. v_i64 % size() /= numel) then
                print *, 'FAIL'
            else if (v_r64 % size() /= numel) then
                print *, 'FAIL'
            else if (diff(1) /= 0_int64 .or. diff(2) /= 0_int64) then
                print *, 'FAIL'
            else if (diff(3) /= 0_int64) then
                print *, 'FAIL'
            else
                print *, 'pass'
            end if


            ! creates vectors of vectors
            vofvec  = vector_t (numel, v_i32)   !! vec < vec<T> >
            avofvec = vector_t (numel, vofvec)  !! vec < vec < vec<T> > >

            ! checks that the iterators point to distintc `internal' arrays
            diffs(:) = 0_int64
            iter => vofvec % deref % it
            select type (iter)
                type is (vector_t)
                    do i = 1_int64, (numel - 1_int64)
                        do j = i + 1_int64, numel

                            associate (it_1 => iter(i) % deref % it, &
                                     & it_2 => iter(j) % deref % it)

                                if ( loc(it_1) == loc(it_2) ) then
                                    diffs(1) = diffs(1) + 1_int64
                                end if

                            end associate

                        end do
                    end do
                class default
                    error stop 'test.vector(): unexpected error'
            end select


            ! same as above but for a `vector < vector < vector<T> > >'
            diffs(2) = 0_int64
            iter => avofvec % deref % it
            select type (iter)
                type is (vector_t)
                    do k = 1_int64, numel
                        associate (it => iter(k) % deref % it)
                            do i = 1_int64, (numel - 1_int64)
                                do j = i + 1_int64, numel

                                    if (loc( it(i) ) == loc( it(j) )) then
                                        diffs(2) = diffs(2) + 1_int64
                                    end if

                                end do
                            end do
                        end associate
                    end do
                class default
                    error stop 'test.vector(): unexpected error'
            end select


            write (*, '(A)', advance='no') '[01] test-vector.construct(): '
            if ( vofvec % size () /= numel ) then
                print *, 'FAIL'
            else if (diffs(1) /= 0_int64 .or. diffs(2) /= 0_int64) then
                print *, 'FAIL'
            else
                print *, 'pass'
            end if


            deallocate (v_i32, v_i64, v_r64, vofvec, avofvec)

            return
        end subroutine


        subroutine test_vector_push_back ()
            ! Synopsis: Tests pushing values unto back of vector.
            type(vector_t), allocatable :: vector !<int64_t> !
            type(vector_t), allocatable :: vecR64 !<real64_t>!
            type(vector_t), allocatable :: vector2!<vector_t>!
            type(vector_t), allocatable :: veccopy!<vector_t>!
            type(vector_t), allocatable :: vector3!<vector_t>!
            type(vector_t), allocatable :: vc3copy!<vector_t>!
            type(chronom) :: stopwatch
            integer(kind = int64):: i
            integer(kind = int64), parameter :: n = 65536_int64
            integer(kind = int32):: mstat

            allocate (vector, vector2, vector3, veccopy, vc3copy,&
                    & vecR64, stat = mstat)
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


            vecR64 = vector_t ()

            i = 0_int64
            call stopwatch % tic ()
            do while (i /= n)
                call vecR64 % push_back ( real(i, kind = real64) )
                i = i + 1_int64
            end do
            call stopwatch % toc ()


            print *, "done"
            print *, new_line('n')//new_line('n')
            print *, "size: ", vecR64 % size()
            write (*, '(1X,A)', advance='no') "push-back test real64:"
            if ( n == vecR64 % size() ) then
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


            deallocate (vector, vector2, vector3, veccopy,&
                      & vecR64, stat = mstat)
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

!! BUG      vector[(:)] = create ()     !! iterators point to invalid obj
            do i = 1_int64, 2_int64
                ! instantiates array of vectors the right way
                vector(i) = create ()   !! invokes ``assignment'' method
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
            type(vector_t), allocatable, target :: vecR64
            type(vector_t), allocatable :: vecopy
            class(*), pointer, contiguous :: iter(:)
            integer(kind = int32), allocatable :: values(:)
            integer(kind = int32):: i, v(1), value
            integer(kind = int32):: mstat

            allocate (vector, vecopy, vecR64, values(256), stat = mstat)
            if (mstat /= 0) then
                error stop "test::vector.iterator: allocation error"
            end if


            vector = vector_t ()
            vecr64 = vector_t ()

            do i = 1, 256
                value = i
                values(i) = value
                call vector % push_back (value)
                call vecR64 % push_back ( real(value, kind=real64) )
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


            ! uses iterator to process values contained in vector<real64_t>
            associate (it => vecR64 % deref % it)
                select type (it)
                    type is ( real(kind = real64) )
                        print *, 'total: ', sum  (it)
                        print *, 'min:   ', minval (it)
                        print *, 'max:   ', maxval (it)
                end select
            end associate


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
    use vector_class_tests, only: construct => test_vector_fill_constructor
    use vector_class_tests, only: get => test_vector_get
    use vector_class_tests, only: push_back => test_vector_push_back
    use vector_class_tests, only: copy => test_vector_copy
    use vector_class_tests, only: find => test_vector_find
    use vector_class_tests, only: iter => test_vector_iterator
    use vector_class_tests, only: it   => test_vector_public_iterator
    use vector_class_tests, only: cast => test_vector_mold_vector_t
    use vector_class_tests, only: clear => test_vector_clear
    use vector_class_tests, only: array_vector_t => &
                                      & test_vector_array_vector_t
    use vector_class_tests, only: up_array_vector_t => &
                                      & test_vector_up_array_vector_t
    implicit none


    call construct ()
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


end program


!!  Previous Tests (some are to be conducted again):
!!  print *, vector % find (0)       ! complains that vector is empty
!!  Tests: out-of-bounds checks
!!  print *, vector < -1_int64 !>
!!  print *, vector < int(huge(0), kind = int64) !>
