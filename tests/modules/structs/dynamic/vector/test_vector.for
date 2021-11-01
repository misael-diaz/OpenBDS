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
    use VectorClass, only: vector_t, arange_t, pointer_t
    use RandomAccessIteratorClass, only: iter_t
    use chronos, only: chronom
    implicit none
    integer(kind = int64), parameter :: max_vector_size = 1048576_int64
    private
    public :: test_vector_based_iterator
    public :: test_vector_range_constructor
    public :: test_vector_array_constructor
    public :: test_vector_fill_constructor
    public :: test_vector_get
    public :: test_vector_push_back
    public :: test_vector_push_back_array
    public :: test_vector_push_back_pointer
    public :: test_vector_copy
    public :: test_vector_find
    public :: test_vector_clear
    public :: test_vector_iterator
    public :: test_vector_public_iterator
    public :: test_vector_array_vector_t
    public :: test_vector_up_array_vector_t
    public :: test_vector_mold_vector_t

    interface check_diffs
        module procedure :: vector_pointer_t_check_diffs
    end interface

    contains

        subroutine test_vector_based_iterator ()
            type(iter_t) :: iter
            type(pointer_t) :: p_x, p_y, p_z
            type(pointer_t), target :: up_x, up_y, up_z
            class(*), pointer :: p => null()
            class(*), pointer, contiguous :: it(:) => null()
            integer(kind = int64), parameter :: x = 16_int64
            integer(kind = int64), parameter :: y = 32_int64
            integer(kind = int64), parameter :: z = 64_int64
            integer(kind = int64) :: idx, diffs, count
            integer(kind = int32) :: mstat
            character(*), parameter :: name = 'test-iterator:'
            character(*), parameter :: malloc_err = name // ' ' // &
                & 'memory (de)allocation error'
            character(*), parameter :: unexpected = name // ' ' // &
                & 'unexpected error'


            ! clones unlimited polymorphic object from source
            allocate (up_x % p, source=x, stat=mstat)
            if (mstat /= 0) error stop malloc_err

            allocate (up_y % p, source=y, stat=mstat)
            if (mstat /= 0) error stop malloc_err

            allocate (up_z % p, source=z, stat=mstat)
            if (mstat /= 0) error stop malloc_err


            ! pointer associations
            p_x = up_x
            p_y = up_y
            p_z = up_z


            iter = iter_t ()            !! instantiates iterator


            ! creates a dynamic array of pointers by insertions
            call iter % insert (p_x)
            call iter % insert (p_y)
            call iter % insert (p_z)


            diffs = 0_int64
            count = 0_int64
            it => iter % deref
            ! checks for memory address differences
            select type (it)

                type is (pointer_t)

                    do idx = 1_int64, size(array=it, dim=1, kind=int64)
                        select case (idx)

                            case (1_int64)

                                if ( loc(up_x % p) /= loc(it(idx)%p) ) then
                                    diffs = diffs + 1_int64
                                end if


                            case (2_int64)

                                if ( loc(up_y % p) /= loc(it(idx)%p) ) then
                                    diffs = diffs + 1_int64
                                end if


                            case (3_int64)

                                if ( loc(up_z % p) /= loc(it(idx)%p) ) then
                                    diffs = diffs + 1_int64
                                end if


                            case default
                                error stop unexpected

                        end select

                        p => it(idx) % p
                        ! accumulates values pointed to by the iterator
                        select type (p)
                            type is ( integer(kind = int64) )
                                count = count + p
                            class default
                                error stop unexpected
                        end select

                    end do

                class default
                    error stop unexpected

            end select


            write (*, '(A)', advance='no') &
                & '[00] test-vector-based-iterator(): '
            if ( diffs /= 0_int64 ) then
                print *, 'FAIL'
            else if ( count /= sum([x, y, z]) ) then
                print *, 'FAIL'
            else
                print *, 'pass'
            end if


            p_x % p => null()
            p_y % p => null()
            p_z % p => null()
            deallocate (up_x % p, up_y % p, up_z % p, stat=mstat)
            if (mstat /= 0) error stop malloc_err


            return
        end subroutine


        subroutine test_vector_range_constructor ()
            ! tests creating vectors from asymmetric ranges

            ! TESTS:
            ! [x] tests empty ranges.
            ! [x] tests decrementing sequences.
            ! [x] tests ranges that start at other values other than zero.
            ! [x] tests ranges that do not `stop' exactly at the `end'.
            !     (Note: When the difference between `begin' and `end'
            !      cannot be evenly divided by the `step'.)
            ! [x] tests the `assignment' of an `empty' vector having memory
            !     allocated for its internal array. (An empty range yields
            !     an `empty' vector with a preallocated internal array.)
            ! [x] tests that the iterators of a vector of empty vectors are
            !     not associated.
            ! [x] tests pushing values into an empty vector.
            ! [x] checks `aliasing' of iterators.
            ! [x] checks that non-allocatable vector of vectors is totally
            !     destroyed (no memory leaks)

            type(arange_t) :: arange
            type(vector_t), allocatable :: vector
            type(vector_t), allocatable :: veci64
            type(vector_t), allocatable :: reverse
            type(vector_t), allocatable :: avector
            type(vector_t), allocatable :: vec_one
            type(vector_t), allocatable :: vec_rng
            type(vector_t), allocatable :: vec_small
            type(vector_t), allocatable :: vec_empty
            type(vector_t), allocatable :: empty
            type(vector_t), allocatable :: vecinv
            type(vector_t), allocatable :: vofvec
            type(vector_t), allocatable :: avofvec
            type(vector_t) :: yavofvec
            class(*), pointer, contiguous :: iter(:) => null()
            class(*), pointer, contiguous :: it(:) => null()
            integer(kind = int64) :: count
            integer(kind = int64) :: diff(9)
            integer(kind = int32) :: i, j
            integer(kind = int32) :: mstat
            integer(kind = int32), parameter :: array(*) = [(i, i = 0, 63)]
            integer(kind = int32), parameter :: r(*) = [(i, i = 63, 1, -1)]
            integer(kind = int32), parameter :: small(*) = [(i, i = 0, 3)]
            integer(kind = int32), parameter :: rng(*) = [(i, i = 1, 9, 3)]
            integer(kind = int32), parameter :: one(*) = [(i, i = 0, 0)]
            integer(kind = int32), parameter :: aryinv(*) = -array(:)
            integer(kind = int32), parameter :: numel = &
                  & size(array = array, dim = 1, kind = int32)
            character(*), parameter :: errmsg = &
                & 'test-range-constructor: unexpected error'


            allocate (vector, vec_one, vec_rng, vec_small, vec_empty, &
                    & vecinv, avector, vofvec,  avofvec,   empty,     &
                    & reverse, veci64, stat=mstat)
            if (mstat /= 0) error stop 'test-range: allocation error'


            ! creates an asymmetric range
            arange = arange_t ( int(numel, kind = int64) )

            ! creates vectors
            vector    = vector_t (numel)   !! asymmetric range [0, numel)
!!          vector    = vector_t (b = 0,  e = numel, s = 1) !! equivalent
            veci64    = vector_t (arange)
            reverse   = vector_t ( arange_t(b = 63, e = 0, s = -1) )
            vec_one   = vector_t ( arange_t(1) )
            vec_rng   = vector_t ( arange_t(b = 1, e = 12, s = 3) )
            vec_small = vector_t ( arange_t(4) )
            vec_empty = vector_t ( arange_t(0) )
            empty     = vector_t ( arange_t(b=0, e=8, s=-1) )
            vecinv    = vector_t ( arange_t(b = 0, e = -numel, s = -1) )
            vofvec    = vector_t (int(numel, kind = int64), vec_empty)
            avector   = vec_empty

            call empty % push_back (array)
            avofvec = vector_t (int(numel, kind = int64), empty)
            call yavofvec % push_back (avofvec)


            diff(:) = 1_int64
            ! tests for differences between stored and pushed values
            associate (it => vector % deref % it)
                select type (it)
                    type is ( integer(kind = int32) )
                        diff(1) = int( sum(it - array), kind = int64)
                    class default
                        error stop errmsg
                end select
            end associate


            associate (it => vec_one % deref % it)
                select type (it)
                    type is ( integer(kind = int32) )
                        diff(2) = int( sum(it - one), kind = int64)
                    class default
                        error stop errmsg
                end select
            end associate


            associate (it => vec_rng % deref % it)
                select type (it)
                    type is ( integer(kind = int32) )
                        diff(3) = int( sum(it - rng), kind = int64)
                    class default
                        error stop errmsg
                end select
            end associate


            associate (it => vec_small % deref % it)
                select type (it)
                    type is ( integer(kind = int32) )
                        diff(4) = int( sum(it - small), kind = int64)
                    class default
                        error stop errmsg
                end select
            end associate


            associate (it => vecinv % deref % it)
                select type (it)
                    type is ( integer(kind = int32) )
                        diff(5) = int( sum(it - aryinv), kind = int64)
                    class default
                        error stop errmsg
                end select
            end associate


            associate (it => empty % deref % it)
                select type (it)
                    type is ( integer(kind = int32) )
                        diff(6) = int( sum(it - array), kind = int64)
                    class default
                        error stop errmsg
                end select
            end associate


            associate (it => reverse % deref % it)
                select type (it)
                    type is ( integer(kind = int32) )
                        diff(7) = int( sum(it - r), kind = int64)
                    class default
                        error stop errmsg
                end select
            end associate


            count = 0_int64
            ! checks the association of the iterators of the empty vectors
            associate (iter => vofvec % deref % it)
                select type (iter)
                    type is (vector_t)
                        do i = 1, numel
                            it => iter(i) % deref % it
                            if ( associated(it) ) then
                                count = count + 1_int64
                            end if
                        end do
                    class default
                        error stop errmsg
                end select
            end associate


            diff(8) = 0_int64
            ! checks aliasing of iterators of a vector of vectors
            iter => avofvec % deref % it
            select type (iter)
                type is (vector_t)
                    do i = 1, (numel - 1)
                        do j = i + 1, numel

                            associate (it_1 => iter(i) % deref % it, &
                                     & it_2 => iter(j) % deref % it)

                                if ( loc(it_1) == loc(it_2) ) then
                                    diff(8) = diff(8) + 1_int64
                                end if

                            end associate

                        end do
                    end do
                class default
                        error stop errmsg
            end select


            associate (it => veci64 % deref % it)
                select type (it)
                    type is ( integer(kind = int64) )
                        diff(9) = sum( it - int(array, kind = int64) )
                    class default
                        error stop errmsg
                end select
            end associate


            write (*, '(A)', advance='no') &
                & '[00] test-vector-range-constructor(): '
            if ( vector % size() /= int(numel, kind = int64) ) then
                print *, 'FAIL'
            else if ( empty % size() /= int(numel, kind = int64) ) then
                print *, 'FAIL'
            else if ( vec_empty % size() /= 0_int64 ) then
                print *, 'FAIL'
            else if ( avector % size() /= 0_int64 ) then
                print *, 'FAIL'
            else if ( associated( vec_empty % deref % it ) ) then
                print *, 'FAIL'
            else if ( associated( avector % deref % it ) ) then
                print *, 'FAIL'
            else if (diff(1) /= 0_int64 .or. diff(2) /= 0_int64) then
                print *, 'FAIL'
            else if (diff(3) /= 0_int64 .or. diff(4) /= 0_int64) then
                print *, 'FAIL'
            else if (diff(5) /= 0_int64 .or. diff(6) /= 0_int64) then
                print *, 'FAIL'
            else if (diff(7) /= 0_int64 .or. diff(8) /= 0_int64) then
                print *, 'FAIL'
            else if (diff(9) /= 0_int64) then
                print *, 'FAIL'
            else if (count /= 0_int64) then
                print *, 'FAIL'
            else
                print *, 'pass'
            end if


            deallocate (vector, vec_one, vec_rng, vec_small, vec_empty, &
                      & vecinv, avector, vofvec,  avofvec,   empty,     &
                      & reverse, veci64)

            return
        end subroutine test_vector_range_constructor


        subroutine test_vector_array_constructor ()
            ! tests creating vectors from arrays
            type(vector_t), allocatable :: vector     !! vector of ints
            type(vector_t), allocatable :: veci64     !! vector of longs
            type(vector_t), allocatable :: vecr64     !! vector of doubles
            type(vector_t), allocatable :: vofvec     !! vector of vectors
            type(vector_t), allocatable :: aryvec(:)  !! array of vectors
            integer(kind = int64) :: j
            integer(kind = int32) :: i, mstat
            integer(kind = int64) :: diff(4)
            integer(kind = int64), parameter :: b = 0_int64, e = 63_int64
            integer(kind = int64), parameter :: ary64(*) = [(j, j = b, e)]
            integer(kind = int32), parameter :: array(*) = [(i, i = 0, 63)]
            integer(kind = int32), parameter :: numel = &
                  & size(array = array, dim = 1, kind = int32)


            allocate (vector, veci64, vecr64, aryvec(numel), vofvec, &
                    & stat=mstat)
            if (mstat /= 0) error stop 'test-array-const: allocation error'


            ! creates vectors from arrays
            vector = vector_t (array)
            veci64 = vector_t (ary64)
            vecr64 = vector_t ( real(ary64, kind = real64) )

            do i = 1, numel
                aryvec(i) = veci64
            end do

            vofvec = vector_t (aryvec)


            ! checks for differences between stored and input values
            associate (it => vector % deref % it)
                select type (it)
                    type is ( integer(kind = int32) )
                        diff(1) = int( sum(it - array), kind = int64)
                    class default
                        error stop 'test-array-constructor: unexpected err'
                end select
            end associate


            associate (it => veci64 % deref % it)
                select type (it)
                    type is ( integer(kind = int64) )
                        diff(2) = sum(it - ary64)
                    class default
                        error stop 'test-array-constructor: unexpected err'
                end select
            end associate


            associate (it => vecr64 % deref % it)
                select type (it)
                    type is ( real(kind = real64) )
                        diff(3) = sum( nint(it, kind = int64) - ary64)
                    class default
                        error stop 'test-array-constructor: unexpected err'
                end select
            end associate


            diff(4) = 0_int64
            associate (iter => vofvec % deref % it)
                select type (iter)
                    type is (vector_t)
                        do i = 1, numel
                            associate (it => iter(i) % deref % it)
                                select type (it)
                                    type is ( integer(kind = int64) )
                                        diff(4) = diff(4) + sum(it - ary64)
                                    class default
                                        error stop 'test: unexpected error'
                                    end select
                            end associate
                        end do
                    class default
                        error stop 'test-array-constructor: unexpected err'
                end select
            end associate


            write (*, '(A)', advance='no') &
                & '[00] test-vector-array-constructor(): '
            if ( vector % size() /= int(numel, kind = int64) ) then
                print *, 'FAIL'
            else if (diff(1) /= 0_int64 .or. diff(2) /= 0_int64) then
                print *, 'FAIL'
            else if (diff(3) /= 0_int64 .or. diff(4) /= 0_int64) then
                print *, 'FAIL'
            else
                print *, 'pass'
            end if

            call vofvec % clear ()
            call vofvec % push_back( veci64 )


            deallocate (vector, veci64, vecr64, aryvec, vofvec)


            return
        end subroutine test_vector_array_constructor


        subroutine test_vector_fill_constructor ()
            type(vector_t), allocatable :: vector  !! `empty' vector
            type(vector_t), allocatable :: veci32  !! vector <int32_t>
            type(vector_t), allocatable :: veci64  !! vector <int64_t>
            type(vector_t), allocatable :: vecr64  !! vector <real64_t>
            type(vector_t), allocatable :: vofvec  !! vector of vectors
            type(vector_t), allocatable :: avofvec !! another vec of vecs
            type(vector_t), allocatable :: yavofvec!! yet another v of vecs
            class(*), pointer, contiguous :: it(:) => null()
            class(*), pointer, contiguous :: iter(:) => null()
            integer(kind = int64), parameter :: numel = 64_int64
            integer(kind = int64):: i, j, k, l, diff(3), diffs(3), addr(2)
            integer(kind = int32), parameter :: value = 64
            integer(kind = int32):: mstat


            allocate (vector, veci32, veci64, vecr64, vofvec, avofvec, &
                    & yavofvec, stat=mstat)
            if (mstat /= 0) error stop 'test.vector(): allocation error'


            ! constructs vectors having `numel' copies of `value'
            veci32 = vector_t (numel, value)
            veci64 = vector_t (numel, int (value, kind = int64) )
            vecr64 = vector_t (numel, real(value, kind = real64) )


            ! checks the stored values for consistency
            it => veci32 % deref % it
            select type (it)
                type is ( integer(kind = int32) )
                    diff(1) = int(sum(it), kind = int64) - numel * value
                class default
                    error stop 'test.vector(): unexpected error'
            end select


            it => veci64 % deref % it
            select type (it)
                type is ( integer(kind = int64) )
                    diff(2) = sum(it) - numel * value
                class default
                    error stop 'test.vector(): unexpected error'
            end select


            it => vecr64 % deref % it
            select type (it)
                type is ( real(kind = real64) )
                    diff(3) = nint( sum(it), kind=int64 ) - numel * value
                class default
                    error stop 'test.vector(): unexpected error'
            end select


            write (*, '(A)', advance='no') '[00] test-vector.construct(): '
            if (veci32 % size() /= numel .or. veci64 % size() /= numel) then
                print *, 'FAIL'
            else if (vecr64 % size() /= numel) then
                print *, 'FAIL'
            else if (diff(1) /= 0_int64 .or. diff(2) /= 0_int64) then
                print *, 'FAIL'
            else if (diff(3) /= 0_int64) then
                print *, 'FAIL'
            else
                print *, 'pass'
            end if


            ! creates vectors of vectors
            vofvec  = vector_t (numel, veci32)   !! vec < vec<T> >
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

                    ! checks for aliasing at the deepest nesting level
                    ! within the same intermediate vector
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

                    ! also checks for aliasing at the deepest level but
                    ! between different intermediate vectors
                    do k = 1_int64, (numel - 1_int64)
                        do l = k + 1_int64, numel

                            associate (it_1 => iter(k) % deref % it, &
                                     & it_2 => iter(l) % deref % it)

                                do i = 1_int64, numel
                                    do j = 1_int64, numel

                                        addr(1) = loc( it_1(i) )
                                        addr(2) = loc( it_2(j) )
                                        if ( addr(1) == addr(2) ) then
                                            diffs(2) = diffs(2) + 1_int64
                                        end if

                                    end do
                                end do

                            end associate

                        end do
                    end do

                    ! checks for aliasing among the intermediate vectors
                    do k = 1_int64, (numel - 1_int64)
                        do l = k + 1_int64, numel

                            associate ( it_1 => iter(k), it_2 => iter(l) )

                                if ( loc(it_1) == loc(it_2) ) then
                                    diffs(2) = diffs(2) + 1_int64
                                end if

                            end associate

                        end do
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

            vector = vector_t ()                !! `empty' vector <T>
            yavofvec = vector_t (numel, vector) !! vector < vector<T> >

            ! checks that the iterator of the `empty' vectors point to NULL
            diffs(3) = 0_int64
            iter => yavofvec % deref % it
            select type (iter)
                type is (vector_t)
                    do i = 1_int64, (numel - 1_int64)
                        do j = i + 1_int64, numel

                            associate (it_1 => iter(i) % deref % it, &
                                     & it_2 => iter(j) % deref % it)

                                if ( loc(it_1) == loc(it_2) ) then
                                    if ( loc(it_1) /= 0_int64 ) then
                                        ! increments if different from NULL
                                        diffs(3) = diffs(3) + 1_int64
                                    end if
                                end if

                            end associate

                        end do
                    end do
                class default
                    error stop 'test.vector(): unexpected error'
            end select


            write (*, '(A)', advance='no') '[02] test-vector.construct(): '
            if ( yavofvec % size () /= numel ) then
                print *, 'FAIL'
            else if (diffs(3) /= 0_int64) then
                print *, 'FAIL'
            else
                print *, 'pass'
            end if


            deallocate (vector, veci32, veci64, vecr64, vofvec, avofvec, &
                      & yavofvec)

            return
        end subroutine


        subroutine test_vector_push_back ()
            ! Synopsis: Tests pushing values unto back of vector.
            type(vector_t), allocatable :: vector    !! vector of int64
            type(vector_t), allocatable :: vecr64    !! vector of real64
            type(vector_t), allocatable :: vofvec    !! vector of vectors
            type(vector_t), allocatable :: avofvec   !! another vec of v...
            type(vector_t), allocatable :: yavofvec  !! yet another vec ...
            type(vector_t), allocatable :: ysavofvec !! yet still anoth ...
            type(chronom) :: stopwatch
            integer(kind = int64):: i
            integer(kind = int64), parameter :: n = 65536_int64
            integer(kind = int32):: mstat

            allocate (vector, vofvec, yavofvec, avofvec, ysavofvec,&
                    & vecr64, stat = mstat)
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


            vecr64 = vector_t ()

            i = 0_int64
            call stopwatch % tic ()
            do while (i /= n)
                call vecr64 % push_back ( real(i, kind = real64) )
                i = i + 1_int64
            end do
            call stopwatch % toc ()


            print *, "done"
            print *, new_line('n')//new_line('n')
            print *, "size: ", vecr64 % size()
            write (*, '(1X,A)', advance='no') "push-back test real64:"
            if ( n == vecr64 % size() ) then
                print *, "pass"
            else
                print *, "FAIL"
            end if


            print *, "elapsed-time (millis): ", stopwatch % etime ()


            vofvec = vector_t ()


            i = 0_int64
            do while (i /= 64_int64)
                call vofvec % push_back (vector)
                i = i + 1_int64
            end do


            avofvec = vofvec   ! copies vector of vectors vector<vector_t>


            write (*, '(1X,A)', advance='no') "push-back test 2:"
            if ( avofvec % size() == vofvec % size() ) then
                print *, "pass"
            else
                print *, "FAIL"
            end if


            ! creates a vector of (vector of vectors)
            yavofvec = vector_t ()
            call yavofvec % push_back (avofvec)
            ysavofvec = yavofvec


            write (*, '(1X,A)', advance='no') "push-back test 3:"
            if ( ysavofvec % size() == yavofvec % size() ) then
                print *, "pass"
            else
                print *, "FAIL"
            end if


            print *, new_line('n')//new_line('n')


            deallocate (vector, vofvec, yavofvec, avofvec,&
                      & vecr64, stat = mstat)
            if (mstat /= 0) then
                error stop "test::vector.push_back: deallocation error"
            end if

            return
        end subroutine


        subroutine test_vector_push_back_array ()
            ! tests pushing an array unto the back of a vector
            type(vector_t), allocatable :: vector
            type(vector_t), allocatable :: veci64
            type(vector_t), allocatable :: vecr64
            type(vector_t), allocatable :: vofvec
            type(vector_t), allocatable :: aryvec(:)
            integer(kind = int64) :: k
            integer(kind = int32) :: i, j
            integer(kind = int32) :: diff(2)
            integer(kind = int64) :: diffs(2)
            integer(kind = int64), parameter :: b = 0_int64, e = 63_int64
            integer(kind = int64), parameter :: ary64(*) = [(k, k = b, e)]
            integer(kind = int32), parameter :: array(*) = [(i, i = 0, 63)]
            integer(kind = int32) :: mstat
            integer(kind = int32), parameter :: numel = &
                  & size(array = array, dim = 1, kind = int32)


            allocate (vector, veci64, vecr64, aryvec(numel), vofvec, &
                    & stat=mstat)
            if (mstat /= 0) error stop 'test-push-back: allocation error'


            vector = vector_t ()                !! instantiates vector
            call vector % push_back (array)     !! pushes array unto back
            call veci64 % push_back (ary64)
            call vecr64 % push_back ( real(ary64, kind = real64) )

            do i = 1, numel
                aryvec(i) = vector
            end do

            call vofvec % push_back (aryvec)

            ! checks for differences between stored and input values
            associate (it => vector % deref % it)
                select type (it)
                    type is ( integer(kind = int32) )
                        diff(1) = sum(it - array)
                    class default
                        error stop 'test-push-back: unexpected error'
                end select
            end associate


            associate (it => veci64 % deref % it)
                select type (it)
                    type is ( integer(kind = int64) )
                        diffs(1) = sum(it - ary64)
                    class default
                        error stop 'test-push-back: unexpected error'
                end select
            end associate


            associate (it => vecr64 % deref % it)
                select type (it)
                    type is ( real(kind = real64) )
                        diffs(2) = sum( nint(it, kind = int64) - ary64)
                    class default
                        error stop 'test-push-back: unexpected error'
                end select
            end associate


            diff(2) = 0
            associate (iter => vofvec % deref % it)
                select type (iter)
                    type is (vector_t)
                        do i = 1, numel
                            associate ( it => iter(i) % deref % it )
                                select type (it)
                                    type is ( integer(kind = int32) )
                                        diff(2) = diff(2) + sum(it - array)
                                    class default
                                        error stop 'unexpected error'
                                end select
                            end associate
                        end do
                    class default
                        error stop 'test-push-back: unexpected error'
                end select
            end associate


            write (*, '(A)', advance='no') '[00] test-push-back-array(): '
            if ( vector % size() /= int(numel, kind = int64) ) then
                print *, 'FAIL'
            else if ( veci64 % size() /= int(numel, kind = int64) ) then
                print *, 'FAIL'
            else if ( vecr64 % size() /= int(numel, kind = int64) ) then
                print *, 'FAIL'
            else if ( vofvec % size() /= int(numel, kind = int64) ) then
                print *, 'FAIL'
            else if (diff(1) /= 0 .or. diff(2) /= 0) then
                print *, 'FAIL'
            else if (diffs(1) /= 0_int64 .or. diffs(2) /= 0_int64) then
                print *, 'FAIL'
            else
                print *, 'pass'
            end if


            ! pushes more arrays to test growing the vector
            call vector % push_back (array)
            call vector % push_back (array)
            call vector % push_back (array)

            call veci64 % push_back (ary64)
            call veci64 % push_back (ary64)
            call veci64 % push_back (ary64)

            call vecr64 % push_back ( real(ary64, kind=real64) )
            call vecr64 % push_back ( real(ary64, kind=real64) )
            call vecr64 % push_back ( real(ary64, kind=real64) )

            call vofvec % push_back (aryvec)
            call vofvec % push_back (aryvec)
            call vofvec % push_back (aryvec)


            diff(1) = 0
            ! checks for differences between stored and input values
            associate (it => vector % deref % it)
                select type (it)
                    type is ( integer(kind = int32) )
                        do i = 0, 3
                            do j = 1, numel
                                diff(1) = diff(1) + it(j + i * numel) - &
                                    & array(j)
                            end do
                        end do
                    class default
                        error stop 'test-push-back: unexpected error'
                end select
            end associate


            diffs(1) = 0_int64
            associate (it => veci64 % deref % it)
                select type (it)
                    type is ( integer(kind = int64) )
                        do i = 0, 3
                            do j = 1, numel
                                diffs(1) = diffs(1) + it(j + i * numel) - &
                                    & ary64(j)
                            end do
                        end do
                    class default
                        error stop 'test-push-back: unexpected error'
                end select
            end associate


            diffs(2) = 0_int64
            associate (it => vecr64 % deref % it)
                select type (it)
                    type is ( real(kind = real64) )
                        do i = 0, 3
                            do j = 1, numel
                                diffs(2) = diffs(2) + &
                                  & nint(it(j + i * numel), kind=int64) - &
                                  & ary64(j)
                            end do
                        end do
                    class default
                        error stop 'test-push-back: unexpected error'
                end select
            end associate


            diff(2) = 0
            associate (iter => vofvec % deref % it)
                select type (iter)
                    type is (vector_t)
                        do i = 1, 4 * numel
                            associate ( it => iter(i) % deref % it )
                                select type (it)
                                    type is ( integer(kind = int32) )
                                        diff(2) = diff(2) + sum(it - array)
                                    class default
                                        error stop 'unexpected error'
                                end select
                            end associate
                        end do
                    class default
                        error stop 'test-push-back: unexpected error'
                end select
            end associate


            write (*, '(A)', advance='no') '[01] test-push-back-array(): '
            if ( vector % size() /= int(4 * numel, kind = int64) ) then
                print *, 'FAIL'
            else if ( veci64 % size() /= int(4 * numel, kind=int64) ) then
                print *, 'FAIL'
            else if ( vecr64 % size() /= int(4 * numel, kind=int64) ) then
                print *, 'FAIL'
            else if ( vofvec % size() /= int(4 * numel, kind=int64) ) then
                print *, 'FAIL'
            else if (diff(1) /= 0 .or. diff(2) /= 0) then
                print *, 'FAIL'
            else if (diffs(1) /= 0_int64 .or. diffs(2) /= 0_int64) then
                print *, 'FAIL'
            else
                print *, 'pass'
            end if


            deallocate (vector, veci64, vecr64, vofvec, aryvec)


            return
        end subroutine test_vector_push_back_array


        subroutine test_vector_push_back_pointer ()
            ! tests pushing a pointer unto the back of a vector
            type(vector_t) :: vector
            type(vector_t) :: avector
            type(vector_t) :: vofvec
            type(vector_t) :: avofvec
            type(pointer_t), allocatable :: pointer
            class(*), pointer, contiguous :: iter(:) => null()
            class(*), allocatable, target :: up_x, up_y, up_z
            integer(kind = int64), parameter :: x = 16_int64
            integer(kind = int64), parameter :: y = 32_int64
            integer(kind = int64), parameter :: z = 64_int64
            integer(kind = int64) :: idx, diff, diffs(2)
            integer(kind = int32) :: mstat
            character(*), parameter :: name = &
                & 'test-vector-push-back-pointer():'
            character(*), parameter :: unexpected = name // ' ' // &
                & 'unexpected error'
            character(*), parameter :: malloc_err = name // ' ' // &
                & 'memory (de)allocation error'


            allocate (pointer, stat=mstat)
            if (mstat /= 0) then
                error stop malloc_err
            end if

            allocate (up_x, source = x, stat=mstat)
            if (mstat /= 0) then
                error stop malloc_err
            end if

            allocate (up_y, source = y, stat=mstat)
            if (mstat /= 0) then
                error stop malloc_err
            end if

            allocate (up_z, source = z, stat=mstat)
            if (mstat /= 0) then
                error stop malloc_err
            end if


            pointer % p => up_x
            call vector % push_back (pointer)

            pointer % p => up_y
            call vector % push_back (pointer)

            pointer % p => up_z
            call vector % push_back (pointer)


            diffs(:) = 0_int64
            ! checks for data and memory address differences
            diff = check_diffs (vector, up_x, up_y, up_z, x, y, z, diffs)


            write (*, '(A)', advance='no') &
                & '[00] test-vector-push-back-pointer(): '
            if ( diff /= 0_int64 ) then
                print *, 'FAIL'
            else
                print *, 'pass'
            end if


            avector = vector    !! copies vector <pointer_t>

            diffs(:) = 0_int64
            ! checks for differences in the copied vector
            diff = check_diffs (vector, up_x, up_y, up_z, x, y, z, diffs)

            write (*, '(A)', advance='no') &
                & '[01] test-vector-push-back-pointer(): '
            if ( diff /= 0_int64 ) then
                print *, 'FAIL'
            else
                print *, 'pass'
            end if


            do idx = 1_int64, 64_int64
                call vofvec % push_back (avector)
            end do


            diffs = 0_int64
            iter => vofvec % deref % it
            do idx = 1_int64, vofvec % size ()

                select type (iter)
                    type is (vector_t)
                        diff = check_diffs (vector, up_x, up_y, up_z, &
                                              & x, y, z, diffs)
                    class default
                        error stop unexpected
                end select

            end do



            write (*, '(A)', advance='no') &
                & '[02] test-vector-push-back-pointer(): '
            if ( diff /= 0_int64 ) then
                print *, 'FAIL'
            else
                print *, 'pass'
            end if


            avofvec = vofvec

            diffs = 0_int64
            iter => avofvec % deref % it
            do idx = 1_int64, avofvec % size ()

                select type (iter)
                    type is (vector_t)
                        diff = check_diffs (vector, up_x, up_y, up_z, &
                                              & x, y, z, diffs)
                    class default
                        error stop unexpected
                end select

            end do


            write (*, '(A)', advance='no') &
                & '[03] test-vector-push-back-pointer(): '
            if ( diff /= 0_int64 ) then
                print *, 'FAIL'
            else
                print *, 'pass'
            end if

            deallocate (up_x, up_y, up_z, stat=mstat)
            if (mstat /= 0) then
                error stop 'test-push-back-pointer: deallocation error'
            end if

            pointer % p => null()
            deallocate (pointer, stat=mstat)
            if (mstat /= 0) then
                error stop 'test-push-back-pointer: deallocation error'
            end if


            return
        end subroutine test_vector_push_back_pointer


        function vector_pointer_t_check_diffs (vector, up_x, up_y, up_z, &
                                             & x, y, z, idiff) result(diff)
            type(vector_t), intent(in) :: vector
            class(*), intent(in) :: up_x, up_y, up_z
            integer(kind = int64), intent(in) :: x, y, z
            integer(kind = int64), intent(in) :: idiff(:)
            integer(kind = int64) :: diffs( size(idiff) )
            integer(kind = int64) :: diff
            class(*), pointer :: p => null()
            class(*), pointer, contiguous :: it(:) => null()
            integer(kind = int64) :: i
            character(*), parameter :: name = &
                & 'test-vector-push-back-pointer():'
            character(*), parameter :: unexpected = name // ' ' // &
                & 'unexpected error'

            diffs = idiff
            it => vector % deref % it
            select type (it)
                type is (pointer_t)

                    do i = 1_int64, size(array=it, dim=1, kind=int64)

                        select case (i)


                            case (1_int64)

                                if ( loc(up_x) /= loc(it(i) % p) ) then
                                    diffs(1) = diffs(1) + 1_int64
                                end if

                                p => it(i) % p
                                select type (p)

                                    type is ( integer(kind = int64) )

                                        if (x /= p) then
                                            diffs(2) = diffs(2) + 1_int64
                                        end if

                                    class default
                                        error stop unexpected

                                end select


                            case (2_int64)

                                if ( loc(up_y) /= loc(it(i) % p) ) then
                                    diffs(1) = diffs(1) + 1_int64
                                end if

                                p => it(i) % p
                                select type (p)

                                    type is ( integer(kind = int64) )

                                        if (y /= p) then
                                            diffs(2) = diffs(2) + 1_int64
                                        end if

                                    class default
                                        error stop unexpected

                                end select


                            case (3_int64)

                                if ( loc(up_z) /= loc(it(i) % p) ) then
                                    diffs(1) = diffs(1) + 1_int64
                                end if

                                p => it(i) % p
                                select type (p)

                                    type is ( integer(kind = int64) )

                                        if (z /= p) then
                                            diffs(2) = diffs(2) + 1_int64
                                        end if

                                    class default
                                        error stop unexpected

                                end select


                            case default
                                error stop unexpected


                        end select

                    end do

                class default
                    error stop unexpected
            end select

            diff = sum(diffs)

            return
        end function vector_pointer_t_check_diffs



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

            vector(:) = create ()
            do i = 1_int64, size(vector, dim=1, kind=int64)
                call vector(i) % valid()        !! validates iterators
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
            type(vector_t), allocatable :: vofvec
            class(*), pointer, contiguous :: it(:) => null()
            integer(kind = int64):: n, t
            integer(kind = int32):: mstat

            allocate (vector, vofvec, stat = mstat)
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


            vofvec = vector_t ()
            call vofvec % push_back (vector)
            it => vofvec % deref % it


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

            deallocate (vector, vofvec, stat = mstat)
            if (mstat /= 0) then
                error stop "test::vector.iterator: deallocation error"
            end if

            return
        end subroutine


        subroutine test_vector_public_iterator ()
            type(vector_t), allocatable, target :: vector
            type(vector_t), allocatable, target :: vecr64
            type(vector_t), allocatable :: avector !! another vector
            class(*), pointer, contiguous :: iter(:)
            integer(kind = int32), allocatable :: values(:)
            integer(kind = int32):: i, v(1), value
            integer(kind = int32):: mstat

            allocate (vector, avector, vecr64, values(256), stat = mstat)
            if (mstat /= 0) then
                error stop "test::vector.iterator: allocation error"
            end if


            vector = vector_t ()
            vecr64 = vector_t ()

            do i = 1, 256
                value = i
                values(i) = value
                call vector % push_back (value)
                call vecr64 % push_back ( real(value, kind=real64) )
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


            avector = vector


            iter => avector % deref % it
            write (*, '(1X,A)', advance='no') "[5] test::v.iter: "
            if ( size(iter, kind = int64) /= avector % size() ) then
                print *, "FAIL"
            else
                print *, "pass"
            end if


            write (*, '(1X,A)', advance='no') "[6] test::v.iter: "
            if (loc(avector % deref % it) == loc(vector % deref % it) ) then
                print *, "FAIL"
            else
                print *, "pass"
            end if


            ! uses iterator to process values contained in vector<real64_t>
            associate (it => vecr64 % deref % it)
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
    use vector_class_tests, only: iterator =>&
                                      & test_vector_based_iterator
    use vector_class_tests, only: range_constructor =>&
                                      & test_vector_range_constructor
    use vector_class_tests, only: array_constructor =>&
                                      & test_vector_array_constructor
    use vector_class_tests, only: construct => test_vector_fill_constructor
    use vector_class_tests, only: get => test_vector_get
    use vector_class_tests, only: push_back => test_vector_push_back
    use vector_class_tests, only: push_back_array => &
                                      & test_vector_push_back_array
    use vector_class_tests, only: push_back_pointer => &
                                      & test_vector_push_back_pointer
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


    call iterator ()
    call range_constructor ()
    call array_constructor ()
    call construct ()
    call push_back ()
    call push_back_array ()
    call push_back_pointer ()
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
