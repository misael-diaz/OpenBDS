!
!   source: test_math_vector.for
!   author: misael-diaz
!   date:   2021-06-17
!
!
!   Synopsis:
!   Tests the math vector class.
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

module neighlists
    use, intrinsic :: iso_fortran_env, only: int32, real64
    implicit none
    public
    contains
        elemental subroutine in_range (d, m, n)
            ! Synopsis:
            ! Sets boolean n[eighbor] equal to .true. for neighbors,
            ! .false. otherwise. The m[ask] excludes the particle itself.

            integer(kind = int32), intent(in) :: d
            logical(kind = int32), intent(in) :: m
            logical(kind = int32), intent(inout) :: n

            if (m .and. d == 0) then
                n = .true.
            else
                n = .false.
            end if

            return
        end subroutine
end module


program test_math_vector_class
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64
    use VectorClass, only: vector_t
    use neighlists, only: in_range
    use math_vector_class, only: math_vector_t => vector_t
    implicit none


    type(vector_t), pointer :: neighbors => null()
    type(math_vector_t), pointer :: vector => null()


    real(kind = real64), parameter :: cutoff2 = 0.25_real64
    real(kind = real64), allocatable :: d(:)


    integer(kind = int64) :: i
    integer(kind = int64), allocatable :: idx(:)
    integer(kind = int64), parameter :: n = 65536_int64
    integer(kind = int32), parameter :: n_int32 = 65536_int32
    logical(kind = int32), allocatable :: mask(:)
    logical(kind = int32), allocatable :: neig(:)
    integer(kind = int32) :: mstat
    integer(kind = int32) :: j



    allocate(neighbors, stat = mstat)
    if (mstat /= 0) error stop "allocation failure"

    allocate(vector, stat = mstat)
    if (mstat /= 0) error stop "allocation failure"


    ! instantiations
    neighbors = vector_t ()
    vector    = math_vector_t (n)


    do i = 1, n
        call random_number  ( vector % x(i) )
    end do

    do i = 1, n
        call random_number  ( vector % y(i) )
    end do

    do i = 1, n
        call random_number  ( vector % z(i) )
    end do




    call vector % normalize ()


    do i = 1, n
        print '(1X,3F8.4)', vector % x(i), vector % y(i), vector % z(i)
    end do




    allocate (d(n), stat = mstat)
    if (mstat /= 0) error stop "allocation failure"


    do i = 1, n
        d(i) = vector % range (n, i)     ! computes vector distance
    end do


    print *, ""
    print *, ""
    print *, "distance::min: ", minval(d)
    print *, "distance::max: ", maxval(d)
    print *, ""
    print *, ""



    call vector % delta2 (n)    ! vector squared distance


    print *, ""
    print *, ""
    print *, "distance::min: ", dsqrt( minval(vector % v) )
    print *, "distance::max: ", dsqrt( maxval(vector % v) )
    print *, ""
    print *, ""




    allocate (idx(n), stat = mstat)
    if (mstat /= 0) error stop "allocation failure"

    allocate (mask(n), stat = mstat)
    if (mstat /= 0) error stop "allocation failure"

    allocate (neig(n), stat = mstat)
    if (mstat /= 0) error stop "allocation failure"


    do i = 1, n
        idx(i) = i
    end do




    call vector % delta2 (n, idx)       ! vector squared distance

    mask     = .true.
    mask (n) = .false.  ! excludes the particle itself via mask
    neig     = .false.
    call in_range( floor(vector % v / cutoff2, kind = int32), mask, neig)


    j = 1
    do i = 1, n
        ! builds the neighbor-list for the nth particle
        if ( neig(i) ) call neighbors % push_back (j)
        j = j + 1
    end do


    print *, ""
    print *, ""
    print *, "distance::min: ", dsqrt( minval(vector % v) )
    print *, "distance::max: ", dsqrt( maxval(vector % v) )
    print *, "vector::neighbors: ", neighbors % size ()
    print *, "find: ", neighbors % find (n_int32)


    ! checks if the particle has included itself in the neighbor-list
    if (neighbors % find (n_int32) /= 0) then
        print *, "fail"
    else
        print *, "pass"
    end if


    print *, ""
    print *, ""
    print *, ""
    print *, ""




    deallocate (d, stat = mstat)
    if (mstat /= 0) error stop "unexpected deallocation failure"

    deallocate (idx, stat = mstat)
    if (mstat /= 0) error stop "unexpected deallocation failure"

    deallocate (mask, stat = mstat)
    if (mstat /= 0) error stop "unexpected deallocation failure"

    deallocate (vector, stat = mstat)
    if (mstat /= 0) error stop "unexpected deallocation failure"

    deallocate (neighbors, stat = mstat)
    if (mstat /= 0) error stop "unexpected deallocation failure"

    print *, ""
    print *, ""
    print *, ""
    print *, ""


end program

! Comments:
! The GNU Fortran Compiler issues a *warning* about reallocating the vector
! upon instantiation when it is declared with the allocatable attribute.
! To avoid this (possible) reallocation it's declared with the pointer 
! attribute.
!
!
! Vectorization:
! The Intel Fortran Compiler vectorizes the random_number() intrinsic. Other
! than that the GNU Fortran Compiler vectorizes everything else its Intel
! counterpart does. I did not perform a runtime check, just checked the
! optimizer reports of both compilers.
