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

module test_neighlists
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64
    use VectorClass, only: vector_t
    use math_vector_class, only: tensor_t => vector_t
    implicit none
    integer(kind = int64), parameter :: n = 1024_int64
!   integer(kind = int64), parameter :: n = 16384_int64
!   integer(kind = int64), parameter :: n = 65536_int64
    real(kind = real64), parameter :: cutoff2 = 0.25_real64
    save


    interface allocator
        module procedure allocate_vector_t
        module procedure allocate_tensor_t
    end interface


    interface deallocator
        module procedure deallocate_vector_t
        module procedure deallocate_tensor_t
    end interface


    private
    public :: neighlists_check
    contains


        subroutine neighlists_check ()
            ! Synopsis: 
            ! Checks if any particle's included itself in the neighbor-list
            type(vector_t), allocatable :: vec_neighborls
            type(vector_t), pointer, contiguous :: it(:) => null()

            integer(kind = int64):: i, j


            call allocator (vec_neighborls)
            vec_neighborls = neighlists_create ()
            call vec_neighborls % iter (it)


            i = 0_int64
            do j = 1_int64, n
                ! searchs itself in its neighbor-list
                if (it(j) % find (j) /= -1) then
                    i = i + 1_int64
                end if
            end do


            write (*, '(1X,A)', advance='no') "test::neighbor-lists: "
            if (i == 0_int64) then
                print *, "pass"
            else
                print *, "FAIL"
            end if

            call deallocator (vec_neighborls)

            return
        end subroutine


        function neighlists_create () result(vec_neighborls)
            type(vector_t), allocatable :: neighbors
            type(vector_t), allocatable :: vec_neighborls
            type(tensor_t), pointer :: particles => null()

            integer(kind = int64), allocatable :: idx(:)
            logical(kind = int32), allocatable :: mask(:)
            logical(kind = int32), allocatable :: is_neighbor(:)

            integer(kind = int64):: i, j
            integer(kind = int32):: mstat


            call allocator (neighbors)
            call allocator (vec_neighborls)
            call spawn (particles)


            allocate (idx(n), mask(n), is_neighbor(n), stat = mstat)
            if (mstat /= 0) error stop "allocation failure"


            do i = 1_int64, n
                idx(i) = i
            end do


            neighbors = vector_t ()
            vec_neighborls = vector_t ()

            ! builds the neighbor-lists for all particles
            do j = 1_int64, n

                ! obtains the squared distance relative to the jth particle
                call particles % delta2 (j, idx)


                mask        = .true.
                mask (j)    = .false.  ! excludes itself
                is_neighbor = .false.


                ! NOTE: tensor.v stores the relative squared distance
                call is_inrange ( floor(particles % v / &
                    & cutoff2, kind = int32), mask, is_neighbor )


                do i = 1_int64, n
                    ! builds the neighbor-list for the jth particle
                    if ( is_neighbor(i) ) then
                        call neighbors % push_back (i)
                    end if
                end do


                call vec_neighborls % push_back (neighbors)
                call neighbors % clear ()

            end do


            deallocate (idx, mask, is_neighbor, stat = mstat)
            call deallocator (particles)
            call deallocator (neighbors)

            return
        end function


        subroutine spawn (particles)
            ! Synopsis: Spawns particles at random locations.
            type(tensor_t), intent(inout), pointer :: particles
            integer(kind = int64):: i


            call allocator (particles)
            particles = tensor_t (n)


            do i = 1, n
                call random_number  ( particles % x(i) )
            end do

            do i = 1, n
                call random_number  ( particles % y(i) )
            end do

            do i = 1, n
                call random_number  ( particles % z(i) )
            end do


            call particles % normalize ()

            return    
        end subroutine


        elemental subroutine is_inrange (d, m, n)
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


        subroutine allocate_tensor_t (tensor)
            type(tensor_t), intent(inout), pointer :: tensor
            integer(kind = int32):: mstat

            if ( associated(tensor) ) then
                deallocate(tensor, stat = mstat)
                if (mstat /= 0) then
                    error stop "failed to deallocate tensor"
                end if
            end if

            allocate(tensor, stat = mstat)
            if (mstat /= 0) error stop "failed to allocate tensor"

            return
        end subroutine


        subroutine allocate_vector_t (vector)
            type(vector_t), intent(inout), allocatable :: vector
            integer(kind = int32):: mstat
            
            if ( allocated(vector) ) then
                deallocate(vector, stat = mstat)
                if (mstat /= 0) then
                    error stop "failed to deallocate vector"
                end if
            end if
            
            allocate(vector, stat = mstat)
            if (mstat /= 0) error stop "failed to allocate vector"

            return
        end subroutine


        subroutine deallocate_tensor_t (tensor)
            type(tensor_t), intent(inout), pointer :: tensor
            integer(kind = int32):: mstat

            if ( associated(tensor) ) then
                deallocate(tensor, stat = mstat)
                if (mstat /= 0) error stop "unexpected deallocation error"
            end if

            return
        end subroutine


        subroutine deallocate_vector_t (vector)
            type(vector_t), intent(inout), allocatable :: vector
            integer(kind = int32):: mstat

            if ( allocated(vector) ) then
                deallocate(vector, stat = mstat)
                if (mstat /= 0) error stop "unexpected deallocation error"
            end if

            return
        end subroutine


end module


program test_math_vector_class
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64
    use test_neighlists, only: test => neighlists_check
    implicit none

    call test ()

end program

! Vectorization:
! The Intel Fortran Compiler vectorizes the random_number() intrinsic. Other
! than that the GNU Fortran Compiler vectorizes everything else its Intel
! counterpart does. I did not perform a runtime check, just checked the
! optimizer reports of both compilers.
