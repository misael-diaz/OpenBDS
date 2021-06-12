!
!   project: OpenBDS
!   source:  particles.f90
!   author:  misael-diaz
!
!   Synopsis:
!   Defines the particle class.
!
!
!   Copyright (C) 2016 - 2021 Misael Diaz-Maldonado
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
!

module particles
    use, intrinsic :: iso_fortran_env, only: int64, real64
    implicit none
    private


    type :: string_t
        character (:), allocatable :: str
    end type


    type, abstract, public :: particle_t
        type(string_t) :: shape
        ! position
        real(kind = real64), allocatable :: r_x(:)
        real(kind = real64), allocatable :: r_y(:)
        real(kind = real64), allocatable :: r_z(:)

        integer(kind = int64), allocatable :: id(:)
!       contains
!           private
!           procedure(i_init), deferred :: initializer
    end type


!   abstract interface
!       subroutine i_init (self, n)
!           use, intrinsic :: iso_fortran_env, only: int64
!           import particle_t
!           implicit none
!           class(particle_t), intent(inout) :: self
!           integer(kind = int64), intent(in) :: n
!       end subroutine
!   end interface


end module particles


module spheres
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64
    use particles, only: particle_t
    use utils, only: allocator   => util_allocate_array
    use utils, only: deallocator => util_deallocate_array
    implicit none


    type, public, extends(particle_t) :: sphere_t
        private
        ! force
        real(kind = real64), allocatable :: f_x(:)
        real(kind = real64), allocatable :: f_y(:)
        real(kind = real64), allocatable :: f_z(:)

        ! unbounded position
        real(kind = real64), allocatable :: ur_x(:)
        real(kind = real64), allocatable :: ur_y(:)
        real(kind = real64), allocatable :: ur_z(:)

        ! linear displacement
        real(kind = real64), allocatable :: dr_x(:)
        real(kind = real64), allocatable :: dr_y(:)
        real(kind = real64), allocatable :: dr_z(:)

        ! pointers to procedures go here

        contains
            private
!           procedure :: initializer
            procedure, public :: spawn => initializer
            final :: finalizer
    end type


    private
    contains


        subroutine initializer (self, n)
            ! Synopsis:
            ! Assigns IDs in the asymmetric range [0, N), where N is the
            ! total number of particles.
            class(sphere_t), intent(inout) :: self
            integer(kind = int64), intent(in) :: n
            integer(kind = int64) :: i
            integer(kind = int32) :: mstat


            call allocator (n, self % id)
            allocate (character(len=len("sphere")) :: self % shape % str, &
                    & stat = mstat)
            if (mstat /= 0) error stop "failed to allocate string"


            i = 0_int64
            do while (i /= n)
                self % id = i
                i = i + 1_int64
            end do

            self % shape % str(:) = "sphere"

            return
        end subroutine


        subroutine finalizer (sph)
            type(sphere_t), intent(inout) :: sph

            print *, 'destroying spheres data ... '

            if ( allocated(sph % id) ) then
                call deallocator(sph % id)
            end if

            if ( allocated(sph % shape % str) ) then
                deallocate(sph % shape % str)
            end if

            return
        end subroutine


end module spheres

! TODO:
! [x] invoke (de)allocator from util
! [x] make particle_t abstract (no finalizer needed since objects
!     are not instantiated from abstract classes)
! [ ] move the finalizer to spheres and of course add code to
!     deallocate the data members
! [x] make the spawn procedure generic (entails defining an interface)
! [ ] add pointers to procedures (for particles interfactions)
! [ ] add a potential type (of sorts) that defines a string with the
!     name of the potential to use for determining the particle
!     interactions.
