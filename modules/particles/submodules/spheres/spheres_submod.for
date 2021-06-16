!
!   source:  spheres_submod.f90
!   author:  misael-diaz
!   date:    2021-06-16
!
!   Synopsis:
!   Implements the particle class.
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
!

submodule (spheres) sphere_implementations
!   use, intrinsic :: iso_fortran_env, only: int64, real64
!   implicit none
    contains


        module subroutine initializer (self, n)
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


        module subroutine finalizer (sph)
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


end submodule

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
