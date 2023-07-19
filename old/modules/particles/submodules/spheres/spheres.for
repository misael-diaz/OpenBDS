!
!   source:  sphere.f90
!   author:  misael-diaz
!   date:    2021-06-16
!
!   Synopsis:
!   Defines the sphere subclass.
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

module sphere_subclass
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64
    use particle_class, only: particle_t
    use utils, only: allocator   => util_allocate_array
    use utils, only: deallocator => util_deallocate_array
    implicit none
    private


    type, extends(particle_t), public :: sphere_t
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


    interface


        module subroutine initializer (self, n)
            ! Synopsis:
            ! Assigns IDs in the asymmetric range [0, N), where N is the
            ! total number of particles.
            class(sphere_t), intent(inout) :: self
            integer(kind = int64), intent(in) :: n
        end subroutine


        module subroutine finalizer (sph)
            type(sphere_t), intent(inout) :: sph
        end subroutine


    end interface


end module

! TODO:
! move components (force, position, and linear displacement) to superclass
