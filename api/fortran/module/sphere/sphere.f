      module sphere
        use, intrinsic :: iso_fortran_env, only: int64
        use, intrinsic :: iso_fortran_env, only: real64
        use :: constant, only: LIMIT
        use :: constant, only: LENGTH
        use :: constant, only: NUM_SPHERES => NUM_PARTICLES
        use :: force, only: force__Brownian_force
        use :: system, only: system__PBC
        use :: dynamic, only: dynamic__shifter
        use :: particle, only: particle_t
        implicit none
        private
        save

c       parameters:

c       sphere radius, diameter, and contact-distance
        real(kind = real64), parameter :: RADIUS = 1.0_real64
        real(kind = real64), parameter :: DIAMETER = 2.0_real64 * RADIUS
        real(kind = real64), parameter :: CONTACT = DIAMETER


        type, extends(particle_t), public :: sphere_t
          contains
            private
            procedure :: updater
            procedure, public :: update => updater
            final :: destructor
        end type

        interface sphere_t
          module procedure constructor
        end interface

      contains

        subroutine grid (spheres)
c         Synopsis:
c         Places the spheres in a grid (or lattice) like structure.
c         Note:
c         The intent(in) attribute refers to the pointer only, meaning that the
c         procedure does not modify its original association; however, the procedure
c         does modify the position vectors of the spheres.
          type(sphere_t), pointer, intent(in) :: spheres
c         pointers to the position vector components
          real(kind = real64), pointer, contiguous :: x(:) => null()
          real(kind = real64), pointer, contiguous :: y(:) => null()
          real(kind = real64), pointer, contiguous :: z(:) => null()
c         uses offset to ensure no particle overlaps across boundaries
          real(kind = real64), parameter :: offset = RADIUS
c         maximum number of spheres that can be stacked in one dimension 1D
          integer(kind = int64), parameter :: max_num_sph_stacked_1d =
     +    floor(LENGTH / CONTACT, kind = int64)
c         maximum number of spheres that can be stacked in two dimensions 2D
          integer(kind = int64), parameter :: max_num_sph_stacked_2d =
     +    (max_num_sph_stacked_1d ** 2)
c         maximum number of spheres that can be stacked in three dimensions 3D
          integer(kind = int64), parameter :: max_num_sph_stacked_3d =
     +    (max_num_sph_stacked_1d ** 3)
c         alias
          integer(kind = int64), parameter :: stacked_1d =
     +    max_num_sph_stacked_1d
c         alias
          integer(kind = int64), parameter :: stacked_2d =
     +    max_num_sph_stacked_2d
c         particle counter
          integer(kind = int64) :: counter
c         particle id
          integer(kind = int64) :: id
c         index
          integer(kind = int64) :: i
c         error message
          character(*), parameter :: errmsg = "sphere::grid(): "//
     +    "it is impossible to fit the requested number of spheres "//
     +    "in a grid (or lattice) like structure without incurring "//
     +    "in particle overlaps"

c         complains if it is not possible to place the particles in the grid-like
c         structure implemented by this procedure
          if (NUM_SPHERES > max_num_sph_stacked_3d) then
            error stop errmsg
          end if

          x => spheres % x

          counter = 0_int64
c         loop-invariant: so far we have updated the `x' position of `counter' spheres
c         NOTE:
c         stacks a pile of spheres (at contact) along the x-dimension; the `x'
c         coordinates increment on each iteration until a pile is completed, and in
c         that instance the `x' coordinate is reseted to zero.
          do while (counter /= NUM_SPHERES)
            id = counter + 1_int64
            i = mod(counter, max_num_sph_stacked_1d)
            x(id) = offset + CONTACT * real(i, kind = real64)
            counter = counter + 1_int64
          end do

c         adjusts `x'-coordinates so that they fall in the range [-LIMIT, +LIMIT]
          x = x - LIMIT

          y => spheres % y

          counter = 0_int64
c         loop-invariant: so far we have updated the `y' position of `counter' spheres
c         NOTE:
c         The `y' coordinates increment when a pile is completed, the `y' coordinate
c         gets reseted upon filling an entire area (in the x-y plane).
          do while (counter /= NUM_SPHERES)
            id = counter + 1_int64
            i = mod(counter, stacked_2d) / stacked_1d
            y(id) = offset + CONTACT * real(i, kind = real64)
            counter = counter + 1_int64
          end do

c         adjusts the `y'-coordinates so that they fall in the range [-LIMIT, +LIMIT]
          y = y - LIMIT

          z => spheres % z

          counter = 0_int64
c         loop-invariant: so far we have updated the `z' position of `counter' spheres
c         NOTE: The `z' coordinates increment upon filling an area (in the x-y plane).
          do while (counter /= NUM_SPHERES)
            id = counter + 1_int64
            i = counter / max_num_sph_stacked_2d
            z(id) = offset + CONTACT * real(i, kind = real64)
            counter = counter + 1_int64
          end do

c         adjusts the `z'-coordinates so that they fall in the range [-LIMIT, +LIMIT]
          z = z - LIMIT

          return
        end subroutine grid


        function constructor () result(spheres)
c         Synopsis:
c         Allocates resources and initializes the spheres.
          type(sphere_t), pointer :: spheres
          integer(kind = int64) :: mstat

c         memory allocations:
          spheres => null()
          allocate(spheres, stat = mstat)
          if (mstat /= 0_int64) then
            error stop "sphere_t(): memory allocation error"
          end if

c         initializations:
          call spheres % initialize()

c         places the spheres in a grid (or lattice) like structure
          call grid(spheres)

          return
        end function constructor


        subroutine updater (particles)
c         Synopsis:
c         Implements non-interacting Brownian spheres.
          class(sphere_t), intent(inout) :: particles

          call force__Brownian_force(particles) ! computes Brownian forces
          call dynamic__shifter(particles)      ! shifts particles Brownianly
          call system__PBC(particles)           ! applies Periodic Boundary Conditions

          return
        end subroutine updater


        subroutine destructor (spheres)
c         Synopsis:
c         Frees resources allocated for the spheres.
          type(sphere_t), intent(inout) :: spheres
          integer(kind = int64) :: mstat
          character(*), parameter :: errmsg =
     +    "sphere::destructor(): unexpected memory deallocation error"

          if ( associated(spheres % data) ) then

            deallocate(spheres % data, stat = mstat)

            if (mstat /= 0_int64) then
              error stop errmsg
            end if

            spheres % data => null()

          end if

          return
        end subroutine destructor

      end module sphere

*   OpenBDS                                             October 22, 2023
*
*   source: api/fortran/module/sphere/sphere.f
*   author: @misael-diaz
*
*   Synopsis:
*   Extends the Particle type to define the Sphere type.
*
*   Copyright (C) 2023 Misael Díaz-Maldonado
*
*   This program is free software: you can redistribute it and/or modify
*   it under the terms of the GNU General Public License as published by
*   the Free Software Foundation, either version 3 of the License, or
*   (at your option) any later version.
*
*   This program is distributed in the hope that it will be useful,
*   but WITHOUT ANY WARRANTY; without even the implied warranty of
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*   GNU General Public License for more details.
*
*   You should have received a copy of the GNU General Public License
*   along with this program. If not, see <http://www.gnu.org/licenses/>.
*
*   References:
*   [0] SJ Chapman, FORTRAN for Scientists and Engineers, 4th edition.
*   [1] MP Allen and DJ Tildesley, Computer Simulation of Liquids.
*   [2] S Kim and S Karrila, Microhydrodynamics.
