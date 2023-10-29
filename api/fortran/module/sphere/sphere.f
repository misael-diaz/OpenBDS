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

        subroutine stackable ()
c         Synopsis:
c         Complains if it is not possible to stack the spheres in a grid (or lattice) like
c         structure without incurring on particle overlaps.
c         cubic system box length
          real(kind = real64), parameter :: L = LENGTH
c         sphere-contact distance
          real(kind = real64), parameter :: C = CONTACT
c         maximum number of spheres that can be stacked in one dimension 1D
          integer(kind = int64), parameter :: max_num_stacked_1d =
     +    floor(L / C, kind = int64)
c         maximum number of spheres that can be stacked in two dimensions 3D
          integer(kind = int64), parameter :: max_num_stacked_3d =
     +    (max_num_stacked_1d ** 3)
c         error message
          character(*), parameter :: errmsg = "sphere::stackable(): "//
     +    "it is impossible to fit the requested number of spheres " //
     +    "in a grid (or lattice) like structure without incurring " //
     +    "in particle overlaps"

          if (NUM_SPHERES > max_num_stacked_3d) then
            error stop errmsg
          end if

          return
        end subroutine stackable


        subroutine sane ()
c         complains if it is not possible to stack the spheres in a grid-like fashion
          call stackable()
          return
        end subroutine sane


        pure subroutine stack_x (x)
c         Synopsis:
c         Sets the `x'-coordinates of the spheres so that they are stacked in 1D.
c         NOTE:
c         The computed  `x' coordinates RHS increment on each iteration until a stack
c         is completed, and in that instance the computed `x' coordinate is set to zero.
c         array of `x'-coordinates of the spheres
          real(kind = real64), intent(out) :: x(NUM_SPHERES)
c         uses offset to ensure no particle overlaps across boundaries
          real(kind = real64), parameter :: offset = RADIUS
c         cubic system box length
          real(kind = real64), parameter :: L = LENGTH
c         sphere-contact distance
          real(kind = real64), parameter :: C = CONTACT
c         maximum number of spheres that can be stacked in one dimension 1D
          integer(kind = int64), parameter :: max_stacked = floor(L / C,
     +    kind = int64)
c         particle counter
          integer(kind = int64) :: counter
c         particle id
          integer(kind = int64) :: id
c         index
          integer(kind = int64) :: i

c         loop-invariant: so far we have updated the `x' position of `counter' spheres
          counter = 0_int64
          do while (counter /= NUM_SPHERES)
            id = counter + 1_int64
            i = mod(counter, max_stacked)
            x(id) = offset + CONTACT * real(i, kind = real64)
            counter = counter + 1_int64
          end do

          return
        end subroutine stack_x


        pure subroutine stack_y (y)
c         Synopsis:
c         Sets the `y'-coordinates of the spheres so that they are stacked in 2D.
c         This is because we have already called `stack_x()' that we can say that
c         the spheres are stacked in 2D.
c         NOTE:
c         The computed `y' coordinates RHS increment when a stack is completed, the
c         `y' coordinate gets reseted upon filling an entire area (in the x-y plane).
c         array of `y'-coordinates of the spheres
          real(kind = real64), intent(out) :: y(NUM_SPHERES)
c         uses offset to ensure no particle overlaps across boundaries
          real(kind = real64), parameter :: offset = RADIUS
c         cubic system box length
          real(kind = real64), parameter :: L = LENGTH
c         sphere-contact distance
          real(kind = real64), parameter :: C = CONTACT
c         maximum number of spheres that can be stacked in one dimension 1D
          integer(kind = int64), parameter :: max_num_stacked_1d =
     +    floor(L / C, kind = int64)
          integer(kind = int64), parameter :: max_num_stacked_2d =
     +    (max_num_stacked_1d ** 2)
c         alias
          integer(kind = int64), parameter :: stacked_1d =
     +    max_num_stacked_1d
c         alias
          integer(kind = int64), parameter :: stacked_2d =
     +    max_num_stacked_2d
          integer(kind = int64) :: counter
c         particle id
          integer(kind = int64) :: id
c         index
          integer(kind = int64) :: i

c         loop-invariant: so far we have updated the `y' position of `counter' spheres
          counter = 0_int64
          do while (counter /= NUM_SPHERES)
            id = counter + 1_int64
            i = mod(counter, stacked_2d) / stacked_1d
            y(id) = offset + CONTACT * real(i, kind = real64)
            counter = counter + 1_int64
          end do

          return
        end subroutine stack_y


        pure subroutine stack_z (z)
c         Synopsis:
c         Sets the `z'-coordinates of the spheres so that they are stacked in 3D.
c         We can say this because we have already called `stack_x()' and `stack_y()'.
c         NOTE:
c         The computed `z' coordinates increment upon filling an area (in the x-y plane).
c         array of `z'-coordinates of the spheres
          real(kind = real64), intent(out) :: z(NUM_SPHERES)
c         uses offset to ensure no particle overlaps across boundaries
          real(kind = real64), parameter :: offset = RADIUS
c         cubic system box length
          real(kind = real64), parameter :: L = LENGTH
c         sphere-contact distance
          real(kind = real64), parameter :: C = CONTACT
c         maximum number of spheres that can be stacked in one dimension 1D
          integer(kind = int64), parameter :: max_num_stacked_1d =
     +    floor(L / C, kind = int64)
c         maximum number of spheres that can be stacked in two dimensions 2D
          integer(kind = int64), parameter :: max_num_stacked_2d =
     +    (max_num_stacked_1d ** 2)
c         alias
          integer(kind = int64), parameter :: stacked_2d =
     +    max_num_stacked_2d
c         particle counter
          integer(kind = int64) :: counter
c         particle id
          integer(kind = int64) :: id
c         index
          integer(kind = int64) :: i

c         loop-invariant: so far we have updated the `z' position of `counter' spheres
          counter = 0_int64
          do while (counter /= NUM_SPHERES)
            id = counter + 1_int64
            i = counter / stacked_2d
            z(id) = offset + CONTACT * real(i, kind = real64)
            counter = counter + 1_int64
          end do

          return
        end subroutine stack_z


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

          x => spheres % x

c         stacks a pile of spheres (at contact) along the x-dimension
          call stack_x(x)

c         adjusts `x'-coordinates so that they fall in the range [-LIMIT, +LIMIT]
          x = x - LIMIT

          y => spheres % y

c         stacks a pile of spheres (at contact) in the y-dimension
          call stack_y(y)

c         adjusts the `y'-coordinates so that they fall in the range [-LIMIT, +LIMIT]
          y = y - LIMIT

          z => spheres % z

c         stacks a pile of spheres (at contact) in the z-dimension
          call stack_z(z)

c         adjusts the `z'-coordinates so that they fall in the range [-LIMIT, +LIMIT]
          z = z - LIMIT

          return
        end subroutine grid


        function constructor () result(spheres)
c         Synopsis:
c         Allocates resources and initializes the spheres.
c         NOTE:
c         We return a pointer so that we won't need to implement the assigment operator
c         while still being able to express the instantiation in a Python-like fashion.
c         (If that's not the reason why the FORTRAN standard has enabled us to do things
c         as such, then we don't know what is.) We can afford to return a pointer despite
c         all the advice against the use of pointers in FORTRAN because instantiation
c         only happens once, and even if the pointer association `=>' is mistaken in the
c         future with assignment `=' we would know that replacing the assignment operator
c         with pointer association will fix that bug. We don't like the idea of relying
c         on the assignment operator that the FORTRAN compiler can synthesize (have done
c         that and have regretted all the time spent trying to figure out why it does not
c         work and how to fix it). We are open to reconsider our position if the standard
c         provides a better solution than our workaround in the future.
          type(sphere_t), pointer :: spheres
          integer(kind = int64) :: mstat

c         performs sane-checks
          call sane()

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

c         computes Brownian forces
          call force__Brownian_force(particles)
c         shifts particles Brownianly
          call dynamic__shifter(particles)
c         applies Periodic Boundary Conditions
          call system__PBC(particles)

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
