      module sphere
        use, intrinsic :: iso_fortran_env, only: int64
        use, intrinsic :: iso_fortran_env, only: real64
        use :: constants, only: NUM_SPHERES => NUM_PARTICLES
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

          function constructor () result(spheres)
c           Synopsis:
c           Allocates resources and initializes the spheres.
            type(sphere_t), pointer :: spheres
            integer(kind = int64) :: mstat

c           memory allocations:
            spheres => null()
            allocate(spheres, stat = mstat)
            if (mstat /= 0_int64) then
              error stop "sphere_t(): memory allocation error"
            end if

c           initializations:
            call spheres % initialize()

            return
          end function constructor


          subroutine updater (particles)
c           Synopsis:
c           Initial implementation so that the compiler won't complain.
            class(sphere_t), intent(inout) :: particles

            print *, 'sphere::update(): unimplemented'

            return
          end subroutine updater


          subroutine destructor (spheres)
c           Synopsis:
c           Frees resources allocated for the spheres.
            type(sphere_t), intent(inout) :: spheres
            integer(kind = int64) :: mstat
            character(*), parameter :: errmsg =
     +      "sphere::destructor(): unexpected memory deallocation error"

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
