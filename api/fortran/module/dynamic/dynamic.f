      module dynamic
        use, intrinsic :: iso_fortran_env, only: real64
        use :: constant, only: dt => TIME_STEP
        use :: constant, only: N => NUM_PARTICLES
        use :: particle, only: particle_t
        implicit none
        private
        public :: dynamic__shifter

        interface dynamic__shifter
          module procedure shift
        end interface

      contains

        subroutine shifter (mobility, x, F_x)
c         Synopsis:
c         Shifts the particles in the direction of the Brownian force.
c         NOTE:
c         This only applies to spheres. The isotropic hydrodynamic resistance is implied.
          real(kind = real64), intent(in) :: mobility
          real(kind = real64), intent(out) :: x(N)
          real(kind = real64), intent(in) :: F_x(N)

          x = x + mobility * F_x

          return
        end subroutine shifter


        subroutine shift (particles)
c         Synopsis:
c         Shifts the particles position by the action of the Brownian forces.
c         NOTE:
c         This code only applies to spheres. We shall overload this method later
c         with a callback, as in the clang API, to handle anisotropic particles.
          class(particle_t), intent(inout), target :: particles
          real(kind = real64), pointer, contiguous :: x(:) => null()
          real(kind = real64), pointer, contiguous :: y(:) => null()
          real(kind = real64), pointer, contiguous :: z(:) => null()
          real(kind = real64), pointer, contiguous :: F_x(:) => null()
          real(kind = real64), pointer, contiguous :: F_y(:) => null()
          real(kind = real64), pointer, contiguous :: F_z(:) => null()
          real(kind = real64), parameter :: m = sqrt(2.0_real64 * dt)
          real(kind = real64), parameter :: mobility = m

          F_x => particles % F_x
          F_y => particles % F_y
          F_z => particles % F_z

c         updates the position vector subjected to periodic boundaries:
          x => particles % x
          y => particles % y
          z => particles % z

          call shifter(mobility, x, F_x)
          call shifter(mobility, y, F_y)
          call shifter(mobility, z, F_z)

c         updates the position vector independent of periodic boundaries:
          x => particles % r_x
          y => particles % r_y
          z => particles % r_z

          call shifter(mobility, x, F_x)
          call shifter(mobility, y, F_y)
          call shifter(mobility, z, F_z)

c         updates the Verlet displacement vector:
          x => particles % Vdx
          y => particles % Vdy
          z => particles % Vdz

          call shifter(mobility, x, F_x)
          call shifter(mobility, y, F_y)
          call shifter(mobility, z, F_z)

          return
        end subroutine shift

      end module dynamic

*   OpenBDS                                             October 24, 2023
*
*   source: api/fortran/module/dynamic/dynamic.f
*   author: @misael-diaz
*
*   Synopsis:
*   Implements methods for updating the position and orientation of the particles.
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
