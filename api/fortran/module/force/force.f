      module force
        use, intrinsic :: iso_fortran_env, only: real64
        use :: constant, only: N => NUM_PARTICLES
        use :: random, only: fnrand
        implicit none
        private
        public :: force__Brownian_force

        interface force__Brownian_force
          module procedure Brownian_force
        end interface

      contains

        impure elemental subroutine Brownian_force_component (F_x)
c         Synopsis:
c         Fills the Brownian force component (elementwise) with normally distributed
c         pseudo-random numbers.
          real(kind = real64), intent(out) :: F_x

          F_x = fnrand()

          return
        end subroutine Brownian_force_component


        subroutine Brownian_force (F_x, F_y, F_z)
c         Synopsis:
c         Fills the Brownian forces with normally distributed pseudo-random numbers.
          real(kind = real64), intent(out) :: F_x(N)
          real(kind = real64), intent(out) :: F_y(N)
          real(kind = real64), intent(out) :: F_z(N)

          call Brownian_force_component(F_x)
          call Brownian_force_component(F_y)
          call Brownian_force_component(F_z)

          return
        end subroutine Brownian_force

      end module force

*   OpenBDS                                             October 24, 2023
*
*   source: api/fortran/module/force/force.f
*   author: @misael-diaz
*
*   Synopsis:
*   Implements methods for computing the forces on the particles.
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
