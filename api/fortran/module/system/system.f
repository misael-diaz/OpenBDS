      module system
        use, intrinsic :: iso_fortran_env, only: real64
        use :: constant, only: N => NUM_PARTICLES
        use :: constant, only: L => LENGTH
        use :: particle, only: particle_t
        implicit none
        private
        public :: system__PBC   ! applies Periodic Boundary Conditions

        interface system__PBC
          module procedure update_position_vector_base
        end interface

      contains

        pure subroutine update_position_vector_component (x)
c         Synopsis:
c         Updates the position vector component by applying periodic boundary conditions.
          real(kind = real64), intent(inout) :: x(N)

          x = x - anint(x / L) * L

          return
        end subroutine update_position_vector_component


        pure subroutine update_position_vector (x, y, z)
c         Synopsis:
c         Updates the position vectors by applying periodic boundary conditions.
          real(kind = real64), intent(inout) :: x(N)
          real(kind = real64), intent(inout) :: y(N)
          real(kind = real64), intent(inout) :: z(N)

          call update_position_vector_component(x)
          call update_position_vector_component(y)
          call update_position_vector_component(z)

          return
        end subroutine update_position_vector


        pure subroutine update_position_vector_base (particles)
c         Synopsis:
c         Updates the position vectors of the particles by applying periodic boundary
c         conditions.
          class(particle_t), intent(inout), target :: particles
          real(kind = real64), pointer, contiguous :: x(:)
          real(kind = real64), pointer, contiguous :: y(:)
          real(kind = real64), pointer, contiguous :: z(:)

          x => particles % x
          y => particles % y
          z => particles % z

          call update_position_vector(x, y, z)

          return
        end subroutine update_position_vector_base

      end module system

*   OpenBDS                                             October 24, 2023
*
*   source: api/fortran/module/system/system.f
*   author: @misael-diaz
*
*   Synopsis:
*   Implements periodic boundaries.
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
