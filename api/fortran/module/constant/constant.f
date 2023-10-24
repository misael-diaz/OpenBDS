      module constant
        use, intrinsic :: iso_fortran_env, only: int64
        use, intrinsic :: iso_fortran_env, only: real64
        implicit none
        private
        save
        public :: LIMIT
        public :: LENGTH
        public :: NUM_PARTICLES

c       system box limits and length
        real(kind = real64), parameter :: LIMIT = 8.0_real64
        real(kind = real64), parameter :: LENGTH = (2.0_real64 * LIMIT)
        integer(kind = int64), parameter :: NUM_PARTICLES = 256_int64

      end module constant

*   OpenBDS                                             October 21, 2023
*
*   source: api/fortran/module/constant/constant.f
*   author: @misael-diaz
*
*   Synopsis:
*   Defines OBDS Constants.
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
