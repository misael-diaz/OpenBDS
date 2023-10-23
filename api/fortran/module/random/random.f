      module random
        use, intrinsic :: iso_fortran_env, only: real64
        use :: ieee_arithmetic, only: ieee_positive_inf
        use :: ieee_arithmetic, only: ieee_value
        implicit none
        private
        public :: fnrand

        interface fnrand
          module procedure frandom
        end interface

      contains

        function furand() result(x)
c         Synopsis:
c         Returns a uniformly distributed pseudo-random number in [0, 1).
c         Forwards its task to the intrinsic `random_number()'.
          real(kind = real64) :: x

          call random_number(x)

          return
        end function furand


        function frandom() result(x)
c         Synopsis:
c         Implements Böx-Muller's method.
c         Yields a Normal (or Gaussian) Pseudo-Random Deviate (or Number).
          real(kind = real64) :: positive_infinity
          real(kind = real64) :: x1
          real(kind = real64) :: x2
          real(kind = real64) :: r
          real(kind = real64) :: x

          positive_infinity = ieee_value(0.0_real64,
     +                                   ieee_positive_inf)
          r = positive_infinity
c         NOTE: compiled with gfortran's -Wno-compare-reals, consider that if `r'
c         is equal to zero that means that furand()'s underlying PRNG is bogus.
          do while (r == 0.0_real64 .or. r > 1.0_real64)

            x1 = furand()
            x2 = furand()

            x1 = 2.0_real64 * x1 - 1.0_real64
            x2 = 2.0_real64 * x2 - 1.0_real64

            r = x1**2 + x2**2

          end do

          r = dsqrt( ( -2.0_real64 * dlog(r) ) / r )

          x1 = r * x1
          x2 = r * x2

          x = x1

          return
        end function frandom

      end module random

*   OpenBDS                                             October 23, 2023
*
*   source: api/fortran/module/random/random.f
*   author: @misael-diaz
*
*   Synopsis:
*   Implements Pseudo-Random Number Generators PRNGs.
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
