      module tests
        use, intrinsic :: iso_fortran_env, only: int64
        use, intrinsic :: iso_fortran_env, only: real64
        use :: constant, only: NUM_SPHERES => NUM_PARTICLES
        use :: constant, only: LIMIT
        use :: sphere, only: sphere_t
        implicit none
        private
        public :: test_sphere_id
        public :: test_sphere_positions

        interface test_sphere_id
          module procedure ids
        end interface

        interface test_sphere_positions
          module procedure positions
        end interface

        contains

          subroutine ids()

c           collection of spheres
            type(sphere_t), pointer :: spheres => null()
c           pointer to the particle IDs
            real(kind = real64), pointer, contiguous :: id(:) => null()
c           stores the sum of the sequence [1, NUM_SPHERES]
            integer(kind = int64), parameter :: total =
     +      NUM_SPHERES * (NUM_SPHERES + 1_int64) / 2_int64
c           accumulator
            integer(kind = int64) :: isum = 0_int64
c           positional index
            integer(kind = int64) :: i

c           instantiates the collection of spheres:
            spheres => sphere_t()

c           bindings:
            id => spheres % id

c           tests:
            do i = 1, NUM_SPHERES
              isum = isum + nint(id(i), kind = int64)
            end do

            write (*, '(A)', advance='no') 'test-sphere[0]: '
            if (isum /= total) then
              print *, 'FAIL'
            else
              print *, 'PASS'
            end if

c           call spheres % update()

c           release the memory resources allocated for the spheres
            deallocate(spheres)

            return
          end subroutine ids


          elemental subroutine limits (x, limited)
c           tests if the `x' coordinate is limited, writes outcome to `limited'.
            real(kind = real64), intent(in) :: x
            real(kind = real64), intent(out) :: limited

            if (x < -LIMIT .or. x > +LIMIT) then
              limited = 0.0_real64
            else
              limited = 1.0_real64
            end if

            return
          end subroutine limits


          subroutine positions ()
c           Synopsis:
c           Tests that the position of the spheres are bounded to [-LIMIT, +LIMIT].
c           collection of spheres
            type(sphere_t), pointer :: spheres => null()
c           position vector
            real(kind = real64), pointer, contiguous :: x(:) => null()
            real(kind = real64), pointer, contiguous :: y(:) => null()
            real(kind = real64), pointer, contiguous :: z(:) => null()
c           temporary
            real(kind = real64), pointer, contiguous :: tmp(:) => null()
c           alias
            real(kind = real64), pointer, contiguous :: lim(:) => null()
c           number of failures (that is, the number of unbounded spheres)
            integer(kind = int64) :: fails

            spheres => sphere_t()

            x => spheres % x
            y => spheres % y
            z => spheres % z
            tmp => spheres % tmp
            lim => tmp

            call spheres % update()

            fails = 0_int64
            call limits(x, lim)
            fails = fails + nint(sum(lim), kind = int64)

            call limits(y, lim)
            fails = fails + nint(sum(lim), kind = int64)

            call limits(z, lim)
            fails = fails + nint(sum(lim), kind = int64)

            fails = (3_int64 * NUM_SPHERES - fails)

            write (*, '(A)', advance='no') 'test-sphere[1]: '
            if (fails /= 0_int64) then
              print *, 'FAIL'
            else
              print *, 'PASS'
            end if

            deallocate(spheres)

            return
          end subroutine positions

      end module tests


      program test_sphere_type
        use :: tests, only: test_sphere_id
        use :: tests, only: test_sphere_positions
        implicit none

        call test_sphere_id()
        call test_sphere_positions()

      end program test_sphere_type


*   OpenBDS                                             October 22, 2023
*
*   source: api/fortran/test/sphere/tsphere.f
*   author: @misael-diaz
*
*   Synopsis:
*   Tests the implementation of the sphere type.
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
