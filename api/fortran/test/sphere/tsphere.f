      program test_sphere_type
        use, intrinsic :: iso_fortran_env, only: int64
        use, intrinsic :: iso_fortran_env, only: real64
        use :: constants, only: NUM_SPHERES => NUM_PARTICLES
        use :: sphere, only: sphere_t
        implicit none

c       collection of spheres
        type(sphere_t), pointer :: spheres => null()
c       pointer to the particle IDs
        real(kind = real64), pointer, contiguous :: id(:) => null()
c       stores the sum of the sequence [1, NUM_SPHERES]
        integer(kind = int64), parameter :: total =
     +  NUM_SPHERES * (NUM_SPHERES + 1_int64) / 2_int64
c       accumulator
        integer(kind = int64) :: isum = 0_int64
c       positional index
        integer(kind = int64) :: i

c       instantiates the collection of spheres:
        spheres => sphere_t()

c       bindings:
        id => spheres % id

c       tests:
        do i = 1, NUM_SPHERES
          isum = isum + nint(id(i), kind = int64)
        end do

        write (*, '(A)', advance='no') 'test-sphere[0]: '
        if (isum /= total) then
          print *, 'FAIL'
        else
          print *, 'PASS'
        end if

c       call spheres % update()

c       release the memory resources allocated for the spheres
        deallocate(spheres)

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
