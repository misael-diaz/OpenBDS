      program OBDS
        use, intrinsic :: iso_fortran_env, only: int64
        use, intrinsic :: iso_fortran_env, only: real64
        use :: config, only: dt => TIME_STEP
        use :: sphere, only: sphere_t
        implicit none
c       initializes pointer to collection of spheres
        type(sphere_t), pointer :: spheres => null()
c       sets the end (or final) time of the simulation 
        real(kind = real64), parameter :: t_end = 16.0_real64
c       sets the number of time steps
        integer(kind = int64) :: steps = int(t_end / dt, kind = int64)
c       initializes the step counter
        integer(kind = int64) :: step = 0_int64

        spheres => sphere_t() ! instantiates the collection of spheres

        step = 0_int64
c       executes the OBDS loop
        do while (step /= steps)
c         updates the position and orientation of the particles according to the
c         Brownian and particle-particle interaction forces acting on them
          call spheres % update()
          step = step + 1_int64
        end do

        deallocate(spheres)

      end program OBDS

*   OpenBDS                                             October 22, 2023
*
*   source: api/fortran/module/bds/OpenBDS.f
*   author: @misael-diaz
*
*   Synopsis:
*   OBDS Driver code.
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
