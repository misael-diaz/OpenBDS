#define __SUCCESS__  0_int64
#define __FAILURE__ -1_int64

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
c       time interval for logging the particle data
        real(kind = real64), parameter :: t_log = 2.0_real64**(-4)
c       sets the number of time steps
        integer(kind = int64), parameter :: steps = int(t_end / dt,
     +  kind = int64)
c       log step
        integer(kind = int64), parameter :: lstep = int(t_log / dt,
     +  kind = int64)
c       initializes the step counter
        integer(kind = int64) :: step = 0_int64
        integer(kind = int64) :: status = __SUCCESS__

        spheres => sphere_t() ! instantiates the collection of spheres

        step = 0_int64
c       executes the OBDS loop
        do while (step /= steps)
c         updates the position and orientation of the particles according to the
c         Brownian and particle-particle interaction forces acting on them
          call spheres % update()

          if (mod(step + 1_int64, lstep) == 0_int64) then
            status = spheres % flog(step + 1_int64)
          end if

          if (status == __FAILURE__) then
c           TODO:
c           generate a more useful error message for the user
c           the most frequent IO error would be that the output directory does not exist
            print *, 'OBDS: IO ERROR'
            exit
          end if

          step = step + 1_int64
        end do

        deallocate(spheres)
        spheres => null()

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
