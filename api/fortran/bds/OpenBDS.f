#define __SUCCESS__  0_int64
#define __FAILURE__ -1_int64
      program OBDS
        use, intrinsic :: iso_fortran_env, only: int64
        use, intrinsic :: iso_fortran_env, only: real64
        use :: config, only: NUM_STEPS
        use :: config, only: NUM_LOG_STEPS
        use :: sphere, only: sphere_t
        implicit none
c       initializes pointer to collection of spheres
        type(sphere_t), pointer :: spheres => null()
c       sets the number of simulation time-steps
        integer(kind = int64), parameter :: steps = NUM_STEPS
c       after this many steps the OBDS code logs the particle data to a plain text file
        integer(kind = int64), parameter :: log_steps = NUM_LOG_STEPS
c       initializes the step counters
        integer(kind = int64) :: step = 0_int64
        integer(kind = int64) :: istep = 0_int64
        integer(kind = int64) :: status = __SUCCESS__

        spheres => sphere_t() ! instantiates the collection of spheres

        step = 0_int64
c       executes the OBDS loop
c       loop-invariant:
c       so far we have executed `step' OBDS simulation steps
        do while (step /= steps)

          istep = 0_int64
c         loop-invariant:
c         so far we have executed `istep' simulation steps consecutively without logging
          do while (istep /= log_steps)
c           updates the position and orientation of the particles according to the
c           Brownian and particle-particle interaction forces acting on them
            call spheres % update()
            istep = istep + 1_int64
          end do

          print *, 'step:', step + log_steps
          status = spheres % flog(step + log_steps)

          if (STATUS == __FAILURE__) then
c           TODO:
c           generate a more useful error message for the user
c           the most frequent IO error would be that the output directory does not exist
            print *, 'OBDS: IO ERROR'
            exit
          end if

          step = step + log_steps
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
