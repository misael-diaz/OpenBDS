#define __SUCCESS__  0_int64
#define __FAILURE__ -1_int64

      module OBDS
        use, intrinsic :: iso_fortran_env, only: int32
        implicit none
        private
        public :: sane

        interface sane
          module procedure test_system_clock
        end interface

        contains

        subroutine test_system_clock ()
c         Synopsis:
c         Tests the resolution and range of the system-clock.
          integer(kind = int32) :: system_clock_count_rate
          integer(kind = int32) :: system_clock_count_max

          call system_clock(count_rate = system_clock_count_rate)
          call system_clock(count_max = system_clock_count_max)

          if (system_clock_count_rate /= 1000) then
            error stop "OBDS::sane(): unexpected system-clock error: "//
     +      "OBDS code expects a millisecond resolution system-clock"
          end if

          if (system_clock_count_max /= huge(0)) then
            error stop "OBDS::sane(): unexpected system-clock error: "//
     +      "OBDS code expects the max count to be equal to (2**32 - 1)"
          end if

          return
        end subroutine test_system_clock

      end module OBDS

      program OpenBDS
        use, intrinsic :: iso_fortran_env, only: int32
        use, intrinsic :: iso_fortran_env, only: int64
        use, intrinsic :: iso_fortran_env, only: real64
        use :: config, only: NUM_STEPS
        use :: config, only: NUM_LOG_STEPS
        use :: config, only: WALLTIME
        use :: sphere, only: sphere_t
        use :: OBDS, only: sane
        implicit none
c       initializes pointer to collection of spheres
        type(sphere_t), pointer :: spheres => null()
        real(kind = real64), pointer, contiguous :: tmp(:) => null()
c       sets the number of simulation time-steps
        integer(kind = int64), parameter :: steps = NUM_STEPS
c       after this many steps the OBDS code logs the particle data to a plain text file
        integer(kind = int64), parameter :: log_steps = NUM_LOG_STEPS
c       step counters
        integer(kind = int64) :: step
        integer(kind = int64) :: istep
c       IO status
        integer(kind = int64) :: status
        integer(kind = int32) :: time_elapsed
        integer(kind = int32) :: time_start
        integer(kind = int32) :: time_end

c       performs sane-checks
        call sane()

        call system_clock(count = time_start)

        spheres => sphere_t() ! instantiates the collection of spheres

c       fetches the initial step number (NOTE: `sphere_t()' handles this)
        tmp => spheres % tmp
        istep = int(tmp(1), kind = int64)

        step = istep
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

          status = spheres % flog(step + log_steps)

          if (STATUS == __FAILURE__) then
c           TODO:
c           generate a more useful error message for the user
c           the most frequent IO error would be that the output directory does not exist
            print *, 'OBDS: IO ERROR'
            exit
          end if

c         halts execution if the application walltime has been reached
          call system_clock(count = time_end)
          time_elapsed = (time_end - time_start)

          if (time_elapsed >= WALLTIME) then
c           TODO:
c           [ ] dump the pending status to finish the auto-checkpointing implementation
            exit
          end if

          step = step + log_steps
        end do

        deallocate(spheres)
        spheres => null()

      end program OpenBDS

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
