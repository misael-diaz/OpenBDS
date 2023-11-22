#define __SUCCESS__  0_i8
#define __FAILURE__ -1_i8

      module OBDS
        use, intrinsic :: iso_fortran_env, only: i8 => int64
        use, intrinsic :: iso_fortran_env, only: r8 => real64
        use :: config, only: NUM_STEPS
        use :: config, only: NUM_LOG_STEPS
        implicit none
        private
        public :: sane

        interface sane
          module procedure sane_checks
        end interface

      contains

        pure function significand (x) result(res)
c         Synopsis:
c         Returns the significand (or the mantissa) of the double precision floating-point
c         number `x' of kind `r8'.
          real(r8), intent(in) :: x
          real(r8) :: res

          res = 2.0_r8 * fraction(x) - 1.0_r8

          return
        end function significand


        pure function is_pow2 (x) result(res)
c         Synopsis:
c         Returns true if `x' is an exact power of two (has an exact binary floating-point
c         representation), returns false otherwise.
          real(r8), intent(in) :: x
          logical(i8) :: res

          if (significand(x) == 0.0_r8) then
            res = .true.
          else
            res = .false.
          end if

          return
        end function is_pow2


        subroutine sane_checks ()
c         Synopsis:
c         Performs sane checks.
c         Checks that the user has adhered to the design contraints on the number of
c         steps. Note that `NUM_STEPS' must be a multiple of `NUM_LOG_STEPS', for the
c         OBDS loop to execute the intended number of iterations, otherwise it loops
c         indefinitely.
          real(r8), parameter :: NUM_STEPS_R8 = real(NUM_STEPS, kind=r8)
          real(r8), parameter :: NUM_LOG_STEPS_R8 = real(NUM_LOG_STEPS,
     +    kind = r8)
          character(*), parameter :: errmsg1 = 'sane(): '//
     +    'NUM_STEPS must be an exact power of two'
          character(*), parameter :: errmsg2 = 'sane(): '//
     +    'NUM_LOG_STEPS must be an exact power of two'
          character(*), parameter :: errmsg3 = 'sane(): '//
     +    'NUM_LOG_STEPS must be less than or equal to NUM_STEPS'

          if ( .not. is_pow2(NUM_STEPS_R8) ) then
            error stop errmsg1
          end if

          if ( .not. is_pow2(NUM_LOG_STEPS_R8) ) then
            error stop errmsg2
          end if

          if (NUM_LOG_STEPS > NUM_STEPS) then
            error stop errmsg3
          end if

          return
        end subroutine sane_checks

      end module OBDS

      program OpenBDS
        use, intrinsic :: iso_fortran_env, only: i8 => int64
        use, intrinsic :: iso_fortran_env, only: r8 => real64
        use :: config, only: NUM_STEPS
        use :: config, only: NUM_LOG_STEPS
        use :: timer,  only: timer_t
        use :: sphere, only: sphere_t
        use :: OBDS, only: sane
        implicit none
c       initializes pointer to collection of spheres
        type(sphere_t), pointer :: spheres => null()
        type(timer_t) :: clock
        real(r8), pointer, contiguous :: tmp(:) => null()
c       sets the number of simulation time-steps
        integer(i8), parameter :: steps = NUM_STEPS
c       after this many steps the OBDS code logs the particle data to a plain text file
        integer(i8), parameter :: log_steps = NUM_LOG_STEPS
c       step counters
        integer(i8) :: step
        integer(i8) :: istep
c       IO status
        integer(i8) :: status

        call sane()

        clock = timer_t()
        call clock % t_start()

        spheres => sphere_t() ! instantiates the collection of spheres

c       fetches the initial step number (NOTE: `sphere_t()' handles this)
        tmp => spheres % tmp
        istep = int(tmp(1), kind = i8)

        step = istep
c       executes the OBDS loop
c       loop-invariant:
c       so far we have executed `step' OBDS simulation steps
        do while (step /= steps)

          istep = 0_i8
c         loop-invariant:
c         so far we have executed `istep' simulation steps consecutively without logging
          do while (istep /= log_steps)
c           updates the position and orientation of the particles according to the
c           Brownian and particle-particle interaction forces acting on them
            call spheres % update()
            istep = istep + 1_i8
          end do

          status = spheres % flog(step + log_steps)

          if (STATUS == __FAILURE__) then
            exit
          end if

c         halts execution if the application walltime has been reached
          call clock % t_end()

          if ( clock % t_falarm() ) then
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
