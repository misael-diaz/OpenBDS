      module timer
        use, intrinsic :: iso_fortran_env, only: i8 => int64
        use :: config, only: WALLTIME ! OBDS App WALLTIME [seconds]
        implicit none
        private

        type, public :: timer_t
          private
          integer(i8) :: resolution = 0_i8
          integer(i8) :: time_start = 0_i8
          integer(i8) :: time_end   = 0_i8
          contains
            private
            procedure :: t_fgetetime => timer_fgetetime
            procedure :: t_start_setter => timer_start_setter
            procedure :: t_end_setter => timer_end_setter
            procedure :: timer_falarm
            procedure, public :: t_start => timer_start
            procedure, public :: t_end => timer_end
            procedure, public :: t_falarm => timer_falarm
        end type

        interface timer_t
          module procedure constructor
        end interface

      contains

        function ftime () result(t)
          integer(i8) :: t

          call system_clock(count = t)

        end function ftime


        subroutine timer_start (this)
          class(timer_t), intent(inout) :: this
          integer(i8) :: time_start

          time_start = ftime()
          call this % t_start_setter(time_start)

          return
        end subroutine timer_start


        subroutine timer_end (this)
          class(timer_t), intent(inout) :: this
          integer(i8) :: time_end

          time_end = ftime()
          call this % t_end_setter(time_end)

          return
        end subroutine timer_end


        pure subroutine timer_start_setter (this, time_start)
          class(timer_t), intent(inout) :: this
          integer(i8), intent(in) :: time_start

          this % time_start = time_start

          return
        end subroutine timer_start_setter


        pure subroutine timer_end_setter (this, time_end)
          class(timer_t), intent(inout) :: this
          integer(i8), intent(in) :: time_end

          this % time_end = time_end

          return
        end subroutine timer_end_setter


        pure function timer_fgetetime (this) result(etime)
          class(timer_t), intent(in) :: this
          integer(i8) :: etime
          integer(i8) :: time_start
          integer(i8) :: time_end
          integer(i8) :: time_elapsed

          time_start = this % time_start
          time_end = this % time_end
          time_elapsed = (time_end - time_start)
          etime = time_elapsed

          return
        end function timer_fgetetime


        pure function timer_falarm (this) result(enabled)
c         Synopsis:
c         Sets the alarm if the walltime has been reached.
          class(timer_t), intent(in) :: this
          logical(i8) :: enabled
          integer(i8) :: resolution
          integer(i8) :: time_elapsed
          integer(i8) :: wtime

          time_elapsed = this % t_fgetetime()

c         converts seconds to whatever time-unit (millis, us, ns) `system_clock()' uses
          resolution = this % resolution
          wtime = resolution * WALLTIME
          if (time_elapsed >= wtime) then
            enabled = .true.
          else
            enabled = .false.
          end if

          return
        end function timer_falarm


        function constructor () result(timer)
          type(timer_t) :: timer
          integer(i8) :: system_clock__resolution

          call system_clock(count_rate = system_clock__resolution)
          timer % resolution = system_clock__resolution
          timer % time_start = 0_i8
          timer % time_end   = 0_i8

          return
        end function constructor

      end module timer

*   OpenBDS                                             November 05, 2023
*
*   source: api/fortran/module/timer/timer.f
*   author: @misael-diaz
*
*   Synopsis:
*   Implements a timer for stopping the OBDS app after it has reached the walltime.
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
