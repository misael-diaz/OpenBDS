!
!   source: chronos.for
!   author: misael-diaz
!   date:   2021-05-31
!
!
!   Synopsis:
!   Defines timer classes.
!
!
!   Copyright (C) 2021 Misael Diaz-Maldonado
!
!   This program is free software: you can redistribute it and/or modify
!   it under the terms of the GNU General Public License as published by
!   the Free Software Foundation, either version 3 of the License, or
!   (at your option) any later version.
!
!   This program is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   GNU General Public License for more details.
!
!   You should have received a copy of the GNU General Public License
!   along with this program.  If not, see <http://www.gnu.org/licenses/>.

module chronos
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64
    implicit none

    type, public :: chronom
        private
        integer(kind = int64):: b               ! begin time
        integer(kind = int64):: e               ! end time
        real(kind = real64):: lapse             ! elapsed-time
        real(kind = real64):: period            ! clock period
        contains
            procedure, public :: tic => start_method
            procedure, public :: toc => stop_method
            procedure, public :: etime => elapsed_time_method
    end type

    interface chronom
        module procedure constructor
    end interface

    private
    contains
        function constructor() result(chron)
            type(chronom):: chron
            
            call clockspecs(chron)
            
            return
        end function

        subroutine clockspecs(chronometer)
            ! Synopsis:
            ! Queries the system clock to set the period of the chronometer
            ! and assigns default-values to its data members (or fields).

            class(chronom):: chronometer
            real(kind = real64):: clock_period
            integer(kind = int64):: clock_rate


            call system_clock(count_rate = clock_rate)
            clock_period = 1.0_real64 / real(clock_rate, kind = real64)


            chronometer % b      = 0_int64
            chronometer % e      = 0_int64
            chronometer % lapse  = 0.0_real64
            chronometer % period = clock_period

            return
        end subroutine clockspecs

        subroutine start_method(this)
            ! Synopsis:
            ! Starts the chronometer.

            class(chronom), target :: this
            integer(kind = int64), pointer :: p_beg

            p_beg  => this % b
            call system_clock(p_beg)

            return
        end subroutine

        subroutine stop_method(this)
            ! Synopsis:
            ! Stops the chronometer.

            class(chronom), target :: this
            integer(kind = int64), pointer :: p_end

            p_end => this % e
            call system_clock(p_end)

            return
        end subroutine

        function elapsed_time_method(this) result(etime)
            ! Synopsis:
            ! Returns the elapsed-time in milliseconds.

            class(chronom), target :: this
            
            integer(kind = int64), pointer :: p_beg
            integer(kind = int64), pointer :: p_end

            real(kind = real64):: etime
            real(kind = real64), pointer :: p_etime
            real(kind = real64), pointer :: p_period


            p_beg    => this % b
            p_end    => this % e
            p_etime  => this % lapse
            p_period => this % period


            p_etime = real( (p_end - p_beg), kind = real64 ) * p_period
            p_etime = 1.0e+3_real64 * p_etime
            etime   = p_etime

            return
        end function
end module
