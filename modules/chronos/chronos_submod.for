!
!   source: chronos.for
!   author: misael-diaz
!   date:   2021-05-31
!
!
!   Synopsis:
!   Implements the procedures of the timer classes.
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

submodule (chronos) chronos_implementations
    use, intrinsic :: iso_fortran_env, only: int64, real64
    implicit none
    contains


        module function constructor () result(chron)
            type(chronom):: chron
            
            call clockspecs(chron)
            
            return
        end function


        module subroutine clockspecs (chronometer)
            ! Synopsis:
            ! Queries the system clock to set the period of the chronometer
            ! and assigns default-values to its data members (or fields).

            type(chronom), intent(inout) :: chronometer
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


        module subroutine start_method (self)
            ! Synopsis:
            ! Starts the chronometer.

            class(chronom), intent(inout), target :: self
            integer(kind = int64), pointer :: p_beg

            p_beg  => self % b
            call system_clock(p_beg)

            return
        end subroutine


        module subroutine stop_method (self)
            ! Synopsis:
            ! Stops the chronometer.

            class(chronom), intent(inout), target :: self
            integer(kind = int64), pointer :: p_end

            p_end => self % e
            call system_clock(p_end)

            return
        end subroutine


        module function elapsed_time_method (self) result(etime)
            ! Synopsis:
            ! Returns the elapsed-time in milliseconds.

            class(chronom), intent(inout) :: self
            real(kind = real64):: etime

            associate (begin => self % b,        end => self % e, &
                     & lapse => self % lapse, period => self % period)
                lapse = real( (end - begin), kind = real64 ) * period
                lapse = 1.0e+3_real64 * lapse
                etime = lapse
            end associate

            return
        end function
end submodule
