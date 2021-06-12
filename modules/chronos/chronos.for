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
    use, intrinsic :: iso_fortran_env, only: int64, real64
    implicit none
    private

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


    interface
        module function constructor () result(chron)
            type(chronom):: chron
        end function


        module subroutine clockspecs (chronometer)
            type(chronom), intent(inout) :: chronometer
        end subroutine


        module subroutine start_method (self)
            class(chronom), intent(inout), target :: self
        end subroutine


        module subroutine stop_method (self)
            class(chronom), intent(inout), target :: self
        end subroutine


        module function elapsed_time_method (self) result(etime)
            class(chronom), intent(inout), target :: self
            real(kind = real64):: etime
        end function
    end interface


    interface chronom
        module procedure constructor
    end interface


end module chronos
