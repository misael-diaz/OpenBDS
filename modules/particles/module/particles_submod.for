!
!   source:  particles.f90
!   author:  misael-diaz
!   date:    2021-06-16
!
!   Synopsis:
!   Implements the particle class.
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
!

submodule (particle_class) particle_class_implementations
!   use, intrinsic :: iso_fortran_env, only: int64, real64
!   implicit none
    contains


        module subroutine stub (self)
            class(particle_t), intent(inout) :: self
            integer(kind = int32) :: mstat

            allocate (character(len=len("unknown")) :: self % shape % str, &
                    & stat = mstat)
            if (mstat /= 0) error stop "failed to allocate string"

            self % shape % str(:) = "unknown"
            return
        end subroutine


end submodule

! Comments:
! Subroutine stub () has been defined for the sole purpose of producing
! a submodule file.
