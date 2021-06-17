!
!   project: OpenBDS
!   source:  particles.f90
!   author:  misael-diaz
!
!   Synopsis:
!   Defines the particle class.
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

module particle_class
    use, intrinsic :: iso_fortran_env, only: int64, real64
    implicit none
    private


    type :: string_t
        character (:), allocatable :: str
    end type


    type :: vector_t
        real(kind = real64), allocatable :: x(:)
        real(kind = real64), allocatable :: y(:)
        real(kind = real64), allocatable :: z(:)
    end type


    type, abstract, public :: particle_t
        type(string_t) :: shape
        type(vector_t), allocatable :: position
        integer(kind = int64), allocatable :: id(:)
!       contains
!           private
!           procedure(i_init), deferred :: initializer
    end type


!   abstract interface
!       subroutine i_init (self, n)
!           use, intrinsic :: iso_fortran_env, only: int64
!           import particle_t
!           implicit none
!           class(particle_t), intent(inout) :: self
!           integer(kind = int64), intent(in) :: n
!       end subroutine
!   end interface

    interface


        module subroutine stub (self)
            class(particle_t), intent(inout) :: self
        end subroutine


    end interface


end module
