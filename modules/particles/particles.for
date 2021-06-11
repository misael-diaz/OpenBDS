!
!   project: OpenBDS
!   source:  particles.f90
!   author:  misael-diaz
!
!   Synopsis:
!   Defines the particle class.
!
!
!   Copyright (C) 2016 - 2021 Misael Diaz-Maldonado
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

module particles
    use, intrinsic :: iso_fortran_env, only: int64, real64
    implicit none


    type, abstract :: particle_t
        integer(kind = int64), allocatable :: id(:)
    end type


    type, extends(particle_t) :: sphere_t
        ! position
        real(kind = real64), allocatable :: r_x(:)
        real(kind = real64), allocatable :: r_y(:)
        real(kind = real64), allocatable :: r_z(:)

        ! force
        real(kind = real64), allocatable :: f_x(:)
        real(kind = real64), allocatable :: f_y(:)
        real(kind = real64), allocatable :: f_z(:)

        ! unbounded position
        real(kind = real64), allocatable :: ur_x(:)
        real(kind = real64), allocatable :: ur_y(:)
        real(kind = real64), allocatable :: ur_z(:)

        ! linear displacement
        real(kind = real64), allocatable :: dr_x(:)
        real(kind = real64), allocatable :: dr_y(:)
        real(kind = real64), allocatable :: dr_z(:)
    end type

    public
end module particles
