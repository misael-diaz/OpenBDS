!
!   source: system.for
!   author: misael-diaz
!   date:   2021-06-14
!
!
!   Synopsis:
!   Defines system classes (unary_t, binary_t, etc.).
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

module system
    use, intrinsic :: iso_fortran_env, only: int64, real64
    implicit none
    private


    type, public :: unary_t
        private
        type(sphere_t), allocatable :: sphere
!       contains
!           procedure, public :: spawn => spawn_method
!           procedure, public :: overlap => remove_overlaps_method
!           procedure, public :: logger => logger_method
    end type



!   type, public :: binary_t
!       private
!       type(sphere_t), allocatable :: sphere
!       type(axisym_t), allocatable :: ellipsoid
!       contains
!           procedure, public :: interact => interaction_sphere_ellipsoid
!   end type


! interfaces go here



end module system


! Comments:
! I do not intend to compile this code. I am using it for developing my
! ideas.


! Code show how unary_t and binary_t types might look like. The main
! idea is that the system_t has full access to the particle types
! and it's responsible for hiding them from the user.


! For unary systems the type-bound procedures may be implemented as
! wrappers the actual method being defined by the particle class itself.


! For binary systems it might be necessary to define methods at this
! level where one has access to the data members of both particle types.
! For this to work


! user-defined methods:
! I have yet to think about how the users might want to use this code.
! Would they like to define the interaction routines externally or
! as part of the module. Usually one does not want to modify the
! library or facility but to have an interface to the functions
! it provides for customization. Think about the standard template
! library of c++. It provides a generic sorting method that works
! for user-defined types provided that the user defines a suitable
! predicate functions that tell how to compare such times. You may
! want to enable users in a similar way. That should be more practical
! than having to understand the implementation of this application.
