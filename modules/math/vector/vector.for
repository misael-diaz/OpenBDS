!
!   source:  vector.f90
!   author:  misael-diaz
!   date:    2021-06-17
!
!   Synopsis:
!   Defines the math vector class.
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

module math_vector_class
    use, intrinsic :: iso_fortran_env, only: real64
    use utils, only: allocator   => util_allocate_array_real64_by_size
    use utils, only: deallocator => util_deallocate_array_real64
    implicit none
    private


    type, public :: vector_t
        real(kind = real64), allocatable :: x(:)
        real(kind = real64), allocatable :: y(:)
        real(kind = real64), allocatable :: z(:)
        contains
            private
            final :: finalizer
    end type


    interface


        module subroutine finalizer (vector)
            ! Synopsis: Frees the memory allocated for the vector.
            type(vector_t), intent(inout) :: vector
        end subroutine



        module subroutine destructor (vector)
            ! Synopsis: Destroys the components of the vector.
            type(vector_t), intent(inout) :: vector
        end subroutine


    end interface


end module
