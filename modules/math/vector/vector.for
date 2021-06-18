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
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64
    use utils, only: util_allocate_array_real64_by_size
    use utils, only: util_deallocate_array_real64
    implicit none
    real(kind = real64), parameter :: vector_eps_mod = 1.0e-12_real64
    private
    save


    type :: size_t
        integer(kind = int64) :: n = 0_int64
    end type


    type :: stat_t
        logical(kind = int64) :: init = .false.
    end type


    type, public :: vector_t
        type(stat_t), allocatable :: stat
        type(size_t), allocatable :: size 
        real(kind = real64), allocatable :: x(:)
        real(kind = real64), allocatable :: y(:)
        real(kind = real64), allocatable :: z(:)
        real(kind = real64), allocatable :: v(:)        ! magnitude
        contains
            private
            procedure, public :: normalize => normalize_method
            final :: finalizer
    end type


    interface vector_t
        module procedure constructor
    end interface


    interface allocator
        module procedure allocate_stat_t
        module procedure allocate_size_t
        module procedure util_allocate_array_real64_by_size
    end interface


    interface deallocator
        module procedure deallocate_stat_t
        module procedure deallocate_size_t
        module procedure util_deallocate_array_real64
    end interface


    interface


        module function constructor (n) result(vector)
            type(vector_t) :: vector
            integer(kind = int64), intent(in) :: n
        end function


        module subroutine initializer (vector, n)
            type(vector_t), intent(inout) :: vector
            integer(kind = int64), intent(in) :: n
        end subroutine


        module subroutine normalize_method (self)
            class(vector_t), intent(inout) :: self
        end subroutine


        module subroutine normalizer (vector)
            type(vector_t), intent(inout) :: vector
        end subroutine


        module subroutine vector_guard_singular (vector)
            type(vector_t), intent(in) :: vector
        end subroutine


        module elemental subroutine normalize (x, y, z, t, v)
            real(kind = real64), intent(inout) :: x
            real(kind = real64), intent(inout) :: y
            real(kind = real64), intent(inout) :: z
            real(kind = real64), intent(inout) :: t
            real(kind = real64), intent(in)    :: v
        end subroutine


        module subroutine moduli (vector)
            type(vector_t), intent(inout) :: vector
        end subroutine


        module elemental subroutine modulus (x, y, z, v)
            real(kind = real64), intent(in)    :: x
            real(kind = real64), intent(in)    :: y
            real(kind = real64), intent(in)    :: z
            real(kind = real64), intent(inout) :: v
        end subroutine


        module subroutine finalizer (vector)
            ! Synopsis: Frees the memory allocated for the vector.
            type(vector_t), intent(inout) :: vector
        end subroutine


        module subroutine allocate_stat_t (s)
            type(stat_t), intent(inout), allocatable :: s
        end subroutine


        module subroutine allocate_size_t (s)
            type(size_t), intent(inout), allocatable :: s
        end subroutine


        module subroutine deallocate_stat_t (s)
            type(stat_t), intent(inout), allocatable :: s
        end subroutine


        module subroutine deallocate_size_t (s)
            type(size_t), intent(inout), allocatable :: s
        end subroutine


        module subroutine destructor (vector)
            ! Synopsis: Destroys the components of the vector.
            type(vector_t), intent(inout) :: vector
        end subroutine


    end interface


end module
