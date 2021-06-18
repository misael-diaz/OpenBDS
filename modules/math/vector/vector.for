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
            procedure :: delta_forall_method
            procedure :: delta_for_indexed_method
            procedure, public :: range => range_method
            procedure, public :: normalize => normalize_method
            generic, public :: delta2 => delta_forall_method, &
                                      & delta_for_indexed_method
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


        module elemental subroutine initialize (value)
            real(kind = real64), intent(out) :: value
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


        module subroutine delta_forall_method (self, i)
            class(vector_t), intent(inout) :: self
            integer(kind = int64), intent(in) :: i
        end subroutine

        module subroutine delta_for_indexed_method (self, i, idx)
            class(vector_t), intent(inout) :: self
            integer(kind = int64), intent(in) :: i
            integer(kind = int64), intent(in) :: idx(:)
        end subroutine



        module function range_method (self, i, j) result(d)
            class(vector_t), intent(in) :: self
            integer(kind = int64), intent(in) :: i
            integer(kind = int64), intent(in) :: j
            real(kind = real64) :: d
        end function


        module subroutine distance (v, i, j, d)
            type(vector_t), intent(in) :: v
            integer(kind = int64), intent(in) :: i
            integer(kind = int64), intent(in) :: j
            real(kind = real64), intent(out) :: d
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




! Comments:
!
! Coord(inate) type
!   type, public :: coord_t
!       real(kind = real64) :: x = 0.0_real64
!       real(kind = real64) :: y = 0.0_real64
!       real(kind = real64) :: z = 0.0_real64
!       real(kind = real64) :: v = 0.0_real64
!   end type
!
! Have been considering introducing this type however I am still
! reluctant to do so. If one were to use this type say to compute the
! difference of two vectors, one would have to copy the corresponding
! elements to initialize two :[coord_t]: objects. If one were dealing
! with small data sets one might not worry (too much) but that's not
! the case. The alternative (which is what's going to be implemented)
! is to address the elements of the vector and store the distance in
! a scalar, which might be reused as much as possible. In terms of
! memory usage this one is preferable.
!
! Why not vectorize the code that computes the difference of two vectors?
! Well, it could be done. However the difference of two vectors is most
! useful when building the neighbor-lists. One can find ways to write
! the code so that the compiler vectorizes the operation but bear in mind
! that you can only push one value at a time to the neighbor-list. Note
! that the neighbor-list is implemented as a dynamic vector which grows
! as needed, and that property might hinder vectorization. I am not an
! expert on vectorization but my intuition tells me that the compiler
! might not vectorize without suitable compiler directives. Personally,
! I prefer not to use compiler directives at all since these are bound to
! be compiler dependent and besides users might prefer a compiler other
! than the one I might choose. Ideally, writing code that performs
! similarly independent of the compiler might be something worth aiming
! for.
!
! Anyone interested in optimizing the code on this respect is welcomed.
