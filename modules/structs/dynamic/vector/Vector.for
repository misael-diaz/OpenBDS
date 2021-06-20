!
!   source: Vector.for
!   author: misael-diaz
!   date:   2021-06-10
!
!
!   Synopsis:
!   Defines the (dynamic) vector class.
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

module idata
    ! Defines the type i[nternal]data of the vector class
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use utils, only: deallocator => util_deallocate_array
    implicit none
    private


    type, public :: data_t
        integer(kind = int64), allocatable :: values_int64_t(:)
        integer(kind = int32), allocatable :: values_int32_t(:)
        contains
            private
            final :: finalizer
    end type


    contains

        subroutine finalizer (data)
            type(data_t), intent(inout) :: data

!           print *, "destroying dynamic vector data ... "

            call deallocator (data % values_int64_t)
            call deallocator (data % values_int32_t)

            return
        end subroutine

end module idata


module vectors
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use utils, only: util_allocate_array_int32_by_bounds
    use utils, only: util_allocate_array_int64_by_bounds
    use utils, only: reallocator => util_reallocate_array
    use utils, only: util_deallocate_array_int32
    use utils, only: util_deallocate_array_int64
    use idata, only: data_t
    implicit none
    private


    type :: iter_t
        integer(kind = int64) :: idx = 0_int64
    end type


    type :: stat_t
        logical(kind = int64) :: init = .false.
    end type


    type, public :: vector_t
        private
        type(iter_t), allocatable :: begin
        type(iter_t), allocatable :: avail
        type(iter_t), allocatable :: limit
        type(data_t), allocatable :: array
        type(stat_t), allocatable :: state
        contains
            private
            procedure :: addressing_method
            generic, public :: operator (<) => addressing_method
            procedure, public :: size => size_method
            procedure, public :: clear => clear_method
            procedure, public :: findloc => findloc_method
            procedure, public :: push_back => push_back_method
            final :: finalizer
    end type


    interface vector_t
        module procedure default_constructor
    end interface


    interface allocator
        module procedure allocate_iter_t
        module procedure allocate_data_t
        module procedure allocate_stat_t
        module procedure allocate_vector_t
        module procedure util_allocate_array_int32_by_bounds
        module procedure util_allocate_array_int64_by_bounds
    end interface


    interface deallocator
        module procedure deallocate_iter_t
        module procedure deallocate_data_t
        module procedure deallocate_stat_t
        module procedure util_deallocate_array_int32
        module procedure util_deallocate_array_int64
    end interface


    interface


        module function default_constructor () result(vector)
            ! Synopsis: Returns an empty vector
            type(vector_t):: vector
        end function


        module subroutine instantiate (vector)
            type(vector_t), intent(inout) :: vector
        end subroutine


        module function findloc_method (self, value) result(idx)
            class(vector_t), intent(in) :: self
            integer(kind = int64) :: idx
            integer(kind = int32), intent(in) :: value
        end function


        module subroutine findloc_wrapper (vector, value, idx)
            type(vector_t), intent(in) :: vector
            integer(kind = int64), intent(out) :: idx
            integer(kind = int32), intent(in) :: value
        end subroutine


        module function addressing_method (self, idx) result(value)
            ! Synopsis: Addresses the element pointed to by index.
            class(vector_t), intent(in) :: self
            integer(kind = int64), intent(in) :: idx
            integer(kind = int32) :: value
        end function


        module function size_method (self) result(vector_size)
            ! Synopsis: Returns the size of the vector.
            class(vector_t), intent(in) :: self
            integer(kind = int64) :: vector_size
        end function


        module subroutine clear_method (self)
            ! Synopsis: Clears the vector elements.
            class(vector_t), intent(inout) :: self
        end subroutine


        module subroutine push_back_method (self, value)
            ! Synopsis: Pushes value unto back of vector.
            class(vector_t), intent(inout) :: self
            integer(kind = int32), intent(in) :: value
        end subroutine


        module subroutine insert_back (vector, value)
            ! Synopsis: Inserts value unto back, vector grows as needed.
            type(vector_t), intent(inout) :: vector
            integer(kind = int32), intent(in) :: value
        end subroutine


        module subroutine grow (vector)
            ! Synopsis: Doubles the vector size.
            type(vector_t), intent(inout) :: vector
        end subroutine


        module subroutine initializer (vector, value)
            type(vector_t), intent(inout) :: vector
            integer(kind = int32), intent(in) :: value
        end subroutine


        module subroutine create (vector, value)
            ! Synopsis: Creates the first element in vector.
            type(vector_t), intent(inout) :: vector
            integer(kind = int32), intent(in) :: value
        end subroutine


        module subroutine allocate_vector_t (v)
            type(vector_t), intent(inout) :: v
        end subroutine


        module subroutine allocate_iter_t (i)
            type(iter_t), intent(inout), allocatable :: i
        end subroutine


        module subroutine allocate_data_t (d)
            type(data_t), intent(inout), allocatable :: d
        end subroutine


        module subroutine allocate_stat_t (s)
            type(stat_t), intent(inout), allocatable :: s
        end subroutine


        module subroutine deallocate_iter_t (i)
            type(iter_t), intent(inout), allocatable :: i
        end subroutine


        module subroutine deallocate_data_t (d)
            type(data_t), intent(inout), allocatable :: d
        end subroutine


        module subroutine deallocate_stat_t (s)
            type(stat_t), intent(inout), allocatable :: s
        end subroutine


        module subroutine is_empty (vector)
            type(vector_t), intent(in) :: vector
        end subroutine


        module subroutine is_instantiated (vector)
            type(vector_t), intent(inout) :: vector
        end subroutine

        module subroutine check_bounds (vector, idx)
            type(vector_t), intent(in) :: vector
            integer(kind = int64), intent(in) :: idx
        end subroutine


        module subroutine finalizer (vector)
            type(vector_t), intent(inout) :: vector
        end subroutine


    end interface


end module


! References:
! SJ Chapman, FORTRAN for Scientists and Engineers, fourth edition
! A Koenig and B Moo, Accelerated C++ Practical Programming by Example


! Comments:
! Easiest way to get at the values in vector is by having direct
! access. However, that's a dangerous road that I would only use
! in the absence of suitable alternatives.


! TODO:
! [ ] make the components of :[vector_t]: allocatable
! [x] Implement a wrapper for the findloc intrinsic. Full support is
!     not intended. Just for the case in which array, value, and dim
!     are supplied (the result is a scalar).
!     This method will be used for testing the minimalistic neighbor-list
!     implemented in test/modules/math/vector.
