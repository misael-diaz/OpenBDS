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
    implicit none
    private


    type, public :: data_t
        class(*), allocatable :: values(:)
        contains
            private
            final :: finalizer
    end type


    contains


        subroutine finalizer (data)
            type(data_t), intent(inout) :: data
            integer(kind = int32) :: mstat

!           print *, "destroying dynamic vector data ... "

            mstat = 0
            if ( allocated(data % values) ) then
                deallocate (data % values, stat = mstat)
            end if

            if (mstat /= 0) error stop "dynamic::vector.data: dealloc err"

            return
        end subroutine


end module idata


module vectors
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use utils, only: util_allocate_array_int32_by_bounds
    use utils, only: util_allocate_array_int64_by_bounds
    use utils, only: util_reallocate_array_int32_by_bounds
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
        character(:), allocatable :: errmsg
        contains
            final :: destructor_stat_t
    end type


    type, public :: vector_t
        private
        type(data_t), allocatable :: array
        type(iter_t), allocatable :: begin
        type(iter_t), allocatable :: avail
        type(iter_t), allocatable :: limit
        type(stat_t), allocatable :: state
        contains
            private
            procedure :: vector_int32_t_indexing_method
            procedure :: vector_int64_t_indexing_method
            procedure :: vector_int32_t_find_method
            procedure :: vector_int64_t_find_method
            procedure :: vector_int32_t_iterator_method
            procedure :: vector_int64_t_iterator_method
            procedure :: vector_int32_t_push_back_method
            procedure :: vector_int64_t_push_back_method
            generic, public :: get  => vector_int32_t_indexing_method, &
                                     & vector_int64_t_indexing_method
            generic, public :: iter => vector_int32_t_iterator_method, &
                                     & vector_int64_t_iterator_method
            generic, public :: find => vector_int32_t_find_method, &
                                     & vector_int64_t_find_method
            generic, public :: push_back => &
                                     & vector_int32_t_push_back_method, &
                                     & vector_int64_t_push_back_method
            procedure, public :: size => size_method
            procedure, public :: clear => clear_method
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
        module procedure vector_int32_t_allocate_dynamic
        module procedure vector_int64_t_allocate_dynamic
        module procedure util_allocate_array_int32_by_bounds
        module procedure util_allocate_array_int64_by_bounds
    end interface


    interface reallocator
        module procedure vector_int32_t_allocate_dynamic
        module procedure vector_int64_t_allocate_dynamic
        module procedure util_reallocate_array_int32_by_bounds
    end interface


    interface deallocator
        module procedure deallocate_iter_t
        module procedure deallocate_data_t
        module procedure deallocate_stat_t
        module procedure vector_int32_t_deallocate_dynamic
        module procedure vector_int64_t_deallocate_dynamic
        module procedure util_deallocate_array_int32
        module procedure util_deallocate_array_int64
    end interface


    interface initializer
        module procedure vector_int32_t_initializer
        module procedure vector_int64_t_initializer
    end interface


    interface create
        module procedure vector_int32_t_create
        module procedure vector_int64_t_create
    end interface


    interface back_inserter
        module procedure vector_int32_t_push_back
        module procedure vector_int64_t_push_back
    end interface


    interface insert_back
        module procedure vector_int32_t_insert_back
        module procedure vector_int64_t_insert_back
    end interface


    interface indexer
        module procedure vector_int32_t_indexer
        module procedure vector_int64_t_indexer
    end interface


    interface find
        module procedure vector_int32_t_findloc_wrapper
        module procedure vector_int64_t_findloc_wrapper
    end interface


    interface slice
        module procedure vector_int32_t_slice
        module procedure vector_int64_t_slice
    end interface


    interface grow
        module procedure vector_int32_t_grow
        module procedure vector_int64_t_grow
    end interface


    interface copy
        module procedure array_int32_t_copy
        module procedure array_int64_t_copy
    end interface


!   interface to_string
!       module procedure to_string_int32
!       module procedure to_string_int64
!   end interface


    interface


        module function default_constructor () result(vector)
            ! Synopsis: Returns an empty vector
            type(vector_t):: vector
        end function


        module subroutine instantiate (vector)
            type(vector_t), intent(inout) :: vector
        end subroutine


        module function vector_int32_t_find_method (self, value) result(i)
            class(vector_t), intent(in) :: self
            integer(kind = int64) :: i
            integer(kind = int32), intent(in) :: value
        end function


        module function vector_int64_t_find_method (self, value) result(i)
            class(vector_t), intent(in) :: self
            integer(kind = int64) :: i
            integer(kind = int64), intent(in) :: value
        end function


        module subroutine vector_int32_t_iterator_method (self, it)
            class(vector_t), intent(in) :: self
            integer(kind = int32), intent(inout), &
                & pointer, contiguous :: it(:)
        end subroutine


        module subroutine vector_int64_t_iterator_method (self, it)
            class(vector_t), intent(in) :: self
            integer(kind = int64), intent(inout), &
                & pointer, contiguous :: it(:)
        end subroutine


        module subroutine vector_int32_t_slice (vector, it)
            type(vector_t), intent(in), target :: vector
            integer(kind = int32), intent(inout), &
                & pointer, contiguous :: it(:)
        end subroutine


        module subroutine vector_int64_t_slice (vector, it)
            type(vector_t), intent(in), target :: vector
            integer(kind = int64), intent(inout), &
                & pointer, contiguous :: it(:)
        end subroutine


        module subroutine vector_int32_t_findloc_wrapper (vector, value, i)
            type(vector_t), intent(in) :: vector
            integer(kind = int64), intent(out) :: i
            integer(kind = int32), intent(in) :: value
        end subroutine


        module subroutine vector_int64_t_findloc_wrapper (vector, value, i)
            type(vector_t), intent(in) :: vector
            integer(kind = int64), intent(out) :: i
            integer(kind = int64), intent(in) :: value
        end subroutine


        module subroutine vector_int32_t_indexing_method (self, idx, value)
            ! Synopsis: Addresses the element pointed to by index.
            class(vector_t), intent(in) :: self
            integer(kind = int64), intent(in) :: idx
            integer(kind = int32), intent(out) :: value
        end subroutine


        module subroutine vector_int64_t_indexing_method (self, idx, value)
            ! Synopsis: Addresses the element pointed to by index.
            class(vector_t), intent(in) :: self
            integer(kind = int64), intent(in) :: idx
            integer(kind = int64), intent(out) :: value
        end subroutine


        module subroutine vector_int32_t_indexer (vector, idx, value)
            type(vector_t), intent(in) :: vector
            integer(kind = int64), intent(in) :: idx
            integer(kind = int32), intent(out) :: value
        end subroutine


        module subroutine vector_int64_t_indexer (vector, idx, value)
            type(vector_t), intent(in) :: vector
            integer(kind = int64), intent(in) :: idx
            integer(kind = int64), intent(out) :: value
        end subroutine


        module function size_method (self) result(vector_size)
            ! Synopsis: Returns the size of the vector.
            class(vector_t), intent(in) :: self
            integer(kind = int64) :: vector_size
        end function


        module subroutine clear_method (self)
            ! Synopsis: Clears the vector elements.
            class(vector_t), intent(inout) :: self
        end subroutine


        module subroutine vector_int32_t_push_back_method (self, value)
            class(vector_t), intent(inout) :: self
            integer(kind = int32), intent(in) :: value
        end subroutine


        module subroutine vector_int64_t_push_back_method (self, value)
            class(vector_t), intent(inout) :: self
            integer(kind = int64), intent(in) :: value
        end subroutine


        module subroutine vector_int32_t_push_back (vector, value)
            type(vector_t), intent(inout) :: vector
            integer(kind = int32), intent(in) :: value
        end subroutine


        module subroutine vector_int64_t_push_back (vector, value)
            type(vector_t), intent(inout) :: vector
            integer(kind = int64), intent(in) :: value
        end subroutine


        module subroutine vector_int32_t_insert_back (vector, value)
            ! Synopsis: Inserts value unto back, vector grows as needed.
            type(vector_t), intent(inout) :: vector
            integer(kind = int32), intent(in) :: value
        end subroutine


        module subroutine vector_int64_t_insert_back (vector, value)
            ! Synopsis: Inserts value unto back, vector grows as needed.
            type(vector_t), intent(inout) :: vector
            integer(kind = int64), intent(in) :: value
        end subroutine


        module subroutine vector_int32_t_grow (vector, value)
            ! Synopsis: Doubles the vector size.
            type(vector_t), intent(inout), target :: vector
            integer(kind = int32), intent(in) :: value
        end subroutine


        module subroutine vector_int64_t_grow (vector, value)
            ! Synopsis: Doubles the vector size.
            type(vector_t), intent(inout), target :: vector
            integer(kind = int64), intent(in) :: value
        end subroutine


        module elemental subroutine array_int32_t_copy (dst, src)
            integer(kind = int32), intent(inout) :: dst
            integer(kind = int32), intent(in) :: src
        end subroutine


        module elemental subroutine array_int64_t_copy (dst, src)
            integer(kind = int64), intent(inout) :: dst
            integer(kind = int64), intent(in) :: src
        end subroutine


        module subroutine vector_int32_t_initializer (vector, value)
            type(vector_t), intent(inout) :: vector
            integer(kind = int32), intent(in) :: value
        end subroutine


        module subroutine vector_int64_t_initializer (vector, value)
            type(vector_t), intent(inout) :: vector
            integer(kind = int64), intent(in) :: value
        end subroutine


        module subroutine vector_int32_t_create (vector, value)
            ! Synopsis: Creates the first element in vector.
            type(vector_t), intent(inout) :: vector
            integer(kind = int32), intent(in) :: value
        end subroutine


        module subroutine vector_int64_t_create (vector, value)
            ! Synopsis: Creates the first element in vector.
            type(vector_t), intent(inout) :: vector
            integer(kind = int64), intent(in) :: value
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


        module subroutine vector_int32_t_allocate_dynamic (b, array, value)
            integer(kind = int64), intent(in) :: b(0:1)
            class(*), intent(inout), allocatable :: array(:)
            integer(kind = int32), intent(in) :: value
        end subroutine


        module subroutine vector_int64_t_allocate_dynamic (b, array, value)
            integer(kind = int64), intent(in) :: b(0:1)
            class(*), intent(inout), allocatable :: array(:)
            integer(kind = int64), intent(in) :: value
        end subroutine


        module subroutine deallocate_data_t (d)
            type(data_t), intent(inout), allocatable :: d
        end subroutine


        module subroutine deallocate_stat_t (s)
            type(stat_t), intent(inout), allocatable :: s
        end subroutine


        module subroutine vector_int32_t_deallocate_dynamic (array, value)
            class(*), intent(inout), allocatable :: array(:)
            integer(kind = int32), intent(in) :: value
        end subroutine


        module subroutine vector_int64_t_deallocate_dynamic (array, value)
            class(*), intent(inout), allocatable :: array(:)
            integer(kind = int64), intent(in) :: value
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


!       module function to_string_int32 (i) result(str)
!           integer(kind = int32), intent(in) :: i
!           character(len = 64) :: str
!       end function


!       module function to_string_int64 (i) result(str)
!           integer(kind = int64), intent(in) :: i
!           character(len = 64) :: str
!       end function


        module subroutine destructor_stat_t (s)
            type(stat_t), intent(inout) :: s
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
! in the absence of suitable alternatives. Iterator to the stored
! data solve this.
! Resolution:
! In c++ a user may change the values in the
! vector via an iterator so we are just providing an analogous
! functionality.


! TODO:
! [x] make the components of :[vector_t]: allocatable
! [x] Implement a wrapper for the findloc intrinsic. Full support is
!     not intended. Just for the case in which array, value, and dim
!     are supplied (the result is a scalar).
!     This method will be used for testing the minimalistic neighbor-list
!     implemented in test/modules/math/vector.
