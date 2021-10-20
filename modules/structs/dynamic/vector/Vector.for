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

module bits
use, intrinsic :: iso_fortran_env, only: int32, int64
implicit none
private
save
public :: BITS_MAX_BIT
public :: imsb

integer(kind = int32), parameter :: BITS_MAX_BIT = 63

contains

  pure function imsb (n) result(msb)
      ! Synopsis:
      ! Finds the Most Significant Bit MSB of a 64-bit (signed) integer.
      integer(kind = int64), intent(in) :: n    !! 64-bit integer
      integer(kind = int32) :: pos              !! position
      integer(kind = int32) :: msb              !! most significant bit

      msb = 0
      do pos = 0, 63
          if ( btest(n, pos) ) then
              msb = pos
          end if
      end do

      return
  end function

end module bits


module idata
  ! Defines the type i[nternal]data of the vector class
  use, intrinsic :: iso_fortran_env, only: int32, int64
  implicit none
  private


  type, public :: data_t
      class(*), allocatable :: values(:)
      contains
        private
!       procedure :: assign
!       generic, public :: assignment(=) => assign
        final :: finalizer
  end type


  contains


!   subroutine assign (to, from)
!       class(data_t), intent(out) :: to
!       class(data_t), intent(in) :: from
!
!       if ( allocated(from % values) ) then
!           error stop "dynamic::vector.assignment(=): unimplemented"
!       end if
!
!       return
!   end subroutine


    recursive subroutine finalizer (data)
        type(data_t), intent(inout) :: data
        integer(kind = int32) :: mstat

        mstat = 0
        if ( allocated(data % values) ) then
            deallocate (data % values, stat = mstat)
        end if

        if (mstat /= 0) error stop "dynamic::vector.data: dealloc err"

        return
    end subroutine


end module idata


module VectorClass
  use, intrinsic :: iso_fortran_env, only: int32, int64, real64
  use utils, only: util_allocate_array_int32_by_bounds
  use utils, only: util_allocate_array_int64_by_bounds
  use utils, only: util_allocate_array_real64_by_bounds
  use utils, only: util_reallocate_array_int32_by_bounds
  use utils, only: util_deallocate_array_int32
  use utils, only: util_deallocate_array_int64
  use bits,  only: BITS_MAX_BIT
  use bits,  only: imsb
  use idata, only: data_t
  implicit none
  private

  integer(kind = int64), parameter :: VECTOR_MIN_SIZE = 8_int64

  type :: iter_t
      class(*), pointer, contiguous :: it(:) => null()
      integer(kind = int64) :: idx = 0_int64
      contains
        final :: destructor_iter_t
  end type


  type :: stat_t
      logical(kind = int64) :: init = .false.
      character(:), allocatable :: errmsg
      contains
        final :: destructor_stat_t
  end type


  type, public :: vector_t
      private
      type(iter_t), allocatable, public :: deref
      type(iter_t), allocatable :: begin
      type(iter_t), allocatable :: avail
      type(iter_t), allocatable :: limit
      type(data_t), allocatable :: array
      type(stat_t), allocatable :: state
      contains
        private
        procedure :: vector_vector_t_copy_method
        procedure :: vector_int32_t_indexing_method
        procedure :: vector_int64_t_indexing_method
        procedure :: vector_real64_t_indexing_method
        procedure :: vector_vector_t_indexing_method
        procedure :: vector_int32_t_find_method
        procedure :: vector_int64_t_find_method
        procedure :: vector_int32_t_push_back_method
        procedure :: vector_int64_t_push_back_method
        procedure :: vector_real64_t_push_back_method
        procedure :: vector_vector_t_push_back_method
        procedure :: vector_int32_t_push_back_array_method
        procedure :: vector_int64_t_push_back_array_method
        procedure :: vector_real64_t_push_back_array_method
        procedure :: vector_vector_t_push_back_array_method
!       procedure :: vector_int32_t_erase_method
        generic, public :: assignment(=) => vector_vector_t_copy_method
        generic, public :: get  => vector_int32_t_indexing_method, &
                                 & vector_int64_t_indexing_method, &
                                 & vector_real64_t_indexing_method,&
                                 & vector_vector_t_indexing_method
        generic, public :: find => vector_int32_t_find_method, &
                                 & vector_int64_t_find_method
!       generic, public :: erase => vector_int32_t_erase_method
!       procedure, public :: erase => vector_int32_t_erase_method
        generic, public :: push_back => vector_int32_t_push_back_method, &
                              & vector_int64_t_push_back_method, &
                              & vector_real64_t_push_back_method,&
                              & vector_vector_t_push_back_method,&
                              & vector_int32_t_push_back_array_method, &
                              & vector_int64_t_push_back_array_method, &
                              & vector_real64_t_push_back_array_method,&
                              & vector_vector_t_push_back_array_method
        procedure, public :: size => size_method
        procedure, public :: clear => clear_method
        procedure, public :: addr => vector_print_container_address_method
        procedure, public :: valid => vector_validate_iterator_method
        final :: finalizer
  end type


  interface vector_t
      module procedure default_constructor
      module procedure vector_int32_t_rangeConstructor
      module procedure vector_int32_t_arrayConstructor
      module procedure vector_int64_t_arrayConstructor
      module procedure vector_real64_t_arrayConstructor
      module procedure vector_vector_t_arrayConstructor
      module procedure vector_int32_t_fillConstructor
      module procedure vector_int64_t_fillConstructor
      module procedure vector_real64_t_fillConstructor
      module procedure vector_vector_t_fillConstructor
  end interface


  interface allocator
      module procedure allocate_iter_t
      module procedure allocate_data_t
      module procedure allocate_stat_t
      module procedure allocate_vector_t
      module procedure vector_allocate_errmsg
      module procedure vector_allocate_array_vector_t
      module procedure vector_int32_t_allocate_dynamic
      module procedure vector_int64_t_allocate_dynamic
      module procedure vector_real64_t_allocate_dynamic
      module procedure vector_vector_t_allocate_dynamic
      module procedure util_allocate_array_int32_by_bounds
      module procedure util_allocate_array_int64_by_bounds
      module procedure util_allocate_array_real64_by_bounds
  end interface


  interface reallocator
      module procedure vector_allocate_array_vector_t
      module procedure vector_int32_t_allocate_dynamic
      module procedure vector_int64_t_allocate_dynamic
      module procedure vector_real64_t_allocate_dynamic
      module procedure vector_vector_t_allocate_dynamic
      module procedure util_reallocate_array_int32_by_bounds
  end interface


  interface deallocator
      module procedure deallocate_iter_t
      module procedure deallocate_data_t
      module procedure deallocate_stat_t
      module procedure vector_int32_t_deallocate_dynamic
      module procedure vector_int64_t_deallocate_dynamic
      module procedure vector_vector_t_deallocate_dynamic
      module procedure util_deallocate_array_int32
      module procedure util_deallocate_array_int64
  end interface


  interface initializer
      module procedure vector_int32_t_initializer
      module procedure vector_int64_t_initializer
      module procedure vector_real64_t_initializer
      module procedure vector_vector_t_initializer
      module procedure vector_int32_t_initializerArray
      module procedure vector_int64_t_initializerArray
      module procedure vector_real64_t_initializerArray
      module procedure vector_vector_t_initializerArray
  end interface


  interface create
      module procedure vector_int32_t_create
      module procedure vector_int64_t_create
      module procedure vector_real64_t_create
      module procedure vector_vector_t_create
      module procedure vector_int32_t_createArray
      module procedure vector_int64_t_createArray
      module procedure vector_real64_t_createArray
      module procedure vector_vector_t_createArray
  end interface


  interface back_inserter
      module procedure vector_int32_t_push_back
      module procedure vector_int64_t_push_back
      module procedure vector_real64_t_push_back
      module procedure vector_vector_t_push_back
      module procedure vector_int32_t_push_back_array
      module procedure vector_int64_t_push_back_array
      module procedure vector_real64_t_push_back_array
      module procedure vector_vector_t_push_back_array
  end interface


  interface insert_back
      module procedure vector_int32_t_insert_back
      module procedure vector_int64_t_insert_back
      module procedure vector_real64_t_insert_back
      module procedure vector_vector_t_insert_back
      module procedure vector_int32_t_insert_back_array
      module procedure vector_int64_t_insert_back_array
      module procedure vector_real64_t_insert_back_array
      module procedure vector_vector_t_insert_back_array
  end interface


  interface indexer
      module procedure vector_int32_t_indexer
      module procedure vector_int64_t_indexer
      module procedure vector_real64_t_indexer
      module procedure vector_vector_t_indexer
  end interface


  interface find
      module procedure vector_int32_t_findloc_wrapper
      module procedure vector_int64_t_findloc_wrapper
  end interface


  interface grow
      module procedure vector_int32_t_grow
      module procedure vector_int64_t_grow
      module procedure vector_real64_t_grow
      module procedure vector_vector_t_grow
      module procedure vector_int32_t_growArray
      module procedure vector_int64_t_growArray
      module procedure vector_real64_t_growArray
      module procedure vector_vector_t_growArray
  end interface


  interface push
      module procedure vector_int32_t_push
      module procedure vector_int64_t_push
      module procedure vector_real64_t_push
      module procedure vector_vector_t_push
      module procedure vector_int32_t_push_n_copies
      module procedure vector_int64_t_push_n_copies
      module procedure vector_real64_t_push_n_copies
      module procedure vector_vector_t_push_n_copies
      module procedure vector_int32_t_push_array
      module procedure vector_int64_t_push_array
      module procedure vector_real64_t_push_array
      module procedure vector_vector_t_push_array
  end interface


  interface copy
      module procedure array_int32_t_copy
      module procedure array_int64_t_copy
  end interface


  interface backup
      module procedure vector_int32_t_backup
      module procedure vector_int64_t_backup
      module procedure vector_real64_t_backup
      module procedure vector_vector_t_backup
  end interface


  interface restore
      module procedure vector_int32_t_restore
      module procedure vector_int64_t_restore
      module procedure vector_real64_t_restore
      module procedure vector_vector_t_restore
  end interface


  interface increase
      module procedure increase_container_size
  end interface


! interface to_string
!     module procedure to_string_int32
!     module procedure to_string_int64
! end interface


  interface


    module function default_constructor () result(vector)
        ! Synopsis: Returns an empty vector
        type(vector_t), allocatable :: vector
    end function


    module function vector_int32_t_rangeConstructor (e, b, s) result(vec)
        ! Synopsis: Creates vector from asymmetric range.
        type(vector_t), allocatable :: vec
        integer(kind = int32), intent(in) :: e                  !! end
        integer(kind = int32), intent(in), optional :: b        !! begin
        integer(kind = int32), intent(in), optional :: s        !! step
    end function


    module function vector_int32_t_arrayConstructor (array) result(vec)
        type(vector_t), allocatable :: vec
        integer(kind = int32), intent(in) :: array(:)
    end function


    module function vector_int64_t_arrayConstructor (array) result(vec)
        type(vector_t), allocatable :: vec
        integer(kind = int64), intent(in) :: array(:)
    end function


    module function vector_real64_t_arrayConstructor (array) result(vec)
        type(vector_t), allocatable :: vec
        real(kind = real64), intent(in) :: array(:)
    end function


    module function vector_vector_t_arrayConstructor (array) result(vec)
        type(vector_t), allocatable :: vec
        type(vector_t), intent(in) :: array(:)
    end function


    module function vector_int32_t_fillConstructor (n, value) result(vec)
        type(vector_t), allocatable :: vec
        integer(kind = int64), intent(in) :: n
        integer(kind = int32), intent(in) :: value
    end function


    module function vector_int64_t_fillConstructor (n, value) result(vec)
        type(vector_t), allocatable :: vec
        integer(kind = int64), intent(in) :: n
        integer(kind = int64), intent(in) :: value
    end function


    module function vector_real64_t_fillConstructor (n, value) result(vec)
        type(vector_t), allocatable :: vec
        integer(kind = int64), intent(in) :: n
        real(kind = real64), intent(in) :: value
    end function


    module function vector_vector_t_fillConstructor (n, value) result(vec)
        type(vector_t), allocatable :: vec
        type(vector_t), intent(in) :: value
        integer(kind = int64), intent(in) :: n
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


    module subroutine vector_real64_t_indexing_method (self, i, value)
        ! Synopsis: Addresses the element pointed to by index.
        class(vector_t), intent(in) :: self
        real(kind = real64), intent(out) :: value
        integer(kind = int64), intent(in) :: i
    end subroutine


    module subroutine vector_vector_t_indexing_method (self, i, value)
        class(vector_t), intent(in) :: self
        type(vector_t), intent(inout) :: value
        integer(kind = int64), intent(in) :: i
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


    module subroutine vector_real64_t_indexer (vector, idx, value)
        type(vector_t), intent(in) :: vector
        real(kind = real64), intent(out) :: value
        integer(kind = int64), intent(in) :: idx
    end subroutine


    module subroutine vector_vector_t_indexer (vector, idx, value)
        type(vector_t), intent(in) :: vector
        type(vector_t), intent(inout) :: value
        integer(kind = int64), intent(in) :: idx
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


    module subroutine vector_print_container_address_method (self)
        ! Synopsis: Prints the addresses of the internal array and iterator.
        class(vector_t), intent(in) :: self
    end subroutine


    module subroutine vector_int32_t_push_back_method (self, value)
        class(vector_t), intent(inout) :: self
        integer(kind = int32), intent(in) :: value
    end subroutine


    module subroutine vector_int64_t_push_back_method (self, value)
        class(vector_t), intent(inout) :: self
        integer(kind = int64), intent(in) :: value
    end subroutine


    module subroutine vector_real64_t_push_back_method (self, value)
        class(vector_t), intent(inout) :: self
        real(kind = real64), intent(in) :: value
    end subroutine


    module subroutine vector_vector_t_push_back_method (self, value)
        class(vector_t), intent(inout) :: self
        type(vector_t), intent(in) :: value
    end subroutine


    module subroutine vector_int32_t_push_back_array_method (self, array)
        class(vector_t), intent(inout) :: self
        integer(kind = int32), intent(in) :: array(:)
    end subroutine


    module subroutine vector_int64_t_push_back_array_method (self, array)
        class(vector_t), intent(inout) :: self
        integer(kind = int64), intent(in) :: array(:)
    end subroutine


    module subroutine vector_real64_t_push_back_array_method (self, array)
        class(vector_t), intent(inout) :: self
        real(kind = real64), intent(in) :: array(:)
    end subroutine


    module subroutine vector_vector_t_push_back_array_method (self, array)
        class(vector_t), intent(inout) :: self
        type(vector_t), intent(in) :: array(:)
    end subroutine


    module subroutine vector_int32_t_push_back (vector, value)
        type(vector_t), intent(inout) :: vector
        integer(kind = int32), intent(in) :: value
    end subroutine


    module subroutine vector_int64_t_push_back (vector, value)
        type(vector_t), intent(inout) :: vector
        integer(kind = int64), intent(in) :: value
    end subroutine


    module subroutine vector_real64_t_push_back (vector, value)
        type(vector_t), intent(inout) :: vector
        real(kind = real64), intent(in) :: value
    end subroutine


    module subroutine vector_vector_t_push_back (vector, value)
        type(vector_t), intent(inout) :: vector
        type(vector_t), intent(in) :: value
    end subroutine


    module subroutine vector_int32_t_push_back_array (vector, array)
        type(vector_t), intent(inout) :: vector
        integer(kind = int32), intent(in) :: array(:)
    end subroutine


    module subroutine vector_int64_t_push_back_array (vector, array)
        type(vector_t), intent(inout) :: vector
        integer(kind = int64), intent(in) :: array(:)
    end subroutine


    module subroutine vector_real64_t_push_back_array (vector, array)
        type(vector_t), intent(inout) :: vector
        real(kind = real64), intent(in) :: array(:)
    end subroutine


    module subroutine vector_vector_t_push_back_array (vector, array)
        type(vector_t), intent(inout) :: vector
        type(vector_t), intent(in) :: array(:)
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


    module subroutine vector_real64_t_insert_back (vector, value)
        ! Synopsis: Inserts value unto back, vector grows as needed.
        type(vector_t), intent(inout) :: vector
        real(kind = real64), intent(in) :: value
    end subroutine


    module subroutine vector_vector_t_insert_back (vector, value)
        type(vector_t), intent(inout) :: vector
        type(vector_t), intent(in) :: value
    end subroutine


    module subroutine vector_int32_t_insert_back_array (vector, array)
        ! Synopsis: Inserts array unto back, grows vector if needed.
        type(vector_t), intent(inout) :: vector
        integer(kind = int32), intent(in) :: array(:)
    end subroutine


    module subroutine vector_int64_t_insert_back_array (vector, array)
        type(vector_t), intent(inout) :: vector
        integer(kind = int64), intent(in) :: array(:)
    end subroutine


    module subroutine vector_real64_t_insert_back_array (vector, array)
        type(vector_t), intent(inout) :: vector
        real(kind = real64), intent(in) :: array(:)
    end subroutine


    module subroutine vector_vector_t_insert_back_array (vector, array)
        type(vector_t), intent(inout) :: vector
        type(vector_t), intent(in) :: array(:)
    end subroutine


    module subroutine vector_int32_t_grow (vector, value)
        ! Synopsis: Doubles the vector size.
        type(vector_t), intent(inout) :: vector
        integer(kind = int32), intent(in) :: value
    end subroutine


    module subroutine vector_int64_t_grow (vector, value)
        type(vector_t), intent(inout) :: vector
        integer(kind = int64), intent(in) :: value
    end subroutine


    module subroutine vector_real64_t_grow (vector, value)
        type(vector_t), intent(inout) :: vector
        real(kind = real64), intent(in) :: value
    end subroutine


    module subroutine vector_vector_t_grow (vector, value)
        type(vector_t), intent(inout) :: vector
        type(vector_t), intent(in) :: value
    end subroutine


    module subroutine vector_int32_t_growArray (vector, values)
        ! Synopsis: Grows vector so that it can store the array `values'.
        type(vector_t), intent(inout) :: vector
        integer(kind = int32), intent(in) :: values(:)
    end subroutine


    module subroutine vector_int64_t_growArray (vector, values)
        type(vector_t), intent(inout) :: vector
        integer(kind = int64), intent(in) :: values(:)
    end subroutine


    module subroutine vector_real64_t_growArray (vector, values)
        type(vector_t), intent(inout) :: vector
        real(kind = real64), intent(in) :: values(:)
    end subroutine


    module subroutine vector_vector_t_growArray (vector, values)
        type(vector_t), intent(inout) :: vector
        type(vector_t), intent(in) :: values(:)
    end subroutine


    module subroutine vector_int32_t_backup (vector, array)
        type(vector_t), intent(in) :: vector
        integer(kind = int32), intent(inout), allocatable :: array(:)
    end subroutine


    module subroutine vector_int64_t_backup (vector, array)
        type(vector_t), intent(in) :: vector
        integer(kind = int64), intent(inout), allocatable :: array(:)
    end subroutine


    module subroutine vector_real64_t_backup (vector, array)
        type(vector_t), intent(in) :: vector
        real(kind = real64), intent(inout), allocatable :: array(:)
    end subroutine


    module subroutine vector_vector_t_backup (vector, array)
        type(vector_t), intent(in) :: vector
        type(vector_t), intent(inout), allocatable :: array(:)
    end subroutine


    module subroutine vector_int32_t_restore (vector, array, value)
        type(vector_t), intent(inout) :: vector
        integer(kind = int32), intent(in) :: array(:)
        integer(kind = int32), intent(in) :: value
    end subroutine


    module subroutine vector_int64_t_restore (vector, array, value)
        type(vector_t), intent(inout) :: vector
        integer(kind = int64), intent(in) :: array(:)
        integer(kind = int64), intent(in) :: value
    end subroutine


    module subroutine vector_real64_t_restore (vector, array, value)
        type(vector_t), intent(inout) :: vector
        real(kind = real64), intent(in) :: array(:)
        real(kind = real64), intent(in) :: value
    end subroutine


    module subroutine vector_vector_t_restore (vector, array, value)
        type(vector_t), intent(inout) :: vector
        type(vector_t), intent(in) :: array(:)
        type(vector_t), intent(in) :: value
    end subroutine


    module pure subroutine vector_int32_t_push (vector, value)
        type(vector_t), intent(inout), target :: vector
        integer(kind = int32), intent(in) :: value
    end subroutine


    module pure subroutine vector_int64_t_push (vector, value)
        type(vector_t), intent(inout), target :: vector
        integer(kind = int64), intent(in) :: value
    end subroutine


    module pure subroutine vector_real64_t_push (vector, value)
        type(vector_t), intent(inout), target :: vector
        real(kind = real64), intent(in) :: value
    end subroutine


    module subroutine vector_vector_t_push (vector, value)
        type(vector_t), intent(inout), target :: vector
        type(vector_t), intent(in) :: value
    end subroutine


    module pure subroutine vector_int32_t_push_n_copies (vector, n, value)
        type(vector_t), intent(inout), target :: vector
        integer(kind = int64), intent(in) :: n
        integer(kind = int32), intent(in) :: value
    end subroutine


    module pure subroutine vector_int64_t_push_n_copies (vector, n, value)
        type(vector_t), intent(inout), target :: vector
        integer(kind = int64), intent(in) :: n
        integer(kind = int64), intent(in) :: value
    end subroutine


    module pure subroutine vector_real64_t_push_n_copies (vector, n, value)
        type(vector_t), intent(inout), target :: vector
        integer(kind = int64), intent(in) :: n
        real(kind = real64), intent(in) :: value
    end subroutine


    module subroutine vector_vector_t_push_n_copies (vector, n, value)
        type(vector_t), intent(inout), target :: vector
        type(vector_t), intent(in) :: value
        integer(kind = int64), intent(in) :: n
    end subroutine


    module pure subroutine vector_int32_t_push_array (vector, array)
        type(vector_t), intent(inout), target :: vector
        integer(kind = int32), intent(in) :: array(:)
    end subroutine


    module pure subroutine vector_int64_t_push_array (vector, array)
        type(vector_t), intent(inout), target :: vector
        integer(kind = int64), intent(in) :: array(:)
    end subroutine


    module pure subroutine vector_real64_t_push_array (vector, array)
        type(vector_t), intent(inout), target :: vector
        real(kind = real64), intent(in) :: array(:)
    end subroutine


    module subroutine vector_vector_t_push_array (vector, array)
        type(vector_t), intent(inout), target :: vector
        type(vector_t), intent(in) :: array(:)
    end subroutine


    module elemental subroutine array_int32_t_copy (dst, src)
        integer(kind = int32), intent(inout) :: dst
        integer(kind = int32), intent(in) :: src
    end subroutine


    module elemental subroutine array_int64_t_copy (dst, src)
        integer(kind = int64), intent(inout) :: dst
        integer(kind = int64), intent(in) :: src
    end subroutine


    module subroutine vector_vector_t_copy_method (self, vector)
        class(vector_t), intent(inout) :: self
        class(vector_t), intent(in) :: vector
    end subroutine


    module subroutine vector_validate_iterator_method (self)
        class(vector_t), intent(inout) :: self
    end subroutine


    module subroutine vector_vector_t_copy (to, from)
        type(vector_t), intent(out), target :: to
        type(vector_t), intent(in) :: from
    end subroutine


    module recursive subroutine vector_validate_iterator (vector)
        type(vector_t), intent(inout), target :: vector
    end subroutine


    module subroutine vector_int32_t_initializer (vector, value)
        type(vector_t), intent(inout) :: vector
        integer(kind = int32), intent(in) :: value
    end subroutine


    module subroutine vector_int64_t_initializer (vector, value)
        type(vector_t), intent(inout) :: vector
        integer(kind = int64), intent(in) :: value
    end subroutine


    module subroutine vector_real64_t_initializer (vector, value)
        type(vector_t), intent(inout) :: vector
        real(kind = real64), intent(in) :: value
    end subroutine


    module subroutine vector_vector_t_initializer (vector, value)
        type(vector_t), intent(inout) :: vector
        type(vector_t), intent(in) :: value
    end subroutine


    module subroutine vector_int32_t_initializerArray (vector, array)
        ! Synopsis: Initializes a vector from array.
        type(vector_t), intent(inout) :: vector
        integer(kind = int32), intent(in) :: array(:)
    end subroutine


    module subroutine vector_int64_t_initializerArray (vector, array)
        type(vector_t), intent(inout) :: vector
        integer(kind = int64), intent(in) :: array(:)
    end subroutine


    module subroutine vector_real64_t_initializerArray (vector, array)
        type(vector_t), intent(inout) :: vector
        real(kind = real64), intent(in) :: array(:)
    end subroutine


    module subroutine vector_vector_t_initializerArray (vector, array)
        type(vector_t), intent(inout) :: vector
        type(vector_t), intent(in) :: array(:)
    end subroutine


    module subroutine vector_int32_t_create (vector, value)
        ! Synopsis: Creates the first element in vector.
        type(vector_t), intent(inout) :: vector
        integer(kind = int32), intent(in) :: value
    end subroutine


    module subroutine vector_int64_t_create (vector, value)
        type(vector_t), intent(inout) :: vector
        integer(kind = int64), intent(in) :: value
    end subroutine


    module subroutine vector_real64_t_create (vector, value)
        type(vector_t), intent(inout) :: vector
        real(kind = real64), intent(in) :: value
    end subroutine


    module subroutine vector_vector_t_create (vector, value)
        type(vector_t), intent(inout) :: vector
        type(vector_t), intent(in) :: value
    end subroutine


    module subroutine vector_int32_t_createArray (vector, array)
        ! Synopsis: Creates vector from array.
        type(vector_t), intent(inout) :: vector
        integer(kind = int32), intent(in) :: array(:)
    end subroutine


    module subroutine vector_int64_t_createArray (vector, array)
        type(vector_t), intent(inout) :: vector
        integer(kind = int64), intent(in) :: array(:)
    end subroutine


    module subroutine vector_real64_t_createArray (vector, array)
        type(vector_t), intent(inout) :: vector
        real(kind = real64), intent(in) :: array(:)
    end subroutine


    module subroutine vector_vector_t_createArray (vector, array)
        type(vector_t), intent(inout) :: vector
        type(vector_t), intent(in) :: array(:)
    end subroutine


    module subroutine vector_int32_t_erase_method (vec, i, b, s, v, m, f)
        ! Synopsis:
        ! Erases values either by index, range, subscript, or value(s).
        class(vector_t), intent(inout) :: vec
        integer(kind = int64), intent(in), optional :: i       ! index
        integer(kind = int64), intent(in), optional :: b(2)    ! bounds
        integer(kind = int64), intent(in), optional :: s(:)    ! isubs
        integer(kind = int32), intent(in), optional :: v(:)    ! values
        logical(kind = int32), intent(in), optional :: f       ! flip
        character(len=9),      intent(in), optional :: m       ! mode
    end subroutine


    module subroutine vector_int32_t_erase (vec, i, b, s, v, m, f)
        type(vector_t), intent(inout) :: vec
        integer(kind = int64), intent(in), optional :: i       ! index
        integer(kind = int64), intent(in), optional :: b(2)    ! bounds
        integer(kind = int64), intent(in), optional :: s(:)    ! isubs
        integer(kind = int32), intent(in), optional :: v(:)    ! values
        logical(kind = int32), intent(in), optional :: f       ! flip
        character(len=9),      intent(in), optional :: m       ! mode
    end subroutine


    module subroutine vector_int32_t_erase_all (vector)
        type(vector_t), intent(inout) :: vector
    end subroutine


    module subroutine vector_int32_t_erase_byIndexShadow (vec, idx, f)
        type(vector_t), intent(inout) :: vec
        integer(kind = int64), intent(in) :: idx
        logical(kind = int32), intent(in), optional :: f
    end subroutine


    module subroutine vector_int32_t_erase_by_index (vector, idx)
        type(vector_t), intent(inout) :: vector
        integer(kind = int64), intent(in) :: idx
    end subroutine


    module subroutine vector_int32_t_erase_byRangeShadow (vec, b, f)
        type(vector_t), intent(inout) :: vec
        integer(kind = int64), intent(in) :: b(2)
        logical(kind = int32), intent(in), optional :: f
    end subroutine


    module subroutine vector_int32_t_erase_by_range (vector, bounds)
        type(vector_t), intent(inout) :: vector
        integer(kind = int64), intent(in) :: bounds(2)
    end subroutine


    module subroutine vector_int32_t_erase_byVecSubShadow (vec, vs, f)
        type(vector_t), intent(inout) :: vec
        integer(kind = int64), intent(in) :: vs(:)
        logical(kind = int32), intent(in), optional :: f
    end subroutine


    module subroutine vector_int32_t_erase_by_subscript (vector, vs)
        type(vector_t), intent(inout), target :: vector
        integer(kind = int64), intent(in) :: vs(:)
    end subroutine


    module subroutine vector_int32_t_erase_byValueShadow (vec, elem, f)
        type(vector_t), intent(inout) :: vec
        integer(kind = int32), intent(in) :: elem(:)
        logical(kind = int32), intent(in), optional :: f
    end subroutine


    module subroutine vector_int32_t_erase_values (vector, elements)
        type(vector_t), intent(inout) :: vector
        integer(kind = int32), intent(in) :: elements(:)
    end subroutine


    module subroutine vector_int32_t_trim (vector, vec_bounds)
        type(vector_t), intent(inout), target :: vector
        integer(kind = int64), intent(in) :: vec_bounds(2)
    end subroutine


    module subroutine vector_int32_t_erase_intermediate (vector, idx)
        type(vector_t), intent(inout), target :: vector
        integer(kind = int64), intent(in) :: idx
    end subroutine


    module subroutine vector_int32_t_erase_final_value (vector)
        type(vector_t), intent(inout), target :: vector
    end subroutine


    module subroutine vector_int32_t_erase_argsCheck (i, b, s, v, m)
        integer(kind = int64), intent(in), optional :: i
        integer(kind = int64), intent(in), optional :: b(2)
        integer(kind = int64), intent(in), optional :: s(:)
        integer(kind = int32), intent(in), optional :: v(:)
        character(len=9),      intent(in), optional :: m
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


    module subroutine vector_allocate_errmsg (vector, errMSG)
        type(vector_t), intent(inout) :: vector
        character(*), intent(in) :: errMSG
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


    module subroutine vector_real64_t_allocate_dynamic (b, ary, value)
        integer(kind = int64), intent(in) :: b(0:1)
        class(*), intent(inout), allocatable :: ary(:)
        real(kind = real64), intent(in) :: value
    end subroutine


    module subroutine vector_vector_t_allocate_dynamic (b, ary, value)
        type(vector_t), intent(in) :: value
        class(*), intent(inout), allocatable :: ary(:)
        integer(kind = int64), intent(in) :: b(0:1)
    end subroutine


    module subroutine vector_allocate_array_vector_t (b, array)
        type(vector_t), intent(inout), allocatable :: array(:)
        integer(kind = int64), intent(in) :: b(0:1)
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


    module subroutine vector_vector_t_deallocate_dynamic (array, value)
        type(vector_t), intent(in) :: value
        class(*), intent(inout), allocatable :: array(:)
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


    module pure subroutine increase_container_size (vector, alloc)
        type(vector_t), intent(inout) :: vector
        integer(kind = int64), intent(in), optional :: alloc
    end subroutine


!   module function to_string_int32 (i) result(str)
!       integer(kind = int32), intent(in) :: i
!       character(len = 64) :: str
!   end function


!   module function to_string_int64 (i) result(str)
!       integer(kind = int64), intent(in) :: i
!       character(len = 64) :: str
!   end function


    module subroutine destructor_iter_t (i)
        type(iter_t), intent(inout) :: i
    end subroutine


    module subroutine destructor_stat_t (s)
        type(stat_t), intent(inout) :: s
    end subroutine


    module recursive subroutine finalizer (vector)
        type(vector_t), intent(inout) :: vector
    end subroutine


  end interface


end module


! References:
! SJ Chapman, FORTRAN for Scientists and Engineers, fourth edition
! A Koenig and B Moo, Accelerated C++ Practical Programming by Example


! TODO:
! [x] IMPLEMENT type-bound assignment for the data component of vector.
! [x] make the components of :[vector_t]: allocatable
! [x] Implement a wrapper for the findloc intrinsic. Full support is
!     not intended. Just for the case in which array, value, and dim
!     are supplied (the result is a scalar).
!     This method will be used for testing the minimalistic neighbor-list
!     implemented in test/modules/math/vector.
