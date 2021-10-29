!
!   source: Vector_utils.for
!   author: misael-diaz
!   date:   2021-06-27
!
!
!   Synopsis:
!   Implements (particular) utilities for the vector class.
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

submodule (VectorClass) vector_utils
implicit none
contains

  module subroutine pointer_associate_method (self, target)
      class(pointer_t), intent(inout) :: self
      class(pointer_t), intent(in), target :: target

      self % p => target % p

      return
  end subroutine


  module function arange_int32_t_constructor (e, b, s) result(arange)
      ! Synopsis: Creates an asymmetric range of 32-bit integers.
      type(arange_t), target :: arange
      class(*), pointer :: p_begin => null()
      class(*), pointer :: p_end   => null()
      class(*), pointer :: p_step  => null()
      integer(kind = int32), intent(in) :: e            !! end
      integer(kind = int32), intent(in), optional :: b  !! begin
      integer(kind = int32), intent(in), optional :: s  !! step
      integer(kind = int32) :: begin, final, step

      call allocator (arange % b, e)
      call allocator (arange % e, e)
      call allocator (arange % s, e)

      ! uses the defaults for the optional arguments
      if ( present(b) ) then
          begin = b
      else
          begin = 0
      end if

      final = e

      if ( present(s) ) then
          step = s
      else
          step = 1
      end if

      ! sets the arange components
      p_begin => arange % b
      select type (p_begin)
          type is ( integer(kind = int32) )
              p_begin = begin
          class default
              error stop 'arange: unexpected error'
      end select

      p_end => arange % e
      select type (p_end)
          type is ( integer(kind = int32) )
              p_end = final
          class default
              error stop 'arange: unexpected error'
      end select

      p_step => arange % s
      select type (p_step)
          type is ( integer(kind = int32) )
              p_step = step
          class default
              error stop 'arange: unexpected error'
      end select

      return
  end function arange_int32_t_constructor


  module function arange_int64_t_constructor (e, b, s) result(arange)
      ! Synopsis: Creates an asymmetric range of 64-bit integers.
      type(arange_t), target :: arange
      class(*), pointer :: p_begin => null()
      class(*), pointer :: p_end   => null()
      class(*), pointer :: p_step  => null()
      integer(kind = int64), intent(in) :: e            !! end
      integer(kind = int64), intent(in), optional :: b  !! begin
      integer(kind = int64), intent(in), optional :: s  !! step
      integer(kind = int64) :: begin, final, step

      call allocator (arange % b, e)
      call allocator (arange % e, e)
      call allocator (arange % s, e)

      ! uses the defaults for the optional arguments
      if ( present(b) ) then
          begin = b
      else
          begin = 0_int64
      end if

      final = e

      if ( present(s) ) then
          step = s
      else
          step = 1_int64
      end if

      ! sets the arange components
      p_begin => arange % b
      select type (p_begin)
          type is ( integer(kind = int64) )
              p_begin = begin
          class default
              error stop 'arange: unexpected error'
      end select

      p_end => arange % e
      select type (p_end)
          type is ( integer(kind = int64) )
              p_end = final
          class default
              error stop 'arange: unexpected error'
      end select

      p_step => arange % s
      select type (p_step)
          type is ( integer(kind = int64) )
              p_step = step
          class default
              error stop 'arange: unexpected error'
      end select

      return
  end function arange_int64_t_constructor


  module subroutine arange_int32_t_allocate_dynamic (p, value)
      class(*), intent(inout), allocatable :: p
      integer(kind = int32), intent(in) :: value
      integer(kind = int32) :: mstat

      allocate (p, mold = value, stat = mstat)

      if (mstat /= 0) then
          error stop 'allocator: allocation error'
      end if

      return
  end subroutine


  module subroutine arange_int64_t_allocate_dynamic (p, value)
      class(*), intent(inout), allocatable :: p
      integer(kind = int64), intent(in) :: value
      integer(kind = int32) :: mstat

      allocate (p, mold = value, stat = mstat)

      if (mstat /= 0) then
          error stop 'allocator: allocation error'
      end if

      return
  end subroutine


  module subroutine allocate_vector_t (v)
      ! Synopsis: Allocates memory for the vector components.
      type(vector_t), intent(inout) :: v

      call allocator (v % begin)
      call allocator (v % avail)
      call allocator (v % limit)
      call allocator (v % array)
      call allocator (v % state)
      call allocator (v % deref)

      return
  end subroutine


  module elemental subroutine array_int32_t_copy (dst, src)
      integer(kind = int32), intent(inout) :: dst
      integer(kind = int32), intent(in) :: src
      dst = src
      return
  end subroutine


  module elemental subroutine array_int64_t_copy (dst, src)
      integer(kind = int64), intent(inout) :: dst
      integer(kind = int64), intent(in) :: src
      dst = src
      return
  end subroutine


  module subroutine allocate_iter_t (i)
      type(iter_t), intent(inout), allocatable :: i
      integer(kind = int32) :: mstat
      character(*), parameter :: errMSG = &
          & "vector.allocate_iter_t: (de)allocation error"

      call dealloc

      allocate (i, stat = mstat)
      if (mstat /= 0) error stop errMSG

      return
      contains

          subroutine dealloc

              mstat = 0
              if ( allocated(i) ) then
                  deallocate (i, stat = mstat)
              end if

              if (mstat /= 0) then
                  error stop errMSG
              end if

              return
          end subroutine
  end subroutine


  module subroutine allocate_data_t (d)
      type(data_t), intent(inout), allocatable :: d
      integer(kind = int32) :: mstat
      character(*), parameter :: errMSG = &
          & "vector.allocate_data_t: (de)allocation error"

      call dealloc

      allocate (d, stat = mstat)
      if (mstat /= 0) error stop errMSG

      return
      contains

          subroutine dealloc

              mstat = 0
              if ( allocated(d) ) then
                  deallocate (d, stat = mstat)
              end if

              if (mstat /= 0) then
                  error stop errMSG
              end if

              return
          end subroutine

  end subroutine allocate_data_t


  module subroutine allocate_stat_t (s)
      type(stat_t), intent(inout), allocatable :: s
      integer(kind = int32) :: mstat
      character(*), parameter :: errMSG = &
          & "vector.allocate_stat_t: (de)allocation error"

      call dealloc

      allocate (s, stat = mstat)
      if (mstat /= 0) error stop errMSG

      return
      contains

          subroutine dealloc

              mstat = 0
              if ( allocated(s) ) then
                  deallocate (s, stat = mstat)
              end if

              if (mstat /= 0) then
                  error stop errMSG
              end if

              return
          end subroutine

  end subroutine allocate_stat_t


  module subroutine vector_allocate_errmsg (vector, errMSG)
      ! Synopsis: Allocates memory for the error message `errMSG'.
      type(vector_t), intent(inout) :: vector
      integer(kind = int32) :: mstat
      character(*), intent(in) :: errMSG
      character(*), parameter :: name = 'dynamic::vector.alloc_errMSG():'
      character(*), parameter :: mallocErr  = name // ' ' // &
          & 'memory (de)allocation error'
      character(*), parameter :: unexpected = name // ' ' // &
          & 'vector has not been instantiated'

      call state_check
      call dealloc_errmsg


      allocate (character( len=len(errMSG) ) :: vector % state % errmsg, &
              & stat = mstat)

      if (mstat /= 0) then
          error stop mallocErr
      end if


      return
      contains

          subroutine state_check

              if ( .not. allocated (vector % state) ) then
                  error stop unexpected
              end if

              return
          end subroutine


          subroutine dealloc_errmsg

              if ( allocated (vector % state % errmsg) ) then
                  deallocate (vector % state % errmsg, stat=mstat)
                  if (mstat /= 0) error stop mallocErr
              end if

              return
          end subroutine

  end subroutine vector_allocate_errmsg


  module subroutine vector_int32_t_allocate_dynamic (b, array, value)
      integer(kind = int64), intent(in) :: b(0:1)       ! b[ounds]
      class(*), intent(inout), allocatable :: array(:)
      integer(kind = int64):: lb
      integer(kind = int64):: ub
      integer(kind = int32), intent(in) :: value
      integer(kind = int32):: mstat


      if ( allocated(array) ) then

          deallocate (array, stat = mstat)

          if (mstat /= 0) then
              error stop "dynamic::vector.allocate: unexpected error"
          end if

      end if


      lb = b(0)
      ub = b(1)
      allocate (array(lb:ub), mold = value, stat = mstat)

      if (mstat /= 0) then
          error stop "dynamic::vector.allocate: allocation error"
      end if


      return
  end subroutine


  module subroutine vector_int64_t_allocate_dynamic (b, array, value)
      integer(kind = int64), intent(in) :: b(0:1)
      class(*), intent(inout), allocatable :: array(:)
      integer(kind = int64):: lb
      integer(kind = int64):: ub
      integer(kind = int64), intent(in) :: value
      integer(kind = int32):: mstat


      if ( allocated(array) ) then

          deallocate (array, stat = mstat)

          if (mstat /= 0) then
              error stop "dynamic::vector.allocate: unexpected error"
          end if

      end if


      lb = b(0)
      ub = b(1)
      allocate (array(lb:ub), mold = value, stat = mstat)

      if (mstat /= 0) then
          error stop "dynamic::vector.allocate: allocation error"
      end if


      return
  end subroutine


  module subroutine vector_real64_t_allocate_dynamic (b, ary, value)
      integer(kind = int64), intent(in) :: b(0:1)
      class(*), intent(inout), allocatable :: ary(:)
      integer(kind = int64):: lb
      integer(kind = int64):: ub
      real(kind = real64), intent(in) :: value
      integer(kind = int32):: mstat


      if ( allocated(ary) ) then

          deallocate (ary, stat = mstat)

          if (mstat /= 0) then
              error stop "dynamic::vector.allocate: unexpected error"
          end if

      end if


      lb = b(0)
      ub = b(1)
      allocate (ary(lb:ub), mold = value, stat = mstat)

      if (mstat /= 0) then
          error stop "dynamic::vector.allocate: allocation error"
      end if


      return
  end subroutine vector_real64_t_allocate_dynamic


  module subroutine vector_vector_t_allocate_dynamic (b, ary, value)
      type(vector_t), intent(in) :: value
      integer(kind = int64), intent(in) :: b(0:1)
      class(*), intent(inout), allocatable :: ary(:)
      integer(kind = int64):: lb
      integer(kind = int64):: ub
      integer(kind = int32):: mstat


      if ( allocated(ary) ) then

          deallocate (ary, stat = mstat)

          if (mstat /= 0) then
              error stop "dynamic::vector.allocate: unexpected error"
          end if

      end if


      lb = b(0)
      ub = b(1)
      allocate (ary(lb:ub), mold = value, stat = mstat)

      if (mstat /= 0) then
          error stop "dynamic::vector.allocate: allocation error"
      end if


      return
  end subroutine


  module subroutine vector_pointer_t_allocate_dynamic (b, ary, value)
      type(pointer_t), intent(in) :: value
      integer(kind = int64), intent(in) :: b(0:1)
      class(*), intent(inout), allocatable :: ary(:)
      integer(kind = int64):: lb
      integer(kind = int64):: ub
      integer(kind = int32):: mstat


      if ( allocated(ary) ) then

          deallocate (ary, stat = mstat)

          if (mstat /= 0) then
              error stop "dynamic::vector.allocate: unexpected error"
          end if

      end if


      lb = b(0)
      ub = b(1)
      allocate (ary(lb:ub), mold = value, stat = mstat)

      if (mstat /= 0) then
          error stop "dynamic::vector.allocate: allocation error"
      end if


      return
  end subroutine vector_pointer_t_allocate_dynamic


  module subroutine vector_allocate_array_vector_t (b, array)
      type(vector_t), intent(inout), allocatable :: array(:)
      integer(kind = int64), intent(in) :: b(0:1)
      integer(kind = int64):: lb
      integer(kind = int64):: ub
      integer(kind = int32):: mstat


      if ( allocated(array) ) then
          deallocate(array, stat = mstat)
          if (mstat /= 0) then
              error stop "error allocating array of vectors"
          end if
      end if


      lb = b(0)
      ub = b(1)
      allocate (array(lb:ub), stat = mstat)
      if (mstat /= 0) then
          error stop "error allocating array of vectors"
      end if

      return
  end subroutine


  module subroutine vector_allocate_array_pointer_t (b, array)
      type(pointer_t), intent(inout), allocatable :: array(:)
      integer(kind = int64), intent(in) :: b(0:1)
      integer(kind = int64):: lb
      integer(kind = int64):: ub
      integer(kind = int32):: mstat


      if ( allocated(array) ) then
          deallocate(array, stat = mstat)
          if (mstat /= 0) then
              error stop "error allocating array of vectors"
          end if
      end if


      lb = b(0)
      ub = b(1)
      allocate (array(lb:ub), stat = mstat)
      if (mstat /= 0) then
          error stop "error allocating array of vectors"
      end if

      return
  end subroutine


  module subroutine deallocate_iter_t (i)
      type(iter_t), intent(inout), allocatable :: i
      integer(kind = int32) :: mstat
      character(len=*), parameter :: errmsg = &
          & "dynamic::vector.deallocate_iter: deallocation error"

      mstat = 0
      if ( allocated(i) ) then
          deallocate (i, stat = mstat)
      end if

      if (mstat /= 0) then
          error stop errmsg
      end if

      return
  end subroutine


  module subroutine deallocate_data_t (d)
      type(data_t), intent(inout), allocatable :: d
      integer(kind = int32) :: mstat
      character(len=*), parameter :: errmsg = &
          & "dynamic::vector.deallocate_data: deallocation error"

      mstat = 0
      if ( allocated(d) ) then
          deallocate (d, stat = mstat)
      end if

      if (mstat /= 0) then
          error stop errmsg
      end if

      return
  end subroutine


  module subroutine deallocate_stat_t (s)
      type(stat_t), intent(inout), allocatable :: s
      integer(kind = int32) :: mstat
      character(len=*), parameter :: errmsg = &
          & "dynamic::vector.deallocate_stat: deallocation error"

      mstat = 0
      if ( allocated(s) ) then
          deallocate (s, stat = mstat)
      end if

      if (mstat /= 0) then
          error stop errmsg
      end if

      return
  end subroutine


  module subroutine vector_int32_t_deallocate_dynamic (array, value)
      ! the compiler cannot differentiate without the dummy value
      class(*), intent(inout), allocatable :: array(:)
      integer(kind = int32), intent(in) :: value
      integer(kind = int32) :: mstat
      character(len=*), parameter :: errmsg = &
          & "dynamic::vector.deallocate_polymorphic: " // &
          & "deallocation error"

      mstat = 0
      if ( allocated(array) ) then
          deallocate (array, stat = mstat)
      end if

      if (mstat /= 0) then
          print *, value  ! use input so that compiler won't complain
          error stop errmsg
      end if

      return
  end subroutine


  module subroutine vector_int64_t_deallocate_dynamic (array, value)
      class(*), intent(inout), allocatable :: array(:)
      integer(kind = int64), intent(in) :: value
      integer(kind = int32) :: mstat
      character(len=*), parameter :: errmsg = &
          & "dynamic::vector.deallocate_polymorphic: " // &
          & "deallocation error"

      mstat = 0
      if ( allocated(array) ) then
          deallocate (array, stat = mstat)
      end if

      if (mstat /= 0) then
          print *, value  ! use input so that compiler won't complain
          error stop errmsg
      end if

      return
  end subroutine
        

  module subroutine vector_vector_t_deallocate_dynamic (array, value)
      type(vector_t), intent(in) :: value
      class(*), intent(inout), allocatable :: array(:)
      integer(kind = int32) :: mstat
      character(len=*), parameter :: errmsg = &
          & "dynamic::vector.deallocate_polymorphic: " // &
          & "deallocation error"

      mstat = 0
      if ( allocated(array) ) then
          deallocate (array, stat = mstat)
      end if

      if (mstat /= 0) then
          print *, value % size ()
          error stop errmsg
      end if

      return
  end subroutine


  module subroutine vector_pointer_t_deallocate_dynamic (array, value)
      type(pointer_t), intent(in) :: value
      class(*), intent(inout), allocatable :: array(:)
      integer(kind = int32) :: mstat
      character(len=*), parameter :: errmsg = &
          & "dynamic::vector.deallocate_polymorphic: " // &
          & "deallocation error"

      mstat = 0
      if ( allocated(array) ) then
          deallocate (array, stat = mstat)
      end if

      if (mstat /= 0) then
          print *, associated(value % p)
          error stop errmsg
      end if

      return
  end subroutine


  module subroutine is_empty (vector)
      type(vector_t), intent(in) :: vector

      if ( .not. allocated(vector % state) ) then
          error stop "dynamic::vector.is_empty: empty vector"
      else if ( .not. vector % state % init ) then
          error stop "dynamic::vector.is_empty: empty vector"
      end if

      return
  end subroutine


  module subroutine is_instantiated (vector)
      type(vector_t), intent(inout) :: vector

      if ( .not. allocated(vector % state) ) then
          call instantiate (vector)
      end if

      return
  end subroutine


  module subroutine check_bounds (vector, idx)
      type(vector_t), intent(in) :: vector
      integer(kind = int64), intent(in) :: idx

      if ( idx < vector % begin % idx ) then
          error stop "dynamic::vector.[i]: i < lbound"
      else if ( idx >= vector % avail % idx ) then
          error stop "dynamic::vector.[i]: i > ubound"
      end if

      return
  end subroutine


  module pure subroutine increase_container_size (vector, alloc)
      ! Synopsis:
      ! Doubles the vector-size at most, complains if doing so would yield
      ! an ill-formed object.
      type(vector_t), intent(inout) :: vector
      integer(kind = int64), intent(in), optional :: alloc
      integer(kind = int64) :: limit            !! vector-size
      integer(kind = int64) :: mask             !! bitmask
      integer(kind = int32) :: msb              !! most significant bit
      character(*), parameter :: errMSG = &     !! error message
          & 'vector has reached its size limit'

      if ( present(alloc) ) then
          limit = alloc
      else
          limit = vector % limit % idx
      end if

      mask = 0_int64
      msb = imsb(limit) + 1
      call illformed_check

      limit = ibset(mask, msb)
      vector % limit % idx = limit

      return
      contains

          pure subroutine illformed_check
              ! checks if doubling the size yields an ill-formed object
              if ( msb == BITS_MAX_BIT ) then
                  error stop errMSG
              end if

              return
          end subroutine

  end subroutine increase_container_size


!       module function to_string_int32 (i) result(str)
!           integer(kind = int32), intent(in) :: i
!           character(len = 64) :: str
!
!           write (str, '(I16)') i
!           str = adjustl (str)
!
!           return
!       end function


!       module function to_string_int64 (i) result(str)
!           integer(kind = int64), intent(in) :: i
!           character(len = 64) :: str
!
!           write (str, '(I32)') i
!           str = adjustl (str)
!
!           return
!       end function


  module subroutine destructor_arange_t (arange)
      type(arange_t), intent(inout) :: arange
      integer(kind = int32) :: mstat
      character(*), parameter :: errmsg = &
          & "arange.destructor(): unexpected deallocation error"

      if ( allocated(arange % b) ) then
          deallocate (arange % b, stat=mstat)
          if (mstat /= 0) error stop errmsg
      end if

      if ( allocated(arange % e) ) then
          deallocate (arange % e, stat=mstat)
          if (mstat /= 0) error stop errmsg
      end if

      if ( allocated(arange % s) ) then
          deallocate (arange % s, stat=mstat)
          if (mstat /= 0) error stop errmsg
      end if

      return
  end subroutine


  module subroutine destructor_iter_t (i)
      type(iter_t), intent(inout) :: i

      if ( associated(i % it) ) then
          i % it => null()
      end if

      return
  end subroutine


  module subroutine destructor_stat_t (s)
      type(stat_t), intent(inout) :: s
      integer(kind = int32):: mstat
      character(len=*), parameter :: errmsg = &
          & "dynamic::vector.state: deallocation error"

      mstat = 0
      if ( allocated(s % errmsg) ) then
          deallocate (s % errmsg, stat = mstat)
      end if

      if (mstat /= 0) then
          error stop errmsg
      end if

      return
  end subroutine


  module recursive subroutine finalizer (vector)
      type(vector_t), intent(inout) :: vector

      call deallocator (vector % begin)
      call deallocator (vector % avail)
      call deallocator (vector % limit)
      call deallocator (vector % array)
      call deallocator (vector % state)
      call deallocator (vector % deref)

      return
  end subroutine


end submodule


! References:
! SJ Chapman, FORTRAN for Scientists and Engineers, fourth edition
! A Koenig and B Moo, Accelerated C++ Practical Programming by Example
