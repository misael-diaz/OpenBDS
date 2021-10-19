!   source: Vector_type_oblivious_methods.for
!   author: misael-diaz
!   date:   2021-06-28
!
!
!   Synopsis:
!   Defines type-oblivious methods of the vector class.
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

submodule (VectorClass) vector_type_oblivious_methods
implicit none
contains


  module function default_constructor () result(vector)
      ! Synopsis: Returns an empty vector
      type(vector_t), allocatable :: vector
      integer(kind = int32) :: mstat
      character(*), parameter :: name = 'dynamic::vector():'
      character(*), parameter :: errmsg = name // ' ' // &
          & 'allocation error'

      call alloc        !! allocates memory for vector
      call init         !! initializes vector components

      return
      contains

          subroutine alloc

              allocate (vector, stat=mstat)
              if (mstat /= 0) then
                  error stop errmsg
              end if

              return
          end subroutine


          subroutine init

              call instantiate (vector)

              return
          end subroutine

  end function


  module subroutine instantiate (vector)
      type(vector_t), intent(inout) :: vector

      call allocator (vector)


      vector % begin % idx  = 0_int64
      vector % avail % idx  = 0_int64
      vector % limit % idx  = 0_int64
      vector % deref % idx  = 0_int64
      vector % deref % it   => null()
      vector % state % init = .false.

      return
  end subroutine


  module function size_method (self) result(vector_size)
      ! Synopsis: Returns the size of the vector.
      class(vector_t), intent(in) :: self
      integer(kind = int64) :: vector_size

      if ( .not. allocated(self % state) ) then
          vector_size = 0_int64
      else

          associate (begin => self % begin % idx, &
                   & end   => self % avail % idx)
              vector_size = end - begin
          end associate

      end if

      return
  end function


  module subroutine clear_method (self)
      ! Synopsis: Clears the vector elements.
      class(vector_t), intent(inout) :: self

      call is_instantiated (self)

      self % avail % idx = 0_int64
      self % deref % it  => null()
      return
  end subroutine


  module subroutine vector_vector_t_copy_method (self, vector)
      class(vector_t), intent(inout) :: self
      class(vector_t), intent(in) :: vector


      if ( loc(self) /= loc(vector) ) then

          if ( allocated(vector % array) ) then

              if ( allocated(vector % array % values) ) then
                  call vector_vector_t_copy (self, vector)
              end if

          else

              call instantiate (self)

          end if

      end if


      return
  end subroutine


  module subroutine vector_validate_iterator_method (self)
      class(vector_t), intent(inout) :: self
      call vector_validate_iterator (self)
      return
  end subroutine


  module subroutine vector_vector_t_copy (to, from)
      ! implements copying a vector into another
      type(vector_t), intent(out), target :: to
      type(vector_t), intent(in) :: from
      type(vector_t), allocatable :: aryvec(:)
      type(vector_t) :: vec
      integer(kind = int64), allocatable :: aryi64(:)
      integer(kind = int32), allocatable :: aryi32(:)
      real(kind = real64), allocatable :: aryr64(:)
      integer(kind = int64) :: bounds(0:1)
      integer(kind = int64) :: lb
      integer(kind = int64) :: ub
      real(kind = real64), parameter :: r64 = 0.0_real64
      integer(kind = int64), parameter :: i64 = 0_int64
      integer(kind = int32), parameter :: i32 = 0_int32
      character(len=*), parameter :: errmsg = &
          & "dynamic::vector.copy: unimplemented vector<T>"
      character(len=*), parameter :: unexpected = &
          & "dynamic::vector.copy: unexpected error"
      character(len=*), parameter :: name = 'dynamic::vector.error:'
      character(len=*), parameter :: errmsg_i32 = name // ' ' // &
          & 'container of 32-bit integers'
      character(len=*), parameter :: errmsg_i64 = name // ' ' // &
          & 'container of 64-bit integers'
      character(len=*), parameter :: errmsg_r64 = name // ' ' // &
          & 'container of 64-bit reals'
      character(len=*), parameter :: errmsg_vec = name // ' ' // &
          & 'container of vectors'

      call limit        !! defines vector limits
      call init         !! initializes (destination) vector
      call fetch        !! fetches data for copying
      call clone        !! clones the type of the internal array
      call error        !! sets the error message of (destination) vector
      call copy         !! copies data into (destination) vector
      call valid        !! validates iterators
!!    call debug

      to % state % init = .true.

      return
      contains

          subroutine limit

              ! defines internal array bounds of (source) vector
              lb = from % begin % idx
              ub = from % avail % idx - 1_int64

              ! defines the size limit of the (destination) vector
              bounds(0) = from % begin % idx
              bounds(1) = from % limit % idx

              return
          end subroutine


          subroutine init
              ! Synopsis:
              ! Initializes (destination) fields from (source) vector while
              ! leaving the `avail' field and the `iterator' to be set by
              ! the generic `push' method at an appropriate time.

              call instantiate (to)

              to % begin % idx = from % begin % idx
              to % limit % idx = from % limit % idx
              to % deref % idx = from % deref % idx

              return
          end subroutine


          subroutine fetch
              ! fetches (source) data and puts it in a suitable placeholder

              associate (values => from % array % values)
                  select type (values)
                      type is ( integer(kind = int32) )
                          call backup (from, aryi32)
                      type is ( integer(kind = int64) )
                          call backup (from, aryi64)
                      type is ( real(kind = real64) )
                          call backup (from, aryr64)
                      type is (vector_t)
                          call backup (from, aryvec)
                      class default
                          error stop errmsg
                      end select
              end associate

              return
          end subroutine


          subroutine clone
              ! clones the type of the internal array from (source)

              associate (values => from % array % values)
                  select type (values)
                      type is ( integer(kind = int32) )
                          call allocator (bounds, to % array % values, i32)
                      type is ( integer(kind = int64) )
                          call allocator (bounds, to % array % values, i64)
                      type is ( real(kind = real64) )
                          call allocator (bounds, to % array % values, r64)
                      type is (vector_t)
                          call allocator (bounds, to % array % values, vec)
                      class default
                          error stop errmsg
                      end select
              end associate

              return
          end subroutine


          subroutine error
              ! defines the error message of the (destination) vector

              associate (values => from % array % values)
                  select type (values)

                      type is ( integer(kind = int32) )

                          call allocator      (to, errmsg_i32)
                          to % state % errmsg(:) = errmsg_i32

                      type is ( integer(kind = int64) )

                          call allocator      (to, errmsg_i64)
                          to % state % errmsg(:) = errmsg_i64

                      type is ( real(kind = real64) )

                          call allocator      (to, errmsg_r64)
                          to % state % errmsg(:) = errmsg_r64

                      type is (vector_t)

                          call allocator      (to, errmsg_vec)
                          to % state % errmsg(:) = errmsg_vec

                      class default
                          error stop errmsg

                  end select
              end associate

              return
          end subroutine


          subroutine copy
              ! copies data from placeholder into (destination) vector

              associate (values => to % array % values)
                  select type (values)
                      type is ( integer(kind = int32) )
                          call push (to, aryi32)
                      type is ( integer(kind = int64) )
                          call push (to, aryi64)
                      type is ( real(kind = real64) )
                          call push (to, aryr64)
                      type is (vector_t)
                          call push (to, aryvec)
                      class default
                          error stop unexpected
                  end select
              end associate

              return
          end subroutine


          subroutine valid
              ! validates iterators by re-associating them

              call vector_validate_iterator (to)

              return
          end subroutine


          subroutine debug
              ! prints the addresses of the internal array and iterator

              class(*), pointer, contiguous :: iter(:) => null()
              integer(kind = int64) :: i

              iter => to % deref % it
              select type (iter)
                  type is (vector_t)

                      do i = 1_int64, size(iter, kind = int64)
                          call iter(i) % addr()
                      end do

              end select

              return
          end subroutine

  end subroutine vector_vector_t_copy


  subroutine vector_print_container_address_method (self)
      class(vector_t), intent(in) :: self


      print *, 'address(array, iter): ', loc( self % array % values ), &
                                       & loc( self % deref % it )

      return
  end subroutine


  module recursive subroutine vector_validate_iterator (vector)
      ! validates iterators by re-associating them

      type(vector_t), intent(inout), target :: vector
      integer(kind = int64) :: i, b, e

      if ( .not. allocated(vector % array) ) then
          call instantiate (vector)             !! caters `empty' vectors
      else
          if ( .not. allocated (vector % array % values) ) then
              vector % deref % it => null()
          else
              call assoc
          end if
      end if

      return
      contains

      recursive subroutine assoc

      associate (begin => vector % begin % idx, &
               & avail => vector % avail % idx, &
               & ary => vector % array % values)

          select type (ary)

              type is (vector_t)                !! vector of vectors


                  do i = begin, (avail - 1_int64)

                      call vector_validate_iterator ( ary(i) )

                      b = ary(i) % begin % idx
                      e = ary(i) % avail % idx - 1_int64
                      if (e >= b) then
                          ! caters `empty' vectors
                          ary(i) % deref % it => ary(i) % array % values(b:e)
                      else
                          ary(i) % deref % it => null()
                      end if

                  end do


              type is ( integer(kind = int32) ) !! vector<int32_t>

                  b = vector % begin % idx
                  e = vector % avail % idx - 1_int64
                  vector % deref % it => vector % array % values(b:e)

              type is ( integer(kind = int64) ) !! vector<int64_t>

                  b = vector % begin % idx
                  e = vector % avail % idx - 1_int64
                  vector % deref % it => vector % array % values(b:e)

              type is ( real(kind = real64) )   !! vector<real64_t>

                  b = vector % begin % idx
                  e = vector % avail % idx - 1_int64
                  vector % deref % it => vector % array % values(b:e)

              class default
                  error stop 'validate iterators: unexpected error'

          end select

      end associate
      return
      end subroutine

  end subroutine vector_validate_iterator

end submodule


! References:
! SJ Chapman, FORTRAN for Scientists and Engineers, fourth edition
! A Koenig and B Moo, Accelerated C++ Practical Programming by Example


! subroutine copy_method()
! Caters self-assignment by checking the memory addresses and this is also
! why the intent(inout) is used for self. If the user attempts a
! self-assignment the data contained shouldn't be destroyed which it would
! if the intent(out) was used. (Recall that allocatable dummy arguments
! with intent(out) are deallocated automatically.
!
! I wanted to define the array temporaries (placeholder) as a polymorphic
! type but the GNU Fortran Compiler did not let me. I am aware that OOP
! is not fully supported by it. The workaround is to use allocatable
! arrays of the appropriate type.
!
! Notes:
! [0] lb, ub are aliases for ary_bounds(0) and ary_bounds(1), where ary
!     stands for array
! [1] An exact copy implies that the vector components match the type,
!     values, and allocation size. Only the actual values [begin, avail)
!     are ultimately copied into the vector.
