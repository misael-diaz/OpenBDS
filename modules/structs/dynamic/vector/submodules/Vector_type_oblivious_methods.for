!
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
      type(vector_t):: vector

      call instantiate (vector)

      return
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

              call deallocator (self % array)
              call allocator   (self)
              call instantiate (self)

          end if

      end if


      return
  end subroutine


  module subroutine vector_vector_t_copy (to, from)
      type(vector_t), intent(out), target :: to
      type(vector_t), intent(in) :: from
      type(vector_t), allocatable :: aryvec(:)
      type(vector_t) :: vec
      integer(kind = int32), allocatable :: aryi32(:)
      integer(kind = int64), allocatable :: aryi64(:)
      integer(kind = int64) :: ary_bounds(0:1)
      integer(kind = int64) :: vec_bounds(0:1)
      integer(kind = int64) :: first
      integer(kind = int64) :: last
      integer(kind = int64) :: lb
      integer(kind = int64) :: ub
      integer(kind = int64) :: i64
      integer(kind = int32) :: i32
      character(len=*), parameter :: errmsg = &
          & "dynamic::vector.copy: unimplemented vector<T>"


      i32 = 0_int32
      i64 = 0_int64


      call allocator (to)


      to % begin % idx = from % begin % idx
      to % avail % idx = from % avail % idx
      to % limit % idx = from % limit % idx
      to % deref % idx = from % deref % idx


      lb            = from % begin % idx
      ub            = from % avail % idx

      ary_bounds(0) = from % begin % idx
      ary_bounds(1) = from % avail % idx

      vec_bounds(0) = from % begin % idx
      vec_bounds(1) = from % limit % idx


      ! copies from (source) vector into a suitable placeholder
      associate (values => from % array % values)

          select type (values)

              type is ( integer(kind = int32) )

                  call allocator (ary_bounds, aryi32)
                  call allocator (vec_bounds, to % array % values, &
                                & i32)

                  aryi32(:) = values(lb:ub)
                  to % state % errmsg(:) = "vector<int32_t>"


              type is ( integer(kind = int64) )


                  call allocator (ary_bounds, aryi64)
                  call allocator (vec_bounds, to % array % values, &
                                & i64)

                  aryi64(:) = values(lb:ub)
                  to % state % errmsg(:) = "vector<int64_t>"


              type is (vector_t)


                  call allocator (ary_bounds, aryvec)
                  call allocator (vec_bounds, to % array % values, &
                                & vec)

                  aryvec(:) = values(lb:ub)
                  to % state % errmsg(:) = "vector<vector_t>"


              class default

                  error stop errmsg

          end select

      end associate


      ! copies into (destination) vector
      associate (values => to % array % values)

          select type (values)

              type is ( integer(kind = int32) )
                  values(lb:ub) = aryi32(:)
              type is ( integer(kind = int64) )
                  values(lb:ub) = aryi64(:)
              type is (vector_t)
                  values(lb:ub) = aryvec(:)
              class default
                  error stop "dynamic::vector.copy: unexpected error"

          end select

      end associate

      first = to % begin % idx
      last  = to % avail % idx - 1_int64
      to % deref % it   => to % array % values(first:last)
      to % state % init = .true.

      return
  end subroutine


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
