!
!   source: Vector_vector_t_implementations.for
!   author: misael-diaz
!   date:   2021-06-27
!
!
!   Synopsis:
!   Implements the push-back method of the vector class.
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

submodule (VectorClass) vector_vector_t_push_back_methods
implicit none
contains


  module subroutine vector_vector_t_push_back_method (self, value)
      ! Synopsis: Pushes value unto back of vector.
      class(vector_t), intent(inout) :: self
      type(vector_t), intent(in) :: value
      call back_inserter (self, value)
      return
  end subroutine


  module subroutine vector_vector_t_push_back (vector, value)
      ! Synopsis: Pushes 32-bit integer unto back of vector.
      type(vector_t), intent(inout) :: vector
      type(vector_t), intent(in) :: value


      call is_instantiated (vector)


      if ( vector % state % init ) then
          call insert_back (vector, value)
      else
          call initializer (vector, value)
      end if

      return
  end subroutine


  module subroutine vector_vector_t_insert_back (vector, value)
      ! Synopsis: Inserts value unto back, vector grows as needed.
      type(vector_t), intent(inout) :: vector
      type(vector_t), intent(in) :: value


      if (vector % avail % idx == vector % limit % idx) then
          call grow (vector, value)
      end if

      call push (vector, value)


      return
  end subroutine


  module subroutine vector_vector_t_push (vector, value)
      ! Synopsis: pushes value unto the back of vector.
      type(vector_t), intent(inout), target :: vector
      type(vector_t), intent(in) :: value


      associate (begin  => vector % begin % idx,  &
               & avail  => vector % avail % idx,  &
               & values => vector % array % values)

          select type (values)
              type is (vector_t)
                  values (avail) = value
              class default
                  ! caters inserting mixed-types
                  error stop vector % state % errmsg
          end select

          vector % deref % it => vector % array % values(begin:avail)
          avail = avail + 1_int64

      end associate


      return
  end subroutine vector_vector_t_push


  module subroutine vector_vector_t_grow (vector, value)
      ! Synopsis: Doubles the vector size.
      type(vector_t), intent(inout) :: vector
      type(vector_t), intent(in) :: value
      type(vector_t), allocatable :: array(:)

      call backup  (vector, array)

      vector % limit % idx = 2_int64 * vector % limit % idx !! doubles size

      call restore (vector, array, value)

      return
  end subroutine vector_vector_t_grow


  subroutine vector_vector_t_backup (vector, array)
      ! Synopsis:
      ! Creates backup of data contained in vector.
      type(vector_t), intent(in) :: vector
      integer(kind = int64):: lb
      integer(kind = int64):: ub
      integer(kind = int64):: bounds(0:1)
      type(vector_t), intent(inout), allocatable :: array(:)

      ! bounds for copying the data
      lb = vector % begin % idx
      ub = vector % avail % idx - 1_int64
      bounds(0) = lb
      bounds(1) = ub
      call allocator (bounds, array)


      ! copies existing values into placeholder
      associate (values => vector % array % values)

          select type (values)
              type is (vector_t)
                  array(:) = values(lb:ub)
              class default
                  error stop vector % state % errmsg
          end select

      end associate

      return
  end subroutine vector_vector_t_backup


  subroutine vector_vector_t_restore (vector, array, value)
      ! Synopsis:
      ! Restores vector with data in placeholder array.
      type(vector_t), intent(inout) :: vector
      integer(kind = int64):: lb
      integer(kind = int64):: ub
      integer(kind = int64):: bounds(0:1)
      type(vector_t), intent(in) :: array(:)
      type(vector_t), intent(in) :: value
      character(len=*), parameter :: errmsg = &
          & "dynamic::vector.restore: unexpected error"

      ! copying bounds
      lb = vector % begin % idx
      ub = vector % avail % idx - 1_int64
      ! grown vector bounds
      bounds(0) = vector % begin % idx
      bounds(1) = vector % limit % idx
      call reallocator (bounds, vector % array % values, value)

      ! copies values in placeholder into vector
      associate (values => vector % array % values)

         select type (values)
              type is (vector_t)
                  values(lb:ub) = array(:)
              class default
                  error stop errmsg
          end select

      end associate

      return
  end subroutine vector_vector_t_restore


  module subroutine vector_vector_t_initializer (vector, value)
      type(vector_t), intent(inout) :: vector
      type(vector_t), intent(in) :: value
      call create (vector, value)
      return
  end subroutine


  module subroutine vector_vector_t_create (vector, value)
      ! Synopsis: Creates the first element in vector.
      type(vector_t), intent(inout), target :: vector
      integer(kind = int64), parameter :: lb = 0_int64
      integer(kind = int64), parameter :: ub = VECTOR_MIN_SIZE
      integer(kind = int64), parameter :: bounds(0:1) = [lb, ub]
      type(vector_t), intent(in) :: value
      integer(kind = int32) :: mstat
      character(len=*), parameter :: errmsg = &
          & "dynamic::vector.error: container of vectors"


!     TODO: consider moving to utils of the vector class
      allocate(character(len=len(errmsg)):: vector % state % errmsg,&
             & stat = mstat)
      if (mstat /= 0) then
          error stop "dynamic::vector.create: allocation error"
      end if


      ! allocates memory for vector
      call allocator (bounds, vector % array % values, value)

      ! sets the vector state variables
      vector % begin % idx = 0_int64
      vector % avail % idx = 0_int64
      vector % limit % idx = VECTOR_MIN_SIZE
      vector % state % errmsg(:) = errMSG

      ! initializes the vector
      call push (vector, value)
      vector % state % init = .true.


      return
  end subroutine vector_vector_t_create


end submodule


! References:
! SJ Chapman, FORTRAN for Scientists and Engineers, fourth edition
! A Koenig and B Moo, Accelerated C++ Practical Programming by Example
