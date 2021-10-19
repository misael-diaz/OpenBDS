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


  module subroutine vector_vector_t_push_back_array_method (self, array)
      ! Synopsis: Pushes array unto back of vector.
      class(vector_t), intent(inout) :: self
      type(vector_t), intent(in) :: array(:)
      call back_inserter (self, array)
      return
  end subroutine


  module subroutine vector_vector_t_push_back (vector, value)
      ! Synopsis: Pushes vector unto back of vector.
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


  module subroutine vector_vector_t_push_back_array (vector, array)
      ! Synopsis: Pushes vector array unto back of vector.
      type(vector_t), intent(inout) :: vector
      type(vector_t), intent(in) :: array(:)


      call is_instantiated (vector)


      if ( vector % state % init ) then
          call insert_back (vector, array)
      else
          call initializer (vector, array)
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


  module subroutine vector_vector_t_insert_back_array (vector, array)
      ! Synopsis: Inserts array unto back, grows vector if needed.
      type(vector_t), intent(inout) :: vector
      integer(kind = int64) :: numel, avail, limit
      type(vector_t), intent(in) :: array(:)

      numel = size(array = array, dim = 1, kind = int64)
      avail = vector % avail % idx
      limit = vector % limit % idx

      if ( numel > (limit - avail) ) then
          call grow (vector, array)
      end if

      call push (vector, array)

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


  module subroutine vector_vector_t_push_array (vector, array)
      ! Synopsis: pushes value unto the back of vector.
      type(vector_t), intent(inout), target :: vector
      type(vector_t), intent(in) :: array(:)
      integer(kind = int64) :: numel
      integer(kind = int64) :: final


      numel = size(array = array, dim = 1, kind = int64)

      associate (begin  => vector % begin % idx,  &
               & avail  => vector % avail % idx,  &
               & values => vector % array % values)

          final = avail + numel - 1_int64

          select type (values)
              type is (vector_t)
                  values (avail:final) = array(:)
              class default
                  ! caters inserting mixed-types
                  error stop vector % state % errmsg
          end select

          vector % deref % it => vector % array % values(begin:final)
          avail = avail + numel

      end associate


      return
  end subroutine vector_vector_t_push_array


  module subroutine vector_vector_t_push_n_copies (vector, n, value)
      ! Synopsis: pushes `n' copies of value unto the back of vector.
      type(vector_t), intent(inout), target :: vector
      type(vector_t), intent(in) :: value
      integer(kind = int64), intent(in) :: n
      integer(kind = int64) :: numel
      integer(kind = int64) :: final


      numel = n

      associate (begin  => vector % begin % idx,  &
               & avail  => vector % avail % idx,  &
               & values => vector % array % values)

          final = avail + numel - 1_int64

          select type (values)
              type is (vector_t)
                  values (avail:final) = value
              class default
                  ! caters inserting mixed-types
                  error stop vector % state % errmsg
          end select

          vector % deref % it => vector % array % values(begin:final)
          avail = avail + numel

      end associate


      return
  end subroutine vector_vector_t_push_n_copies


  module subroutine vector_vector_t_grow (vector, value)
      ! Synopsis: Doubles the vector size.
      type(vector_t), intent(inout) :: vector
      type(vector_t), intent(in) :: value
      type(vector_t), allocatable :: array(:)

      call backup  (vector, array)
      call double  (vector)
      call restore (vector, array, value)

      return
  end subroutine vector_vector_t_grow


  module subroutine vector_vector_t_growArray (vector, values)
      ! Synopsis: Grows vector so that it can store the array `values'.
      type(vector_t), intent(inout) :: vector
      integer(kind = int64) :: alloc, avail, numel
      type(vector_t), intent(in) :: values(:)
      type(vector_t), allocatable :: array(:)
      type(vector_t) :: value

      avail = vector % avail % idx
      numel = size(array = values, dim = 1, kind = int64)
      alloc = avail + numel

      call backup  (vector, array)
      call double  (vector, alloc)
      call restore (vector, array, value)

      return
  end subroutine


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


  module subroutine vector_vector_t_initializerArray (vector, array)
      type(vector_t), intent(inout) :: vector
      type(vector_t), intent(in) :: array(:)
      call create (vector, array)
      return
  end subroutine


  module subroutine vector_vector_t_create (vector, value)
      ! Synopsis: Creates the first element in vector.
      type(vector_t), intent(inout), target :: vector
      integer(kind = int64), parameter :: lb = 0_int64
      integer(kind = int64), parameter :: ub = VECTOR_MIN_SIZE
      integer(kind = int64), parameter :: bounds(0:1) = [lb, ub]
      type(vector_t), intent(in) :: value
      character(len=*), parameter :: errmsg = &
          & "dynamic::vector.error: container of vectors"


      ! allocates memory for vector
      call allocator (vector, errmsg)
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


  module subroutine vector_vector_t_createArray (vector, array)
      ! Synopsis: Creates vector from array.
      type(vector_t), intent(inout) :: vector
      integer(kind = int64) :: numel
      integer(kind = int64) :: bounds(0:1)
      type(vector_t), intent(in) :: array(:)
      type(vector_t) :: value
      character(len=*), parameter :: errmsg = &
          & "dynamic::vector.error: container of vectors"

      ! tailors vector for storing array
      numel = size(array = array, dim = 1, kind = int64)
      call double (vector, max(VECTOR_MIN_SIZE, numel) )

      ! allocates memory for vector components
      bounds(0) = 0_int64
      bounds(1) = vector % limit % idx
      call allocator (bounds, vector % array % values, value)
      call allocator (vector, errmsg)

      ! initializes vector components
      vector % state % errmsg(:) = errMSG
      call push (vector, array)
      vector % state % init = .true.

      return
  end subroutine vector_vector_t_createArray

end submodule


! References:
! SJ Chapman, FORTRAN for Scientists and Engineers, fourth edition
! A Koenig and B Moo, Accelerated C++ Practical Programming by Example
