!
!   source: Vector_int32_t_implementations.for
!   author: misael-diaz
!   date:   2021-06-27
!
!
!   Synopsis:
!   Implementation procedures of the vector class.
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

submodule (VectorClass) vector_int32_t_implementation
implicit none
contains


  module function vector_int32_t_arrayConstructor (array) result(vec)
      ! Synopsis: Creates a vector from array.
      type(vector_t), allocatable :: vec
      integer(kind = int64) :: numel
      integer(kind = int64) :: bounds(0:1)
      integer(kind = int32), intent(in) :: array(:)
      integer(kind = int32), parameter :: value = 0
      integer(kind = int32) :: mstat
      character(len=*), parameter :: name = 'dynamic::vector.error:'
      character(len=*), parameter :: errmsg_i32 = name // ' ' // &
          & 'container of 32-bit integers'
      character(len=*), parameter :: unimplmntd = name // ' ' // &
          & 'unimplemented vector<T>'

      call alloc                !! allocates memory for vector
      call init                 !! initializes the vector components
      call tailor               !! tailors the vector to store array
      call error                !! sets the internal error message
      call insert               !! pushes array unto back of vector
      call valid                !! validates iterator(s)

      vec % state % init = .true.

      return
      contains

          subroutine alloc
              ! allocates memory for a vector

              allocate (vec, stat=mstat)
              if (mstat /= 0) then
                  error stop 'vector(): memory allocation error'
              end if

              return
          end subroutine


          subroutine init
              ! provides initial values to the vector components

              call instantiate (vec)

              return
          end subroutine


          subroutine tailor
              ! tailors the vector size based on the array size

              numel = size(array = array, dim = 1, kind = int64)
              vec % limit % idx  = numel  !! sets the vector `limit' to fit
              call increase (vec)         !! and raises it for convenience

              bounds(0) = 0_int64
              bounds(1) = vec % limit % idx
              call allocator (bounds, vec % array % values, value)

              return
          end subroutine


          subroutine error
              ! defines the internal error message of vector

              associate (values => vec % array % values)
                  select type (values)
                      type is ( integer(kind = int32) )
                          call allocator      (vec, errmsg_i32)
                          vec % state % errmsg(:) = errmsg_i32
                      class default
                          error stop unimplmntd
                  end select
              end associate

              return
          end subroutine error


          subroutine insert
              ! inserts values unto the back of vector

              call push (vec, array)

              return
          end subroutine


          subroutine valid
              ! validates iterators by re-associating them

              call vector_validate_iterator (vec)

              return
          end subroutine

  end function vector_int32_t_arrayConstructor


  module function vector_int32_t_fillConstructor (n, value) result(vec)
      ! Synopsis: Creates a vector having `n' copies of `value'.
      type(vector_t), allocatable :: vec
      integer(kind = int64), intent(in) :: n
      integer(kind = int32), intent(in) :: value
      integer(kind = int32) :: mstat
      integer(kind = int64) :: bounds(0:1)
      character(*), parameter :: errMSG = &
          & "vector(): the number of copies must be a positive integer"
      character(len=*), parameter :: name = 'dynamic::vector.error:'
      character(len=*), parameter :: errmsg_i32 = name // ' ' // &
          & 'container of 32-bit integers'
      character(len=*), parameter :: unimplmntd = name // ' ' // &
          & 'unimplemented vector<T>'

      call check                !! complains on invalid inputs
      call alloc                !! allocates memory for vector
      call init                 !! initializes the vector components
      call tailor               !! tailors the vector to store `n' copies
      call error                !! sets the internal error message
      call insert               !! pushes `n' copies of `value' unto vector
      call valid                !! validates iterator(s)

      vec % state % init = .true.

      return
      contains

          subroutine check
              ! complains on invalid input

              if (n <= 0_int64) then
                  error stop errMSG
              end if

              return
          end subroutine


          subroutine alloc
              ! allocates memory for a vector

              allocate (vec, stat=mstat)
              if (mstat /= 0) then
                  error stop 'vector(): memory allocation error'
              end if

              return
          end subroutine


          subroutine init
              ! provides initial values to the vector components

              call instantiate (vec)

              return
          end subroutine


          subroutine tailor
              ! tailors the vector size based on the number of copies

              vec % limit % idx = n     !! sets the vector `limit' to fit
              call increase (vec)       !! and doubles it for convenience

              bounds(0) = 0_int64
              bounds(1) = vec % limit % idx
              call allocator (bounds, vec % array % values, value)

              return
          end subroutine


          subroutine error
              ! defines the internal error message of vector

              associate (values => vec % array % values)
                  select type (values)
                      type is ( integer(kind = int32) )
                          call allocator      (vec, errmsg_i32)
                          vec % state % errmsg(:) = errmsg_i32
                      class default
                          error stop unimplmntd
                  end select
              end associate

              return
          end subroutine error


          subroutine insert
              ! inserts values unto the back of vector

              call push (vec, n, value)

              return
          end subroutine


          subroutine valid
              ! validates iterators by re-associating them

              call vector_validate_iterator (vec)

              return
          end subroutine

  end function vector_int32_t_fillConstructor


  module subroutine vector_int32_t_findloc_wrapper (vector, value, i)
      type(vector_t), intent(in) :: vector
      integer(kind = int64), intent(out) :: i
      integer(kind = int64) :: lb
      integer(kind = int64) :: ub
      integer(kind = int32), intent(in) :: value


      lb = vector % begin % idx
      ub = vector % avail % idx
      ub = ub - 1_int64

      associate (values => vector % array % values)

          select type (values)
              type is ( integer(kind = int32) )
                  i = findloc(array = values(lb:ub), &
                            & value = value, dim = 1, kind = int64)
                  i = i - 1_int64
              class default
                  error stop vector % state % errmsg
          end select

      end associate
      return
  end subroutine


  module subroutine vector_int32_t_indexer (vector, idx, value)
      type(vector_t), intent(in) :: vector
      integer(kind = int64), intent(in) :: idx
      integer(kind = int32), intent(out) :: value

      associate (values => vector % array % values)

          select type (values)
              type is ( integer(kind = int32) )
                  value = values (idx)
              class default
                  error stop vector % state % errmsg
          end select

      end associate


      return
  end subroutine


end submodule


! References:
! SJ Chapman, FORTRAN for Scientists and Engineers, fourth edition
! A Koenig and B Moo, Accelerated C++ Practical Programming by Example


! The vector class presented by Koening and Moo inspired me to write my
! own in FORTRAN. I borrowed some of their ideas to implement it.


! Comments:
! A vector is a container that holds values of the same type.


! Comments on Procedures:
!
! subroutine vector_int32_t_create()
! Allocates one more element on purpose to compute the size of the
! vector via: (end - begin) as in c++.
!
! function findloc_wrapper_method (self, value) result(idx)
! lower and upper bounds (lb, ub) are chosen so that we do not include
! the element pointed to by (end). It's a valid index but it should not
! be referenced since it does not hold an actual value.
!
! subroutine clear_method()
! Placing the `avail' iterator at the beginning is equivalent to
! clearing the vector without deallocating memory. I have designed
! the vector class thinking on how it will be used for keeping
! track of neighbors. In that context it's convenient to
! clear the vector without deallocating memory since it would
! be expensive to have to grow the size of the vector over and
! over again during the simulation. Why not use fixed-size arrays?
! I have use them in the past worrying that I might have to allocate
! more memory than actually needed to avoid exceding the array
! bounds. Some systems might be more dynamic having particles with
! far more neighbors than others. Vectors would come in handy for
! such cases.
