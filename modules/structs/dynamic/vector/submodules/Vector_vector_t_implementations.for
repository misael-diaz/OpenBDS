!
!   source: Vector_vector_t_implementations.for
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

submodule (VectorClass) vector_vector_t_implementation
implicit none
contains


  module function vector_vector_t_fillConstructor (n, value) result(vec)
      ! Synopsis: Creates a vector having `n' copies of `value'.
      type(vector_t), allocatable :: vec
      type(vector_t), intent(in) :: value
      integer(kind = int64), intent(in) :: n
      class(*), pointer, contiguous :: iter(:) => null()
      integer(kind = int32) :: mstat
      integer(kind = int64) :: bounds(0:1)
      integer(kind = int64) :: b, e, idx
      character(*), parameter :: errMSG = &
          & "vector(): the number of copies must be a positive integer"

      call check                !! complains on invalid inputs
      call alloc                !! allocates memory for vector
      call instantiate (vec)    !! initializes the vector components
      call tailor               !! tailors the vector to store `n' copies


      ! pushes `n' copies of the value `value' unto vector
      call push (vec, n, value)
      call valid                !! validates iterators

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


          subroutine tailor
              ! tailors the vector size based on the number of copies

              vec % limit % idx = n     !! sets the vector `limit' to fit
              call double (vec)         !! and doubles it for convenience

              bounds(0) = 0_int64
              bounds(1) = vec % limit % idx
              call allocator (bounds, vec % array % values, value)

              return
          end subroutine


          subroutine valid
              ! recreates iterators to validate them

              iter => vec % deref % it
              select type (iter)
                  type is (vector_t)
                      do idx = 1, n

                          call allocator (iter(idx) % deref)
                          b = iter(idx) % begin % idx
                          e = iter(idx) % avail % idx - 1_int64
                          associate (values => iter(idx) % array % values)
                              iter(idx) % deref % it => values(b:e)
                          end associate

                      end do
                  class default
                      error stop 'vector(): unexpected error'
              end select

              return
          end subroutine

  end function vector_vector_t_fillConstructor


  module subroutine vector_vector_t_indexer (vector, idx, value)
      type(vector_t), intent(in) :: vector
      type(vector_t), intent(inout) :: value
      integer(kind = int64), intent(in) :: idx

      associate (values => vector % array % values)

          select type (values)
              type is (vector_t)
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
