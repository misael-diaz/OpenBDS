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
! own in fortran. I borrowed some of their ideas to implement it.


! Comments:
! A vector is a container that holds values of the same type. The
! "default class" in select type constructs sometimes is used as a
! mechanism that saves the user from catastrophy:
! An array in Fortran can only hold values of the same type and so the
! vector class which uses the Fortran array as the underlying data
! structure to store values.


! Comments:
! For now iterators are implemented as indexes.
!


! Comments on Procedures:
!
! subroutine vector_int32_t_create()
! Allocates one more element on purpose to compute the size of the
! vector via: (end - begin) as in c++.
!
!
! function findloc_wrapper_method (self, value) result(idx)
! lower and upper bounds (lb, ub) are chosen so that we do not include
! the element pointed to by (end). It's a valid index but it should not
! be referenced since it does not hold an actual value.
!
! Index returned by findloc intrinsic has been adjusted by one to
! account for the array bounds. I am unsure if this a BUG in the
! implementation of findloc or a misinterpretation on my part.
! Both the Intel and GNU Fortran Compilers yield the same result.
! I cannot conclude anything from that but at least account for it
! when using the compilers I have available.
!
! Documentation of findloc from GNU Fortran Compiler:
! Determines the *location* of the element in the array with the value
! given in the VALUE argument ... They used *location* instead of index
! so maybe the user is responsible for obtaining the index from the
! location as it has been done here.
!
!
! subroutine clear_method()
! Placing the /avail/ iterator at the beginning is equivalent to
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
