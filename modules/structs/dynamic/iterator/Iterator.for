!
!   source: Iterator.for
!   author: misael-diaz
!   date:   2021-10-28
!
!
!   Synopsis:
!   Implements a random-access iterator class.
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

module RandomAccessIteratorClass
  use, intrinsic :: iso_fortran_env, only: int32, int64
  use VectorClass, only: vector_t, pointer_t
  implicit none
  private


  type, public :: iter_t        !! random-access iterator class
    private
    type(vector_t), allocatable :: vector
    class(*), pointer, contiguous, public :: deref(:) => null()
    contains
      private
      procedure, public :: clear  => it_clear_method
      procedure, public :: insert => it_insert_method
      procedure, public :: remove => it_remove_method
      final :: it_destructor
  end type


  interface iter_t
      module procedure :: it_default_constructor
  end interface


  interface instantiate
      module procedure :: it_instantiate
  end interface


  interface is_instantiated
      module procedure :: it_is_instantiated
  end interface


  interface

    module function it_default_constructor () result(iter)
        type(iter_t) :: iter
    end function

  end interface


  interface

    module subroutine it_clear_method (self)
        class(iter_t), intent(inout) :: self
    end subroutine


    module subroutine it_insert_method (self, p)
        class(iter_t), intent(inout) :: self
        type(pointer_t), intent(in) :: p
    end subroutine


    module subroutine it_remove_method (self)
        class(iter_t), intent(inout) :: self
    end subroutine

  end interface


  interface

    module subroutine it_insert (iter, p)
        type(iter_t), intent(inout), target :: iter
        type(pointer_t), intent(in) :: p
    end subroutine


    module subroutine it_remove (iter)
        type(iter_t), intent(inout), target :: iter
    end subroutine

  end interface


  interface

    module subroutine it_is_instantiated (iter)
        type(iter_t), intent(inout) :: iter
    end subroutine


    module subroutine it_instantiate (iter)
        type(iter_t), intent(inout), target :: iter
    end subroutine

  end interface


  interface

    module subroutine it_destructor (iter)
        type(iter_t), intent(inout) :: iter
    end subroutine

  end interface

end module

! References:
! SJ Chapman, FORTRAN for Scientists and Engineers, fourth edition
! A Koenig and B Moo, Accelerated C++ Practical Programming by Example


! TODO:
! [x] implement the `insert' method as an interface to the push-back method
!     of the underlying vector.
