!
!   source: Iterator-implementations.for
!   author: misael-diaz
!   date:   2021-10-28
!
!
!   Synopsis:
!   Implements the random-access iterator class.
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

submodule (RandomAccessIteratorClass) iterator_implementations
implicit none
contains

  module function it_default_constructor () result(iter)
      type(iter_t) :: iter

      call instantiate (iter)

      return
  end function


  module subroutine it_instantiate (iter)
      type(iter_t), intent(inout), target :: iter
      integer(kind = int32) :: mstat
      character(*), parameter :: errmsg = 'iterator: allocation error'

      allocate (iter % vector, stat=mstat)
      if (mstat /= 0) error stop errmsg

      iter % deref => null()

      return
  end subroutine


  module subroutine it_insert (iter, p)
      type(iter_t), intent(inout), target :: iter
      type(pointer_t), intent(in) :: p

      call it_is_instantiated (iter)

      call iter % vector % push_back (p)
      iter % deref => iter % vector % deref % it

      return
  end subroutine


  module subroutine it_is_instantiated (iter)
      type(iter_t), intent(inout) :: iter

      if ( .not. allocated(iter % vector) ) then
          call it_instantiate (iter)
      end if

      return
  end subroutine


  module subroutine it_destructor (iter)
      type(iter_t), intent(inout) :: iter
      integer(kind = int32) :: mstat
      character(*), parameter :: errmsg = 'iterator: deallocation error'

      if ( allocated(iter % vector) ) then
          deallocate (iter % vector, stat=mstat)
          if (mstat /= 0) error stop errmsg
      end if

      iter % deref => null()

      return
  end subroutine

end submodule

! References:
! SJ Chapman, FORTRAN for Scientists and Engineers, fourth edition
! A Koenig and B Moo, Accelerated C++ Practical Programming by Example
