!
!   source: Iterator-methods.for
!   author: misael-diaz
!   date:   2021-10-28
!
!
!   Synopsis:
!   Defines the methods of the random-access iterator class.
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

submodule (RandomAccessIteratorClass) iterator_methods
implicit none
contains

  module subroutine it_clear_method (iter)
      class(iter_t), intent(inout) :: iter

      call iter % vector % clear ()

      return
  end subroutine


  module subroutine it_insert_method (iter, p)
      class(iter_t), intent(inout) :: iter
      type(pointer_t), intent(in) :: p

      call it_insert (iter, p)

      return
  end subroutine


  module subroutine it_remove_method (iter)
      class(iter_t), intent(inout) :: iter

      call it_remove (iter)

      return
  end subroutine

end submodule

! References:
! SJ Chapman, FORTRAN for Scientists and Engineers, fourth edition
! A Koenig and B Moo, Accelerated C++ Practical Programming by Example
