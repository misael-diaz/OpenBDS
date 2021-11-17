!
!   source: List_utils.for
!   author: misael-diaz
!   date:   2021-10-25
!
!
!   Synopsis:
!   Implements (particular) utilities for the linked-list class.
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

submodule (ListClass) list_utils
implicit none
contains

  module subroutine associate (node, link)
      type(link_t), intent(in), pointer :: link
      type(node_t), intent(inout), pointer :: node
      class(*), pointer :: h_node => null()
      character(*), parameter :: errmsg = &
          & 'node<*data_t>.associate(): unexpected type error'

      if ( associated(link) ) then

          if ( associated(link % node % p) ) then

              h_node => link % node % p
              select type (h_node)
                  type is (node_t)
                      node => h_node
                  class default
                      error stop errmsg
              end select

          else
              node => null()
          end if

      else
              node => null()
      end if

      h_node => null()

      return
  end subroutine

end submodule


! References:
! SJ Chapman, FORTRAN for Scientists and Engineers, fourth edition
! A Koenig and B Moo, Accelerated C++ Practical Programming by Example
