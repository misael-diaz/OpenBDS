!
!   source: ListClass.for
!   author: misael-diaz
!   date:   2021-10-25
!
!
!   Synopsis:
!   Possible implementation of the linked-list class in FORTRAN.
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

module ListClass
  use, intrinsic :: iso_fortran_env, only: int32, int64
  use VectorClass, only: pointer_t
  implicit none
  private
  public :: link_conservative_destructor
  public :: link_aggressive_destructor

  type, public :: link_t        !! link<*node_t>: pointer to node object
    type(pointer_t) :: node
    contains
      private
      final :: link_finalizer
  end type

  type, public :: data_t
    type(pointer_t) :: data
    contains
      private
      final :: data_destructor
  end type

  type, public :: node_t        !! node<*data_t>: pointer to data object
    type(data_t) :: item
    type(link_t) :: next
    contains
      private
      procedure :: node_assign_method
      generic, public :: assignment(=) => node_assign_method
      final :: node_finalizer
  end type

  type, public :: list_t
    type(link_t) :: head
    type(link_t) :: tail
    contains
      private
      final :: list_finalizer
  end type


  interface list_t
      module procedure :: list_default_constructor
  end interface


  interface node_t
      module procedure :: node_default_constructor
      module procedure :: node_int32_t_constructor
  end interface


  interface

    module function list_default_constructor () result(list)
        type(list_t) :: list
    end function


    module subroutine list_finalizer (list)
        type(list_t), intent(inout) :: list
    end subroutine

  end interface


  interface

    module recursive subroutine link_conservative_destructor (link)
        type(link_t), intent(inout) :: link
    end subroutine


    module recursive subroutine link_aggressive_destructor (link)
        type(link_t), intent(inout) :: link
    end subroutine


    module recursive subroutine link_destructor (link)
        type(link_t), intent(inout) :: link
    end subroutine


    module recursive subroutine link_finalizer (link)
        type(link_t), intent(inout) :: link
    end subroutine

  end interface


  interface

    module subroutine node_assign_method (self, node)
        class(node_t), intent(inout) :: self
        type(node_t), intent(in) :: node
    end subroutine


    module function node_default_constructor () result(node)
        type(node_t) :: node
    end function


    module function node_int32_t_constructor (value) result(node)
        type(node_t) :: node
        integer(kind = int32), intent(in) :: value
    end function


    module recursive subroutine node_finalizer (node)
        type(node_t), intent(inout) :: node
    end subroutine

  end interface


  interface

    module recursive subroutine data_destructor (data)
        type(data_t), intent(inout) :: data
    end subroutine

  end interface

end module
