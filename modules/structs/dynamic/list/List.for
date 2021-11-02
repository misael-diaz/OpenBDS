!
!   source: ListClass.for
!   author: misael-diaz
!   date:   2021-10-25
!
!
!   Synopsis:
!   Possible implementation of a doubly linked-list class in FORTRAN.
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

module DoublyLinkedListClass
  use, intrinsic :: iso_fortran_env, only: int32, int64
  use VectorClass, only: pointer_t
  use RandomAccessIteratorClass, only: iter_t
  implicit none
  private

  type :: link_t        !! link<*node_t>: pointer to node object
    private
    type(pointer_t) :: node
    contains
      private
      final :: link_finalizer
  end type

  type :: data_t
    private
    type(pointer_t) :: data
    contains
      private
      final :: data_destructor
  end type

  type :: node_t        !! node<*data_t>: pointer to data object
    private
    type(data_t) :: item
    type(link_t) :: next
    type(link_t) :: prev
    contains
      private
      procedure :: node_assign_method
      generic, public :: assignment(=) => node_assign_method
      final :: node_finalizer
  end type

  type, public :: list_t
    private
    type(link_t) :: head
    type(link_t) :: tail
    contains
      private
      procedure :: list_int32_t_append_method
      procedure, public :: iterator => list_random_access_iterator_method
      procedure, public :: validate => list_validate_iterator_method
      generic, public :: append => list_int32_t_append_method
      final :: list_finalizer
  end type


  interface list_t
      module procedure :: list_default_constructor
  end interface


  interface node_t
      module procedure :: node_default_constructor
      module procedure :: node_int32_t_constructor
  end interface



  interface iterator
      module procedure :: list_genIterator
  end interface


  interface append
      module procedure :: list_int32_t_append
  end interface


  interface create
      module procedure :: node_int32_t_create
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

    module subroutine list_validate_iterator_method (self, iter)
        class(list_t), intent(in) :: self
        type(iter_t), intent(inout) :: iter
    end subroutine


    module function list_random_access_iterator_method (self) result(iter)
        type(iter_t) :: iter
        class(list_t), intent(in) :: self
    end function


    module subroutine list_int32_t_append_method (self, value)
        class(list_t), intent(inout) :: self
        integer(kind = int32), intent(in) :: value
    end subroutine

  end interface


  interface

    module subroutine list_genIterator (link, iter)
        type(link_t), intent(in), target :: link
        type(iter_t), intent(inout) :: iter
    end subroutine


    module recursive subroutine list_recursive_genIterator (link, iter)
        type(link_t), intent(in), target :: link
        type(iter_t), intent(inout) :: iter
    end subroutine !! unused


    module subroutine list_int32_t_append (list, value)
        type(list_t), intent(inout) :: list
        integer(kind = int32), intent(in) :: value
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

    module subroutine node_int32_t_create (link, value)
        type(link_t), intent(inout) :: link
        integer(kind = int32), intent(in) :: value
    end subroutine


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


  interface

    module subroutine associate (node, link)
        type(link_t), intent(in), target :: link
        type(node_t), intent(inout), pointer :: node
    end subroutine

  end interface

end module
