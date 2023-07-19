!
!   source: LinkedList.for
!   author: misael-diaz
!   date:   2021-06-06
!
!
!   Synopsis:
!   Defines the linked-list class.
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

module linkedlists
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use utils, only: reallocator => util_reallocate_array
    use utils, only: deallocate_array => util_deallocate_array_int32
    implicit none
    private


    type :: link_t
        type(node_t), pointer :: node => null()
    end type


    type :: node_t
        integer(kind = int32) :: value = 0
        type(link_t) :: next
    end type


    type :: stat_t
        logical(kind = int32) :: init = .false.
    end type


    type, public :: linkedlist
        private
        type(link_t):: iter     ! iterator
        type(link_t):: head     ! begin
        type(link_t):: tail     ! end
        type(stat_t):: stat     ! status
        contains
            procedure, public :: begin => iterator_begin_method
            procedure, public :: next => iterator_advance_method
            procedure, public :: end => iterator_end_method
            procedure, public :: size => size_method
            procedure, public :: clear => clear_method
            procedure, public :: print => display_method
            procedure, public :: copy => to_array_method
            procedure, public :: push_back => push_back_method
            procedure, public :: push_front => push_front_method
            procedure, public :: insort => insertion_sort_method
            final :: finalizer
    end type


    interface linkedlist
        module procedure default_constructor
    end interface

    interface allocator
        module procedure allocate_node
    end interface

!   interface reallocator
!       module procedure reallocate_array
!   end interface

    interface deallocator
        module procedure deallocate_node
        module procedure deallocate_array
    end interface

    interface


        module function default_constructor () result(list)
            type(linkedlist):: list
        end function


        module subroutine initializer (list, value)
            type(linkedlist), intent(inout) :: list
            integer(kind = int32), intent(in) :: value
        end subroutine


        module subroutine create (list, value)
            ! Synopsis:
            ! Creates the first link of the linked-list.
            type(linkedlist), intent(inout) :: list
            integer(kind = int32), intent(in) :: value
        end subroutine


        module subroutine iterator_begin_method (self)
            class(linkedlist), intent(inout) :: self
        end subroutine


        module subroutine iterator_end_method (self)
            class(linkedlist), intent(inout) :: self
        end subroutine


        module subroutine iterator_advance_method (self)
            class(linkedlist), intent(inout) :: self
        end subroutine


        module subroutine display_method (self)
            class(linkedlist), intent(in) :: self
        end subroutine


        module subroutine to_array_method (self, values)
            ! Synopsis:
            ! Copies values into an array.
            class(linkedlist), intent(in) :: self
            integer(kind = int32), intent(inout), allocatable :: values(:)
        end subroutine


        module subroutine push_back_method (self, value)
            class(linkedlist), intent(inout):: self
            integer(kind = int32), intent(in) :: value
        end subroutine


        module subroutine push_front_method (self, value)
            class(linkedlist), intent(inout):: self
            integer(kind = int32), intent(in) :: value
        end subroutine


        module subroutine insertion_sort_method (self, value)
            class(linkedlist), intent(inout):: self
            integer(kind = int32), intent(in) :: value
        end subroutine



        module subroutine insert_back (list, value)
            ! Synopsis:
            ! Inserts value at the back of the list.
            type(linkedlist), intent(inout):: list
            integer(kind = int32), intent(in) :: value
        end subroutine


        module subroutine insert_front (list, value)
            type(linkedlist), intent(inout):: list
            type(node_t), pointer :: node => null()
            integer(kind = int32), intent(in) :: value
        end subroutine


        module subroutine insert_sort (self, value)
            ! Synopsis:
            ! Inserts values so that these are in non-decreasing order.
            class(linkedlist), intent(inout):: self
            integer(kind = int32), intent(in) :: value
        end subroutine


        module function size_method (self) result(n)
            ! Synopsis:
            ! Returns the size of the list.
            class(linkedlist), intent(in) :: self
            integer(kind=int64) :: n
        end function


        module subroutine clear_method (self)
            class(linkedlist), intent(inout) :: self
        end subroutine


        module function numel (list) result(n)
            ! Synopsis:
            ! Returns the number of elements in the list.
            type(linkedlist), intent(in) :: list
            integer(kind=int64) :: n
        end function


        module subroutine allocate_node (node)
            type(node_t), intent(inout), pointer :: node
        end subroutine


        module subroutine deallocate_node (node)
            type(node_t), intent(inout), pointer :: node
        end subroutine


        module function unassociated (node) result(stat)
            type(node_t), intent(inout), pointer :: node
            logical(kind = int32):: stat
        end function


        module subroutine finalizer (list)
            type(linkedlist), intent(inout) :: list
        end subroutine


        module subroutine destructor (list)
            ! Synopsis:
            ! Destroys the linked-list from tail to head.
            type(linkedlist), intent(inout) :: list
        end subroutine


    end interface


end module

! References:
! SJ Chapman, FORTRAN for Scientists and Engineers, fourth edition
! github.com/tomedunn/fortran-linked-list
!
! Wrapping pointers in derived-types:
! Wrapping the pointers to nodes in a derived-type has the advantage of
! default nullifying them upon allocation of a new node.
!
