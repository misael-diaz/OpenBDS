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
    implicit none


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


    private
    contains


        function default_constructor() result(list)
            type(linkedlist):: list

            list % iter % node => null()
            list % head % node => null()
            list % tail % node => null()
            list % stat % init = .false.

            return
        end function


        subroutine initializer(list, value)
            type(linkedlist), intent(inout) :: list
            integer(kind = int32), intent(in) :: value
            call create(list, value)
            return
        end subroutine


        subroutine create(list, value)
            ! Synopsis:
            ! Creates the first link of the linked-list.
            type(linkedlist), intent(inout) :: list
            integer(kind = int32), intent(in) :: value

            call allocator(list % head % node)
            list % iter % node => list % head % node
            list % tail % node => list % head % node
            list % tail % node % value = value
            list % stat % init = .true.

            return
        end subroutine


        subroutine iterator_begin_method (self)
            class(linkedlist), intent(inout) :: self
            self % iter % node => self % head % node
            return
        end subroutine


        subroutine iterator_end_method (self)
            class(linkedlist), intent(inout) :: self
            self % iter % node => self % tail % node
            return
        end subroutine


        subroutine iterator_advance_method (self)
            class(linkedlist), intent(inout) :: self
            if ( associated(self % iter % node % next % node) ) then
                self % iter % node => self % iter % node % next % node
            else
                print *, 'linkedlist_warning(): there are no more elements'
            end if
            return
        end subroutine


        subroutine display_method (self)
            class(linkedlist), intent(in) :: self
            type(node_t), pointer :: it => null()

            it => self % head % node
            do while ( associated(it) )
                print *, it % value
                it => it % next % node
            end do

            return
        end subroutine


        subroutine to_array_method (self, values)
            ! Synopsis:
            ! Copies values into an array.
            class(linkedlist), intent(in) :: self
            type(node_t), pointer :: it => null()
            integer(kind = int32), intent(inout), allocatable :: values(:)
            integer(kind = int32):: mstat
            integer(kind = int64):: i
            integer(kind = int64):: b
            integer(kind = int64):: e


            if ( allocated(values) ) then
                deallocate(values)
            end if

            allocate (values( numel(self) ), stat=mstat)
            if (mstat /= 0) then
                error stop "copy: insufficient memory"
            end if


            it => self % head % node
            b = lbound(array = values, dim = 1, kind = int64)
            e = ubound(array = values, dim = 1, kind = int64)
            do i = b, e
                values(i) = it % value
                it => it % next % node
            end do


            return
        end subroutine to_array_method


        subroutine push_back_method (self, value)
            class(linkedlist), intent(inout):: self
            integer(kind = int32), intent(in) :: value

            if (self % stat % init) then
                call insert_back (self, value)
            else
                call initializer (self, value)
            end if

            return
        end subroutine


        subroutine push_front_method (self, value)
            class(linkedlist), intent(inout):: self
            integer(kind = int32), intent(in) :: value

            if (self % stat % init) then
                call insert_front (self, value)
            else
                call initializer (self, value)
            end if

            return
        end subroutine


        subroutine insertion_sort_method (self, value)
            class(linkedlist), intent(inout):: self
            integer(kind = int32), intent(in) :: value

            if (self % stat % init) then
                call insert_sort (self, value)
            else
                call initializer (self, value)
            end if

            return
        end subroutine



        subroutine insert_back (list, value)
            ! Synopsis:
            ! Inserts value at the back of the list.
            type(linkedlist), intent(inout):: list
            integer(kind = int32), intent(in) :: value

            call allocator(list % tail % node % next % node)
            list % tail % node => list % tail % node % next % node
            list % tail % node % value = value

            return
        end subroutine


        subroutine insert_front (list, value)
            type(linkedlist), intent(inout):: list
            type(node_t), pointer :: node => null()
            integer(kind = int32), intent(in) :: value

            node => list % head % node
            list % head % node => null()
            call allocator(list % head % node)
            list % head % node % value = value
            list % head % node % next % node => node

            return
        end subroutine


        subroutine insert_sort(self, value)
            ! Synopsis:
            ! Inserts values so that these are in non-decreasing order.
            class(linkedlist), intent(inout):: self
            integer(kind = int32), intent(in) :: value
            type(node_t), pointer :: it => null()
            type(node_t), pointer :: node => null()

            if (value <= self % head % node % value) then
                call insert_front(self, value)
            else if (value > self % tail % node % value) then
                call insert_back(self, value)
            else

                it => self % head % node
                do while (value > it % next % node % value)
                    it => it % next % node
                end do

                node => it % next % node
                it % next % node => null()
                call allocator(it % next % node)
                it % next % node % value = value
                it % next % node % next % node => node

            end if

            return
        end subroutine


        function size_method(self) result(n)
            ! Synopsis:
            ! Returns the size of the list.
            class(linkedlist), intent(in) :: self
            integer(kind=int64) :: n
            n = numel(self)
            return
        end function


        function numel(list) result(n)
            ! Synopsis:
            ! Returns the number of elements in the list.
            type(linkedlist), intent(in) :: list
            type(node_t), pointer :: it => null()
            integer(kind=int64) :: n

            n = 0_int64
            it => list % head % node
            do while( associated(it) )
                it => it % next % node
                n = n + 1_int64
            end do

            return
        end function


        subroutine allocator(node)
            type(node_t), intent(inout), pointer :: node
            integer(kind = int32):: alloc_stat = 0

            if ( unassociated(node) ) then
                allocate(node, stat = alloc_stat)
            end if

            if (alloc_stat /= 0) then
                error stop ("insufficient memory to allocate link")
            end if

            return
        end subroutine


        function unassociated(node) result(stat)
            type(node_t), intent(inout), pointer :: node
            logical(kind = int32):: stat

            if ( .not. associated(node) ) then
                stat = .true.
            else
                stat = .false.
            end if

            return
        end function


        subroutine finalizer(list)
            type(linkedlist), intent(inout) :: list

            if ( associated(list % head % node) ) then
                call destructor(list)
            end if

            return
        end subroutine


        subroutine destructor(list)
            ! Synopsis:
            ! Destroys the linked-list from tail to head.
            type(linkedlist), intent(inout) :: list
            type(node_t), pointer :: it => null()
            integer(kind = int64):: i = 0_int64
            integer(kind = int64):: n = 0_int64

            n = numel(list)
            do while (n /= 1_int64)
                i = 0_int64
                it => list % head % node
                do while (i /= n - 2_int64)
                    it => it % next % node
                    i = i + 1_int64
                end do
                deallocate(it % next % node)
                it % next % node => null()
                n = n - 1_int64
            end do

            deallocate(list % head % node)
            list % head % node => null()
            list % tail % node => null()
            list % iter % node => null()
            list % stat % init = .false.

            return
        end subroutine
end module

! References:
! SJ Chapman, FORTRAN for Scientists and Engineers, fourth edition
! github.com/tomedunn/fortran-linked-list
!
! Wrapping pointers in derived-types:
! Wrapping the pointers to nodes in a derived-type has the advantage of
! default nullifying them upon allocation of a new node.
!
