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
    use, intrinsic :: iso_fortran_env, only: int32
    implicit none


    type :: link_t
        type(node_t), pointer :: node => null()
    end type


    type :: node_t
        integer(kind = int32) :: value = 0
        type(link_t) :: next
    end type


    type, public :: linkedlist
        private
        type(link_t):: iter     ! iterator
        type(link_t):: head     ! begin
        type(link_t):: tail     ! end
        contains
            procedure, public :: begin => iterator_begin
            procedure, public :: next => iterator_advance
            procedure, public :: end => iterator_end
            procedure, public :: size => size_method
            procedure, public :: print => display
            procedure, public :: copy => to_array
            procedure, public :: push_back => insert_back
            procedure, public :: push_front => insert_front
            procedure, public :: insort => insert_sort
            final :: finalizer
    end type


    interface linkedlist
        module procedure initializer
    end interface


    private
    public :: linkedlist_initializer
    contains


        subroutine linkedlist_initializer(list, value)
            ! Synopsis: Constructor
            type(linkedlist), intent(inout) :: list
            integer(kind = int32), intent(in) :: value
            call create(list, value)
            return
        end subroutine


        function initializer(value) result(list)
            ! Synopsis: Python-like Constructor.
            type(linkedlist):: list
            integer(kind = int32), intent(in) :: value
            call create(list, value)
            return
        end function


        subroutine create(list, value)
            ! Synopsis:
            ! Creates the first link of the linked-list.
            type(linkedlist), intent(inout) :: list
            integer(kind = int32), intent(in) :: value

            call allocator(list % head % node)
            list % iter % node => list % head % node
            list % tail % node => list % head % node
            list % tail % node % value = value
            return
        end subroutine


        subroutine iterator_begin(self)
            class(linkedlist), intent(inout) :: self
            self % iter % node => self % head % node
            return
        end subroutine


        subroutine iterator_end(self)
            class(linkedlist), intent(inout) :: self
            self % iter % node => self % tail % node
            return
        end subroutine


        subroutine iterator_advance(self)
            class(linkedlist), intent(inout) :: self
            if ( associated(self % iter % node % next % node) ) then
                self % iter % node => self % iter % node % next % node
            else
                print *, 'linkedlist_warning(): there are no more elements'
            end if
            return
        end subroutine


        subroutine display(self)
            class(linkedlist), intent(in) :: self
            type(node_t), pointer :: it => null()

            it => self % head % node
            do while ( associated(it) )
                print *, it % value
                it => it % next % node
            end do

            return
        end subroutine


        subroutine to_array(self, values)
            ! Synopsis:
            ! Copies values into an array.
            class(linkedlist), intent(in) :: self
            type(node_t), pointer :: it => null()
            integer(kind = int32), intent(inout), pointer :: values(:)
            integer(kind = int32):: mstat
            integer(kind = int32):: i
            integer(kind = int32):: b
            integer(kind = int32):: e


            if ( .not. associated(values) ) then
                allocate (values( numel(self) ), stat=mstat)
                if (mstat /= 0) then
                    error stop "copy: insufficient memory"
                end if
            end if


            it => self % head % node
            b = lbound(array = values, dim = 1, kind = int32)
            e = ubound(array = values, dim = 1, kind = int32)
            do i = b, e
                values(i) = it % value
                it => it % next % node
            end do


            return
        end subroutine to_array


        subroutine insert_back(self, value)
            ! Synopsis:
            ! Inserts value at the back of the list.
            class(linkedlist), intent(inout):: self
            integer(kind = int32), intent(in) :: value

            call allocator(self % tail % node % next % node)
            self % tail % node => self % tail % node % next % node
            self % tail % node % value = value

            return
        end subroutine


        subroutine insert_front(self, value)
            class(linkedlist), intent(inout):: self
            type(node_t), pointer :: node => null()
            integer(kind = int32), intent(in) :: value

            node => self % head % node
            self % head % node => null()
            call allocator(self % head % node)
            self % head % node % value = value
            self % head % node % next % node => node

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


        function size_method(self) result(sz)
            ! Synopsis:
            ! Returns the size of the list.
            class(linkedlist), intent(in) :: self
            integer(kind=int32) :: sz
            sz = numel(self)
            return
        end function


        function numel(list) result(n)
            ! Synopsis:
            ! Returns the number of elements in the list.
            type(linkedlist), intent(in) :: list
            type(node_t), pointer :: it => null()
            integer(kind=int32) :: n

            n = 0
            it => list % head % node
            do while( associated(it) )
                it => it % next % node
                n = n + 1
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
            integer(kind = int32):: i = 0
            integer(kind = int32):: n = 0

            n = numel(list)
            do while (n /= 1)
                i = 0
                it => list % head % node
                do while (i /= n - 2)
                    it => it % next % node
                    i = i + 1
                end do
                deallocate(it % next % node)
                it % next % node => null()
                n = n - 1
            end do

            deallocate(list % head % node)
            list % head % node => null()
            list % tail % node => null()
            list % iter % node => null()

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
! Known Issues:
! Using the Python-like constructor might incurr in a segmentation fault
! when compiling the code with the Intel FORTRAN Compiler:
!
! ifort (IFORT) 2021.1 Beta 20201112
!
! Solution:
! To avoid the segfault invoke the linkedlist_initializer() procedure.
! See the linked-list test.
