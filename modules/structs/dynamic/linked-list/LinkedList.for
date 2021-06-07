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


    type :: node
        integer(kind = int32) :: value = 0
        type(node), pointer :: next => null()
    end type


    type, public :: linkedlist
        private
        type(node), pointer :: iter => null()           ! iterator
        type(node), pointer :: head => null()           ! begin
        type(node), pointer :: tail => null()           ! end
        contains
            procedure, public :: size => size_method
            procedure, public :: push_back => insert_back
            final :: finalizer
    end type


    interface linkedlist
        module procedure initializer
    end interface


    private
    contains


        function initializer(value) result(list)
            type(linkedlist):: list
            integer(kind = int32), intent(in) :: value
            call create(list, value)
            return
        end function


        subroutine create(list, value)
            ! Synopsis:
            ! Creates the first link of the linked-list.
            type(linkedlist), intent(inout) :: list
            type(node), pointer :: it => null()                 ! iterator
            integer(kind = int32), intent(in) :: value

            call allocator(list % head)
            it => list % head
            it % value = value
            it % next  => null()
            list % tail => list % head
            return
        end subroutine


        subroutine insert_back(self, value)
            ! Synopsis:
            ! Inserts value at the back of the list.
            class(linkedlist), intent(inout) :: self
            type(node), pointer :: tail => null()
            integer(kind = int32), intent(in) :: value

            call allocator(self % tail % next)
            self % tail => self % tail % next
            tail => self % tail
            tail % value = value
            tail % next  => null()
            return
        end subroutine


        function size_method(self) result(sz)
            ! Synopsis:
            ! Returns the size of the list.
            class(linkedlist), intent(in) :: self
            integer(kind=int32) :: sz
            sz = numel(self % head)
            return
        end function
        

        function numel(head) result(n)
            ! Synopsis:
            ! Returns the number of elements in the list.
            type(node), pointer, intent(in) :: head
            type(node), pointer :: it => null()
            integer(kind=int32) :: n

            n = 0
            it => head
            do while( associated(it) )
                it => it % next
                n = n + 1
            end do

            return
        end function


        subroutine allocator(link)
            type(node), intent(inout), pointer :: link
            integer(kind = int32):: alloc_stat = 0

            if ( unassociated(link) ) then
                allocate(link, stat = alloc_stat)
            end if

            if (alloc_stat /= 0) then
                error stop ("insufficient memory to allocate link")
            end if

            return
        end subroutine


        function unassociated(link) result(stat)
            type(node), intent(inout), pointer :: link
            logical(kind = int32):: stat

            if ( .not. associated(link) ) then
                stat = .true.
            else
                stat = .false.
            end if

            return
        end function


        subroutine finalizer(list)
            type(linkedlist), intent(inout) :: list

            if ( associated(list % head) ) then
                call destructor(list)
            end if

            return
        end subroutine


        subroutine destructor(list)
            ! Synopsis:
            ! Destroys the linked-list from tail to head.
            type(linkedlist), intent(inout) :: list
            type(node), pointer :: it => null()
            integer(kind = int32):: i = 0
            integer(kind = int32):: n = 0

            n = numel(list % head)
            do while (n /= 1)
                i = 0
                it => list % head
                do while (i /= n - 2)
                    it => it % next
                    i = i + 1
                end do
                deallocate(it % next)
                it % next => null()
                n = n - 1
            end do

            deallocate(list % head)
            list % head => null()
            list % tail => null()
            list % iter => null()

            return
        end subroutine
end module

! References:
! SJ Chapman, FORTRAN for Scientists and Engineers, fourth edition
! github.com/tomedunn/fortran-linked-list
