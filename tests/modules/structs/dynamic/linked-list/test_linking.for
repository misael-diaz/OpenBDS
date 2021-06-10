!
!   source: test_linking.for
!   author: misael-diaz
!   date:   2021-06-07
!
!
!   Synopsis:
!   Tests linking of nodes.
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

module testlinking
    use, intrinsic :: iso_fortran_env, only: int32
    implicit none

    type :: node
        integer(kind = int32) :: value = 0
        type(node), pointer :: next => null()
    end type


    type :: linkedlist
        type(node), pointer :: head => null()
        type(node), pointer :: tail => null()
    end type


    public
end module

program test_linking
    use, intrinsic :: iso_fortran_env, only: int32
    use testlinking, only: node, linkedlist
    implicit none
    type(linkedlist):: list

    type(node), pointer :: link => null()
    type(node), pointer :: newlink => null()

    integer(kind = int32):: mstat
    integer(kind = int32):: value = 0


    allocate(list % head, stat = mstat)
    if (mstat /= 0) error stop "memory allocation failure"
    list % tail => list % head
    list % tail % value =  value
    list % tail % next  => null()


    allocate(list % tail % next, stat = mstat)
    if (mstat /= 0) error stop "memory allocation failure"
    list % tail => list % tail % next
    print *, 'associated: ', associated(list % head % next)

    deallocate(list % tail)
    deallocate(list % head)


    allocate(link, stat = mstat)
    if (mstat /= 0) error stop "memory allocation failure"
    link % value = value


    allocate(link % next, stat = mstat)
    if (mstat /= 0) error stop "memory allocation failure"
    newlink => link % next
    newlink % value = value
    newlink % next  => null()


    deallocate(link % next)
    deallocate(link)


end program


! Runtime tests:
!
!
! We have tested for memory leaks with valgrind:
!
! $ valgrind ./tests/modules/structs/dynamic/linked-list/test-linked-list
!
! No leaks were detected.
! The output from valgrind has been logged to: leaks.out
!
!
! Empty linked-list test:
! To check if the linked-list implementation handles empty lists
! correctly (does not attempt to deallocate an unassociated pointer)
! we had to relax the compilation options so that unused variables
! are not treated as errors. In fact, the only option used to compile
! main is the free-form option (since F90) to tell the compiler that
! source layout is free (or not fixed as in F77 and former). 
! To achieve this we modified the local Makefile.
!
! We have reverted the source and the Makefile so that building the code
! with other compilers does not require tweeking Makefiles.
