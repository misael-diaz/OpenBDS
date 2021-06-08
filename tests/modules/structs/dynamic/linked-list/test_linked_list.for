!
!   source: test_linked_list.for
!   author: misael-diaz
!   date:   2021-06-06
!
!
!   Synopsis:
!   Tests the linked-list class.
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

program test_linked_list
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use linkedlists, only: linkedlist, linkedlist_initializer
    implicit none
    type(linkedlist):: list
!   type(linkedlist):: empty_list
!   type(linkedlist):: single_valued_list
    integer(kind = int32):: i = 0
    integer(kind = int32):: value = 0
    integer(kind = int32), parameter :: n = 256
    integer(kind = int32), parameter :: lb = 0
    integer(kind = int32), parameter :: ub = n - 1
    integer(kind = int32), dimension(lb:ub) :: values


    call linkedlist_initializer(list, value) ! ifort-friendly constructor
    do while (i /= n - 1)
        value = i + 1
        call list % push_back(value)    ! pushes values into linked-list
        i = i + 1
    end do


    print *, 'size: ', n, list % size()


    i = 0
    call list % begin()
    do while ( i /= list % size() )
        call list % print()             ! displays values
        call list % next()
        i = i + 1
    end do


    i = 0
    call list % begin()
    do while (i /= n)
        values(i) = list % copy()       ! copies values into array elements
        call list % next()
        i = i + 1
    end do

    print *, sum(values), real(n * (n - 1), kind = real64) / 2.0_real64


!   single_valued_list = linkedlist(value)

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
!
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




! Diagnostic Messages:
! The code will display a warning about having no more elements but
! it's safe to ignore. In the near future I shall write code to
! suppress warning messages for production settings.




! Known Issues:
!
!
! Summary:
! Code segfaults when invoking the python-like constructor in a program
! compiled with the Intel FORTRAN Compiler ifort.
!
!
! Long Description:
! I had a hard time debugging the code when compiling with the Intel
! FORTRAN Compiler. The source of the problem was using the python-like
! constructor, which is implemented as a function that returns a
! linked-list object. The GNU FORTRAN Compiler and Flang-7 were able
! to intepret my intention and produced a code that met my expectations.
! I have researched for the right way to return a derived-type object
! but really there's nothing special about that. Perhaps is a compiler
! bug, which could be the case since it's a beta:
!
! ifort (IFORT) 2021.1 Beta 20201112
!
!
! Closure:
! I do not have the insight to tell what was ifort doing under the
! covers. All that I know is that if I want to compile the code with
! ifort I must use the linkedlist_initialzer() procedure as in this
! testing program.
