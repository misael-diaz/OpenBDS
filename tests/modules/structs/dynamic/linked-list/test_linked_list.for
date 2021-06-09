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

module test_linked_list_supporting
    use, intrinsic :: iso_fortran_env, only: int32
    implicit none

    interface test_sort
        module procedure nondecreasing
    end interface

    private
    public :: test_sort
    contains


        subroutine nondecreasing(values)
            integer(kind = int32), intent(in) :: values(:)

            if ( is_sorted(values) ) then
                print *, 'sort-test: pass'
            else
                print *, 'sort-test: fail'
            end if

            return
        end subroutine


        function is_sorted(values)
            ! Synopsis:
            ! Checks if the values are organized in non-decreasing order.
            integer(kind = int32), intent(in) :: values(:)
            logical(kind = int32):: is_sorted
            integer(kind = int32):: b
            integer(kind = int32):: e
            integer(kind = int32):: i

            b = lbound(array = values, dim = 1, kind = int32)
            e = ubound(array = values, dim = 1, kind = int32)
            is_sorted = .false.
            do while (b /= e)
                i = b
                if ( values(i) > values(i + 1) ) then
                    is_sorted = .false.
                    exit
                else
                    is_sorted = .true.
                end if
                b = b + 1
            end do

            return
        end function
end module

program test_linked_list
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64
    use test_linked_list_supporting, only: test_sort
    use chronos, only: chronom
    use linkedlists, only: linkedlist, linkedlist_initializer
    implicit none
    type(linkedlist):: list
    type(linkedlist):: blist
    type(linkedlist):: flist
!   type(linkedlist):: empty_list
!   type(linkedlist):: single_valued_list
    type(chronom):: stopwatch
    real(kind = real64):: r
    real(kind = real64), parameter :: int32_max = 2147483647.0_real64
    integer(kind = int64):: i = 0_int64
    integer(kind = int64), parameter :: n = 65536_int64
    integer(kind = int32), allocatable :: values(:)
    integer(kind = int32):: value = 0
    integer(kind = int32):: alloc_stat
    integer(kind = int32):: j
    integer(kind = int32):: b
    integer(kind = int32):: e


    print *, ""
    print *, "pushing values unto back of list ... "
    print *, ""


    value = 0
    call linkedlist_initializer(blist, value) ! ifort-friendly constructor
    do while (i /= n - 1_int64)
        value = value + 1
        call blist % push_back(value)         ! pushes unto back of list
        i = i + 1_int64
    end do


    print *, ""
    print *, 'size: ', n, blist % size()
    print *, ""


    print *, ""
    print *, "pushing values unto front of list ... "
    print *, ""


    i = 0_int64
    value = 0
    call linkedlist_initializer(flist, value)
    do while (i /= n - 1_int64)
        value = value + 1
        call flist % push_front(value)        ! pushes unto front of list
        i = i + 1_int64
    end do


    print *, ""
    print *, 'size: ', n, flist % size()
    print *, ""



    print *, ""
    print *, "generating pseudo-random numbers ... "
    print *, ""


    if ( allocated(values) ) then
        deallocate(values)
    else
        allocate(values(n), stat = alloc_stat)
        if (alloc_stat /= 0) error stop ("insufficient memory for array")
    end if


    stopwatch = chronom()
    call stopwatch % tic()
    b = lbound(array = values, dim = 1, kind = int32)
    e = ubound(array = values, dim = 1, kind = int32)
    do j = b, e
        call random_number(r)
        values(j) = int(r * int32_max, kind = int32)
    end do
    call stopwatch % toc()
    print *, 'elapsed-time (millis): ', stopwatch % etime()


    print *, ""
    print *, "sorting pseudo-random numbers ... "
    print *, ""


    call stopwatch % tic()
    call linkedlist_initializer(list, value)
    do j = b, e
        call list % insort( values(j) )
    end do
    call stopwatch % toc()
    print *, 'insort :: elapsed-time (millis): ', stopwatch % etime()
    print *, 'insort :: average speed (values / millis): ', &
        & real(n, kind = real64) / stopwatch % etime()


    print *, ""
    print *, 'size: ', n, list % size()
    print *, ""


    print *, ""
    print *, "insert-back values:"
    print *, ""


    call blist % print()             ! displays values


    print *, ""
    print *, "insert-front values:"
    print *, ""


    call flist % print()


    print *, ""
    print *, "sorted pseudo-random numbers:"
    print *, ""


    call list % copy(values)    ! copies values into array
    call list % print()


    print *, ""
    print *, ""
    call test_sort(values)
    print *, ""
    print *, ""


    call blist % copy(values)


    print *, ""
    print *, sum(values), real(n * (n - 1), kind = real64) / 2.0_real64
    print *, ""


    if ( allocated(values) ) then
        deallocate(values)
    end if
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
!
!
! Sorting:
! Tests shows that the insertion-sorting implementation was successful.




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
