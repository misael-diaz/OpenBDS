!
!   source: test_vector.for
!   author: misael-diaz
!   date:   2021-06-10
!
!
!   Synopsis:
!   Tests the vector class.
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

program test_vector_class
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use chronos, only: chronom
    use vectors, only: vector_t
    implicit none

    type(vector_t), pointer :: vector => null()
    type(chronom) :: stopwatch
    
    integer(kind = int64) :: i = 0_int64
    integer(kind = int64), parameter :: n = 65536_int64
!   integer(kind = int64), parameter :: n = 2147483648_int64

    integer(kind = int32) :: value = 0
    integer(kind = int32) :: alloc_stat = 0


    allocate(vector, stat = alloc_stat)
    if (alloc_stat /= 0) error stop "allocation failure"

    ! instantiations
    vector = vector_t()
    stopwatch = chronom()


    print *, ""
    print *, ""
    write (*, '(1X,A)', advance='no') & 
        & "pushing values unto back of vector ... "


    value = 0
    i = 0_int64
    call stopwatch % tic()
    do while (i /= n)
        if (value == huge(0)) value = 0
        call vector % push_back(value)         ! pushes unto back of vector
        value = value + 1
        i = i + 1_int64
    end do
    call stopwatch % toc()
    
    
    print *, "done"
    print *, ""
    print *, ""


    print *, ""
    print *, ""
    print *, "size: ", n, vector % size()
    print *, "push_back :: elapsed-time (millis): ", stopwatch % etime()
    print *, ""
    print *, ""



    write (*, '(1X,A)', advance='no') "clearing ... "
    call vector % clear()
    print *, "done"


    print *, ""
    print *, ""
    write (*, '(1X,A)', advance='no') &
        & "pushing values unto back of vector ... "


    value = 0
    i = 0_int64
    call stopwatch % tic()
    do while (i /= n)
        if (value == huge(0)) value = 0
        call vector % push_back(value)
        value = value + 1
        i = i + 1_int64
    end do
    call stopwatch % toc()


    print *, "done"
    print *, ""
    print *, ""



    print *, ""
    print *, ""
    print *, "size: ", n, vector % size()
    print *, "push_back :: elapsed-time (millis): ", stopwatch % etime()
    print *, ""
    print *, ""


    i = 0_int64
    ! tests element addressing
    do while ( i /= vector % size() )
        print *, vector < i!>    ! returns the value at the ith element
        i = i + 1_int64
    end do


    print *, ""
    print *, ""
    print *, ""
    print *, ""


    write (*, '(1X,A)', advance='no') 'freeing memory buffers ... '

    deallocate(vector)
    
    print *, "done"


    print *, ""
    print *, ""
    print *, ""
    print *, ""


end program

! Comments:
! The GNU Fortran Compiler issues a *warning* about reallocating the vector
! upon instantiation when it is declared with the allocatable attribute.
! To avoid this (possible) reallocation it's declared with the pointer 
! attribute.
