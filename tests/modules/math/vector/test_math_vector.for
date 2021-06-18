!
!   source: test_math_vector.for
!   author: misael-diaz
!   date:   2021-06-17
!
!
!   Synopsis:
!   Tests the math vector class.
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

program test_math_vector_class
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64
    use math_vector_class, only: math_vector_t => vector_t
    implicit none

    type(math_vector_t), pointer :: vector => null()
    
    integer(kind = int64) :: i
    integer(kind = int64), parameter :: n = 65536_int64
    integer(kind = int32) :: mstat = 0


    allocate(vector, stat = mstat)
    if (mstat /= 0) error stop "allocation failure"


    vector = math_vector_t (n)  ! instantiation
    vector % x = 1.0_real64
    vector % y = 1.0_real64
    vector % z = 1.0_real64


    call vector % normalize ()


    do i = 1, n
        print *, vector % x(i), vector % y(i), vector % z(i)
    end do


    deallocate (vector, stat = mstat)
    if (mstat /= 0) error stop "unexpected deallocation failure"


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
