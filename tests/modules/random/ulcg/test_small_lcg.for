!
!   source: test_small_lcg.for
!   author: misael-diaz
!   date:   2021-06-05
!
!
!   Synopsis:
!   A simple runtime test of the small Linear Congruential Generator.
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

program test_small_lcg
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use ulcg, only: ulcg_gen
    implicit none
    type(ulcg_gen):: prng
    real(kind = real64):: r
    real(kind = real64):: total = 0.0_real64
    integer(kind = int32):: i = 0
    integer(kind = int32), parameter :: n = 256


    prng = ulcg_gen ()
    do while (i /= n)
        r = prng % getu01 ()
        total = total + r
        print '(1X,F8.6)', r
        i = i + 1
    end do


    print *, ""
    print *, ""
    print *, 'lcg:     ', prng % name
    print *, 'average: ', total / real(n, kind = real64)
    print *, ""
    print *, ""


end program


! Comments:
!
! I just wanted to check if the generator would yield garbage or not.
! From the runtime test it is found that the generator yields numbers
! in the expected range (0, 1).
!
! Statistic tests shall be conducted later.
