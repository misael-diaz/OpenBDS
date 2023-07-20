!   OpenBDS                                                     July 19, 2023
!
!   source: main.for
!   author: @misael-diaz
!
!   Synopsis:
!   Creates a system of non-interacting Brownian spheres, checks their
!   initial values, and destroys them.
!
!   Copyright (C) 2023 Misael Diaz-Maldonado
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
!
!   References:
!   [0] SJ Chapman, FORTRAN for Scientists and Engineers, 4th edition.
!   [1] MP Allen and DJ Tildesley, Computer Simulation of Liquids.
!   [2] S Kim and S Karrila, Microhydrodynamics.
!

#include "system.h"

module random
  use :: ieee_arithmetic, only: ieee_value, ieee_positive_inf
  use, intrinsic :: iso_fortran_env, only: real64
  implicit none
  private

  public :: rand

  interface rand
    module procedure prng
  end interface

  contains

    subroutine prng (x)
      ! Implements Box-Muller's method, yields a Gaussian pseudo-random number.
      real(kind = real64), intent(out) :: x
      real(kind = real64) :: x1
      real(kind = real64) :: x2
      real(kind = real64) :: dist
      real(kind = real64) :: positive_infinity

      positive_infinity = ieee_value(0.0_real64, ieee_positive_inf)
      dist = positive_infinity
      do while (dist > 1.0_real64)

        call random_number(x1)
        call random_number(x2)

        x1 = 2.0_real64 * x1 - 1.0_real64
        x2 = 2.0_real64 * x2 - 1.0_real64

        dist = x1**2 + x2**2

      end do

      dist = dsqrt( ( -2.0_real64 * dlog(dist) ) / dist )

      x1 = dist * x1
      x2 = dist * x2

      x = x1

      return
    end subroutine prng

end module random

module bds
  use, intrinsic :: iso_c_binding, only: c_ptr
  use, intrinsic :: iso_fortran_env, only: real64
  implicit none
  private
  save

  ! C and FORTRAN sphere types:

  public :: csphere_t
  public :: fsphere_t

  ! memory handling functions:

  public :: create
  public :: destroy

  ! type definitions:

  type :: csphere_t     ! clang sphere type
    type(c_ptr) :: x
    type(c_ptr) :: y
    type(c_ptr) :: z
    type(c_ptr) :: f_x
    type(c_ptr) :: f_y
    type(c_ptr) :: f_z
    type(c_ptr) :: id
    type(c_ptr) :: data
  end type

  type :: fsphere_t     ! flang sphere type
    real(kind = real64), pointer, contiguous :: x(:) => null()
    real(kind = real64), pointer, contiguous :: y(:) => null()
    real(kind = real64), pointer, contiguous :: z(:) => null()
    real(kind = real64), pointer, contiguous :: f_x(:) => null()
    real(kind = real64), pointer, contiguous :: f_y(:) => null()
    real(kind = real64), pointer, contiguous :: f_z(:) => null()
    real(kind = real64), pointer, contiguous :: id(:) => null()
  end type

  ! defines interfaces to memory handling functions:

  interface
    function create () bind(c) result(sph)
      use, intrinsic :: iso_c_binding, only: c_ptr
      type(c_ptr) :: sph
    end function
  end interface

  interface
    function destroy (sph) bind(c) result(res)
      use, intrinsic :: iso_c_binding, only: c_ptr
      type(c_ptr), value :: sph
      type(c_ptr) :: res
    end function
  end interface

end module bds

module tests
  use, intrinsic :: iso_c_binding, only: c_ptr
  use, intrinsic :: iso_c_binding, only: c_f_pointer
  use, intrinsic :: iso_fortran_env, only: real64
  use, intrinsic :: iso_fortran_env, only: int64
  use :: random, only: rand
  use :: bds, only: csphere_t
  use :: bds, only: fsphere_t
  use :: bds, only: destroy
  use :: bds, only: create
  implicit none
  private

  public :: test_init
  public :: test_rand

  interface test_init
    module procedure initialization
  end interface

  interface test_rand
    module procedure prng
  end interface

  contains

    subroutine initialization ()
      implicit none

      integer(kind = int64), parameter :: numel = NUM_SPHERES

      ! C pointer to the data of the spheres
      type(c_ptr) :: sph
      ! FORTRAN pointer for binding to the C pointer
      type(csphere_t), pointer :: p_spheres
      ! with this we get access to the data from FORTRAN
      type(fsphere_t) :: spheres

      real(kind = real64) :: f              ! accumulator for floating-point numbers
      integer(kind = int64) :: num          ! accumulator for integral numbers
      integer(kind = int64) :: i            ! counter (or index)

      sph = create()

      call c_f_pointer(sph, p_spheres)
      call c_f_pointer(p_spheres % x, spheres % x, [NUM_SPHERES])
      call c_f_pointer(p_spheres % y, spheres % y, [NUM_SPHERES])
      call c_f_pointer(p_spheres % z, spheres % z, [NUM_SPHERES])
      call c_f_pointer(p_spheres % f_x, spheres % f_x, [NUM_SPHERES])
      call c_f_pointer(p_spheres % f_y, spheres % f_y, [NUM_SPHERES])
      call c_f_pointer(p_spheres % f_z, spheres % f_z, [NUM_SPHERES])
      call c_f_pointer(p_spheres % id, spheres % id, [NUM_SPHERES])

      ! checks the data (in an aggregate sense) against the expected values:

      num = 0_int64
      do i = 1, numel
        num = num + int(spheres % id(i), kind=int64)
      end do

      write (*, '(A)', advance='no') 'test[0]: '
      if (num /= NUM_SPHERES * (NUM_SPHERES - 1) / 2) then
        print '(A)', 'FAIL'
      else
        print '(A)', 'PASS'
      end if

      f = 0.0_real64
      do i = 1, numel
        f = f + spheres % f_x(i)**2 + spheres % f_y(i)**2 + spheres % f_z(i)**2
      end do

      write (*, '(A)', advance='no') 'test[1]: '
      if (f /= 0.0_real64) then
        print '(A)', 'FAIL'
      else
        print '(A)', 'PASS'
      end if

      do i = 1, numel
        f = f + spheres % x(i)**2 + spheres % y(i)**2 + spheres % z(i)**2
      end do

      write (*, '(A)', advance='no') 'test[2]: '
      if (f /= 0.0_real64) then
        print '(A)', 'FAIL'
      else
        print '(A)', 'PASS'
      end if

      sph = destroy(sph)

      return
    end subroutine initialization

    subroutine prng ()
      ! checks the statistics of the Gaussian pseudo-random numbers

      real(kind = real64) :: x
      real(kind = real64) :: avg
      real(kind = real64) :: std
      integer(kind = int64) :: i

      avg = 0.0_real64
      std = 0.0_real64
      do i = 1, NUM_SPHERES
        call rand(x)
        avg = avg + x
        std = std + x**2
      end do

      avg = avg / real(NUM_SPHERES, kind = real64)
      std = sqrt(std / real(NUM_SPHERES - 1, kind = real64) )

      print *, 'avg (should be close to zero): ', avg
      print *, 'std (should be close to one):  ', std

      return
    end subroutine prng

end module tests

program main
  use :: tests, only: test_init
  use :: tests, only: test_rand
  implicit none

  call test_init()
  call test_rand()

end program main
