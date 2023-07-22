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

module param
  use, intrinsic :: iso_fortran_env, only: real64
  implicit none
  public
  save

  real(kind = real64), parameter :: param_dt = real(TIME_STEP, kind = real64)

end module param

module random
  use :: ieee_arithmetic, only: ieee_value, ieee_positive_inf
  use, intrinsic :: iso_fortran_env, only: real64
  implicit none
  private

  public :: random_prng

  interface random_prng
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

module dynamic
  use, intrinsic :: iso_fortran_env, only: real64
  use, intrinsic :: iso_fortran_env, only: int64
  use :: param, only: dt => param_dt
  use :: random, only: random_prng
  implicit none
  private
  save

  public :: dynamic_stochastic_update

  interface dynamic_stochastic_update
    module procedure stochastic_update
  end interface

  real(kind = real64), parameter :: mobility_sphere = dsqrt(2.0_real64 * dt)

  contains

    subroutine stochastic_force (f_x)
      ! gets the stochastic forces
      real(kind = real64), dimension(NUM_SPHERES), intent(out) :: f_x
      real(kind = real64) :: force
      integer(kind = int64) :: i

      do i = 1, NUM_SPHERES
        call random_prng(force)
        f_x(i) = force
      end do

      return
    end subroutine stochastic_force

    subroutine stochastic_displ (x, f_x)
      ! displaces the spheres (along some axis) due to the (respective) stochastic forces
      real(kind = real64), dimension(NUM_SPHERES), intent(inout) :: x
      real(kind = real64), dimension(NUM_SPHERES), intent(in) :: f_x

      x = x + mobility_sphere * f_x

      return
    end subroutine stochastic_displ

    subroutine stochastic_update (x, y, z, f_x, f_y, f_z)
      ! updates the positions of the spheres by the action of to the stochastic forces
      real(kind = real64), dimension(NUM_SPHERES), intent(inout) :: x
      real(kind = real64), dimension(NUM_SPHERES), intent(inout) :: y
      real(kind = real64), dimension(NUM_SPHERES), intent(inout) :: z
      real(kind = real64), dimension(NUM_SPHERES), intent(out) :: f_x
      real(kind = real64), dimension(NUM_SPHERES), intent(out) :: f_y
      real(kind = real64), dimension(NUM_SPHERES), intent(out) :: f_z

      call stochastic_force(f_x)
      call stochastic_force(f_y)
      call stochastic_force(f_z)

      call stochastic_displ(x, f_x)
      call stochastic_displ(y, f_y)
      call stochastic_displ(z, f_z)

      return
    end subroutine stochastic_update

end module dynamic

module bds
  use, intrinsic :: iso_c_binding, only: c_ptr
  use, intrinsic :: iso_fortran_env, only: real64
  use, intrinsic :: iso_fortran_env, only: int64
  use :: param, only: dt => param_dt
  use :: dynamic, only: dynamic_stochastic_update
  implicit none
  private
  save

  ! C and FORTRAN sphere types:

  public :: c_sphere_t
  public :: f_sphere_t

  ! memory handling functions:

  public :: c_create
  public :: c_destroy

  ! methods:

  public :: bds_integrator

  ! type definitions:

  type :: c_sphere_t    ! clang sphere type
    type(c_ptr) :: x
    type(c_ptr) :: y
    type(c_ptr) :: z
    type(c_ptr) :: r_x
    type(c_ptr) :: r_y
    type(c_ptr) :: r_z
    type(c_ptr) :: f_x
    type(c_ptr) :: f_y
    type(c_ptr) :: f_z
    type(c_ptr) :: t_x
    type(c_ptr) :: t_y
    type(c_ptr) :: t_z
    type(c_ptr) :: list
    type(c_ptr) :: id
    type(c_ptr) :: data
  end type

  type :: f_sphere_t    ! flang sphere type
    real(kind = real64), pointer, contiguous :: x(:) => null()
    real(kind = real64), pointer, contiguous :: y(:) => null()
    real(kind = real64), pointer, contiguous :: z(:) => null()
    real(kind = real64), pointer, contiguous :: r_x(:) => null()
    real(kind = real64), pointer, contiguous :: r_y(:) => null()
    real(kind = real64), pointer, contiguous :: r_z(:) => null()
    real(kind = real64), pointer, contiguous :: f_x(:) => null()
    real(kind = real64), pointer, contiguous :: f_y(:) => null()
    real(kind = real64), pointer, contiguous :: f_z(:) => null()
    real(kind = real64), pointer, contiguous :: t_x(:) => null()
    real(kind = real64), pointer, contiguous :: t_y(:) => null()
    real(kind = real64), pointer, contiguous :: t_z(:) => null()
    real(kind = real64), pointer, contiguous :: list(:) => null()
    real(kind = real64), pointer, contiguous :: id(:) => null()
  end type

  ! defines interfaces to memory handling functions:

  interface
    function c_create () bind(c, name = 'create') result(sph)
      use, intrinsic :: iso_c_binding, only: c_ptr
      type(c_ptr) :: sph
    end function
  end interface

  interface
    function c_destroy (sph) bind(c, name = 'destroy') result(res)
      use, intrinsic :: iso_c_binding, only: c_ptr
      type(c_ptr), value :: sph
      type(c_ptr) :: res
    end function
  end interface

  interface bds_integrator
    module procedure integrator
  end interface

  contains

    subroutine integrator (x, y, z, f_x, f_y, f_z, t_x, t_y, t_z, list)
      ! implements Euler's forward integration method, updates the position vectors
      real(kind = real64), dimension(NUM_SPHERES), intent(inout) :: x
      real(kind = real64), dimension(NUM_SPHERES), intent(inout) :: y
      real(kind = real64), dimension(NUM_SPHERES), intent(inout) :: z
      real(kind = real64), dimension(NUM_SPHERES), intent(out) :: f_x
      real(kind = real64), dimension(NUM_SPHERES), intent(out) :: f_y
      real(kind = real64), dimension(NUM_SPHERES), intent(out) :: f_z
      real(kind = real64), dimension(NUM_SPHERES), intent(out) :: t_x
      real(kind = real64), dimension(NUM_SPHERES), intent(out) :: t_y
      real(kind = real64), dimension(NUM_SPHERES), intent(out) :: t_z
      real(kind = real64), dimension(NUM_SPHERES), intent(out) :: list
      real(kind = real64) :: msd
      real(kind = real64) :: time
      integer(kind = int64) :: funit
      integer(kind = int64) :: stat
      integer(kind = int64) :: step
      character(*), parameter :: fname = 'msd.txt'
      character(*), parameter :: fmt = '(SP,2E32.15)'

      open(newunit = funit, file = fname, action = 'write', iostat = stat)
      if (stat /= 0_int64) then
        return
      end if

      step = 0_int64
      msd = 0.0_real64
      do while (step /= NUM_STEPS)

        t_x = x
        t_y = y
        t_z = z

        call dynamic_stochastic_update(x, y, z, f_x, f_y, f_z)

        f_x = x
        f_y = y
        f_z = z

        list = (f_x - t_x)**2 + (f_y - t_y)**2 + (f_z - t_z)**2

        ! on-the-fly computation of the MSD
        msd = msd + ( sum(list) / real(3 * NUM_SPHERES, kind = real64) )

        if (mod(step + 1_int64, 16_int64) == 0_int64) then
          time = real(step + 1_int64, kind = real64) * dt
          write (funit, fmt) time, msd
        end if

        step = step + 1_int64
      end do

      close(funit)

      return
    end subroutine integrator

end module bds

module test
  use, intrinsic :: iso_c_binding, only: c_ptr
  use, intrinsic :: iso_c_binding, only: c_f_pointer
  use, intrinsic :: iso_fortran_env, only: real64
  use, intrinsic :: iso_fortran_env, only: int64
  use :: ieee_arithmetic, only: ieee_value
  use :: ieee_arithmetic, only: ieee_positive_inf
  use :: ieee_arithmetic, only: ieee_negative_inf
  use :: random, only: random_prng
  use :: bds, only: c_sphere_t
  use :: bds, only: f_sphere_t
  use :: bds, only: c_destroy
  use :: bds, only: c_create
  use :: bds, only: integrator => bds_integrator
  implicit none
  private

  public :: test_init
  public :: test_rand
  public :: test_msd
  public :: test_flooring
  public :: test_signbit

  interface
    function c_floor (x) result(res) bind(c, name = 'floor')
      use, intrinsic :: iso_c_binding, only: c_double
      implicit none
      real(kind = c_double), value :: x
      real(kind = c_double) :: res
    end function
  end interface

  interface
    function c_sign (x) result(res) bind(c, name = 'sign')
      use, intrinsic :: iso_c_binding, only: c_double
      use, intrinsic :: iso_c_binding, only: c_bool
      implicit none
      real(kind = c_double), value :: x
      logical(kind = c_bool) :: res
    end function
  end interface

  interface test_init
    module procedure initialization
  end interface

  interface test_rand
    module procedure prng
  end interface

  interface test_msd
    module procedure msd
  end interface

  interface test_flooring
    module procedure flooring
  end interface

  interface test_signbit
    module procedure signbit
  end interface

  contains

    subroutine initialization ()
      ! tests the initialization (done by the c_create() method implemented in C)
      integer(kind = int64), parameter :: numel = NUM_SPHERES

      ! C pointer to the data of the spheres
      type(c_ptr) :: c_spheres
      ! FORTRAN pointer for binding to the C pointer
      type(c_sphere_t), pointer :: ptr_c_spheres
      ! with this we get access to the data from FORTRAN
      type(f_sphere_t) :: spheres

      real(kind = real64) :: f              ! accumulator for floating-point numbers
      integer(kind = int64) :: num          ! accumulator for integral numbers
      integer(kind = int64) :: i            ! counter (or index)

      c_spheres = c_create()

      call c_f_pointer(c_spheres, ptr_c_spheres)
      call c_f_pointer(ptr_c_spheres % x, spheres % x, [NUM_SPHERES])
      call c_f_pointer(ptr_c_spheres % y, spheres % y, [NUM_SPHERES])
      call c_f_pointer(ptr_c_spheres % z, spheres % z, [NUM_SPHERES])
      call c_f_pointer(ptr_c_spheres % r_x, spheres % r_x, [NUM_SPHERES])
      call c_f_pointer(ptr_c_spheres % r_y, spheres % r_y, [NUM_SPHERES])
      call c_f_pointer(ptr_c_spheres % r_z, spheres % r_z, [NUM_SPHERES])
      call c_f_pointer(ptr_c_spheres % f_x, spheres % f_x, [NUM_SPHERES])
      call c_f_pointer(ptr_c_spheres % f_y, spheres % f_y, [NUM_SPHERES])
      call c_f_pointer(ptr_c_spheres % f_z, spheres % f_z, [NUM_SPHERES])
      call c_f_pointer(ptr_c_spheres % t_x, spheres % t_x, [NUM_SPHERES])
      call c_f_pointer(ptr_c_spheres % t_y, spheres % t_y, [NUM_SPHERES])
      call c_f_pointer(ptr_c_spheres % t_z, spheres % t_z, [NUM_SPHERES])
      call c_f_pointer(ptr_c_spheres % list, spheres % list, [NUM_SPHERES])
      call c_f_pointer(ptr_c_spheres % id, spheres % id, [NUM_SPHERES])

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
        f = f + spheres % r_x(i)**2 + spheres % r_y(i)**2 + spheres % r_z(i)**2
      end do

      write (*, '(A)', advance='no') 'test[1]: '
      if (f /= 0.0_real64) then
        print '(A)', 'FAIL'
      else
        print '(A)', 'PASS'
      end if

      f = 0.0_real64
      do i = 1, numel
        f = f + spheres % f_x(i)**2 + spheres % f_y(i)**2 + spheres % f_z(i)**2
      end do

      write (*, '(A)', advance='no') 'test[2]: '
      if (f /= 0.0_real64) then
        print '(A)', 'FAIL'
      else
        print '(A)', 'PASS'
      end if

      f = 0.0_real64
      do i = 1, numel
        f = f + spheres % t_x(i)**2 + spheres % t_y(i)**2 + spheres % t_z(i)**2
      end do

      write (*, '(A)', advance='no') 'test[3]: '
      if (f /= 0.0_real64) then
        print '(A)', 'FAIL'
      else
        print '(A)', 'PASS'
      end if

      do i = 1, numel
        f = f + spheres % x(i)**2 + spheres % y(i)**2 + spheres % z(i)**2
      end do

      write (*, '(A)', advance='no') 'test[4]: '
      if (f /= 0.0_real64) then
        print '(A)', 'FAIL'
      else
        print '(A)', 'PASS'
      end if

      do i = 1, numel
        f = f + spheres % list(i)
      end do

      write (*, '(A)', advance='no') 'test[5]: '
      if (f /= 0.0_real64) then
        print '(A)', 'FAIL'
      else
        print '(A)', 'PASS'
      end if

      c_spheres = c_destroy(c_spheres)

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
        call random_prng(x)
        avg = avg + x
        std = std + x**2
      end do

      avg = avg / real(NUM_SPHERES, kind = real64)
      std = sqrt(std / real(NUM_SPHERES - 1, kind = real64) )

      print *, 'avg (should be close to zero): ', avg
      print *, 'std (should be close to one):  ', std

      return
    end subroutine prng

    subroutine msd ()
      ! exports the Mean Squared Displacement MSD as a function of time

      type(c_ptr) :: c_spheres
      type(c_sphere_t), pointer :: ptr_c_spheres
      type(f_sphere_t), target :: spheres

      real(kind = real64), pointer, contiguous :: x(:) => null()
      real(kind = real64), pointer, contiguous :: y(:) => null()
      real(kind = real64), pointer, contiguous :: z(:) => null()
      real(kind = real64), pointer, contiguous :: r_x(:) => null()
      real(kind = real64), pointer, contiguous :: r_y(:) => null()
      real(kind = real64), pointer, contiguous :: r_z(:) => null()
      real(kind = real64), pointer, contiguous :: f_x(:) => null()
      real(kind = real64), pointer, contiguous :: f_y(:) => null()
      real(kind = real64), pointer, contiguous :: f_z(:) => null()
      real(kind = real64), pointer, contiguous :: t_x(:) => null()
      real(kind = real64), pointer, contiguous :: t_y(:) => null()
      real(kind = real64), pointer, contiguous :: t_z(:) => null()
      real(kind = real64), pointer, contiguous :: list(:) => null()

      c_spheres = c_create()

      call c_f_pointer(c_spheres, ptr_c_spheres)
      call c_f_pointer(ptr_c_spheres % x, spheres % x, [NUM_SPHERES])
      call c_f_pointer(ptr_c_spheres % y, spheres % y, [NUM_SPHERES])
      call c_f_pointer(ptr_c_spheres % z, spheres % z, [NUM_SPHERES])
      call c_f_pointer(ptr_c_spheres % r_x, spheres % r_x, [NUM_SPHERES])
      call c_f_pointer(ptr_c_spheres % r_y, spheres % r_y, [NUM_SPHERES])
      call c_f_pointer(ptr_c_spheres % r_z, spheres % r_z, [NUM_SPHERES])
      call c_f_pointer(ptr_c_spheres % f_x, spheres % f_x, [NUM_SPHERES])
      call c_f_pointer(ptr_c_spheres % f_y, spheres % f_y, [NUM_SPHERES])
      call c_f_pointer(ptr_c_spheres % f_z, spheres % f_z, [NUM_SPHERES])
      call c_f_pointer(ptr_c_spheres % t_x, spheres % t_x, [NUM_SPHERES])
      call c_f_pointer(ptr_c_spheres % t_y, spheres % t_y, [NUM_SPHERES])
      call c_f_pointer(ptr_c_spheres % t_z, spheres % t_z, [NUM_SPHERES])
      call c_f_pointer(ptr_c_spheres % list, spheres % list, [NUM_SPHERES])
      call c_f_pointer(ptr_c_spheres % id, spheres % id, [NUM_SPHERES])

      x => spheres % x
      y => spheres % y
      z => spheres % z

      r_x => spheres % r_x
      r_y => spheres % r_y
      r_z => spheres % r_z

      f_x => spheres % f_x
      f_y => spheres % f_y
      f_z => spheres % f_z

      t_x => spheres % t_x
      t_y => spheres % t_y
      t_z => spheres % t_z

      list => spheres % list

      call integrator(x, y, z, f_x, f_y, f_z, t_x, t_y, t_z, list)

      c_spheres = c_destroy(c_spheres)

      return
    end subroutine msd

    subroutine flooring ()
      ! prints 3.0 as expected
      real(kind = real64) :: x
      x = c_floor(3.5_real64)

      write (*, '(A)', advance='no') 'floor-test[0]: '
      if (x /= 3.0_real64) then
        print '(A)', 'FAIL'
      else
        print '(A)', 'PASS'
      end if

      return
    end subroutine flooring

    subroutine signbit ()

      write (*, '(A)', advance='no') 'signbit-test[0]: '
      if ( c_sign(+0.0_real64) ) then
        print '(A)', 'FAIL'
      else
        print '(A)', 'PASS'
      end if

      write (*, '(A)', advance='no') 'signbit-test[1]: '
      if ( .not. c_sign(-0.0_real64) ) then
        print '(A)', 'FAIL'
      else
        print '(A)', 'PASS'
      end if

      write (*, '(A)', advance='no') 'signbit-test[2]: '
      if ( c_sign(+1.0_real64) ) then
        print '(A)', 'FAIL'
      else
        print '(A)', 'PASS'
      end if

      write (*, '(A)', advance='no') 'signbit-test[3]: '
      if ( .not. c_sign(-1.0_real64) ) then
        print '(A)', 'FAIL'
      else
        print '(A)', 'PASS'
      end if

      write (*, '(A)', advance='no') 'signbit-test[4]: '
      if ( c_sign(ieee_value(0.0_real64, ieee_positive_inf)) ) then
        print '(A)', 'FAIL'
      else
        print '(A)', 'PASS'
      end if

      write (*, '(A)', advance='no') 'signbit-test[5]: '
      if ( .not. c_sign(ieee_value(0.0_real64, ieee_negative_inf)) ) then
        print '(A)', 'FAIL'
      else
        print '(A)', 'PASS'
      end if

      return
    end subroutine signbit

end module test

program main
  use, intrinsic :: iso_fortran_env, only: real64
  use :: test, only: test_init
  use :: test, only: test_flooring
  use :: test, only: test_signbit
  use :: test, only: test_rand
  use :: test, only: test_msd
  implicit none

  call test_init()
  call test_flooring()
  call test_signbit()
  call test_rand()
  call test_msd()

end program main
