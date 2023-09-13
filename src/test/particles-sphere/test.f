#include "fconfig.h"

#define LIMIT FCONF_LIMIT
#define NUMEL FCONF_NUM_PARTICLES

module api
  use, intrinsic :: iso_c_binding, only: c_ptr
  use, intrinsic :: iso_c_binding, only: c_funptr
  implicit none
  private

  public :: OBDS_Sphere_t
  public :: sphere_t
  public :: limiter
  public :: c_malloc
  public :: c_free
  public :: c_init

  ! sphere types defined in the header particles/sphere/type.h

  type, bind(c) :: OBDS_Sphere_t
    type(c_ptr) :: x
    type(c_ptr) :: y
    type(c_ptr) :: z
    type(c_ptr) :: r_x
    type(c_ptr) :: r_y
    type(c_ptr) :: r_z
    type(c_ptr) :: a_x
    type(c_ptr) :: a_y
    type(c_ptr) :: a_z
    type(c_ptr) :: f_x
    type(c_ptr) :: f_y
    type(c_ptr) :: f_z
    type(c_ptr) :: t_x
    type(c_ptr) :: t_y
    type(c_ptr) :: t_z
    type(c_ptr) :: id
  end type

  type, bind(c) :: sphere_t
    type(c_ptr) :: props
    type(c_funptr) :: update
    type(c_funptr) :: limit
    type(c_funptr) :: log
  end type

  interface
    function c_malloc (sz) bind(c, name = 'malloc') result(p)
      use, intrinsic :: iso_c_binding, only: c_ptr
      use, intrinsic :: iso_c_binding, only: c_size_t
      implicit none
      integer(kind = c_size_t), value :: sz
      type(c_ptr) :: p
    end function
  end interface

  interface
    subroutine c_free (p) bind(c, name = 'free')
      use, intrinsic :: iso_c_binding, only: c_ptr
      implicit none
      type(c_ptr), value :: p
    end subroutine
  end interface

  interface
    function c_init (ptr, lvl) bind(c, name = 'particles_sphere_initializer') result(sph)
      use, intrinsic :: iso_c_binding, only: c_ptr
      use, intrinsic :: iso_c_binding, only: c_int
      implicit none
      type(c_ptr), value :: ptr                 ! pointer to workspace
      integer(kind = c_int), value :: lvl       ! log level (NOTE: enum is of type int)
      type(c_ptr) :: sph
    end function
  end interface

  interface
    subroutine limiter (spheres)
      import sphere_t
      implicit none
      type(sphere_t), intent(inout) :: spheres
    end subroutine
  end interface

end module api

program main
  use, intrinsic :: iso_c_binding, only: c_ptr
  use, intrinsic :: iso_c_binding, only: c_bool
  use, intrinsic :: iso_c_binding, only: c_size_t
  use, intrinsic :: iso_c_binding, only: c_double
  use, intrinsic :: iso_c_binding, only: c_sizeof
  use, intrinsic :: iso_c_binding, only: c_associated
  use, intrinsic :: iso_c_binding, only: c_f_procpointer
  use, intrinsic :: iso_c_binding, only: c_f_pointer
  use, intrinsic :: iso_c_binding, only: c_null_ptr
  use api, only: OBDS_Sphere_t
  use api, only: sphere_t
  use api, only: limiter
  use api, only: c_malloc
  use api, only: c_free
  use api, only: c_init
  implicit none

  type(sphere_t) :: c_sphere_t
  type(OBDS_Sphere_t) :: c_OBDS_Sphere_t
  type(c_ptr) :: workspace = c_null_ptr
  type(c_ptr) :: c_spheres = c_null_ptr
  type(sphere_t), pointer :: spheres => null()
  type(OBDS_Sphere_t), pointer :: props => null()
  procedure(limiter), pointer :: limit => null()
  real(kind = c_double), pointer, contiguous :: x(:) => null()
  real(kind = c_double), pointer, contiguous :: y(:) => null()
  real(kind = c_double), pointer, contiguous :: z(:) => null()
  real(kind = c_double), parameter :: syslim = real(LIMIT, kind = c_double)
  integer(kind = c_size_t), parameter :: numel = int(NUMEL, kind = c_size_t)
  integer(kind = c_size_t), parameter :: size_sphere_t = 32_c_size_t
  integer(kind = c_size_t), parameter :: size_OBDS_Sphere_t = 128_c_size_t
  integer(kind = c_size_t), parameter :: size_prop_t = 8_c_size_t
  integer(kind = c_size_t), parameter :: sz = size_sphere_t + &
                                            & size_OBDS_Sphere_t + &
                                            & 16_c_size_t * numel * size_prop_t
  logical(kind = c_bool) :: failed = .false.
  integer(kind = c_size_t) :: i

  ! performs runtime checks (that are more like reminders that these are C types)

  if (c_sizeof(c_sphere_t) /= 32) then
    error stop "test(): expects size of sphere_t to be 32 bytes"
  end if

  if (c_sizeof(c_OBDS_Sphere_t) /= 128) then
    error stop "test(): expects size of OBDS_Sphere_t to be 128 bytes"
  end if

  workspace = c_malloc(sz)                      ! allocates workspace on the heap
  c_spheres = c_init(workspace, 0)              ! initializes the sphere properties
  call c_f_pointer(c_spheres, spheres)          ! binds to the C spheres container
  call c_f_pointer(spheres % props, props)      ! binds to the sphere properties
  call c_f_pointer(props % x, x, [numel])       ! binds to the x positions of the spheres
  call c_f_pointer(props % y, y, [numel])       ! binds to the y positions of the spheres
  call c_f_pointer(props % z, z, [numel])       ! binds to the z positions of the spheres
  call c_f_procpointer(spheres % limit, limit)  ! binds to the type-bound limit() method

  ! puts the particles outside the box (we can afford particle overlapping in this test)

  x = 1.0009765625_c_double * syslim
  y = 1.0009765625_c_double * syslim
  z = 1.0009765625_c_double * syslim

  call limit(spheres)                           ! applies periodic boundary conditions

  ! we can afford not to break the loops right away since the computing cost is just O(N)
  do i = 1, numel
    if (x(i) < -syslim .or. x(i) > +syslim) then
      failed = .true.
    end if
  end do

  do i = 1, numel
    if (y(i) < -syslim .or. y(i) > +syslim) then
      failed = .true.
    end if
  end do

  do i = 1, numel
    if (z(i) < -syslim .or. z(i) > +syslim) then
      failed = .true.
    end if
  end do

  ! checks if the limit() method failed to limit the particles to the system box

  write (*, '(A)', advance = 'no') 'test-sphere[0]: '
  if (failed) then
    print '(A)', 'FAIL'
  else
    print '(A)', 'PASS'
  end if

  call c_free(workspace)                        ! frees workspace from memory
  workspace = c_null_ptr                        ! nullifies pointer to workspace

end program main

!   OpenBDS                                                     September 07, 2023
!
!   source: test.f
!   author: @misael-diaz
!
!   Synopsis:
!   Tests the limiter method which applies the periodic boundary conditions.
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
