#include "fconfig.h"

#define LIMIT FCONF_LIMIT
#define LOG_NUMEL FCONF_LOG_NUM_PARTICLES

module API
  use, intrinsic :: iso_c_binding, only: c_ptr
  use, intrinsic :: iso_c_binding, only: c_funptr
  use, intrinsic :: iso_c_binding, only: c_int64_t
  implicit none
  private

  public :: OBDS_Sphere_t
  public :: sphere_t
  public :: updater
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
    type(c_ptr) :: udx
    type(c_ptr) :: udy
    type(c_ptr) :: udz
    type(c_ptr) :: a_x
    type(c_ptr) :: a_y
    type(c_ptr) :: a_z
    type(c_ptr) :: d_x
    type(c_ptr) :: d_y
    type(c_ptr) :: d_z
    type(c_ptr) :: f_x
    type(c_ptr) :: f_y
    type(c_ptr) :: f_z
    type(c_ptr) :: t_x
    type(c_ptr) :: t_y
    type(c_ptr) :: t_z
    type(c_ptr) :: tmp
    type(c_ptr) :: temp
    type(c_ptr) :: bitmask
    type(c_ptr) :: list
    type(c_ptr) :: id
    integer(kind = c_int64_t) :: pad1
    integer(kind = c_int64_t) :: pad2
    integer(kind = c_int64_t) :: pad3
    integer(kind = c_int64_t) :: pad4
    integer(kind = c_int64_t) :: pad5
    integer(kind = c_int64_t) :: pad6
  end type

  type, bind(c) :: sphere_t
    type(c_ptr) :: props
    type(c_ptr) :: prng
    type(c_funptr) :: update
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
    function updater (spheres) result(stat)
      use, intrinsic :: iso_c_binding, only: c_int
      import sphere_t
      implicit none
      type(sphere_t), intent(inout) :: spheres
      integer(kind = c_int) :: stat
    end function
  end interface

end module API

module OBDS
  use, intrinsic :: iso_c_binding, only: c_ptr
  use, intrinsic :: iso_c_binding, only: c_null_ptr
  use, intrinsic :: iso_c_binding, only: c_associated
  use, intrinsic :: iso_c_binding, only: c_f_procpointer
  use, intrinsic :: iso_c_binding, only: c_f_pointer
  use, intrinsic :: iso_c_binding, only: c_double
  use, intrinsic :: iso_c_binding, only: c_size_t
  use, intrinsic :: iso_fortran_env, only: int32
  use API, only: c_sphere_t => sphere_t
  use API, only: c_updater => updater
  use API, only: c_malloc
  use API, only: c_free
  use API, only: c_init
  implicit none
  private

  type, abstract, public :: particle_t
    contains
      procedure(updater), deferred, public :: update
  end type

  type, extends(particle_t), public :: sphere_t
    private
    type(c_ptr) :: workspace = c_null_ptr
    type(c_sphere_t), pointer :: c_spheres => null()
    procedure(c_updater), pointer, nopass :: c_update => null()
    contains
      private
      procedure, public :: update
      final :: destructor
  end type

  interface
    function updater (this) result(stat)
      use, intrinsic :: iso_fortran_env, only: int32
      import particle_t
      implicit none
      class(particle_t), intent(inout) :: this
      integer(kind = int32) :: stat
    end function
  end interface

  interface sphere_t
    module procedure constructor
  end interface

contains

  function constructor () result(spheres)
    type(c_ptr) :: ptr = c_null_ptr
    type(sphere_t), pointer :: spheres
    integer(kind = c_size_t), parameter :: log_numel = LOG_NUMEL
    real(kind = c_double), parameter :: dnumel = 2.0_c_double ** (log_numel)
    integer(kind = c_size_t), parameter :: numel = int(dnumel, kind = c_size_t)
    integer(kind = c_size_t), parameter :: size_sphere_t = 32_c_size_t
    integer(kind = c_size_t), parameter :: size_OBDS_Sphere_t = 256_c_size_t
    integer(kind = c_size_t), parameter :: size_prop_t = 8_c_size_t
    integer(kind = c_size_t), parameter :: size_random_t = 16_c_size_t
    integer(kind = c_size_t), parameter :: size_generator_t = 32_c_size_t
    integer(kind = c_size_t), parameter :: size_double = 8_c_size_t
    integer(kind = c_size_t), parameter :: size_uint64_t = 8_c_size_t
    integer(kind = c_size_t), parameter :: numel_list = (numel * log_numel)
    integer(kind = c_size_t), parameter :: sz = size_sphere_t + &
                                              & size_OBDS_Sphere_t + &
                                              & 25_c_size_t * numel * size_prop_t + &
                                              & numel_list * size_prop_t + &
                                              & size_random_t + &
                                              & size_generator_t + &
                                              & size_double + &
                                              & size_uint64_t
    integer(kind = int32) :: mstat
    character(*), parameter :: errmsg = "constructor(): memory allocation error"

    spheres => null()
    allocate(spheres, stat=mstat)
    if (mstat /= 0) then
      error stop errmsg
    end if

    spheres % workspace = c_malloc(sz)

    if ( .not. c_associated(spheres % workspace) ) then
      error stop errmsg
    end if

    ptr = c_init(spheres % workspace, 0)
    call c_f_pointer(ptr, spheres % c_spheres)
    call c_f_procpointer(spheres % c_spheres % update, spheres % c_update)

    return
  end function constructor

  function update (this) result(stat)
    class(sphere_t), intent(inout) :: this
    integer(kind = int32) :: stat

    stat = this % c_update(this % c_spheres)

    return
  end function update

  subroutine destructor (spheres)
    type(sphere_t), intent(inout) :: spheres

    if ( c_associated(spheres % workspace) ) then
      call c_free(spheres % workspace)
      spheres % workspace = c_null_ptr
      spheres % c_spheres => null()
    end if

    return
  end subroutine destructor

end module OBDS

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
  use, intrinsic :: iso_fortran_env, only: int32
  use API, only: OBDS_Sphere_t
  use API, only: sphere_t
  use API, only: c_malloc
  use API, only: c_free
  use API, only: c_init
  use API, only: updater
  use OBDS, only: f_sphere_t => sphere_t
  implicit none

  type(sphere_t) :: c_sphere_t
  type(OBDS_Sphere_t) :: c_OBDS_Sphere_t
  type(c_ptr) :: workspace = c_null_ptr
  type(c_ptr) :: c_spheres = c_null_ptr
  type(sphere_t), pointer :: spheres => null()
  type(OBDS_Sphere_t), pointer :: props => null()
  type(f_sphere_t), pointer :: f_spheres => null()
  procedure(updater), pointer :: update => null()
  real(kind = c_double), pointer, contiguous :: x(:) => null()
  real(kind = c_double), pointer, contiguous :: y(:) => null()
  real(kind = c_double), pointer, contiguous :: z(:) => null()
  integer(kind = c_size_t), parameter :: log_numel = LOG_NUMEL
  real(kind = c_double), parameter :: dnumel = 2.0_c_double ** (log_numel)
  integer(kind = c_size_t), parameter :: numel = int(dnumel, kind = c_size_t)
  real(kind = c_double), parameter :: syslim = real(LIMIT, kind = c_double)
  integer(kind = c_size_t), parameter :: size_sphere_t = 32_c_size_t
  integer(kind = c_size_t), parameter :: size_OBDS_Sphere_t = 256_c_size_t
  integer(kind = c_size_t), parameter :: size_prop_t = 8_c_size_t
  integer(kind = c_size_t), parameter :: size_random_t = 16_c_size_t
  integer(kind = c_size_t), parameter :: size_generator_t = 32_c_size_t
  integer(kind = c_size_t), parameter :: size_double = 8_c_size_t
  integer(kind = c_size_t), parameter :: size_uint64_t = 8_c_size_t
  integer(kind = c_size_t), parameter :: numel_list = (numel * log_numel)
  integer(kind = c_size_t), parameter :: sz = size_sphere_t + &
                                            & size_OBDS_Sphere_t + &
                                            & 25_c_size_t * numel * size_prop_t + &
                                            & numel_list * size_prop_t + &
                                            & size_random_t + &
                                            & size_generator_t + &
                                            & size_double + &
                                            & size_uint64_t

  logical(kind = c_bool) :: failed = .false.
  integer(kind = c_size_t) :: i
  integer(kind = int32) :: stat

  ! performs runtime checks (that are more like reminders that these are C types)

  if (c_sizeof(c_sphere_t) /= 32) then
    error stop "test(): expects size of sphere_t to be 32 bytes"
  end if

  if (c_sizeof(c_OBDS_Sphere_t) /= 256) then
    error stop "test(): expects size of OBDS_Sphere_t to be 128 bytes"
  end if

  workspace = c_malloc(sz)                      ! allocates workspace on the heap
  c_spheres = c_init(workspace, 0)              ! initializes the sphere properties
  call c_f_pointer(c_spheres, spheres)          ! binds to the C spheres container
  call c_f_pointer(spheres % props, props)      ! binds to the sphere properties
  call c_f_pointer(props % x, x, [numel])       ! binds to the x positions of the spheres
  call c_f_pointer(props % y, y, [numel])       ! binds to the y positions of the spheres
  call c_f_pointer(props % z, z, [numel])       ! binds to the z positions of the spheres
  call c_f_procpointer(spheres % update, update)! binds to the type-bound update() method

  ! puts the particles outside the box (we can afford particle overlapping in this test)

  x = 1.0009765625_c_double * syslim
  y = 1.0009765625_c_double * syslim
  z = 1.0009765625_c_double * syslim

  stat = update(spheres)                        ! applies periodic boundary conditions

  if (stat /= 0) then
    call c_free(workspace)                      ! frees workspace from memory
    workspace = c_null_ptr                      ! nullifies pointer to workspace
    error stop "main(): ERROR"
  end if

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

  ! checks if the update() method failed to limit the particles to the system box

  write (*, '(A)', advance = 'no') 'test-sphere[0]: '
  if (failed) then
    print '(A)', 'FAIL'
  else
    print '(A)', 'PASS'
  end if

  call c_free(workspace)                        ! frees workspace from memory
  workspace = c_null_ptr                        ! nullifies pointer to workspace

  f_spheres => f_sphere_t()                     ! instantiates OBDS f_sphere_t object
  stat = f_spheres % update()                   ! updates the positions of the spheres
  if (stat /= 0) then
    deallocate(f_spheres)
    error stop "main(): ERROR"
  end if
  deallocate(f_spheres)                         ! invokes the OBDS f_sphere_t finalizer

end program main

!   OpenBDS                                                     September 07, 2023
!
!   source: test.f
!   author: @misael-diaz
!
!   Synopsis:
!   Tests the updater method which applies the periodic boundary conditions.
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
