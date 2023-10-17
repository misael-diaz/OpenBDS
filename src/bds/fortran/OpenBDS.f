#include "config/fconfig.h"

#define LIMIT FCONF_LIMIT
#define LOG_NUMEL FCONF_LOG_NUM_PARTICLES

module API
  use, intrinsic :: iso_c_binding, only: c_ptr
  use, intrinsic :: iso_c_binding, only: c_funptr
  use, intrinsic :: iso_c_binding, only: c_int64_t
  implicit none
  private

  public :: c_sphere_t
  public :: c_updater
  public :: c_malloc
  public :: c_free
  public :: c_init

  type, bind(c) :: c_sphere_t
    type(c_ptr) :: c_props
    type(c_ptr) :: c_prng
    type(c_funptr) :: c_update
    type(c_funptr) :: c_log
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
    function c_updater (spheres) result(stat)
      use, intrinsic :: iso_c_binding, only: c_int
      import c_sphere_t
      implicit none
      type(c_sphere_t), intent(inout) :: spheres
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
  use, intrinsic :: iso_c_binding, only: c_int
  use API, only: c_sphere_t
  use API, only: c_updater
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
      use, intrinsic :: iso_c_binding, only: c_int
      import particle_t
      implicit none
      class(particle_t), intent(inout) :: this
      integer(kind = c_int) :: stat
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
    integer(kind = c_int) :: mstat
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

    if ( .not. c_associated(ptr) ) then
      call c_free(spheres % workspace)
      spheres % workspace = c_null_ptr
      error stop "constructor(): ERROR"
    end if

    call c_f_pointer(ptr, spheres % c_spheres)
    call c_f_procpointer(spheres % c_spheres % c_update, spheres % c_update)

    return
  end function constructor

  function update (this) result(stat)
    class(sphere_t), intent(inout) :: this
    integer(kind = c_int) :: stat

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

program OpenBDS
  use, intrinsic :: iso_c_binding, only: c_int
  use, intrinsic :: iso_c_binding, only: c_size_t
  use OBDS, only: sphere_t
  implicit none

  type(sphere_t), pointer :: spheres => null()
  integer(kind = c_size_t) :: step
  integer(kind = c_int) :: stat

  spheres => sphere_t()                 ! instantiates OBDS sphere_t object
  step = 0_c_size_t
  do while (step /= 16777216_c_size_t)

    stat = spheres % update()           ! updates spheres positions and orientations
    if (stat /= 0) then
      deallocate(spheres)
      error stop "OBDS(): ERROR"
    end if

    step = step + 1_c_size_t
  end do

  deallocate(spheres)                   ! invokes the OBDS sphere_t finalizer

end program OpenBDS

!   OpenBDS                                                     September 21, 2023
!
!   source: OpenBDS.f
!   author: @misael-diaz
!
!   Synopsis:
!   OpenBDS driver code.
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
