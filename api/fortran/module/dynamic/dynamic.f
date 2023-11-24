#define TD_ARG \
      particles,\
      x,\
      y,\
      z,\
      r_x,\
      r_y,\
      r_z,\
      Vdx,\
      Vdy,\
      Vdz,\
      F_x,\
      F_y,\
      F_z

#define T_ARG \
      mob,\
      F_x,\
      F_y,\
      F_z,\
      x,\
      y,\
      z,\
      r_x,\
      r_y,\
      r_z,\
      Vdx,\
      Vdy,\
      Vdz

      module dynamic
        use, intrinsic :: iso_fortran_env, only: i4 => int32
        use, intrinsic :: iso_fortran_env, only: r8 => real64
        use :: config, only: dt => TIME_STEP
        use :: config, only: N => NUM_PARTICLES
        use :: config, only: DETERMINISTIC
        use :: config, only: BROWNIAN
        use :: particle, only: particle_t
        implicit none
        private
        public :: dynamic__translator

        interface dynamic__translator
          module procedure translate_base
        end interface

        interface
c         shifts the (isotropic) particles in the direction of the force
          pure module subroutine shifter (mobility, F_x, x)
            real(r8), intent(in) :: mobility
            real(r8), intent(in) :: F_x(N)
            real(r8), intent(inout) :: x(N)
          end subroutine
        end interface

        interface
c         shifts the particles position vectors
          pure module subroutine shift (mob, F_x, F_y, F_z, x, y, z)
c           particle mobility (either Brownian or deterministic)
            real(r8), intent(in) :: mob
c           force vectors
            real(r8), intent(in) :: F_x(N)
            real(r8), intent(in) :: F_y(N)
            real(r8), intent(in) :: F_z(N)
c           position vectors
            real(r8), intent(inout) :: x(N)
            real(r8), intent(inout) :: y(N)
            real(r8), intent(inout) :: z(N)
          end subroutine shift
        end interface

        interface
c         shifts the particles position vectors
          pure module subroutine translate (T_ARG)
c           particle mobility (either Brownian or deterministic)
            real(r8), intent(in) :: mob
c           force vectors
            real(r8), intent(in) :: F_x(N)
            real(r8), intent(in) :: F_y(N)
            real(r8), intent(in) :: F_z(N)
c           position vectors affected by periodic conditions
            real(r8), intent(inout) :: x(N)
            real(r8), intent(inout) :: y(N)
            real(r8), intent(inout) :: z(N)
c           position vectors independent of the system periodicity
            real(r8), intent(inout) :: r_x(N)
            real(r8), intent(inout) :: r_y(N)
            real(r8), intent(inout) :: r_z(N)
c           Verlet displacement vectors
            real(r8), intent(inout) :: Vdx(N)
            real(r8), intent(inout) :: Vdy(N)
            real(r8), intent(inout) :: Vdz(N)
          end subroutine translate
        end interface

        interface
c         destructures the position and force vectors of the particles
          pure module subroutine translate_dstruct (TD_ARG)
            class(particle_t), intent(inout), target :: particles
c           position vectors affected by periodic conditions
            real(r8), pointer, contiguous, intent(out) :: x(:)
            real(r8), pointer, contiguous, intent(out) :: y(:)
            real(r8), pointer, contiguous, intent(out) :: z(:)
c           position vectors independent of the system periodicity
            real(r8), pointer, contiguous, intent(out) :: r_x(:)
            real(r8), pointer, contiguous, intent(out) :: r_y(:)
            real(r8), pointer, contiguous, intent(out) :: r_z(:)
c           Verlet displacement vectors
            real(r8), pointer, contiguous, intent(out) :: Vdx(:)
            real(r8), pointer, contiguous, intent(out) :: Vdy(:)
            real(r8), pointer, contiguous, intent(out) :: Vdz(:)
c           force vectors
            real(r8), pointer, contiguous, intent(out) :: F_x(:)
            real(r8), pointer, contiguous, intent(out) :: F_y(:)
            real(r8), pointer, contiguous, intent(out) :: F_z(:)
          end subroutine translate_dstruct
        end interface

        interface
c         translates the particles according to the MODE (BROWNIAN | DETERMINISTIC)
          pure module subroutine translate_base (particles, MODE)
            class(particle_t), intent(inout) :: particles
            integer(i4), intent(in) :: MODE
          end subroutine
        end interface

      end module dynamic


      submodule (dynamic) dynamic_mod_methods
        implicit none
      contains

        module procedure shifter

          x = x + mobility * F_x

          return
        end procedure shifter


        module procedure shift

          call shifter(mob, F_x, x)
          call shifter(mob, F_y, y)
          call shifter(mob, F_z, z)

          return
        end procedure shift


        module procedure translate

          call shift(mob, F_x, F_y, F_z,   x,   y,   z)
          call shift(mob, F_x, F_y, F_z, r_x, r_y, r_z)
          call shift(mob, F_x, F_y, F_z, Vdx, Vdy, Vdz)

          return
        end procedure translate


        module procedure translate_dstruct

          x => particles % x
          y => particles % y
          z => particles % z

          r_x => particles % r_x
          r_y => particles % r_y
          r_z => particles % r_z

          Vdx => particles % Vdx
          Vdy => particles % Vdy
          Vdz => particles % Vdz

          F_x => particles % F_x
          F_y => particles % F_y
          F_z => particles % F_z

          return
        end procedure translate_dstruct


        module procedure translate_base
          real(r8), pointer, contiguous :: x(:)
          real(r8), pointer, contiguous :: y(:)
          real(r8), pointer, contiguous :: z(:)
          real(r8), pointer, contiguous :: r_x(:)
          real(r8), pointer, contiguous :: r_y(:)
          real(r8), pointer, contiguous :: r_z(:)
          real(r8), pointer, contiguous :: Vdx(:)
          real(r8), pointer, contiguous :: Vdy(:)
          real(r8), pointer, contiguous :: Vdz(:)
          real(r8), pointer, contiguous :: F_x(:)
          real(r8), pointer, contiguous :: F_y(:)
          real(r8), pointer, contiguous :: F_z(:)

          call translate_dstruct(TD_ARG)

          select case (MODE)
            case (BROWNIAN)
              block
                real(r8), parameter :: mob = sqrt(2.0_r8 * dt)
                call translate(T_ARG)
              end block
            case default
              block
                real(r8), parameter :: mob = dt
                call translate(T_ARG)
              end block
          end select

          return
        end procedure translate_base

      end submodule

*   OpenBDS                                             October 24, 2023
*
*   source: api/fortran/module/dynamic/dynamic.f
*   author: @misael-diaz
*
*   Synopsis:
*   Implements methods for updating the position and orientation of the particles.
*
*   Copyright (C) 2023 Misael Díaz-Maldonado
*
*   This program is free software: you can redistribute it and/or modify
*   it under the terms of the GNU General Public License as published by
*   the Free Software Foundation, either version 3 of the License, or
*   (at your option) any later version.
*
*   This program is distributed in the hope that it will be useful,
*   but WITHOUT ANY WARRANTY; without even the implied warranty of
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*   GNU General Public License for more details.
*
*   You should have received a copy of the GNU General Public License
*   along with this program. If not, see <http://www.gnu.org/licenses/>.
*
*   References:
*   [0] SJ Chapman, FORTRAN for Scientists and Engineers, 4th edition.
*   [1] MP Allen and DJ Tildesley, Computer Simulation of Liquids.
*   [2] S Kim and S Karrila, Microhydrodynamics.

c   NOTE:
c   This methods apply to spheres. We shall overload these method later
c   with a callback, as in the clang API, to handle anisotropic particles.
