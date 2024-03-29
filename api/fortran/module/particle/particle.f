#define __PASS__  0_i8
#define __FAIL__ -1_i8

      module particle
        use, intrinsic :: iso_fortran_env, only: i8 => int64
        use, intrinsic :: iso_fortran_env, only: r8 => real64
        use :: config, only: LOG_NUM_PARTICLES
        use :: config, only: NUM_PARTICLES
        use :: config, only: TIME_STEP
        use :: config, only: LIMIT
        implicit none
        private

        type, abstract, public :: particle_t
c         position vector components (subjected to periodic conditions)
          real(r8), pointer, contiguous :: x(:) => null()
          real(r8), pointer, contiguous :: y(:) => null()
          real(r8), pointer, contiguous :: z(:) => null()
c         Verlet displacement vector components
          real(r8), pointer, contiguous :: Vdx(:) => null()
          real(r8), pointer, contiguous :: Vdy(:) => null()
          real(r8), pointer, contiguous :: Vdz(:) => null()
c         position vector components (periodic conditions independent)
          real(r8), pointer, contiguous :: r_x(:) => null()
          real(r8), pointer, contiguous :: r_y(:) => null()
          real(r8), pointer, contiguous :: r_z(:) => null()
c         Euler angle vector components
          real(r8), pointer, contiguous :: Eax(:) => null()
          real(r8), pointer, contiguous :: Eay(:) => null()
          real(r8), pointer, contiguous :: Eaz(:) => null()
c         orientation (or director) vector components
          real(r8), pointer, contiguous :: d_x(:) => null()
          real(r8), pointer, contiguous :: d_y(:) => null()
          real(r8), pointer, contiguous :: d_z(:) => null()
c         force vector components
          real(r8), pointer, contiguous :: f_x(:) => null()
          real(r8), pointer, contiguous :: f_y(:) => null()
          real(r8), pointer, contiguous :: f_z(:) => null()
c         torque vector components
          real(r8), pointer, contiguous :: t_x(:) => null()
          real(r8), pointer, contiguous :: t_y(:) => null()
          real(r8), pointer, contiguous :: t_z(:) => null()
c         temporary placeholder
          real(r8), pointer, contiguous :: tmp(:) => null()
c         Verlet neighbor-list
          real(r8), pointer, contiguous :: Vnl(:) => null()
c         particle identifiers IDs
          real(r8), pointer, contiguous :: id(:) => null()
c         data placeholder
          real(r8), pointer, contiguous :: data(:) => null()
          contains
            private
            procedure :: initializer
            procedure, public :: initialize => initializer
            procedure(iupdate), deferred, public :: update
            procedure(icallback), deferred, public :: callback
        end type particle_t

        abstract interface
c         updates the particle positions and orientations
          subroutine iupdate (particles)
            import particle_t
            implicit none
            class(particle_t), intent(inout) :: particles
          end subroutine
        end interface

        abstract interface
c         defines the inteparticle force (and torque) computations on the ith-particle
          pure subroutine icallback (particles, i)
            use, intrinsic :: iso_fortran_env, only: i8 => int64
            import particle_t
            implicit none
            class(particle_t), intent(inout) :: particles
            integer(i8), intent(in) :: i
          end subroutine
        end interface

      contains

        pure function test (t) result(outcome)
          logical(i8), intent(in) :: t
          integer(i8) :: outcome

          if (t) then
            outcome = __PASS__
          else
            outcome = __FAIL__
          end if

          return
        end function test


        subroutine sane ()
c         Synopsis:
c         Complains if the module parameters in `config` are illegal.
          integer(i8), parameter :: N = NUM_PARTICLES
          integer(i8), parameter :: LOG_N = LOG_NUM_PARTICLES
          integer(i8), parameter :: E = LOG_N ! Exponent
          real(r8), parameter :: L = LIMIT
          real(r8), parameter :: T = TIME_STEP
          logical(i8), parameter :: t1 = (N == 2_i8 ** E)
          logical(i8), parameter :: t2 = (L > 0.0_r8)
          logical(i8), parameter :: t3 = (T > 0.0_r8)
          character(*), parameter :: e1 = "particle::sane(): " //
     +    "IllegalParameterError: the number of particles is not a " //
     +    "power of two"
          character(*), parameter :: e2 = "particle::sane(): " //
     +    "IllegalParameterError: the system limit must be positive"
          character(*), parameter :: e3 = "particle::sane(): " //
     +    "IllegalParameterError: the time-step must be positive"

          if ( test(t1) == __FAIL__ ) then
            error stop e1
          end if

          if ( test(t2) == __FAIL__ ) then
            error stop e2
          end if

          if ( test(t3) == __FAIL__ ) then
            error stop e3
          end if

          return
        end subroutine sane


        pure subroutine iota (x)
c         Synopsis:
c         implements a std::iota C++ like method
c         fills array `x' with values in symmetric range [1, numel]
          integer(i8), parameter :: numel = NUM_PARTICLES
          real(r8), intent(out) :: x(numel)
          integer(i8) :: i

          do i = 1, numel
            x(i) = real(i, kind = r8)
          end do

          return
        end subroutine iota


        subroutine initializer (particles)
c         Synopsis:
c         In the Object-Oriented Programming OOP sense this subroutine initializes the
c         core of the particle objects (though bear in mind that we are handling them
c         as a collection and not as individual objects so that we can ultimately write
c         loops in a way that the compiler can optimize them via auto-vectorization).
c         NOTES:
c         The constructor of the extending classes must invoke this subroutine, as you
c         would do in C++ or Java.
          class(particle_t), intent(inout) :: particles
          real(r8), pointer, contiguous :: id(:) => null()
          integer(i8), parameter :: N = NUM_PARTICLES
c         size of the components of the position vector
          integer(i8), parameter :: size_x = N
          integer(i8), parameter :: size_y = N
          integer(i8), parameter :: size_z = N
c         size of the components of the Verlet displacement vector
          integer(i8), parameter :: size_Vdx = N
          integer(i8), parameter :: size_Vdy = N
          integer(i8), parameter :: size_Vdz = N
c         size of the components of the absolute position vector
          integer(i8), parameter :: size_r_x = N
          integer(i8), parameter :: size_r_y = N
          integer(i8), parameter :: size_r_z = N
c         size of the components of the Euler angle vector
          integer(i8), parameter :: size_Eax = N
          integer(i8), parameter :: size_Eay = N
          integer(i8), parameter :: size_Eaz = N
c         size of the components of the director (or orientation) vector
          integer(i8), parameter :: size_d_x = N
          integer(i8), parameter :: size_d_y = N
          integer(i8), parameter :: size_d_z = N
c         size of the components of the force vector
          integer(i8), parameter :: size_f_x = N
          integer(i8), parameter :: size_f_y = N
          integer(i8), parameter :: size_f_z = N
c         size of the components of the torque vector
          integer(i8), parameter :: size_t_x = N
          integer(i8), parameter :: size_t_y = N
          integer(i8), parameter :: size_t_z = N
c         size of the temporary placeholder
          integer(i8), parameter :: size_tmp = 12_i8 * N
c         size of the Verlet neighbor-list (NOTE: we shall allocate more later)
          integer(i8), parameter :: size_Vnl = N
c         size of the particle identifiers IDs
          integer(i8), parameter :: size_id = N
c         size of the whole contiguous data block that holds the particle fields
          integer(i8), parameter :: size_data =
c         sizes the components of the position vector
     +                                          size_x +
     +                                          size_y +
     +                                          size_z +
c         sizes the components of the Verlet displacement vector
     +                                          size_Vdx +
     +                                          size_Vdy +
     +                                          size_Vdz +
c         sizes the components of the absolute position vector
     +                                          size_r_x +
     +                                          size_r_y +
     +                                          size_r_z +
c         sizes the components of the Euler angle vector
     +                                          size_Eax +
     +                                          size_Eay +
     +                                          size_Eaz +
c         sizes the components of the director (or orientation) vector
     +                                          size_d_x +
     +                                          size_d_y +
     +                                          size_d_z +
c         sizes the components of the force vector
     +                                          size_f_x +
     +                                          size_f_y +
     +                                          size_f_z +
c         sizes the components of the torque vector
     +                                          size_t_x +
     +                                          size_t_y +
     +                                          size_t_z +
c         sizes the temporary placeholder
     +                                          size_tmp +
c         sizes the Verlet neighbor-list
     +                                          size_Vnl +
c         sizes the particle identifiers IDs
     +                                          size_id
          integer(i8) :: mstat
          integer(i8) :: sz ! size

          call sane()

          allocate(particles % data(size_data), stat = mstat)
          if (mstat /= 0_i8) then
            error stop "particle::initializer(): allocation error"
          end if

          particles % data = 0.0_r8

c         position vector binding
          sz = 0_i8
          particles % x => particles % data(sz + 1:sz + size_x)

          sz = sz + size_x
          particles % y => particles % data(sz + 1:sz + size_y)

          sz = sz + size_y
          particles % z => particles % data(sz + 1:sz + size_z)

c         Verlet displacement vector binding
          sz = sz + size_z
          particles % Vdx => particles % data(sz + 1:sz + size_Vdx)

          sz = sz + size_Vdx
          particles % Vdy => particles % data(sz + 1:sz + size_Vdy)

          sz = sz + size_Vdy
          particles % Vdz => particles % data(sz + 1:sz + size_Vdz)

c         absolute position vector binding
          sz = sz + size_Vdz
          particles % r_x => particles % data(sz + 1:sz + size_r_x)

          sz = sz + size_r_x
          particles % r_y => particles % data(sz + 1:sz + size_r_y)

          sz = sz + size_r_y
          particles % r_z => particles % data(sz + 1:sz + size_r_z)

c         Euler angles vector binding
          sz = sz + size_r_z
          particles % Eax => particles % data(sz + 1:sz + size_Eax)

          sz = sz + size_Eax
          particles % Eay => particles % data(sz + 1:sz + size_Eay)

          sz = sz + size_Eay
          particles % Eaz => particles % data(sz + 1:sz + size_Eaz)

c         director (or orientation) vector binding
          sz = sz + size_Eaz
          particles % d_x => particles % data(sz + 1:sz + size_d_x)

          sz = sz + size_d_x
          particles % d_y => particles % data(sz + 1:sz + size_d_y)

          sz = sz + size_d_y
          particles % d_z => particles % data(sz + 1:sz + size_d_z)

c         force vector binding
          sz = sz + size_d_z
          particles % f_x => particles % data(sz + 1:sz + size_f_x)

          sz = sz + size_f_x
          particles % f_y => particles % data(sz + 1:sz + size_f_y)

          sz = sz + size_f_z
          particles % f_z => particles % data(sz + 1:sz + size_f_z)

c         torque vector binding
          sz = sz + size_f_z
          particles % t_x => particles % data(sz + 1:sz + size_t_x)

          sz = sz + size_t_x
          particles % t_y => particles % data(sz + 1:sz + size_t_y)

          sz = sz + size_t_y
          particles % t_z => particles % data(sz + 1:sz + size_t_z)

c         temporary placeholder binding
          sz = sz + size_t_z
          particles % tmp => particles % data(sz + 1:sz + size_tmp)

c         Verlet neighbor-list binding
          sz = sz + size_tmp
          particles % Vnl => particles % data(sz + 1:sz + size_Vnl)

c         particle identifier IDs binding
          sz = sz + size_Vnl
          particles % id => particles % data(sz + 1:sz + size_id)

c         assigns unique IDs to the particles
          id => particles % id

          call iota(id)

          return
        end subroutine initializer

      end module particle

*   OpenBDS                                             October 21, 2023
*
*   source: api/fortran/module/particle/particle.f
*   author: @misael-diaz
*
*   Synopsis:
*   Defines the Core Particle Type.
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
