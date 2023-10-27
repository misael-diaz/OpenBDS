      module particle
        use, intrinsic :: iso_fortran_env, only: int64
        use, intrinsic :: iso_fortran_env, only: real64
        use :: constant, only: NUM_PARTICLES
        implicit none
        private

        type, abstract, public :: particle_t
c         position vector components (subjected to periodic conditions)
          real(kind = real64), pointer, contiguous :: x(:) => null()
          real(kind = real64), pointer, contiguous :: y(:) => null()
          real(kind = real64), pointer, contiguous :: z(:) => null()
c         Verlet displacement vector components
          real(kind = real64), pointer, contiguous :: Vdx(:) => null()
          real(kind = real64), pointer, contiguous :: Vdy(:) => null()
          real(kind = real64), pointer, contiguous :: Vdz(:) => null()
c         position vector components (periodic conditions independent)
          real(kind = real64), pointer, contiguous :: r_x(:) => null()
          real(kind = real64), pointer, contiguous :: r_y(:) => null()
          real(kind = real64), pointer, contiguous :: r_z(:) => null()
c         Euler angle vector components
          real(kind = real64), pointer, contiguous :: Eax(:) => null()
          real(kind = real64), pointer, contiguous :: Eay(:) => null()
          real(kind = real64), pointer, contiguous :: Eaz(:) => null()
c         orientation (or director) vector components
          real(kind = real64), pointer, contiguous :: d_x(:) => null()
          real(kind = real64), pointer, contiguous :: d_y(:) => null()
          real(kind = real64), pointer, contiguous :: d_z(:) => null()
c         force vector components
          real(kind = real64), pointer, contiguous :: f_x(:) => null()
          real(kind = real64), pointer, contiguous :: f_y(:) => null()
          real(kind = real64), pointer, contiguous :: f_z(:) => null()
c         torque vector components
          real(kind = real64), pointer, contiguous :: t_x(:) => null()
          real(kind = real64), pointer, contiguous :: t_y(:) => null()
          real(kind = real64), pointer, contiguous :: t_z(:) => null()
c         temporary placeholder
          real(kind = real64), pointer, contiguous :: tmp(:) => null()
c         Verlet neighbor-list
          real(kind = real64), pointer, contiguous :: Vnl(:) => null()
c         particle identifiers IDs
          real(kind = real64), pointer, contiguous :: id(:) => null()
c         data placeholder
          real(kind = real64), pointer, contiguous :: data(:) => null()
          contains
            private
c           memory allocations and initializations
            procedure :: initializer
c           bindings:
            procedure, public :: initialize => initializer
c           updates the particle positions and orientations
            procedure(iupdate), deferred, public :: update
        end type particle_t

        abstract interface
          subroutine iupdate (particles)
            import particle_t
            class(particle_t), intent(inout) :: particles
          end subroutine
        end interface

      contains

        subroutine iota (x)
c         Synopsis:
c         implements a std::iota C++ like method
c         fills array `x' with values in symmetric range [1, numel]
          integer(kind = int64), parameter :: numel = NUM_PARTICLES
          real(kind = real64), intent(out) :: x(numel)
          integer(kind = int64) :: i

          do i = 1, numel
            x(i) = real(i, kind = real64)
          end do

          return
        end subroutine iota


        subroutine initializer (particles)
c         Synopsis:
c         Particles Initializer.
c         In the Object-Oriented Programming OOP sense this subroutine initializes the
c         core of the particle objects (though bear in mind that we are handling them
c         as a collection and not as individual objects so that we can ultimately write
c         loops in a way that the compiler can optimize them via auto-vectorization).
c         NOTES:
c         The constructor of the extending classes must invoke this subroutine, as you
c         would do in C++ or Java.
          class(particle_t), intent(inout) :: particles
          real(kind = real64), pointer, contiguous :: id(:) => null()
          integer(kind = int64), parameter :: N = NUM_PARTICLES
c         size position vector
          integer(kind = int64), parameter :: size_x = N
          integer(kind = int64), parameter :: size_y = N
          integer(kind = int64), parameter :: size_z = N
c         size Verlet displacement vector
          integer(kind = int64), parameter :: size_Vdx = N
          integer(kind = int64), parameter :: size_Vdy = N
          integer(kind = int64), parameter :: size_Vdz = N
c         size absolute position vector
          integer(kind = int64), parameter :: size_r_x = N
          integer(kind = int64), parameter :: size_r_y = N
          integer(kind = int64), parameter :: size_r_z = N
c         size Euler angle vector
          integer(kind = int64), parameter :: size_Eax = N
          integer(kind = int64), parameter :: size_Eay = N
          integer(kind = int64), parameter :: size_Eaz = N
c         size director (or orientation) vector
          integer(kind = int64), parameter :: size_d_x = N
          integer(kind = int64), parameter :: size_d_y = N
          integer(kind = int64), parameter :: size_d_z = N
c         size force vector
          integer(kind = int64), parameter :: size_f_x = N
          integer(kind = int64), parameter :: size_f_y = N
          integer(kind = int64), parameter :: size_f_z = N
c         size torque vector
          integer(kind = int64), parameter :: size_t_x = N
          integer(kind = int64), parameter :: size_t_y = N
          integer(kind = int64), parameter :: size_t_z = N
c         size temporary placeholder
          integer(kind = int64), parameter :: size_tmp = N
c         size Verlet neighbor-list (NOTE: we shall allocate more later)
          integer(kind = int64), parameter :: size_Vnl = N
c         size particle identifiers IDs
          integer(kind = int64), parameter :: size_id = N
c         sizes position vector
          integer(kind = int64), parameter :: size_data = size_x +
     +                                                    size_y +
     +                                                    size_z +
c         sizes Verlet displacement vector
     +                                                    size_Vdx +
     +                                                    size_Vdy +
     +                                                    size_Vdz +
c         sizes absolute position vector
     +                                                    size_r_x +
     +                                                    size_r_y +
     +                                                    size_r_z +
c         sizes Euler angle vector
     +                                                    size_Eax +
     +                                                    size_Eay +
     +                                                    size_Eaz +
c         sizes director (or orientation) vector
     +                                                    size_d_x +
     +                                                    size_d_y +
     +                                                    size_d_z +
c         sizes force vector
     +                                                    size_f_x +
     +                                                    size_f_y +
     +                                                    size_f_z +
c         sizes torque vector
     +                                                    size_t_x +
     +                                                    size_t_y +
     +                                                    size_t_z +
c         sizes temporary placeholder
     +                                                    size_tmp +
c         sizes Verlet neighbor-list
     +                                                    size_Vnl +
c         sizes particle identifiers IDs
     +                                                    size_id
c         memory allocation status
          integer(kind = int64) :: mstat
c         size
          integer(kind = int64) :: sz

c         allocates memory for the particle data
          allocate(particles % data(size_data), stat = mstat)
          if (mstat /= 0_int64) then
            error stop "particle::initializer(): allocation error"
          end if

c         initializes the particle data with zeros
          particles % data = 0.0_real64

c         position vector binding
          sz = 0_int64
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
