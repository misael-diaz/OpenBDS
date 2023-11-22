      module vector
        use, intrinsic :: iso_fortran_env, only: r8 => real64
        use :: config, only: N => NUM_PARTICLES
        implicit none
        private
        public :: vector__vec
        public :: vector__add
        public :: vector__diff
        public :: vector__norm

        interface vector__vec
          module procedure vec
        end interface

        interface vector__add
          module procedure add
        end interface

        interface vector__diff
          module procedure diff
        end interface
        
        interface vector__norm
          module procedure norm
        end interface

      contains

        pure subroutine vec (x, y, z, v_x, v_y, v_z)
c         Synopsis:
c         Vectorizer.
          real(r8), intent(in) :: x
          real(r8), intent(in) :: y
          real(r8), intent(in) :: z
          real(r8), intent(out) :: v_x(N)
          real(r8), intent(out) :: v_y(N)
          real(r8), intent(out) :: v_z(N)

          v_x = x
          v_y = y
          v_z = z

          return
        end subroutine vec


        elemental subroutine add_elem (x1, x2, x3)
c         Synopsis:
c         Implements vector addition.
          real(r8), intent(in) :: x1
          real(r8), intent(in) :: x2
          real(r8), intent(out) :: x3
          
          x3 = (x1 + x2)

          return
        end subroutine add_elem


        elemental subroutine diff_elem (x1, x2, x3)
c         Synopsis:
c         Implements vector difference.
          real(r8), intent(in) :: x1
          real(r8), intent(in) :: x2
          real(r8), intent(out) :: x3

          x3 = (x2 - x1)

          return
        end subroutine diff_elem


        elemental subroutine norm_elem (x, y, z, vn)
c         Synopsis:
c         Implements vector norm.
c         vector components
          real(r8), intent(in) :: x
          real(r8), intent(in) :: y
          real(r8), intent(in) :: z
c         squared vector norm
          real(r8), intent(out) :: vn

          vn = x**2 + y**2 + z**2

          return
        end subroutine norm_elem


        pure subroutine add (
     +    u_x,
     +    u_y,
     +    u_z,
     +    v_x,
     +    v_y,
     +    v_z,
     +    w_x,
     +    w_y,
     +    w_z
     +  )
c         Synopsis:
c         Implements vector addition.
c         components of vector v1
          real(r8), intent(in) :: u_x(N)
          real(r8), intent(in) :: u_y(N)
          real(r8), intent(in) :: u_z(N)
c         components of vector v2
          real(r8), intent(in) :: v_x(N)
          real(r8), intent(in) :: v_y(N)
          real(r8), intent(in) :: v_z(N)
c         components of vector v3
          real(r8), intent(out) :: w_x(N)
          real(r8), intent(out) :: w_y(N)
          real(r8), intent(out) :: w_z(N)

          call add_elem(u_x, v_x, w_x)
          call add_elem(u_y, v_y, w_y)
          call add_elem(u_z, v_z, w_z)

          return
        end subroutine add


        pure subroutine diff (
     +    u_x,
     +    u_y,
     +    u_z,
     +    v_x,
     +    v_y,
     +    v_z,
     +    w_x,
     +    w_y,
     +    w_z
     +  )
c         Synopsis:
c         Synopsis:
c         Implements vector difference.
c         components of vector v1
          real(r8), intent(in) :: u_x(N)
          real(r8), intent(in) :: u_y(N)
          real(r8), intent(in) :: u_z(N)
c         components of vector v2
          real(r8), intent(in) :: v_x(N)
          real(r8), intent(in) :: v_y(N)
          real(r8), intent(in) :: v_z(N)
c         components of vector v3
          real(r8), intent(out) :: w_x(N)
          real(r8), intent(out) :: w_y(N)
          real(r8), intent(out) :: w_z(N)

          call diff_elem(u_x, v_x, w_x)
          call diff_elem(u_y, v_y, w_y)
          call diff_elem(u_z, v_z, w_z)

          return
        end subroutine diff


        pure subroutine norm (x, y, z, squares, vnorm)
c         Synopsis:
c         Implements vector norm.
c         vector components
          real(r8), intent(in) :: x(N)
          real(r8), intent(in) :: y(N)
          real(r8), intent(in) :: z(N)
c         array temporary storing the squares (that is, x**2 + y**2 + z**2)
          real(r8), intent(out) :: squares(N)
c         vector norm
          real(r8), intent(out) :: vnorm(N)

          call norm_elem(x, y, z, squares)

          vnorm = sqrt(squares)

          return
        end subroutine norm

      end module vector

*   OpenBDS                                             November 07, 2023
*
*   source: api/fortran/module/vector/vector.f
*   author: @misael-diaz
*
*   Synopsis:
*   Implements (some) vector operations.
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
