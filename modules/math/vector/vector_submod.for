!
!   source:  vector_submod.f90
!   author:  misael-diaz
!   date:    2021-06-17
!
!   Synopsis:
!   Implements the procedures of the math vector class.
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
!

submodule (math_vector_class) math_vector_class_implementations
    contains


        module subroutine finalizer (vector)
            ! Synopsis: Frees memory allocated for vector.
            type(vector_t), intent(inout) :: vector

            write (*, '(A)', advance='no') "destroying vector ... "
            call destructor (vector)
            print *, "done"

            return
        end subroutine



        module subroutine destructor (vector)
            ! Synopsis: Destroys the components of the vector.
            type(vector_t), intent(inout) :: vector

            call deallocator (vector % x)
            call deallocator (vector % y)
            call deallocator (vector % z)

            return
        end subroutine


end submodule
