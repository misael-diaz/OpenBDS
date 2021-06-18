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


        module function constructor (n) result(vector)
            type(vector_t) :: vector
            integer(kind = int64), intent(in) :: n
            call initializer (vector, n)
            return
        end function


        module subroutine initializer (vector, n)
            ! Synopsis: Assigns default values to vector components.
            type(vector_t), intent(inout) :: vector
            integer(kind = int64), intent(in) :: n
            

            call allocator (vector % stat)
            call allocator (vector % size)
            call allocator (n, vector % x)
            call allocator (n, vector % y)
            call allocator (n, vector % z)
            call allocator (n, vector % v)


            associate (x => vector % x,    y => vector % y, &
                     & z => vector % z,    v => vector % v, &
                     & size => vector % size % n, &
                     & init => vector % stat % init)

                x    = 0.0_real64
                y    = 0.0_real64
                z    = 0.0_real64
                v    = 0.0_real64
                size = n
                init = .true.

            end associate

            return
        end subroutine


        module subroutine normalize_method (self)
            class(vector_t), intent(inout) :: self

            if ( allocated(self % stat) ) then
                call normalizer (self)
            end if

            return
        end subroutine


        module subroutine normalizer (vector)
            type(vector_t), intent(inout) :: vector
            real(kind = real64) :: t(vector % size % n)


            call moduli (vector)
            call vector_guard_singular (vector)
 

            t = 0.0_real64
            associate(x => vector % x, y => vector % y, &
                    & z => vector % z, v => vector % v)

               call normalize (x, y, z, t, v)

            end associate


            return
        end subroutine


        module subroutine vector_guard_singular (vector)
            type(vector_t), intent(in) :: vector

            if ( minval(vector % v) < vector_eps_mod ) then
                error stop "math::vector.vector_guard_singular (): "//&
                    & "abort singular"
            end if

            return
        end subroutine


        module elemental subroutine normalize (x, y, z, t, v)
            real(kind = real64), intent(inout) :: x
            real(kind = real64), intent(inout) :: y
            real(kind = real64), intent(inout) :: z
            real(kind = real64), intent(inout) :: t
            real(kind = real64), intent(in)    :: v

            t = x / v
            x = t

            t = y / v
            y = t

            t = z / v
            z = t

            return
        end subroutine


        module subroutine moduli (vector)
            type(vector_t), intent(inout) :: vector
            real(kind = real64) :: t(vector % size % n)


            t = 0.0_real64
            associate(x => vector % x, y => vector % y, &
                    & z => vector % z, v => vector % v)
               
               call modulus (x, y, z, t)
               v = dsqrt(t)

            end associate


            return
        end subroutine


        module elemental subroutine modulus (x, y, z, v)
            real(kind = real64), intent(in)    :: x
            real(kind = real64), intent(in)    :: y
            real(kind = real64), intent(in)    :: z
            real(kind = real64), intent(inout) :: v


            v = x**2 + y**2 + z**2

            
            return
        end subroutine


        module subroutine allocate_stat_t (s)
            type(stat_t), intent(inout), allocatable :: s
            integer(kind = int32) :: mstat = 0

            if ( .not. allocated(s) ) then
                allocate (s, stat = mstat)
            end if

            if (mstat /= 0) then
                error stop "math::vector.allocate_stat: allocation failure"
            end if

            return
        end subroutine


        module subroutine allocate_size_t (s)
            type(size_t), intent(inout), allocatable :: s
            integer(kind = int32) :: mstat = 0

            if ( .not. allocated(s) ) then
                allocate (s, stat = mstat)
            end if

            if (mstat /= 0) then
                error stop "math::vector.allocate_size: allocation failure"
            end if

            return
        end subroutine


        module subroutine deallocate_stat_t (s)
            type(stat_t), intent(inout), allocatable :: s
            integer(kind = int32) :: mstat = 0

            if ( allocated(s) ) then
                deallocate (s, stat = mstat)
            end if

            if (mstat /= 0) then
                error stop "math::vector.deallocate_stat: "// &
                    & "deallocation failure"
            end if

            return
        end subroutine


        module subroutine deallocate_size_t (s)
            type(size_t), intent(inout), allocatable :: s
            integer(kind = int32) :: mstat = 0

            if ( allocated(s) ) then
                deallocate (s, stat = mstat)
            end if

            if (mstat /= 0) then
                error stop "math::vector.deallocate_size: "// &
                    & "deallocation failure"
            end if

            return
        end subroutine


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
            call deallocator (vector % v)
            call deallocator (vector % size)
            call deallocator (vector % stat)

            return
        end subroutine


end submodule


! TODO:
! [x] implement GUARD against operating on uninitialized vectors
! [x] to implement GUARD against normalizing a singular vector
! [x] check vectorizer report and fix vectorization misses (if possible)


! Vectorization:
! Both the GNU and Intel Fortran Compilers vectorize all the loops. Using
! array temporaries was necessary in some cases for the compiler to
! vectorize the code without directives. Intel Fortran Compiler was
! reluctant to vectorize loops with potential aliasing (when the
! variable to write was a component of vector in associate constructs).


! Comments on Procedures:
!
!
! subroutine normalize_method (self):
!
! Guards against attempts to invoke method on an uninitialized vector.
! Even though the value of the state/status type :[stat_t]: is set
! upon initialization it won't be available for uninitialized vectors
! since memory needs to be allocated. This is why we only check if this
! component is allocated. At this point any component could do to
! achieve this however, having a state/status component might be useful
! later. This is the reason for defining it.
