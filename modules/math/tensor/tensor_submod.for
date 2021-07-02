!
!   source:  tensor_submod.f90
!   author:  misael-diaz
!   date:    2021-06-17
!
!   Synopsis:
!   Implements the procedures of the tensor class.
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

submodule (TensorClass) tensor_class_implementations
    contains


        module function constructor (n) result(tensor)
            type(tensor_t) :: tensor
            integer(kind = int64), intent(in) :: n
            call initializer (tensor, n)
            return
        end function


        module subroutine initializer (tensor, n)
            ! Synopsis: Assigns default values to tensor components.
            type(tensor_t), intent(inout) :: tensor
            integer(kind = int64), intent(in) :: n
            

            call allocator (tensor % stat)
            call allocator (tensor % size)
            call allocator (n, tensor % x)
            call allocator (n, tensor % y)
            call allocator (n, tensor % z)
            call allocator (n, tensor % v)


            associate (x => tensor % x,    y => tensor % y, &
                     & z => tensor % z,    v => tensor % v, &
                     & size => tensor % size % n, &
                     & init => tensor % stat % init)

                size = n
                init = .true.
                call initialize (x)
                call initialize (y)
                call initialize (z)
                call initialize (v)

            end associate

            return
        end subroutine


        module elemental subroutine initialize (value)
            real(kind = real64), intent(out) :: value
            value = 0.0_real64
            return
        end subroutine


        module subroutine delta_forall_method (self, i)
            ! Synopsis:
            ! Obtains the distance of the ith tensor v[i] relative to v[:].
            class(tensor_t), intent(inout) :: self
            integer(kind = int64), intent(in) :: i

            self % v(:) = ( self % x(i) - self % x(:) )**2 + &
                        & ( self % y(i) - self % y(:) )**2 + &
                        & ( self % z(i) - self % z(:) )**2

            return
        end subroutine


        module subroutine delta_for_indexed_method (self, i, idx)
            ! Synopsis:
            ! Obtains the distance of the ith tensor v[i] relative to v[*].
            class(tensor_t), intent(inout) :: self
            integer(kind = int64), intent(in) :: i
            integer(kind = int64), intent(in) :: idx(:)


            self % v(1 : size(idx, kind=int64)) = &
                & ( self % x(i) - self % x(idx(:)) )**2 + &
                & ( self % y(i) - self % y(idx(:)) )**2 + &
                & ( self % z(i) - self % z(idx(:)) )**2


            return
        end subroutine


        module function range_method (self, i, j) result(d)
            class(tensor_t), intent(in) :: self
            integer(kind = int64), intent(in) :: i
            integer(kind = int64), intent(in) :: j
            real(kind = real64) :: d

            call distance (self, i, j, d)

            return
        end function


        module subroutine distance (v, i, j, d)
            type(tensor_t), intent(in) :: v
            integer(kind = int64), intent(in) :: i
            integer(kind = int64), intent(in) :: j
            real(kind = real64), intent(out) :: d

            associate ( x => v % x(i), y => v % y(i), z => v % z(i), &
                     &  p => v % x(j), q => v % y(j), r => v % z(j) )


                d = (p - x)**2 + (q - y)**2 + (r - z)**2
                d = dsqrt(d)


            end associate

            return
        end subroutine


        module subroutine normalize_method (self)
            class(tensor_t), intent(inout) :: self

            if ( allocated(self % stat) ) then
                call normalizer (self)
            end if

            return
        end subroutine


        module subroutine normalizer (tensor)
            type(tensor_t), intent(inout) :: tensor
            real(kind = real64) :: t(tensor % size % n)


            call moduli (tensor)
            call tensor_guard_singular (tensor)
 

            call initialize (t)
            associate(x => tensor % x, y => tensor % y, &
                    & z => tensor % z, v => tensor % v)

               call normalize (x, y, z, t, v)

            end associate


            return
        end subroutine


        module subroutine tensor_guard_singular (tensor)
            type(tensor_t), intent(in) :: tensor

            if ( minval(tensor % v) < tensor_eps_mod ) then
                error stop "math::tensor.tensor_guard_singular (): "//&
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


        module subroutine moduli (tensor)
            type(tensor_t), intent(inout) :: tensor
            real(kind = real64) :: t(tensor % size % n)


            call initialize (t)
            associate(x => tensor % x, y => tensor % y, &
                    & z => tensor % z, v => tensor % v)
               
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
                error stop "math::tensor.allocate_stat: allocation failure"
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
                error stop "math::tensor.allocate_size: allocation failure"
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
                error stop "math::tensor.deallocate_stat: "// &
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
                error stop "math::tensor.deallocate_size: "// &
                    & "deallocation failure"
            end if

            return
        end subroutine


        module subroutine finalizer (tensor)
            ! Synopsis: Frees memory allocated for tensor.
            type(tensor_t), intent(inout) :: tensor

!           write (*, '(A)', advance='no') "destroying tensor ... "


            call destructor (tensor)
        
        
!           print *, "done"

            return
        end subroutine



        module subroutine destructor (tensor)
            ! Synopsis: Destroys the components of the tensor.
            type(tensor_t), intent(inout) :: tensor

            call deallocator (tensor % x)
            call deallocator (tensor % y)
            call deallocator (tensor % z)
            call deallocator (tensor % v)
            call deallocator (tensor % size)
            call deallocator (tensor % stat)

            return
        end subroutine


end submodule


! TODO:
! [x] implement GUARD against operating on uninitialized vectors
! [x] to implement GUARD against normalizing a singular vector
! [x] check vectorizer report and fix vectorization misses (if possible)
! [ ] implement the cross product, you want to consider writing the result
!     in another object of :[vector_t]: at a location delimited by some
!     index. You may also want to consider not passing :[self]: so that
!     the procedure operates on the objects of the type. You may want
!     to check if you can write the code so that it can be vectorized.


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
!
!
! subroutine delta_for_indexed_method (self, i, idx):
! Intel Fortran Compiler vectorizes loop though I did not check if it
! only vectorizes a versioned loop assuming unit stride (see SIMD
! directive section):
!
! (software.intel.com/content/www/us/en/develop/articles/
!  explicit-vector-programming-in-fortran.html)
!
! The GNU Fortran Compiler 10 did not vectorize the loop.
