!
!
!
!
! / ===================================================================== \
!   NOTICE:
!   This Work is derived from the TestU01 Package. Translates portions of
!   the following source files to FORTRAN: unif01.c and ulcg.c. Names of
!   variables and functions have been preserved to some extent.
! \ ===================================================================== /
!
!
!
!
!  Copyright (c) 2002 Pierre L'Ecuyer, DIRO, Université de Montréal.
!  e-mail: lecuyer@iro.umontreal.ca
!  All rights reserved.
!
!  Redistribution and use in source and binary forms, with or without
!  modification, are permitted without a fee for private, research,
!  academic, or other non-commercial purposes.
!  Any use of this software in a commercial environment requires a
!  written licence from the copyright owner.
!
!  Any changes made to this package must be clearly identified as such.
!
!  In scientific publications which used this software, a reference to it
!  would be appreciated.
!
!  Redistributions of source code must retain this copyright notice
!  and the following disclaimer.
!
!  THIS PACKAGE IS PROVIDED "AS IS" AND WITHOUT ANY EXPRESS OR
!  IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
!  WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
!
!
!
!  Derived Work
!
!  source: ulcg.for
!  author: misael-diaz
!  date:   2021/06/02
!
!
!  Synopsis:
!  Defines linear congruential generators of pseudo-random numbers.
!
!
!  Copyright (c) 2021 Misael Diaz-Maldonado
!  All rights reserved.
!
!  Licensed under the Apache License, Version 2.0 (the "License");
!  you may not use this file except in compliance with the License.
!  You may obtain a copy of the License at
!
!    http://www.apache.org/licenses/LICENSE-2.0
!
!  Unless required by applicable law or agreed to in writing, software
!  distributed under the License is distributed on an "AS IS" BASIS,
!  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
!  See the License for the specific language governing permissions and
!  limitations under the License.
!
!


module limits   ! borrows definitions from limits.h
    use, intrinsic :: iso_fortran_env, only: int64
    implicit none
    save

    integer(kind = int64), parameter:: long_max = 9223372036854775807_int64
end module


module unif01
    use, intrinsic :: iso_fortran_env, only: int64, real64
    implicit none
    save


    real(kind = real64), parameter :: unif01_norm32 = 4294967296.0_real64
    real(kind = real64), parameter :: unif01_inv32  = &
        & 2.328306436538696e-10_real64


    type, abstract, public :: unif01_gen
        character(len=64):: name
        contains
            procedure(i_generator), public, deferred :: getu01
    end type


    abstract interface
        function i_generator(self) result(r)
            ! Synopsis:
            ! Returns a uniformly distributed pseudo-random number.
            use, intrinsic :: iso_fortran_env, only: real64
            import unif01_gen
            implicit none
            class(unif01_gen):: self
            real(kind = real64):: r
        end function
    end interface


    private
    public :: unif01_norm32
    public :: unif01_inv32
end module unif01


module ulcg
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64
    use limits, only: long_max
    use unif01, only: unif01_gen
    implicit none


    type :: lcg_param
        integer(kind = int64):: m               ! modulus
        integer(kind = int64):: a               ! multiplier
        integer(kind = int64):: c               ! increment
        integer(kind = int64):: q               ! quotient
        integer(kind = int64):: r               ! reminder
        real(kind = real64):: norm
    end type


    type :: lcg_state
        integer(kind = int64):: s               ! state
    end type


    type, extends(unif01_gen), public :: ulcg_gen
        private
        type(lcg_param):: param
        type(lcg_state):: state
        procedure(i_lcg), pointer, pass :: fp_lcg
        contains
            procedure :: getu01 => lcg
    end type


    interface
        function i_lcg(self) result(r)
            use, intrinsic :: iso_fortran_env, only: real64
            import ulcg_gen
            implicit none
            class(ulcg_gen), target :: self
            real(kind = real64):: r
        end function
    end interface


    interface ulcg_gen
        module procedure constructor
        module procedure default_constructor
    end interface


    private
    contains


        function default_constructor () result(gen)
            ! Synopsis: Defines the default constructor.
            type(ulcg_gen):: gen
            call ulcg_create_gen_default (gen)
            return
        end function


        function constructor(m, a, c, s) result(gen)
            ! Synopsis: Creates generator from values.
            type(ulcg_gen):: gen
            integer(kind = int64), intent(in) :: m
            integer(kind = int64), intent(in) :: a
            integer(kind = int64), intent(in) :: c
            integer(kind = int64), intent(in) :: s
            call ulcg_create_gen_from_values (gen, m, a, c, s)
            return
        end function


        subroutine ulcg_create_gen_default (gen)
            ! Synopsis:
            ! Creates the default Linear Congruential Generator.

            type(ulcg_gen), intent(inout) :: gen

            integer(kind = int64), parameter :: m = 2147483647_int64
            integer(kind = int64), parameter :: a = 12001_int64
            integer(kind = int64), parameter :: c = 0_int64
            integer(kind = int64), parameter :: s = 12345_int64

            call ulcg_create_gen_from_values (gen, m, a, c, s)

            return
        end subroutine


        subroutine ulcg_create_gen_from_values (gen, m, a, c, s)
            ! Synopsis:
            ! Creates a Linear Congruential Generator from values.

            type(ulcg_gen), intent(inout), target :: gen
            integer(kind = int64), intent(in) :: m
            integer(kind = int64), intent(in) :: a
            integer(kind = int64), intent(in) :: c
            integer(kind = int64), intent(in) :: s

            type(lcg_param), pointer :: p_param => null()
            type(lcg_state), pointer :: p_state => null()


            p_param => gen % param
            p_state => gen % state


            p_param % m = m
            p_param % a = a
            p_param % c = c
            p_param % q = m / a
            p_param % r = mod(m, a)
            p_param % norm = 1.0_real64 / real(m, kind = real64)

            p_state % s = s


            call ulcg_select_gen (gen)


            return
        end subroutine ulcg_create_gen_from_values


        subroutine ulcg_select_gen (gen)
            ! Synopsis:
            ! Selects a generator depending on the parameter values.

            type(ulcg_gen), intent(inout), target :: gen
            type(lcg_param), pointer :: p_param

            integer(kind = int64):: lim
            logical(kind = int32):: is_not_small_lcg


            p_param => gen % param


            lim = (long_max - p_param % c) / p_param % a
            is_not_small_lcg = (p_param % m - 1_int64) > lim


            if (is_not_small_lcg) then
                print *, "medium and large LCGs have yet to be implemented"
                stop
            else
                call ulcg_create_gen_small (gen)
            end if


            return
        end subroutine ulcg_select_gen


        subroutine ulcg_create_gen_small (gen)
            type(ulcg_gen), intent(inout) :: gen
            gen % name   = "smallLCG_U01"
            gen % fp_lcg => small_lcg_u01
            return
        end subroutine


        function lcg (self) result(r)
            ! Synopsis:
            ! Invokes the dynamically bound Linear Congruential Generator,
            ! LCG, to return a uniformly distributed pseudo-random number.

            class(ulcg_gen):: self
            real(kind = real64):: r

            r = self % fp_lcg()

            return
        end function


        function small_lcg_u01 (gen) result(r)
            ! Synopsis:
            ! Implements a small Linear Congruential Generator of
            ! uniformly distributed pseudo-random numbers.

            class(ulcg_gen), target :: gen
            real(kind = real64):: r

            type(lcg_param), pointer :: p_param
            type(lcg_state), pointer :: p_state


            p_param => gen % param
            p_state => gen % state


            p_state % s = mod(p_param % a * p_state % s + p_param % c, &
                            & p_param % m)


            r = real(p_state % s, kind = real64) * p_param % norm

            return
        end function small_lcg_u01
end module ulcg


! Comments:
! for the time being ulcg_create uses default values so that a small lcg
! is created. In the near future either a small or medium lcg is created.



! TODO:
! Perform a runtime check. compiles but who knows if it will not generate
! garbage.
