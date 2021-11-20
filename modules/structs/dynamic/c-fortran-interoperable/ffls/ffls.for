!
!   source: ffls.for
!   author: misael-diaz
!   date:   2021-11-19
!
!
!   Synopsis:
!   Implements a FORTRAN Forward List class. It is in essence an
!   Application Programming Interface API to a forward linked-list
!   written in C.
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

module FFLinkedListClass
  use, intrinsic :: iso_c_binding, only: c_size_t
  use, intrinsic :: iso_c_binding, only: c_int32_t, c_int64_t
  use, intrinsic :: iso_c_binding, only: c_ptr, c_funptr
  use, intrinsic :: iso_c_binding, only: c_f_pointer, c_f_procpointer
  implicit none
  private


  type, bind(c) :: list_t
    type(c_ptr) :: head
    type(c_ptr) :: tail
    type(c_ptr) :: errmsg
    integer(kind = c_size_t) :: id
  end type


  type, public :: ffls_t     !! FORTRAN Forward List Type
    private
    type(c_ptr) :: list
    contains
      private
      procedure :: ffls_append_int32_t_method
      procedure :: ffls_append_int64_t_method
      generic, public :: append => ffls_append_int32_t_method, &
                                 & ffls_append_int64_t_method
      final :: ffls_finalizer
  end type


  interface ffls_t
    module procedure :: ffls_default_constructor
    module procedure :: ffls_int32_t_constructor
    module procedure :: ffls_int64_t_constructor
  end interface


  interface

    function flist_create_list_t () result(list) bind(c)
        use, intrinsic :: iso_c_binding, only: c_ptr
        implicit none
        type(c_ptr) :: list
    end function


    function flist_create_list_int32_t (value) result(list) bind(c)
        use, intrinsic :: iso_c_binding, only: c_ptr, c_int32_t
        implicit none
        type(c_ptr) :: list
        integer(kind = c_int32_t), intent(in) :: value
    end function


    function flist_create_list_int64_t (value) result(list) bind(c)
        use, intrinsic :: iso_c_binding, only: c_ptr, c_int64_t
        implicit none
        type(c_ptr) :: list
        integer(kind = c_int64_t), intent(in) :: value
    end function


    function flist_list_destructor (list) result(ret) bind(c)
        use, intrinsic :: iso_c_binding, only: c_ptr
        implicit none
        type(c_ptr) :: ret
        type(c_ptr), intent(in), value :: list
    end function

  end interface


  interface

    subroutine flist_append_int32_t_method (list, value) bind(c)
        use, intrinsic :: iso_c_binding, only: c_ptr, c_int32_t
        implicit none
        type(c_ptr), value :: list
        integer(kind = c_int32_t), intent(in) :: value
    end subroutine


    subroutine flist_append_int64_t_method (list, value) bind(c)
        use, intrinsic :: iso_c_binding, only: c_ptr, c_int64_t
        implicit none
        type(c_ptr), value :: list
        integer(kind = c_int64_t), intent(in) :: value
    end subroutine

  end interface


  interface

    module function ffls_default_constructor () result(ffls)
        type(ffls_t) :: ffls
    end function


    module function ffls_int32_t_constructor (value) result(ffls)
        type(ffls_t) :: ffls
        integer(kind = c_int32_t), intent(in) :: value
    end function


    module function ffls_int64_t_constructor (value) result(ffls)
        type(ffls_t) :: ffls
        integer(kind = c_int64_t), intent(in) :: value
    end function


    module subroutine ffls_finalizer (ffls)
        type(ffls_t), intent(inout) :: ffls
    end subroutine

  end interface


  interface

    module subroutine ffls_append_int32_t_method (self, value)
        class(ffls_t), intent(inout) :: self
        integer(kind = c_int32_t), intent(in) :: value
    end subroutine


    module subroutine ffls_append_int64_t_method (self, value)
        class(ffls_t), intent(inout) :: self
        integer(kind = c_int64_t), intent(in) :: value
    end subroutine

  end interface

end module FFLinkedListClass
