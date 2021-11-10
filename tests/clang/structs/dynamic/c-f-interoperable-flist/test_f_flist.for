!
!   source: test_f_flist.for
!   author: misael-diaz
!   date:   2021-10-25
!
!
!   Synopsis:
!   Implements a forward linked-list class in FORTRAN whose methods
!   delegate their work to lower-level routines written in C.
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

module ListClass
  use, intrinsic :: iso_c_binding, only: c_int
  use, intrinsic :: iso_c_binding, only: c_ptr, c_funptr
  use, intrinsic :: iso_c_binding, only: c_f_pointer, c_f_procpointer
  use, intrinsic :: iso_fortran_env, only: int32, int64, real64
  implicit none
  private
  public :: test_f_flist
  public :: test_create_node
  public :: test_append_method


  type, bind(c) :: data_t       !! generic data<..> type: void pointer
    type(c_ptr) :: data
  end type


  type, bind(c) :: link_t       !! link<*node_t>: pointer to node object
    type(c_ptr) :: node
  end type


  type, bind(c) :: node_t       !! node<*data_t>: pointer to data object
    type(c_ptr) :: item
    type(c_ptr) :: next
  end type


  type, bind(c) :: list_t       !! list<*link_t>: C linked-list
    type(c_ptr) :: self
    type(c_ptr) :: head
    type(c_ptr) :: tail
    type(c_funptr) :: append
  end type


  type, public :: f_flist_t     !! FORTRAN forward linked-list class
    private
    type(c_ptr) :: flist
    contains
      private
      procedure, public :: append => f_flist_append_method
      final :: f_flist_finalizer
  end type


  interface f_flist_t
    module procedure :: f_flist_default_constructor
  end interface


  abstract interface
    subroutine i_append (self, value)
        use, intrinsic :: iso_c_binding, only: c_ptr, c_int
        implicit none
        type(c_ptr), intent(in), value :: self
        integer(kind = c_int), intent(in), value :: value
    end subroutine
  end interface


  interface

    function flist_create_list_t () result(list) bind(c)
        use, intrinsic :: iso_c_binding, only: c_ptr
        implicit none
        type(c_ptr) :: list
    end function


    function flist_list_destructor (list) result(ret) bind(c)
        use, intrinsic :: iso_c_binding, only: c_ptr
        implicit none
        type(c_ptr) :: ret
        type(c_ptr), intent(in), value :: list
    end function

  end interface


  interface

    function flist_create_node_int32_t (value) result(node) bind(c)
        use, intrinsic :: iso_c_binding, only: c_ptr, c_int
        implicit none
        type(c_ptr) :: node
        integer(kind = c_int), intent(in), value :: value
    end function


    function util_ffree_node_t (node) result(ret) bind(c)
        use, intrinsic :: iso_c_binding, only: c_ptr
        implicit none
        type(c_ptr) :: ret
        type(c_ptr), intent(in), value :: node
    end function

  end interface


  interface
    function util_is_next_associated_node_t (node) result(stat) bind(c)
        use, intrinsic :: iso_c_binding, only: c_ptr, c_int
        implicit none
        type(c_ptr), intent(in), value :: node
        integer(kind = c_int) :: stat
    end function
  end interface


contains

  subroutine f_flist_append_method (self, value)
      ! FORTRAN forward linked-list append method
      class(f_flist_t), intent(inout) :: self
      type(list_t), pointer :: list => null()
      procedure(i_append), pointer :: append => null()
      integer(kind = c_int), intent(in) :: value

      call c_f_pointer (self % flist, list)
      call c_f_procpointer (list % append, append)

      call append (list % self, value)

      return
  end subroutine


  function f_flist_default_constructor () result(list)
      ! FORTRAN forward linked-list default constructor
      type(f_flist_t) :: list

      list % flist = flist_create_list_t ()

      return
  end function


  subroutine f_flist_finalizer (list)
      ! FORTRAN forward linked-list finalizer
      type(f_flist_t), intent(inout) :: list

      list % flist = flist_list_destructor (list % flist)

      return
  end subroutine


  subroutine test_f_flist ()
      ! tests appending values to the FORTRAN forward linked-list class
      type(f_flist_t) :: list
      integer(kind = int32), parameter :: numel = 256
      integer(kind = int32) :: i

      list = f_flist_t ()

      do i = 1, numel
          call list % append (i)
      end do

      return
  end subroutine


  subroutine test_append_method ()
      ! tests appending values to a C linked-list object
      type(c_ptr) :: list
      type(list_t), pointer :: p_list => null()
      type(link_t), pointer :: p_head => null()
      type(node_t), pointer :: p_node => null()
      type(node_t), pointer :: p_next => null()
      type(data_t), pointer :: p_item => null()
      integer(kind = c_int), pointer :: p_data => null()
      procedure(i_append), pointer :: append => null()
      integer(kind = int64) :: t_start, t_end, clock_rate
!     integer(kind = c_int), parameter :: numel = 65536
      integer(kind = c_int), parameter :: numel = 256
      integer(kind = c_int) :: i, diff


      call system_clock (count_rate=clock_rate)
      call system_clock (t_start)

      write (*, '(1X,A)', advance='no') 'creating list ... '
      list = flist_create_list_t ()
      print *, 'done'


      call c_f_pointer (list, p_list)
      call c_f_procpointer (p_list % append, append)

      do i = 1, numel
          call append (p_list % self, i)        !! appends values to list
      end do


      i = 1
      diff = 0
      call c_f_pointer (p_list % head, p_head)
      call c_f_pointer (p_head % node, p_node)
      call c_f_pointer (p_node % next, p_next)

      do while ( associated(p_node) )
          ! computes differences between input and contained data

          call c_f_pointer (p_node % item, p_item)
          call c_f_pointer (p_item % data, p_data)
          diff = i - p_data
          i = i + 1

          ! updates forward iterators
          call c_f_pointer (p_node % next, p_node)
          if ( associated(p_next) ) then
              call c_f_pointer (p_next % next, p_next)
          end if
      end do


      write (*, '(1X,A)', advance='no') 'destroying list ... '
      list = flist_list_destructor (list)
      print *, 'done'

      call system_clock (t_end)
      print *, 'elapsed time (millis): ', 1.0e3_real64 * &
          & real(t_end - t_start, kind=real64) /         &
          & real(clock_rate, kind=real64)


      write (*, '(1X,A)', advance='no') '[00] test-append-method: '
      if (diff /= 0) then
          print *, 'FAIL'
      else
          print *, 'pass'
      end if

      return
  end subroutine


  subroutine test_create_node ()
      ! Synopsis:
      ! Tests creating a node, getting at its value, checking if it is
      ! linked to another node (association status), and destroying it.

      type(c_ptr) :: ret, node
      type(node_t), pointer :: p_node => null()
      type(data_t), pointer :: p_item => null()
      integer(kind = c_int), pointer :: p_data => null()


      write (*, '(1X,A)', advance='no') 'creating node ... '

      node = flist_create_node_int32_t (64)

      print *, 'done'


      call c_f_pointer (node, p_node)
      call c_f_pointer (p_node % item, p_item)
      call c_f_pointer (p_item % data, p_data)
      print *, 'value: ', p_data
      print *, 'assoc:', util_is_next_associated_node_t (node)


      write (*, '(1X,A)', advance='no') 'destroying node ... '

      ret  = util_ffree_node_t (node)

      print *, 'done'

      return
  end subroutine test_create_node

end module

program test_flist
  use ListClass, only: test_f_flist
  use ListClass, only: test_create_node
  use ListClass, only: test_append_method
  implicit none

  call test_create_node ()
  call test_append_method ()
  call test_f_flist ()

end program
