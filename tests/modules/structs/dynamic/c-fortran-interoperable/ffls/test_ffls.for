!
!   source: test_ffls.for
!   author: misael-diaz
!   date:   2021-11-19
!
!
!   Synopsis:
!   Tests the FORTRAN Forward Linked-list class.
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

module test_forward_linked_list
! use, intrinsic :: iso_c_binding, only: c_int32_t
  use, intrinsic :: iso_fortran_env, only: int32
  use chronos, only: timer_t => chronom
  use FFLinkedListClass, only: list_t => ffls_t
  implicit none
  private
  public :: test_fforward_linked_list


contains

  subroutine test_fforward_linked_list ()
      ! tests appending values to the FORTRAN Forward Linked-list class
      type(list_t), allocatable :: list
      type(timer_t) :: timer
      integer(kind = int32), parameter :: numel = 65536
      integer(kind = int32) :: mstat
      integer(kind = int32) :: i


      allocate (list, stat=mstat)
      if (mstat /= 0) error stop 'allocation error'


      list = list_t ()
      timer = timer_t ()


      write (*, '(1X,A)', advance='no') 'appending to list ... '


      call timer % tic ()
      do i = 1, numel
          call list % append (i)
      end do
      call timer % toc ()


      print *, 'done'
      print *, 'elapsed time (millis): ', timer % etime ()


      write (*, '(1X,A)', advance='no') 'destroying list ... '


      call timer % tic ()
      deallocate (list)
      call timer % toc ()


      print *, 'done'
      print *, 'elapsed time (millis): ', timer % etime ()


      return
  end subroutine test_fforward_linked_list

end module

program test_flist
  use test_forward_linked_list, only: ffls => test_fforward_linked_list
  implicit none

  call ffls ()

end program
