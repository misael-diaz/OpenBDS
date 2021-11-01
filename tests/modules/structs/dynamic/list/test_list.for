!
!   source: test_list.for
!   author: misael-diaz
!   date:   2021-10-30
!
!
!   Synopsis:
!   Tests the list class.
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

module list_class_tests
use, intrinsic :: iso_fortran_env, only: int32, int64
use ListClass, only: list_t
use VectorClass, only: arange_t, vector_t, pointer_t
use RandomAccessIteratorClass, only: iterator_t => iter_t
!use ListClass, only: link_t, node_t, list_t
!use ListClass, only: link_conservative_destructor, link_aggressive_destructor
use chronos, only: timer_t => chronom
implicit none
private
public :: test_list_append_method
!public :: test_list
!public :: test_node_contructors
!public :: test_link_destructor
!public :: test_aggressive_link_destructor
!public :: test_link_conservative_destructor

contains

   subroutine test_list_append_method ()
       type(list_t) :: list
       type(vector_t) :: vector
       type(iterator_t) :: iterator
       type(timer_t) :: timer
       class(*), pointer :: p => null()
       class(*), pointer, contiguous :: it(:) => null()
       integer(kind = int32), pointer, contiguous :: array(:) => null()
       integer(kind = int64) :: idx
       integer(kind = int32) :: diff
       integer(kind = int32), parameter :: value = 0
       integer(kind = int32), parameter :: numel = 65536


       list = list_t ()
       timer = timer_t ()

       vector = vector_t ( arange_t(numel) )
       it => vector % deref % it

       select type (it)
           type is ( integer (kind = int32) )
               array => it
           class default
               error stop 'test-append: unexpected type error'
       end select


       print *, new_line('n')
       write (*, '(A)', advance='no') 'appending data to list ... '

       call timer % tic ()
       do idx = 1_int64, size(array=array, dim=1, kind=int64)
           call list % append ( array(idx) )
       end do
       call timer % toc ()

       print *, 'done'
       print '(A,F8.2)', "elapsed-time (millis): ", timer % etime ()
       print *, new_line('n')


       print *, new_line('n')
       write (*, '(A)', advance='no') &
           & 'generating random-access iterator ... '

       call timer % tic ()
       iterator = list % iterator ()
       call timer % toc ()

       print *, 'done'
       print '(A,F8.2)', "elapsed-time (millis): ", timer % etime ()
       print *, new_line('n')


       print *, new_line('n')
       write (*, '(A)', advance='no') &
           & 'traversing random-access iterator ... '

       call timer % tic ()
       diff = 0
       it => iterator % deref
       select type (it)
           type is (pointer_t)
               do idx = 1_int64, size(array=it, dim=1, kind=int64)
                   associate (p => it(idx) % p)
                       select type (p)
                           type is ( integer (kind = int32) )
                               diff = diff + (array(idx) - p)
                           class default
                               error stop 'test-append: unxpctd type error'
                       end select
                   end associate
               end do
           class default
               error stop 'test-append: unexpected type error'
       end select
       call timer % toc ()

       print *, 'done'
       print '(A,F8.2)', "elapsed-time (millis): ", timer % etime ()
       print *, new_line('n')



       write (*, '(A)', advance='no') '[00] test-list-append(): '
       if (diff /= 0) then
           print *, 'FAIL'
       else
           print *, 'pass'
       end if


       return
   end subroutine


!  subroutine test_list ()
!      ! shows how to append nodes to list
!      type(list_t) :: list
!      type(node_t) :: node
!      class(*), pointer :: p => null()
!      class(*), pointer :: h_node => null()
!      type(node_t), pointer :: p_node => null()
!      integer(kind = int32), parameter :: value = 64
!      integer(kind = int32) :: mstat, diff
!      character(*), parameter :: name = 'test-list():'
!      character(*), parameter :: malloc_err = name // ' ' // &
!          & 'memory (de)allocation error'
!
!      list = list_t ()
!
!      allocate (list % tail % node % p, mold=node_t(value), stat=mstat)
!      if (mstat /= 0) error stop malloc_err
!
!      list % head % node % p => list % tail % node % p
!
!      h_node => list % tail % node % p
!      select type (h_node)
!          type is (node_t)
!              p_node => h_node
!          class default
!              error stop '1 unexpected error'
!      end select
!
!
!      allocate (p_node % item % data % p, source=value, stat=mstat)
!      if (mstat /= 0) error stop malloc_err
!
!
!      p => p_node % item % data % p
!      select type (p)
!          type is ( integer(kind = int32) )
!              print *, 'data: ', p, value
!          class default
!              error stop '1 unexpected error'
!      end select
!
!
!
!      return
!  end subroutine


!  subroutine test_node_contructors ()
!      type(node_t) :: node
!      type(node_t) :: inode
!      integer(kind = int32), parameter :: value = 0
!
!      node  = node_t ()
!      inode = node_t (value)
!
!      return
!  end subroutine
!
!
!  subroutine test_link_destructor ()
!      type(link_t), allocatable :: link
!      type(node_t) :: node
!      class(*), pointer :: p => null()                  !! general purpose
!      class(*), pointer :: h_node => null()             !! handle to node
!      class(*), pointer :: hh_node => null()            !! handle to node
!      class(*), pointer :: hhh_node => null()           !! handle to node
!      type(node_t), pointer :: p_node => null()         !! pointer to node
!      type(node_t), pointer :: pp_node => null()        !! pointer to node
!      type(node_t), pointer :: ppp_node => null()       !! pointer to node
!      integer(kind = int32), parameter :: value = 64
!      integer(kind = int32) :: mstat, diff
!      character(*), parameter :: name = 'test-link-destructor():'
!      character(*), parameter :: malloc_err = name // ' ' // &
!          & 'memory (de)allocation error'
!
!
!      allocate (link, stat=mstat)
!      if (mstat /= 0) error stop malloc_err
!
!
!      allocate (link % node % p, mold=node_t(), stat=mstat)
!      if (mstat /= 0) error stop malloc_err
!
!
!      h_node => link % node % p
!      select type (h_node)
!          type is (node_t)
!              h_node % item % data % p => null()
!              h_node % next % node % p => null()
!              p_node => h_node
!          class default
!              error stop 'unexpected error'
!      end select
!
!
!      allocate (p_node % item % data % p, source=value, stat=mstat)
!      if (mstat /= 0) error stop malloc_err
!
!
!      p => p_node % item % data % p
!      select type (p)
!          type is ( integer(kind = int32) )
!              print *, 'data: ', p
!          class default
!              error stop 'unexpected error'
!      end select
!
!
!      p_node % next % node % p => null()
!
!
!      allocate (p_node % next % node % p, mold=node_t(), stat=mstat)
!      if (mstat /= 0) error stop malloc_err
!
!
!      hh_node => p_node % next % node % p
!      select type (hh_node)
!          type is (node_t)
!              hh_node % item % data % p => null()
!              hh_node % next % node % p => null()
!              pp_node => hh_node
!          class default
!              error stop 'unexpected error'
!      end select
!
!      pp_node % next % node % p => null()
!
!      allocate (pp_node % item % data % p, source=value, stat=mstat)
!      if (mstat /= 0) error stop malloc_err
!
!
!      allocate (pp_node % next % node % p, mold=node_t(), stat=mstat)
!      if (mstat /= 0) error stop malloc_err
!
!
!      hhh_node => pp_node % next % node % p
!      select type (hhh_node)
!          type is (node_t)
!              hhh_node % item % data % p => null()
!              hhh_node % next % node % p => null()
!              ppp_node => hhh_node
!          class default
!              error stop 'unexpected error'
!      end select
!
!
!      allocate (ppp_node % item % data % p, source=value, stat=mstat)
!      if (mstat /= 0) error stop malloc_err
!
!
!      allocate (ppp_node % next % node % p, mold=node_t(), stat=mstat)
!      if (mstat /= 0) error stop malloc_err
!
!
!
!      deallocate (link, stat=mstat)
!      if (mstat /= 0) error stop malloc_err
!
!
!
!      return
!  end subroutine
!
!
!  subroutine test_aggressive_link_destructor ()
!      type(link_t) :: link
!      type(node_t) :: node
!      class(*), pointer :: p => null()                  !! general purpose
!      class(*), pointer :: h_node => null()             !! handle to node
!      class(*), pointer :: hh_node => null()            !! handle to node
!      type(node_t), pointer :: p_node => null()         !! pointer to node
!      type(node_t), pointer :: pp_node => null()        !! pointer to node
!      integer(kind = int32), parameter :: value = 64
!      integer(kind = int32) :: mstat, diff
!      character(*), parameter :: name = 'test-link-destructor():'
!      character(*), parameter :: malloc_err = name // ' ' // &
!          & 'memory (de)allocation error'
!
!
!      allocate (link % node % p, mold=node_t(), stat=mstat)
!      if (mstat /= 0) error stop malloc_err
!
!
!      h_node => link % node % p
!      select type (h_node)
!          type is (node_t)
!              h_node % item % data % p => null()
!              h_node % next % node % p => null()
!              p_node => h_node
!          class default
!              error stop 'unexpected error'
!      end select
!
!
!      allocate (p_node % item % data % p, source=value, stat=mstat)
!      if (mstat /= 0) error stop malloc_err
!
!
!      p => p_node % item % data % p
!      select type (p)
!          type is ( integer(kind = int32) )
!              print *, 'data: ', p
!          class default
!              error stop 'unexpected error'
!      end select
!
!
!      p_node % next % node % p => null()
!
!
!      allocate (p_node % next % node % p, mold=node_t(), stat=mstat)
!      if (mstat /= 0) error stop malloc_err
!
!
!      hh_node => p_node % next % node % p
!      select type (hh_node)
!          type is (node_t)
!              hh_node % item % data % p => null()
!              hh_node % next % node % p => null()
!              pp_node => hh_node
!          class default
!              error stop 'unexpected error'
!      end select
!
!
!      allocate (pp_node % item % data % p, source=value, stat=mstat)
!      if (mstat /= 0) error stop malloc_err
!
!
!      allocate (pp_node % next % node % p, mold=node_t(), stat=mstat)
!      if (mstat /= 0) error stop malloc_err
!
!
!      call link_aggressive_destructor (link)
!
!
!      return
!  end subroutine
!
!
!  subroutine test_link_conservative_destructor ()
!      type(link_t) :: link
!      type(node_t) :: node
!      class(*), pointer :: p => null()                  !! general purpose
!      class(*), pointer :: h_node => null()             !! handle to node
!      type(node_t), pointer :: p_node => null()         !! pointer to node
!      integer(kind = int32), parameter :: value = 64
!      integer(kind = int32) :: mstat, diff
!      character(*), parameter :: name = 'test-link-destructor():'
!      character(*), parameter :: malloc_err = name // ' ' // &
!          & 'memory (de)allocation error'
!
!
!      allocate (link % node % p, mold=node_t(), stat=mstat)
!      if (mstat /= 0) error stop malloc_err
!
!
!      h_node => link % node % p
!      select type (h_node)
!          type is (node_t)
!              h_node % item % data % p => null()
!              h_node % next % node % p => null()
!              p_node => h_node
!          class default
!              error stop 'unexpected error'
!      end select
!
!
!      allocate (p_node % item % data % p, source=value, stat=mstat)
!      if (mstat /= 0) error stop malloc_err
!
!
!      p => p_node % item % data % p
!      select type (p)
!          type is ( integer(kind = int32) )
!              print *, 'data: ', p
!          class default
!              error stop 'unexpected error'
!      end select
!
!
!      p_node % next % node % p => null()
!
!
!      allocate (p_node % next % node % p, mold=node_t(), stat=mstat)
!      if (mstat /= 0) error stop malloc_err
!
!      do while ( associated(link % node % p) )
!          call link_conservative_destructor (link)
!      end do
!
!
!      return
!  end subroutine

end module list_class_tests


program test
  use list_class_tests, only: append => test_list_append_method
! use list_class_tests, only: list => test_list
! use list_class_tests, only: node_constructors => &
!                           & test_node_contructors
! use list_class_tests, only: destructor => &
!                           & test_link_destructor
! use list_class_tests, only: aggressive_destructor => &
!                           & test_aggressive_link_destructor
! use list_class_tests, only: link_conservative_destructor => &
!                           & test_link_conservative_destructor
  implicit none

! call link_conservative_destructor ()
! call aggressive_destructor ()
! call destructor ()
! call node_constructors ()
! call list ()
  call append ()

end program
