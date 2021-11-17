!
!   source: List_implements.for
!   author: misael-diaz
!   date:   2021-10-29
!
!
!   Synopsis:
!   Implements methods of the linked-list class.
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

submodule (ListClass) list_implements
implicit none
contains

  module subroutine list_genIterator (link, iter)
      ! generates the random-access iterator
      type(link_t), intent(in), target :: link
      type(iter_t), intent(inout) :: iter
      type(node_t), pointer :: node => null()
      type(node_t), pointer :: next => null()

      call associate (node, link)
      call iter % insert (node % item % data)
      call associate (next, node % next)

      do while ( associated(next) )

          call associate (node, node % next)
          call iter % insert (node % item % data)
          call associate (next, next % next)

      end do

      return
  end subroutine


  module recursive subroutine list_recursive_genIterator (link, iter)
      ! generates the random-access iterator of the list recursively
      type(link_t), intent(in), target :: link
      type(iter_t), intent(inout) :: iter
      type(node_t), pointer :: node => null()

      call associate (node, link)

      call iter % insert (node % item % data)

      if ( associated(node % next % node % p) ) then
          call list_recursive_genIterator (node % next, iter)
      end if

      return
  end subroutine


  module subroutine list_int32_t_append (list, value)
      type(list_t), intent(inout) :: list
      type(node_t), pointer :: node => null()
      integer(kind = int32), intent(in) :: value

      if ( associated(list % tail) ) then

          call associate (node, list % tail)
          call create (node % next, value)
          list % tail => node % next

      else
          call create (list % head, value)
          list % tail => list % head
      end if

      return
  end subroutine


  module function list_default_constructor () result(list)
      type(list_t) :: list

      list % head => null()
      list % tail => null()

      return
  end function


  module subroutine node_int32_t_create (link, value)
      ! creates a node<*int32_t>
      type(link_t), intent(inout), pointer :: link
      class(*), pointer :: p => null()          !! general purpose pointer
      type(node_t), pointer :: node => null()   !! node pointer
      integer(kind = int32), intent(in) :: value
      integer(kind = int32) :: mstat

      allocate (link, stat = mstat)
      if (mstat /= 0) error stop 'node<*int32_t>: alloc error'

      allocate (link % node % p, mold = node_t(), stat = mstat)
      if (mstat /= 0) error stop 'node<*int32_t>: alloc error'

      call associate (node, link)

      allocate (node % item, stat=mstat)
      if (mstat /= 0) error stop 'node<*int32_t>: alloc error'

      allocate (node % item % data % p, source=value, stat=mstat)
      if (mstat /= 0) error stop 'node<*int32_t>: alloc error'

      node % next => null()

      return
  end subroutine node_int32_t_create


  module subroutine node_assign_method (self, node)
      ! defines the assignment method for the node class
      class(node_t), intent(inout) :: self
      type(node_t), intent(in) :: node
      integer(kind = int32) :: mstat
      integer(kind = int32), parameter :: value = 0
      character(*), parameter :: name = 'node.assignment()::'
      character(*), parameter :: impl_err = name // &
          & 'implementation-error: missing deep copy'


      if ( loc(self) /= loc(node) ) then  !! caters self-assignment


          ! complains that ``deep copy'' has not been implemented
          if ( associated(node % next % node % p) ) then
              error stop impl_err
          end if


          if ( associated(node % item % data % p) ) then

              ! destroys existing `data' in destination
              if ( associated(self % next % node % p) ) then
                  call link_destructor (self % next)
                  self % next % node % p => null()
              end if

              if ( associated(self % item % data % p) ) then
                  deallocate (self % item % data % p, stat=mstat)
                  if (mstat /= 0) error stop 'node.assign: dealloc error'
                  self % item % data % p => null()
              end if

              ! associates to `data' in source node
              self % item % data % p => node % item % data % p
              self % next % node % p => null()

          else

              ! destroys existing `data' in destination node
              if ( associated(self % next % node % p) ) then

                  call link_destructor (self % next)
                  self % next % node % p => null()

              end if

              if ( associated(self % item % data % p) ) then

                  deallocate (self % item % data % p, stat=mstat)
                  if (mstat /= 0) error stop 'node.assign: dealloc error'
                  self % item % data % p => null()

              end if

          end if


      end if


      return
  end subroutine


  module function node_default_constructor () result(node)!! returns node<>
      type(node_t) :: node

      node % item => null()
      node % next => null()

      return
  end function


  module function node_int32_t_constructor (value) result(node)
      !! returns node<*int32_t>
      type(node_t) :: node
      integer(kind = int32), intent(in) :: value
      integer(kind = int32) :: mstat

      allocate (node % item, stat=mstat)
      if (mstat /= 0) error stop 'node<*int32_t>: alloc error'

      allocate (node % item % data % p, source=value, stat=mstat)
      if (mstat /= 0) error stop 'node<*int32_t>: alloc error'

      node % next => null()

      return
  end function


  module subroutine list_finalizer (list)
      type(list_t), intent(inout) :: list
      integer(kind = int32) :: mstat

      if ( associated(list % head) ) then
          call link_destructor (list % head)
          list % head => null()
          list % tail => null()
      end if

      return
  end subroutine


  module recursive subroutine node_finalizer (node)
      type(node_t), intent(inout) :: node
      integer(kind = int32) :: mstat

      if ( associated(node % item) ) then

          deallocate (node % item % data % p, stat=mstat)
          if (mstat /= 0) error stop 'node.final: dealloc error'

          node % item % data % p => null()
          deallocate (node % item, stat=mstat)
          if (mstat /= 0) error stop 'node.final: dealloc error'

          node % item => null()
          node % next => null()

      end if

      return
  end subroutine node_finalizer


  module recursive subroutine link_finalizer (link)
      type(link_t), intent(inout) :: link

      link % node % p => null()

      return
  end subroutine


  module subroutine link_destructor (head)
      type(link_t), intent(inout), pointer :: head
      type(link_t), pointer :: link => null()
      type(node_t), pointer :: node => null()
      type(node_t), pointer :: next => null()
      integer(kind = int32) :: mstat


      if ( associated(head) ) then
          call associate (node, head)
      else
          node => null()
      end if


      if ( associated(node) ) then
          call associate (next, node % next)
      else
          next => null()
      end if


      do while ( associated(node) )


          if ( associated(node) ) then
              if ( associated(node % next) ) then
                  link => node % next
              else
                  link => null()
              end if

              deallocate (node, stat=mstat)
              if (mstat /= 0) error stop 'alloc err'
          else
              link => null()
          end if


          node => next
          if ( associated(node) ) then
              call associate (next, node % next)
          else
              next => null()
          end if


          if ( associated(link) ) then
              link % node % p => null()
              deallocate (link, stat=mstat)
              if (mstat /= 0) error stop 'alloc err'
              link => null()
          end if

      end do


      if ( associated(head) ) then
          head % node % p => null()
          deallocate(head, stat=mstat)
          head => null()
      end if


      return
  end subroutine link_destructor


  module recursive subroutine data_destructor (data)
      type(data_t), intent(inout) :: data
      integer(kind = int32) :: mstat

      if ( associated(data % data % p) ) then

          deallocate (data % data % p, stat=mstat)
          if (mstat /= 0) error stop 'data.destructor: dealloc error'
          data % data % p => null()

      end if

      return
  end subroutine

end submodule


! References:
! SJ Chapman, FORTRAN for Scientists and Engineers, fourth edition
! A Koenig and B Moo, Accelerated C++ Practical Programming by Example


! COMMENTS
!
! node_assign_method:
! Does not yet support deep copy, which would involve creating copies
! of all the linked nodes. Complains if the link of the source node is
! associated. At the moment the assignment method is being used to support
! the node constructors.
!
!
! WARNINGS
! Recursive methods are fast but might overflow the stack for long lists.
! For this reason destructors that use a recursive scheme have been
! replaced by memory inexpensive methods, though these tend to be slower
! because one has to traverse the list several times on account of the
! forward iterators that these use. (A doubly-linked list should perform
! better.) I prefer to use a slower but reliable linked-list than a fast
! list prone to stack overflows.
