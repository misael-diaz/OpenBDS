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

  module recursive subroutine list_genIterator (link, iter)
      ! generates the random-access iterator of the list recursively
      type(link_t), intent(in), target :: link
      type(iter_t), intent(inout) :: iter
      class(*), pointer :: h_node => null()
      type(node_t), pointer :: node => null()

      h_node => link % node % p
      select type (h_node)
          type is (node_t)
              node => h_node
          class default
              error stop 'list<*data_t>.iterator(): unexpected type err'
      end select

      call iter % insert (node % item % data)

      if ( associated(node % next % node % p) ) then
          call iterator (node % next, iter)
      end if

      return
  end subroutine

  module subroutine list_int32_t_append (list, value)
      type(list_t), intent(inout) :: list
      class(*), pointer :: h_node => null()
      type(node_t), pointer :: node => null()
      integer(kind = int32), intent(in) :: value

      if ( associated(list % tail % node % p) ) then

          h_node => list % tail % node % p
          select type (h_node)
              type is (node_t)
                  node => h_node
              class default
                  error stop 'list<*int32_t>.append(): unexpected type err'
          end select

          call create (node % next, value)
          list % tail % node % p => node % next % node % p

      else
          call create (list % head, value)
          list % tail % node % p => list % head % node % p
      end if

      return
  end subroutine


  module function list_default_constructor () result(list)
      type(list_t) :: list

      list % head % node % p => null()
      list % tail % node % p => null()

      return
  end function


  module subroutine node_int32_t_create (link, value)
      ! creates a node<*int32_t>
      type(link_t), intent(inout) :: link
      class(*), pointer :: p => null()          !! general purpose pointer
      class(*), pointer :: h_node => null()     !! node handle
      type(node_t), pointer :: node => null()   !! node pointer
      integer(kind = int32), intent(in) :: value
      integer(kind = int32) :: mstat

      allocate (link % node % p, mold = node_t(), stat = mstat)
      if (mstat /= 0) error stop 'node<*int32_t>: alloc error'

      h_node => link % node % p
      select type (h_node)
          type is (node_t)
              node => h_node
          class default
              error stop 'node<*int32_t>: unexpected type error'
      end select

      allocate (node % item % data % p, source=value, stat=mstat)
      if (mstat /= 0) error stop 'node<*int32_t>: alloc error'

      node % next % node % p => null()

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

      node % item % data % p => null()
      node % next % node % p => null()

      return
  end function


  module function node_int32_t_constructor (value) result(node)
      !! returns node<*int32_t>
      type(node_t) :: node
      integer(kind = int32), intent(in) :: value
      integer(kind = int32) :: mstat

      node % item % data % p => null()
      node % next % node % p => null()

      allocate (node % item % data % p, source=value, stat=mstat)
      if (mstat /= 0) error stop 'node<*int32_t>: alloc error'

      return
  end function


  module subroutine list_finalizer (list)
      type(list_t), intent(inout) :: list

      if ( associated(list % head % node % p) ) then
          call link_destructor (list % head)
          list % head % node % p => null()
          list % tail % node % p => null()
      end if

      return
  end subroutine


  module recursive subroutine node_finalizer (node)
      type(node_t), intent(inout) :: node
      integer(kind = int32) :: mstat

      if ( associated(node % next % node % p) ) then

          call link_destructor (node % next)
          node % next % node % p => null()

      end if


      if ( associated(node % item % data % p) ) then

          deallocate (node % item % data % p, stat=mstat)
          if (mstat /= 0) error stop 'node.final: dealloc error'

          node % item % data % p => null()

      end if

      return
  end subroutine node_finalizer


  module recursive subroutine link_finalizer (link)
      type(link_t), intent(inout) :: link

      if ( associated(link % node % p) ) then
          call link_destructor (link)
          link % node % p => null()
      end if

      return
  end subroutine


  module recursive subroutine link_destructor (link)
      ! destroys links to nodes recursively from back to front
      type(link_t), intent(inout) :: link
      class(*), pointer :: h_node => null()
      class(*), pointer :: h_next => null()
      type(node_t), pointer :: node => null()
      integer(kind = int32) :: mstat


      h_node => link % node % p
      select type (h_node)
          type is (node_t)
              node => h_node
          class default
              error stop '1 destructor unexpected error'
      end select

      h_next => node % next % node % p
      if ( associated(h_next) ) then

          call link_destructor (node % next)

          h_node => link % node % p
          select type (h_node)
              type is (node_t)
                  node => h_node        !! validates pointer to current
              class default
                  error stop '3 destructor unexpected error'
          end select

          deallocate (node, stat=mstat)
          if (mstat /= 0) error stop 'deallocation error'

          node => null()
          h_node => null()
          link % node % p => null()


      else

          deallocate (node, stat=mstat)
          if (mstat /= 0) error stop 'deallocation error'

          node => null()
          link % node % p => null()

      end if

      return
  end subroutine link_destructor


  module recursive subroutine link_aggressive_destructor (link)
      ! destroys links to nodes recursively from back to front
      type(link_t), intent(inout) :: link
      class(*), pointer :: h_node => null()
      class(*), pointer :: h_next => null()
      type(node_t), pointer :: node => null()
      integer(kind = int32) :: mstat

      h_node => link % node % p
      select type (h_node)
          type is (node_t)
              node => h_node
          class default
              error stop '1 destructor unexpected error'
      end select

      h_next => node % next % node % p
      if ( associated(h_next) ) then


          call link_aggressive_destructor (node % next)


          h_node => link % node % p
          select type (h_node)
              type is (node_t)
                  node => h_node        !! validates pointer to current
              class default
                  error stop '3 destructor unexpected error'
          end select


          if ( associated(node % item % data % p) ) then
              deallocate (node % item % data % p, stat=mstat)
              if (mstat /= 0) error stop 'deallocation error'
          end if


          node % item % data % p => null()
          node % next % node % p => null()


          deallocate (node, stat=mstat)
          if (mstat /= 0) error stop 'deallocation error'


          node => null()
          h_node => null()
          link % node % p => null()


      else

          if ( associated(node % item % data % p) ) then
              deallocate (node % item % data % p, stat=mstat)
              if (mstat /= 0) error stop 'deallocation error'
          end if

          node % item % data % p => null()
          node % next % node % p => null()

          deallocate (node, stat=mstat)
          if (mstat /= 0) error stop 'deallocation error'

          node => null()
          link % node % p => null()

      end if

      return
  end subroutine link_aggressive_destructor


  module recursive subroutine link_conservative_destructor (link)
      ! destroys data in the last node only
      type(link_t), intent(inout) :: link
      class(*), pointer :: h_node => null()
      class(*), pointer :: h_next => null()
      type(node_t), pointer :: node => null()
      integer(kind = int32) :: mstat

      h_node => link % node % p
      select type (h_node)
          type is (node_t)
              node => h_node
          class default
              error stop '1 destructor unexpected error'
      end select

      h_next => node % next % node % p
      if ( associated(h_next) ) then

          call link_conservative_destructor (node % next)

      else

          if ( associated(node % item % data % p) ) then
              deallocate (node % item % data % p, stat=mstat)
              if (mstat /= 0) error stop 'deallocation error'
          end if

          node % item % data % p => null()
          node % next % node % p => null()

          deallocate (node, stat=mstat)
          if (mstat /= 0) error stop 'deallocation error'

          node => null()
          link % node % p => null()

      end if

      return
  end subroutine link_conservative_destructor


  module recursive subroutine data_destructor (data)
      type(data_t), intent(inout) :: data
      integer(kind = int32) :: mstat

      if ( associated(data % data % p) ) then

          deallocate (data % data % p, stat=mstat)
          if (mstat /= 0) error stop 'data.destructor: dealloc error'

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
