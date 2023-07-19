!
!   source: Vector_int32_t_erase_methods.for
!   author: misael-diaz
!   date:   2021-10-09
!
!
!   Synopsis:
!   Implements the erase method of the vector class.
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

submodule (VectorClass) vector_int32_t_erase_methods
implicit none
contains

  module subroutine vector_int32_t_erase_method(vec, i, b, s, v, m, f)
      ! Synopsis:
      ! Erases values either by index, range, subscript, or value(s).
      ! mode: [in|ex]clusive
      ! flip: inverts logic, erases all but those specified.
      class(vector_t), intent(inout) :: vec
      integer(kind = int64), intent(in), optional :: i       ! index
      integer(kind = int64), intent(in), optional :: b(2)    ! bounds
      integer(kind = int64), intent(in), optional :: s(:)    ! isubs
      integer(kind = int32), intent(in), optional :: v(:)    ! values
      logical(kind = int32), intent(in), optional :: f       ! flip
      character(len=9), intent(in),      optional :: m       ! mode

      call vector_int32_t_erase_argsCheck (i, b, s, v, m)

      not_empty: if ( vec % size () /= 0_int64 ) then
          call vector_int32_t_erase (vec, i, b, s, v, m, f)
      end if not_empty

      return
  end subroutine


  module subroutine vector_int32_t_erase (vec, i, b, s, v, m, f)
      ! implements the erase method
      type(vector_t), intent(inout) :: vec
      integer(kind = int64), intent(in), optional :: i       ! index
      integer(kind = int64), intent(in), optional :: b(2)    ! bounds
      integer(kind = int64), intent(in), optional :: s(:)    ! isubs
      integer(kind = int32), intent(in), optional :: v(:)    ! values
      logical(kind = int32), intent(in), optional :: f       ! flip
      character(len=9), intent(in),      optional :: m       ! mode

      if ( present(i) ) then
          print *, "erasing by index ... "
          call vector_int32_t_erase_byIndexShadow (vec, i, f)
      else if ( present(b) ) then
          print *, "erasing by range ... "
          call vector_int32_t_erase_byRangeShadow (vec, b, f)
      else if ( present(s) ) then
          print *, "erasing by subscript ... "
          call vector_int32_t_erase_byVecSubShadow (vec, s, f)
      else if ( present(v) ) then
          print *, "erasing by values ... "
          call vector_int32_t_erase_byValueShadow (vec, v, f)
      else
          print *, "erasing all ... "
          call vector_int32_t_erase_all (vec)
      end if

      return
  end subroutine


  module subroutine vector_int32_t_erase_all (vector)
      type(vector_t), intent(inout) :: vector
      integer(kind = int32):: value

      call deallocator (vector % array % values, value)

      vector % begin % idx  = 0_int64
      vector % avail % idx  = 0_int64
      vector % limit % idx  = 0_int64
      vector % deref % idx  = 0_int64
      vector % deref % it   => null()
      vector % state % init = .false.

      return
  end subroutine


  module subroutine vector_int32_t_erase_byIndexShadow (vec, idx, f)
      ! delegates the task to (specialized) subroutines
      type(vector_t), intent(inout) :: vec
      integer(kind = int64), intent(in) :: idx
      logical(kind = int32), intent(in), optional :: f

      call check_bounds (vec, idx)
      call vector_int32_t_erase_by_index (vec, idx)

      return
  end subroutine


  module subroutine vector_int32_t_erase_by_index (vector, idx)
      type(vector_t), intent(inout) :: vector
      integer(kind = int64), intent(in) :: idx


      associate (avail => vector % avail % idx)
          if (idx == avail - 1_int64) then
              call vector_int32_t_erase_final_value (vector)
          else
              call vector_int32_t_erase_intermediate (vector, idx)
          end if
      end associate


      return
  end subroutine


  module subroutine vector_int32_t_erase_final_value (vector)
      ! Synopsis: Erases the last stored value in vector.
      type(vector_t), intent(inout), target :: vector
      integer(kind = int64) :: final

      associate (begin => vector % begin % idx, &
               & avail => vector % avail % idx)
          avail = avail - 1_int64
          final = avail - 1_int64
          vector % deref % it => vector % array % values(begin:final)
      end associate

      return
  end subroutine


  module subroutine vector_int32_t_erase_intermediate (vector, idx)
      ! Synopsis: Erases the value pointed to by index.
      type(vector_t), intent(inout), target :: vector
      integer(kind = int64), intent(in) :: idx
      integer(kind = int64):: final
      integer(kind = int64):: lb_ary, lb_vec, lb
      integer(kind = int64):: ub_ary, ub_vec, ub
      integer(kind = int64):: ary_bounds(0:1)
      integer(kind = int32), allocatable :: array(:)
      character(len=*), parameter :: errmsg = &
          & "dynamic::vector.erase(idx): unexpected error"


      associate (begin => vector % begin % idx,   &
               & avail => vector % avail % idx,   &
               & values => vector % array % values)

          ! overwrites vector values in range [idx, avail - 1)
          lb     = idx
          ub     = avail - 2_int64
          ! copies vector values in range [idx + 1, avail)
          lb_vec = lb + 1_int64
          ub_vec = ub + 1_int64

          ! placeholder's range for vector values in [idx + 1, avail)
          lb_ary = 0_int64
          ub_ary = avail - (idx + 1_int64) - 1_int64
          ary_bounds(0) = lb_ary
          ary_bounds(1) = ub_ary
          call allocator (ary_bounds, array)


          select type (values)
              type is ( integer(kind = int32 ) )
                  array(:) = values(lb_vec:ub_vec)
                  values(lb:ub) = array(:)
              class default
                  error stop errmsg
          end select


          avail = avail - 1_int64
          final = avail - 1_int64
          vector % deref % it => vector % array % values(begin:final)

      end associate


      return
  end subroutine


  module subroutine vector_int32_t_erase_byRangeShadow (vec, b, f)
      type(vector_t), intent(inout) :: vec
      integer(kind = int64), intent(in) :: b(2)
      integer(kind = int64):: lb, ub
      logical(kind = int32), intent(in), optional :: f

      lb = lbound(b, dim = 1, kind = int64)
      ub = ubound(b, dim = 1, kind = int64)
      lb = b(lb)
      ub = b(ub)
      call check_bounds (vec, lb)
      call check_bounds (vec, ub)
      if (lb <= ub) then
          call vector_int32_t_erase_by_range (vec, b)
!           else
!         print *, "empty range ... " ! passed test
      end if

      return
  end subroutine


  module subroutine vector_int32_t_erase_by_range (vector, bounds)
      type(vector_t), intent(inout) :: vector
      integer(kind = int64), intent(in) :: bounds(2)
      integer(kind = int64) :: lb, ub
      integer(kind = int64) :: final

      lb = lbound(bounds, dim = 1, kind = int64)
      ub = ubound(bounds, dim = 1, kind = int64)

      lb = bounds(lb)
      ub = bounds(ub)

      final = vector % avail % idx - 1_int64
      if (lb == 0_int64 .and. ub == final) then
          call vector_int32_t_erase_all (vector)
      else
          call vector_int32_t_trim (vector, bounds)
      end if

      return
  end subroutine


  module subroutine vector_int32_t_trim (vector, vec_bounds)
      type(vector_t), intent(inout), target :: vector
      integer(kind = int64), intent(in) :: vec_bounds(2)
      integer(kind = int64):: final
      integer(kind = int64):: ary_bounds(0:1)
      integer(kind = int64):: lb, lb_ary
      integer(kind = int64):: ub, ub_ary
      integer(kind = int64):: i, j, idx, numel
      integer(kind = int32), allocatable :: array(:)
      integer(kind = int32), allocatable :: mask(:)
      character(len=*), parameter :: errmsg = &
          & "dynamic::vector.trimming: unexpected error"

      lb = lbound(vec_bounds, dim = 1, kind = int64)
      ub = ubound(vec_bounds, dim = 1, kind = int64)

      ! defines the (inclusive) trimming range lb:ub
      lb = vec_bounds(lb)
      ub = vec_bounds(ub)


      ! masks (numel) elements to copy from vector into array
      lb_ary = 0_int64
      ub_ary = vector % avail % idx - 1_int64
      ary_bounds(0) = lb_ary
      ary_bounds(1) = ub_ary
      call allocator (ary_bounds, mask)

      ! masks values for erasing
      mask(:) = 0
      mask(lb:ub) = 1
      numel = (ub - lb) + 1_int64

      lb_ary = 0_int64
      ub_ary = vector % avail % idx - numel - 1_int64
      ary_bounds(0) = lb_ary
      ary_bounds(1) = ub_ary
      call allocator (ary_bounds, array)

      associate (begin  => vector % begin % idx,  &
               & avail  => vector % avail % idx,  &
               & values => vector % array % values)


          select type (values)
              type is ( integer(kind = int32) )

                  idx = 0_int64
                  ! copies values unselected (for removal) into array
                  do i = begin, avail - 1_int64
                      if ( mask(i) == 0 ) then
                          array(idx) = values(i)
                          idx = idx + 1_int64
                      end if
                  end do

                  j = 0_int64
                  idx = 0_int64
                  ! effectively erases values by overwriting
                  do i = begin, avail - 1_int64
                      if ( mask(i) == 1 ) then
                          values(j) = array(idx)
                          j = j + 1_int64
                          idx = idx + 1_int64
                      end if

                      ! breaks if there are no more values to copy
                      if ( idx == avail - numel ) then
                          exit
                      end if

                  end do

              class default
                  error stop errmsg
          end select


          avail = avail - numel
          final = avail - 1_int64
          vector % deref % it => vector % array % values(begin:final)

      end associate

      return
  end subroutine


  module subroutine vector_int32_t_erase_byVecSubShadow (vec, vs, f)
      ! delegates the task to (specialized) subroutines
      type(vector_t), intent(inout) :: vec
      integer(kind = int64), intent(in) :: vs(:)
      integer(kind = int64):: lb, ub, numel
      logical(kind = int32), intent(in), optional :: f

      ! queries the bounds and size of the vector-subscript
      lb = minval(vs)
      ub = maxval(vs)
      numel = size(vs, kind = int64)

      call check_bounds (vec, lb)
      call check_bounds (vec, ub)

      if ( numel == vec % size() ) then
          call vector_int32_t_erase_all (vec)
      else
          call vector_int32_t_erase_by_subscript (vec, vs)
      end if

      return
  end subroutine


  module subroutine vector_int32_t_erase_by_subscript (vector, vs)
      ! erases vector elements marked by the vector-subscript `vs'
      type(vector_t), intent(inout), target :: vector
      integer(kind = int64), intent(in) :: vs(:)
      integer(kind = int64):: final
      integer(kind = int64):: ary_bounds(0:1)
      integer(kind = int64):: lb, ub
      integer(kind = int64):: i, idx, numel
      integer(kind = int32), allocatable :: array(:)
      integer(kind = int32), allocatable :: mask(:)
      character(len=*), parameter :: errmsg = &
          & "dynamic::vector.erase_by_subscript: unexpected error"

      ! queries for the bounds and size of the vector-subscript
      lb = lbound(vs, dim = 1, kind = int64)
      ub = ubound(vs, dim = 1, kind = int64)
      numel = size(vs, kind = int64)

      ary_bounds(0) = 0_int64
      ary_bounds(1) = vector % avail % idx - numel - 1_int64
      call allocator (ary_bounds, array)

      ary_bounds(0) = 0_int64
      ary_bounds(1) = vector % avail % idx - 1_int64
      call allocator (ary_bounds, mask)

      mask = 0
      ! marks vector elements for erasing
      do idx = lb, ub
          mask( vs(idx) ) = 1
      end do


      associate (begin  => vector % begin % idx,  &
               & avail  => vector % avail % idx,  &
               & values => vector % array % values)

          select type (values)
              type is ( integer(kind = int32) )


                  i = 0_int64
                  ! copies ``unselected'' elements into placeholder
                  do idx = begin, (avail - 1_int64)
                      if ( mask(idx) == 0 ) then
                          array(i) = values(idx)
                          i = i + 1_int64
                      end if
                  end do


                  ! erases selected elements by overwriting
                  values( 0 : (avail - numel - 1_int64) ) = array(:)


              class default
                  error stop errmsg
          end select

          avail = avail - numel
          final = avail - 1_int64
          vector % deref % it => vector % array % values(begin:final)
      end associate


      return
  end subroutine


  module subroutine vector_int32_t_erase_byValueShadow (vec, elem, f)
      type(vector_t), intent(inout) :: vec
      integer(kind = int32), intent(in) :: elem(:)
      logical(kind = int32), intent(in), optional :: f

      call vector_int32_t_erase_values (vec, elem)

      return
  end subroutine


  module subroutine vector_int32_t_erase_values (vector, elements)
      ! erases vector elements equal to ``elements''
      type(vector_t), intent(inout) :: vector
      integer(kind = int64), allocatable :: vs(:)
      integer(kind = int32), intent(in) :: elements(:)
      integer(kind = int32), allocatable :: mask(:)
      integer(kind = int64):: ary_bounds(0:1), bounds(0:1)
      integer(kind = int64):: i, lb, ub, idx, numel
      character(len=*), parameter :: errmsg = &
          & "dynamic::vector.erase_values: unexpected error"


      ! queries for the bounds and size of the array of elements
      numel = size(elements, kind = int64)
      lb = lbound(elements, dim = 1, kind = int64)
      ub = ubound(elements, dim = 1, kind = int64)

      ary_bounds(0) = 0_int64
      ary_bounds(1) = vector % avail % idx - 1_int64
      call allocator (ary_bounds, mask)


      associate (begin  => vector % begin % idx,  &
               & avail  => vector % avail % idx,  &
               & values => vector % array % values)

          select type (values)
              type is ( integer(kind = int32) )

                  mask = 0
                  do idx = begin, (avail - 1_int64)

                      ! masks values equal to any of the elements
                      do i = lb, ub
                          if ( values(idx) == elements(i) ) then
                              mask(idx) = mask(idx) + 1
                          else
                              mask(idx) = mask(idx) + 0
                          end if
                      end do

                      mask(idx) = min(mask(idx), 1) !! mask is 0|1
                  end do

              class default
                  error stop errmsg
          end select

      end associate


      ! finds the number of elements marked for removal and delegates
      numel = int (sum(mask), kind = int64)

      if ( numel > 0_int64 ) then

          if ( numel == vector % avail % idx ) then
              call vector_int32_t_erase_all (vector)
          else

              bounds(0) = 0_int64
              bounds(1) = numel - 1_int64
              call allocator (bounds, vs)

              i = 0_int64
              ! generates vector-subscript for erase method
              do idx = ary_bounds(0), ary_bounds(1)
                  if ( mask(idx) == 1 ) then
                      vs(i) = idx
                      i = i + 1_int64
                  end if
              end do

              call vector_int32_t_erase_by_subscript (vector, vs)

          end if

      end if


      return
  end subroutine


  module subroutine vector_int32_t_erase_argsCheck (i, b, s, v, m)
      ! Synopsis:
      ! Checks input arg[ument]s of erase method.
      integer(kind = int64), intent(in), optional :: i
      integer(kind = int64), intent(in), optional :: b(2)
      integer(kind = int64), intent(in), optional :: s(:)
      integer(kind = int32), intent(in), optional :: v(:)
      character(len=9), intent(in),      optional :: m
      character(len=*), parameter :: wrnmsg = &
          & "dynamic::vector.erase(): ignoring mode, it's only " // &
          & "meaningful for ranges"
      character(len=*), parameter :: errmsg = &
          & "dynamic::vector.erase(): erases either by index, "  // &
          & "subscript, range, or value, exclusively"
      character(len=*), parameter :: errmsg_mode = &
          & "dynamic::vector.erase(): missing range for " // &
          & "applying the supplied mode"


      if ( present(i) ) then
          if ( present(b) ) then
              print *, errmsg
          else if ( present(s) ) then
              print *, errmsg
          else if ( present(v) ) then
              print *, errmsg
          end if
      end if


      if ( present(b) ) then
          if ( present(s) ) then
              print *, errmsg
          else if ( present(v) ) then
              print *, errmsg
          end if
      end if


      if ( present(s) ) then
          if ( present(v) ) then
              print *, errmsg
          end if
      end if


      if ( present(m) ) then

          if ( .not. present(b) ) then
              print *, errmsg_mode
          end if

          if ( present(i) .or. present(s) .or. present(v) ) then
              print *, wrnmsg
          end if

      end if


      return
  end subroutine


end submodule


! References:
! SJ Chapman, FORTRAN for Scientists and Engineers, fourth edition
! A Koenig and B Moo, Accelerated C++ Practical Programming by Example
