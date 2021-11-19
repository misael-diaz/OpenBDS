!
!   source: ffls_constructors.for
!   author: misael-diaz
!   date:   2021-11-19
!
!
!   Synopsis:
!   Defines constructors for the FORTRAN forward linked-list class.
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

submodule (FFLinkedListClass) ffls_constructors
implicit none
contains

  module function ffls_default_constructor () result(ffls)
      type(ffls_t) :: ffls
      type(list_t), pointer :: list => null()

      ffls % list = flist_create_list_t ()

      call c_f_pointer (ffls % list, list)
      ffls % self = list % self
      call c_f_procpointer (list % append_int32_t, ffls % append_int32_t)
      call c_f_procpointer (list % append_int64_t, ffls % append_int64_t)

      return
  end function


  module function ffls_int32_t_constructor (value) result(ffls)
      type(ffls_t) :: ffls
      type(list_t), pointer :: list => null()
      integer(kind = c_int32_t), intent(in) :: value

      ffls % list = flist_create_list_int32_t (value)

      call c_f_pointer (ffls % list, list)
      ffls % self = list % self
      call c_f_procpointer (list % append_int32_t, ffls % append_int32_t)
      call c_f_procpointer (list % append_int64_t, ffls % append_int64_t)

      return
  end function


  module function ffls_int64_t_constructor (value) result(ffls)
      type(ffls_t) :: ffls
      type(list_t), pointer :: list => null()
      integer(kind = c_int64_t), intent(in) :: value

      ffls % list = flist_create_list_int64_t (value)

      call c_f_pointer (ffls % list, list)
      ffls % self = list % self
      call c_f_procpointer (list % append_int32_t, ffls % append_int32_t)
      call c_f_procpointer (list % append_int64_t, ffls % append_int64_t)

      return
  end function

end submodule
