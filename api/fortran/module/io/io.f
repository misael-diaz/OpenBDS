#define __SUCCESS__  0_int64
#define __FAILURE__ -1_int64
#define __FNAME_LENGTH__ 256

      module io
        use, intrinsic :: iso_fortran_env, only: dp => real64
        use, intrinsic :: iso_fortran_env, only: real64
        use, intrinsic :: iso_fortran_env, only: int64
#if !defined(__GFORTRAN__)
c       uses Intel FORTRAN Portability `IFPORT' Module if we are not compiling with the
c       GNU FORTRAN Compiler, this is needed because `rename()` is a GNU Extension
        use :: ifport, only: rename
#endif
        use :: config, only: N => NUM_PARTICLES
        use :: particle, only: particle_t
        implicit none
        private
        public :: io__flogger

        interface io__flogger
          module procedure flog_base
        end interface

      contains

        subroutine bind (x, y, z,
     +                   r_x, r_y, r_z,
     +                   Eax, Eay, Eaz,
     +                   d_x, d_y, d_z,
     +                   F_x, F_y, F_z,
     +                   T_x, T_y, T_z,
     +                   id, particles)
c         Synopsis:
c         Binds pointers to their respective particle fields.
          class(particle_t), intent(in), target :: particles
          real(kind = dp), pointer, contiguous, intent(inout) :: x(:)
          real(kind = dp), pointer, contiguous, intent(inout) :: y(:)
          real(kind = dp), pointer, contiguous, intent(inout) :: z(:)
          real(kind = dp), pointer, contiguous, intent(inout) :: r_x(:)
          real(kind = dp), pointer, contiguous, intent(inout) :: r_y(:)
          real(kind = dp), pointer, contiguous, intent(inout) :: r_z(:)
          real(kind = dp), pointer, contiguous, intent(inout) :: Eax(:)
          real(kind = dp), pointer, contiguous, intent(inout) :: Eay(:)
          real(kind = dp), pointer, contiguous, intent(inout) :: Eaz(:)
          real(kind = dp), pointer, contiguous, intent(inout) :: d_x(:)
          real(kind = dp), pointer, contiguous, intent(inout) :: d_y(:)
          real(kind = dp), pointer, contiguous, intent(inout) :: d_z(:)
          real(kind = dp), pointer, contiguous, intent(inout) :: F_x(:)
          real(kind = dp), pointer, contiguous, intent(inout) :: F_y(:)
          real(kind = dp), pointer, contiguous, intent(inout) :: F_z(:)
          real(kind = dp), pointer, contiguous, intent(inout) :: T_x(:)
          real(kind = dp), pointer, contiguous, intent(inout) :: T_y(:)
          real(kind = dp), pointer, contiguous, intent(inout) :: T_z(:)
          real(kind = dp), pointer, contiguous, intent(inout) :: id(:)

          x => particles % x
          y => particles % y
          z => particles % z

          r_x => particles % r_x
          r_y => particles % r_y
          r_z => particles % r_z

          Eax => particles % Eax
          Eay => particles % Eay
          Eaz => particles % Eaz

          d_x => particles % d_x
          d_y => particles % d_y
          d_z => particles % d_z

          F_x => particles % F_x
          F_y => particles % F_y
          F_z => particles % F_z

          T_x => particles % T_x
          T_y => particles % T_y
          T_z => particles % T_z

          id => particles % id

          return
        end subroutine bind


        function fstatus (IOSTAT) result(STATUS)
c         Synopsis:
c         Sets `STATUS' based on the value stored in `IOSTAT'.
          integer(kind = int64), intent(in) :: IOSTAT
          integer(kind = int64) :: STATUS

          if (IOSTAT == __SUCCESS__) then
            STATUS = __SUCCESS__
          else
            STATUS = __FAILURE__
          end if

          return
        end function fstatus


        function fopen_wr (filename, fd) result(status)
c         Synopsis:
c         Opens filename for writing.
c         On success (failure) the file descriptor `fd' is set (unknown).
c         Returns the status of this operation to the caller.
          character(len=__FNAME_LENGTH__), intent(in) :: filename
c         file descriptor
          integer(kind = int64), intent(out) :: fd
c         IO status
          integer(kind = int64) :: status
          integer(kind = int64) :: iostat

          open(newunit = fd,
     +         file = trim(filename),
     +         form = 'formatted',
     +         access = 'sequential',
     +         status = 'new',
     +         action = 'write',
     +         iostat = iostat)

          status = fstatus(iostat)

          return
        end function fopen_wr


        function fopen_rd (filename, fd) result(status)
c         Synopsis:
c         Opens filename for reading.
c         On success (failure) the file descriptor `fd' is set (unknown).
c         Returns the status of this operation to the caller.
          character(len=__FNAME_LENGTH__), intent(in) :: filename
c         file descriptor
          integer(kind = int64), intent(out) :: fd
c         IO status
          integer(kind = int64) :: status
          integer(kind = int64) :: iostat

          open(newunit = fd,
     +         file = trim(filename),
     +         form = 'formatted',
     +         access = 'sequential',
     +         status = 'old',
     +         action = 'read',
     +         iostat = iostat)

          status = fstatus(iostat)

          return
        end function fopen_rd


        function fopen (filename, fd, action) result(status)
c         Synopsis:
c         Opens filename for writing the default.
c         If the optional argument `action' is supplied it opens the file for reading if
c         `action' is `r' and for writting if `action' is `w'.
c         On success (failure) the file descriptor `fd' is set (unknown).
c         Returns the status of this operation to the caller.
          character(len=__FNAME_LENGTH__), intent(in) :: filename
c         file descriptor
          integer(kind = int64), intent(out) :: fd
c         IO status
          integer(kind = int64) :: status
          character(len=1), intent(in), optional :: action

          if ( present(action) ) then

            if (action == 'w') then
              status = fopen_wr(filename, fd)
            else
              status = fopen_rd(filename, fd)
            end if

          else

            status = fopen_wr(filename, fd)

          end if

          return
        end function fopen


        function fclose (fd) result(status)
c         Synopsis:
c         Closes the file associated with the file descriptor `fd'.
c         Returns the status of this operation to the caller.
          integer(kind = int64), intent(in) :: fd
c         IO status
          integer(kind = int64) :: status
          integer(kind = int64) :: iostat

          close(unit = fd, iostat = iostat)

          status = fstatus(iostat)

          return
        end function fclose


        function flogger (x, y, z,
     +                    r_x, r_y, r_z,
     +                    Eax, Eay, Eaz,
     +                    d_x, d_y, d_z,
     +                    F_x, F_y, F_z,
     +                    T_x, T_y, T_z,
     +                    id, fd)
     +  result(status)
c         Synopsis:
c         Logs the particle fields (or properties).
c         Returns the status of this operation to the caller.
c         position vector components subject to periodic conditions
          real(kind = real64), intent(in) :: x(N)
          real(kind = real64), intent(in) :: y(N)
          real(kind = real64), intent(in) :: z(N)
c         position vector components independent of periodic conditions
          real(kind = real64), intent(in) :: r_x(N)
          real(kind = real64), intent(in) :: r_y(N)
          real(kind = real64), intent(in) :: r_z(N)
c         Euler angle vector components
          real(kind = real64), intent(in) :: Eax(N)
          real(kind = real64), intent(in) :: Eay(N)
          real(kind = real64), intent(in) :: Eaz(N)
c         director (or orientation vector) components
          real(kind = real64), intent(in) :: d_x(N)
          real(kind = real64), intent(in) :: d_y(N)
          real(kind = real64), intent(in) :: d_z(N)
c         force vector components
          real(kind = real64), intent(in) :: F_x(N)
          real(kind = real64), intent(in) :: F_y(N)
          real(kind = real64), intent(in) :: F_z(N)
c         torque vector components
          real(kind = real64), intent(in) :: T_x(N)
          real(kind = real64), intent(in) :: T_y(N)
          real(kind = real64), intent(in) :: T_z(N)
c         identifiers IDs
          real(kind = real64), intent(in) :: id(N)
c         file descriptor
          integer(kind = int64), intent(in) :: fd
c         IO status
          integer(kind = int64) :: status
          integer(kind = int64) :: iostat
c         loop index
          integer(kind = int64) :: i
c         format `fmt' specifier, NEw.d
          character(*), parameter :: fmt = '(SP,19E32.16)'
c         NOTE:
c         SP: Sign Print
c         N: number of values to print
c         E: exponential (or scientific) format
c         w: width, 25 positions
c         d: digits after the decimal place, 16 digits

          do i = 1_int64, N
            write(unit = fd, fmt = fmt, iostat = iostat)
     +        x(i), y(i), z(i),      ! position vector
     +        r_x(i), r_y(i), r_z(i),! `absolute' position vector
     +        Eax(i), Eay(i), Eaz(i),! Euler angle vector
     +        d_x(i), d_y(i), d_z(i),! director (or orientation vector)
     +        F_x(i), F_y(i), F_z(i),! force vector
     +        T_x(i), T_y(i), T_z(i),! torque vector
     +        id(i)                  ! identifier ID
          end do

          status = fstatus(iostat)

          return
        end function flogger


        function flog (particles, fd) result(status)
c         Synopsis:
c         Logs the particle fields (or properties) to the file associated with the
c         file descriptor `fd'.
c         Returns the status of this operation to the caller.
          class(particle_t), intent(in), target :: particles
c         file descriptor
          integer(kind = int64), intent(in) :: fd
c         IO status
          integer(kind = int64) :: status
          real(kind = real64), pointer, contiguous :: x(:) => null()
          real(kind = real64), pointer, contiguous :: y(:) => null()
          real(kind = real64), pointer, contiguous :: z(:) => null()
          real(kind = real64), pointer, contiguous :: r_x(:) => null()
          real(kind = real64), pointer, contiguous :: r_y(:) => null()
          real(kind = real64), pointer, contiguous :: r_z(:) => null()
          real(kind = real64), pointer, contiguous :: Eax(:) => null()
          real(kind = real64), pointer, contiguous :: Eay(:) => null()
          real(kind = real64), pointer, contiguous :: Eaz(:) => null()
          real(kind = real64), pointer, contiguous :: d_x(:) => null()
          real(kind = real64), pointer, contiguous :: d_y(:) => null()
          real(kind = real64), pointer, contiguous :: d_z(:) => null()
          real(kind = real64), pointer, contiguous :: F_x(:) => null()
          real(kind = real64), pointer, contiguous :: F_y(:) => null()
          real(kind = real64), pointer, contiguous :: F_z(:) => null()
          real(kind = real64), pointer, contiguous :: T_x(:) => null()
          real(kind = real64), pointer, contiguous :: T_y(:) => null()
          real(kind = real64), pointer, contiguous :: T_z(:) => null()
          real(kind = real64), pointer, contiguous :: id(:) => null()

          call bind(x, y, z,
     +              r_x, r_y, r_z,
     +              Eax, Eay, Eaz,
     +              d_x, d_y, d_z,
     +              F_x, F_y, F_z,
     +              T_x, T_y, T_z,
     +              id, particles)

          status = flogger(x, y, z,
     +                     r_x, r_y, r_z,
     +                     Eax, Eay, Eaz,
     +                     d_x, d_y, d_z,
     +                     F_x, F_y, F_z,
     +                     T_x, T_y, T_z,
     +                     id, fd)

          return
        end function flog


        function flog_base (particles, step) result(status)
c         Synopsis:
c         Logs the current particle fields (or properties).
c         Returns the status of this operation to the caller.
          class(particle_t), intent(in) :: particles
c         OBDS simulation step number (or identifier)
          integer(kind = int64), intent(in) :: step
c         file descriptor
          integer(kind = int64) :: fd
c         IO status
          integer(kind = int64) :: status
c         placeholder to store the step number in a string
          character(len = 64) :: step_str
          character(len = __FNAME_LENGTH__) :: tempname
          character(len = __FNAME_LENGTH__) :: filename
          character(len = *), parameter :: path =
     +    'run/bds/data/positions/particles-'

c         writes step number to a string of suitable size
          write(step_str, '(I64)') step
          step_str = adjustl(step_str)

c         defines the temporary and designated filenames
          tempname = path // trim(step_str) // '.tmp'
          filename = path // trim(step_str) // '.txt'

c         opens temporary file for writing
          status = fopen(tempname, fd)

c         logs the (current) particle fields (or properties) to the file
          if (STATUS == __SUCCESS__) then
            status = flog(particles, fd)
          end if

c         closes the file
          if (STATUS == __SUCCESS__) then
            status = fclose(fd)
          end if

c         renames file temporary to its designated name (GNU Extension)
c         NOTE:
c         This is so that we know for sure during post-processing that the data was
c         written successfully (we won't need to worry about data corruption due to
c         IO errors).
          if (STATUS == __SUCCESS__) then
            status = int(rename(tempname, filename), kind = int64)
          end if

          return
        end function flog_base

      end module io

*   OpenBDS                                             October 29, 2023
*
*   source: api/fortran/module/io/io.f
*   author: @misael-diaz
*
*   Synopsis:
*   Implements methods for performing Input-Output IO operations.
*
*   Copyright (C) 2023 Misael Díaz-Maldonado
*
*   This program is free software: you can redistribute it and/or modify
*   it under the terms of the GNU General Public License as published by
*   the Free Software Foundation, either version 3 of the License, or
*   (at your option) any later version.
*
*   This program is distributed in the hope that it will be useful,
*   but WITHOUT ANY WARRANTY; without even the implied warranty of
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*   GNU General Public License for more details.
*
*   You should have received a copy of the GNU General Public License
*   along with this program. If not, see <http://www.gnu.org/licenses/>.
*
*   References:
*   [0] SJ Chapman, FORTRAN for Scientists and Engineers, 4th edition.
*   [1] MP Allen and DJ Tildesley, Computer Simulation of Liquids.
*   [2] S Kim and S Karrila, Microhydrodynamics.
