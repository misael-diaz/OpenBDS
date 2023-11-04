#define __SUCCESS__  0_int64
#define __FAILURE__ -1_int64
#define __FNAME_LENGTH__ 256

      module io
        use, intrinsic :: iso_fortran_env, only: r8 => real64
        use, intrinsic :: iso_fortran_env, only: real64
        use, intrinsic :: iso_fortran_env, only: int64
        use, intrinsic :: iso_fortran_env, only: int32
#if !defined(__GFORTRAN__)
c       uses Intel FORTRAN Portability `IFPORT' Module if we are not compiling with the
c       GNU FORTRAN Compiler, this is needed because `rename()` is a GNU Extension
        use :: ifport, only: rename
#endif
        use :: config, only: N => NUM_PARTICLES
        use :: config, only: NUM_STEPS
        use :: config, only: PENDING
        use :: config, only: DONE
        use :: particle, only: particle_t
        implicit none
        private
        public :: io__flogger
        public :: io__floader
        public :: io__fdump_state
        public :: io__ffetch_state
        public :: io__fdump_status

        interface io__flogger
          module procedure flog_base
        end interface

        interface io__floader
          module procedure fload_base
        end interface

        interface io__fdump_state
          module procedure fdump_state_base
        end interface

        interface io__ffetch_state
          module procedure ffetch_state_base
        end interface

        interface io__fdump_status
          module procedure fdump_status_base
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
c         position vector components subject to periodic conditions
          real(kind = r8), pointer, contiguous, intent(inout) :: x(:)
          real(kind = r8), pointer, contiguous, intent(inout) :: y(:)
          real(kind = r8), pointer, contiguous, intent(inout) :: z(:)
c         position vector components independent of periodic conditions
          real(kind = r8), pointer, contiguous, intent(inout) :: r_x(:)
          real(kind = r8), pointer, contiguous, intent(inout) :: r_y(:)
          real(kind = r8), pointer, contiguous, intent(inout) :: r_z(:)
c         Euler angle vector components
          real(kind = r8), pointer, contiguous, intent(inout) :: Eax(:)
          real(kind = r8), pointer, contiguous, intent(inout) :: Eay(:)
          real(kind = r8), pointer, contiguous, intent(inout) :: Eaz(:)
c         director (or orientation vector) components
          real(kind = r8), pointer, contiguous, intent(inout) :: d_x(:)
          real(kind = r8), pointer, contiguous, intent(inout) :: d_y(:)
          real(kind = r8), pointer, contiguous, intent(inout) :: d_z(:)
c         force vector components
          real(kind = r8), pointer, contiguous, intent(inout) :: F_x(:)
          real(kind = r8), pointer, contiguous, intent(inout) :: F_y(:)
          real(kind = r8), pointer, contiguous, intent(inout) :: F_z(:)
c         torque vector components
          real(kind = r8), pointer, contiguous, intent(inout) :: T_x(:)
          real(kind = r8), pointer, contiguous, intent(inout) :: T_y(:)
          real(kind = r8), pointer, contiguous, intent(inout) :: T_z(:)
c         identifiers IDs
          real(kind = r8), pointer, contiguous, intent(inout) :: id(:)

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


        function fopen_ow (filename, fd) result(status)
c         Synopsis:
c         Opens filename for writing, overwrites the file contents if existing.
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
     +         status = 'unknown',
     +         action = 'write',
     +         iostat = iostat)

          status = fstatus(iostat)

          return
        end function fopen_ow


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
              status = fopen_wr(filename, fd) ! opens for writing
            else if (action == 'o') then
              status = fopen_ow(filename, fd) ! opens for overwriting
            else
              status = fopen_rd(filename, fd) ! opens for reading
            end if

          else

            status = fopen_wr(filename, fd)   ! defaults to writing

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


        function floader (x, y, z,
     +                    r_x, r_y, r_z,
     +                    Eax, Eay, Eaz,
     +                    d_x, d_y, d_z,
     +                    F_x, F_y, F_z,
     +                    T_x, T_y, T_z,
     +                    id, fd)
     +  result(status)
c         Synopsis:
c         Loads the particle fields (or properties) from the data file whose file
c         descriptor is `fd'.
c         Returns the status of this operation to the caller.
c         position vector components subject to periodic conditions
          real(kind = real64), intent(out) :: x(N)
          real(kind = real64), intent(out) :: y(N)
          real(kind = real64), intent(out) :: z(N)
c         position vector components independent of periodic conditions
          real(kind = real64), intent(out) :: r_x(N)
          real(kind = real64), intent(out) :: r_y(N)
          real(kind = real64), intent(out) :: r_z(N)
c         Euler angle vector components
          real(kind = real64), intent(out) :: Eax(N)
          real(kind = real64), intent(out) :: Eay(N)
          real(kind = real64), intent(out) :: Eaz(N)
c         director (or orientation vector) components
          real(kind = real64), intent(out) :: d_x(N)
          real(kind = real64), intent(out) :: d_y(N)
          real(kind = real64), intent(out) :: d_z(N)
c         force vector components
          real(kind = real64), intent(out) :: F_x(N)
          real(kind = real64), intent(out) :: F_y(N)
          real(kind = real64), intent(out) :: F_z(N)
c         torque vector components
          real(kind = real64), intent(out) :: T_x(N)
          real(kind = real64), intent(out) :: T_y(N)
          real(kind = real64), intent(out) :: T_z(N)
c         identifiers IDs
          real(kind = real64), intent(out) :: id(N)
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
            read(unit = fd, fmt = fmt, iostat = iostat)
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
        end function floader


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


        function fload (particles, fd) result(status)
c         Synopsis:
c         Loads the particle fields (or properties) from the datga file associated with
c         the file descriptor `fd'.
c         Returns the status of this operation to the caller.
          class(particle_t), intent(inout), target :: particles
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

          status = floader(x, y, z,
     +                     r_x, r_y, r_z,
     +                     Eax, Eay, Eaz,
     +                     d_x, d_y, d_z,
     +                     F_x, F_y, F_z,
     +                     T_x, T_y, T_z,
     +                     id, fd)

          return
        end function fload


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
          integer(kind = int64) :: iostat
c         placeholder to store the step number in a string
          character(len = 64) :: step_str
          character(len = __FNAME_LENGTH__) :: tempname
          character(len = __FNAME_LENGTH__) :: filename
          character(len = *), parameter :: path =
     +    'run/bds/data/positions/particles-'

c         writes step number to a string of suitable size
c         NOTE:
c         guards against invalid input
          write(step_str, '(I64)') abs(step)
          step_str = adjustl(step_str)

c         defines the temporary and designated filenames
          tempname = path // trim(step_str) // '.tmp'
          filename = path // trim(step_str) // '.txt'

c         opens temporary file for writing
          status = fopen(tempname, fd)
          if (STATUS == __FAILURE__) then
            print *, 'IO ERROR with file: ', trim(tempname)
            return
          end if

c         logs the (current) particle fields (or properties) to the file
          status = flog(particles, fd)
          if (STATUS == __FAILURE__) then
            print *, 'WRITE ERROR with file: ', trim(tempname)
            close(fd)
            return
          end if

c         closes the file
          status = fclose(fd)
          if (STATUS == __FAILURE__) then
            print *, 'UNEXPECTED IO ERROR with file: ', trim(tempname)
            return
          end if

c         renames file temporary to its designated name (GNU Extension)
c         NOTE:
c         This is so that we know for sure during post-processing that the data was
c         written successfully (we won't need to worry about data corruption due to
c         IO errors).
          iostat = int(rename(tempname, filename), kind = int64)
          status = fstatus(iostat)
          if (STATUS == __FAILURE__) then
            print *, 'UNEXPECTED IO ERROR with file: ', trim(filename)
            return
          end if

          return
        end function flog_base


        function fload_base (particles, step) result(status)
c         Synopsis:
c         Loads the current particle fields (or properties) from the data file whose
c         step number is `step'.
c         Returns the status of this operation to the caller.
          class(particle_t), intent(inout) :: particles
c         OBDS simulation step number (or identifier)
          integer(kind = int64), intent(in) :: step
c         file descriptor
          integer(kind = int64) :: fd
c         IO status
          integer(kind = int64) :: status
c         placeholder to store the step number in a string
          character(len = 64) :: step_str
          character(len = __FNAME_LENGTH__) :: filename
          character(len = *), parameter :: path =
     +    'run/bds/data/positions/particles-'

c         writes step number to a string of suitable size
c         NOTE:
c         guards against invalid input
          write(step_str, '(I64)') abs(step)
          step_str = adjustl(step_str)

c         defines the temporary and designated filenames
          filename = path // trim(step_str) // '.txt'

c         opens data file for reading
          status = fopen(filename = filename, fd = fd, action = 'r')
          if (STATUS == __FAILURE__) then
            print *, 'IO ERROR with file: ', trim(filename)
            return
          end if

c         loads the particle fields (or properties) from the file
          status = fload(particles, fd)
          if (STATUS == __FAILURE__) then
            print *, 'READ ERROR with file: ', trim(filename)
            return
          end if

c         closes the file
          status = fclose(fd)
          if (STATUS == __FAILURE__) then
            print *, 'UNEXPECTED IO ERROR with file: ', trim(filename)
            return
          end if

          return
        end function fload_base


        function fdump_state (istate) result(status)
c         Synopsis:
c         Dumps the last known system state to the state file, where the `state' is
c         the last known simulation step number
c         Returns the status of this operation to the caller.
          integer(kind = int64), intent(in) :: istate
c         file descriptor
          integer(kind = int64) :: fd
c         IO status
          integer(kind = int64) :: status
          integer(kind = int64) :: iostat
c         name of the state file
          character(len = __FNAME_LENGTH__), parameter :: fname =
     +    'run/bds/state/state.txt'
c         format for reading the step number (or state)
          character(*), parameter :: fmt = '(I64)'

c         tries to open the state file for reading
          status = fopen(filename = fname, fd = fd, action = 'o')
          if (status == __FAILURE__) then
            print *, 'IO ERROR with file: ', trim(fname)
            return
          end if

c         tries to write the state to the state file
          write(unit = fd, fmt = fmt, iostat = iostat) istate

c         queries the IO status
          status = fstatus(iostat)
          if (status == __FAILURE__) then
            print *, 'WRITE ERROR with file: ', trim(fname)
            close(fd)
            return
          end if

          status = fclose(fd)
          if (status == __FAILURE__) then
            print *, 'UNEXPECTED IO ERROR with file: ', trim(fname)
            return
          end if

          return
        end function fdump_state


        function fdump_status (stat) result(status)
c         Synopsis:
c         Dumps the OBDS simulation status to the status file.
c         Returns the status of this operation to the caller.
c         NOTE:
c         We attempt to write first to a temporary file as a fail-safe
c         mechanism, for the script that schedules this job to the HPC
c         dumps the `unknown' status and we should only change this if
c         we are successful in dumping a status. This is done by
c         renaming the status file from its temporary name to its
c         designated name. It is better to err on the safe side than
c         to cause a job-scheduling hell.
          integer(kind = int32), intent(in) :: stat
c         file descriptor
          integer(kind = int64) :: fd
c         IO status
          integer(kind = int64) :: status
          integer(kind = int64) :: iostat
c         temporary status file
          character(len = __FNAME_LENGTH__), parameter :: tname =
     +    'run/bds/status/status.tmp'
c         designated status file name
          character(len = __FNAME_LENGTH__), parameter :: fname =
     +    'run/bds/status/status.txt'
c         format for writing the status
          character(*), parameter :: fmt = '(A)'

c         tries to open a new status file for writing
          status = fopen(filename = tname, fd = fd, action = 'w')
          if (status == __FAILURE__) then
            print *, 'IO ERROR with file: ', trim(fname)
            return
          end if

c         tries to write the status to the temporary state file
          select case (stat)
            case (DONE)
              write(unit = fd, fmt = fmt, iostat = iostat) 'done'
            case (PENDING)
              write(unit = fd, fmt = fmt, iostat = iostat) 'pending'
            case default
              write(unit = fd, fmt = fmt, iostat = iostat) 'unknown'
          end select

c         queries the IO status
          status = fstatus(iostat)
          if (status == __FAILURE__) then
            print *, 'WRITE ERROR with file: ', trim(fname)
            close(fd)
            return
          end if

          status = fclose(fd)
          if (status == __FAILURE__) then
            print *, 'UNEXPECTED IO ERROR with file: ', trim(fname)
            return
          end if

          iostat = int(rename(tname, fname), kind = int64)
          status = fstatus(iostat)
          if (STATUS == __FAILURE__) then
            print *, 'UNEXPECTED IO ERROR with file: ', trim(fname)
            return
          end if

          return
        end function fdump_status


        function ffetch_state (state) result(status)
c         Synopsis:
c         Fetches the last known state from the state file.
c         Returns the status of this operation to the caller.
c         the state is the last known simulation step number
c         Returns the status of this operation to the caller.
          real(kind = real64), intent(out) :: state
c         file descriptor
          integer(kind = int64) :: fd
c         IO status
          integer(kind = int64) :: status
          integer(kind = int64) :: iostat
          integer(kind = int64) :: istate
c         name of the state file
          character(len = __FNAME_LENGTH__), parameter :: fname =
     +    'run/bds/state/state.txt'
c         format for reading the step number (or state)
          character(*), parameter :: fmt = '(I64)'

c         tries to open the state file for reading
          status = fopen(filename = fname, fd = fd, action = 'r')

c         NOTE:
c         if there's no state (IO status is in a failure state) this means that we might
c         be executing the OBDS code for this first time so we set the OBDS state to its
c         initial state (zero); otherwise, the state is set to whatever state is stored
c         in the state file in the next codeblocks
          if (STATUS == __FAILURE__) then
            print *, 'IO ERROR with file: ', trim(fname)
            print *, 'initializing state with its default value'
            state = 0.0_real64 ! zeros intent(out) argument (default)
            return
          end if

c         tries to read the contents of the state file into `state'
          read(unit = fd, fmt = fmt, iostat = iostat) istate

c         queries the IO status
          status = fstatus(iostat)
          if (status == __FAILURE__) then
            print *, 'UNEXPECTED READ ERROR with file: ', trim(fname)
            print *, 'initializing state with its default value'
            state = 0.0_real64 ! zeros intent(out) argument (default)
            close(fd)
            return
          end if

c         sets the state with the fecthed value
c         NOTE:
c         applies an upper bound on the `state' (step number) to guard
c         against invalid inputs
          state = real( min(istate, NUM_STEPS), kind = real64 )

          status = fclose(fd)
          if (status == __FAILURE__) then
            print *, 'UNEXPECTED IO ERROR with file: ', trim(fname)
            return
          end if

          return
        end function ffetch_state


        function fdump_state_base (istate) result(status)
c         Synopsis:
c         Dumps the OBDS state (holds the simulation step number) to the state file.
c         Returns the status of this operation to the caller.
c         system state (stands for the simulation step number)
          integer(kind = int64), intent(in) :: istate
c         IO status
          integer(kind = int64) :: status

          status = fdump_state(istate)

          return
        end function fdump_state_base


        function ffetch_state_base (particles) result(status)
c         Synopsis:
c         Fetches the OBDS state (holds the simulation step number) from the state file.
c         Returns the status of this operation to the caller.
          class(particle_t), intent(inout), target :: particles
c         temporary placeholder array
          real(kind = real64), pointer, contiguous :: tmp(:) => null()
c         IO status
          integer(kind = int64) :: status
c         system state (stands for the simulation step number)
          real(kind = real64) :: state

          status = ffetch_state(state)

c         we can afford to commit the state regardless of the IO status because
c         `ffetch_state()' either yields either the default or the (actual fetched) state;
c         the caller procedure has been designed to handle either case accordingly
          if (STATUS == __SUCCESS__ .OR. STATUS == __FAILURE__) then
            tmp => particles % tmp
            tmp(1) = state
          end if

          return
        end function ffetch_state_base


        function fdump_status_base (stat) result(status)
c         Synopsis:
c         Dumps the OBDS status to the status file to implement auto-scheduling.
c         Returns the status of this operation to the caller.
c         status of the simulation either (DONE, PENDING, or UNKNOWN)
          integer(kind = int32), intent(in) :: stat
c         IO status
          integer(kind = int64) :: status

          status = fdump_status(stat)

          return
        end function fdump_status_base

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
