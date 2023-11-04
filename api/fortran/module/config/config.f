      module config
        use, intrinsic :: iso_fortran_env, only: int32
        use, intrinsic :: iso_fortran_env, only: int64
        use, intrinsic :: iso_fortran_env, only: real64
        implicit none
        private
        save
        public :: LIMIT
        public :: LENGTH
        public :: TIME_STEP
        public :: WALLTIME
        public :: NUM_STEPS
        public :: NUM_LOG_STEPS
        public :: NUM_PARTICLES
        public :: LOG_NUM_PARTICLES
        public :: PENDING
        public :: DONE


**                                                                    **
*       OBDS Job status, either `unknown', `pending', or `done'        *
        enum, bind(c)
          enumerator :: UNKNOWN
          enumerator :: PENDING
          enumerator :: DONE
        end enum
*       NOTE:                                                          *
*       The `unknown' status is not really dumped by the OBDS code, it *
*       is dumped by the shell script that submits the code to the HPC *
*       as a fail-safe mechanism to avert a job-scheduling hell, which *
*       can happen if an unexpected error occurs (without fail-safe)   *
*       The `pending' status signals the shell script to submit the    *
*       job to the HPC because the simulation has not ended yet. The   *
*       `done' status is dumped by the OBDS code when the simulation   *
*       finishes, that happens the step counter is equal to NUM_STEPS. *
**                                                                    **


**                                                                    **
*       system box limits and length                                   *
        real(kind = real64), parameter :: LIMIT  = 8.0_real64
        real(kind = real64), parameter :: LENGTH = (2.0_real64 * LIMIT)
        real(kind = real64), parameter :: WIDTH  = LENGTH
        real(kind = real64), parameter :: HEIGHT = LENGTH
*       NOTE:                                                          *
*       The particle coordinates are bound to [-LIMIT, +LIMIT].        *
**                                                                    **


**                                                                    **
*       OBDS time-step, start-time, and end-time of the simulation     *
        real(kind = real64), parameter :: TIME_STEP = 2.0_real64**(-16)
        real(kind = real64), parameter :: TIME_START = 0.0_real64
        real(kind = real64), parameter :: TIME_END = 2.0_real64**4
*       OBDS logging time-step                                         *
        real(kind = real64), parameter :: TS_LOG = 2.0_real64**(-4)
        real(kind = real64), parameter :: TIME_STEP_LOG = TS_LOG
*       number of (simulation) steps                                   *
        integer(kind = int64), parameter :: NUM_STEPS =
     +  int( (TIME_END - TIME_START) / TIME_STEP, kind = int64 )
*       after this number of steps the OBDS code logs data to a file   *
        integer(kind = int64), parameter :: NUM_LOG_STEPS =
     +  int(TIME_STEP_LOG / TIME_STEP, kind = int64)
*       NOTE:
*       The OBDS code logs the particle fields (or properties) to a    *
*       plain text file after this (simulation) time interval has      *
*       elapsed. The logged data is used for post-processing to study  *
*       suspension properties, such as the Mean Squared Displacement   *
*       MSD, the diffusivity, the pair distribution function, etc.     *
**                                                                    **


**                                                                    **
*       defines the walltime (alloted runtime for the application in   *
*       the High Performance Computing facility HPCf in milliseconds)  *
        integer(kind = int32), parameter :: WALLTIME_HRS = 164_int32
        integer(kind = int32), parameter :: WALLTIME_SEC = 3600_int32 *
     +                                      WALLTIME_HRS
        integer(kind = int32), parameter :: WALLTIME_MILLIS =
     +                                      1000_int32 * WALLTIME_SEC
        integer(kind = int32), parameter :: WALLTIME = WALLTIME_MILLIS
*                                                                      *
**                                                                    **


**                                                                    **
*       stores log base two of N, log2(N), where `N' is #particles     *
        integer(kind = int64), parameter :: LOG_NUM_PARTICLES = 8_int64
*       defines the number of particles in the system
        integer(kind = int64), parameter :: NUM_PARTICLES =
     +  2_int64 ** LOG_NUM_PARTICLES
*       NOTE:
*       The number of particles is a power of two by design. Some of   *
*       the algorithms that will be implemented expect the number of   *
*       particles to be expressible exactly as a power of two. Change  *
*       this design constraint only if you know what you are doing.    *
*       If all that you want is to achieve a certain volume fraction   *
*       that can be done more easily by adjusting the system LIMIT. We *
*       use configs that can be expressed as powers of two because     *
*       these have exact binary floating-point representations.        *
**                                                                    **


**                                                                    **
*       enables computation of particle-particle interactions
        logical(kind = int64), parameter :: INTERACT_ENABLE = .false.
*       NOTE:
*       Disabled temporarily since we have to yet implement this       *
*       feature. Other reasons for disabling it are code profiling and *
*       validating the statistics of the normally-distributed          *
*       Pseudo Random Number Generator PRNG.                           *
**                                                                    **

      end module config

*   OpenBDS                                             October 21, 2023
*
*   source: api/fortran/module/config/config.f
*   author: @misael-diaz
*
*   Synopsis:
*   Defines OBDS Constants.
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
