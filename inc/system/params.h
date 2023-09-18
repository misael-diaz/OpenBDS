#ifndef GUARD_OPENBDS_SYSTEM_PARAMS_H
#define GUARD_OPENBDS_SYSTEM_PARAMS_H

#include "config.h"

#define __OBDS_INV_SQRT_TIME_STEP__ ( (size_t) 0x100 )
#define __OBDS_SQRT_TIME_STEP__ ( 1.0 / ( (double) ( __OBDS_INV_SQRT_TIME_STEP__ ) ) )
#define __OBDS_INV_TIME_STEP__ (__OBDS_INV_SQRT_TIME_STEP__ * __OBDS_INV_SQRT_TIME_STEP__)
#define __OBDS_TIME_STEP__ ( 1.0 / ( (double) ( __OBDS_INV_TIME_STEP__ ) ) )

#define __OBDS_LOG_NUM_PARTICLES__ ( (size_t) ( FCONF_LOG_NUM_PARTICLES ) )
#define __OBDS_NUM_PARTICLES__ ( (size_t) ( 1 << ( __OBDS_LOG_NUM_PARTICLES__ ) ) )

#endif

/*

OpenBDS							September 05, 2023

source: system/params.h
author: @misael-diaz

Synopsis:
Defines system parameters, such as the time-step and the number of particles.

Copyright (c) 2023 Misael Diaz-Maldonado
This file is released under the GNU General Public License as published
by the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

References:
[0] A Koenig and B Moo, Accelerated C++ Practical Programming by Example.
[1] MP Allen and DJ Tildesley, Computer Simulation of Liquids.
[2] S Kim and S Karrila, Microhydrodynamics: Principles and Selected Applications.

*/
