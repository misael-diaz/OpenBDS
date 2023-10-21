#ifndef GUARD_OPENBDS_SYSTEM_PARAMS_H
#define GUARD_OPENBDS_SYSTEM_PARAMS_H

#include <stdbool.h>

#include "config/config.h"

// true if we have isotropic particles (spheres), false if we have anisotropic particles;
// we use this MACRO to check if the OBDS has been configured in a consistent manner
#define __OBDS_ISOTROPIC_HYDRODYNAMIC_RESISTANCE__ (\
	(bool) ( FCONF_ISOTROPIC_HYDRODYNAMIC_RESISTANCE_PARTICLE )\
)

// defines 1 / sqrt(dt), where `dt' is the non-dimensional time-step; by design the code
// expects this to be a power of two (at this point it is not necessary for the user to
// modify it, this is why it is defined here)
#define __OBDS_INV_SQRT_TIME_STEP__ ( (size_t) 0x100 )
// calculates sqrt(dt)
#define __OBDS_SQRT_TIME_STEP__ ( 1.0 / ( (double) ( __OBDS_INV_SQRT_TIME_STEP__ ) ) )
// calculates 1 / dt
#define __OBDS_INV_TIME_STEP__ (__OBDS_INV_SQRT_TIME_STEP__ * __OBDS_INV_SQRT_TIME_STEP__)
// calculates dt
#define __OBDS_TIME_STEP__ ( 1.0 / ( (double) ( __OBDS_INV_TIME_STEP__ ) ) )

// defines log2(N), where `N' is the number of particles
#define __OBDS_LOG_NUM_PARTICLES__ ( (size_t) ( FCONF_LOG_NUM_PARTICLES ) )
// calculates the number of particles `N'
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
