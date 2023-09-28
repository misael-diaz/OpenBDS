#ifndef GUARD_OPENBDS_PARTICLE_SPHERE_PARAMS_H
#define GUARD_OPENBDS_PARTICLE_SPHERE_PARAMS_H

#include "system/params.h"

#define __OBDS_SPH_SQRT_OF_TWO__ 1.4142135623730951
#define __OBDS_SPH_SQRT_OF_THREE_HALVES__ 1.2247448713915890
#define __OBDS_SPH_LIN_BROWNIAN_MOBILITY__ (\
	( (double) ( __OBDS_SPH_SQRT_OF_TWO__ ) ) *\
	( (double) ( __OBDS_SQRT_TIME_STEP__ ) )\
	)
#define __OBDS_SPH_ANG_BROWNIAN_MOBILITY__ (\
	( (double) ( __OBDS_SPH_SQRT_OF_THREE_HALVES__ ) ) *\
	( (double) ( __OBDS_SQRT_TIME_STEP__ ) )\
	)
#define __OBDS_SPH_LIN_MOBILITY__  ( (double) ( __OBDS_TIME_STEP__ ) )

#define __OBDS_SPH_RADIUS__ ( (double) 1.0 )
#define __OBDS_SPH_DIAMETER__ ( (double) ( 2.0 * __OBDS_SPH_RADIUS__ ) )
#define __OBDS_SPH_CONTACT__ ( (double) ( __OBDS_SPH_DIAMETER__ ) )
#define __OBDS_SPH_RANGE__ ( (double) ( 4.0 * ( __OBDS_SPH_CONTACT__ ) ) )
#define __OBDS_SPH_EPSILON__ ( (double) 1.0 )

#define __OBDS_NUM_SPHERES__ ( (size_t) ( __OBDS_NUM_PARTICLES__ ) )
#define __OBDS_LOG_NUM_SPHERES__ ( (size_t) ( __OBDS_LOG_NUM_PARTICLES__ ) )

#endif

/*

OpenBDS							September 05, 2023

source: particle/sphere/params.h
author: @misael-diaz

Synopsis:
Defines the sphere parameters.

Copyright (c) 2023 Misael Diaz-Maldonado
This file is released under the GNU General Public License as published
by the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

References:
[0] A Koenig and B Moo, Accelerated C++ Practical Programming by Example.
[1] MP Allen and DJ Tildesley, Computer Simulation of Liquids.
[2] S Kim and S Karrila, Microhydrodynamics: Principles and Selected Applications.

*/
