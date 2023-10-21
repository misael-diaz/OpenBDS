#ifndef GUARD_OPENBDS_PARTICLE_PARAMS_H
#define GUARD_OPENBDS_PARTICLE_PARAMS_H

#include "system/params.h"

/* stands for the sqrt(2) */
#define __OBDS_SQRT_OF_TWO__ 1.4142135623730951

/* stands for the sqrt(3/2) */
#define __OBDS_SQRT_OF_THREE_HALVES__ 1.2247448713915890

/* ``effective'' translational mobility of a hydrodynamically isotropic particle under the
 * action of Brownian (or stochastic) forces */
#define __OBDS_ISOTROPIC_LINEAR_BROWNIAN_MOBILITY__ (\
	( (double) ( __OBDS_SQRT_OF_TWO__ ) ) *\
	( (double) ( __OBDS_SQRT_TIME_STEP__ ) )\
)

/* ``effective'' rotational mobility of a hydrodynamically isotropic particle under the
 * action of Brownian (or stochastic) torques */
#define __OBDS_ISOTROPIC_ANGULAR_BROWNIAN_MOBILITY__ (\
	( (double) ( __OBDS_SQRT_OF_THREE_HALVES__ ) ) *\
	( (double) ( __OBDS_SQRT_TIME_STEP__ ) )\
)

/* ``effective'' translational mobility of a hydrodynamically isotropic particle under the
 * action of deterministic (or conservative) forces */
#define __OBDS_ISOTROPIC_LINEAR_MOBILITY__  ( (double) ( __OBDS_TIME_STEP__ ) )

#endif

/*

OpenBDS							September 05, 2023

source: particle/params.h
author: @misael-diaz

Synopsis:
Defines particle parameters.

Copyright (c) 2023 Misael Diaz-Maldonado
This file is released under the GNU General Public License as published
by the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

References:
[0] A Koenig and B Moo, Accelerated C++ Practical Programming by Example.
[1] MP Allen and DJ Tildesley, Computer Simulation of Liquids.
[2] S Kim and S Karrila, Microhydrodynamics: Principles and Selected Applications.

*/
