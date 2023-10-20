#ifndef GUARD_OPENBDS_PARTICLE_SPHERE_PARAMS_H
#define GUARD_OPENBDS_PARTICLE_SPHERE_PARAMS_H

#include <stdbool.h>
#include "system/params.h"

// true if we are dealing with spheres, false otherwise
#define __OBDS_SPH__ ( (bool) (FCONF_ISOTROPIC_HYDRODYNAMIC_RESISTANCE_PARTICLE) )

// sqrt(2)
#define __OBDS_SPH_SQRT_OF_TWO__ 1.4142135623730951

// sqrt(3 / 2)
#define __OBDS_SPH_SQRT_OF_THREE_HALVES__ 1.2247448713915890

// sqrt(2 * dt), where `dt' is the OBDS time-step; this quantity times the Brownian
// force yields a change in the particle position
#define __OBDS_SPH_LIN_BROWNIAN_MOBILITY__ (\
	( (double) ( __OBDS_SPH_SQRT_OF_TWO__ ) ) *\
	( (double) ( __OBDS_SQRT_TIME_STEP__ ) )\
)

// sqrt(3 * dt / 2), where `dt' is the OBDS time-step; this quantity times the Brownian
// torque yields a change in the particle orientation
#define __OBDS_SPH_ANG_BROWNIAN_MOBILITY__ (\
	( (double) ( __OBDS_SPH_SQRT_OF_THREE_HALVES__ ) ) *\
	( (double) ( __OBDS_SQRT_TIME_STEP__ ) )\
)

// OBDS time-step, `dt'; this quantity times the interparticle force yields a change
// in the particle position
#define __OBDS_SPH_LIN_MOBILITY__  ( (double) ( __OBDS_TIME_STEP__ ) )

// sphere radius, diameter, and contact-distance
#define __OBDS_SPH_RADIUS__ ( (double) 1.0 )
#define __OBDS_SPH_DIAMETER__ ( (double) ( 2.0 * __OBDS_SPH_RADIUS__ ) )
#define __OBDS_SPH_CONTACT__ ( (double) ( __OBDS_SPH_DIAMETER__ ) )

// interaction range (pairs beyond this distance experience zero interparticle force)
#define __OBDS_SPH_RANGE__ ( (double) ( 4.0 * ( __OBDS_SPH_CONTACT__ ) ) )

// non-dimensional energy
#define __OBDS_SPH_EPSILON__ ( (double) 1.0 )

// number of spheres, `N'
#define __OBDS_NUM_SPHERES__ ( (size_t) ( __OBDS_NUM_PARTICLES__ ) )

// log(N) / log(2), base two logarithm of the number of spheres
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
