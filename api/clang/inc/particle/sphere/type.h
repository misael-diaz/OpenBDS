#ifndef GUARD_OPENBDS_PARTICLE_SPHERE_TYPE_H
#define GUARD_OPENBDS_PARTICLE_SPHERE_TYPE_H

#include "bds/types.h"

// defines enum for selecting the logging level
enum SPHLOG
{
  SPH_LOG_LEVEL_DEFAULT,
  SPH_LOG_LEVEL_VERBOSE
};

typedef enum SPHLOG SPHLOG;

typedef struct __OBDS_PARTICLE_TYPE__ __OBDS_SPHERE_TYPE__ ;
typedef __OBDS_SPHERE_TYPE__ OBDS_Sphere_t;

// forward declares the underlying type of the Pseudo Random Number Generator `random_t'
struct random;

struct sphere
{
  OBDS_Sphere_t* props;			// properties
  struct random* prng;			// Pseudo Random Number Generator PRNG
  int (*update) (struct sphere*);	// updates the particles position and orientation
  int (*log) (const struct sphere* spheres, size_t const step);	// logs the positions
};

typedef struct sphere sphere_t;

#endif

/*

OpenBDS							September 05, 2023

source: particle/sphere/type.h
author: @misael-diaz

Synopsis:
Defines the sphere type and properties.

Copyright (c) 2023 Misael Diaz-Maldonado
This file is released under the GNU General Public License as published
by the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

References:
[0] A Koenig and B Moo, Accelerated C++ Practical Programming by Example.
[1] MP Allen and DJ Tildesley, Computer Simulation of Liquids.
[2] S Kim and S Karrila, Microhydrodynamics: Principles and Selected Applications.

*/
