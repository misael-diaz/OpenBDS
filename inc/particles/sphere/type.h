#ifndef GUARD_OPENBDS_PARTICLES_SPHERE_TYPE_H
#define GUARD_OPENBDS_PARTICLES_SPHERE_TYPE_H

#include "bds/types.h"
#include "util/random/type.h"

// defines enum for selecting the logging level
enum SPHLOG
{
  SPH_LOG_LEVEL_DEFAULT,
  SPH_LOG_LEVEL_VERBOSE
};

typedef enum SPHLOG SPHLOG;

// defines the underlying properties of the sphere type:
struct __OBDS_SPHERE_TYPE__
{
  // position vector subject to periodic boundaries:
  prop_t* x;
  prop_t* y;
  prop_t* z;
  // absolute position vector:
  prop_t* r_x;
  prop_t* r_y;
  prop_t* r_z;
  // displacement vector:
  prop_t* _dx;
  prop_t* _dy;
  prop_t* _dz;
  // angular position vector:
  prop_t* a_x;
  prop_t* a_y;
  prop_t* a_z;
  // director (or orientation vector):
  prop_t* d_x;
  prop_t* d_y;
  prop_t* d_z;
  // force:
  prop_t* f_x;
  prop_t* f_y;
  prop_t* f_z;
  // torque:
  prop_t* t_x;
  prop_t* t_y;
  prop_t* t_z;
  // placeholders:
  prop_t* tmp;
  prop_t* temp;
  prop_t* bitmask;
  // neighbor-list:
  prop_t* list;
  // identifier:
  prop_t* id;
  // padding:
  uint64_t: 64;
  uint64_t: 64;
  uint64_t: 64;
  uint64_t: 64;
  uint64_t: 64;
  uint64_t: 64;
};

typedef struct __OBDS_SPHERE_TYPE__ OBDS_Sphere_t;

struct sphere
{
  OBDS_Sphere_t* props;			// properties
  random_t* prng;			// Pseudo Random Number Generator PRNG
  int (*update) (struct sphere*);	// updates the particles position and orientation
  int (*log) (const struct sphere* spheres, size_t const step);	// logs the positions
};

typedef struct sphere sphere_t;

#endif

/*

OpenBDS							September 05, 2023

source: particles/sphere/type.h
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
