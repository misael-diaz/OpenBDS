#ifndef GUARD_OPENBDS_PARTICLES_SPHERE_TYPE_H
#define GUARD_OPENBDS_PARTICLES_SPHERE_TYPE_H

#include "bds/types.h"

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
  // position subject to periodic boundaries:
  prop_t* x;
  prop_t* y;
  prop_t* z;
  // absolute position:
  prop_t* r_x;
  prop_t* r_y;
  prop_t* r_z;
  // angular position:
  prop_t* a_x;
  prop_t* a_y;
  prop_t* a_z;
  // force:
  prop_t* f_x;
  prop_t* f_y;
  prop_t* f_z;
  // torque:
  prop_t* t_x;
  prop_t* t_y;
  prop_t* t_z;
  // identifier:
  prop_t* id;
};

typedef struct __OBDS_SPHERE_TYPE__ OBDS_Sphere_t;

struct sphere
{
  OBDS_Sphere_t* props;			// properties
  void (*update) (struct sphere*);	// updates the particles position and orientation
  void (*limit) (struct sphere*);	// limits the particles to the system boundaries
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
