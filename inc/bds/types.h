#ifndef GUARD_OPENBDS_BDS_TYPES_H
#define GUARD_OPENBDS_BDS_TYPES_H

#include <stdint.h>

union __OBDS_PROP_TYPE__
{
  double data;
  uint64_t bin;
};

typedef union __OBDS_PROP_TYPE__ prop_t;

struct __OBDS_PARTICLE_TYPE__
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

typedef struct __OBDS_PARTICLE_TYPE__ particle_t;

#endif

/*

OpenBDS							September 07, 2023

source: bds/types.h
author: @misael-diaz

Synopsis:
Defines the OpenBDS property type.
Sometimes we operate on floats as if they were integers so this definition is quite handy.

Copyright (c) 2023 Misael Diaz-Maldonado
This file is released under the GNU General Public License as published
by the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

References:
[0] A Koenig and B Moo, Accelerated C++ Practical Programming by Example.
[1] MP Allen and DJ Tildesley, Computer Simulation of Liquids.
[2] S Kim and S Karrila, Microhydrodynamics: Principles and Selected Applications.

*/
