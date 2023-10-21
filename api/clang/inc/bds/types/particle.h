#ifndef GUARD_OPENBDS_BDS_TYPES_PARTICLE_TYPE_H
#define GUARD_OPENBDS_BDS_TYPES_PARTICLE_TYPE_H

#include <stdint.h>

// forward declares the OBDS property type
union __OBDS_PROP_TYPE__ ;

// defines ``base class'' for the particles (in the Object-Oriented Programming OOP sense)
struct __OBDS_PARTICLE_TYPE__
{
  // position vector subject to periodic boundaries:
  union __OBDS_PROP_TYPE__* x;
  union __OBDS_PROP_TYPE__* y;
  union __OBDS_PROP_TYPE__* z;
  // absolute position vector:
  union __OBDS_PROP_TYPE__* r_x;
  union __OBDS_PROP_TYPE__* r_y;
  union __OBDS_PROP_TYPE__* r_z;
  // displacement vector:
  union __OBDS_PROP_TYPE__* _dx;
  union __OBDS_PROP_TYPE__* _dy;
  union __OBDS_PROP_TYPE__* _dz;
  // angular position vector:
  union __OBDS_PROP_TYPE__* a_x;
  union __OBDS_PROP_TYPE__* a_y;
  union __OBDS_PROP_TYPE__* a_z;
  // director (or orientation vector):
  union __OBDS_PROP_TYPE__* d_x;
  union __OBDS_PROP_TYPE__* d_y;
  union __OBDS_PROP_TYPE__* d_z;
  // force:
  union __OBDS_PROP_TYPE__* f_x;
  union __OBDS_PROP_TYPE__* f_y;
  union __OBDS_PROP_TYPE__* f_z;
  // torque:
  union __OBDS_PROP_TYPE__* t_x;
  union __OBDS_PROP_TYPE__* t_y;
  union __OBDS_PROP_TYPE__* t_z;
  // placeholders:
  union __OBDS_PROP_TYPE__* tmp;
  union __OBDS_PROP_TYPE__* temp;
  union __OBDS_PROP_TYPE__* bitmask;
  // neighbor-list:
  union __OBDS_PROP_TYPE__* list;
  // identifier:
  union __OBDS_PROP_TYPE__* id;
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

OpenBDS							October 15, 2023

source: bds/types/particle.h
author: @misael-diaz

Synopsis:
Defines the OpenBDS particle type.

Copyright (c) 2023 Misael Diaz-Maldonado
This file is released under the GNU General Public License as published
by the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

References:
[0] A Koenig and B Moo, Accelerated C++ Practical Programming by Example.
[1] MP Allen and DJ Tildesley, Computer Simulation of Liquids.
[2] S Kim and S Karrila, Microhydrodynamics: Principles and Selected Applications.

*/
