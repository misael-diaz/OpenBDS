#ifndef GUARD_OPENBDS_SPHERE_H
#define GUARD_OPENBDS_SPHERE_H

typedef struct
{
  // position:
  double* x;
  double* y;
  double* z;
  // force:
  double* f_x;
  double* f_y;
  double* f_z;
  // torque:
  double* t_x;
  double* t_y;
  double* t_z;
  // neighbor-list
  double* list;
  // identifier:
  double* id;
  // container (this is what we allocate on the heap memory, the rest are just pointers)
  double* data;
} sphere_t;

sphere_t* create();
sphere_t* destroy(sphere_t*);

#endif

/*

OpenBDS								July 19, 2023

source: sphere.h
author: @misael-diaz

Synopsis:
Defines the sphere object and memory handling methods.

Copyright (c) 2023 Misael Diaz-Maldonado
This file is released under the GNU General Public License as published
by the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

References:
[0] A Koenig and B Moo, Accelerated C++ Practical Programming by Example.
[1] MP Allen and DJ Tildesley, Computer Simulation of Liquids.
[2] S Kim and S Karrila, Microhydrodynamics: Principles and Selected Applications.

*/
