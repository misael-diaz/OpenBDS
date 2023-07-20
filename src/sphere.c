#include <stdio.h>
#include "system.h"
#include "sphere.h"
#include "util.h"

#define SIZE NUM_SPHERES

sphere_t* create ()
{
  size_t const size_x = SIZE;
  size_t const size_y = SIZE;
  size_t const size_z = SIZE;
  size_t const size_f_x = SIZE;
  size_t const size_f_y = SIZE;
  size_t const size_f_z = SIZE;
  size_t const size_t_x = SIZE;
  size_t const size_t_y = SIZE;
  size_t const size_t_z = SIZE;
  size_t const size_list = SIZE;
  size_t const size_id = SIZE;
  size_t const size_data = size_x +
			     size_y +
			     size_z +
			     size_f_x +
			     size_f_y +
			     size_f_z +
			     size_t_x +
			     size_t_y +
			     size_t_z +
			     size_list +
			     size_id;

  size_t const bytes = size_data * sizeof(double);
  double* data = malloc(bytes);
  if (data == NULL)
  {
    printf("create(): failed to allocate memory to store the sphere data\n");
    return NULL;
  }

  sphere_t* spheres = malloc( sizeof(sphere_t) );
  if (spheres == NULL)
  {
    free(data);
    data = NULL;
    printf("create(): failed to allocate memory for the sphere data type\n");
    return NULL;
  }

  spheres -> data = data;
  data = NULL;

  spheres -> x = spheres -> data;
  spheres -> y = spheres -> x + size_x;
  spheres -> z = spheres -> y + size_y;
  spheres -> f_x = spheres -> z + size_z;
  spheres -> f_y = spheres -> f_x + size_f_x;
  spheres -> f_z = spheres -> f_y + size_f_y;
  spheres -> t_x = spheres -> f_z + size_f_z;
  spheres -> t_y = spheres -> t_x + size_t_x;
  spheres -> t_z = spheres -> t_y + size_t_y;
  spheres -> list = spheres -> t_z + size_t_z;
  spheres -> id = spheres -> list + size_list;

  double* x = spheres -> x;
  double* y = spheres -> y;
  double* z = spheres -> z;
  double* f_x = spheres -> f_x;
  double* f_y = spheres -> f_y;
  double* f_z = spheres -> f_z;
  double* t_x = spheres -> t_x;
  double* t_y = spheres -> t_y;
  double* t_z = spheres -> t_z;
  double* list = spheres -> list;
  double* id = spheres -> id;

  zeros(size_x, x);
  zeros(size_y, y);
  zeros(size_z, z);
  zeros(size_f_x, f_x);
  zeros(size_f_y, f_y);
  zeros(size_f_z, f_z);
  zeros(size_t_x, t_x);
  zeros(size_t_y, t_y);
  zeros(size_t_z, t_z);
  zeros(size_list, list);
  iota(size_id, id);

  return spheres;
}


sphere_t* destroy (sphere_t* spheres)
{
  if (spheres == NULL)
  {
    return NULL;
  }

  free(spheres -> data);
  spheres -> data = NULL;
  free(spheres);
  spheres = NULL;
  return spheres;
}


/*

OpenBDS								July 19, 2023

source: sphere.c
author: @misael-diaz

Synopsis:
Implements memory handling methods for spheres.

Copyright (c) 2023 Misael Diaz-Maldonado
This file is released under the GNU General Public License as published
by the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

References:
[0] A Koenig and B Moo, Accelerated C++ Practical Programming by Example.
[1] MP Allen and DJ Tildesley, Computer Simulation of Liquids.
[2] S Kim and S Karrila, Microhydrodynamics: Principles and Selected Applications.

*/
