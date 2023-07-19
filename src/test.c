#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

// defines the system size, or equivalently, the number of spheres
#define SIZE ( (size_t) 256 )

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
  // identifier:
  double* id;
  // container (this is what we allocate on the heap memory, the rest are just pointers)
  double* data;
} sphere_t;

sphere_t* create();
sphere_t* destroy(sphere_t*);

int main ()
{
  sphere_t* spheres = create();

  size_t const size = SIZE;
  const double* id = spheres -> id;
  for (size_t i = 0; i != size; ++i)
  {
    printf("id: %.0f \n", id[i]);
  }

  double f = 0;
  const double* f_x = spheres -> f_x;
  const double* f_y = spheres -> f_y;
  const double* f_z = spheres -> f_z;
  // sums the "net force" on the spheres (we expect zero owing to the initialization and
  // we are not computing the actual force magnitude via the squared root on purpose)
  for (size_t i = 0; i != size; ++i)
  {
    f += (f_x[i] * f_x[i]) + (f_y[i] * f_y[i]) + (f_z[i] * f_z[i]);
  }

  printf("f: %f \n", f);

  spheres = destroy(spheres);
  return 0;
}


void zeros (size_t size, double* x)
{
  for (size_t i = 0; i != size; ++i)
  {
    x[i] = 0.0;
  }
}


void iota (size_t size, double* x)
{
  for (size_t i = 0; i != size; ++i)
  {
    double const id = i;
    x[i] = id;
  }
}


sphere_t* create ()
{
  size_t const size_x = SIZE;
  size_t const size_y = SIZE;
  size_t const size_z = SIZE;
  size_t const size_f_x = SIZE;
  size_t const size_f_y = SIZE;
  size_t const size_f_z = SIZE;
  size_t const size_id = SIZE;
  size_t const size_data = size_x +
			     size_y +
			     size_z +
			     size_f_x +
			     size_f_y +
			     size_f_z +
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
  spheres -> id = spheres -> f_z + size_f_z;

  double* x = spheres -> x;
  double* y = spheres -> y;
  double* z = spheres -> z;
  double* f_x = spheres -> f_x;
  double* f_y = spheres -> f_y;
  double* f_z = spheres -> f_z;
  double* id = spheres -> id;

  zeros(size_x, x);
  zeros(size_y, y);
  zeros(size_z, z);
  zeros(size_f_x, f_x);
  zeros(size_f_y, f_y);
  zeros(size_f_z, f_z);
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

source: test.c
author: @misael-diaz

Synopsis:
Creates a system of non-interacting Brownian spheres and then destroys it.

Copyright (c) 2023 Misael Diaz-Maldonado
This file is released under the GNU General Public License as published
by the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

References:
[0] A Koenig and B Moo, Accelerated C++ Practical Programming by Example.
[1] MP Allen and DJ Tildesley, Computer Simulation of Liquids.
[2] S Kim and S Karrila, Microhydrodynamics: Principles and Selected Applications.

*/
