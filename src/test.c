#include <stdio.h>
#include "sphere.h"

// defines the system size, or equivalently, the number of spheres
#define SIZE ( (size_t) 256 )

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
