#include <stdio.h>
#include <stdint.h>
#include <math.h>

#include "sphere.h"
#include "system.h"
#include "util.h"

// defines the system size, or equivalently, the number of spheres
#define SIZE ( (size_t) NUM_SPHERES )

typedef union
{
  uint64_t bin;
  double data;
} alias_t;

void test_init();
void test_partition_masking();
void test_unlimited_masking();
void test_pbc();

int main ()
{
  test_init();
  test_partition_masking();
  test_unlimited_masking();
  test_pbc();
  return 0;
}


void test_init ()
{
  sphere_t* spheres = create();

  size_t const size = SIZE;
  const int64_t* id = spheres -> id;
  for (size_t i = 0; i != size; ++i)
  {
    printf("id: %ld \n", id[i]);
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
}


void test_partition_masking ()
{
  size_t const numel = SIZE;	// number of elements (also number of particles)
  double x[numel];		// particles x-axis coordinates
  double mask[numel];		// bitmask (non-zero for particles beyond system limits)

  double const x_min = -1.25 * LIMIT;
  double const x_max = +1.25 * LIMIT;
  // simulates the presence of (some) particles exceeding the system dimensions
  for (size_t i = 0; i != numel; ++i)
  {
    x[i] = x_min + ( (double) rand() / RAND_MAX ) * (x_max - x_min);
  }

  // scaling so that x ~ 1 (it's really not required to do the scaling for this test)
  for (size_t i = 0; i != numel; ++i)
  {
    x[i] /= (0.5 * LENGTH);
  }

  size_t count = 0;
  // counts number of particles in left partition via signbit() MACRO
  for (size_t i = 0; i != numel; ++i)
  {
    if ( signbit(x[i]) )
    {
      ++count;
    }
  }

  // masks particles in the right partition so that we work on those in the left:

  mask_partition(numel, x, mask);

  // counts the number of particles in the left partition to test the masking algorithm:

  alias_t* m = mask;
  for (size_t i = 0; i != numel; ++i)
  {
    m[i].bin &= 1;
  }

  size_t sum = 0;
  for (size_t i = 0; i != numel; ++i)
  {
    sum += m[i].bin;
  }

  printf("mask-partition-test[0]: ");
  if (sum != count)
  {
    printf("FAIL\n");
  }
  else
  {
    printf("PASS\n");
  }
}


// tests the application of periodic boundary conditions via bitwise operations
void test_unlimited_masking ()
{
  size_t const numel = SIZE;	// number of elements (also number of particles)
  double x[numel];		// particles x-axis coordinates
  double temp[numel];		// array temporary
  double mask[numel];		// bitmask for partitioning
  double bitmask[numel];	// bitmask (non-zero for particles beyond system limits)
  double offset[numel];		// applies offset to bound unlimited particles

  double const x_min = -1.25 * LIMIT;
  double const x_max = +1.25 * LIMIT;
  // simulates the presence of (some) particles exceeding the system dimensions
  for (size_t i = 0; i != numel; ++i)
  {
    x[i] = x_min + ( (double) rand() / RAND_MAX ) * (x_max - x_min);
  }

  // scaling so that abs(x) = [0, 2.0) (note: the masking algorithm expects this scaling)
  for (size_t i = 0; i != numel; ++i)
  {
    x[i] /= (0.5 * LENGTH);
  }

  // masks particles in the right partition:

  mask_partition(numel, x, mask);
  mask_unlimited(numel, x, mask, temp, bitmask);

  // applies periodic boundary conditions (for particles whose x < -1.0):

  alias_t* o = offset;
  const alias_t* b = bitmask;
  alias_t  l = { .data = 1.0 };
  for (size_t i = 0; i != numel; ++i)
  {
    o[i].bin = (b[i].bin & l.bin);
  }

  for (size_t i = 0; i != numel; ++i)
  {
    x[i] += offset[i];
  }

  // masks particles in the left partition:

  alias_t* m = mask;
  for (size_t i = 0; i != numel; ++i)
  {
    m[i].bin = ~m[i].bin;
  }

  mask_unlimited(numel, x, mask, temp, bitmask);

  l.data = -1.0;
  for (size_t i = 0; i != numel; ++i)
  {
    o[i].bin = (b[i].bin & l.bin);
  }

  // applies periodic boundary conditions (for particles whose x > +1.0):

  for (size_t i = 0; i != numel; ++i)
  {
    x[i] += offset[i];
  }

  // counts the number of remaining unlimited particles (we expect none):

  size_t count = 0;
  for (size_t i = 0; i != numel; ++i)
  {
    if (x[i] < -1.0 || x[i] > 1.0)
    {
      ++count;
    }
  }

  printf("mask-unlimited-test[0]: ");
  if (count != 0)
  {
    printf("FAIL\n");
  }
  else
  {
    printf("PASS\n");
  }

  // scales back so that x = [-LIMIT, +LIMIT] for all particles
  for (size_t i = 0; i != numel; ++i)
  {
    x[i] *= (0.5 * LENGTH);
  }
}


void test_pbc ()
{
  size_t const numel = SIZE;	// number of elements (also number of particles)
  double x[numel];		// particles x-axis coordinates
  double temp[numel];		// array temporary
  double mask[numel];		// bitmask for partitioning
  double bitmask[numel];	// bitmask (non-zero for particles beyond system limits)

  double const x_min = -1.25 * LIMIT;
  double const x_max = +1.25 * LIMIT;
  // simulates the presence of (some) particles exceeding the system dimensions
  for (size_t i = 0; i != numel; ++i)
  {
    x[i] = x_min + ( (double) rand() / RAND_MAX ) * (x_max - x_min);
  }

  pbc(x, temp, mask, bitmask);

  // counts the number of remaining unlimited particles (we expect none):

  size_t count = 0;
  for (size_t i = 0; i != numel; ++i)
  {
    if (x[i] < -LIMIT || x[i] > +LIMIT)
    {
      ++count;
    }
  }

  printf("pbc-test[0]: ");
  if (count != 0)
  {
    printf("FAIL\n");
  }
  else
  {
    printf("PASS\n");
  }
}


/*

OpenBDS								July 19, 2023

source: test.c
author: @misael-diaz

Synopsis:
Tests the lower-level code that supports the Brownian Dynamics Simulator BDS.

Copyright (c) 2023 Misael Diaz-Maldonado
This file is released under the GNU General Public License as published
by the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

References:
[0] A Koenig and B Moo, Accelerated C++ Practical Programming by Example.
[1] MP Allen and DJ Tildesley, Computer Simulation of Liquids.
[2] S Kim and S Karrila, Microhydrodynamics: Principles and Selected Applications.

*/
