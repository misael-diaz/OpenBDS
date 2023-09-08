#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <errno.h>

#include "system/box.h"
#include "particles/sphere/params.h"
#include "particles/sphere/type.h"
#include "util/arrays.h"

#define STDC17 201710L
#define LIMIT ( (double) ( __OBDS_LIMIT__ ) )
#define SIZED ( (double) ( __OBDS_NUM_SPHERES__ ) )
#define TSTEP ( (double) ( __OBDS_TIME_STEP__ ) )
#define NUMEL ( (size_t) ( __OBDS_NUM_SPHERES__ ) )
#define RADIUS ( (double) ( __OBDS_SPH_RADIUS__ ) )
#define CONTACT ( (double) ( __OBDS_SPH_CONTACT__ ) )
#define MANTISSA ( (uint64_t) 0x000fffffffffffff )

static void grid (prop_t* restrict xprop, prop_t* restrict yprop, prop_t* restrict zprop)
{
  // gets the number of spheres (at contact) that can be fitted along any dimension:

  size_t const count = (LIMIT / RADIUS);
  size_t const count2 = (count * count);

  // sets particles at grid locations:

  double* x = &xprop[0].data;
  for (size_t n = 0; n != NUMEL; ++n)
  {
    size_t const i = (n % count);
    double const pos = RADIUS + CONTACT * ( (double) i );
    x[n] = pos;
  }

  double* y = &yprop[0].data;
  for (size_t n = 0; n != NUMEL; ++n)
  {
    size_t const i = (n % count2) / count;
    double const pos = RADIUS + CONTACT * ( (double) i );
    y[n] = pos;
  }

  double* z = &zprop[0].data;
  for (size_t n = 0; n != NUMEL; ++n)
  {
    size_t const i = (n / count2);
    double const pos = RADIUS + CONTACT * ( (double) i );
    z[n] = pos;
  }

  // shifts the particles so that their coordinates are in the range [-LIMIT, +LIMIT]:

  for (size_t i = 0; i != NUMEL; ++i)
  {
    x[i] -= LIMIT;
  }

  for (size_t i = 0; i != NUMEL; ++i)
  {
    y[i] -= LIMIT;
  }

  for (size_t i = 0; i != NUMEL; ++i)
  {
    z[i] -= LIMIT;
  }
}


static void updater (sphere_t* spheres)
{
  printf("x: %+.16e\n", spheres -> props -> x -> data);
}


// applies periodic boundary conditions PBCs on the position vectors
static void limiter (sphere_t* spheres)
{
  // NOTE: the torque is used as a temporary placeholder for now
  pbc(spheres -> props -> x, spheres -> props -> t_x, spheres -> props -> t_y);
  pbc(spheres -> props -> y, spheres -> props -> t_x, spheres -> props -> t_y);
  pbc(spheres -> props -> z, spheres -> props -> t_x, spheres -> props -> t_y);
}


static void logger (const sphere_t* spheres)
{
  printf("x: %+.16e\n", spheres -> props -> x -> data);
}


sphere_t* particles_sphere_initializer (void* data)
{
  // compile-time sane checks:

#if ( ( __GNUC__ > 12 ) && ( __STDC_VERSION__ > STDC17 ) )
  static_assert(sizeof(NUMEL) == 8);
  static_assert(sizeof(RADIUS) == 8);
  static_assert(sizeof(CONTACT) == 8);
  static_assert(sizeof(OBDS_Sphere_t) == 128);
  static_assert(sizeof(size_t) == sizeof(uint64_t));
  static_assert(sizeof(sphere_t) == 32);
  static_assert(sizeof(prop_t) == 8);
  static_assert(NUMEL != 0);
  static_assert(NUMEL % 2 == 0);
  static_assert(SIZE_MAX == UINT64_MAX);
  static_assert(NUMEL <= 0x7ffffffffffffffe);
  static_assert( SIZED * (RADIUS * RADIUS * RADIUS) < (LIMIT * LIMIT * LIMIT) );
  static_assert(CONTACT == 2.0);
  static_assert(RADIUS == 1.0);
#else
  _Static_assert(sizeof(NUMEL) == 8);
  _Static_assert(sizeof(RADIUS) == 8);
  _Static_assert(sizeof(CONTACT) == 8);
  _Static_assert(sizeof(OBDS_Sphere_t) == 128);
  _Static_assert(sizeof(size_t) == sizeof(uint64_t));
  _Static_assert(sizeof(sphere_t) == 32);
  _Static_assert(sizeof(prop_t) == 8);
  _Static_assert(NUMEL != 0);
  _Static_assert(NUMEL % 2 == 0);
  _Static_assert(SIZE_MAX == UINT64_MAX);
  _Static_assert(NUMEL <= 0x7fffffffffffffff);
  _Static_assert( SIZED * (RADIUS * RADIUS * RADIUS) < (LIMIT * LIMIT * LIMIT) );
  _Static_assert(CONTACT == 2.0);
  _Static_assert(RADIUS == 1.0);
#endif

  // runtime sane checks:

  prop_t const dt = { .data = TSTEP };
  uint64_t const bin = dt.bin;
  uint64_t const mantissa = (bin & MANTISSA);
  if (mantissa != 0)
  {
    errno = EINVAL;
    char err[] = "sphere-initializer() expects the time-step to be a power of two: %s\n";
    fprintf(stderr, err, strerror(errno));
#if ( ( __GNUC__ > 12 ) && ( __STDC_VERSION__ > STDC17 ) )
    return nullptr;
#else
    return NULL;
#endif
  }

  // bindinds:

  void* iter = data;
  sphere_t* spheres = (sphere_t*) iter;
  iter += sizeof(sphere_t);

  spheres -> props = (OBDS_Sphere_t*) iter;
  iter += sizeof(OBDS_Sphere_t);

  spheres -> props -> x = (prop_t*) iter;
  iter += NUMEL * sizeof(prop_t);
  spheres -> props -> y = (prop_t*) iter;
  iter += NUMEL * sizeof(prop_t);
  spheres -> props -> z = (prop_t*) iter;
  iter += NUMEL * sizeof(prop_t);

  spheres -> props -> r_x = (prop_t*) iter;
  iter += NUMEL * sizeof(prop_t);
  spheres -> props -> r_y = (prop_t*) iter;
  iter += NUMEL * sizeof(prop_t);
  spheres -> props -> r_z = (prop_t*) iter;
  iter += NUMEL * sizeof(prop_t);

  spheres -> props -> a_x = (prop_t*) iter;
  iter += NUMEL * sizeof(prop_t);
  spheres -> props -> a_y = (prop_t*) iter;
  iter += NUMEL * sizeof(prop_t);
  spheres -> props -> a_z = (prop_t*) iter;
  iter += NUMEL * sizeof(prop_t);

  spheres -> props -> f_x = (prop_t*) iter;
  iter += NUMEL * sizeof(prop_t);
  spheres -> props -> f_y = (prop_t*) iter;
  iter += NUMEL * sizeof(prop_t);
  spheres -> props -> f_z = (prop_t*) iter;
  iter += NUMEL * sizeof(prop_t);

  spheres -> props -> t_x = (prop_t*) iter;
  iter += NUMEL * sizeof(prop_t);
  spheres -> props -> t_y = (prop_t*) iter;
  iter += NUMEL * sizeof(prop_t);
  spheres -> props -> t_z = (prop_t*) iter;
  iter += NUMEL * sizeof(prop_t);

  spheres -> props -> id = (prop_t*) iter;
  iter += NUMEL * sizeof(prop_t);

  spheres -> update = updater;
  spheres -> limit = limiter;
  spheres -> log = logger;

  prop_t* x = spheres -> props -> x;
  prop_t* y = spheres -> props -> y;
  prop_t* z = spheres -> props -> z;
  prop_t* r_x = spheres -> props -> r_x;
  prop_t* r_y = spheres -> props -> r_y;
  prop_t* r_z = spheres -> props -> r_z;
  prop_t* a_x = spheres -> props -> a_x;
  prop_t* a_y = spheres -> props -> a_y;
  prop_t* a_z = spheres -> props -> a_z;
  prop_t* f_x = spheres -> props -> f_x;
  prop_t* f_y = spheres -> props -> f_y;
  prop_t* f_z = spheres -> props -> f_z;
  prop_t* t_x = spheres -> props -> t_x;
  prop_t* t_y = spheres -> props -> t_y;
  prop_t* t_z = spheres -> props -> t_z;
  prop_t* id = spheres -> props -> id;

  // initializations:

  zeros(x);
  zeros(y);
  zeros(z);
  zeros(r_x);
  zeros(r_y);
  zeros(r_z);
  zeros(a_x);
  zeros(a_y);
  zeros(a_z);
  zeros(f_x);
  zeros(f_y);
  zeros(f_z);
  zeros(t_x);
  zeros(t_y);
  zeros(t_z);
  iota(id);

  grid(x, y, z);

  return spheres;
}


/*

OpenBDS							September 06, 2023

source: particles/sphere/sphere.c
author: @misael-diaz

Synopsis:
Implements methods for spheres.

Copyright (c) 2023 Misael Diaz-Maldonado
This file is released under the GNU General Public License as published
by the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

References:
[0] A Koenig and B Moo, Accelerated C++ Practical Programming by Example.
[1] MP Allen and DJ Tildesley, Computer Simulation of Liquids.
[2] S Kim and S Karrila, Microhydrodynamics: Principles and Selected Applications.

*/
