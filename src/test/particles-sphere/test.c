#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>
#include <errno.h>
#include <stdio.h>

#include "system/box/params.h"
#include "particles/sphere/params.h"
#include "particles/sphere/utils.h"

#define STDC17 201710L
#define IOERR ( (int) 0xffffffff )
#define LIMIT ( (double) ( __OBDS_LIMIT__ ) )
#define TSTEP ( (double) ( __OBDS_TIME_STEP__ ) )
#define STEPS ( 256LU * ( (size_t) (1.0 / TSTEP) ) )
#define NUMEL ( (size_t) ( __OBDS_NUM_SPHERES__ ) )
#define PROPS ( (size_t) ( 16 * ( NUMEL ) ) )
#define DEFAULT SPH_LOG_LEVEL_DEFAULT
#define VERBOSE SPH_LOG_LEVEL_VERBOSE
#define LOG(x) ( (bool) ( ( x ) % ( (size_t) ( 0.001953125 / TSTEP ) ) == 0 ) )

void test(void);
void test1(void);

int main ()
{
//test();
  test1();
  return 0;
}


#if ( ( __GNUC__ > 12 ) && ( __STDC_VERSION__ > STDC17 ) )
void test (void)
{
  static_assert(sizeof(sphere_t) == 32);
  static_assert(sizeof(OBDS_Sphere_t) == 128);
  static_assert(sizeof(prop_t) == 8);
  constexpr size_t sz = sizeof(sphere_t) +
			sizeof(OBDS_Sphere_t) +
			PROPS * sizeof(prop_t) +
			sizeof(random_t) +
			sizeof(generator_t) +
			sizeof(double) +
			sizeof(uint64_t);
  void* workspace = malloc(sz);
  if (workspace == nullptr)
  {
    fprintf(stderr, "test-spheres(): memory allocation error %s\n", strerror(errno));

    return;
  }

  SPHLOG lvl = DEFAULT;
  sphere_t* spheres = particles_sphere_initializer(workspace, lvl);
  if (spheres == nullptr)
  {
    free(workspace);
    workspace = nullptr;
    fprintf(stderr, "test-spheres(): ERROR\n");
    return;
  }

  double* x = &(spheres -> props -> x -> data);
  for (size_t i = 0; i != NUMEL; ++i)
  {
    *x = 1.0009765625 * LIMIT;
    ++x;
  }

  double* y = &(spheres -> props -> y -> data);
  for (size_t i = 0; i != NUMEL; ++i)
  {
    *y = 1.0009765625 * LIMIT;
    ++y;
  }

  double* z = &(spheres -> props -> z -> data);
  for (size_t i = 0; i != NUMEL; ++i)
  {
    *z = 1.0009765625 * LIMIT;
    ++z;
  }

  spheres -> update(spheres);

  bool failed = false;
  x = &(spheres -> props -> x -> data);
  for (size_t i = 0; i != NUMEL; ++i)
  {
    if (x[i] < -LIMIT || x[i] > +LIMIT)
    {
      failed = true;
      break;
    }
  }

  y = &(spheres -> props -> y -> data);
  for (size_t i = 0; i != NUMEL; ++i)
  {
    if (y[i] < -LIMIT || y[i] > +LIMIT)
    {
      failed = true;
      break;
    }
  }

  z = &(spheres -> props -> z -> data);
  for (size_t i = 0; i != NUMEL; ++i)
  {
    if (z[i] < -LIMIT || z[i] > +LIMIT)
    {
      failed = true;
      break;
    }
  }

  printf("test-sphere[0]: ");
  if (failed)
  {
    printf("FAIL\n");
  }
  else
  {
    printf("PASS\n");
  }

  free(workspace);
  workspace = nullptr;
}
#else
void test (void)
{
  _Static_assert(sizeof(sphere_t) == 32);
  _Static_assert(sizeof(OBDS_Sphere_t) == 128);
  _Static_assert(sizeof(prop_t) == 8);
  size_t const sz = sizeof(sphere_t) +
		    sizeof(OBDS_Sphere_t) +
		    PROPS * sizeof(prop_t) +
		    sizeof(random_t) +
		    sizeof(generator_t) +
		    sizeof(double) +
		    sizeof(uint64_t);
  void* workspace = malloc(sz);
  if (workspace == NULL)
  {
    fprintf(stderr, "test-spheres(): memory allocation error %s\n", strerror(errno));
    return;
  }

  SPHLOG lvl = VERBOSE;
  sphere_t* spheres = particles_sphere_initializer(workspace, lvl);
  if (spheres == NULL)
  {
    free(workspace);
    workspace = NULL;
    fprintf(stderr, "test-spheres(): ERROR\n");
    return;
  }


  double* x = &(spheres -> props -> x -> data);
  for (size_t i = 0; i != NUMEL; ++i)
  {
    *x = 1.0009765625 * LIMIT;
    ++x;
  }

  double* y = &(spheres -> props -> y -> data);
  for (size_t i = 0; i != NUMEL; ++i)
  {
    *y = 1.0009765625 * LIMIT;
    ++y;
  }

  double* z = &(spheres -> props -> z -> data);
  for (size_t i = 0; i != NUMEL; ++i)
  {
    *z = 1.0009765625 * LIMIT;
    ++z;
  }

  spheres -> update(spheres);

  bool failed = false;
  x = &(spheres -> props -> x -> data);
  for (size_t i = 0; i != NUMEL; ++i)
  {
    if (x[i] < -LIMIT || x[i] > +LIMIT)
    {
      failed = true;
      break;
    }
  }

  y = &(spheres -> props -> y -> data);
  for (size_t i = 0; i != NUMEL; ++i)
  {
    if (y[i] < -LIMIT || y[i] > +LIMIT)
    {
      failed = true;
      break;
    }
  }

  z = &(spheres -> props -> z -> data);
  for (size_t i = 0; i != NUMEL; ++i)
  {
    if (z[i] < -LIMIT || z[i] > +LIMIT)
    {
      failed = true;
      break;
    }
  }

  printf("test-sphere[0]: ");
  if (failed)
  {
    printf("FAIL\n");
  }
  else
  {
    printf("PASS\n");
  }

  free(workspace);
  workspace = NULL;
}
#endif


#if ( ( __GNUC__ > 12 ) && ( __STDC_VERSION__ > STDC17 ) )
void test1 (void)
{
  static_assert(sizeof(sphere_t) == 32);
  static_assert(sizeof(OBDS_Sphere_t) == 128);
  static_assert(sizeof(prop_t) == 8);
  constexpr size_t sz = sizeof(sphere_t) +
			sizeof(OBDS_Sphere_t) +
			PROPS * sizeof(prop_t) +
			sizeof(random_t) +
			sizeof(generator_t) +
			sizeof(double) +
			sizeof(uint64_t);
  void* workspace = malloc(sz);
  if (workspace == nullptr)
  {
    fprintf(stderr, "test-spheres1(): memory allocation error %s\n", strerror(errno));
    return;
  }

  SPHLOG lvl = VERBOSE;
  sphere_t* spheres = particles_sphere_initializer(workspace, lvl);
  if (spheres == nullptr)
  {
    free(workspace);
    workspace = nullptr;
    fprintf(stderr, "test-spheres(): ERROR\n");
    return;
  }

  if (spheres -> log(spheres, 0) == IOERR)
  {
    free(workspace);
    workspace = nullptr;
    fprintf(stderr, "test1() ERROR\n");
    return;
  }

  double* x = &(spheres -> props -> x -> data);
  double* y = &(spheres -> props -> y -> data);
  double* z = &(spheres -> props -> z -> data);

  bool failed = false;
  size_t const steps = STEPS;
  for (size_t step = 0; step != steps; ++step)
  {
    if (spheres -> update(spheres) != 0)
    {
      fprintf(stderr, "test1(): ERROR\n");
      free(workspace);
      workspace = nullptr;
      return;
    }

    if ( LOG(step + 1) )
    {
      if (spheres -> log(spheres, step + 1) == IOERR)
      {
	fprintf(stderr, "test1(): UNEXPECTED IO ERROR\n");
	free(workspace);
	workspace = nullptr;
	return;
      }
    }

    for (size_t i = 0; i != NUMEL; ++i)
    {
      if (x[i] < -LIMIT || x[i] > +LIMIT)
      {
	failed = true;
	break;
      }
    }

    for (size_t i = 0; i != NUMEL; ++i)
    {
      if (y[i] < -LIMIT || y[i] > +LIMIT)
      {
	failed = true;
	break;
      }
    }

    for (size_t i = 0; i != NUMEL; ++i)
    {
      if (z[i] < -LIMIT || z[i] > +LIMIT)
      {
	failed = true;
	break;
      }
    }

    if (failed)
    {
      break;
    }
  }

  printf("test-sphere[1]: ");
  if (failed)
  {
    printf("FAIL\n");
  }
  else
  {
    printf("PASS\n");
  }

  free(workspace);
  workspace = nullptr;
}
#else
void test1 (void)
{
  _Static_assert(sizeof(sphere_t) == 32);
  _Static_assert(sizeof(OBDS_Sphere_t) == 128);
  _Static_assert(sizeof(prop_t) == 8);
  size_t const sz = sizeof(sphere_t) +
		    sizeof(OBDS_Sphere_t) +
		    PROPS * sizeof(prop_t) +
		    sizeof(random_t) +
		    sizeof(generator_t) +
		    sizeof(double) +
		    sizeof(uint64_t);
  void* workspace = malloc(sz);
  if (workspace == NULL)
  {
    fprintf(stderr, "test-spheres1(): memory allocation error %s\n", strerror(errno));
    return;
  }

  SPHLOG lvl = VERBOSE;
  sphere_t* spheres = particles_sphere_initializer(workspace, lvl);
  if (spheres == NULL)
  {
    free(workspace);
    workspace = NULL;
    fprintf(stderr, "test-spheres(): ERROR\n");
    return;
  }

  if (spheres -> log(spheres, 0) == IOERR)
  {
    free(workspace);
    workspace = NULL;
    fprintf(stderr, "test1() ERROR\n");
    return;
  }

  double* x = &(spheres -> props -> x -> data);
  double* y = &(spheres -> props -> y -> data);
  double* z = &(spheres -> props -> z -> data);

  bool failed = false;
  size_t const steps = STEPS;
  for (size_t step = 0; step != steps; ++step)
  {
    if (spheres -> update(spheres) != 0)
    {
      fprintf(stderr, "test1(): ERROR\n");
      free(workspace);
      workspace = NULL;
      return;
    }

    if ( LOG(step + 1) )
    {
      if (spheres -> log(spheres, step + 1) == IOERR)
      {
	fprintf(stderr, "test1(): UNEXPECTED IO ERROR\n");
	free(workspace);
	workspace = NULL;
	return;
      }
    }

    for (size_t i = 0; i != NUMEL; ++i)
    {
      if (x[i] < -LIMIT || x[i] > +LIMIT)
      {
	failed = true;
	break;
      }
    }

    for (size_t i = 0; i != NUMEL; ++i)
    {
      if (y[i] < -LIMIT || y[i] > +LIMIT)
      {
	failed = true;
	break;
      }
    }

    for (size_t i = 0; i != NUMEL; ++i)
    {
      if (z[i] < -LIMIT || z[i] > +LIMIT)
      {
	failed = true;
	break;
      }
    }

    if (failed)
    {
      break;
    }
  }

  printf("test-sphere[1]: ");
  if (failed)
  {
    printf("FAIL\n");
  }
  else
  {
    printf("PASS\n");
  }

  free(workspace);
  workspace = NULL;
}
#endif


/*

OpenBDS							September 05, 2023

source: test/particles-sphere/test.c
author: @misael-diaz

Synopsis:
Tests the methods that update the spheres.

Copyright (c) 2023 Misael Diaz-Maldonado
This file is released under the GNU General Public License as published
by the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

References:
[0] A Koenig and B Moo, Accelerated C++ Practical Programming by Example.
[1] MP Allen and DJ Tildesley, Computer Simulation of Liquids.
[2] S Kim and S Karrila, Microhydrodynamics: Principles and Selected Applications.

*/
