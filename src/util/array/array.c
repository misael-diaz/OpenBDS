#include <stddef.h>
#include "util/array.h"
#include "system/params.h"

#define NUMEL ( (size_t) __OBDS_NUM_PARTICLES__ )

static void copy (const prop_t* __restrict__ source, prop_t* __restrict__ dest)
{
  double* dst = &dest[0].data;
  const double* src = &source[0].data;
  for (size_t i = 0; i != NUMEL; ++i)
  {
    dst[i] = src[i];
  }
}

static void zeros (prop_t* x)
{
  double* data = &x[0].data;
  for (size_t i = 0; i != NUMEL; ++i)
  {
    data[i] = 0.0;
  }
}

static void ones (prop_t* x)
{
  double* data = &x[0].data;
  for (size_t i = 0; i != NUMEL; ++i)
  {
    data[i] = 1.0;
  }
}

static void iota (prop_t* ID)
{
  uint64_t* id = &ID[0].bin;
  for (size_t i = 0; i != NUMEL; ++i)
  {
    id[i] = i;
  }
}

void util_array_copy (const prop_t* __restrict__ src, prop_t* __restrict__ dst)
{
  copy(src, dst);
}

void util_array_zeros (prop_t* x)
{
  zeros(x);
}

void util_array_ones (prop_t* x)
{
  ones(x);
}

void util_array_iota (prop_t* x)
{
  iota(x);
}

/*

OpenBDS								July 19, 2023

source: util/array/arrays.c
author: @misael-diaz

Synopsis:
Implements numpy-like utility methods for arrays.

Copyright (c) 2023 Misael Diaz-Maldonado
This file is released under the GNU General Public License as published
by the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

References:
[0] A Koenig and B Moo, Accelerated C++ Practical Programming by Example.
[1] MP Allen and DJ Tildesley, Computer Simulation of Liquids.
[2] S Kim and S Karrila, Microhydrodynamics: Principles and Selected Applications.

*/
