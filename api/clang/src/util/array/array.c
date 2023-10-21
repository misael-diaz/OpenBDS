#include <stddef.h>
#include "util/array.h"
#include "system/params.h"
#include "bds/types/property.h"

// hardcoded number of elements `numel' of the arrays
#define NUMEL ( (size_t) ( __OBDS_NUM_PARTICLES__ ) )

// copies the `numel' elements of the source array `src' into the destination array `dst'
static void copy (const double* __restrict__ src, double* __restrict__ dst)
{
  for (size_t i = 0; i != NUMEL; ++i)
  {
    dst[i] = src[i];
  }
}

// fills array `x' with zeros, numpy-like zeros
static void zeros (double* x)
{
  for (size_t i = 0; i != NUMEL; ++i)
  {
    x[i] = 0.0;
  }
}

// fills array `x' with ones, numpy-like ones
static void ones (double* x)
{
  for (size_t i = 0; i != NUMEL; ++i)
  {
    x[i] = 1.0;
  }
}

// fills array `x' with the sequence in the asymmetric range [0, NUMEL)
static void iota (uint64_t* x)
{
  uint64_t const numel = NUMEL;
  for (uint64_t i = 0; i != numel; ++i)
  {
    x[i] = i;
  }
}

// array copy utility
void util_array_copy (const prop_t* __restrict__ source, prop_t* __restrict__ destination)
{
  double* dst = &(destination[0].data);
  const double* src = &(source[0].data);
  copy(src, dst);
}

// numpy-like zeros utility
void util_array_zeros (prop_t* properties)
{
  double* x = &(properties[0].data);
  zeros(x);
}

// numpy-like ones utility
void util_array_ones (prop_t* properties)
{
  double* x = &(properties[0].data);
  ones(x);
}

// C++ std::iota like utility
void util_array_iota (prop_t* properties)
{
  uint64_t* x = &(properties[0].bin);
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
