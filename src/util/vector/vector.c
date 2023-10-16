#include <stddef.h>
#include <stdint.h>
#include <float.h>

#include "util/vector/type.h"
#include "system/params.h"

// hardcoded number of elements `numel' of the arrays
#define NUMEL ( (size_t) ( __OBDS_NUM_PARTICLES__ ) )
// hardcoded number of elements `vecsz' of the vectors
#define VECSZ ( (size_t) ( ( (size_t) 3 ) * ( NUMEL ) ) )
// defines bitmask for the Most Significant Bit MSB
#define MSB ( (uint64_t) 0x8000000000000000 )
#define ABS(x) (x)

// fills vector `x' with zeros
static void zeros (double* __restrict__ x)
{
  for (size_t i = 0; i != VECSZ; ++i)
  {
    x[i] = 0.0;
  }
}

// stores the elementwise sum of `src' and `dst' vectors in `dst'
static void sum (double* __restrict__ dst, const double* __restrict__ src)
{
  for (size_t i = 0; i != VECSZ; ++i)
  {
    dst[i] += src[i];
  }
}

// returns the smallest, absolute-valued, element of the vector `x', min( abs(x[:] )
static double min (const vector_t* __restrict__ x)
{
  double min = DBL_MAX;
  for (size_t i = 0; i != VECSZ; ++i)
  {
    uint64_t const bin = x[i].bin;
    prop_t const ux = { .bin = ( bin & (~MSB) ) };
    double const udata = ux.data;
    if (udata < min)
    {
      min = udata;
    }
  }
  return min;
}

// returns the largest, absolute-valued element of the vector `x', max( abs(x[:]) )
static double max (const vector_t* vectors)
{
  double max = 0;
  for (size_t i = 0; i != VECSZ; ++i)
  {
    uint64_t const bin = vectors[i].bin;
    prop_t const p = { .bin = ( bin & (~MSB) ) };
    double const data = p.data;
    if (data > max)
    {
      max = data;
    }
  }
  return max;
}

// fills vector with zeros
void util_vector_zeros (vector_t* __restrict__ vector)
{
  double* x = &(vector[0].data);
  zeros(x);
}

// returns the smallest, absolute-valued, element of the vector `x', min( abs(x[:] )
double util_vector_min (const vector_t* __restrict__ x)
{
  return min( ABS(x) );
}

// returns the largest, absolute-valued element of the vector `x', max( abs(x[:]) )
double util_vector_max (const vector_t* __restrict__ x)
{
  return max( ABS(x) );
}

// elementwise sum of vectors `dest' and `source'
void util_vector_sum (vector_t* __restrict__ dest, const vector_t* __restrict__ source)
{
  double* dst = &(dest[0].data);
  const double* src = &(source[0].data);
  sum(dst, src);
}

/*

OpenBDS								October 12, 2023

source: util/vector/vector.c
author: @misael-diaz

Synopsis:
Implements utility methods for vectors.

Copyright (c) 2023 Misael Diaz-Maldonado
This file is released under the GNU General Public License as published
by the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

References:
[0] A Koenig and B Moo, Accelerated C++ Practical Programming by Example.
[1] MP Allen and DJ Tildesley, Computer Simulation of Liquids.
[2] S Kim and S Karrila, Microhydrodynamics: Principles and Selected Applications.

*/
