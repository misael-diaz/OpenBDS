#include "system/params.h"
#include "system/box/params.h"
#include "bds/types.h"

#define NUMEL ( (size_t) ( __OBDS_NUM_PARTICLES__ ) )
#define LIMIT ( (double) ( __OBDS_LIMIT__ ) )
#define MSBMASK ( (uint64_t) 0x8000000000000000 )


// gets the 11-bits that comprise the exponent of a double precision floating-point number
static uint64_t exponent (uint64_t const x)
{
  return ( (x >> 52) & 0x7ff );
}


// possible implementation of the 2's complement
static uint64_t twos_complement (uint64_t const x)
{
  return (~x + 1);
}


// static uint64_t pexponent (uint64_t const x)
//
// Synopsis:
// Positive Exponent, pexponent().
// Yields one if the exponent bits correspond to an exponent n > 0, yields zero otherwise.
// The parameter `x' stores the binary floating-point representation of a 64-bit floating
// point number.


static uint64_t pexponent (uint64_t const x)
{
  return ( (exponent(x) & 0x400) >> 10 );
}


// static uint64_t zexponent (uint64_t const x)
//
// Synopsis:
// Zero Exponent, zexponent().
// Yields one if the exponent bits correspond to an exponent n = 0, yields zero otherwise.
// The parameter `x' stores the binary floating-point representation of a 64-bit floating
// point number.


static uint64_t zexponent (uint64_t const x)
{
 return ((((((((exponent(x) & 0x3ff) ^ 0x3ff) & 0x3ff) + 0x3ff) & 0x400) >> 10) + 1) & 1);
}


// yields a bitmask of ones if abs(x) >= 1, bitmask of zeros otherwise
static uint64_t unlimited (uint64_t const x)
{
  return ( twos_complement( pexponent(x) | zexponent(x) ) );
}


// masks particles beyond the system limits with a binary pattern of ones, otherwise zeros
static void mask (const prop_t* __restrict__ x, prop_t* __restrict__ bitmask)
{
  uint64_t* b = &bitmask[0].bin;
  const uint64_t* fp = &x[0].bin;
  for (size_t i = 0; i != NUMEL; ++i)
  {
    b[i] = unlimited(fp[i]);
  }
}


// performs the pre-scaling so that x ~ 1 (or x = O(1))
static void scale (prop_t* x)
{
  double* data = &x[0].data;
  double const c = 1.0 / LIMIT;
  for (size_t i = 0; i != NUMEL; ++i)
  {
    data[i] *= c;
  }
}


// restores the original scaling of x ~ LIMIT
static void rescale (prop_t* x)
{
  double const c = LIMIT;
  double* data = &x[0].data;
  for (size_t i = 0; i != NUMEL; ++i)
  {
    data[i] *= c;
  }
}


// sets the offset on the particles according to the bitmask and their positions (signbit)
static void offset (const prop_t* __restrict__ x,
		    const prop_t* __restrict__ bitmask,
		    prop_t* __restrict__ offset)
{
  double const length = 2.0;
  uint64_t* o = &offset[0].bin;
  const uint64_t* fp = &x[0].bin;
  const uint64_t* b = &bitmask[0].bin;
  prop_t const len = { .data = length };
  uint64_t const l = len.bin;
  for (size_t i = 0; i != NUMEL; ++i)
  {
    o[i] = (b[i] & l) | ( ( (MSBMASK & fp[i]) ^ MSBMASK ) & MSBMASK );
  }
}


// shifts x, y, or z-axis coordinates by the offset
static void shift (prop_t* __restrict__ x, const prop_t* __restrict__ offset)
{
  double* data = &x[0].data;
  const double* o = &offset[0].data;
  for (size_t i = 0; i != NUMEL; ++i)
  {
    data[i] += o[i];
  }
}


// applies periodic boundary conditions to the particles
static void pbc (prop_t* __restrict__ x,
		 prop_t* __restrict__ distance,
		 prop_t* __restrict__ bitmask)
{
  // applies required pre-scaling:

  scale(x);

  // applies periodic boundary conditions on particles beyond the system limits:

  mask(x, bitmask);
  offset(x, bitmask, distance);
  shift(x, distance);

  // restores the original scaling:

  rescale(x);
}

// applies periodic boundary conditions at x = y = z = -LIMIT and +LIMIT
void system_box_applyPeriodicBoundaryConditions (particle_t* particles)
{
  prop_t* x = particles -> x;
  prop_t* y = particles -> y;
  prop_t* z = particles -> z;
  prop_t* offset = particles -> temp;
  prop_t* bitmask = particles -> bitmask;
  pbc(x, offset, bitmask);
  pbc(y, offset, bitmask);
  pbc(z, offset, bitmask);
}


/*

OpenBDS								September 05, 2023

source: system/box/box.c
author: @misael-diaz

Synopsis:
Implements methods for applying periodic boundary conditions on the particles.

Copyright (c) 2023 Misael Diaz-Maldonado
This file is released under the GNU General Public License as published
by the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

References:
[0] A Koenig and B Moo, Accelerated C++ Practical Programming by Example.
[1] MP Allen and DJ Tildesley, Computer Simulation of Liquids.
[2] S Kim and S Karrila, Microhydrodynamics: Principles and Selected Applications.

*/
