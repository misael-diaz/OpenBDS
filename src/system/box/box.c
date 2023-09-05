#include <stddef.h>
#include <stdint.h>

#include "system.h"
#include "system/box.h"

#define NUMEL NUM_PARTICLES
#define MSBMASK 0x8000000000000000

// we use this for bitmasking floating-point numbers and for writing vectorizable code
typedef union
{
  uint64_t bin;
  double data;
} alias_t;


// gets the 11-bits that comprise the exponent of a double precision floating-point number
static uint64_t exp (uint64_t const x)
{
  return ( (x >> 52) & 0x7ff );
}


// possible implementation of the 2's complement
static uint64_t twos_complement (uint64_t const x)
{
  return (~x + 1);
}


// static uint64_t pexp (uint64_t const x)
//
// Synopsis:
// Positive Exponent, pexp().
// Yields one if the exponent bits correspond to an exponent n > 0, yields zero otherwise.
// The parameter `x' stores the binary floating-point representation of a 64-bit floating
// point number.


static uint64_t pexp (uint64_t const x)
{
  return ( (exp(x) & 0x400) >> 10 );
}


// static uint64_t zexp (uint64_t const x)
//
// Synopsis:
// Zero Exponent, zexp().
// Yields one if the exponent bits correspond to an exponent n = 0, yields zero otherwise.
// The parameter `x' stores the binary floating-point representation of a 64-bit floating
// point number.


static uint64_t zexp (uint64_t const x)
{
 return ((((((((exp(x) & 0x3ff) ^ 0x3ff) & 0x3ff) + 0x3ff) & 0x400) >> 10) + 1) & 1);
}


// yields a bitmask of ones if abs(x) >= 1, bitmask of zeros otherwise
static uint64_t unlimited (uint64_t const x)
{
  return ( twos_complement( pexp(x) | zexp(x) ) );
}


// masks particles beyond the system limits with a binary pattern of ones, otherwise zeros
static void mask (const double* restrict x, double* restrict bitmask)
{
  alias_t* b = bitmask;
  const alias_t* fp = x;
  for (size_t i = 0; i != NUMEL; ++i)
  {
    b[i].bin = unlimited(fp[i].bin);
  }
}


// performs the pre-scaling so that x ~ 1 (or x = O(1))
static void scale (double* x)
{
  double const c = 1.0 / LIMIT;
  for (size_t i = 0; i != NUMEL; ++i)
  {
    x[i] *= c;
  }
}


// restores the original scaling of x ~ LIMIT
static void rescale (double* x)
{
  double const c = LIMIT;
  for (size_t i = 0; i != NUMEL; ++i)
  {
    x[i] *= c;
  }
}


// sets the offset on the particles according to the bitmask and their positions (signbit)
static void offset (const double* restrict x,
		    const double* restrict bitmask,
		    double* restrict offset)
{
  alias_t* o = offset;
  const alias_t* fp = x;
  double const len = 2.0;
  const alias_t* b = bitmask;
  alias_t const l = { .data = len };
  for (size_t i = 0; i != NUMEL; ++i)
  {
    o[i].bin = (b[i].bin & l.bin) | ( ( (MSBMASK & fp[i].bin) ^ MSBMASK ) & MSBMASK );
  }
}


// shifts x, y, or z-axis coordinates by the offset
static void shift (double* restrict x, const double* restrict offset)
{
  for (size_t i = 0; i != NUMEL; ++i)
  {
    x[i] += offset[i];
  }
}


// applies periodic boundary conditions to the particles
void pbc (double* restrict x, double* restrict distance, double* restrict bitmask)
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


/*

OpenBDS								September 05, 2023

source: box.c
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
