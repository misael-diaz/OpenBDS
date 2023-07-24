#include <stdint.h>
#include <math.h>
#include "system.h"
#include "util.h"

#define MSBMASK 0x8000000000000000
#define BITMASK 0xffffffffffffffff

// we use this for bitmasking floating-point numbers and for writing vectorizable code
typedef union
{
  uint64_t bin;
  double data;
} alias_t;


// returns the Most Significant Bit MSB (yields either zero or one)
static uint64_t get_msb (uint64_t const x)
{
  return ( (x & MSBMASK) >> 63 );
}


// gets the 11-bits that comprise the exponent of a double precision floating-point number
static uint64_t get_exp (uint64_t const x)
{
  return ( (x >> 52) & 0x7ff );
}


// possible implementation of the 2's complement
static uint64_t twos_complement (uint64_t const x)
{
  return (~x + 1);
}


// returns one if x = [1.0, 2.0), otherwise returns zero (because x < 1); this method
// assumes that the abs(x) values to be less than two (this is ensured by prescaling)
static uint64_t get_unlimited (uint64_t const x)
{
  return ( ( ( ( ( (get_exp(x) ^ 0x3ff) + 0x3ff ) & 0x400 ) >> 10 ) + 1 ) & 1);
}


bool sign (double const x)
{
  bool const ret = signbit(x);
  return ret;
}


void zeros (size_t const size, double* x)
{
  for (size_t i = 0; i != size; ++i)
  {
    x[i] = 0.0;
  }
}


void iota (size_t const size, int64_t* x)
{
  for (size_t i = 0; i != size; ++i)
  {
    x[i] = i;
  }
}


// void mask_partition (size_t size, double* x, double* mask)
//
// Synopsis:
// Vectorizable by GCC.
// Masks particles in the right partition; that is, the mask is zero for particles
// whose (any of its) coordinates satisfy the inequality x >= 0, y >= 0, or z >= 0.
// Note that this method does not care about the dimensionality of the system (1D, 2D,
// or 3D), for it only acts on one dimension at a time (and that's exactly what we need).
//
// Inputs:
// size		number of elements of the arrays (both `x' and `mask')
// x		either x, y, or z-axis coordinates of the particles
//
// Output:
// bitmask	zero for particles in the right partition (x >= 0, y >= 0, or z >= 0),
//		otherwise it is equal to the binary pattern of the BITMASK MACRO.


void mask_partition(size_t const size,
		    const double* restrict x,
		    double* restrict mask)
{
  alias_t* fp = x;	// floating-point number fp
  alias_t* m = mask;	// bitmasks with respect to the x, y, or z-axis coordinates
  for (size_t i = 0; i != size; ++i)
  {
    m[i].bin = twos_complement( get_msb(fp[i].bin) );
  }
}


// analogous to mask_partition(), vectorizable by gcc
void mask_unlimited(size_t const size,
		    const double* restrict x,
		    const double* restrict mask,
		    double* restrict temp,
		    double* restrict bitmask)
{
  alias_t* fp = x;
  alias_t* t = temp;
  // uses the binary representation of `x' to determine if abs(x) > 1.0
  for (size_t i = 0; i != size; ++i)
  {
    t[i].bin = twos_complement( get_unlimited(fp[i].bin) );
  }

  const alias_t* m = mask;
  alias_t* b = bitmask;
  // masks unlimited particles whose x < -1.0 (x > +1.0):
  for (size_t i = 0; i != size; ++i)
  {
    b[i].bin = (m[i].bin & t[i].bin);
  }
}


// performs the pre-scaling so that x ~ 1 (or x = O(1))
static void scale (double* x)
{
  double const c = 1.0 / (0.5 * LENGTH);
  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    x[i] *= c;
  }
}


// restores the original scaling of x ~ LIMIT
static void rescale (double* x)
{
  double const c = (0.5 * LENGTH);
  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    x[i] *= c;
  }
}


// sets the offset to be applied given the magnitude length and the bitmask
static void offset (double const len,
		    double* restrict offset,
		    const double* restrict mask)
{
  alias_t* o = offset;
  const alias_t* m = mask;
  alias_t const l = { .data = len };
  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    o[i].bin = (m[i].bin & l.bin);
  }
}


// shifts x, y, or z-axis coordinates by offset
static void shift (double* restrict x, const double* offset)
{
  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    x[i] += offset[i];
  }
}


// applies periodic boundary conditions to the particles
void pbc (double* restrict x,
	  double* restrict temp,
	  double* restrict mask,
	  double* restrict bitmask)
{
  // applies required pre-scaling:

  scale(x);

  // applies periodic boundary conditions on the particles in the left partition:

  mask_partition(NUM_SPHERES, x, mask);
  mask_unlimited(NUM_SPHERES, x, mask, temp, bitmask);
  offset(+1.0, temp, bitmask);
  shift(x, temp);

  // applies periodic boundary conditions on the particles in the right partition:

  alias_t* t = temp;
  alias_t* m = mask;
  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    t[i].bin = ~m[i].bin;
  }

  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    m[i].bin = t[i].bin;
  }

  mask_unlimited(NUM_SPHERES, x, mask, temp, bitmask);
  offset(-1.0, temp, bitmask);
  shift(x, temp);

  // restores the original scaling

  rescale(x);
}


/*

OpenBDS								July 19, 2023

source: util.c
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
