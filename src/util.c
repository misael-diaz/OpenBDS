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


// sets the spheres at grid locations so that they do not overlap with one another
void grid (double* restrict x, double* restrict y, double* restrict z)
{
  size_t const len = LENGTH;
  // gets the number of spheres (at contact) that fit along any dimension (x, y, or z)
  size_t const count = (len / 2);
  size_t const count2 = (count * count);

  // sets particles at grid locations:

  for (size_t n = 0; n != NUM_SPHERES; ++n)
  {
    size_t const i = (n % count);
    double const pos = 1.0 + 2.0 * ( (double) i );
    x[n] = pos;
  }

  for (size_t n = 0; n != NUM_SPHERES; ++n)
  {
    size_t const i = (n % count2) / count;
    double const pos = 1.0 + 2.0 * ( (double) i );
    y[n] = pos;
  }

  for (size_t n = 0; n != NUM_SPHERES; ++n)
  {
    size_t const i = (n / count2);
    double const pos = 1.0 + 2.0 * ( (double) i );
    z[n] = pos;
  }

  // shift the particles so that their coordinates are in the range [-LIMIT, +LIMIT]:

  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    x[i] -= LIMIT;
  }

  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    y[i] -= LIMIT;
  }

  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    z[i] -= LIMIT;
  }
}


static size_t overlap(const double* x,
		      const double* y,
		      const double* z,
		      double const offset_x,
		      double const offset_y,
		      double const offset_z)
{
  size_t overlaps = 0;
  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    for (size_t j = 0; j != NUM_SPHERES; ++j)
    {
      double const r_x = (x[j] + offset_x);
      double const r_y = (y[j] + offset_y);
      double const r_z = (z[j] + offset_z);
      double const d_x = (x[i] - r_x);
      double const d_y = (y[i] - r_y);
      double const d_z = (z[i] - r_z);
      // computes the squared distance of the pair for speed
      double const d = (d_x * d_x) + (d_y * d_y) + (d_z * d_z);

      size_t const contact = 4.0;
      if (d < contact)
      {
	++overlaps;
      }
    }
  }

  return overlaps;
}


// counts overlapping particles
int64_t overlaps (const double* restrict x,
		  const double* restrict y,
		  const double* restrict z)
{
  // checks for overlapping particles in the system:

  int64_t overlaps = 0;
  for (size_t i = 0; i != (NUM_SPHERES - 1); ++i)
  {
    for (size_t j = (i + 1); j != NUM_SPHERES; ++j)
    {
      double const d_x = (x[i] - x[j]);
      double const d_y = (y[i] - y[j]);
      double const d_z = (z[i] - z[j]);
      // computes the squared distance of the pair for speed
      double const d = (d_x * d_x) + (d_y * d_y) + (d_z * d_z);

      size_t const contact = 4.0;
      if (d < contact)
      {
	++overlaps;
      }
    }
  }

  // checks for overlaps with the images (accounts for the periodicity of the system):

  double offset_x = 0.0;
  double offset_y = 0.0;
  double offset_z = 0.0;

  offset_x = -LENGTH;
  overlaps += overlap(x, y, z, offset_x, offset_y, offset_z);

  offset_x = +LENGTH;
  overlaps += overlap(x, y, z, offset_x, offset_y, offset_z);

  offset_x = 0.0;
  offset_y = 0.0;
  offset_z = 0.0;

  offset_y = -LENGTH;
  overlaps += overlap(x, y, z, offset_x, offset_y, offset_z);

  offset_y = +LENGTH;
  overlaps += overlap(x, y, z, offset_x, offset_y, offset_z);

  offset_x = 0.0;
  offset_y = 0.0;
  offset_z = 0.0;

  offset_z = -LENGTH;
  overlaps += overlap(x, y, z, offset_x, offset_y, offset_z);

  offset_z = +LENGTH;
  overlaps += overlap(x, y, z, offset_x, offset_y, offset_z);

  // checks even the less likely images:

  offset_x = 0.0;
  offset_y = 0.0;
  offset_z = 0.0;

  offset_x = -LENGTH;
  offset_y = -LENGTH;
  overlaps += overlap(x, y, z, offset_x, offset_y, offset_z);

  offset_x = -LENGTH;
  offset_y = +LENGTH;
  overlaps += overlap(x, y, z, offset_x, offset_y, offset_z);

  offset_x = 0.0;
  offset_y = 0.0;
  offset_z = 0.0;

  offset_x = +LENGTH;
  offset_y = -LENGTH;
  overlaps += overlap(x, y, z, offset_x, offset_y, offset_z);

  offset_x = +LENGTH;
  offset_y = +LENGTH;
  overlaps += overlap(x, y, z, offset_x, offset_y, offset_z);

  offset_x = 0.0;
  offset_y = 0.0;
  offset_z = 0.0;

  offset_x = -LENGTH;
  offset_y = -LENGTH;
  offset_z = -LENGTH;
  overlaps += overlap(x, y, z, offset_x, offset_y, offset_z);

  offset_x = -LENGTH;
  offset_y = +LENGTH;
  offset_z = -LENGTH;
  overlaps += overlap(x, y, z, offset_x, offset_y, offset_z);

  offset_x = 0.0;
  offset_y = 0.0;
  offset_z = 0.0;

  offset_x = +LENGTH;
  offset_y = -LENGTH;
  offset_z = -LENGTH;
  overlaps += overlap(x, y, z, offset_x, offset_y, offset_z);

  offset_x = +LENGTH;
  offset_y = +LENGTH;
  offset_z = -LENGTH;
  overlaps += overlap(x, y, z, offset_x, offset_y, offset_z);

  offset_x = 0.0;
  offset_y = 0.0;
  offset_z = 0.0;

  offset_x = -LENGTH;
  offset_y = -LENGTH;
  offset_z = +LENGTH;
  overlaps += overlap(x, y, z, offset_x, offset_y, offset_z);

  offset_x = -LENGTH;
  offset_y = +LENGTH;
  offset_z = +LENGTH;
  overlaps += overlap(x, y, z, offset_x, offset_y, offset_z);

  offset_x = 0.0;
  offset_y = 0.0;
  offset_z = 0.0;

  offset_x = +LENGTH;
  offset_y = -LENGTH;
  offset_z = +LENGTH;
  overlaps += overlap(x, y, z, offset_x, offset_y, offset_z);

  offset_x = +LENGTH;
  offset_y = +LENGTH;
  offset_z = +LENGTH;
  overlaps += overlap(x, y, z, offset_x, offset_y, offset_z);

  return overlaps;
}


// returns the index of the head node (or root) given the ith node index
static int64_t head (const int64_t* list, int64_t const i)
{
  int64_t tail = i;
  while (tail >= 0)
  {
    tail = list[tail];
  }

  size_t const head = -(tail + 1);
  return head;
}


// returns the index of the tail node given the ith node index
static int64_t tail (const int64_t* list, int64_t const i)
{
  if (i < 0)
  {
    // return the node itself for it is unlinked to other nodes
    return -(i + 1);
  }

  int64_t tail = i;
  int64_t last = tail;
  while (tail >= 0)
  {
    last = tail;
    tail = list[tail];
  }

  return last;
}


// links "clusters" of particles if the ith and jth particles interact with one another
// note that the "clusters" could each contain a single particle or several
static bool link (int64_t const i,
		  int64_t const j,
		  int64_t* restrict list,
		  double const d)
{
  int64_t const root_i = head(list, i);
  int64_t const root_j = head(list, j);
  // returns false if the i and j nodes are already linked, for there's nothing to do
  if (root_i == root_j)
  {
    return false;
  }

  // checks if the pair of particles interacts with one another:

  bool linked = false;

  if (d <= RANGE)
  {
    // merges the "clusters" by hierarchy (lowest rank has the highest hierarchy):

    int64_t const leaf_i = tail(list, i);
    int64_t const leaf_j = tail(list, j);

    if (root_i < root_j)
    {
      // nests the (high rank) jth "cluster" under the (low rank) ith "cluster"
      list[leaf_i] = root_j;
      list[leaf_j] = -(root_i + 1);
    }
    else
    {
      // nests the (high rank) ith "cluster" under the (low rank) jth "cluster"
      list[leaf_j] = root_i;
      list[leaf_i] = -(root_j + 1);
    }

    linked = true;
  }

  return linked;
}


// generates the neighbor-list
void list(int64_t* restrict list,
	  double* restrict d,
	  const double* restrict x,
	  const double* restrict y,
	  const double* restrict z)
{
  // initializes the linked-list array so that each node is a root:

  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    list[i] = -(i + 1);
  }

  // generates the initial list of neighbors without considering the periodic boundaries:

  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    // vectorizable by GCC, calculates the (squared) interpaticle distance for speed
    for (size_t j = 0; j != NUM_SPHERES; ++j)
    {
      d[j] = (x[i] - x[j]) * (x[i] - x[j]) +
	     (y[i] - y[j]) * (y[i] - y[j]) +
	     (z[i] - z[j]) * (z[i] - z[j]);
    }

    for (size_t j = 0; j != NUM_SPHERES; ++j)
    {
      double const dist = d[j];
      bool const linked = link(i, j, list, dist);
      // if the pair was linked we continue with the next ith particle, for each particle
      // can only be linked to just one other jth particle; otherwise we keep looking
      if (linked)
      {
	break;
      }
    }
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
