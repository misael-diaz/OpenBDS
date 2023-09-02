#include <math.h>
#include "util/random.h"

#define MSBMASK ( (int64_t) 0x8000000000000000 )
#define SCALING ( 1.0 / ( (double) MSBMASK ) )
// 64-bit binary floatint-poing representation of 2^N
#define BIAS ( (uint64_t) 1023 )
#define EXP(N) ( (N + BIAS) << 52 )


static void seeder (struct generator* generator)
{
  *generator -> state = 0xffffffffffffffff;
  *generator -> count = 0;
}


// double sxorshift64(int64_t* state)
//
// Synopsis:
// Implements Marsaglia's 64-bit xorshift Pseudo Random Number Generator PRNG.
// Yields uniformly distributed pseudo-random numbers in [0, 1).
// This implementation uses signed 64-bit integer for interoperability with FORTRAN.
//
// Input:
// state	initial state of the pseudo-random number generator
//
// Output:
// state	updated state
//
// Return:
// prn		uniformly distributed pseudo-random number in [0, 1)


double sxorshift64 (int64_t* state)
{
  int64_t x = *state;
  x ^= (x << 13);
  x ^= (x >> 7);
  x ^= (x << 17);
  *state = x;

  double const c = SCALING;
  double const r = c * x;
  return ( 0.5 * (r + 1.0) );
}


// as sxorshift64() but uses unsigned 64-bit integers to update the PRNG state
static double xorshift64 (struct generator* generator)
{
  uint64_t x = *(generator -> state);
  x ^= (x << 13);
  x ^= (x >> 7);
  x ^= (x << 17);
  *(generator -> state) = x;
  ++( *(generator -> count) );

  // hardcodes the binary floating-point representation of 2^(-64) for quick scaling
  uint64_t const n = -64;
  union { uint64_t bin; double data; } const c = { .bin = EXP(n) };
  double const data = c.data;
  double const prn = ( data * ( (double) x ) );
  return prn;
}


// aliases Marsaglia's xorshift64() uniform pseudo random number generator
static double urand (struct generator* generator)
{
  return xorshift64(generator);
}


// implements Böx-Muller's method to yield normally distributed pseudo-random numbers
static double nrand (struct generator* generator)
{
  double const inf = INFINITY;
  double x = inf;
  double y = inf;
  double r = inf;
  while (r > 1.0)
  {
    x = 2.0 * urand(generator) - 1.0;
    y = 2.0 * urand(generator) - 1.0;
    r = (x * x) + (y * y);
  }
  r = sqrt( ( -2.0 * log(r) ) / r );
  x *= r;
  return x;
}


static double fetcher (struct random* random)
{
  return random -> generator -> fetch(random -> generator);
}


// initial version of the PRNG initializer, state seeding and binding
void util_random_initializer (struct random* random, enum PRNG PRNG)
{
  struct generator* generator = random -> generator;
  generator -> seed = seeder;
  if (PRNG == URAND)
  {
    generator -> fetch = urand;
  }
  else // (PRNG == NRAND)
  {
    generator -> fetch = nrand;
  }
  generator -> seed(generator);
  random -> fetch = fetcher;
}


/*

OpenBDS								July 19, 2023

source: random.c
author: @misael-diaz

Synopsis:
Implements pseudo-random number generator utils.

Copyright (c) 2023 Misael Diaz-Maldonado
This file is released under the GNU General Public License as published
by the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

References:
[0] A Koenig and B Moo, Accelerated C++ Practical Programming by Example.
[1] MP Allen and DJ Tildesley, Computer Simulation of Liquids.
[2] S Kim and S Karrila, Microhydrodynamics: Principles and Selected Applications.

*/
