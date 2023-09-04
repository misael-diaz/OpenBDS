#include <fcntl.h>	// for reading /dev/urandom
#include <sys/stat.h>	// for reading /dev/urandom
#include <sys/types.h>	// required by getpid(), see man getpid
#if ( (__GLIBC__ > 2) || (__GLIBC__ == 2 && __GLIBC_MINOR__ >= 25) )
#include <sys/random.h>	// for seeding the PRNG with getrandom()
#endif
#include <unistd.h>	// required by getpid(), see man getpid
#include <assert.h>	// for performing assertions at compile time via static_assert()
#include <string.h>	// for logging errors on the standard error stream
#include <stdio.h>	// for logging errors on the standard error stream
#include <errno.h>	// number of last error, see man errno
#include <time.h>	// provides system time(), see man time(2)
#include <math.h>	// for generating normally distributed pseudo-random numbers

#include "util/random.h"

#define FAILURE ( (int) 0xffffffff )
#define SUCCESS ( (int) 0x00000000 )
#define MSBMASK ( (int64_t) 0x8000000000000000 )
#define PERIOD ( (uint64_t) 0xffffffffffffffff )
#define SCALING ( 1.0 / ( (double) MSBMASK ) )
// 64-bit binary floatint-poing representation of 2^N
#define BIAS ( (uint64_t) 1023 )
#define EXP(N) ( (N + BIAS) << 52 )


static uint32_t xor ()	// XORs the current time and the process ID for seeding the PRNG
{
#if ( (__GLIBC__ > 2) || (__GLIBC__ == 2 && __GLIBC_MINOR__ >= 25) )
  static_assert(sizeof(time_t) == 8);
#else
  _Static_assert(sizeof(time_t) == 8);
#endif
  // if the underlying type of `time_t' is 64-bits, then use both the low and high bits
  time_t const t = time(NULL);
  if (t == -1)
  {
    // warns user about unexpected error, for time(NULL) should not fail see `man time(2)'
    fprintf(stderr, "xor(): UNEXPECTED ERROR %s\n", strerror(errno));
    return 0xffffffff;
  }
  uint32_t const hi = ( (uint32_t) ( (t >> 32) & 0xffffffff ) );
  uint32_t const lo = ( (uint32_t) (t & 0xffffffff) );
  uint32_t const pid = ( (uint32_t) getpid() );
  uint32_t const ret = ( ( (hi | lo) ^ pid ) & 0xfffffffe );
  return ret;
}


#if ( (__GLIBC__ > 2) || (__GLIBC__ == 2 && __GLIBC_MINOR__ >= 25) )
static uint32_t genseed () // generates seed by fetching /dev/urandom or fallsback to XOR
{
#if ( (__GLIBC__ > 2) || (__GLIBC__ == 2 && __GLIBC_MINOR__ >= 25) )
  static_assert(sizeof(uint32_t) == sizeof(unsigned int));
#else
  _Static_assert(sizeof(uint32_t) == sizeof(unsigned int));
#endif

  uint32_t prn = 0xffffffff;
  if (getrandom(&prn, sizeof(uint32_t), GRND_NONBLOCK) == -1)
  {
    fprintf(stderr, "genseed(): ERROR %s\n", strerror(errno));
    fprintf(stderr, "genseed(): falling back to XORing\n");
    prn = xor();
  }

  return (prn & 0xfffffffe);
}
#else
static uint32_t genseed ()
{
#if ( (__GLIBC__ > 2) || (__GLIBC__ == 2 && __GLIBC_MINOR__ >= 25) )
  static_assert(sizeof(uint32_t) == sizeof(unsigned int));
#else
  _Static_assert(sizeof(uint32_t) == sizeof(unsigned int));
#endif

  int devurand = open("/dev/urandom", O_RDONLY);
  if (devurand == FAILURE)
  {
    fprintf(stderr, "genseed(): ERROR %s\n", strerror(errno));
    fprintf(stderr, "genseed(): falling back to XORing\n");
    return xor();
  }

  uint32_t prn = 0xffffffff;
  ssize_t size = read(devurand, &prn, sizeof(uint32_t));
  if (size == -1 || size != sizeof(uint32_t))
  {
    // NOTE that read() may not check for errors (see `man read(2)')
    fprintf(stderr, "genseed(): ERROR %s\n", strerror(errno));
    fprintf(stderr, "genseed(): falling back to XORing\n");
    prn = xor();
    return prn;
  }

  return (prn & 0xfffffffe);
}
#endif


static int seeder (generator_t* generator)
{
#if ( (__GLIBC__ > 2) || (__GLIBC__ == 2 && __GLIBC_MINOR__ >= 25) )
  static_assert(sizeof(int) == 4);
#else
  _Static_assert(sizeof(int) == 4);
#endif
  uint64_t const seed = genseed();
  if ( (seed == 0) || (seed & 0x0000000000000001) )
  {
    fprintf(stderr, "seed(): ERROR\n");
    return FAILURE;
  }
  uint64_t const hi = 0xffffffff00000000;
  uint64_t const lo = 0x00000000ffffffff;
  // using the bitwise AND to be explicit about the bits (un)set
  *generator -> state = ( ( (seed << 32) & hi ) | (seed & lo) );
  *generator -> count = 0;
  return SUCCESS;
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
static double xorshift64 (generator_t* generator)
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
static double urand (generator_t* generator)
{
  return xorshift64(generator);
}


// implements Böx-Muller's method to yield normally distributed pseudo-random numbers
static double nrand (generator_t* generator)
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


// resets the PRNG by seeding and resetting the counter to zero
static int reset (random_t* random)
{
  if (random -> generator -> seed(random -> generator) == FAILURE)
  {
    fprintf(stderr, "reset(): ERROR\n");
    return FAILURE;
  }
  *(random -> generator -> count) = 0;
  return SUCCESS;
}


static double fetcher (random_t* random)
{
  double const count = *(random -> generator -> count);
  if (count >= PERIOD)
  {
    if (reset(random) == FAILURE)
    {
      fprintf(stderr, "fetch(): ERROR\n");
      union { double dat; uint64_t bin; } const errstat = { .bin = OBDS_ERR_PRNG };
      return errstat.dat;
    }
  }
  return random -> generator -> fetch(random -> generator);
}


// initial version of the PRNG initializer, state seeding and binding
int util_random_initializer (random_t* random, enum PRNG PRNG)
{
  generator_t* generator = random -> generator;
  generator -> seed = seeder;
  if (PRNG == URAND)
  {
    generator -> fetch = urand;
  }
  else // (PRNG == NRAND)
  {
    generator -> fetch = nrand;
  }
  int const stat = generator -> seed(generator);
  random -> fetch = fetcher;
  return stat;
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


// TODO:
// [ ] use either stdlib's getrandom() or read /dev/urandom to seed the PRNG
// [ ] fallback to XORing if the /dev/urandom does not have enough entropy
