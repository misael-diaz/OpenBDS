#ifndef GUARD_OPENBDS_UTIL_RANDOM_TYPE_H
#define GUARD_OPENBDS_UTIL_RANDOM_TYPE_H

#include <stdint.h>

enum PRNG {				// Pseudo Random Number Generator PRNG
  URAND,				// Uniform PRNG
  NRAND					// Normal PRNG
};

struct generator {
  double* count;			// count of generated pseudo-random numbers
  uint64_t* state;			// internal state of the PRNG
  int (*seed) (struct generator*);	// seeds the PRNG via OS resources
  double (*fetch) (struct generator*);	// fetches a new pseudo-random number
};

typedef struct generator generator_t;

struct random
{
  generator_t* generator;		// the PRNG
  double (*fetch) (struct random*);	// provides simpler interface to PRNG fetch()
};

typedef struct random random_t;

int util_random_initializer(random_t*, enum PRNG);

#endif

/*

OpenBDS							September 05, 2023

source: util/random/type.h
author: @misael-diaz

Synopsis:
Defines the Pseudo Random Number Generator PRNG types.

Copyright (c) 2023 Misael Diaz-Maldonado
This file is released under the GNU General Public License as published
by the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

References:
[0] A Koenig and B Moo, Accelerated C++ Practical Programming by Example.
[1] MP Allen and DJ Tildesley, Computer Simulation of Liquids.
[2] S Kim and S Karrila, Microhydrodynamics: Principles and Selected Applications.

*/
