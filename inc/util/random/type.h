#ifndef GUARD_OPENBDS_UTIL_RANDOM_TYPE_H
#define GUARD_OPENBDS_UTIL_RANDOM_TYPE_H

#include <stdint.h>

enum PRNG {		// Pseudo Random Number Generator
  URAND,		// Uniform PRNG
  NRAND			// Normal PRNG
};

struct generator {
  double* count;
  uint64_t* state;
  int (*seed) (struct generator*);
  double (*fetch) (struct generator*);
};

typedef struct generator generator_t;

struct random
{
  generator_t* generator;
  double (*fetch) (struct random*);
};

typedef struct random random_t;

struct iPRNG {
  int (*initializer) (struct random*, enum PRNG);
};

typedef struct iPRNG iPRNG_t;

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
