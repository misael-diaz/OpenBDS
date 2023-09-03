#ifndef GUARD_OPENBDS_UTIL_RANDOM_H
#define GUARD_OPENBDS_UTIL_RANDOM_H

#include <stdint.h>
#include <stddef.h>

#define OBDS_ERR_PRNG ( (uint64_t) 0xfff0000000000000 )

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
