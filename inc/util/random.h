#ifndef GUARD_OPENBDS_UTIL_RANDOM_H
#define GUARD_OPENBDS_UTIL_RANDOM_H

#include <stdint.h>
#include <stddef.h>

enum PRNG {		// Pseudo Random Number Generator
  URAND,		// Uniform PRNG
  NRAND			// Normal PRNG
};

struct generator {
  size_t* count;
  uint64_t* state;
  void (*seed) (struct generator*);
  double (*fetch) (struct generator*);
} generator_t;

struct random
{
  struct generator* generator;
  double (*fetch) (struct random*);
} random_t;

struct iPRNG {
  void (*initializer) (struct random*, enum PRNG);
} initPRNG_t;

#endif
