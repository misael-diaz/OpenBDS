#ifndef GUARD_OPENBDS_UTIL_TYPE_H
#define GUARD_OPENBDS_UTIL_TYPE_H

#include "random.h"

struct util
{
  iPRNG_t random;		// PRNG initializer
};

typedef struct util util_t;

#endif
