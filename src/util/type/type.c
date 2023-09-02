#include "util/type.h"

extern void util_random_initializer(random_t*, enum PRNG);

static iPRNG_t const random = {
  .initializer = util_random_initializer
};

util_t const util = {
  .random = random
};
