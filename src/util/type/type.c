#include "util/type.h"

extern void util_random_initializer(struct random*, enum PRNG);

static struct iPRNG const random = {
  .initializer = util_random_initializer
};

struct util const util = {
  .random = random
};
