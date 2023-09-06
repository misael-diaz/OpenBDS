#ifndef GUARD_OPENBDS_SYSTEM_BOX_H
#define GUARD_OPENBDS_SYSTEM_BOX_H

#include "bds/types.h"

#define LIMIT 8.0
#define LENGTH (2.0 * LIMIT)

void pbc(prop_t* restrict x, prop_t* restrict offset, prop_t* restrict bitmask);

#endif
