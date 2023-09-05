#ifndef GUARD_OPENBDS_SYSTEM_BOX_H
#define GUARD_OPENBDS_SYSTEM_BOX_H

#define LIMIT 8.0
#define LENGTH (2.0 * LIMIT)

void pbc(double* restrict x, double* restrict offset, double* restrict bitmask);

#endif
