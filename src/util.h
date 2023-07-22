#ifndef GUARD_OPENBDS_UTIL_H
#define GUARD_OPENBDS_UTIL_H

#include <stdlib.h>
#include <stdbool.h>

bool sign (double const x);
void zeros (size_t const size, double* x);
void iota (size_t const size, double* x);

void mask_partition (size_t const size, const double* restrict x, double* restrict mask);

#endif
