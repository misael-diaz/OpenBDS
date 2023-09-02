#ifndef GUARD_OPENBDS_UTIL_ARRAYS_H
#define GUARD_OPENBDS_UTIL_ARRAYS_H

#include <stdint.h>

void copy (const double* restrict src, double* restrict dst);
void zeros (double* x);
void iota (int64_t* x);

#endif
