#ifndef GUARD_OPENBDS_UTIL_H
#define GUARD_OPENBDS_UTIL_H

#include <stdlib.h>
#include <stdbool.h>

bool sign (double const x);
void zeros (size_t const size, double* x);
void iota (size_t const size, double* x);

void mask_partition (size_t const size, const double* restrict x, double* restrict mask);
void mask_unlimited (size_t const size,
		     const double* restrict x,
		     const double* restrict mask,
		     double* restrict temp,
		     double* restrict bitmask);

void pbc (double* restrict x,
	  double* restrict temp,
	  double* restrict mask,
	  double* restrict bitmask);

#endif
