#ifndef GUARD_OPENBDS_UTIL_H
#define GUARD_OPENBDS_UTIL_H

#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>

bool sign (double const x);
void zeros (size_t const size, double* x);
void grid (double* restrict x, double* restrict y, double* restrict z);
void iota (size_t const size, int64_t* x);

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

int64_t overlaps (const double* restrict x,
		  const double* restrict y,
		  const double* restrict z);

void list(int64_t* restrict list,
	  double* restrict d,
	  const double* restrict x,
	  const double* restrict y,
	  const double* restrict z);

int64_t clusters (const int64_t* restrict list, double* restrict mask);
int64_t head (const int64_t* list, int64_t const i);
#endif
