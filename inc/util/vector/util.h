#ifndef GUARD_OPENBDS_UTIL_VECTOR_UTIL_H
#define GUARD_OPENBDS_UTIL_VECTOR_UTIL_H

// includes the vector type
#include "type.h"

// returns the smallest, absolute-valued, element of vector `vec', min( abs(vec[:]) )
double util_vector_min (const vector_t* __restrict__ vec);

// returns the largest,  absolute-valued, element of vector `vec', max( abs(vec[:]) )
double util_vector_max (const vector_t* __restrict__ vec);

// fills vector `vec' with zeros
void util_vector_zeros (vector_t* __restrict__ vec);

// sums the vectors `dst' and `src', the result is stored in `dst'
void util_vector_sum (vector_t* __restrict__ dst,
		      const vector_t* __restrict__ src);

#endif

/*

OpenBDS							October 05, 2023

source: util/vector/util.h
author: @misael-diaz

Synopsis:
Provides prototypes for utils that operate on vectors.

Copyright (c) 2023 Misael Diaz-Maldonado
This file is released under the GNU General Public License as published
by the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

References:
[0] A Koenig and B Moo, Accelerated C++ Practical Programming by Example.
[1] MP Allen and DJ Tildesley, Computer Simulation of Liquids.
[2] S Kim and S Karrila, Microhydrodynamics: Principles and Selected Applications.

*/
