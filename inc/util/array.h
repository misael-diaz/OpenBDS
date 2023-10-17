#ifndef GUARD_OPENBDS_UTIL_ARRAY_H
#define GUARD_OPENBDS_UTIL_ARRAY_H

// forward declares the OBDS property type
union __OBDS_PROP_TYPE__ ;

// copies source `src' into destination `dst' array
void util_array_copy (const union __OBDS_PROP_TYPE__ * __restrict__ src,
		      union __OBDS_PROP_TYPE__ * __restrict__ dst);

// numpy-like utils:

// fills array `x' with zeros, numel(x) = `N' where `N' is the number of particles
void util_array_zeros (union __OBDS_PROP_TYPE__ * x);
// as zeros but fills array with ones
void util_array_ones (union __OBDS_PROP_TYPE__ * x);
// fills array with values in the asymmetric range [0, `N'] (based on C++ std::iota)
void util_array_iota (union __OBDS_PROP_TYPE__ * x);

#endif

/*

OpenBDS							September 05, 2023

source: util/array.h
author: @misael-diaz

Synopsis:
Provides prototypes for utils that operate on arrays.

Copyright (c) 2023 Misael Diaz-Maldonado
This file is released under the GNU General Public License as published
by the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

References:
[0] A Koenig and B Moo, Accelerated C++ Practical Programming by Example.
[1] MP Allen and DJ Tildesley, Computer Simulation of Liquids.
[2] S Kim and S Karrila, Microhydrodynamics: Principles and Selected Applications.

*/
