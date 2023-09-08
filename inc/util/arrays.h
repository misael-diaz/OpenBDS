#ifndef GUARD_OPENBDS_UTIL_ARRAYS_H
#define GUARD_OPENBDS_UTIL_ARRAYS_H

#include "bds/types.h"

void copy (const prop_t* restrict src, prop_t* restrict dst);
void zeros (prop_t* x);
void iota (prop_t* x);

#endif

/*

OpenBDS							September 05, 2023

source: util/arrays.h
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
