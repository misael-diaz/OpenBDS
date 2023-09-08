#ifndef GUARD_OPENBDS_UTIL_TYPE_H
#define GUARD_OPENBDS_UTIL_TYPE_H

#include "random.h"

struct util
{
  iPRNG_t random;		// PRNG initializer
};

typedef struct util util_t;

#endif

/*

OpenBDS							September 05, 2023

source: util/type.h
author: @misael-diaz

Synopsis:
Defines a namespace for some utilities.

Copyright (c) 2023 Misael Diaz-Maldonado
This file is released under the GNU General Public License as published
by the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

References:
[0] A Koenig and B Moo, Accelerated C++ Practical Programming by Example.
[1] MP Allen and DJ Tildesley, Computer Simulation of Liquids.
[2] S Kim and S Karrila, Microhydrodynamics: Principles and Selected Applications.

*/
