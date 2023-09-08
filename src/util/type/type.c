#include "util/type.h"

extern int util_random_initializer(random_t*, enum PRNG);

static iPRNG_t const random = {
  .initializer = util_random_initializer
};

util_t const util = {
  .random = random
};

/*

OpenBDS								September 07, 2023

source: util/type/type.c
author: @misael-diaz

Synopsis:
Initializes the util object of type `util_t' (which is effectively a namespace).

Copyright (c) 2023 Misael Diaz-Maldonado
This file is released under the GNU General Public License as published
by the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

References:
[0] A Koenig and B Moo, Accelerated C++ Practical Programming by Example.
[1] MP Allen and DJ Tildesley, Computer Simulation of Liquids.
[2] S Kim and S Karrila, Microhydrodynamics: Principles and Selected Applications.

*/
