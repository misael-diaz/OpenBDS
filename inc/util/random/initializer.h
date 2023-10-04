#ifndef GUARD_OPENBDS_UTIL_RANDOM_INITIALIZER_H
#define GUARD_OPENBDS_UTIL_RANDOM_INITIALIZER_H

// forward declarations:

enum PRNG;
struct random;

// utils:

int util_random_initializer(struct random*, enum PRNG);

#endif

/*

OpenBDS							October 04, 2023

source: util/random/initializer.h
author: @misael-diaz

Synopsis:
Provides prototype for the Pseudo Random Number Generator PRNG initializer.

Copyright (c) 2023 Misael Diaz-Maldonado
This file is released under the GNU General Public License as published
by the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

References:
[0] A Koenig and B Moo, Accelerated C++ Practical Programming by Example.
[1] MP Allen and DJ Tildesley, Computer Simulation of Liquids.
[2] S Kim and S Karrila, Microhydrodynamics: Principles and Selected Applications.

*/
