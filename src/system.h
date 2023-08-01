#ifndef GUARD_OPENBDS_SYSTEM_H
#define GUARD_OPENBDS_SYSTEM_H

#define NUM_SPHERES 256
#define LIMIT 8.0
#define LENGTH (2.0 * LIMIT)
#define RADIUS 1.0
#define CONTACT (2.0 * RADIUS)
#define RANGE ( (1.5 * CONTACT) * (1.5 * CONTACT) )

#endif

/*

OpenBDS								July 19, 2023

source: system.h
author: @misael-diaz

Synopsis:
Defines the number of spheres in the system.

Copyright (c) 2023 Misael Diaz-Maldonado
This file is released under the GNU General Public License as published
by the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

References:
[0] A Koenig and B Moo, Accelerated C++ Practical Programming by Example.
[1] MP Allen and DJ Tildesley, Computer Simulation of Liquids.
[2] S Kim and S Karrila, Microhydrodynamics: Principles and Selected Applications.

*/
