#ifndef GUARD_OPENBDS_SYSTEM_BOX_UTILS_H
#define GUARD_OPENBDS_SYSTEM_BOX_UTILS_H

#include "bds/types.h"

void system_box_apply_periodic_boundary_conditions(particle_t* particles);

#endif

/*

OpenBDS							September 05, 2023

source: system/box/utils.h
author: @misael-diaz

Synopsis:
Defines the system box utils for applying periodic boundary conditions PBCs.

Copyright (c) 2023 Misael Diaz-Maldonado
This file is released under the GNU General Public License as published
by the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

References:
[0] A Koenig and B Moo, Accelerated C++ Practical Programming by Example.
[1] MP Allen and DJ Tildesley, Computer Simulation of Liquids.
[2] S Kim and S Karrila, Microhydrodynamics: Principles and Selected Applications.

*/
