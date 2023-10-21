#ifndef GUARD_OPENBDS_BDS_TYPES_FORCE_ENUM_H
#define GUARD_OPENBDS_BDS_TYPES_FORCE_ENUM_H

enum FORCE
{
  DETERMINISTIC,
  BROWNIAN,
};

#endif

/*

OpenBDS							October 20, 2023

source: bds/types/force.h
author: @misael-diaz

Synopsis:
Provides a way to differentiate deterministic from Brownian (or stochastic) forces.

Copyright (c) 2023 Misael Diaz-Maldonado
This file is released under the GNU General Public License as published
by the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

References:
[0] A Koenig and B Moo, Accelerated C++ Practical Programming by Example.
[1] MP Allen and DJ Tildesley, Computer Simulation of Liquids.
[2] S Kim and S Karrila, Microhydrodynamics: Principles and Selected Applications.

*/
