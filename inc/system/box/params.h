#ifndef GUARD_OPENBDS_SYSTEM_BOX_PARAMS_H
#define GUARD_OPENBDS_SYSTEM_BOX_PARAMS_H

#include "config.h"

#define __OBDS_LIMIT__ ( (double) ( CONF_LIMIT ) )
#define __OBDS_LENGTH__ ( (double) ( 2.0 * ( __OBDS_LIMIT__ ) ) )

#endif

/*

OpenBDS							September 05, 2023

source: system/box/params.h
author: @misael-diaz

Synopsis:
Defines the system box (cubic) limits.

Copyright (c) 2023 Misael Diaz-Maldonado
This file is released under the GNU General Public License as published
by the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

References:
[0] A Koenig and B Moo, Accelerated C++ Practical Programming by Example.
[1] MP Allen and DJ Tildesley, Computer Simulation of Liquids.
[2] S Kim and S Karrila, Microhydrodynamics: Principles and Selected Applications.

*/
