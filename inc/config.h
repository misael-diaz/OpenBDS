#ifndef GUARD_OPENBDS_CONFIG_H
#define GUARD_OPENBDS_CONFIG_H

#include <stddef.h>

#include "fconfig.h"

#define CONF_NUM_PARTICLES ( (size_t) FCONF_NUM_PARTICLES )
#define CONF_LIMIT ( (double) FCONF_LIMIT )

#endif

/*

OpenBDS                                                 September 07, 2023

source: config.h
author: @misael-diaz

Synopsis:
Defines some parameters for configuring the Brownian Dynamics Simulations BDS.
Do not edit this header file manually, edit the FORTRAN fconfig.h instead.

Copyright (c) 2023 Misael Diaz-Maldonado
This file is released under the GNU General Public License as published
by the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

References:
[0] A Koenig and B Moo, Accelerated C++ Practical Programming by Example.
[1] MP Allen and DJ Tildesley, Computer Simulation of Liquids.
[2] S Kim and S Karrila, Microhydrodynamics: Principles and Selected Applications.

*/
