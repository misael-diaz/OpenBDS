#ifndef GUARD_OPENBDS_BDS_TYPES_OBDS_PROPERTY_TYPE_H
#define GUARD_OPENBDS_BDS_TYPES_OBDS_PROPERTY_TYPE_H

#include <stdint.h>

// we take advantage of this union to reuse data placeholders and for writing vectorizable
// for-loops that would not be otherwise optimized by the GCC compiler
union __OBDS_PROP_TYPE__
{
  double data;
  uint64_t bin;
};

typedef union __OBDS_PROP_TYPE__ prop_t;

#endif

/*

OpenBDS							October 15, 2023

source: bds/types/property.h
author: @misael-diaz

Synopsis:
Defines the OpenBDS property type.
Sometimes we operate on floats as if they were integers so this definition is quite handy.

Copyright (c) 2023 Misael Diaz-Maldonado
This file is released under the GNU General Public License as published
by the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

References:
[0] A Koenig and B Moo, Accelerated C++ Practical Programming by Example.
[1] MP Allen and DJ Tildesley, Computer Simulation of Liquids.
[2] S Kim and S Karrila, Microhydrodynamics: Principles and Selected Applications.

*/
