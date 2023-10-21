#ifndef GUARD_OPENBDS_BDS_PARAMS_H
#define GUARD_OPENBDS_BDS_PARAMS_H

#include <stdint.h>

// defines the OBDS success `0' and error `-1' codes, these are returned by some functions
// to signal the caller that an error has occurred so that the caller can handle the error
// or signal its caller; this is done until we have reached a point in the OBDS code where
// the error can be properly handled
#define __OBDS_SUCCESS__ ( (int) 0x00000000 )
#define __OBDS_FAILURE__ ( (int) 0xffffffff )
// defines the OBDS Pseudo-Random Number Generator PRNG Error; upon failure the PRNG
// returns the binary floating-point representation of negative infinity by design
#define __OBDS_ERR_PRNG__ ( (uint64_t) 0xfff0000000000000 )

#endif

/*

OpenBDS							September 26, 2023

source: bds/params.h
author: @misael-diaz

Synopsis:
Defines OpenBDS Parameter MACROS.

Copyright (c) 2023 Misael Diaz-Maldonado
This file is released under the GNU General Public License as published
by the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

References:
[0] A Koenig and B Moo, Accelerated C++ Practical Programming by Example.
[1] MP Allen and DJ Tildesley, Computer Simulation of Liquids.
[2] S Kim and S Karrila, Microhydrodynamics: Principles and Selected Applications.

*/
