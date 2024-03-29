#ifndef GUARD_OPENBDS_IO_LOGGER_H
#define GUARD_OPENBDS_IO_LOGGER_H

// forward declares the OBDS particle type
struct __OBDS_PARTICLE_TYPE__ ;

// logs the vectors of position, orientation, force, torque, etc., and the particle IDs
int io_logger_log(const struct __OBDS_PARTICLE_TYPE__*, const char*);

#endif

/*

OpenBDS							September 05, 2023

source: io/logger.h
author: @misael-diaz

Synopsis:
Provides a prototype for the OBDS logger.

Copyright (c) 2023 Misael Diaz-Maldonado
This file is released under the GNU General Public License as published
by the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

References:
[0] A Koenig and B Moo, Accelerated C++ Practical Programming by Example.
[1] MP Allen and DJ Tildesley, Computer Simulation of Liquids.
[2] S Kim and S Karrila, Microhydrodynamics: Principles and Selected Applications.

*/
