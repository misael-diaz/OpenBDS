#include "system/box/utils.h"

static void pbcs (particle_t* particles)
{
  prop_t* x = particles -> x;
  prop_t* y = particles -> y;
  prop_t* z = particles -> z;
  prop_t* offset = particles -> tmp;
  prop_t* bitmask = particles -> bitmask;
  pbc(x, offset, bitmask);
  pbc(y, offset, bitmask);
  pbc(z, offset, bitmask);
}

// applies periodic boundary conditions on the particles
void util_particle_pbcs (particle_t* particles)
{
  pbcs(particles);
}

/*

OpenBDS								September 26, 2023

source: util/particle/particle.c
author: @misael-diaz

Synopsis:
Implements utility methods for OBDS particles.

Copyright (c) 2023 Misael Diaz-Maldonado
This file is released under the GNU General Public License as published
by the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

References:
[0] A Koenig and B Moo, Accelerated C++ Practical Programming by Example.
[1] MP Allen and DJ Tildesley, Computer Simulation of Liquids.
[2] S Kim and S Karrila, Microhydrodynamics: Principles and Selected Applications.

*/
